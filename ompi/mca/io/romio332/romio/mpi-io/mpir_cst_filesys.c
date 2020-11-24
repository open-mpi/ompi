/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2016 UChicago/Argonne LLC
 *      See COPYRIGHT in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

static int comm_split_filesystem_exhaustive(MPI_Comm comm, int key,
                                            const char *dirname, MPI_Comm * newcomm)
{
    /* If you run this at scale against GPFS, be prepared to spend 30 mintues
     * creating 10,000 files -- and the numbers only get worse from there.
     *
     * - create random directory
     * - create files in that directory
     * - based on the visible files, construct a new group, then a new
     *   communicator
     * - there are no directory operation routines in MPI so we'll do it via
     *   POSIX.  */
    int rank, nprocs, ret;
    int *ranks;
    MPI_Group comm_group, newgroup;
    int j = 0, mpi_errno = MPI_SUCCESS;
    char *filename = NULL, *testdirname = NULL;
    DIR *dir;
    struct dirent *entry;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    /* rank zero constructs the candidate directory name (just the
     * name).  Everyone will create the directory though -- this will be
     * a headache for the file system at scale..  don't do this on a
     * large parallel file system! */

    testdirname = MPL_malloc(PATH_MAX, MPL_MEM_IO);
    filename = MPL_malloc(PATH_MAX, MPL_MEM_IO);
    ranks = MPL_malloc(nprocs * sizeof(int), MPL_MEM_IO);

    if (rank == 0)
        MPL_create_pathname(testdirname, dirname, ".commonfstest.0", 1);

    MPI_Bcast(testdirname, PATH_MAX, MPI_BYTE, 0, comm);
    /* ignore EEXIST: quite likely another process will have made this
     * directory, but since the whole point is to figure out who we share this
     * directory with, brute force it is! */
    ret = mkdir(testdirname, S_IRWXU);
    if (ret == -1 && errno != EEXIST)
        goto fn_fail;
    MPL_snprintf(filename, PATH_MAX, "%s/%d", testdirname, rank);
    open(filename, O_CREAT, S_IRUSR | S_IWUSR);

    MPI_Barrier(comm);
    /* each process has created a file in a M-way shared directory (where M in
     * the range [1-nprocs]).  Let's see who else can see this directory */
    if ((dir = opendir(testdirname)) == NULL)
        goto fn_fail;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0)
            continue;
        if (strcmp(entry->d_name, "..") == 0)
            continue;
        ranks[j++] = atoi(entry->d_name);
    }

    MPI_Comm_group(comm, &comm_group);
    MPI_Group_incl(comm_group, j, ranks, &newgroup);
    MPI_Comm_create(comm, newgroup, newcomm);
    MPI_Group_free(&newgroup);
    MPI_Group_free(&comm_group);

    unlink(filename);
    /* ok to ignore errors */
    rmdir(testdirname);

  fn_exit:
    MPL_free(ranks);
    MPL_free(filename);
    MPL_free(testdirname);
    return mpi_errno;
  fn_fail:
    goto fn_exit;
}

static int comm_split_filesystem_heuristic(MPI_Comm comm, int key,
                                           const char *dirname, MPI_Comm * newcomm)
{
    int i, mpi_errno = MPI_SUCCESS;
    int rank, nprocs;
    int id;
    int32_t *all_ids;
    char *filename = NULL;
    int challenge_rank, globally_visible = 0;
    MPI_Request check_req;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);
    MPIR_Get_node_id(comm, rank, &id);

    /* We could detect the common file systems by parsing 'df'-style
     * output, but that's fidgety, fragile, and error prone.  Instead,
     * determine who shares a file system through testing.
     *
     * As an optimization, we should try to avoid creating a lot of
     * files: we want something that could work at hundreds of thousands
     * of nodes, and creating a hundred thousand files in a directory is
     * a recipe for sadness
     *
     * In CH3 and in wider practice "shared memory" is the same as "on
     * the same node, so let's start there.
     *
     * - Create file on one processor
     * - pick a processor outside the "on this node" group
     * - if that processor can see the file, then assume the file is
     *   visible to all groups.
     *
     * note that this scheme works really well for traditional linux clusters:
     * think nodes with a local scratch drive.  this scheme works less well for
     * a deeper heirarchy.  what if the directory in question was hosted by an
     * i/o forwarding agent?
     */

    /* learn a bit about what groups were created: as a scalable
     * optimization we want to check a file's presence from a group
     * other than which created it */
    all_ids = MPL_malloc(nprocs * sizeof(*all_ids), MPL_MEM_IO);

    mpi_errno = MPI_Gather(&id, 1, MPI_INT32_T, all_ids, 1, MPI_INT32_T, 0, comm);

    if (rank == 0) {
        for (i = 0; i < nprocs; i++) {
            if (all_ids[i] != id)
                break;
        }
        if (i >= nprocs)
            /* everyone is in the same group; pick a process that's not rank 0
             * just in case the file system is really weird */
            challenge_rank = nprocs - 1;
        else
            challenge_rank = i;
    }
    mpi_errno = MPI_Bcast(&challenge_rank, 1, MPI_INT, 0, comm);

    /* now that we've informally lumped everyone into groups based on node
     * (like shared memory does) it's time to poke the file system and see
     * which group can see what files */

    /* here come a bunch of assumptions:
     * - file system layouts are homogenous: if one system has /scratch,
     *   all have /scratch
     * - a globally visible parallel file system will have the same name
     *   everywhere: e.g /gpfs/users/something
     * - a file created on one node will be deterministically visible on
     *   another.  NFS has problems with this
     * - if a process from one group creates a file, and a process from
     *   another group finds that file, then a process from all groups
     *   can find that file
     */

    /* is the file globally visible to all?  create on rank 0, test on a
     * different off-group rank.
     * Use a single short message to force check after create: ordering
     * is a little odd in case we are creating and checking on the same
     * rank  */

    filename = MPL_calloc(PATH_MAX, sizeof(char), MPL_MEM_IO);

    if (rank == 0)
        MPL_create_pathname(filename, dirname, ".commonfstest.0", 0);

    MPI_Bcast(filename, PATH_MAX, MPI_BYTE, 0, comm);

    if (rank == challenge_rank) {
        MPI_Irecv(NULL, 0, MPI_BYTE, 0, 0, comm, &check_req);
    }

    if (rank == 0) {
        MPI_File fh;
        mpi_errno = MPI_File_open(MPI_COMM_SELF, filename,
                                  MPI_MODE_CREATE | MPI_MODE_EXCL | MPI_MODE_WRONLY,
                                  MPI_INFO_NULL, &fh);
        if (mpi_errno != MPI_SUCCESS)
            goto fn_exit;
        MPI_File_close(&fh);
        /* the check for file has to happen after file created. only need one
         * process, though, not a full barrier */
        MPI_Send(NULL, 0, MPI_BYTE, challenge_rank, 0, comm);
    }

    if (rank == challenge_rank) {
        MPI_File fh;

        MPI_Wait(&check_req, MPI_STATUS_IGNORE);

        /* too bad there's no ADIO equivalent of access: we'll have to
         * open/close the file instead */

        mpi_errno = MPI_File_open(MPI_COMM_SELF, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);
        if (mpi_errno == MPI_SUCCESS) {
            globally_visible = 1;
            MPI_File_close(&fh);
        } else {
            /* do not report error up to caller.  we are merely testing the
             * presence of the file */
            mpi_errno = MPI_SUCCESS;
            globally_visible = 0;
        }
    }
    MPI_Bcast(&globally_visible, 1, MPI_INT, challenge_rank, comm);

    /*   with the above assumptions, we have two cases for a flie
     *   created on one process:
     *   -- either a process not in the group can access it (node-local
     *      storage of some sort)
     *   -- or a process not in the group cannot access it (globally
     *      accessable parallel file system) */

    if (globally_visible) {
        MPI_Comm_dup(comm, newcomm);
    } else {
        MPI_Comm_split(comm, id, key, newcomm);
    }
    if (rank == 0)
        MPI_File_delete(filename, MPI_INFO_NULL);

  fn_exit:
    MPL_free(all_ids);
    MPL_free(filename);
    return mpi_errno;
}

/* not to be called directly (note the MPIR_ prefix), but instead from
 * MPI-level MPI_Comm_split_type implementation (e.g.
 * MPIR_Comm_split_type_impl). */
#undef FUNCNAME
#define FUNCNAME MPIR_Comm_split_filesystem
#undef FCNAME
#define FCNAME MPL_QUOTE(FUNCNAME)

/* split communicator based on access to directory 'dirname'. */
int MPIR_Comm_split_filesystem(MPI_Comm comm, int key, const char *dirname, MPI_Comm * newcomm)
{
    int mpi_errno = MPI_SUCCESS;
    char *s;
    if ((s = getenv("MPIX_SPLIT_DISABLE_HEURISTIC")) != NULL) {
        mpi_errno = comm_split_filesystem_exhaustive(comm, key, dirname, newcomm);
    } else {
        mpi_errno = comm_split_filesystem_heuristic(comm, key, dirname, newcomm);
    }
    return mpi_errno;
}

/*
 * vim: ts=8 sts=4 sw=4 noexpandtab
 */
