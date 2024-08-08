/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/* This file is quickly becoming the single one, outside the ADIO
 * implementations, which has "what ADIO components are built in" code in it.
 */

#include "adio.h"

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_PVFS_H
#include "pvfs.h"
#endif

#ifdef HAVE_PVFS2_H
#include "pvfs2.h"
#endif

#ifdef HAVE_ZOIDFS_H
#include "zoidfs.h"
#endif

#ifdef HAVE_GPFS_H
#include "gpfs.h"
#endif

/* Notes on detection process:
 *
 * There are three more "general" mechanisms that we use for detecting
 * file system type:
 * - struct statfs's f_type field
 * - struct statvfs's f_basetype field
 * - struct stat's st_fstype field
 *
 * Otherwise we'll fall back on some OS-specific approach.
 */

#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
 /* On Linux platforms, linux/nfs_fs.h is all messed up and cannot be
  * reliably included.
  */
#if !defined(NFS_SUPER_MAGIC)
#define NFS_SUPER_MAGIC 0x6969
#endif

#if !defined(PAN_KERNEL_FS_CLIENT_SUPER_MAGIC)
#define PAN_KERNEL_FS_CLIENT_SUPER_MAGIC 0xAAD7AAEA
#endif

#if !defined(XFS_SUPER_MAGIC)
#define XFS_SUPER_MAGIC 0x58465342
#endif

#if !defined(EXFS_SUPER_MAGIC)
#define EXFS_SUPER_MAGIC 0x45584653
#endif

#if !defined(PVFS2_SUPER_MAGIC)
#define PVFS2_SUPER_MAGIC (0x20030528)
#endif

#if !defined(GPFS_SUPER_MAGIC)
#define GPFS_SUPER_MAGIC 0x47504653
#endif

#ifndef LL_SUPER_MAGIC
#define LL_SUPER_MAGIC 0x0BD00BD0
#endif

#if !defined(DAOS_SUPER_MAGIC)
#define DAOS_SUPER_MAGIC (0xDA05AD10)
#endif

#define UNKNOWN_SUPER_MAGIC (0xDEADBEEF)

#ifdef HAVE_STRUCT_STATVFS_WITH_F_BASETYPE
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#endif

#ifdef HAVE_STRUCT_STAT_WITH_ST_FSTYPE
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#endif

#ifdef HAVE_STRUCT_STAT_WITH_ST_FSID
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#endif

#ifndef ROMIO_NTFS
#define ROMIO_NEEDS_ADIOPARENTDIR
static void ADIO_FileSysType_parentdir(const char *filename, char **dirnamep);
#endif
static int ADIO_FileSysType_prefix(const char *filename, int *fstype,
                                   ADIOI_Fns ** ops, int *error_code);
static void ADIO_FileSysType_fncall(const char *filename, int *fstype, int *error_code);
struct ADIO_FSTypes {
    ADIOI_Fns *fileops;         /* function table */
    int fstype;                 /* ADIO_xxx constant */
    const char *prefix;         /* file prefix */
    int64_t magic;              /* identifier for file system, or UNKNOWN */
};

/*
 * To add an ADIO
 *    - add to the table below
 *    - add a constant for your ADIO in include/adio.h
 *    - add a guarded include in include/adioi_fs_proto.h
 *    - add your prefix to the 'fstype_prefix' array
 */
static struct ADIO_FSTypes fstypes[] = {
#ifdef ROMIO_UFS
    {&ADIO_UFS_operations, ADIO_UFS, "ufs:", UNKNOWN_SUPER_MAGIC},
#endif
#ifdef ROMIO_NFS
    {&ADIO_NFS_operations, ADIO_NFS, "nfs:", NFS_SUPER_MAGIC},
#endif
#ifdef ROMIO_XFS
    {&ADIO_XFS_operations, ADIO_XFS, "xfs:", XFS_SUPER_MAGIC},
    /* rare, but could be on some systems. Both magic values use the same ROMIO
     * driver, though */
    {&ADIO_XFS_operations, ADIO_XFS, "xfs:", EXFS_SUPER_MAGIC},
#endif
#ifdef ROMIO_PVFS2
    {&ADIO_PVFS2_operations, ADIO_PVFS2, "pvfs2:", PVFS2_SUPER_MAGIC},
#endif
#ifdef ROMIO_GPFS
    {&ADIO_GPFS_operations, ADIO_GPFS, "gpfs:", GPFS_SUPER_MAGIC},
#endif
#ifdef ROMIO_PANFS
    {&ADIO_PANFS_operations, ADIO_PANFS, "panfs:", PAN_KERNEL_FS_CLIENT_SUPER_MAGIC},
#endif
#ifdef ROMIO_LUSTRE
    {&ADIO_LUSTRE_operations, ADIO_LUSTRE, "lustre:", LL_SUPER_MAGIC},
#endif
#ifdef ROMIO_DAOS
    {&ADIO_DAOS_operations, ADIO_DAOS, "daos:", DAOS_SUPER_MAGIC},
#endif
#ifdef ROMIO_TESTFS
    /* never selected automatically */
    {&ADIO_TESTFS_operations, ADIO_TESTFS, "testfs:", 0},
#endif
#ifdef ROMIO_IME
    /* userspace driver only selected via prefix */
    {&ADIO_IME_operations, ADIO_IME, "ime:", 0},
#endif
#ifdef ROMIO_QUOBYTEFS
    /* userspace driver only selected via prefix */
    {&ADIO_QUOBYTEFS_operations, ADIO_QUOBYTEFS, "quobyte:", 0},
#endif
    {0, 0, 0, 0}        /* guard entry */
};

/* File system type prefix name recognized by ROMIO: this is slightly different
 * from the 'fstypes' table above -- this is any kind of file system romio
 * might know about, whereas the 'fstype' table is populated only by file
 * systems enabled when ROMIO was built */
static const char *fstype_prefix[] = {
    "ufs",
    "nfs",
    "xfs",
    "pvfs2",
    "gpfs",
    "panfs",
    "lustre",
    "daos",
    "testfs",
    "ime",
    "quobyte",
    NULL        /* guard entry */
};

/*
 ADIO_FileSysType_parentdir - determines a string pathname for the
 parent directory of a given filename.

Input Parameters:
. filename - pointer to file name character array

Output Parameters:
. dirnamep - pointer to location in which to store a pointer to a string

 Note that the caller should free the memory located at the pointer returned
 after the string is no longer needed.
*/
#ifdef ROMIO_NEEDS_ADIOPARENTDIR

/* In a strict ANSI environment, S_ISLNK may not be defined.  Fix that
   here.  We assume that S_ISLNK is *always* defined as a macro.  If
   that is not universally true, then add a test to the romio
   configure that tries to link a program that references S_ISLNK */
#if !defined(S_ISLNK)
#if defined(S_IFLNK)
     /* Check for the link bit */
#define S_ISLNK(mode) ((mode) & S_IFLNK)
#else
     /* no way to check if it is a link, so say false */
#define S_ISLNK(mode) 0
#endif
#endif /* !(S_ISLNK) */

/* ADIO_FileSysType_parentdir
 *
 * Returns pointer to string in dirnamep; that string is allocated with
 * strdup and must be free()'d.
 */
static void ADIO_FileSysType_parentdir(const char *filename, char **dirnamep)
{
    int err;
    char *dir = NULL, *slash;
    struct stat statbuf;

    err = lstat(filename, &statbuf);

    if (err || (!S_ISLNK(statbuf.st_mode))) {
        /* no such file, or file is not a link; these are the "normal"
         * cases where we can just return the parent directory.
         */
        dir = ADIOI_Strdup(filename);
    } else {
        /* filename is a symlink.  we've presumably already tried
         * to stat it and found it to be missing (dangling link),
         * but this code doesn't care if the target is really there
         * or not.
         */
        ssize_t namelen;
        char *linkbuf;

        linkbuf = ADIOI_Malloc(PATH_MAX + 1);
        namelen = readlink(filename, linkbuf, PATH_MAX + 1);
        if (namelen == -1) {
            /* something strange has happened between the time that
             * we determined that this was a link and the time that
             * we attempted to read it; punt and use the old name.
             */
            dir = ADIOI_Strdup(filename);
        } else {
            /* successfully read the link */
            linkbuf[namelen] = '\0';    /* readlink doesn't null terminate */
            dir = ADIOI_Strdup(linkbuf);
        }
        ADIOI_Free(linkbuf);
    }

    slash = strrchr(dir, '/');
    if (!slash)
        ADIOI_Strncpy(dir, ".", 2);
    else {
        if (slash == dir)
            *(dir + 1) = '\0';
        else
            *slash = '\0';
    }

    *dirnamep = dir;
    return;
}
#endif /* ROMIO_NTFS */


static int romio_statfs(const char *filename, int64_t * file_id)
{

    int err = 0;

#ifdef HAVE_STRUCT_STATVFS_WITH_F_BASETYPE
    /* rare: old solaris machines */
    struct statvfs vfsbuf;
#endif
#if defined(HAVE_STRUCT_STATFS_F_TYPE) || defined(HAVE_STRUCT_STATFS_F_FSTYPENAME)
    /* common fs-detection logic for any modern POSIX-compliant environment,
     * with the one wrinkle that some platforms (Darwin, BSD) give us a file
     * system as a string, not an identifier */
    struct statfs fsbuf;
#endif
#if defined (HAVE_STRUCT_STAT_ST_FSTYPE)
    struct stat sbuf;
#endif

    *file_id = UNKNOWN_SUPER_MAGIC;

#ifdef HAVE_STRUCT_STATVFS_WITH_F_BASETYPE
    err = statvfs(filename, &vfsbuf);
    if (err == 0)
        *file_id = vfsbuf.f_basetype;
#endif

/* remember above how I said 'statfs with f_type' was the common linux-y way to
 * report file system type?  Darwin (and probably the BSDs) *also* uses f_type
 * but it is "reserved" and does not give us anything meaningful.  Fine.  If
 * configure detects f_type we'll use it here and on those "reserved" platforms
 * we'll ignore that result and check the f_fstypename field  */
#ifdef HAVE_STRUCT_STATFS_F_TYPE
    err = statfs(filename, &fsbuf);
    if (err == 0)
        *file_id = fsbuf.f_type;
#endif


#if defined(HAVE_STRUCT_STATFS_F_FSTYPENAME) || defined(HAVE_STRUCT_STAT_ST_FSTYPE)
    /* these stat routines store the file system type in a string */
    char *fstype;
#ifdef HAVE_STRUCT_STATFS_F_FSTYPENAME
    err = statfs(filename, &fsbuf);
    fstype = fsbuf.f_fstypename;
#else
    err = stat(filename, &sbuf);
    fstype = sbuf.st_fstype;
#endif
    if (err == 0) {
        int i = 0;
        /* any file system type not explicitly in the fstype table (ffs, hfs)
         * will be "unknown" which ROMIO will service with ADIO_UFS */
        while (fstypes[i].fileops) {
            /* '-1' to ignore the trailing colon */
            if (!strncasecmp(fstypes[i].prefix, fstype, strlen(fstypes[i].prefix) - 1)) {
                *file_id = fstypes[i].magic;
            }
            i++;
        }
    }
#endif

    return err;

}

/*
 ADIO_FileSysType_fncall - determines the file system type for a given file
 using a system-dependent function call

Input Parameters:
. filename - pointer to file name character array

Output Parameters:
. fstype - location in which to store file system type (ADIO_XXX)
. error_code - location in which to store error code

 MPI_SUCCESS is stored in the location pointed to by error_code on success.

 This function is used by MPI_File_open() and MPI_File_delete() to determine
 file system type.  Most other functions use the type which is stored when the
 file is opened.
 */
static void ADIO_FileSysType_fncall(const char *filename, int *fstype, int *error_code)
{
    int err;
    int64_t file_id;
    static char myname[] = "ADIO_RESOLVEFILETYPE_FNCALL";


/* NFS can get stuck and end up returning ESTALE "forever" */
#define MAX_ESTALE_RETRY 10000
    int retry_cnt;

    *error_code = MPI_SUCCESS;

    retry_cnt = 0;
    do {
        err = romio_statfs(filename, &file_id);
    } while (err && (errno == ESTALE) && retry_cnt++ < MAX_ESTALE_RETRY);

    if (err) {
        /* ENOENT may be returned in two cases:
         * 1) no directory entry for "filename"
         * 2) "filename" is a dangling symbolic link
         *
         * ADIO_FileSysType_parentdir tries to deal with both cases.
         */
        if (errno == ENOENT) {
            char *dir;
            ADIO_FileSysType_parentdir(filename, &dir);
            err = romio_statfs(dir, &file_id);

            ADIOI_Free(dir);
        } else {
            *error_code = ADIOI_Err_create_code(myname, filename, errno);
            if (*error_code != MPI_SUCCESS)
                return;
        }
    }

    /* --BEGIN ERROR HANDLING-- */
    if (err) {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                           myname, __LINE__, MPI_ERR_NO_SUCH_FILE,
                                           "**filename", "**filename %s", filename);
        return;
    }
    /* --END ERROR HANDLING-- */

    switch (file_id) {
#ifdef ROMIO_NFS
        case NFS_SUPER_MAGIC:
            *fstype = ADIO_NFS;
            return;
#endif
#ifdef ROMIO_XFS
        case XFS_SUPER_MAGIC:
        case EXFS_SUPER_MAGIC:
            *fstype = ADIO_XFS;
            return;
#endif
#ifdef ROMIO_GPFS
        case GPFS_SUPER_MAGIC:
            *fstype = ADIO_GPFS;
            return;
#endif
#ifdef ROMIO_LUSTRE
        case LL_SUPER_MAGIC:
            *fstype = ADIO_LUSTRE;
            return;
#endif
#ifdef ROMIO_DAOS
        case DAOS_SUPER_MAGIC:
            *fstype = ADIO_DAOS;
            return;
#endif
#ifdef ROMIO_PANFS
        case PAN_KERNEL_FS_CLIENT_SUPER_MAGIC:
            *fstype = ADIO_PANFS;
            return;
#endif
#ifdef ROMIO_PVFS2
        case PVFS2_SUPER_MAGIC:
            *fstype = ADIO_PVFS2;
            return;
#endif
        default:
            /* UFS support if we don't know what else to use */
            *fstype = ADIO_UFS;
            return;
    }
}

/* all proceeses opening, creating, or deleting a file end up invoking several
 * stat system calls (unless a fs prefix is given).  Cary out this file system
 * detection in a more scalable way by having rank 0 stat the file and broadcast the result (fs type and error code) to the other mpi processes */

static void ADIO_FileSysType_fncall_scalable(MPI_Comm comm, const char *filename,
                                             int *file_system, int *error_code)
{
    int rank;
    int buf[2];
    MPI_Comm_rank(comm, &rank);

    if (rank == 0) {
        ADIO_FileSysType_fncall(filename, file_system, error_code);
        buf[0] = *file_system;
        buf[1] = *error_code;
    }
    MPI_Bcast(buf, 2, MPI_INT, 0, comm);
    *file_system = buf[0];
    *error_code = buf[1];
}



/*
  ADIO_FileSysType_prefix - determines file system type for a file using
  a prefix on the file name.  upper layer should have already determined
  that a prefix is present.

Input Parameters:
. filename - path to file, including prefix (xxx:)

Output Parameters:
. fstype - pointer to integer in which to store file system type (ADIO_XXX)
. error_code - pointer to integer in which to store error code

  Returns MPI_SUCCESS in error_code on success.  Filename not having a prefix
  is considered an error. Except for on Windows systems where the default is NTFS.

Return value:
. 1 - a known file system prefix name is found
. 0 - otherwise
 */
static int ADIO_FileSysType_prefix(const char *filename, int *fstype,
                                   ADIOI_Fns ** ops, int *error_code)
{
    char *prefix, *colon;
    int i, known_prefix = 0;
    static char myname[] = "ADIO_FileSysType_prefix";

    *error_code = MPI_SUCCESS;
    *fstype = -1;

    /* check if there is a prefix on the filename and if so, could it possibly
     * be a ROMIO file system? (Trying to avoid cases where the file name has a
     * colon for non-ROMIO reasons -- such as an HH:MM:SS formatted time stamp */

    prefix = (char *) filename;

    colon = strchr(prefix, ':');
    if (colon == NULL) {        /* no prefix */
        /* there may be situations where one cannot override the file system
         * detection with a prefix -- maybe the file name is passed to both
         * posix and MPI-IO routines, or maybe the file name is hard-coded into
         * an application.
         * Assumes all processes set the same environment varialble.  Values:
         * the same prefix you would stick on a file path. e.g. pvfs2: --
         * including the colon!
         */
        prefix = getenv("ROMIO_FSTYPE_FORCE");
        if (prefix != NULL)
            colon = strchr(prefix, ':');
    }
    if (colon != NULL) {        /* there is a prefix end with : */
        size_t prefix_len = colon - prefix;
        /* check if prefix is one of recognized file system types */
        i = 0;
        while (fstype_prefix[i] != NULL) {
            if (!strncmp(prefix, fstype_prefix[i], prefix_len)) {
                known_prefix = 1;
                break;
            }
            i++;
        }
    }
    if (!known_prefix)  /* prefix is not meant for file system type */
        return 0;

    /* for a known prefix, check if it file system is enabled */
    i = 0;
    while (fstypes[i].fileops) {
        if (!strncasecmp(fstypes[i].prefix, prefix, strlen(fstypes[i].prefix))) {
            *fstype = fstypes[i].fstype;
            *ops = fstypes[i].fileops;
            break;
        }
        ++i;
    }
    if (-1 == *fstype) {
        *fstype = 0;
        /* --BEGIN ERROR HANDLING-- */
        *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                           myname, __LINE__, MPI_ERR_IO,
                                           "**iofstypeunsupported",
                                           "*iofstypeunsupported %s", prefix);
        /* --END ERROR HANDLING-- */
    }
    return 1;
}

/*@
    ADIO_ResolveFileType - determines file system type and operations from
                           file name string; this is a collective call

Input Parameters:
. comm - communicator across which collective open is performed
. filename - name of file (string)

Output Parameters:
. fstype - (pointer to) int holding file system type
. ops - (address of) pointer to table of valid file operations
. error_code - (pointer to) int holding error code

Return value:
. 1 - a known file system prefix name is found
. 0 - otherwise

Notes:
This code used to be in MPI_File_open(), but it has been moved into here in
order to clean things up.  The goal is to separate all this "did we compile
for this fs type" code from the MPI layer and also to introduce the ADIOI_Fns
tables in a reasonable way. -- Rob, 06/06/2001
@*/
int ADIO_ResolveFileType(MPI_Comm comm, const char *filename, int *fstype,
                         ADIOI_Fns ** ops, int *error_code)
{
    int myerrcode, file_system, min_code, max_code;
    int i, known_fstype = 0;
    static char myname[] = "ADIO_RESOLVEFILETYPE";
    *ops = 0;

    file_system = -1;
    if (filename == NULL) {
        *error_code = ADIOI_Err_create_code(myname, filename, ENOENT);
        return known_fstype;
    }

    /* check prefix for known file system types; just match via prefix and
     * assume everyone got the same thing.
     *
     * perhaps we should have this code go through the allreduce as well?
     */
    known_fstype = ADIO_FileSysType_prefix(filename, &file_system, ops, &myerrcode);
    if (myerrcode != MPI_SUCCESS) {
        *error_code = myerrcode;
        return known_fstype;
    }

    if (file_system == -1) {    /* filename contains no known file system prefix */
        int have_nfs_enabled = 0;
        *error_code = MPI_SUCCESS;
        /* no prefix; use system-dependent function call to determine type */
        /* Optimization: we can reduce the 'storm of stats' that result from
         * thousands of mpi processes determinig file type this way.  Let us
         * have just one process stat the file and broadcast the result to
         * everyone else.
         * - Note that we will not catch cases like
         * http://www.mcs.anl.gov/web-mail-archive/lists/mpich-discuss/2007/08/msg00042.html
         * (edit: now http://lists.mcs.anl.gov/pipermail/mpich-discuss/2007-August/002648.html)
         *
         * where file systems are not mounted or available on other processes,
         * but we'll catch those a few functions later in ADIO_Open
         * - Note that if we have NFS enabled, we might have a situation where,
         *   for example, /home/user/data.out is UFS on one process but NFS on
         *   others, so we won't perform this optimization if NFS is enabled.
         * - Another point: error codes and file system types are broadcast to
         *   all members of the communicator, so we get to skip the allreduce
         *   steps*/

#ifdef ROMIO_NFS
        have_nfs_enabled = 1;
#endif
        if (!have_nfs_enabled) {
            ADIO_FileSysType_fncall_scalable(comm, filename, &file_system, &myerrcode);
            if (myerrcode != MPI_SUCCESS) {
                *error_code = myerrcode;
                return known_fstype;
            }
        } else {
            ADIO_FileSysType_fncall(filename, &file_system, &myerrcode);

            /* the check for file system type will hang if any process got
             * an error in ADIO_FileSysType_fncall.  Processes encountering
             * an error will return early, before the collective file
             * system type check below.  This case could happen if a full
             * path exists on one node but not on others, and no prefix
             * like ufs: was provided.  see discussion at
             * http://www.mcs.anl.gov/web-mail-archive/lists/mpich-discuss/2007/08/msg00042.html
             * (edit: now
             * http://lists.mcs.anl.gov/pipermail/mpich-discuss/2007-August/002648.html)
             */

            MPI_Allreduce(&myerrcode, &max_code, 1, MPI_INT, MPI_MAX, comm);
            if (max_code != MPI_SUCCESS) {
                *error_code = max_code;
                return known_fstype;
            }
            /* ensure everyone came up with the same file system type */
            MPI_Allreduce(&file_system, &min_code, 1, MPI_INT, MPI_MIN, comm);
            if (min_code == ADIO_NFS)
                file_system = ADIO_NFS;
        }
    }

    if (!(*ops)) {
        for (i = 0; fstypes[i].fileops; i++)
            if (file_system == fstypes[i].fstype) {
                *ops = fstypes[i].fileops;
                break;
            }
    }
    if (!(*ops)) {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                                           myname, __LINE__, MPI_ERR_IO,
                                           "**iofstypeunsupported", 0);
        return known_fstype;
    }

    *error_code = MPI_SUCCESS;
    *fstype = file_system;
    return known_fstype;
}
