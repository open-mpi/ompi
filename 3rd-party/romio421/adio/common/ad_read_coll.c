/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "adio_extern.h"

#ifdef MPL_USE_DBG_LOGGING
#define RDCOLL_DEBUG 1
#endif
#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif

/* prototypes of functions used for collective reads only. */
static void ADIOI_Read_and_exch(ADIO_File fd, void *buf, MPI_Datatype
                                datatype, int nprocs,
                                int myrank, ADIOI_Access
                                * others_req, ADIO_Offset * offset_list,
                                ADIO_Offset * len_list, int contig_access_count,
                                ADIO_Offset
                                min_st_offset, ADIO_Offset fd_size,
                                ADIO_Offset * fd_start, ADIO_Offset * fd_end,
                                MPI_Aint * buf_idx, MPI_Status * status, int *error_code);
static void ADIOI_R_Exchange_data(ADIO_File fd, void *buf, ADIOI_Flatlist_node
                                  * flat_buf, ADIO_Offset * offset_list, ADIO_Offset
                                  * len_list, int *send_size, int *recv_size,
                                  int *count, int *start_pos,
                                  int *partial_send,
                                  int *recd_from_proc, int nprocs,
                                  int myrank, int
                                  buftype_is_contig, int contig_access_count,
                                  ADIO_Offset min_st_offset,
                                  ADIO_Offset fd_size,
                                  ADIO_Offset * fd_start, ADIO_Offset * fd_end,
                                  ADIOI_Access * others_req,
                                  int iter, MPI_Aint buftype_extent, MPI_Aint * buf_idx,
                                  MPI_Aint * actual_recved_bytes);
void ADIOI_Fill_user_buffer(ADIO_File fd, void *buf, ADIOI_Flatlist_node
                            * flat_buf, char **recv_buf, ADIO_Offset
                            * offset_list, ADIO_Offset * len_list,
                            unsigned *recv_size,
                            MPI_Request * requests, MPI_Status * statuses,
                            int *recd_from_proc, int nprocs,
                            int contig_access_count,
                            ADIO_Offset min_st_offset,
                            ADIO_Offset fd_size, ADIO_Offset * fd_start,
                            ADIO_Offset * fd_end, MPI_Aint buftype_extent);

#ifdef LUSTRE_RD_LOCK_AHEAD
/* There is no ad_lustre_rdcoll.c, so stub in some basic common code here
   If it's called for over non-lustre file systems, it will turn itself off
   when the ioctl fails. */
void ADIOI_LUSTRE_lock_ahead_ioctl(ADIO_File fd, int avail_cb_nodes, ADIO_Offset next_offset, int *error_code); /* ad_lustre_lock.c */
/* Handle lock ahead.  If this read is outside our locked region, lock it now */
/* The generic collective read code isn't reading the stripes quite how this lustre code (ad_lustre_lock.c) expects it. */
/* There are some comments in the debug code in ad_lustre_lock.c. */
#define ADIOI_LUSTRE_RD_LOCK_AHEAD(fd,cb_nodes,offset,error_code)                             \
if ((fd->file_system == ADIO_LUSTRE) && (fd->hints->fs_hints.lustre.lock_ahead_read)) {        \
    if (offset > fd->hints->fs_hints.lustre.lock_ahead_end_extent) {                          \
        ADIOI_LUSTRE_lock_ahead_ioctl(fd,cb_nodes,offset,error_code);                         \
    }                                                                                         \
    else if (offset < fd->hints->fs_hints.lustre.lock_ahead_start_extent) {                   \
        ADIOI_LUSTRE_lock_ahead_ioctl(fd,cb_nodes,offset,error_code);                         \
    }                                                                                         \
}
#else
#define ADIOI_LUSTRE_RD_LOCK_AHEAD(fd,cb_nodes,offset,error_code)
#endif

void ADIOI_GEN_ReadStridedColl(ADIO_File fd, void *buf, MPI_Aint count,
                               MPI_Datatype datatype, int file_ptr_type,
                               ADIO_Offset offset, ADIO_Status * status, int
                               *error_code)
{
/* Uses a generalized version of the extended two-phase method described
   in "An Extended Two-Phase Method for Accessing Sections of
   Out-of-Core Arrays", Rajeev Thakur and Alok Choudhary,
   Scientific Programming, (5)4:301--317, Winter 1996.
   http://www.mcs.anl.gov/home/thakur/ext2ph.ps */

    ADIOI_Access *my_req;
    /* array of nprocs structures, one for each other process in
     * whose file domain this process's request lies */

    ADIOI_Access *others_req;
    /* array of nprocs structures, one for each other process
     * whose request lies in this process's file domain. */

    int i, filetype_is_contig, nprocs, nprocs_for_coll, myrank;
    int contig_access_count = 0, interleave_count = 0, buftype_is_contig;
    int *count_my_req_per_proc, count_my_req_procs;
    int *count_others_req_per_proc, count_others_req_procs;
    ADIO_Offset start_offset, end_offset, orig_fp, fd_size, min_st_offset, off;
    ADIO_Offset *offset_list = NULL, *st_offsets = NULL, *fd_start = NULL,
        *fd_end = NULL, *end_offsets = NULL;
    ADIO_Offset *len_list = NULL;
    MPI_Aint *buf_idx = NULL;

    if (fd->hints->cb_pfr != ADIOI_HINT_DISABLE) {
        ADIOI_IOStridedColl(fd, buf, count, ADIOI_READ, datatype,
                            file_ptr_type, offset, status, error_code);
        return;
    }


    MPI_Comm_size(fd->comm, &nprocs);
    MPI_Comm_rank(fd->comm, &myrank);

    /* number of aggregators, cb_nodes, is stored in the hints */
    nprocs_for_coll = fd->hints->cb_nodes;
    orig_fp = fd->fp_ind;

    /* only check for interleaving if cb_read isn't disabled */
    if (fd->hints->cb_read != ADIOI_HINT_DISABLE) {
        /* For this process's request, calculate the list of offsets and
         * lengths in the file and determine the start and end offsets. */

        /* Note: end_offset points to the last byte-offset that will be accessed.
         * e.g., if start_offset=0 and 100 bytes to be read, end_offset=99 */

        ADIOI_Calc_my_off_len(fd, count, datatype, file_ptr_type, offset,
                              &offset_list, &len_list, &start_offset,
                              &end_offset, &contig_access_count);

#ifdef RDCOLL_DEBUG
        for (i = 0; i < contig_access_count; i++) {
            DBG_FPRINTF(stderr, "rank %d  off %lld  len %lld\n",
                        myrank, (long long) offset_list[i], (long long) len_list[i]);
        }
#endif

        /* each process communicates its start and end offsets to other
         * processes. The result is an array each of start and end offsets
         * stored in order of process rank. */

        st_offsets = (ADIO_Offset *) ADIOI_Malloc(nprocs * 2 * sizeof(ADIO_Offset));
        end_offsets = st_offsets + nprocs;

        MPI_Allgather(&start_offset, 1, ADIO_OFFSET, st_offsets, 1, ADIO_OFFSET, fd->comm);
        MPI_Allgather(&end_offset, 1, ADIO_OFFSET, end_offsets, 1, ADIO_OFFSET, fd->comm);

        /* are the accesses of different processes interleaved? */
        for (i = 1; i < nprocs; i++)
            if ((st_offsets[i] < end_offsets[i - 1]) && (st_offsets[i] <= end_offsets[i]))
                interleave_count++;
        /* This is a rudimentary check for interleaving, but should suffice
         * for the moment. */
    }

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);

    if (fd->hints->cb_read == ADIOI_HINT_DISABLE
        || (!interleave_count && (fd->hints->cb_read == ADIOI_HINT_AUTO))) {
        /* don't do aggregation */
        if (fd->hints->cb_read != ADIOI_HINT_DISABLE) {
            ADIOI_Free(offset_list);
            ADIOI_Free(st_offsets);
        }

        fd->fp_ind = orig_fp;
        ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);

        if (buftype_is_contig && filetype_is_contig) {
            if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
                off = fd->disp + (fd->etype_size) * offset;
                ADIO_ReadContig(fd, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
                                off, status, error_code);
            } else
                ADIO_ReadContig(fd, buf, count, datatype, ADIO_INDIVIDUAL, 0, status, error_code);
        } else
            ADIO_ReadStrided(fd, buf, count, datatype, file_ptr_type, offset, status, error_code);

        return;
    }

    /* We're going to perform aggregation of I/O.  Here we call
     * ADIOI_Calc_file_domains() to determine what processes will handle I/O
     * to what regions.  We pass nprocs_for_coll into this function; it is
     * used to determine how many processes will perform I/O, which is also
     * the number of regions into which the range of bytes must be divided.
     * These regions are called "file domains", or FDs.
     *
     * When this function returns, fd_start, fd_end, fd_size, and
     * min_st_offset will be filled in.  fd_start holds the starting byte
     * location for each file domain.  fd_end holds the ending byte location.
     * min_st_offset holds the minimum byte location that will be accessed.
     *
     * Both fd_start[] and fd_end[] are indexed by an aggregator number; this
     * needs to be mapped to an actual rank in the communicator later.
     *
     */
    ADIOI_Calc_file_domains(st_offsets, end_offsets, nprocs,
                            nprocs_for_coll, &min_st_offset,
                            &fd_start, &fd_end,
                            fd->hints->min_fdomain_size, &fd_size, fd->hints->striping_unit);

    /* calculate where the portions of the access requests of this process
     * are located in terms of the file domains.  this could be on the same
     * process or on other processes.  this function fills in:
     * count_my_req_procs - number of processes (including this one) for which
     *     this process has requests in their file domain
     * count_my_req_per_proc - count of requests for each process, indexed
     *     by rank of the process
     * my_req[] - array of data structures describing the requests to be
     *     performed by each process (including self).  indexed by rank.
     * buf_idx[] - array of locations into which data can be directly moved;
     *     this is only valid for contiguous buffer case
     */
    ADIOI_Calc_my_req(fd, offset_list, len_list, contig_access_count,
                      min_st_offset, fd_start, fd_end, fd_size,
                      nprocs, &count_my_req_procs, &count_my_req_per_proc, &my_req, &buf_idx);

    /* perform a collective communication in order to distribute the
     * data calculated above.  fills in the following:
     * count_others_req_procs - number of processes (including this
     *     one) which have requests in this process's file domain.
     * count_others_req_per_proc[] - number of separate contiguous
     *     requests from proc i lie in this process's file domain.
     */
    ADIOI_Calc_others_req(fd, count_my_req_procs,
                          count_my_req_per_proc, my_req,
                          nprocs, myrank, &count_others_req_procs, &count_others_req_per_proc,
                          &others_req);

    /* read data in sizes of no more than ADIOI_Coll_bufsize,
     * communicate, and fill user buf.
     */
    ADIOI_Read_and_exch(fd, buf, datatype, nprocs, myrank,
                        others_req, offset_list,
                        len_list, contig_access_count, min_st_offset,
                        fd_size, fd_start, fd_end, buf_idx, status, error_code);


    /* free all memory allocated for collective I/O */
    ADIOI_Free_my_req(nprocs, count_my_req_per_proc, my_req, buf_idx);
    ADIOI_Free_others_req(nprocs, count_others_req_per_proc, others_req);

    ADIOI_Free(offset_list);
    ADIOI_Free(st_offsets);
    ADIOI_Free(fd_start);

    fd->fp_sys_posn = -1;       /* set it to null. */
}

void ADIOI_Calc_my_off_len(ADIO_File fd, MPI_Aint bufcount, MPI_Datatype
                           datatype, int file_ptr_type, ADIO_Offset
                           offset, ADIO_Offset ** offset_list_ptr, ADIO_Offset
                           ** len_list_ptr, ADIO_Offset * start_offset_ptr,
                           ADIO_Offset * end_offset_ptr, int
                           *contig_access_count_ptr)
{
    MPI_Count filetype_size, etype_size;
    MPI_Count buftype_size;
    int i, j, k;
    ADIO_Offset i_offset;
    ADIO_Offset frd_size = 0, old_frd_size = 0;
    int st_index = 0;
    ADIO_Offset n_filetypes, etype_in_filetype;
    ADIO_Offset abs_off_in_filetype = 0;
    ADIO_Offset bufsize;
    ADIO_Offset sum, n_etypes_in_filetype, size_in_filetype;
    int contig_access_count, filetype_is_contig;
    ADIO_Offset *len_list;
    MPI_Aint filetype_extent, filetype_lb;
    ADIOI_Flatlist_node *flat_file;
    ADIO_Offset *offset_list, off, end_offset = 0, disp;

#ifdef AGGREGATION_PROFILE
    MPE_Log_event(5028, 0, NULL);
#endif

/* For this process's request, calculate the list of offsets and
   lengths in the file and determine the start and end offsets. */

    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);

    MPI_Type_size_x(fd->filetype, &filetype_size);
    MPI_Type_get_extent(fd->filetype, &filetype_lb, &filetype_extent);
    MPI_Type_size_x(datatype, &buftype_size);
    etype_size = fd->etype_size;

    if (!filetype_size) {
        *contig_access_count_ptr = 0;
        *offset_list_ptr = (ADIO_Offset *) ADIOI_Malloc(4 * sizeof(ADIO_Offset));
        *len_list_ptr = *offset_list_ptr + 2;
        /* 2 is for consistency. everywhere I malloc one more than needed */

        offset_list = *offset_list_ptr;
        len_list = *len_list_ptr;
        offset_list[0] = (file_ptr_type == ADIO_INDIVIDUAL) ? fd->fp_ind :
            fd->disp + (ADIO_Offset) etype_size *offset;
        len_list[0] = 0;
        *start_offset_ptr = offset_list[0];
        *end_offset_ptr = offset_list[0] + len_list[0] - 1;

        return;
    }

    if (filetype_is_contig) {
        *contig_access_count_ptr = 1;
        *offset_list_ptr = (ADIO_Offset *) ADIOI_Malloc(4 * sizeof(ADIO_Offset));
        *len_list_ptr = *offset_list_ptr + 2;
        /* 2 is for consistency. everywhere I malloc one more than needed */

        offset_list = *offset_list_ptr;
        len_list = *len_list_ptr;
        offset_list[0] = (file_ptr_type == ADIO_INDIVIDUAL) ? fd->fp_ind :
            fd->disp + (ADIO_Offset) etype_size *offset;
        len_list[0] = (ADIO_Offset) bufcount *(ADIO_Offset) buftype_size;
        *start_offset_ptr = offset_list[0];
        *end_offset_ptr = offset_list[0] + len_list[0] - 1;

        /* update file pointer */
        if (file_ptr_type == ADIO_INDIVIDUAL)
            fd->fp_ind = *end_offset_ptr + 1;
    }

    else {

        /* First calculate what size of offset_list and len_list to allocate */

        flat_file = ADIOI_Flatten_and_find(fd->filetype);
        disp = fd->disp;

#ifdef RDCOLL_DEBUG
        {
            int ii;
            DBG_FPRINTF(stderr, "flattened %3lld : ", (long long) flat_file->count);
            for (ii = 0; ii < flat_file->count; ii++) {
                DBG_FPRINTF(stderr, "%16lld:%-16lld", (long long) flat_file->indices[ii],
                            (long long) flat_file->blocklens[ii]);
            }
            DBG_FPRINTF(stderr, "\n");
        }
#endif
        if (file_ptr_type == ADIO_INDIVIDUAL) {
            /* Wei-keng reworked type processing to be a bit more efficient */
            offset = fd->fp_ind - disp;
            n_filetypes = (offset - flat_file->indices[0]) / filetype_extent;
            offset -= (ADIO_Offset) n_filetypes *filetype_extent;
            /* now offset is local to this extent */

            /* find the block where offset is located, skip blocklens[i]==0 */
            for (i = 0; i < flat_file->count; i++) {
                ADIO_Offset dist;
                if (flat_file->blocklens[i] == 0)
                    continue;
                dist = flat_file->indices[i] + flat_file->blocklens[i] - offset;
                /* frd_size is from offset to the end of block i */
                if (dist == 0) {
                    i++;
                    offset = flat_file->indices[i];
                    frd_size = flat_file->blocklens[i];
                    break;
                }
                if (dist > 0) {
                    frd_size = dist;
                    break;
                }
            }
            st_index = i;       /* starting index in flat_file->indices[] */
            offset += disp + (ADIO_Offset) n_filetypes *filetype_extent;
        } else {
            n_etypes_in_filetype = filetype_size / etype_size;
            n_filetypes = offset / n_etypes_in_filetype;
            etype_in_filetype = offset % n_etypes_in_filetype;
            size_in_filetype = etype_in_filetype * etype_size;

            sum = 0;
            for (i = 0; i < flat_file->count; i++) {
                sum += flat_file->blocklens[i];
                if (sum > size_in_filetype) {
                    st_index = i;
                    frd_size = sum - size_in_filetype;
                    abs_off_in_filetype = flat_file->indices[i] +
                        size_in_filetype - (sum - flat_file->blocklens[i]);
                    break;
                }
            }

            /* abs. offset in bytes in the file */
            offset = disp + n_filetypes * (ADIO_Offset) filetype_extent + abs_off_in_filetype;
        }

        /* calculate how much space to allocate for offset_list, len_list */

        old_frd_size = frd_size;
        contig_access_count = i_offset = 0;
        j = st_index;
        bufsize = (ADIO_Offset) buftype_size *(ADIO_Offset) bufcount;
        frd_size = MPL_MIN(frd_size, bufsize);
        while (i_offset < bufsize) {
            if (frd_size)
                contig_access_count++;
            i_offset += frd_size;
            j = (j + 1) % flat_file->count;
            frd_size = MPL_MIN(flat_file->blocklens[j], bufsize - i_offset);
        }

        /* allocate space for offset_list and len_list */

        *offset_list_ptr =
            (ADIO_Offset *) ADIOI_Malloc((contig_access_count + 1) * 2 * sizeof(ADIO_Offset));
        *len_list_ptr = *offset_list_ptr + (contig_access_count + 1);
        /* +1 to avoid a 0-size malloc */

        offset_list = *offset_list_ptr;
        len_list = *len_list_ptr;

        /* find start offset, end offset, and fill in offset_list and len_list */

        *start_offset_ptr = offset;     /* calculated above */

        i_offset = k = 0;
        j = st_index;
        off = offset;
        frd_size = MPL_MIN(old_frd_size, bufsize);
        while (i_offset < bufsize) {
            if (frd_size) {
                offset_list[k] = off;
                len_list[k] = frd_size;
                k++;
            }
            i_offset += frd_size;
            end_offset = off + frd_size - 1;

            /* Note: end_offset points to the last byte-offset that will be accessed.
             * e.g., if start_offset=0 and 100 bytes to be read, end_offset=99 */

            if (off + frd_size < disp + flat_file->indices[j] +
                flat_file->blocklens[j] + n_filetypes * (ADIO_Offset) filetype_extent) {
                off += frd_size;
                /* did not reach end of contiguous block in filetype.
                 * no more I/O needed. off is incremented by frd_size.
                 */
            } else {
                j = (j + 1) % flat_file->count;
                n_filetypes += (j == 0) ? 1 : 0;
                while (flat_file->blocklens[j] == 0) {
                    j = (j + 1) % flat_file->count;
                    n_filetypes += (j == 0) ? 1 : 0;
                    /* hit end of flattened filetype; start at beginning
                     * again */
                }
                off = disp + flat_file->indices[j] + n_filetypes * (ADIO_Offset) filetype_extent;
                frd_size = MPL_MIN(flat_file->blocklens[j], bufsize - i_offset);
            }
        }

        /* update file pointer */
        if (file_ptr_type == ADIO_INDIVIDUAL)
            fd->fp_ind = off;

        *contig_access_count_ptr = contig_access_count;
        *end_offset_ptr = end_offset;
    }
#ifdef AGGREGATION_PROFILE
    MPE_Log_event(5029, 0, NULL);
#endif
}

static void ADIOI_Read_and_exch(ADIO_File fd, void *buf, MPI_Datatype
                                datatype, int nprocs,
                                int myrank, ADIOI_Access
                                * others_req, ADIO_Offset * offset_list,
                                ADIO_Offset * len_list, int contig_access_count, ADIO_Offset
                                min_st_offset, ADIO_Offset fd_size,
                                ADIO_Offset * fd_start, ADIO_Offset * fd_end,
                                MPI_Aint * buf_idx, MPI_Status * status, int *error_code)
{
/* Read in sizes of no more than coll_bufsize, an info parameter.
   Send data to appropriate processes.
   Place recd. data in user buf.
   The idea is to reduce the amount of extra memory required for
   collective I/O. If all data were read all at once, which is much
   easier, it would require temp space more than the size of user_buf,
   which is often unacceptable. For example, to read a distributed
   array from a file, where each local array is 8Mbytes, requiring
   at least another 8Mbytes of temp space is unacceptable. */

    int i, j, m, ntimes, max_ntimes, buftype_is_contig;
    ADIO_Offset st_loc = -1, end_loc = -1, off, done, real_off;
    char *read_buf = NULL, *tmp_buf;
    int *curr_offlen_ptr, *count, *send_size, *recv_size;
    int *partial_send, *recd_from_proc, *start_pos;
    /* Not convinced end_loc-st_loc couldn't be > int, so make these offsets */
    ADIO_Offset real_size, size, for_curr_iter, for_next_iter;
    int rank;
    ADIOI_Flatlist_node *flat_buf = NULL;
    MPI_Aint lb, buftype_extent;
    MPI_Aint coll_bufsize;
    MPI_Aint actual_recved_bytes = 0;

    *error_code = MPI_SUCCESS;  /* changed below if error */
    /* only I/O errors are currently reported */

/* calculate the number of reads of size coll_bufsize
   to be done by each process and the max among all processes.
   That gives the no. of communication phases as well.
   coll_bufsize is obtained from the hints object. */

    coll_bufsize = fd->hints->cb_buffer_size;

    /* grab some initial values for st_loc and end_loc */
    for (i = 0; i < nprocs; i++) {
        if (others_req[i].count) {
            st_loc = others_req[i].offsets[0];
            end_loc = others_req[i].offsets[0];
            break;
        }
    }

    /* now find the real values */
    for (i = 0; i < nprocs; i++)
        for (j = 0; j < others_req[i].count; j++) {
            st_loc = MPL_MIN(st_loc, others_req[i].offsets[j]);
            end_loc = MPL_MAX(end_loc, (others_req[i].offsets[j]
                                        + others_req[i].lens[j] - 1));
        }

    /* calculate ntimes, the number of times this process must perform I/O
     * operations in order to complete all the requests it has received.
     * the need for multiple I/O operations comes from the restriction that
     * we only use coll_bufsize bytes of memory for internal buffering.
     */
    if ((st_loc == -1) && (end_loc == -1)) {
        /* this process does no I/O. */
        ntimes = 0;
    } else {
        /* ntimes=ceiling_div(end_loc - st_loc + 1, coll_bufsize) */
        ntimes = (int) ((end_loc - st_loc + coll_bufsize) / coll_bufsize);
    }

    MPI_Allreduce(&ntimes, &max_ntimes, 1, MPI_INT, MPI_MAX, fd->comm);

    read_buf = fd->io_buf;      /* Allocated at open time */

    curr_offlen_ptr = (int *) ADIOI_Calloc(nprocs * 7, sizeof(int));
    /* its use is explained below. calloc initializes to 0. */

    count = curr_offlen_ptr + nprocs;
    /* to store count of how many off-len pairs per proc are satisfied
     * in an iteration. */

    partial_send = count + nprocs;
    /* if only a portion of the last off-len pair is sent to a process
     * in a particular iteration, the length sent is stored here.
     * calloc initializes to 0. */

    send_size = partial_send + nprocs;
    /* total size of data to be sent to each proc. in an iteration */

    recv_size = send_size + nprocs;
    /* total size of data to be recd. from each proc. in an iteration.
     * Of size nprocs so that I can use MPI_Alltoall later. */

    recd_from_proc = recv_size + nprocs;
    /* amount of data recd. so far from each proc. Used in
     * ADIOI_Fill_user_buffer. initialized to 0 here. */

    start_pos = recd_from_proc + nprocs;
    /* used to store the starting value of curr_offlen_ptr[i] in
     * this iteration */

    ADIOI_Datatype_iscontig(datatype, &buftype_is_contig);
    if (!buftype_is_contig) {
        flat_buf = ADIOI_Flatten_and_find(datatype);
    }
    MPI_Type_get_extent(datatype, &lb, &buftype_extent);

    done = 0;
    off = st_loc;
    for_curr_iter = for_next_iter = 0;

    MPI_Comm_rank(fd->comm, &rank);

    for (m = 0; m < ntimes; m++) {
        /* read buf of size coll_bufsize (or less) */
        /* go through all others_req and check if any are satisfied
         * by the current read */

        /* since MPI guarantees that displacements in filetypes are in
         * monotonically nondecreasing order, I can maintain a pointer
         * (curr_offlen_ptr) to
         * current off-len pair for each process in others_req and scan
         * further only from there. There is still a problem of filetypes
         * such as:  (1, 2, 3 are not process nos. They are just numbers for
         * three chunks of data, specified by a filetype.)
         *
         * 1  -------!--
         * 2    -----!----
         * 3       --!-----
         *
         * where ! indicates where the current read_size limitation cuts
         * through the filetype.  I resolve this by reading up to !, but
         * filling the communication buffer only for 1. I copy the portion
         * left over for 2 into a tmp_buf for use in the next
         * iteration. i.e., 2 and 3 will be satisfied in the next
         * iteration. This simplifies filling in the user's buf at the
         * other end, as only one off-len pair with incomplete data
         * will be sent. I also don't need to send the individual
         * offsets and lens along with the data, as the data is being
         * sent in a particular order. */

        /* off = start offset in the file for the data actually read in
         * this iteration
         * size = size of data read corresponding to off
         * real_off = off minus whatever data was retained in memory from
         * previous iteration for cases like 2, 3 illustrated above
         * real_size = size plus the extra corresponding to real_off
         * req_off = off in file for a particular contiguous request
         * minus what was satisfied in previous iteration
         * req_size = size corresponding to req_off */

        size = MPL_MIN(coll_bufsize, end_loc - st_loc + 1 - done);
        bool flag = false;
        for (i = 0; i < nprocs; i++) {
            if (others_req[i].count) {
                for (j = curr_offlen_ptr[i]; j < others_req[i].count; j++) {
                    ADIO_Offset req_off;
                    if (partial_send[i]) {
                        req_off = others_req[i].offsets[j] + partial_send[i];
                    } else {
                        req_off = others_req[i].offsets[j];
                    }
                    if (req_off < off + size) {
                        flag = true;
                    }
                }
            }
        }
        if (flag) {
            MPI_Status read_status;
            ADIOI_Assert(size == (int) size);
            ADIOI_LUSTRE_RD_LOCK_AHEAD(fd, fd->hints->cb_nodes, off, error_code);
            ADIO_ReadContig(fd, read_buf + for_curr_iter, (int) size, MPI_BYTE,
                            ADIO_EXPLICIT_OFFSET, off, &read_status, error_code);
            if (*error_code != MPI_SUCCESS) {
                /* TODO: proper error return */
                return;
            }
            int actual_size;
            MPI_Get_count(&read_status, MPI_BYTE, &actual_size);
            if (actual_size < size) {
                size = actual_size;
                /* TODO: need abort the further rounds */
            }
        }

        real_off = off - for_curr_iter;
        real_size = size + for_curr_iter;

        for (i = 0; i < nprocs; i++)
            count[i] = send_size[i] = 0;
        for_next_iter = 0;

        for (i = 0; i < nprocs; i++) {
#ifdef RDCOLL_DEBUG
            DBG_FPRINTF(stderr, "rank %d, i %d, others_count %d\n", rank, i, others_req[i].count);
#endif
            if (others_req[i].count) {
                start_pos[i] = curr_offlen_ptr[i];
                for (j = curr_offlen_ptr[i]; j < others_req[i].count; j++) {
                    ADIO_Offset req_off;
                    MPI_Aint req_len;
                    if (partial_send[i]) {
                        /* this request may have been partially
                         * satisfied in the previous iteration. */
                        req_off = others_req[i].offsets[j] + partial_send[i];
                        req_len = others_req[i].lens[j] - partial_send[i];
                        partial_send[i] = 0;
                        /* modify the off-len pair to reflect this change */
                        others_req[i].offsets[j] = req_off;
                        others_req[i].lens[j] = req_len;
                    } else {
                        req_off = others_req[i].offsets[j];
                        req_len = others_req[i].lens[j];
                    }
                    if (req_off < real_off + real_size) {
                        count[i]++;
                        ADIOI_Assert((((ADIO_Offset) (uintptr_t) read_buf) + req_off - real_off) ==
                                     (ADIO_Offset) (uintptr_t) (read_buf + req_off - real_off));
                        MPI_Get_address(read_buf + req_off - real_off,
                                        &(others_req[i].mem_ptrs[j]));
                        ADIOI_Assert((real_off + real_size - req_off) ==
                                     (int) (real_off + real_size - req_off));
                        send_size[i] += (int) (MPL_MIN(real_off + real_size - req_off, req_len));

                        if (real_off + real_size - req_off < req_len) {
                            partial_send[i] = (int) (real_off + real_size - req_off);
                            if ((j + 1 < others_req[i].count) &&
                                (others_req[i].offsets[j + 1] < real_off + real_size)) {
                                /* this is the case illustrated in the
                                 * figure above. */
                                for_next_iter = MPL_MAX(for_next_iter,
                                                        real_off + real_size -
                                                        others_req[i].offsets[j + 1]);
                                /* max because it must cover requests
                                 * from different processes */
                            }
                            break;
                        }
                    } else
                        break;
                }
                curr_offlen_ptr[i] = j;
            }
        }

        for_curr_iter = for_next_iter;

        MPI_Aint recved_bytes = 0;
        ADIOI_R_Exchange_data(fd, buf, flat_buf, offset_list, len_list,
                              send_size, recv_size, count,
                              start_pos, partial_send, recd_from_proc, nprocs,
                              myrank,
                              buftype_is_contig, contig_access_count,
                              min_st_offset, fd_size, fd_start, fd_end,
                              others_req, m, buftype_extent, buf_idx, &recved_bytes);
        actual_recved_bytes += recved_bytes;


        if (for_next_iter) {
            tmp_buf = (char *) ADIOI_Malloc(for_next_iter);
            ADIOI_Assert((((ADIO_Offset) (uintptr_t) read_buf) + real_size - for_next_iter) ==
                         (ADIO_Offset) (uintptr_t) (read_buf + real_size - for_next_iter));
            ADIOI_Assert((for_next_iter + coll_bufsize) == (size_t) (for_next_iter + coll_bufsize));
            memcpy(tmp_buf, read_buf + real_size - for_next_iter, for_next_iter);
            ADIOI_Free(fd->io_buf);
            fd->io_buf = (char *) ADIOI_Malloc(for_next_iter + coll_bufsize);
            memcpy(fd->io_buf, tmp_buf, for_next_iter);
            read_buf = fd->io_buf;
            ADIOI_Free(tmp_buf);
        }

        off += size;
        done += size;
    }

    for (i = 0; i < nprocs; i++)
        count[i] = send_size[i] = 0;
    for (m = ntimes; m < max_ntimes; m++) {
        /* nothing to send, but check for recv. */
        MPI_Aint recved_bytes = 0;
        ADIOI_R_Exchange_data(fd, buf, flat_buf, offset_list, len_list,
                              send_size, recv_size, count,
                              start_pos, partial_send, recd_from_proc, nprocs,
                              myrank,
                              buftype_is_contig, contig_access_count,
                              min_st_offset, fd_size, fd_start, fd_end,
                              others_req, m, buftype_extent, buf_idx, &recved_bytes);
        actual_recved_bytes += recved_bytes;
    }

#ifdef HAVE_STATUS_SET_BYTES
    MPIR_Status_set_bytes(status, datatype, actual_recved_bytes);
#endif

    ADIOI_Free(curr_offlen_ptr);
}

static void ADIOI_R_Exchange_data(ADIO_File fd, void *buf, ADIOI_Flatlist_node
                                  * flat_buf, ADIO_Offset * offset_list, ADIO_Offset
                                  * len_list, int *send_size, int *recv_size,
                                  int *count, int *start_pos, int *partial_send,
                                  int *recd_from_proc, int nprocs,
                                  int myrank, int
                                  buftype_is_contig, int contig_access_count,
                                  ADIO_Offset min_st_offset, ADIO_Offset fd_size,
                                  ADIO_Offset * fd_start, ADIO_Offset * fd_end,
                                  ADIOI_Access * others_req,
                                  int iter, MPI_Aint buftype_extent, MPI_Aint * buf_idx,
                                  MPI_Aint * actual_recved_bytes)
{
    int i, j, k = 0, nprocs_recv, nprocs_send;
    char **recv_buf = NULL;
    size_t memLen;
    MPI_Request *requests;
    MPI_Datatype send_type;
    MPI_Status *statuses;

/* exchange send_size info so that each process knows how much to
   receive from whom and how much memory to allocate. */

    MPI_Alltoall(send_size, 1, MPI_INT, recv_size, 1, MPI_INT, fd->comm);

    nprocs_recv = 0;
    nprocs_send = 0;
    memLen = 0;
    for (i = 0; i < nprocs; i++) {
        memLen += recv_size[i];
        if (recv_size[i])
            nprocs_recv++;
        if (send_size[i])
            nprocs_send++;
    }

    requests = (MPI_Request *)
        ADIOI_Malloc((nprocs_send + nprocs_recv + 1) * sizeof(MPI_Request));
/* +1 to avoid a 0-size malloc */

/* post recvs. if buftype_is_contig, data can be directly recd. into
   user buf at location given by buf_idx. else use recv_buf. */

#ifdef AGGREGATION_PROFILE
    MPE_Log_event(5032, 0, NULL);
#endif

    if (buftype_is_contig) {
        j = 0;
        for (i = 0; i < nprocs; i++) {
            if (recv_size[i]) {
                MPI_Irecv(((char *) buf) + buf_idx[i], recv_size[i],
                          MPI_BYTE, i, ADIOI_COLL_TAG(i, iter), fd->comm, requests + j);
                j++;
                buf_idx[i] += recv_size[i];
            }
        }
    } else {
        /* allocate memory for recv_buf and post receives */
        recv_buf = (char **) ADIOI_Malloc(nprocs * sizeof(char *));
        recv_buf[0] = (char *) ADIOI_Malloc(memLen);
        for (i = 1; i < nprocs; i++)
            recv_buf[i] = recv_buf[i - 1] + recv_size[i - 1];

        j = 0;
        for (i = 0; i < nprocs; i++) {
            if (recv_size[i]) {
                MPI_Irecv(recv_buf[i], recv_size[i], MPI_BYTE, i,
                          ADIOI_COLL_TAG(i, iter), fd->comm, requests + j);
                j++;
#ifdef RDCOLL_DEBUG
                DBG_FPRINTF(stderr, "node %d, recv_size %d, tag %d \n",
                            myrank, recv_size[i], ADIOI_COLL_TAG(i, iter));
#endif
            }
        }
    }

/* create derived datatypes and send data */

    j = 0;
    for (i = 0; i < nprocs; i++) {
        if (send_size[i]) {
            /* take care if the last off-len pair is a partial send */
            ADIO_Offset tmp = 0;
            if (partial_send[i]) {
                k = start_pos[i] + count[i] - 1;
                tmp = others_req[i].lens[k];
                others_req[i].lens[k] = partial_send[i];
            }
            ADIOI_Type_create_hindexed_x(count[i],
                                         &(others_req[i].lens[start_pos[i]]),
                                         &(others_req[i].mem_ptrs[start_pos[i]]),
                                         MPI_BYTE, &send_type);
            /* absolute displacement; use MPI_BOTTOM in send */
            MPI_Type_commit(&send_type);
            MPI_Isend(MPI_BOTTOM, 1, send_type, i, ADIOI_COLL_TAG(i, iter),
                      fd->comm, requests + nprocs_recv + j);
            MPI_Type_free(&send_type);
            if (partial_send[i])
                others_req[i].lens[k] = tmp;
            j++;
        }
    }

    /* +1 to avoid a 0-size malloc */
    statuses = (MPI_Status *) ADIOI_Malloc((nprocs_send + nprocs_recv + 1) * sizeof(MPI_Status));

    /* wait on the receives */
    if (nprocs_recv) {
#ifdef NEEDS_MPI_TEST
        j = 0;
        while (!j)
            MPI_Testall(nprocs_recv, requests, &j, statuses);
#else
        MPI_Waitall(nprocs_recv, requests, statuses);
#endif
        *actual_recved_bytes = 0;
        j = 0;
        for (i = 0; i < nprocs; i++) {
            if (recv_size[i]) {
                int count_recved;
                MPI_Get_count(&statuses[j], MPI_BYTE, &count_recved);
                *actual_recved_bytes += count_recved;
                j++;
            }
        }

        /* if noncontiguous, to the copies from the recv buffers */
        if (!buftype_is_contig)
            ADIOI_Fill_user_buffer(fd, buf, flat_buf, recv_buf,
                                   offset_list, len_list, (unsigned *) recv_size,
                                   requests, statuses, recd_from_proc,
                                   nprocs, contig_access_count,
                                   min_st_offset, fd_size, fd_start, fd_end, buftype_extent);
    }

    /* wait on the sends */
#ifdef MPI_STATUSES_IGNORE
    MPI_Waitall(nprocs_send, requests + nprocs_recv, MPI_STATUSES_IGNORE);
#else
    MPI_Waitall(nprocs_send, requests + nprocs_recv, statuses + nprocs_recv);
#endif

    ADIOI_Free(statuses);
    ADIOI_Free(requests);

    if (!buftype_is_contig) {
        ADIOI_Free(recv_buf[0]);
        ADIOI_Free(recv_buf);
    }
#ifdef AGGREGATION_PROFILE
    MPE_Log_event(5033, 0, NULL);
#endif
}

#define ADIOI_BUF_INCR                                                  \
    {                                                                   \
        while (buf_incr) {                                              \
            size_in_buf = MPL_MIN(buf_incr, flat_buf_sz);               \
            user_buf_idx += size_in_buf;                                \
            flat_buf_sz -= size_in_buf;                                 \
            if (!flat_buf_sz) {                                         \
                if (flat_buf_idx < (flat_buf->count - 1)) flat_buf_idx++; \
                else {                                                  \
                    flat_buf_idx = 0;                                   \
                    n_buftypes++;                                       \
                }                                                       \
                user_buf_idx = flat_buf->indices[flat_buf_idx] +        \
                    (ADIO_Offset)n_buftypes*(ADIO_Offset)buftype_extent; \
                flat_buf_sz = flat_buf->blocklens[flat_buf_idx];        \
            }                                                           \
            buf_incr -= size_in_buf;                                    \
        }                                                               \
    }


#define ADIOI_BUF_COPY                                                  \
    {                                                                   \
        while (size) {                                                  \
            size_in_buf = MPL_MIN(size, flat_buf_sz);                   \
            ADIOI_Assert((((ADIO_Offset)(uintptr_t)buf) + user_buf_idx) == (ADIO_Offset)(uintptr_t)((uintptr_t)buf + user_buf_idx)); \
            ADIOI_Assert(size_in_buf == (size_t)size_in_buf);           \
            memcpy(((char *) buf) + user_buf_idx,                       \
                   &(recv_buf[p][recv_buf_idx[p]]), size_in_buf);       \
            recv_buf_idx[p] += size_in_buf; /* already tested (size_t)size_in_buf*/ \
            user_buf_idx += size_in_buf;                                \
            flat_buf_sz -= size_in_buf;                                 \
            if (!flat_buf_sz) {                                         \
                if (flat_buf_idx < (flat_buf->count - 1)) flat_buf_idx++; \
                else {                                                  \
                    flat_buf_idx = 0;                                   \
                    n_buftypes++;                                       \
                }                                                       \
                user_buf_idx = flat_buf->indices[flat_buf_idx] +        \
                    (ADIO_Offset)n_buftypes*(ADIO_Offset)buftype_extent; \
                flat_buf_sz = flat_buf->blocklens[flat_buf_idx];        \
            }                                                           \
            size -= size_in_buf;                                        \
            buf_incr -= size_in_buf;                                    \
        }                                                               \
        ADIOI_BUF_INCR                                                  \
    }

void ADIOI_Fill_user_buffer(ADIO_File fd, void *buf, ADIOI_Flatlist_node
                            * flat_buf, char **recv_buf, ADIO_Offset
                            * offset_list, ADIO_Offset * len_list,
                            unsigned *recv_size,
                            MPI_Request * requests, MPI_Status * statuses,
                            int *recd_from_proc, int nprocs,
                            int contig_access_count,
                            ADIO_Offset min_st_offset,
                            ADIO_Offset fd_size, ADIO_Offset * fd_start,
                            ADIO_Offset * fd_end, MPI_Aint buftype_extent)
{

/* this function is only called if buftype is not contig */

    int i, p, flat_buf_idx;
    ADIO_Offset flat_buf_sz, size_in_buf, buf_incr, size;
    int n_buftypes;
    ADIO_Offset off, len, rem_len, user_buf_idx;
    /* Not sure unsigned is necessary, but it makes the math safer */
    unsigned *curr_from_proc, *done_from_proc, *recv_buf_idx;

    MPL_UNREFERENCED_ARG(requests);
    MPL_UNREFERENCED_ARG(statuses);

/*  curr_from_proc[p] = amount of data recd from proc. p that has already
                        been accounted for so far
    done_from_proc[p] = amount of data already recd from proc. p and
                        filled into user buffer in previous iterations
    user_buf_idx = current location in user buffer
    recv_buf_idx[p] = current location in recv_buf of proc. p  */
    curr_from_proc = (unsigned *) ADIOI_Malloc(nprocs * 3 * sizeof(unsigned));
    done_from_proc = curr_from_proc + nprocs;
    recv_buf_idx = done_from_proc + nprocs;

    for (i = 0; i < nprocs; i++) {
        recv_buf_idx[i] = curr_from_proc[i] = 0;
        done_from_proc[i] = recd_from_proc[i];
    }

    user_buf_idx = flat_buf->indices[0];
    flat_buf_idx = 0;
    n_buftypes = 0;
    flat_buf_sz = flat_buf->blocklens[0];

    /* flat_buf_idx = current index into flattened buftype
     * flat_buf_sz = size of current contiguous component in
     * flattened buf */

    for (i = 0; i < contig_access_count; i++) {
        off = offset_list[i];
        rem_len = len_list[i];

        /* this request may span the file domains of more than one process */
        while (rem_len != 0) {
            len = rem_len;
            /* NOTE: len value is modified by ADIOI_Calc_aggregator() to be no
             * longer than the single region that processor "p" is responsible
             * for.
             */
            p = ADIOI_Calc_aggregator(fd, off, min_st_offset, &len, fd_size, fd_start, fd_end);

            if (recv_buf_idx[p] < recv_size[p]) {
                if (curr_from_proc[p] + len > done_from_proc[p]) {
                    if (done_from_proc[p] > curr_from_proc[p]) {
                        size = MPL_MIN(curr_from_proc[p] + len -
                                       done_from_proc[p], recv_size[p] - recv_buf_idx[p]);
                        buf_incr = done_from_proc[p] - curr_from_proc[p];
                        ADIOI_BUF_INCR buf_incr = curr_from_proc[p] + len - done_from_proc[p];
                        ADIOI_Assert((done_from_proc[p] + size) ==
                                     (unsigned) ((ADIO_Offset) done_from_proc[p] + size));
                        curr_from_proc[p] = done_from_proc[p] + size;
                    ADIOI_BUF_COPY} else {
                        size = MPL_MIN(len, recv_size[p] - recv_buf_idx[p]);
                        buf_incr = len;
                        ADIOI_Assert((curr_from_proc[p] + size) ==
                                     (unsigned) ((ADIO_Offset) curr_from_proc[p] + size));
                        curr_from_proc[p] += (unsigned) size;
                    ADIOI_BUF_COPY}
                } else {
                    ADIOI_Assert((curr_from_proc[p] + len) ==
                                 (unsigned) ((ADIO_Offset) curr_from_proc[p] + len));
                    curr_from_proc[p] += (unsigned) len;
                    buf_incr = len;
                ADIOI_BUF_INCR}
            } else {
                buf_incr = len;
            ADIOI_BUF_INCR}
            off += len;
            rem_len -= len;
        }
    }
    for (i = 0; i < nprocs; i++)
        if (recv_size[i])
            recd_from_proc[i] = curr_from_proc[i];

    ADIOI_Free(curr_from_proc);
}
