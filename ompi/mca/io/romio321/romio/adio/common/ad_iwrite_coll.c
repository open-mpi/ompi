/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2014 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "mpiu_greq.h"
#include "mpioimpl.h"

#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif

/* ADIOI_GEN_IwriteStridedColl */
struct ADIOI_GEN_IwriteStridedColl_vars {
    /* requests */
    MPI_Request req_offset[2]; /* ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL */
    MPI_Request req_ind_io;    /* ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL_INDIO */
    MPI_Request req_err;       /* ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL_BCAST */

    /* parameters */
    ADIO_File fd;
    const void *buf;
    int count;
    MPI_Datatype datatype;
    int file_ptr_type;
    ADIO_Offset offset;

    /* stack variables */
    ADIOI_Access *my_req;
    /* array of nprocs access structures, one for each other process in
       whose file domain this process's request lies */

    ADIOI_Access *others_req;
    /* array of nprocs access structures, one for each other process
       whose request lies in this process's file domain. */

    int nprocs;
    int nprocs_for_coll;
    int myrank;
    int contig_access_count;
    int interleave_count;
    int buftype_is_contig;
    int *count_my_req_per_proc;
    int count_my_req_procs;
    int count_others_req_procs;
    ADIO_Offset start_offset;
    ADIO_Offset end_offset;
    ADIO_Offset orig_fp;
    ADIO_Offset fd_size;
    ADIO_Offset min_st_offset;
    ADIO_Offset *offset_list;
    ADIO_Offset *st_offsets;
    ADIO_Offset *fd_start;
    ADIO_Offset *fd_end;
    ADIO_Offset *end_offsets;
    int *buf_idx;
    ADIO_Offset *len_list;
    int old_error;
    int tmp_error;
    int error_code;
};

/* ADIOI_Iexch_and_write */
struct ADIOI_Iexch_and_write_vars {
    /* requests */
    MPI_Request req1;       /* ADIOI_IWC_STATE_IEXCH_AND_WRITE */
    MPI_Request req3;       /* ADIOI_IWC_STATE_IEXCH_AND_WRITE_L1_BODY */

    /* parameters */
    ADIO_File fd;
    void *buf;
    MPI_Datatype datatype;
    int nprocs;
    int myrank;
    ADIOI_Access *others_req;
    ADIO_Offset *offset_list;
    ADIO_Offset *len_list;
    int contig_access_count;
    ADIO_Offset min_st_offset;
    ADIO_Offset fd_size;
    ADIO_Offset *fd_start;
    ADIO_Offset *fd_end;
    int *buf_idx;

    /* stack variables */
    /* Not convinced end_loc-st_loc couldn't be > int, so make these offsets*/
    ADIO_Offset size;
    int hole;
    int m;
    int ntimes;
    int max_ntimes;
    int buftype_is_contig;
    ADIO_Offset st_loc;
    ADIO_Offset end_loc;
    ADIO_Offset off;
    ADIO_Offset done;
    char *write_buf;
    int *curr_offlen_ptr;
    int *count;
    int *send_size;
    int *recv_size;
    int *partial_recv;
    int *sent_to_proc;
    int *start_pos;
    int *send_buf_idx;
    int *curr_to_proc;
    int *done_to_proc;
    ADIOI_Flatlist_node *flat_buf;
    MPI_Aint buftype_extent;
    int coll_bufsize;

    /* next function to be called */
    void (*next_fn)(ADIOI_NBC_Request *, int *);
};

/* ADIOI_W_Iexchange_data */
struct ADIOI_W_Iexchange_data_vars {
    /* requests */
    MPI_Request req1;   /* ADIOI_IWC_STATE_W_IEXCHANGE_DATA */
    MPI_Request req2;   /* ADIOI_IWC_STATE_W_IEXCHANGE_DATA_HOLE */
    MPI_Request *req3;  /* ADIOI_IWC_STATE_W_IEXCHANGE_DATA_SEND */

    /* parameters */
    ADIO_File fd;
    void *buf;
    char *write_buf;
    ADIOI_Flatlist_node *flat_buf;
    ADIO_Offset *offset_list;
    ADIO_Offset *len_list;
    int *send_size;
    int *recv_size;
    ADIO_Offset off;
    int size;
    int *count;
    int *start_pos;
    int *partial_recv;
    int *sent_to_proc;
    int nprocs;
    int myrank;
    int buftype_is_contig;
    int contig_access_count;
    ADIO_Offset min_st_offset;
    ADIO_Offset fd_size;
    ADIO_Offset *fd_start;
    ADIO_Offset *fd_end;
    ADIOI_Access *others_req;
    int *send_buf_idx;
    int *curr_to_proc;
    int *done_to_proc;
    int *hole;
    int iter;
    MPI_Aint buftype_extent;
    int *buf_idx;

    /* stack variables */
    int nprocs_recv;
    int nprocs_send;
    int err;
    char **send_buf;
    MPI_Request *requests;
    MPI_Request *send_req;
    MPI_Datatype *recv_types;
    int sum;
    ADIO_Offset *srt_off;

    /* next function to be called */
    void (*next_fn)(ADIOI_NBC_Request *, int *);
};


void ADIOI_Fill_send_buffer(ADIO_File fd, void *buf, ADIOI_Flatlist_node
                           *flat_buf, char **send_buf, ADIO_Offset
                           *offset_list, ADIO_Offset *len_list, int *send_size,
                           MPI_Request *requests, int *sent_to_proc,
                           int nprocs, int myrank,
                           int contig_access_count, ADIO_Offset
                           min_st_offset, ADIO_Offset fd_size,
                           ADIO_Offset *fd_start, ADIO_Offset *fd_end,
                           int *send_buf_idx, int *curr_to_proc,
                           int *done_to_proc, int iter,
                           MPI_Aint buftype_extent);
void ADIOI_Heap_merge(ADIOI_Access *others_req, int *count,
                      ADIO_Offset *srt_off, int *srt_len, int *start_pos,
                      int nprocs, int nprocs_recv, int total_elements);


/* prototypes of functions used for nonblocking collective writes only. */
static void ADIOI_GEN_IwriteStridedColl_inter(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IwriteStridedColl_indio(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IwriteStridedColl_exch(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IwriteStridedColl_bcast(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IwriteStridedColl_free(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IwriteStridedColl_fini(ADIOI_NBC_Request *, int *);

static void ADIOI_Iexch_and_write(ADIOI_NBC_Request *, int *);
static void ADIOI_Iexch_and_write_l1_begin(ADIOI_NBC_Request *, int *);
static void ADIOI_Iexch_and_write_l1_body(ADIOI_NBC_Request *, int *);
static void ADIOI_Iexch_and_write_l1_end(ADIOI_NBC_Request *, int *);
static void ADIOI_Iexch_and_write_reset(ADIOI_NBC_Request *, int *);
static void ADIOI_Iexch_and_write_l2_begin(ADIOI_NBC_Request *, int *);
static void ADIOI_Iexch_and_write_l2_end(ADIOI_NBC_Request *, int *);
static void ADIOI_Iexch_and_write_fini(ADIOI_NBC_Request *, int *);

static void ADIOI_W_Iexchange_data(ADIOI_NBC_Request *, int *);
static void ADIOI_W_Iexchange_data_hole(ADIOI_NBC_Request *, int *);
static void ADIOI_W_Iexchange_data_send(ADIOI_NBC_Request *, int *);
static void ADIOI_W_Iexchange_data_wait(ADIOI_NBC_Request *, int *);
static void ADIOI_W_Iexchange_data_fini(ADIOI_NBC_Request *, int *);

static MPIX_Grequest_class ADIOI_GEN_greq_class = 0;
static int ADIOI_GEN_iwc_query_fn(void *extra_state, MPI_Status *status);
static int ADIOI_GEN_iwc_free_fn(void *extra_state);
static int ADIOI_GEN_iwc_poll_fn(void *extra_state, MPI_Status *status);
static int ADIOI_GEN_iwc_wait_fn(int count, void **array_of_states,
                                 double timeout, MPI_Status *status);


/* Non-blocking version of ADIOI_GEN_WriteStridedColl() */
void ADIOI_GEN_IwriteStridedColl(ADIO_File fd, const void *buf, int count,
                       MPI_Datatype datatype, int file_ptr_type,
                       ADIO_Offset offset, MPI_Request *request,
                       int *error_code)
{
    /* Uses a generalized version of the extended two-phase method described
       in "An Extended Two-Phase Method for Accessing Sections of
       Out-of-Core Arrays", Rajeev Thakur and Alok Choudhary,
       Scientific Programming, (5)4:301--317, Winter 1996.
       http://www.mcs.anl.gov/home/thakur/ext2ph.ps */

    ADIOI_NBC_Request *nbc_req = NULL;
    ADIOI_GEN_IwriteStridedColl_vars *vars = NULL;
    int nprocs, myrank;

#if 0
    /* FIXME: need an implementation of ADIOI_IOIstridedColl */
    if (fd->hints->cb_pfr != ADIOI_HINT_DISABLE) {
        /* Cast away const'ness as the below function is used for read
         * and write */
        ADIOI_IOIstridedColl(fd, (char *) buf, count, ADIOI_WRITE, datatype,
                             file_ptr_type, offset, request, error_code);
        return;
    }
#endif

    /* top-level struct keeping the status of function progress */
    nbc_req = (ADIOI_NBC_Request *)ADIOI_Calloc(1, sizeof(ADIOI_NBC_Request));
    nbc_req->rdwr = ADIOI_WRITE;

    /* create a generalized request */
    if (ADIOI_GEN_greq_class == 0) {
        MPIX_Grequest_class_create(ADIOI_GEN_iwc_query_fn,
                ADIOI_GEN_iwc_free_fn, MPIU_Greq_cancel_fn,
                ADIOI_GEN_iwc_poll_fn, ADIOI_GEN_iwc_wait_fn,
                &ADIOI_GEN_greq_class);
    }
    MPIX_Grequest_class_allocate(ADIOI_GEN_greq_class, nbc_req, request);
    memcpy(&nbc_req->req, request, sizeof(MPI_Request));

    /* create a struct for parameters and variables */
    vars = (ADIOI_GEN_IwriteStridedColl_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_GEN_IwriteStridedColl_vars));
    nbc_req->data.wr.wsc_vars = vars;

    /* save the parameters */
    vars->fd = fd;
    vars->buf = buf;
    vars->count = count;
    vars->datatype = datatype;
    vars->file_ptr_type = file_ptr_type;
    vars->offset = offset;

    MPI_Comm_size(fd->comm, &nprocs);
    MPI_Comm_rank(fd->comm, &myrank);
    vars->nprocs = nprocs;
    vars->myrank = myrank;

    /* the number of processes that actually perform I/O, nprocs_for_coll,
     * is stored in the hints off the ADIO_File structure
     */
    vars->nprocs_for_coll = fd->hints->cb_nodes;
    vars->orig_fp = fd->fp_ind;

    /* only check for interleaving if cb_write isn't disabled */
    if (fd->hints->cb_write != ADIOI_HINT_DISABLE) {
        /* For this process's request, calculate the list of offsets and
           lengths in the file and determine the start and end offsets. */

        /* Note: end_offset points to the last byte-offset that will be accessed.
           e.g., if start_offset=0 and 100 bytes to be read, end_offset=99*/

        ADIOI_Calc_my_off_len(fd, count, datatype, file_ptr_type, offset,
                              &vars->offset_list, &vars->len_list,
                              &vars->start_offset, &vars->end_offset,
                              &vars->contig_access_count);

        /* each process communicates its start and end offsets to other
           processes. The result is an array each of start and end offsets
           stored in order of process rank. */

        vars->st_offsets = (ADIO_Offset *)ADIOI_Malloc(nprocs*sizeof(ADIO_Offset));
        vars->end_offsets = (ADIO_Offset *)ADIOI_Malloc(nprocs*sizeof(ADIO_Offset));

        *error_code = MPI_Iallgather(&vars->start_offset, 1, ADIO_OFFSET,
                                     vars->st_offsets, 1, ADIO_OFFSET,
                                     fd->comm, &vars->req_offset[0]);
        if (*error_code != MPI_SUCCESS) return;
        *error_code = MPI_Iallgather(&vars->end_offset, 1, ADIO_OFFSET,
                                     vars->end_offsets, 1, ADIO_OFFSET,
                                     fd->comm, &vars->req_offset[1]);

        nbc_req->data.wr.state = ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL;
        return;
    }

    ADIOI_GEN_IwriteStridedColl_indio(nbc_req, error_code);
}

static void ADIOI_GEN_IwriteStridedColl_inter(ADIOI_NBC_Request *nbc_req,
                                              int *error_code)
{
    ADIOI_GEN_IwriteStridedColl_vars *vars = nbc_req->data.wr.wsc_vars;
    int nprocs = vars->nprocs;
    ADIO_Offset *st_offsets = vars->st_offsets;
    ADIO_Offset *end_offsets = vars->end_offsets;
    int i, interleave_count = 0;

    /* are the accesses of different processes interleaved? */
    for (i = 1; i < nprocs; i++)
        if ((st_offsets[i] < end_offsets[i-1]) &&
            (st_offsets[i] <= end_offsets[i]))
            interleave_count++;
    /* This is a rudimentary check for interleaving, but should suffice
       for the moment. */

    vars->interleave_count = interleave_count;

    ADIOI_GEN_IwriteStridedColl_indio(nbc_req, error_code);
}

static void ADIOI_GEN_IwriteStridedColl_indio(ADIOI_NBC_Request *nbc_req,
                                              int *error_code)
{
    ADIOI_GEN_IwriteStridedColl_vars *vars = nbc_req->data.wr.wsc_vars;
    ADIOI_Icalc_others_req_vars *cor_vars = NULL;
    ADIO_File fd = vars->fd;
    const void *buf;
    int count, file_ptr_type;
    MPI_Datatype datatype = vars->datatype;
    ADIO_Offset offset;
    int filetype_is_contig;
    ADIO_Offset off;
    int nprocs;

    ADIOI_Datatype_iscontig(datatype, &vars->buftype_is_contig);

    if (fd->hints->cb_write == ADIOI_HINT_DISABLE ||
       (!vars->interleave_count && (fd->hints->cb_write == ADIOI_HINT_AUTO)))
    {
        buf = vars->buf;
        count = vars->count;
        file_ptr_type = vars->file_ptr_type;
        offset = vars->offset;

        /* use independent accesses */
        if (fd->hints->cb_write != ADIOI_HINT_DISABLE) {
            ADIOI_Free(vars->offset_list);
            ADIOI_Free(vars->len_list);
            ADIOI_Free(vars->st_offsets);
            ADIOI_Free(vars->end_offsets);
        }

        fd->fp_ind = vars->orig_fp;
        ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);

#if defined(ROMIO_RUN_ON_LINUX) && !defined(HAVE_AIO_LITE_H)
        /* NOTE: This is currently a workaround to avoid weird errors, e.g.,
         * stack fault, occurred on Linux. When the host OS is Linux and
         * aio-lite is not used, a blocking ADIO function is used here.
         * See https://trac.mpich.org/projects/mpich/ticket/2201. */
        MPI_Status status;
        if (vars->buftype_is_contig && filetype_is_contig) {
            if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
                off = fd->disp + (ADIO_Offset)(fd->etype_size) * offset;
                ADIO_WriteContig(fd, buf, count, datatype,
                                 ADIO_EXPLICIT_OFFSET,
                                 off, &status, error_code);
            }
            else ADIO_WriteContig(fd, buf, count, datatype, ADIO_INDIVIDUAL,
                                  0, &status, error_code);
        }
        else {
            ADIO_WriteStrided(fd, buf, count, datatype, file_ptr_type,
                              offset, &status, error_code);
        }
        ADIOI_GEN_IwriteStridedColl_fini(nbc_req, error_code);
#else
        if (vars->buftype_is_contig && filetype_is_contig) {
            if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
                off = fd->disp + (ADIO_Offset)(fd->etype_size) * offset;
                ADIO_IwriteContig(fd, buf, count, datatype,
                                  ADIO_EXPLICIT_OFFSET,
                                  off, &vars->req_ind_io, error_code);
            }
            else ADIO_IwriteContig(fd, buf, count, datatype, ADIO_INDIVIDUAL,
                                   0, &vars->req_ind_io, error_code);
        }
        else {
            ADIO_IwriteStrided(fd, buf, count, datatype, file_ptr_type,
                               offset, &vars->req_ind_io, error_code);
        }

        nbc_req->data.wr.state = ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL_INDIO;
#endif
        return;
    }

    nprocs = vars->nprocs;

    /* Divide the I/O workload among "nprocs_for_coll" processes. This is
       done by (logically) dividing the file into file domains (FDs); each
       process may directly access only its own file domain. */

    ADIOI_Calc_file_domains(vars->st_offsets, vars->end_offsets, nprocs,
            vars->nprocs_for_coll, &vars->min_st_offset,
            &vars->fd_start, &vars->fd_end,
            fd->hints->min_fdomain_size, &vars->fd_size,
            fd->hints->striping_unit);

    /* calculate what portions of the access requests of this process are
       located in what file domains */

    ADIOI_Calc_my_req(fd, vars->offset_list, vars->len_list,
            vars->contig_access_count, vars->min_st_offset,
            vars->fd_start, vars->fd_end, vars->fd_size,
            nprocs, &vars->count_my_req_procs,
            &vars->count_my_req_per_proc, &vars->my_req,
            &vars->buf_idx);

    /* based on everyone's my_req, calculate what requests of other
       processes lie in this process's file domain.
       count_others_req_procs = number of processes whose requests lie in
       this process's file domain (including this process itself)
       count_others_req_per_proc[i] indicates how many separate contiguous
       requests of proc. i lie in this process's file domain. */

    cor_vars = (ADIOI_Icalc_others_req_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_Icalc_others_req_vars));
    nbc_req->cor_vars = cor_vars;
    cor_vars->fd = vars->fd;
    cor_vars->count_my_req_procs = vars->count_my_req_procs;
    cor_vars->count_my_req_per_proc = vars->count_my_req_per_proc;
    cor_vars->my_req = vars->my_req;
    cor_vars->nprocs = vars->nprocs;
    cor_vars->myrank = vars->myrank;
    cor_vars->count_others_req_procs_ptr = &vars->count_others_req_procs;
    cor_vars->others_req_ptr = &vars->others_req;
    cor_vars->next_fn = ADIOI_GEN_IwriteStridedColl_exch;

    ADIOI_Icalc_others_req(nbc_req, error_code);
}

static void ADIOI_GEN_IwriteStridedColl_exch(ADIOI_NBC_Request *nbc_req,
                                             int *error_code)
{
    ADIOI_GEN_IwriteStridedColl_vars *vars = nbc_req->data.wr.wsc_vars;
    ADIOI_Iexch_and_write_vars *eaw_vars = NULL;
    ADIOI_Access *my_req = vars->my_req;
    int nprocs = vars->nprocs;
    int i;

    ADIOI_Free(vars->count_my_req_per_proc);
    for (i = 0; i < nprocs; i++) {
        if (my_req[i].count) {
            ADIOI_Free(my_req[i].offsets);
            ADIOI_Free(my_req[i].lens);
        }
    }
    ADIOI_Free(my_req);

    /* exchange data and write in sizes of no more than coll_bufsize. */
    /* Cast away const'ness for the below function */
    eaw_vars = (ADIOI_Iexch_and_write_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_Iexch_and_write_vars));
    nbc_req->data.wr.eaw_vars = eaw_vars;
    eaw_vars->fd = vars->fd;
    eaw_vars->buf = (char *)vars->buf;
    eaw_vars->datatype = vars->datatype;
    eaw_vars->nprocs = vars->nprocs;
    eaw_vars->myrank = vars->myrank;
    eaw_vars->others_req = vars->others_req;
    eaw_vars->offset_list = vars->offset_list;
    eaw_vars->len_list = vars->len_list;
    eaw_vars->contig_access_count = vars->contig_access_count;
    eaw_vars->min_st_offset = vars->min_st_offset;
    eaw_vars->fd_size = vars->fd_size;
    eaw_vars->fd_start = vars->fd_start;
    eaw_vars->fd_end = vars->fd_end;
    eaw_vars->buf_idx = vars->buf_idx;
    eaw_vars->next_fn = ADIOI_GEN_IwriteStridedColl_bcast;

    ADIOI_Iexch_and_write(nbc_req, error_code);
}

static void ADIOI_GEN_IwriteStridedColl_bcast(ADIOI_NBC_Request *nbc_req,
                                              int *error_code)
{
    ADIOI_GEN_IwriteStridedColl_vars *vars = nbc_req->data.wr.wsc_vars;
    ADIO_File fd = vars->fd;

    /* If this collective write is followed by an independent write,
     * it's possible to have those subsequent writes on other processes
     * race ahead and sneak in before the read-modify-write completes.
     * We carry out a collective communication at the end here so no one
     * can start independent i/o before collective I/O completes.
     *
     * need to do some gymnastics with the error codes so that if something
     * went wrong, all processes report error, but if a process has a more
     * specific error code, we can still have that process report the
     * additional information */

    vars->old_error = *error_code;
    if (*error_code != MPI_SUCCESS) *error_code = MPI_ERR_IO;

    /* optimization: if only one process performing i/o, we can perform
     * a less-expensive Bcast  */
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_postwrite_a, 0, NULL );
#endif
    vars->error_code = *error_code;
    if (fd->hints->cb_nodes == 1) {
        *error_code = MPI_Ibcast(&vars->error_code, 1, MPI_INT,
                                 fd->hints->ranklist[0], fd->comm,
                                 &vars->req_err);
    } else {
        vars->tmp_error = *error_code;
        *error_code  = MPI_Iallreduce(&vars->tmp_error, &vars->error_code, 1,
                                      MPI_INT, MPI_MAX, fd->comm,
                                      &vars->req_err);
    }

    nbc_req->data.wr.state = ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL_BCAST;
}

static void ADIOI_GEN_IwriteStridedColl_free(ADIOI_NBC_Request *nbc_req,
                                             int *error_code)
{
    ADIOI_GEN_IwriteStridedColl_vars *vars = nbc_req->data.wr.wsc_vars;
    ADIO_File fd = vars->fd;
    MPI_Datatype datatype = vars->datatype;
    ADIOI_Access *others_req = vars->others_req;
    int nprocs = vars->nprocs;
    int old_error = vars->old_error;
    int i;

#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event( ADIOI_MPE_postwrite_b, 0, NULL );
#endif
#ifdef AGGREGATION_PROFILE
    MPE_Log_event(5012, 0, NULL);
#endif

    if ( (old_error != MPI_SUCCESS) && (old_error != MPI_ERR_IO) )
        *error_code = old_error;


    if (!vars->buftype_is_contig) ADIOI_Delete_flattened(datatype);

    /* free all memory allocated for collective I/O */
    for (i = 0; i < nprocs; i++) {
        if (others_req[i].count) {
            ADIOI_Free(others_req[i].offsets);
            ADIOI_Free(others_req[i].lens);
            ADIOI_Free(others_req[i].mem_ptrs);
        }
    }
    ADIOI_Free(others_req);

    ADIOI_Free(vars->buf_idx);
    ADIOI_Free(vars->offset_list);
    ADIOI_Free(vars->len_list);
    ADIOI_Free(vars->st_offsets);
    ADIOI_Free(vars->end_offsets);
    ADIOI_Free(vars->fd_start);
    ADIOI_Free(vars->fd_end);

    fd->fp_sys_posn = -1;   /* set it to null. */
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5013, 0, NULL);
#endif

    ADIOI_GEN_IwriteStridedColl_fini(nbc_req, error_code);
}

static void ADIOI_GEN_IwriteStridedColl_fini(ADIOI_NBC_Request *nbc_req,
                                             int *error_code)
{
    ADIOI_GEN_IwriteStridedColl_vars *vars = nbc_req->data.wr.wsc_vars;
    MPI_Count size;

    /* This is a temporary way of filling in status. The right way is to
       keep track of how much data was actually written during collective I/O. */
    MPI_Type_size_x(vars->datatype, &size);
    nbc_req->nbytes = size * vars->count;

    /* free the struct for parameters and variables */
    if (nbc_req->data.wr.wsc_vars) {
        ADIOI_Free(nbc_req->data.wr.wsc_vars);
        nbc_req->data.wr.wsc_vars = NULL;
    }

    /* make the request complete */
    *error_code = MPI_Grequest_complete(nbc_req->req);
    nbc_req->data.wr.state = ADIOI_IWC_STATE_COMPLETE;
}


static void ADIOI_Iexch_and_write(ADIOI_NBC_Request *nbc_req, int *error_code)
{
    ADIOI_Iexch_and_write_vars *vars = nbc_req->data.wr.eaw_vars;
    ADIO_File fd = vars->fd;
    MPI_Datatype datatype = vars->datatype;
    int nprocs = vars->nprocs;
    ADIOI_Access *others_req = vars->others_req;

    /* Send data to appropriate processes and write in sizes of no more
       than coll_bufsize.
       The idea is to reduce the amount of extra memory required for
       collective I/O. If all data were written all at once, which is much
       easier, it would require temp space more than the size of user_buf,
       which is often unacceptable. For example, to write a distributed
       array to a file, where each local array is 8Mbytes, requiring
       at least another 8Mbytes of temp space is unacceptable. */

    int i, j;
    ADIO_Offset st_loc = -1, end_loc = -1;
    int info_flag, coll_bufsize;
    char *value;

    *error_code = MPI_SUCCESS;  /* changed below if error */
    /* only I/O errors are currently reported */

    /* calculate the number of writes of size coll_bufsize
       to be done by each process and the max among all processes.
       That gives the no. of communication phases as well. */

    value = (char *)ADIOI_Malloc((MPI_MAX_INFO_VAL+1) * sizeof(char));
    ADIOI_Info_get(fd->info, "cb_buffer_size", MPI_MAX_INFO_VAL, value,
                   &info_flag);
    coll_bufsize = atoi(value);
    vars->coll_bufsize = coll_bufsize;
    ADIOI_Free(value);

    for (i = 0; i < nprocs; i++) {
        if (others_req[i].count) {
            st_loc = others_req[i].offsets[0];
            end_loc = others_req[i].offsets[0];
            break;
        }
    }

    for (i = 0; i < nprocs; i++)
        for (j = 0; j < others_req[i].count; j++) {
            st_loc = ADIOI_MIN(st_loc, others_req[i].offsets[j]);
            end_loc = ADIOI_MAX(end_loc, (others_req[i].offsets[j]
                        + others_req[i].lens[j] - 1));
        }

    vars->st_loc = st_loc;
    vars->end_loc = end_loc;

    /* ntimes=ceiling_div(end_loc - st_loc + 1, coll_bufsize)*/

    vars->ntimes = (int)((end_loc - st_loc + coll_bufsize) / coll_bufsize);

    if ((st_loc==-1) && (end_loc==-1)) {
        vars->ntimes = 0; /* this process does no writing. */
    }

    *error_code = MPI_Iallreduce(&vars->ntimes, &vars->max_ntimes, 1, MPI_INT,
                                 MPI_MAX, fd->comm, &vars->req1);

    vars->write_buf = fd->io_buf;

    vars->curr_offlen_ptr = (int *)ADIOI_Calloc(nprocs, sizeof(int));
    /* its use is explained below. calloc initializes to 0. */

    vars->count = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    /* to store count of how many off-len pairs per proc are satisfied
       in an iteration. */

    vars->partial_recv = (int *)ADIOI_Calloc(nprocs, sizeof(int));
    /* if only a portion of the last off-len pair is recd. from a process
       in a particular iteration, the length recd. is stored here.
       calloc initializes to 0. */

    vars->send_size = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    /* total size of data to be sent to each proc. in an iteration.
       Of size nprocs so that I can use MPI_Alltoall later. */

    vars->recv_size = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    /* total size of data to be recd. from each proc. in an iteration.*/

    vars->sent_to_proc = (int *)ADIOI_Calloc(nprocs, sizeof(int));
    /* amount of data sent to each proc so far. Used in
       ADIOI_Fill_send_buffer. initialized to 0 here. */

    vars->send_buf_idx = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    vars->curr_to_proc = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    vars->done_to_proc = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    /* Above three are used in ADIOI_Fill_send_buffer*/

    vars->start_pos = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    /* used to store the starting value of curr_offlen_ptr[i] in
       this iteration */

    ADIOI_Datatype_iscontig(datatype, &vars->buftype_is_contig);
    if (!vars->buftype_is_contig) {
	vars->flat_buf = ADIOI_Flatten_and_find(datatype);
    }
    MPI_Type_extent(datatype, &vars->buftype_extent);


    /* I need to check if there are any outstanding nonblocking writes to
       the file, which could potentially interfere with the writes taking
       place in this collective write call. Since this is not likely to be
       common, let me do the simplest thing possible here: Each process
       completes all pending nonblocking operations before completing. */

    /*ADIOI_Complete_async(error_code);
      if (*error_code != MPI_SUCCESS) return;
      MPI_Barrier(fd->comm);
     */

    vars->done = 0;
    vars->off = st_loc;

    /* set the state to wait until MPI_Ialltoall finishes. */
    nbc_req->data.wr.state = ADIOI_IWC_STATE_IEXCH_AND_WRITE;
}

static void ADIOI_Iexch_and_write_l1_begin(ADIOI_NBC_Request *nbc_req,
                                           int *error_code)
{
    ADIOI_Iexch_and_write_vars *vars = nbc_req->data.wr.eaw_vars;
    int nprocs;
    ADIOI_Access *others_req;

    int i, j;
    ADIO_Offset off, req_off;
    char *write_buf;
    int *curr_offlen_ptr, *count, req_len, *recv_size;
    int *partial_recv, *start_pos;
    ADIO_Offset size;
    static char myname[] = "ADIOI_IEXCH_AND_WRITE_L1_BEGIN";

    ADIOI_W_Iexchange_data_vars *wed_vars = NULL;

    /* loop exit condition */
    if (vars->m >= vars->ntimes) {
        ADIOI_Iexch_and_write_reset(nbc_req, error_code);
        return;
    }

    nprocs = vars->nprocs;
    others_req = vars->others_req;

    off = vars->off;
    write_buf = vars->write_buf;
    curr_offlen_ptr = vars->curr_offlen_ptr;
    count = vars->count;
    recv_size = vars->recv_size;
    partial_recv = vars->partial_recv;
    start_pos = vars->start_pos;

    /* go through all others_req and check which will be satisfied
       by the current write */

    /* Note that MPI guarantees that displacements in filetypes are in
       monotonically nondecreasing order and that, for writes, the
       filetypes cannot specify overlapping regions in the file. This
       simplifies implementation a bit compared to reads. */

    /* off = start offset in the file for the data to be written in
       this iteration
       size = size of data written (bytes) corresponding to off
       req_off = off in file for a particular contiguous request
       minus what was satisfied in previous iteration
       req_size = size corresponding to req_off */

    /* first calculate what should be communicated */

    for (i = 0; i < nprocs; i++) count[i] = recv_size[i] = 0;

    size = ADIOI_MIN((unsigned)vars->coll_bufsize,
                     vars->end_loc - vars->st_loc + 1 - vars->done);
    vars->size = size;

    for (i = 0; i < nprocs; i++) {
        if (others_req[i].count) {
            start_pos[i] = curr_offlen_ptr[i];
            for (j = curr_offlen_ptr[i]; j < others_req[i].count; j++) {
                if (partial_recv[i]) {
                    /* this request may have been partially
                       satisfied in the previous iteration. */
                    req_off = others_req[i].offsets[j] +
                        partial_recv[i];
                    req_len = others_req[i].lens[j] -
                        partial_recv[i];
                    partial_recv[i] = 0;
                    /* modify the off-len pair to reflect this change */
                    others_req[i].offsets[j] = req_off;
                    others_req[i].lens[j] = req_len;
                }
                else {
                    req_off = others_req[i].offsets[j];
                    req_len = others_req[i].lens[j];
                }
                if (req_off < off + size) {
                    count[i]++;
                    ADIOI_Assert((((ADIO_Offset)(MPIU_Upint)write_buf)+req_off-off) == (ADIO_Offset)(MPIU_Upint)(write_buf+req_off-off));
                    MPI_Address(write_buf + req_off - off,
                                &(others_req[i].mem_ptrs[j]));
                    ADIOI_Assert((off + size - req_off) == (int)(off + size - req_off));
                    recv_size[i] += (int)(ADIOI_MIN(off + size - req_off,
                                                    (unsigned)req_len));

                    if (off+size-req_off < (unsigned)req_len)
                    {
                        partial_recv[i] = (int)(off + size - req_off);

                        /* --BEGIN ERROR HANDLING-- */
                        if ((j+1 < others_req[i].count) &&
                            (others_req[i].offsets[j+1] < off+size))
                        {
                            *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                    MPIR_ERR_RECOVERABLE,
                                    myname,
                                    __LINE__,
                                    MPI_ERR_ARG,
                                    "Filetype specifies overlapping write regions (which is illegal according to the MPI-2 specification)", 0);
                            /* allow to continue since additional
                             * communication might have to occur
                             */
                        }
                        /* --END ERROR HANDLING-- */
                        break;
                    }
                }
                else break;
            }
            curr_offlen_ptr[i] = j;
        }
    }

    /* create a struct for ADIOI_W_Iexchange_data() */
    wed_vars = (ADIOI_W_Iexchange_data_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_W_Iexchange_data_vars));
    nbc_req->data.wr.wed_vars = wed_vars;

    wed_vars->fd = vars->fd;
    wed_vars->buf = vars->buf;
    wed_vars->write_buf = vars->write_buf;
    wed_vars->flat_buf = vars->flat_buf;
    wed_vars->offset_list = vars->offset_list;
    wed_vars->len_list = vars->len_list;
    wed_vars->send_size = vars->send_size;
    wed_vars->recv_size = vars->recv_size;
    wed_vars->off = vars->off;
    wed_vars->size = vars->size;
    wed_vars->count = vars->count;
    wed_vars->start_pos = vars->start_pos;
    wed_vars->partial_recv = vars->partial_recv;
    wed_vars->sent_to_proc = vars->sent_to_proc;
    wed_vars->nprocs = vars->nprocs;
    wed_vars->myrank = vars->myrank;
    wed_vars->buftype_is_contig = vars->buftype_is_contig;
    wed_vars->contig_access_count = vars->contig_access_count;
    wed_vars->min_st_offset = vars->min_st_offset;
    wed_vars->fd_size = vars->fd_size;
    wed_vars->fd_start = vars->fd_start;
    wed_vars->fd_end = vars->fd_end;
    wed_vars->others_req = vars->others_req;
    wed_vars->send_buf_idx = vars->send_buf_idx;
    wed_vars->curr_to_proc = vars->curr_to_proc;
    wed_vars->done_to_proc = vars->done_to_proc;
    wed_vars->hole = &vars->hole;
    wed_vars->iter = vars->m;
    wed_vars->buftype_extent = vars->buftype_extent;
    wed_vars->buf_idx = vars->buf_idx;
    wed_vars->next_fn = ADIOI_Iexch_and_write_l1_body;

    ADIOI_W_Iexchange_data(nbc_req, error_code);
}

static void ADIOI_Iexch_and_write_l1_body(ADIOI_NBC_Request *nbc_req,
                                          int *error_code)
{
    ADIOI_Iexch_and_write_vars *vars = nbc_req->data.wr.eaw_vars;
    ADIO_File fd = vars->fd;
    int nprocs = vars->nprocs;
    ADIO_Offset size = vars->size;
    char *write_buf = vars->write_buf;
    int *count = vars->count;
    int flag, i;

    flag = 0;
    for (i = 0; i < nprocs; i++)
        if (count[i]) flag = 1;

    if (flag) {
        ADIOI_Assert(size == (int)size);
#if defined(ROMIO_RUN_ON_LINUX) && !defined(HAVE_AIO_LITE_H)
        MPI_Status status;
        ADIO_WriteContig(fd, write_buf, (int)size, MPI_BYTE,
                         ADIO_EXPLICIT_OFFSET, vars->off, &status,
                         error_code);
#else
        ADIO_IwriteContig(fd, write_buf, (int)size, MPI_BYTE,
                          ADIO_EXPLICIT_OFFSET, vars->off, &vars->req3,
                          error_code);

        nbc_req->data.wr.state = ADIOI_IWC_STATE_IEXCH_AND_WRITE_L1_BODY;
        return;
#endif
    }

    ADIOI_Iexch_and_write_l1_end(nbc_req, error_code);
}

static void ADIOI_Iexch_and_write_l1_end(ADIOI_NBC_Request *nbc_req,
                                         int *error_code)
{
    ADIOI_Iexch_and_write_vars *vars = nbc_req->data.wr.eaw_vars;
    ADIO_Offset size = vars->size;

    vars->off += size;
    vars->done += size;

    /* increment m and go back to the beginning of m loop */
    vars->m++;
    ADIOI_Iexch_and_write_l1_begin(nbc_req, error_code);
}

static void ADIOI_Iexch_and_write_reset(ADIOI_NBC_Request *nbc_req,
                                        int *error_code)
{
    ADIOI_Iexch_and_write_vars *vars = nbc_req->data.wr.eaw_vars;
    int nprocs = vars->nprocs;
    int *count = vars->count;
    int *recv_size = vars->recv_size;
    int i;

    for (i = 0; i < nprocs; i++) count[i] = recv_size[i] = 0;

    vars->m = vars->ntimes;
    ADIOI_Iexch_and_write_l2_begin(nbc_req, error_code);
}

static void ADIOI_Iexch_and_write_l2_begin(ADIOI_NBC_Request *nbc_req,
                                           int *error_code)
{
    ADIOI_Iexch_and_write_vars *vars = nbc_req->data.wr.eaw_vars;
    ADIO_Offset size = vars->size;
    ADIOI_W_Iexchange_data_vars *wed_vars = NULL;

    /* loop exit condition */
    if (vars->m >= vars->max_ntimes) {
        ADIOI_Iexch_and_write_fini(nbc_req, error_code);
        return;
    }

    ADIOI_Assert(size == (int)size);

    /* create a struct for ADIOI_W_Iexchange_data() */
    wed_vars = (ADIOI_W_Iexchange_data_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_W_Iexchange_data_vars));
    nbc_req->data.wr.wed_vars = wed_vars;

    wed_vars->fd = vars->fd;
    wed_vars->buf = vars->buf;
    wed_vars->write_buf = vars->write_buf;
    wed_vars->flat_buf = vars->flat_buf;
    wed_vars->offset_list = vars->offset_list;
    wed_vars->len_list = vars->len_list;
    wed_vars->send_size = vars->send_size;
    wed_vars->recv_size = vars->recv_size;
    wed_vars->off = vars->off;
    wed_vars->size = (int)vars->size;
    wed_vars->count = vars->count;
    wed_vars->start_pos = vars->start_pos;
    wed_vars->partial_recv = vars->partial_recv;
    wed_vars->sent_to_proc = vars->sent_to_proc;
    wed_vars->nprocs = vars->nprocs;
    wed_vars->myrank = vars->myrank;
    wed_vars->buftype_is_contig = vars->buftype_is_contig;
    wed_vars->contig_access_count = vars->contig_access_count;
    wed_vars->min_st_offset = vars->min_st_offset;
    wed_vars->fd_size = vars->fd_size;
    wed_vars->fd_start = vars->fd_start;
    wed_vars->fd_end = vars->fd_end;
    wed_vars->others_req = vars->others_req;
    wed_vars->send_buf_idx = vars->send_buf_idx;
    wed_vars->curr_to_proc = vars->curr_to_proc;
    wed_vars->done_to_proc = vars->done_to_proc;
    wed_vars->hole = &vars->hole;
    wed_vars->iter = vars->m;
    wed_vars->buftype_extent = vars->buftype_extent;
    wed_vars->buf_idx = vars->buf_idx;
    wed_vars->next_fn = ADIOI_Iexch_and_write_l2_end;

    /* nothing to recv, but check for send. */
    ADIOI_W_Iexchange_data(nbc_req, error_code);
}

static void ADIOI_Iexch_and_write_l2_end(ADIOI_NBC_Request *nbc_req,
                                         int *error_code)
{
    ADIOI_Iexch_and_write_vars *vars = nbc_req->data.wr.eaw_vars;

    vars->m++;
    ADIOI_Iexch_and_write_l2_begin(nbc_req, error_code);
}

static void ADIOI_Iexch_and_write_fini(ADIOI_NBC_Request *nbc_req, int *error_code)
{
    ADIOI_Iexch_and_write_vars *vars = nbc_req->data.wr.eaw_vars;
    void (*next_fn)(ADIOI_NBC_Request *, int *);

    ADIOI_Free(vars->curr_offlen_ptr);
    ADIOI_Free(vars->count);
    ADIOI_Free(vars->partial_recv);
    ADIOI_Free(vars->send_size);
    ADIOI_Free(vars->recv_size);
    ADIOI_Free(vars->sent_to_proc);
    ADIOI_Free(vars->start_pos);
    ADIOI_Free(vars->send_buf_idx);
    ADIOI_Free(vars->curr_to_proc);
    ADIOI_Free(vars->done_to_proc);

    next_fn = vars->next_fn;

    /* free the struct for parameters and variables */
    ADIOI_Free(nbc_req->data.wr.eaw_vars);
    nbc_req->data.wr.eaw_vars = NULL;

    /* move to the next function */
    next_fn(nbc_req, error_code);
}


static void ADIOI_W_Iexchange_data(ADIOI_NBC_Request *nbc_req, int *error_code)
{
    ADIOI_W_Iexchange_data_vars *vars = nbc_req->data.wr.wed_vars;

    /* exchange recv_size info so that each process knows how much to
       send to whom. */

    *error_code = MPI_Ialltoall(vars->recv_size, 1, MPI_INT, vars->send_size, 1,
                                MPI_INT, vars->fd->comm, &vars->req1);

    nbc_req->data.wr.state = ADIOI_IWC_STATE_W_IEXCHANGE_DATA;
}

static void ADIOI_W_Iexchange_data_hole(ADIOI_NBC_Request *nbc_req,
                                        int *error_code)
{
    ADIOI_W_Iexchange_data_vars *vars = nbc_req->data.wr.wed_vars;
    ADIO_File fd = vars->fd;
    int *recv_size = vars->recv_size;
    ADIO_Offset off = vars->off;
    int size = vars->size;
    int *count = vars->count;
    int *start_pos = vars->start_pos;
    int *partial_recv = vars->partial_recv;
    int nprocs = vars->nprocs;
    ADIOI_Access *others_req = vars->others_req;
    int *hole = vars->hole;

    int i, j, k, *tmp_len, nprocs_recv;
    MPI_Datatype *recv_types;
    int *srt_len = NULL, sum;
    ADIO_Offset *srt_off = NULL;

    /* create derived datatypes for recv */

    nprocs_recv = 0;
    for (i = 0; i < nprocs; i++) if (recv_size[i]) nprocs_recv++;
    vars->nprocs_recv = nprocs_recv;

    recv_types = (MPI_Datatype *)
        ADIOI_Malloc((nprocs_recv+1)*sizeof(MPI_Datatype));
    vars->recv_types = recv_types;
    /* +1 to avoid a 0-size malloc */

    tmp_len = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    j = 0;
    for (i = 0; i < nprocs; i++) {
        if (recv_size[i]) {
            /* take care if the last off-len pair is a partial recv */
            if (partial_recv[i]) {
                k = start_pos[i] + count[i] - 1;
                tmp_len[i] = others_req[i].lens[k];
                others_req[i].lens[k] = partial_recv[i];
            }
            ADIOI_Type_create_hindexed_x(count[i],
                    &(others_req[i].lens[start_pos[i]]),
                    &(others_req[i].mem_ptrs[start_pos[i]]),
                    MPI_BYTE, recv_types+j);
            /* absolute displacements; use MPI_BOTTOM in recv */
            MPI_Type_commit(recv_types+j);
            j++;
        }
    }

    /* To avoid a read-modify-write, check if there are holes in the
       data to be written. For this, merge the (sorted) offset lists
       others_req using a heap-merge. */

    sum = 0;
    for (i = 0; i < nprocs; i++) sum += count[i];
    /* valgrind-detcted optimization: if there is no work on this process we do
     * not need to search for holes */
    if (sum) {
        srt_off = (ADIO_Offset *)ADIOI_Malloc(sum*sizeof(ADIO_Offset));
        srt_len = (int *)ADIOI_Malloc(sum*sizeof(int));

        ADIOI_Heap_merge(others_req, count, srt_off, srt_len, start_pos,
                         nprocs, nprocs_recv, sum);
    }

    /* for partial recvs, restore original lengths */
    for (i = 0; i < nprocs; i++)
        if (partial_recv[i]) {
            k = start_pos[i] + count[i] - 1;
            others_req[i].lens[k] = tmp_len[i];
        }
    ADIOI_Free(tmp_len);

    /* check if there are any holes. If yes, must do read-modify-write.
     * holes can be in three places.  'middle' is what you'd expect: the
     * processes are operating on noncontigous data.  But holes can also show
     * up at the beginning or end of the file domain (see John Bent ROMIO REQ
     * #835). Missing these holes would result in us writing more data than
     * recieved by everyone else. */

    *hole = 0;
    if (sum) {
        if (off != srt_off[0]) /* hole at the front */
            *hole = 1;
        else { /* coalesce the sorted offset-length pairs */
            for (i = 1; i < sum; i++) {
                if (srt_off[i] <= srt_off[0] + srt_len[0]) {
                    /* ok to cast: operating on cb_buffer_size chunks */
                    int new_len = (int)srt_off[i] + srt_len[i] - (int)srt_off[0];
                    if (new_len > srt_len[0]) srt_len[0] = new_len;
                }
                else
                    break;
            }
            if (i < sum || size != srt_len[0]) /* hole in middle or end */
                *hole = 1;
        }

        ADIOI_Free(srt_off);
        ADIOI_Free(srt_len);
    }

    if (nprocs_recv) {
        if (*hole) {
            ADIO_IreadContig(fd, vars->write_buf, size, MPI_BYTE,
                             ADIO_EXPLICIT_OFFSET, off, &vars->req2,
                             &vars->err);
            nbc_req->data.wr.state = ADIOI_IWC_STATE_W_IEXCHANGE_DATA_HOLE;
            return;
        }
    }

    ADIOI_W_Iexchange_data_send(nbc_req, error_code);
}

static void ADIOI_W_Iexchange_data_send(ADIOI_NBC_Request *nbc_req,
                                        int *error_code)
{
    ADIOI_W_Iexchange_data_vars *vars = nbc_req->data.wr.wed_vars;
    ADIO_File fd = vars->fd;
    void *buf = vars->buf;
    int *send_size = vars->send_size;
    int *recv_size = vars->recv_size;
    int nprocs = vars->nprocs;
    int myrank = vars->myrank;
    int iter = vars->iter;
    int *buf_idx = vars->buf_idx;

    int nprocs_recv = vars->nprocs_recv;
    MPI_Datatype *recv_types = vars->recv_types;

    int i, j;
    int nprocs_send;
    char **send_buf = NULL;

    nprocs_send = 0;
    for (i = 0; i < nprocs; i++) if (send_size[i]) nprocs_send++;
    vars->nprocs_send = nprocs_send;

    if (fd->atomicity) {
        /* bug fix from Wei-keng Liao and Kenin Coloma */
        vars->requests = (MPI_Request *)
            ADIOI_Malloc((nprocs_send+1)*sizeof(MPI_Request));
        vars->send_req = vars->requests;
    }
    else {
        vars->requests = (MPI_Request *)
            ADIOI_Malloc((nprocs_send+nprocs_recv+1)*sizeof(MPI_Request));
        /* +1 to avoid a 0-size malloc */

        /* post receives */
        j = 0;
        for (i = 0; i < nprocs; i++) {
            if (recv_size[i]) {
                MPI_Irecv(MPI_BOTTOM, 1, recv_types[j], i, myrank+i+100*iter,
                          fd->comm, vars->requests+j);
                j++;
            }
        }
        vars->send_req = vars->requests + nprocs_recv;
    }

    /* post sends. if buftype_is_contig, data can be directly sent from
       user buf at location given by buf_idx. else use send_buf. */

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5032, 0, NULL);
#endif
    if (vars->buftype_is_contig) {
        j = 0;
        for (i = 0; i < nprocs; i++)
            if (send_size[i]) {
                MPI_Isend(((char *) buf) + buf_idx[i], send_size[i],
                          MPI_BYTE, i,  myrank+i+100*iter, fd->comm,
                          vars->send_req+j);
                j++;
                buf_idx[i] += send_size[i];
            }
    }
    else if (nprocs_send) {
        /* buftype is not contig */
        send_buf = (char **)ADIOI_Malloc(nprocs*sizeof(char*));
        vars->send_buf = send_buf;
        for (i = 0; i < nprocs; i++)
            if (send_size[i])
                send_buf[i] = (char *)ADIOI_Malloc(send_size[i]);

        ADIOI_Fill_send_buffer(fd, buf, vars->flat_buf, send_buf,
                               vars->offset_list, vars->len_list, send_size,
                               vars->send_req,
                               vars->sent_to_proc, nprocs, myrank,
                               vars->contig_access_count,
                               vars->min_st_offset, vars->fd_size,
                               vars->fd_start, vars->fd_end,
                               vars->send_buf_idx, vars->curr_to_proc,
                               vars->done_to_proc, iter,
                               vars->buftype_extent);
        /* the send is done in ADIOI_Fill_send_buffer */
    }

    if (fd->atomicity) {
        vars->req3 = (MPI_Request *)
            ADIOI_Malloc((nprocs_recv+1)*sizeof(MPI_Request));
        /* +1 to avoid a 0-size malloc */

        /* bug fix from Wei-keng Liao and Kenin Coloma */
        j = 0;
        for (i = 0; i < nprocs; i++) {
            if (recv_size[i]) {
                MPI_Irecv(MPI_BOTTOM, 1, recv_types[j], i, myrank+i+100*iter,
                          fd->comm, vars->req3+j);
                j++;
            }
        }

        nbc_req->data.wr.state = ADIOI_IWC_STATE_W_IEXCHANGE_DATA_SEND;
        return;
    }

    ADIOI_W_Iexchange_data_wait(nbc_req, error_code);
}

static void ADIOI_W_Iexchange_data_wait(ADIOI_NBC_Request *nbc_req,
                                        int *error_code)
{
    ADIOI_W_Iexchange_data_vars *vars = nbc_req->data.wr.wed_vars;
    ADIO_File fd = vars->fd;
    int nprocs_send = vars->nprocs_send;
    int nprocs_recv = vars->nprocs_recv;
    MPI_Datatype *recv_types = vars->recv_types;
    int i;

    for (i = 0; i < nprocs_recv; i++) MPI_Type_free(recv_types+i);
    ADIOI_Free(recv_types);

    i= 0;
    if (fd->atomicity) {
        /* bug fix from Wei-keng Liao and Kenin Coloma */
        MPI_Testall(nprocs_send, vars->send_req, &i, MPI_STATUSES_IGNORE);
    }
    else {
        MPI_Testall(nprocs_send+nprocs_recv, vars->requests, &i,
                    MPI_STATUSES_IGNORE);
    }

    if (i) {
        ADIOI_W_Iexchange_data_fini(nbc_req, error_code);
    } else {
        nbc_req->data.wr.state = ADIOI_IWC_STATE_W_IEXCHANGE_DATA_WAIT;
    }
}

static void ADIOI_W_Iexchange_data_fini(ADIOI_NBC_Request *nbc_req, int *error_code)
{
    ADIOI_W_Iexchange_data_vars *vars = nbc_req->data.wr.wed_vars;
    void (*next_fn)(ADIOI_NBC_Request *, int *);
    ADIO_File fd = vars->fd;
    int *send_size = vars->send_size;
    int nprocs = vars->nprocs;
    char **send_buf = vars->send_buf;
    int i;

    if (fd->atomicity) ADIOI_Free(vars->req3);

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5033, 0, NULL);
#endif
    ADIOI_Free(vars->requests);
    if (!vars->buftype_is_contig && vars->nprocs_send) {
        for (i = 0; i < nprocs; i++)
            if (send_size[i]) ADIOI_Free(send_buf[i]);
        ADIOI_Free(send_buf);
    }

    next_fn = vars->next_fn;

    /* free the structure for parameters and variables */
    ADIOI_Free(vars);
    nbc_req->data.wr.wed_vars = NULL;

    /* move to the next function */
    next_fn(nbc_req, error_code);
}


static int ADIOI_GEN_iwc_query_fn(void *extra_state, MPI_Status *status)
{
    ADIOI_NBC_Request *nbc_req;

    nbc_req = (ADIOI_NBC_Request *)extra_state;

    MPI_Status_set_elements_x(status, MPI_BYTE, nbc_req->nbytes);

    /* can never cancel so always true */
    MPI_Status_set_cancelled(status, 0);

    /* choose not to return a value for this */
    status->MPI_SOURCE = MPI_UNDEFINED;
    /* tag has no meaning for this generalized request */
    status->MPI_TAG = MPI_UNDEFINED;

    /* this generalized request never fails */
    return MPI_SUCCESS;
}

static int ADIOI_GEN_iwc_free_fn(void *extra_state)
{
    ADIOI_NBC_Request *nbc_req;

    nbc_req = (ADIOI_NBC_Request *)extra_state;
    ADIOI_Free(nbc_req);

    return MPI_SUCCESS;
}

static int ADIOI_GEN_iwc_poll_fn(void *extra_state, MPI_Status *status)
{
    ADIOI_NBC_Request *nbc_req;
    ADIOI_GEN_IwriteStridedColl_vars *wsc_vars = NULL;
    ADIOI_Icalc_others_req_vars      *cor_vars = NULL;
    ADIOI_Iexch_and_write_vars       *eaw_vars = NULL;
    ADIOI_W_Iexchange_data_vars      *wed_vars = NULL;
    int errcode = MPI_SUCCESS;
    int flag;

    nbc_req = (ADIOI_NBC_Request *)extra_state;

    switch (nbc_req->data.wr.state) {
        case ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL:
            wsc_vars = nbc_req->data.wr.wsc_vars;
            errcode = MPI_Testall(2, wsc_vars->req_offset, &flag,
                                  MPI_STATUSES_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_GEN_IwriteStridedColl_inter(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL_INDIO:
            wsc_vars = nbc_req->data.wr.wsc_vars;
            errcode = MPI_Test(&wsc_vars->req_ind_io, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                /* call the last function */
                ADIOI_GEN_IwriteStridedColl_fini(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_GEN_IWRITESTRIDEDCOLL_BCAST:
            wsc_vars = nbc_req->data.wr.wsc_vars;
            errcode = MPI_Test(&wsc_vars->req_err, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                errcode = wsc_vars->error_code;
                ADIOI_GEN_IwriteStridedColl_free(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_ICALC_OTHERS_REQ:
            cor_vars = nbc_req->cor_vars;
            errcode = MPI_Test(&cor_vars->req1, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_Icalc_others_req_main(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_ICALC_OTHERS_REQ_MAIN:
            cor_vars = nbc_req->cor_vars;
            if (cor_vars->num_req2) {
                errcode = MPI_Testall(cor_vars->num_req2, cor_vars->req2,
                                      &flag, MPI_STATUSES_IGNORE);
                if (errcode == MPI_SUCCESS && flag) {
                    ADIOI_Icalc_others_req_fini(nbc_req, &errcode);
                }
            } else {
                ADIOI_Icalc_others_req_fini(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_IEXCH_AND_WRITE:
            eaw_vars = nbc_req->data.wr.eaw_vars;
            errcode = MPI_Test(&eaw_vars->req1, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                eaw_vars->m = 0;
                ADIOI_Iexch_and_write_l1_begin(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_IEXCH_AND_WRITE_L1_BODY:
            eaw_vars = nbc_req->data.wr.eaw_vars;
            errcode = MPI_Test(&eaw_vars->req3, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_Iexch_and_write_l1_end(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_W_IEXCHANGE_DATA:
            wed_vars = nbc_req->data.wr.wed_vars;
            errcode = MPI_Test(&wed_vars->req1, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_W_Iexchange_data_hole(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_W_IEXCHANGE_DATA_HOLE:
            wed_vars = nbc_req->data.wr.wed_vars;
            errcode = MPI_Test(&wed_vars->req2, &flag, MPI_STATUSES_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                /* --BEGIN ERROR HANDLING-- */
                if (wed_vars->err != MPI_SUCCESS) {
                    errcode = MPIO_Err_create_code(wed_vars->err,
                            MPIR_ERR_RECOVERABLE,
                            "ADIOI_W_EXCHANGE_DATA",
                            __LINE__, MPI_ERR_IO,
                            "**ioRMWrdwr", 0);
                    break;;
                }
                /* --END ERROR HANDLING-- */
                ADIOI_W_Iexchange_data_send(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_W_IEXCHANGE_DATA_SEND:
            wed_vars = nbc_req->data.wr.wed_vars;
            errcode = MPI_Testall(wed_vars->nprocs_recv, wed_vars->req3,
                                  &flag, MPI_STATUSES_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_W_Iexchange_data_wait(nbc_req, &errcode);
            }
            break;

        case ADIOI_IWC_STATE_W_IEXCHANGE_DATA_WAIT:
            wed_vars = nbc_req->data.wr.wed_vars;
            if (wed_vars->fd->atomicity) {
                /* bug fix from Wei-keng Liao and Kenin Coloma */
                errcode = MPI_Testall(wed_vars->nprocs_send, wed_vars->send_req,
                                      &flag, MPI_STATUSES_IGNORE);
            } else {
                errcode = MPI_Testall(wed_vars->nprocs_send +
                                      wed_vars->nprocs_recv,
                                      wed_vars->requests,
                                      &flag, MPI_STATUSES_IGNORE);
            }
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_W_Iexchange_data_fini(nbc_req, &errcode);
            }
            break;

        default:
            break;
    }

    /* --BEGIN ERROR HANDLING-- */
    if (errcode != MPI_SUCCESS) {
        errcode = MPIO_Err_create_code(MPI_SUCCESS,
                MPIR_ERR_RECOVERABLE,
                "ADIOI_GEN_iwc_poll_fn", __LINE__,
                MPI_ERR_IO, "**mpi_grequest_complete",
                0);
    }
    /* --END ERROR HANDLING-- */

    return errcode;
}

/* wait for multiple requests to complete */
static int ADIOI_GEN_iwc_wait_fn(int count, void **array_of_states,
                                 double timeout, MPI_Status *status)
{
    int i, errcode = MPI_SUCCESS;
    double starttime;
    ADIOI_NBC_Request **nbc_reqlist;

    nbc_reqlist = (ADIOI_NBC_Request **)array_of_states;

    starttime = MPI_Wtime();
    for (i = 0; i < count ; i++) {
        while (nbc_reqlist[i]->data.wr.state != ADIOI_IWC_STATE_COMPLETE) {
            errcode = ADIOI_GEN_iwc_poll_fn(nbc_reqlist[i], MPI_STATUS_IGNORE);
            /* --BEGIN ERROR HANDLING-- */
            if (errcode != MPI_SUCCESS) {
                errcode = MPIO_Err_create_code(MPI_SUCCESS,
                        MPIR_ERR_RECOVERABLE,
                        "ADIOI_GEN_iwc_wait_fn",
                        __LINE__, MPI_ERR_IO,
                        "**mpi_grequest_complete", 0);
            }
            /* --END ERROR HANDLING-- */

            if ((timeout > 0) && (timeout < (MPI_Wtime() - starttime)))
                goto fn_exit;

            /* If the progress engine is blocked, we have to yield for another
             * thread to be able to unblock the progress engine. */
            MPIR_Ext_cs_yield();
        }
    }

  fn_exit:
    return errcode;
}

