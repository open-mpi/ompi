/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2014 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "mpiu_greq.h"

#ifdef USE_DBG_LOGGING
  #define RDCOLL_DEBUG 1
#endif
#ifdef AGGREGATION_PROFILE
#include "mpe.h"
#endif

#ifdef HAVE_MPI_GREQUEST_EXTENSIONS

/* ADIOI_GEN_IreadStridedColl */
struct ADIOI_GEN_IreadStridedColl_vars {
    /* requests */
    MPI_Request req_offset[2];  /* ADIOI_IRC_STATE_GEN_IREADSTRIDEDCOLL */
    MPI_Request req_ind_io;     /* ADIOI_IRC_STATE_GEN_IREADSTRIDEDCOLL_INDIO */

    /* parameters */
    ADIO_File fd;
    void *buf;
    int count;
    MPI_Datatype datatype;
    int file_ptr_type;
    ADIO_Offset offset;

    /* stack variables */
    ADIOI_Access *my_req;
    /* array of nprocs structures, one for each other process in
       whose file domain this process's request lies */

    ADIOI_Access *others_req;
    /* array of nprocs structures, one for each other process
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
    ADIO_Offset *len_list;
    int *buf_idx;
};

/* ADIOI_Iread_and_exch */
struct ADIOI_Iread_and_exch_vars {
    /* requests */
    MPI_Request req1;   /* ADIOI_IRC_STATE_IREAD_AND_EXCH */
    MPI_Request req2;   /* ADIOI_IRC_STATE_IREAD_AND_EXCH_L1_BEGIN */

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
    int m;
    int ntimes;
    int max_ntimes;
    int buftype_is_contig;
    ADIO_Offset st_loc;
    ADIO_Offset end_loc;
    ADIO_Offset off;
    ADIO_Offset done;
    char *read_buf;
    int *curr_offlen_ptr;
    int *count;
    int *send_size;
    int *recv_size;
    int *partial_send;
    int *recd_from_proc;
    int *start_pos;
    /* Not convinced end_loc-st_loc couldn't be > int, so make these offsets*/
    ADIO_Offset size;
    ADIO_Offset real_size;
    ADIO_Offset for_curr_iter;
    ADIO_Offset for_next_iter;
    ADIOI_Flatlist_node *flat_buf;
    MPI_Aint buftype_extent;
    int coll_bufsize;

    /* next function to be called */
    void (*next_fn)(ADIOI_NBC_Request *, int *);
};

/* ADIOI_R_Iexchange_data */
struct ADIOI_R_Iexchange_data_vars {
    /* requests */
    MPI_Request req1;   /* ADIOI_IRC_STATE_R_IEXCHANGE_DATA */
    MPI_Request *req2;  /* ADIOI_IRC_STATE_R_IEXCHANGE_DATA_RECV & FILL */

    /* parameters */
    ADIO_File fd;
    void *buf;
    ADIOI_Flatlist_node *flat_buf;
    ADIO_Offset *offset_list;
    ADIO_Offset *len_list;
    int *send_size;
    int *recv_size;
    int *count;
    int *start_pos;
    int *partial_send;
    int *recd_from_proc;
    int nprocs;
    int myrank;
    int buftype_is_contig;
    int contig_access_count;
    ADIO_Offset min_st_offset;
    ADIO_Offset fd_size;
    ADIO_Offset *fd_start;
    ADIO_Offset *fd_end;
    ADIOI_Access *others_req;
    int iter;
    MPI_Aint buftype_extent;
    int *buf_idx;

    /* stack variables */
    int nprocs_recv;
    int nprocs_send;
    char **recv_buf;

    /* next function to be called */
    void (*next_fn)(ADIOI_NBC_Request *, int *);
};


void ADIOI_Fill_user_buffer(ADIO_File fd, void *buf, ADIOI_Flatlist_node
                   *flat_buf, char **recv_buf, ADIO_Offset
                   *offset_list, ADIO_Offset *len_list,
                   unsigned *recv_size,
                   MPI_Request *requests, MPI_Status *statuses,
                   int *recd_from_proc, int nprocs,
                   int contig_access_count,
                   ADIO_Offset min_st_offset,
                   ADIO_Offset fd_size, ADIO_Offset *fd_start,
                   ADIO_Offset *fd_end,
                   MPI_Aint buftype_extent);

/* prototypes of functions used for nonblocking collective reads only. */
static void ADIOI_GEN_IreadStridedColl_inter(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IreadStridedColl_indio(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IreadStridedColl_read(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IreadStridedColl_free(ADIOI_NBC_Request *, int *);
static void ADIOI_GEN_IreadStridedColl_fini(ADIOI_NBC_Request *, int *);

static void ADIOI_Iread_and_exch(ADIOI_NBC_Request *, int *);
static void ADIOI_Iread_and_exch_l1_begin(ADIOI_NBC_Request *, int *);
static void ADIOI_Iread_and_exch_l1_end(ADIOI_NBC_Request *, int *);
static void ADIOI_Iread_and_exch_reset(ADIOI_NBC_Request *, int *);
static void ADIOI_Iread_and_exch_l2_begin(ADIOI_NBC_Request *, int *);
static void ADIOI_Iread_and_exch_l2_end(ADIOI_NBC_Request *, int *);
static void ADIOI_Iread_and_exch_fini(ADIOI_NBC_Request *, int *);

static void ADIOI_R_Iexchange_data(ADIOI_NBC_Request *, int *);
static void ADIOI_R_Iexchange_data_recv(ADIOI_NBC_Request *, int *);
static void ADIOI_R_Iexchange_data_fill(ADIOI_NBC_Request *, int *);
static void ADIOI_R_Iexchange_data_fini(ADIOI_NBC_Request *, int *);

static MPIX_Grequest_class ADIOI_GEN_greq_class = 0;
static int ADIOI_GEN_irc_query_fn(void *extra_state, MPI_Status *status);
static int ADIOI_GEN_irc_free_fn(void *extra_state);
static int ADIOI_GEN_irc_poll_fn(void *extra_state, MPI_Status *status);
static int ADIOI_GEN_irc_wait_fn(int count, void **array_of_states,
                                 double timeout, MPI_Status *status);


/* Nonblocking version of ADIOI_GEN_ReadStridedColl() */
void ADIOI_GEN_IreadStridedColl(ADIO_File fd, void *buf, int count,
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
    ADIOI_GEN_IreadStridedColl_vars *vars = NULL;
    int nprocs, myrank;
#ifdef RDCOLL_DEBUG
    int i;
#endif

    /* FIXME: need an implementation of ADIOI_IOIstridedColl
    if (fd->hints->cb_pfr != ADIOI_HINT_DISABLE) {
        ADIOI_IOIstridedColl(fd, buf, count, ADIOI_READ, datatype,
                             file_ptr_type, offset, request, error_code);
        return;
    }
    */

    /* top-level struct keeping the status of function progress */
    nbc_req = (ADIOI_NBC_Request *)ADIOI_Calloc(1, sizeof(ADIOI_NBC_Request));
    nbc_req->rdwr = ADIOI_READ;

    /* create a generalized request */
    if (ADIOI_GEN_greq_class == 0) {
        MPIX_Grequest_class_create(ADIOI_GEN_irc_query_fn,
                ADIOI_GEN_irc_free_fn, MPIU_Greq_cancel_fn,
                ADIOI_GEN_irc_poll_fn, ADIOI_GEN_irc_wait_fn,
                &ADIOI_GEN_greq_class);
    }
    MPIX_Grequest_class_allocate(ADIOI_GEN_greq_class, nbc_req, request);
    memcpy(&nbc_req->req, request, sizeof(MPI_Request));

    /* create a struct for parameters and variables */
    vars = (ADIOI_GEN_IreadStridedColl_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_GEN_IreadStridedColl_vars));
    nbc_req->data.rd.rsc_vars = vars;

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

    /* number of aggregators, cb_nodes, is stored in the hints */
    vars->nprocs_for_coll = fd->hints->cb_nodes;
    vars->orig_fp = fd->fp_ind;

    /* only check for interleaving if cb_read isn't disabled */
    if (fd->hints->cb_read != ADIOI_HINT_DISABLE) {
        /* For this process's request, calculate the list of offsets and
           lengths in the file and determine the start and end offsets. */

        /* Note: end_offset points to the last byte-offset that will be accessed.
           e.g., if start_offset=0 and 100 bytes to be read, end_offset=99*/

        ADIOI_Calc_my_off_len(fd, count, datatype, file_ptr_type, offset,
                              &vars->offset_list, &vars->len_list,
                              &vars->start_offset, &vars->end_offset,
                              &vars->contig_access_count);

#ifdef RDCOLL_DEBUG
        for (i = 0; i < vars->contig_access_count; i++) {
            DBG_FPRINTF(stderr, "rank %d  off %lld  len %lld\n",
                        myrank, vars->offset_list[i], vars->len_list[i]);
        }
#endif

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

        nbc_req->data.rd.state = ADIOI_IRC_STATE_GEN_IREADSTRIDEDCOLL;
        return;
    }

    ADIOI_GEN_IreadStridedColl_indio(nbc_req, error_code);
}

static void ADIOI_GEN_IreadStridedColl_inter(ADIOI_NBC_Request *nbc_req,
                                             int *error_code)
{
    ADIOI_GEN_IreadStridedColl_vars *vars = nbc_req->data.rd.rsc_vars;
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

    ADIOI_GEN_IreadStridedColl_indio(nbc_req, error_code);
}

static void ADIOI_GEN_IreadStridedColl_indio(ADIOI_NBC_Request *nbc_req,
                                             int *error_code)
{
    ADIOI_GEN_IreadStridedColl_vars *vars = nbc_req->data.rd.rsc_vars;
    ADIOI_Icalc_others_req_vars *cor_vars = NULL;
    ADIO_File fd = vars->fd;
    void *buf;
    int count, file_ptr_type;
    MPI_Datatype datatype = vars->datatype;
    ADIO_Offset offset;
    int filetype_is_contig;
    ADIO_Offset off;
    int nprocs;

    ADIOI_Datatype_iscontig(datatype, &vars->buftype_is_contig);

    if (fd->hints->cb_read == ADIOI_HINT_DISABLE
    || (!vars->interleave_count && (fd->hints->cb_read == ADIOI_HINT_AUTO)))
    {
        buf = vars->buf;
        count = vars->count;
        file_ptr_type = vars->file_ptr_type;
        offset = vars->offset;

        /* don't do aggregation */
        if (fd->hints->cb_read != ADIOI_HINT_DISABLE) {
            ADIOI_Free(vars->offset_list);
            ADIOI_Free(vars->len_list);
            ADIOI_Free(vars->st_offsets);
            ADIOI_Free(vars->end_offsets);
        }

        fd->fp_ind = vars->orig_fp;
        ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);

        if (vars->buftype_is_contig && filetype_is_contig) {
            if (file_ptr_type == ADIO_EXPLICIT_OFFSET) {
                off = fd->disp + (fd->etype_size) * offset;
                ADIO_IreadContig(fd, buf, count, datatype, ADIO_EXPLICIT_OFFSET,
                                 off, &vars->req_ind_io, error_code);
            }
            else ADIO_IreadContig(fd, buf, count, datatype, ADIO_INDIVIDUAL,
                                  0, &vars->req_ind_io, error_code);
        }
        else {
            ADIO_IreadStrided(fd, buf, count, datatype, file_ptr_type,
                              offset, &vars->req_ind_io, error_code);
        }

        nbc_req->data.rd.state = ADIOI_IRC_STATE_GEN_IREADSTRIDEDCOLL_INDIO;
        return;
    }

    nprocs = vars->nprocs;

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
    ADIOI_Calc_file_domains(vars->st_offsets, vars->end_offsets, nprocs,
                vars->nprocs_for_coll, &vars->min_st_offset,
                &vars->fd_start, &vars->fd_end,
                fd->hints->min_fdomain_size, &vars->fd_size,
                fd->hints->striping_unit);

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
    ADIOI_Calc_my_req(fd, vars->offset_list, vars->len_list,
              vars->contig_access_count, vars->min_st_offset,
              vars->fd_start, vars->fd_end, vars->fd_size,
              nprocs, &vars->count_my_req_procs,
              &vars->count_my_req_per_proc, &vars->my_req,
              &vars->buf_idx);

    /* perform a collective communication in order to distribute the
     * data calculated above.  fills in the following:
     * count_others_req_procs - number of processes (including this
     *     one) which have requests in this process's file domain.
     * count_others_req_per_proc[] - number of separate contiguous
     *     requests from proc i lie in this process's file domain.
     */

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
    cor_vars->next_fn = ADIOI_GEN_IreadStridedColl_read;

    ADIOI_Icalc_others_req(nbc_req, error_code);
}

static void ADIOI_GEN_IreadStridedColl_read(ADIOI_NBC_Request *nbc_req,
                                            int *error_code)
{
    ADIOI_GEN_IreadStridedColl_vars *vars = nbc_req->data.rd.rsc_vars;
    ADIOI_Iread_and_exch_vars *rae_vars = NULL;
    ADIOI_Access *my_req = vars->my_req;
    int nprocs = vars->nprocs;
    int i;

    /* my_req[] and count_my_req_per_proc aren't needed at this point, so
     * let's free the memory
     */
    ADIOI_Free(vars->count_my_req_per_proc);
    for (i = 0; i < nprocs; i++) {
        if (my_req[i].count) {
            ADIOI_Free(my_req[i].offsets);
            ADIOI_Free(my_req[i].lens);
        }
    }
    ADIOI_Free(my_req);

    /* read data in sizes of no more than ADIOI_Coll_bufsize,
     * communicate, and fill user buf.
     */
    rae_vars = (ADIOI_Iread_and_exch_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_Iread_and_exch_vars));
    nbc_req->data.rd.rae_vars = rae_vars;
    rae_vars->fd = vars->fd;
    rae_vars->buf = vars->buf;
    rae_vars->datatype = vars->datatype;
    rae_vars->nprocs = vars->nprocs;
    rae_vars->myrank = vars->myrank;
    rae_vars->others_req = vars->others_req;
    rae_vars->offset_list = vars->offset_list;
    rae_vars->len_list = vars->len_list;
    rae_vars->contig_access_count = vars->contig_access_count;
    rae_vars->min_st_offset = vars->min_st_offset;
    rae_vars->fd_size = vars->fd_size;
    rae_vars->fd_start = vars->fd_start;
    rae_vars->fd_end = vars->fd_end;
    rae_vars->buf_idx = vars->buf_idx;
    rae_vars->next_fn = ADIOI_GEN_IreadStridedColl_free;

    ADIOI_Iread_and_exch(nbc_req, error_code);
}

static void ADIOI_GEN_IreadStridedColl_free(ADIOI_NBC_Request *nbc_req,
                                            int *error_code)
{
    ADIOI_GEN_IreadStridedColl_vars *vars = nbc_req->data.rd.rsc_vars;
    ADIO_File fd = vars->fd;
    MPI_Datatype datatype = vars->datatype;
    ADIOI_Access *others_req = vars->others_req;
    int nprocs = vars->nprocs;
    int i;

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

    ADIOI_GEN_IreadStridedColl_fini(nbc_req, error_code);
}

static void ADIOI_GEN_IreadStridedColl_fini(ADIOI_NBC_Request *nbc_req,
                                            int *error_code)
{
    ADIOI_GEN_IreadStridedColl_vars *vars = nbc_req->data.rd.rsc_vars;
    MPI_Count size;

    /* This is a temporary way of filling in status. The right way is to
       keep track of how much data was actually read and placed in buf
       during collective I/O. */
    MPI_Type_size_x(vars->datatype, &size);
    nbc_req->nbytes = size * vars->count;

    /* free the struct for parameters and variables */
    if (nbc_req->data.rd.rsc_vars) {
        ADIOI_Free(nbc_req->data.rd.rsc_vars);
        nbc_req->data.rd.rsc_vars = NULL;
    }

    /* make the request complete */
    *error_code = MPI_Grequest_complete(nbc_req->req);
    nbc_req->data.rd.state = ADIOI_IRC_STATE_COMPLETE;
}


static void ADIOI_Iread_and_exch(ADIOI_NBC_Request *nbc_req, int *error_code)
{
    ADIOI_Iread_and_exch_vars *vars = nbc_req->data.rd.rae_vars;
    ADIO_File fd = vars->fd;
    MPI_Datatype datatype = vars->datatype;
    int nprocs = vars->nprocs;
    ADIOI_Access *others_req = vars->others_req;

    /* Read in sizes of no more than coll_bufsize, an info parameter.
       Send data to appropriate processes.
       Place recd. data in user buf.
       The idea is to reduce the amount of extra memory required for
       collective I/O. If all data were read all at once, which is much
       easier, it would require temp space more than the size of user_buf,
       which is often unacceptable. For example, to read a distributed
       array from a file, where each local array is 8Mbytes, requiring
       at least another 8Mbytes of temp space is unacceptable. */

    int i, j;
    ADIO_Offset st_loc = -1, end_loc = -1;
    ADIOI_Flatlist_node *flat_buf = NULL;
    int coll_bufsize;

    *error_code = MPI_SUCCESS;  /* changed below if error */
    /* only I/O errors are currently reported */

    /* calculate the number of reads of size coll_bufsize
       to be done by each process and the max among all processes.
       That gives the no. of communication phases as well.
       coll_bufsize is obtained from the hints object. */

    coll_bufsize = fd->hints->cb_buffer_size;
    vars->coll_bufsize = coll_bufsize;

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
            st_loc = ADIOI_MIN(st_loc, others_req[i].offsets[j]);
            end_loc = ADIOI_MAX(end_loc, (others_req[i].offsets[j]
                          + others_req[i].lens[j] - 1));
        }

    vars->st_loc = st_loc;
    vars->end_loc = end_loc;

    /* calculate ntimes, the number of times this process must perform I/O
     * operations in order to complete all the requests it has received.
     * the need for multiple I/O operations comes from the restriction that
     * we only use coll_bufsize bytes of memory for internal buffering.
     */
    if ((st_loc == -1) && (end_loc == -1)) {
        /* this process does no I/O. */
        vars->ntimes = 0;
    }
    else {
        /* ntimes=ceiling_div(end_loc - st_loc + 1, coll_bufsize)*/
        vars->ntimes = (int)((end_loc - st_loc + coll_bufsize) / coll_bufsize);
    }

    *error_code = MPI_Iallreduce(&vars->ntimes, &vars->max_ntimes, 1, MPI_INT,
                                 MPI_MAX, fd->comm, &vars->req1);

    vars->read_buf = fd->io_buf;  /* Allocated at open time */

    vars->curr_offlen_ptr = (int *)ADIOI_Calloc(nprocs, sizeof(int));
    /* its use is explained below. calloc initializes to 0. */

    vars->count = (int *)ADIOI_Malloc(nprocs * sizeof(int));
    /* to store count of how many off-len pairs per proc are satisfied
       in an iteration. */

    vars->partial_send = (int *)ADIOI_Calloc(nprocs, sizeof(int));
    /* if only a portion of the last off-len pair is sent to a process
       in a particular iteration, the length sent is stored here.
       calloc initializes to 0. */

    vars->send_size = (int *)ADIOI_Malloc(nprocs * sizeof(int));
    /* total size of data to be sent to each proc. in an iteration */

    vars->recv_size = (int *)ADIOI_Malloc(nprocs * sizeof(int));
    /* total size of data to be recd. from each proc. in an iteration.
       Of size nprocs so that I can use MPI_Alltoall later. */

    vars->recd_from_proc = (int *)ADIOI_Calloc(nprocs, sizeof(int));
    /* amount of data recd. so far from each proc. Used in
       ADIOI_Fill_user_buffer. initialized to 0 here. */

    vars->start_pos = (int *)ADIOI_Malloc(nprocs*sizeof(int));
    /* used to store the starting value of curr_offlen_ptr[i] in
       this iteration */

    ADIOI_Datatype_iscontig(datatype, &vars->buftype_is_contig);
    if (!vars->buftype_is_contig) {
        ADIOI_Flatten_datatype(datatype);
        flat_buf = ADIOI_Flatlist;
        while (flat_buf->type != datatype) flat_buf = flat_buf->next;
        vars->flat_buf = flat_buf;
    }
    MPI_Type_extent(datatype, &vars->buftype_extent);

    vars->done = 0;
    vars->off = st_loc;
    vars->for_curr_iter = vars->for_next_iter = 0;

    /* set the state to wait until MPI_Ialltoall finishes. */
    nbc_req->data.rd.state = ADIOI_IRC_STATE_IREAD_AND_EXCH;
}

static void ADIOI_Iread_and_exch_l1_begin(ADIOI_NBC_Request *nbc_req,
                                          int *error_code)
{
    ADIOI_Iread_and_exch_vars *vars = nbc_req->data.rd.rae_vars;
    ADIO_File fd;
    int nprocs;
    ADIOI_Access *others_req;

    int i, j;
    ADIO_Offset real_off, req_off;
    char *read_buf;
    int *curr_offlen_ptr, *count, *send_size;
    int *partial_send, *start_pos;
    ADIO_Offset size, real_size, for_next_iter;
    int req_len, flag;

    ADIOI_R_Iexchange_data_vars *red_vars = NULL;

    /* loop exit condition */
    if (vars->m >= vars->ntimes) {
        ADIOI_Iread_and_exch_reset(nbc_req, error_code);
        return;
    }

    fd = vars->fd;
    nprocs = vars->nprocs;
    others_req = vars->others_req;

    read_buf = vars->read_buf;
    curr_offlen_ptr = vars->curr_offlen_ptr;
    count = vars->count;
    send_size = vars->send_size;
    partial_send = vars->partial_send;
    start_pos = vars->start_pos;

    /* read buf of size coll_bufsize (or less) */
    /* go through all others_req and check if any are satisfied
       by the current read */

    /* since MPI guarantees that displacements in filetypes are in
       monotonically nondecreasing order, I can maintain a pointer
       (curr_offlen_ptr) to
       current off-len pair for each process in others_req and scan
       further only from there. There is still a problem of filetypes
       such as:  (1, 2, 3 are not process nos. They are just numbers for
       three chunks of data, specified by a filetype.)

       1  -------!--
       2    -----!----
       3       --!-----

       where ! indicates where the current read_size limitation cuts
       through the filetype.  I resolve this by reading up to !, but
       filling the communication buffer only for 1. I copy the portion
       left over for 2 into a tmp_buf for use in the next
       iteration. i.e., 2 and 3 will be satisfied in the next
       iteration. This simplifies filling in the user's buf at the
       other end, as only one off-len pair with incomplete data
       will be sent. I also don't need to send the individual
       offsets and lens along with the data, as the data is being
       sent in a particular order. */

    /* off = start offset in the file for the data actually read in
             this iteration
       size = size of data read corresponding to off
       real_off = off minus whatever data was retained in memory from
             previous iteration for cases like 2, 3 illustrated above
       real_size = size plus the extra corresponding to real_off
       req_off = off in file for a particular contiguous request
                 minus what was satisfied in previous iteration
       req_size = size corresponding to req_off */

    size = ADIOI_MIN((unsigned)vars->coll_bufsize,
                     vars->end_loc - vars->st_loc + 1 - vars->done);
    real_off = vars->off - vars->for_curr_iter;
    real_size = size + vars->for_curr_iter;

    vars->size = size;
    vars->real_size = real_size;

    for (i = 0; i < nprocs; i++) count[i] = send_size[i] = 0;
    for_next_iter = 0;

    for (i = 0; i < nprocs; i++) {
#ifdef RDCOLL_DEBUG
        DBG_FPRINTF(stderr, "rank %d, i %d, others_count %d\n",
                    vars->myrank, i, others_req[i].count);
#endif
        if (others_req[i].count) {
            start_pos[i] = curr_offlen_ptr[i];
            for (j = curr_offlen_ptr[i]; j < others_req[i].count; j++) {
                if (partial_send[i]) {
                    /* this request may have been partially
                       satisfied in the previous iteration. */
                    req_off = others_req[i].offsets[j] + partial_send[i];
                    req_len = others_req[i].lens[j] - partial_send[i];
                    partial_send[i] = 0;
                    /* modify the off-len pair to reflect this change */
                    others_req[i].offsets[j] = req_off;
                    others_req[i].lens[j] = req_len;
                }
                else {
                    req_off = others_req[i].offsets[j];
                    req_len = others_req[i].lens[j];
                }
                if (req_off < real_off + real_size) {
                    count[i]++;
                    ADIOI_Assert((((ADIO_Offset)(MPIR_Upint)read_buf) + req_off - real_off) == (ADIO_Offset)(MPIR_Upint)(read_buf + req_off - real_off));
                    MPI_Address(read_buf + req_off - real_off,
                                &(others_req[i].mem_ptrs[j]));
                    ADIOI_Assert((real_off + real_size - req_off) == (int)(real_off + real_size - req_off));
                    send_size[i] += (int)(ADIOI_MIN(real_off + real_size - req_off,
                                                    (ADIO_Offset)(unsigned)req_len));

                    if (real_off + real_size - req_off < (ADIO_Offset)(unsigned)req_len) {
                        partial_send[i] = (int)(real_off + real_size - req_off);
                        if ((j+1 < others_req[i].count) &&
                            (others_req[i].offsets[j+1] < real_off + real_size)) {
                            /* this is the case illustrated in the
                               figure above. */
                            for_next_iter = ADIOI_MAX(for_next_iter,
                                    real_off + real_size - others_req[i].offsets[j+1]);
                            /* max because it must cover requests
                               from different processes */
                        }
                        break;
                    }
                }
                else break;
            }
            curr_offlen_ptr[i] = j;
        }
    }
    vars->for_next_iter = for_next_iter;

    flag = 0;
    for (i = 0; i < nprocs; i++)
        if (count[i]) flag = 1;

    /* create a struct for ADIOI_R_Iexchange_data() */
    red_vars = (ADIOI_R_Iexchange_data_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_R_Iexchange_data_vars));
    nbc_req->data.rd.red_vars = red_vars;
    red_vars->fd = vars->fd;
    red_vars->buf = vars->buf;
    red_vars->flat_buf = vars->flat_buf;
    red_vars->offset_list = vars->offset_list;
    red_vars->len_list = vars->len_list;
    red_vars->send_size = vars->send_size;
    red_vars->recv_size = vars->recv_size;
    red_vars->count = vars->count;
    red_vars->start_pos = vars->start_pos;
    red_vars->partial_send = vars->partial_send;
    red_vars->recd_from_proc = vars->recd_from_proc;
    red_vars->nprocs = vars->nprocs;
    red_vars->myrank = vars->myrank;
    red_vars->buftype_is_contig = vars->buftype_is_contig;
    red_vars->contig_access_count = vars->contig_access_count;
    red_vars->min_st_offset = vars->min_st_offset;
    red_vars->fd_size = vars->fd_size;
    red_vars->fd_start = vars->fd_start;
    red_vars->fd_end = vars->fd_end;
    red_vars->others_req = vars->others_req;
    red_vars->iter = vars->m;
    red_vars->buftype_extent = vars->buftype_extent;
    red_vars->buf_idx = vars->buf_idx;
    red_vars->next_fn = ADIOI_Iread_and_exch_l1_end;

    if (flag) {
        ADIOI_Assert(size == (int)size);
        ADIO_IreadContig(fd, read_buf+vars->for_curr_iter, (int)size,
                         MPI_BYTE, ADIO_EXPLICIT_OFFSET, vars->off,
                         &vars->req2, error_code);

        nbc_req->data.rd.state = ADIOI_IRC_STATE_IREAD_AND_EXCH_L1_BEGIN;
        return;
    }

    ADIOI_R_Iexchange_data(nbc_req, error_code);
}

static void ADIOI_Iread_and_exch_l1_end(ADIOI_NBC_Request *nbc_req,
                                        int *error_code)
{
    ADIOI_Iread_and_exch_vars *vars = nbc_req->data.rd.rae_vars;
    ADIO_File fd = vars->fd;
    ADIO_Offset size = vars->size;
    ADIO_Offset real_size = vars->real_size;
    ADIO_Offset for_next_iter = vars->for_next_iter;
    char *read_buf = vars->read_buf;
    char *tmp_buf;

    vars->for_curr_iter = for_next_iter;

    if (for_next_iter) {
        tmp_buf = (char *)ADIOI_Malloc(for_next_iter);
        ADIOI_Assert((((ADIO_Offset)(MPIR_Upint)read_buf)+real_size-for_next_iter) == (ADIO_Offset)(MPIR_Upint)(read_buf+real_size-for_next_iter));
        ADIOI_Assert((for_next_iter+vars->coll_bufsize) == (size_t)(for_next_iter+vars->coll_bufsize));
        memcpy(tmp_buf, read_buf+real_size-for_next_iter, for_next_iter);
        ADIOI_Free(fd->io_buf);
        fd->io_buf = (char *)ADIOI_Malloc(for_next_iter+vars->coll_bufsize);
        memcpy(fd->io_buf, tmp_buf, for_next_iter);
        vars->read_buf = fd->io_buf;
        ADIOI_Free(tmp_buf);
    }

    vars->off += size;
    vars->done += size;

    /* increment m and go back to the beginning of m loop */
    vars->m++;
    ADIOI_Iread_and_exch_l1_begin(nbc_req, error_code);
}

static void ADIOI_Iread_and_exch_reset(ADIOI_NBC_Request *nbc_req,
                                       int *error_code)
{
    ADIOI_Iread_and_exch_vars *vars = nbc_req->data.rd.rae_vars;
    int nprocs = vars->nprocs;
    int *count = vars->count;
    int *send_size = vars->send_size;
    int i;

    for (i = 0; i < nprocs; i++) count[i] = send_size[i] = 0;

    vars->m = vars->ntimes;
    ADIOI_Iread_and_exch_l2_begin(nbc_req, error_code);
}

static void ADIOI_Iread_and_exch_l2_begin(ADIOI_NBC_Request *nbc_req,
                                          int *error_code)
{
    ADIOI_Iread_and_exch_vars *vars = nbc_req->data.rd.rae_vars;
    ADIOI_R_Iexchange_data_vars *red_vars = NULL;

    /* loop exit condition */
    if (vars->m >= vars->max_ntimes) {
        ADIOI_Iread_and_exch_fini(nbc_req, error_code);
        return;
    }

    /* create a struct for ADIOI_R_Iexchange_data() */
    red_vars = (ADIOI_R_Iexchange_data_vars *)ADIOI_Calloc(
            1, sizeof(ADIOI_R_Iexchange_data_vars));
    nbc_req->data.rd.red_vars = red_vars;
    red_vars->fd = vars->fd;
    red_vars->buf = vars->buf;
    red_vars->flat_buf = vars->flat_buf;
    red_vars->offset_list = vars->offset_list;
    red_vars->len_list = vars->len_list;
    red_vars->send_size = vars->send_size;
    red_vars->recv_size = vars->recv_size;
    red_vars->count = vars->count;
    red_vars->start_pos = vars->start_pos;
    red_vars->partial_send = vars->partial_send;
    red_vars->recd_from_proc = vars->recd_from_proc;
    red_vars->nprocs = vars->nprocs;
    red_vars->myrank = vars->myrank;
    red_vars->buftype_is_contig = vars->buftype_is_contig;
    red_vars->contig_access_count = vars->contig_access_count;
    red_vars->min_st_offset = vars->min_st_offset;
    red_vars->fd_size = vars->fd_size;
    red_vars->fd_start = vars->fd_start;
    red_vars->fd_end = vars->fd_end;
    red_vars->others_req = vars->others_req;
    red_vars->iter = vars->m;
    red_vars->buftype_extent = vars->buftype_extent;
    red_vars->buf_idx = vars->buf_idx;
    red_vars->next_fn = ADIOI_Iread_and_exch_l2_end;

    ADIOI_R_Iexchange_data(nbc_req, error_code);
}

static void ADIOI_Iread_and_exch_l2_end(ADIOI_NBC_Request *nbc_req,
                                        int *error_code)
{
    ADIOI_Iread_and_exch_vars *vars = nbc_req->data.rd.rae_vars;

    vars->m++;
    ADIOI_Iread_and_exch_l2_begin(nbc_req, error_code);
}

static void ADIOI_Iread_and_exch_fini(ADIOI_NBC_Request *nbc_req, int *error_code)
{
    ADIOI_Iread_and_exch_vars *vars = nbc_req->data.rd.rae_vars;
    void (*next_fn)(ADIOI_NBC_Request *, int *);

    ADIOI_Free(vars->curr_offlen_ptr);
    ADIOI_Free(vars->count);
    ADIOI_Free(vars->partial_send);
    ADIOI_Free(vars->send_size);
    ADIOI_Free(vars->recv_size);
    ADIOI_Free(vars->recd_from_proc);
    ADIOI_Free(vars->start_pos);

    next_fn = vars->next_fn;

    /* free the struct for parameters and variables */
    ADIOI_Free(nbc_req->data.rd.rae_vars);
    nbc_req->data.rd.rae_vars = NULL;

    /* move to the next function */
    next_fn(nbc_req, error_code);
}


static void ADIOI_R_Iexchange_data(ADIOI_NBC_Request *nbc_req, int *error_code)
{
    ADIOI_R_Iexchange_data_vars *vars = nbc_req->data.rd.red_vars;

    /* exchange send_size info so that each process knows how much to
       receive from whom and how much memory to allocate. */
    *error_code = MPI_Ialltoall(vars->send_size, 1, MPI_INT, vars->recv_size, 1,
                                MPI_INT, vars->fd->comm, &vars->req1);

    nbc_req->data.rd.state = ADIOI_IRC_STATE_R_IEXCHANGE_DATA;
}

static void ADIOI_R_Iexchange_data_recv(ADIOI_NBC_Request *nbc_req,
                                        int *error_code)
{
    ADIOI_R_Iexchange_data_vars *vars = nbc_req->data.rd.red_vars;
    ADIO_File fd = vars->fd;
    int *send_size = vars->send_size;
    int *recv_size = vars->recv_size;
    int *count = vars->count;
    int *start_pos = vars->start_pos;
    int *partial_send = vars->partial_send;
    int nprocs = vars->nprocs;
    int myrank = vars->myrank;
    ADIOI_Access *others_req = vars->others_req;
    int iter = vars->iter;
    int *buf_idx = vars->buf_idx;

    int i, j, k = 0, tmp = 0, nprocs_recv, nprocs_send;
    char **recv_buf = NULL;
    MPI_Datatype send_type;

    nprocs_recv = 0;
    for (i = 0; i < nprocs; i++) if (recv_size[i]) nprocs_recv++;
    vars->nprocs_recv = nprocs_recv;

    nprocs_send = 0;
    for (i = 0; i < nprocs; i++) if (send_size[i]) nprocs_send++;
    vars->nprocs_send = nprocs_send;

    vars->req2 = (MPI_Request *)
        ADIOI_Malloc((nprocs_send+nprocs_recv+1)*sizeof(MPI_Request));
    /* +1 to avoid a 0-size malloc */

    /* post recvs. if buftype_is_contig, data can be directly recd. into
       user buf at location given by buf_idx. else use recv_buf. */

#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5032, 0, NULL);
#endif

    if (vars->buftype_is_contig) {
        j = 0;
        for (i = 0; i < nprocs; i++)
            if (recv_size[i]) {
                MPI_Irecv(((char *)vars->buf) + buf_idx[i], recv_size[i],
                          MPI_BYTE, i, myrank+i+100*iter, fd->comm,
                          vars->req2 + j);
                j++;
                buf_idx[i] += recv_size[i];
            }
    }
    else {
        /* allocate memory for recv_buf and post receives */
        recv_buf = (char **) ADIOI_Malloc(nprocs * sizeof(char*));
        vars->recv_buf = recv_buf;
        for (i = 0; i < nprocs; i++)
            if (recv_size[i]) recv_buf[i] = (char *)ADIOI_Malloc(recv_size[i]);

        j = 0;
        for (i = 0; i < nprocs; i++)
            if (recv_size[i]) {
                MPI_Irecv(recv_buf[i], recv_size[i], MPI_BYTE, i,
                          myrank+i+100*iter, fd->comm,
                          vars->req2 + j);
                j++;
#ifdef RDCOLL_DEBUG
                DBG_FPRINTF(stderr, "node %d, recv_size %d, tag %d \n",
                            myrank, recv_size[i], myrank+i+100*iter);
#endif
            }
    }

    /* create derived datatypes and send data */

    j = 0;
    for (i = 0; i < nprocs; i++) {
        if (send_size[i]) {
            /* take care if the last off-len pair is a partial send */
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
            MPI_Isend(MPI_BOTTOM, 1, send_type, i, myrank+i+100*iter,
                      fd->comm, vars->req2 + nprocs_recv + j);
            MPI_Type_free(&send_type);
            if (partial_send[i]) others_req[i].lens[k] = tmp;
            j++;
        }
    }

    /* wait on the receives */
    if (nprocs_recv) {
        nbc_req->data.rd.state = ADIOI_IRC_STATE_R_IEXCHANGE_DATA_RECV;
        return;
    }

    ADIOI_R_Iexchange_data_fill(nbc_req, error_code);
}

static void ADIOI_R_Iexchange_data_fill(ADIOI_NBC_Request *nbc_req,
                                        int *error_code)
{
    ADIOI_R_Iexchange_data_vars *vars = nbc_req->data.rd.red_vars;

    if (vars->nprocs_recv) {
        /* if noncontiguous, to the copies from the recv buffers */
        if (!vars->buftype_is_contig)
            ADIOI_Fill_user_buffer(vars->fd, vars->buf, vars->flat_buf,
                    vars->recv_buf, vars->offset_list, vars->len_list,
                    (unsigned*)vars->recv_size,
                    vars->req2, NULL, vars->recd_from_proc,
                    vars->nprocs, vars->contig_access_count,
                    vars->min_st_offset, vars->fd_size, vars->fd_start,
                    vars->fd_end, vars->buftype_extent);
    }

    nbc_req->data.rd.state = ADIOI_IRC_STATE_R_IEXCHANGE_DATA_FILL;
}

static void ADIOI_R_Iexchange_data_fini(ADIOI_NBC_Request *nbc_req, int *error_code)
{
    ADIOI_R_Iexchange_data_vars *vars = nbc_req->data.rd.red_vars;
    void (*next_fn)(ADIOI_NBC_Request *, int *);
    int i;

    ADIOI_Free(vars->req2);

    if (!vars->buftype_is_contig) {
        for (i = 0; i < vars->nprocs; i++)
            if (vars->recv_size[i]) ADIOI_Free(vars->recv_buf[i]);
        ADIOI_Free(vars->recv_buf);
    }
#ifdef AGGREGATION_PROFILE
    MPE_Log_event (5033, 0, NULL);
#endif

    next_fn = vars->next_fn;

    /* free the structure for parameters and variables */
    ADIOI_Free(vars);
    nbc_req->data.rd.red_vars = NULL;

    /* move to the next function */
    next_fn(nbc_req, error_code);
}


static int ADIOI_GEN_irc_query_fn(void *extra_state, MPI_Status *status)
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

static int ADIOI_GEN_irc_free_fn(void *extra_state)
{
    ADIOI_NBC_Request *nbc_req;

    nbc_req = (ADIOI_NBC_Request *)extra_state;
    ADIOI_Free(nbc_req);

    return MPI_SUCCESS;
}

static int ADIOI_GEN_irc_poll_fn(void *extra_state, MPI_Status *status)
{
    ADIOI_NBC_Request *nbc_req;
    ADIOI_GEN_IreadStridedColl_vars *rsc_vars = NULL;
    ADIOI_Icalc_others_req_vars     *cor_vars = NULL;
    ADIOI_Iread_and_exch_vars       *rae_vars = NULL;
    ADIOI_R_Iexchange_data_vars     *red_vars = NULL;
    int errcode = MPI_SUCCESS;
    int flag;

    nbc_req = (ADIOI_NBC_Request *)extra_state;

    switch (nbc_req->data.rd.state) {
        case ADIOI_IRC_STATE_GEN_IREADSTRIDEDCOLL:
            rsc_vars = nbc_req->data.rd.rsc_vars;
            errcode = MPI_Testall(2, rsc_vars->req_offset, &flag,
                                  MPI_STATUSES_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_GEN_IreadStridedColl_inter(nbc_req, &errcode);
            }
            break;

        case ADIOI_IRC_STATE_GEN_IREADSTRIDEDCOLL_INDIO:
            rsc_vars = nbc_req->data.rd.rsc_vars;
            errcode = MPI_Test(&rsc_vars->req_ind_io, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                /* call the last function */
                ADIOI_GEN_IreadStridedColl_fini(nbc_req, &errcode);
            }
            break;

        case ADIOI_IRC_STATE_ICALC_OTHERS_REQ:
            cor_vars = nbc_req->cor_vars;
            errcode = MPI_Test(&cor_vars->req1, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_Icalc_others_req_main(nbc_req, &errcode);
            }
            break;

        case ADIOI_IRC_STATE_ICALC_OTHERS_REQ_MAIN:
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

        case ADIOI_IRC_STATE_IREAD_AND_EXCH:
            rae_vars = nbc_req->data.rd.rae_vars;
            errcode = MPI_Test(&rae_vars->req1, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                rae_vars->m = 0;
                ADIOI_Iread_and_exch_l1_begin(nbc_req, &errcode);
            }
            break;

        case ADIOI_IRC_STATE_IREAD_AND_EXCH_L1_BEGIN:
            rae_vars = nbc_req->data.rd.rae_vars;
            errcode = MPI_Test(&rae_vars->req2, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_R_Iexchange_data(nbc_req, &errcode);
            }
            break;

        case ADIOI_IRC_STATE_R_IEXCHANGE_DATA:
            red_vars = nbc_req->data.rd.red_vars;
            errcode = MPI_Test(&red_vars->req1, &flag, MPI_STATUS_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_R_Iexchange_data_recv(nbc_req, &errcode);
            }
            break;

        case ADIOI_IRC_STATE_R_IEXCHANGE_DATA_RECV:
            red_vars = nbc_req->data.rd.red_vars;
            errcode = MPI_Testall(red_vars->nprocs_recv, red_vars->req2, &flag,
                                  MPI_STATUSES_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_R_Iexchange_data_fill(nbc_req, &errcode);
            }
            break;

        case ADIOI_IRC_STATE_R_IEXCHANGE_DATA_FILL:
            red_vars = nbc_req->data.rd.red_vars;
            errcode = MPI_Testall(red_vars->nprocs_send,
                                  red_vars->req2 + red_vars->nprocs_recv,
                                  &flag, MPI_STATUSES_IGNORE);
            if (errcode == MPI_SUCCESS && flag) {
                ADIOI_R_Iexchange_data_fini(nbc_req, &errcode);
            }
            break;

        default:
            break;
    }

    /* --BEGIN ERROR HANDLING-- */
    if (errcode != MPI_SUCCESS) {
        errcode = MPIO_Err_create_code(MPI_SUCCESS,
                MPIR_ERR_RECOVERABLE,
                "ADIOI_GEN_irc_poll_fn", __LINE__,
                MPI_ERR_IO, "**mpi_grequest_complete",
                0);
    }
    /* --END ERROR HANDLING-- */

    return errcode;
}

/* wait for multiple requests to complete */
static int ADIOI_GEN_irc_wait_fn(int count, void **array_of_states,
                                 double timeout, MPI_Status *status)
{
    int i, errcode = MPI_SUCCESS;
    double starttime;
    ADIOI_NBC_Request **nbc_reqlist;

    nbc_reqlist = (ADIOI_NBC_Request **)array_of_states;

    starttime = MPI_Wtime();
    for (i = 0; i < count ; i++) {
        while (nbc_reqlist[i]->data.rd.state != ADIOI_IRC_STATE_COMPLETE) {
            errcode = ADIOI_GEN_irc_poll_fn(nbc_reqlist[i], MPI_STATUS_IGNORE);
            /* --BEGIN ERROR HANDLING-- */
            if (errcode != MPI_SUCCESS) {
                errcode = MPIO_Err_create_code(MPI_SUCCESS,
                        MPIR_ERR_RECOVERABLE,
                        "ADIOI_GEN_irc_wait_fn",
                        __LINE__, MPI_ERR_IO,
                        "**mpi_grequest_complete", 0);
            }
            /* --END ERROR HANDLING-- */

            if ((timeout > 0) && (timeout < (MPI_Wtime() - starttime)))
                goto fn_exit;
        }
    }

  fn_exit:
    return errcode;
}

#endif /* HAVE_MPI_GREQUEST_EXTENSIONS */
