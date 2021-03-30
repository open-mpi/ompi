/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_daos.h"

#include "../../mpi-io/mpioimpl.h"
#ifdef MPIO_BUILD_PROFILING
#include "../../mpi-io/mpioprof.h"
#endif
#include "mpiu_greq.h"

enum {
    DAOS_WRITE,
    DAOS_READ
};

static MPIX_Grequest_class ADIOI_DAOS_greq_class = 0;

static void DAOS_IOContig(ADIO_File fd, void *buf, int count,
                          MPI_Datatype datatype, int file_ptr_type,
                          ADIO_Offset offset, ADIO_Status * status,
                          MPI_Request * request, int flag, int *error_code)
{
    MPI_Count datatype_size;
    uint64_t len;
    d_sg_list_t *sgl, loc_sgl;
    d_iov_t *iov, loc_iov;
    daos_size_t *nbytes, loc_nbytes;
    int ret;
    struct ADIO_DAOS_cont *cont = fd->fs_ptr;
    struct ADIO_DAOS_req *aio_req;
    static char myname[] = "ADIOI_DAOS_IOCONTIG";

    MPI_Type_size_x(datatype, &datatype_size);
    len = (ADIO_Offset) datatype_size *(ADIO_Offset) count;

    if (request) {
        aio_req = (struct ADIO_DAOS_req *) ADIOI_Calloc(sizeof(struct ADIO_DAOS_req), 1);
        daos_event_init(&aio_req->daos_event, DAOS_HDL_INVAL, NULL);

        sgl = &aio_req->sgl;
        iov = &aio_req->iov;
        nbytes = &aio_req->nbytes;

        if (ADIOI_DAOS_greq_class == 0) {
            MPIX_Grequest_class_create(ADIOI_GEN_aio_query_fn,
                                       ADIOI_DAOS_aio_free_fn, MPIU_Greq_cancel_fn,
                                       ADIOI_DAOS_aio_poll_fn, ADIOI_DAOS_aio_wait_fn,
                                       &ADIOI_DAOS_greq_class);
        }
        MPIX_Grequest_class_allocate(ADIOI_DAOS_greq_class, aio_req, request);
        memcpy(&(aio_req->req), request, sizeof(MPI_Request));
    } else {
        sgl = &loc_sgl;
        iov = &loc_iov;
        nbytes = &loc_nbytes;
    }

    if (len == 0) {
        *nbytes = 0;
        goto done;
    }

    if (file_ptr_type == ADIO_INDIVIDUAL) {
        offset = fd->fp_ind;
    }

    /** set memory location */
    sgl->sg_nr = 1;
    sgl->sg_nr_out = 0;
    d_iov_set(iov, buf, len);
    sgl->sg_iovs = iov;
#ifdef D_PRINT_IO_MEM
    printf("MEM : off %lld len %zu\n", buf, len);
#endif

#ifdef D_PRINT_IO
    int mpi_rank;

    MPI_Comm_rank(fd->comm, &mpi_rank);
    printf("(%d) CONTIG IO OP %d, Off %llu, Len %zu\n", mpi_rank, flag, offset, len);
#endif

    if (flag == DAOS_WRITE) {
        ret = dfs_write(cont->dfs, cont->obj, sgl, offset, (request ? &aio_req->daos_event : NULL));
        if (ret != 0) {
            PRINT_MSG(stderr, "dfs_write() failed with %d\n", ret);
            *error_code = ADIOI_DAOS_err(myname, cont->obj_name, __LINE__, ret);
            return;
        }
        *nbytes = len;
    } else if (flag == DAOS_READ) {
        ret = dfs_read(cont->dfs, cont->obj, sgl, offset, nbytes,
                       (request ? &aio_req->daos_event : NULL));
        if (ret != 0) {
            PRINT_MSG(stderr, "dfs_read() failed with %d\n", ret);
            *error_code = ADIOI_DAOS_err(myname, cont->obj_name, __LINE__, ret);
            return;
        }
    }

    if (file_ptr_type == ADIO_INDIVIDUAL) {
        fd->fp_ind += len;
    }

    fd->fp_sys_posn = offset + len;

  done:
#ifdef HAVE_STATUS_SET_BYTES
    if (request == NULL && status)
        MPIR_Status_set_bytes(status, datatype, *nbytes);
#endif

    *error_code = MPI_SUCCESS;
}

void ADIOI_DAOS_ReadContig(ADIO_File fd, void *buf, int count,
                           MPI_Datatype datatype, int file_ptr_type,
                           ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    DAOS_IOContig(fd, buf, count, datatype, file_ptr_type,
                  offset, status, NULL, DAOS_READ, error_code);
}

void ADIOI_DAOS_WriteContig(ADIO_File fd, const void *buf, int count,
                            MPI_Datatype datatype, int file_ptr_type,
                            ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    DAOS_IOContig(fd, (void *) buf, count, datatype, file_ptr_type,
                  offset, status, NULL, DAOS_WRITE, error_code);
}

void ADIOI_DAOS_IReadContig(ADIO_File fd, void *buf, int count,
                            MPI_Datatype datatype, int file_ptr_type,
                            ADIO_Offset offset, MPI_Request * request, int *error_code)
{
    DAOS_IOContig(fd, buf, count, datatype, file_ptr_type,
                  offset, NULL, request, DAOS_READ, error_code);
}

void ADIOI_DAOS_IWriteContig(ADIO_File fd, const void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, MPI_Request * request, int *error_code)
{
    DAOS_IOContig(fd, (void *) buf, count, datatype, file_ptr_type,
                  offset, NULL, request, DAOS_WRITE, error_code);
}

int ADIOI_DAOS_aio_free_fn(void *extra_state)
{
    struct ADIO_DAOS_req *aio_req = (struct ADIO_DAOS_req *) extra_state;

    if (aio_req->iovs)
        ADIOI_Free(aio_req->iovs);

    if (aio_req->rgs)
        ADIOI_Free(aio_req->rgs);

    ADIOI_Free(aio_req);

    return MPI_SUCCESS;
}

int ADIOI_DAOS_aio_poll_fn(void *extra_state, MPI_Status * status)
{
    struct ADIO_DAOS_req *aio_req = (struct ADIO_DAOS_req *) extra_state;;
    int ret;
    bool flag;

    /* MSC - MPICH hangs if we just test with NOWAIT.. */
    ret = daos_event_test(&aio_req->daos_event, DAOS_EQ_WAIT, &flag);
    if (ret != 0)
        return MPI_UNDEFINED;

    if (flag)
        MPI_Grequest_complete(aio_req->req);
    else
        return MPI_UNDEFINED;

    if (aio_req->daos_event.ev_error != 0)
        ret = ADIOI_DAOS_err("ADIOI_DAOS_aio_poll_fn", "DAOS Event Error", __LINE__, ret);
    else
        ret = MPI_SUCCESS;

    return ret;
}

/* wait for multiple requests to complete */
int ADIOI_DAOS_aio_wait_fn(int count, void **array_of_states, double timeout, MPI_Status * status)
{

    struct ADIO_DAOS_req **aio_reqlist;
    int i, nr_complete, ret;

    aio_reqlist = (struct ADIO_DAOS_req **) array_of_states;

    nr_complete = 0;
    while (nr_complete < count) {
        for (i = 0; i < count; i++) {
            bool flag;

            ret = daos_event_test(&aio_reqlist[i]->daos_event,
                                  (timeout > 0) ? (int64_t) timeout : DAOS_EQ_WAIT, &flag);
            if (ret != 0)
                return MPI_UNDEFINED;

            if (flag) {
                MPI_Grequest_complete(aio_reqlist[i]->req);
                nr_complete++;
            }
        }
    }
    return MPI_SUCCESS; /* TODO: no idea how to deal with errors */
}
