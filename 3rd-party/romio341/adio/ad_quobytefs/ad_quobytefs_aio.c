/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#define READ 0
#define WRITE 1
#define QUOBYTE_CONCURRENT_REQS 8

#include "ad_quobytefs.h"
#include "mpiu_greq.h"

static int ADIOI_QUOBYTEFS_greq_class = 0;
int global_quobyte_io_context;

int ADIOI_QUOBYTEFS_aio_free_fn(void *extra_state);
int ADIOI_QUOBYTEFS_aio_poll_fn(void *extra_state, MPI_Status * status);
int ADIOI_QUOBYTEFS_aio_wait_fn(int count, void **array_of_states, double timeout,
                                MPI_Status * status);

static void quobyte_io_event_finished(void *event, int ret)
{
    struct quobyte_io_event *aio_event = (struct quobyte_io_event *) event;
    aio_event->result = ret;
    if (ret >= 0) {
        aio_event->errorcode = 0;
    } else {
        aio_event->errorcode = EIO;
    }
}

int ADIOI_QUOBYTEFS_aio(ADIO_File fd, void *buf, int count, MPI_Datatype type,
                        ADIO_Offset offset, int wr, MPI_Request * request)
{

    int err = -1;
    static char myname[] = "ADIOI_QUOBYTEFS_aio";
    struct quobyte_iocb *aiocbp = NULL;
    struct quobyte_io_event *quobyte_aio = NULL;
    ADIOI_AIO_Request *aio_req = NULL;
    MPI_Count len, typesize;

    MPI_Type_size_x(type, &typesize);
    len = count * typesize;
    if (global_quobyte_io_context == -1) {
        global_quobyte_io_context = quobyte_aio_setup(QUOBYTE_CONCURRENT_REQS);
    }
    aio_req = (ADIOI_AIO_Request *) ADIOI_Calloc(sizeof(ADIOI_AIO_Request), 1);
    aiocbp = (struct quobyte_iocb *) ADIOI_Calloc(sizeof(struct quobyte_iocb), 1);
    quobyte_aio = (struct quobyte_io_event *) ADIOI_Calloc(sizeof(struct quobyte_io_event), 1);
    if (wr == WRITE) {
        aiocbp->op_code = QB_WRITE;
    } else {
        aiocbp->op_code = QB_READ;
    }
    aiocbp->io_context = global_quobyte_io_context;
    aiocbp->file_handle = fd->file_handle;
    aiocbp->buffer = buf;
    aiocbp->offset = offset;
    aiocbp->length = len;
    quobyte_aio->iocb = aiocbp;
    quobyte_aio->errorcode = EINPROGRESS;
    quobyte_aio->result = -1;
    aio_req->qaiocbp = quobyte_aio;

    err =
        quobyte_aio_submit_with_callback(global_quobyte_io_context, aiocbp,
                                         quobyte_io_event_finished, quobyte_aio);
    if (err == -1) {
        ADIOI_Free(aio_req);
        ADIOI_Free(aiocbp);
        return MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, myname,
                                    __LINE__, MPI_ERR_IO,
                                    "Quobyte failed to submit aio context", 0);
    }

    if (ADIOI_QUOBYTEFS_greq_class == 0) {
        MPIX_Grequest_class_create(ADIOI_GEN_aio_query_fn,
                                   ADIOI_QUOBYTEFS_aio_free_fn, MPIU_Greq_cancel_fn,
                                   ADIOI_QUOBYTEFS_aio_poll_fn, ADIOI_QUOBYTEFS_aio_wait_fn,
                                   &ADIOI_QUOBYTEFS_greq_class);
    }
    MPIX_Grequest_class_allocate(ADIOI_QUOBYTEFS_greq_class, aio_req, request);
    memcpy(&(aio_req->req), request, sizeof(MPI_Request));
    return 0;
}

void ADIOI_QUOBYTEFS_IreadContig(ADIO_File fd, void *buf, int count,
                                 MPI_Datatype datatype, int file_ptr_type,
                                 ADIO_Offset offset, MPI_Request * request, int *error_code)
{
    MPI_Count len, typesize;
    int aio_errno = 0;
    static char myname[] = "ADIOI_QUOBYTEFS_IREADCONTIG";

    MPI_Type_size_x(datatype, &typesize);
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL)
        offset = fd->fp_ind;
    aio_errno = ADIOI_QUOBYTEFS_aio(fd, buf, count, datatype, offset, READ, request);


    /* --BEGIN ERROR HANDLING-- */
    if (aio_errno != 0) {
        MPIO_ERR_CREATE_CODE_ERRNO(myname, aio_errno, error_code);
        return;
    }
    /* --END ERROR HANDLING-- */

    if (file_ptr_type == ADIO_INDIVIDUAL)
        fd->fp_ind += len;

    fd->fp_sys_posn = -1;
    *error_code = MPI_SUCCESS;
}

void ADIOI_QUOBYTEFS_IwriteContig(ADIO_File fd, const void *buf, int count,
                                  MPI_Datatype datatype, int file_ptr_type,
                                  ADIO_Offset offset, ADIO_Request * request, int *error_code)
{
    MPI_Count len, typesize;
    int aio_errno = 0;
    static char myname[] = "ADIOI_QUOBYTEFS_IWRITECONTIG";

    MPI_Type_size_x(datatype, &typesize);
    len = count * typesize;

    if (file_ptr_type == ADIO_INDIVIDUAL)
        offset = fd->fp_ind;
    /* Cast away the const'ness of 'buf' as ADIOI_GEN_aio is used for
     * both read and write calls */
    aio_errno = ADIOI_QUOBYTEFS_aio(fd, (char *) buf, count, datatype, offset, WRITE, request);

    /* --BEGIN ERROR HANDLING-- */
    if (aio_errno != 0) {
        MPIO_ERR_CREATE_CODE_ERRNO(myname, aio_errno, error_code);
        return;
    }
    /* --END ERROR HANDLING-- */

    if (file_ptr_type == ADIO_INDIVIDUAL)
        fd->fp_ind += len;

    fd->fp_sys_posn = -1;

    *error_code = MPI_SUCCESS;
}


int ADIOI_QUOBYTEFS_aio_free_fn(void *extra_state)
{
    ADIOI_AIO_Request *aio_req;
    aio_req = (ADIOI_AIO_Request *) extra_state;

    if (aio_req != NULL && aio_req->qaiocbp != NULL && aio_req->qaiocbp->iocb != NULL) {

        ADIOI_Free(aio_req->qaiocbp->iocb);
        ADIOI_Free(aio_req->qaiocbp);
        ADIOI_Free(aio_req);
    } else {
        return MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL,
                                    "ADIOI_QUOBYTEFS_aio_free_fn",
                                    __LINE__, MPI_ERR_IO, "Quobyte aio destruction failed", 0);
    }

    return MPI_SUCCESS;
}

int ADIOI_QUOBYTEFS_aio_poll_fn(void *extra_state, MPI_Status * status)
{
    ADIOI_AIO_Request *aio_req;
    int errcode = MPI_SUCCESS;

    aio_req = (ADIOI_AIO_Request *) extra_state;

    if (aio_req != NULL && aio_req->qaiocbp != NULL) {
        if (aio_req->qaiocbp->errorcode == 0) {
            aio_req->nbytes = aio_req->qaiocbp->result;
            errcode = MPI_Grequest_complete(aio_req->req);
            if (errcode != MPI_SUCCESS) {
                errcode = MPIO_Err_create_code(MPI_SUCCESS,
                                               MPIR_ERR_RECOVERABLE,
                                               "ADIOI_QUOYBTEFS_aio_poll_fn", __LINE__,
                                               MPI_ERR_IO, "**mpi_grequest_complete", 0);
            }
        } else {
            if (aio_req->qaiocbp->errorcode == EIO) {
                errcode = MPIO_Err_create_code(MPI_SUCCESS,
                                               MPIR_ERR_RECOVERABLE,
                                               "ADIOI_QUOYBTEFS_aio_poll_fn", __LINE__,
                                               MPI_ERR_IO, "Quobyte aio failed", 0);
            }
        }
    }
    return errcode;
}

/* wait for multiple requests to complete */
int ADIOI_QUOBYTEFS_aio_wait_fn(int count, void **array_of_states, double timeout,
                                MPI_Status * status)
{
    ADIOI_AIO_Request **aio_reqlist;
    struct quobyte_io_event **events =
        (struct quobyte_io_event **) ADIOI_Calloc(sizeof(struct quobyte_io_event *), count);
    int i = 0;
    int errcode = MPI_SUCCESS;
    int num_in_progress = 0;
    aio_reqlist = (ADIOI_AIO_Request **) array_of_states;

    while (i < count && aio_reqlist[i] != NULL) {
        struct quobyte_io_event *current_event = aio_reqlist[i]->qaiocbp;
        if (current_event->errorcode == EINPROGRESS) {
            events[i] = current_event;
            num_in_progress++;
        } else {
            errcode = MPI_Grequest_complete(aio_reqlist[i]->req);
        }
        i++;
    }

    i = 0;

    double start_time = MPI_Wtime();
    int no_timeout = timeout > 0 ? 0 : 1;       /* when timeout is <= 0 the loop will run until all events are done */
    while (num_in_progress > 0 && (no_timeout || MPI_Wtime() - start_time < timeout)) {
        if (events[i] != NULL && events[i]->errorcode != EINPROGRESS) {
            errcode = MPI_Grequest_complete(aio_reqlist[i]->req);
            events[i] = NULL;
            num_in_progress--;
        }
        if (i >= count) {
            i = 0;
        } else {
            i++;
        }
    }
    ADIOI_Free(events);

    if (errcode != MPI_SUCCESS) {
        errcode = MPIO_Err_create_code(MPI_SUCCESS,
                                       MPIR_ERR_RECOVERABLE,
                                       "ADIOI_QUOBYTEFS_aio_wait_fn",
                                       __LINE__, MPI_ERR_IO, "**mpi_grequest_complete", 0);
    }
    return errcode;
}
