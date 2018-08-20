/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "fbtl_ime.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

static ssize_t  mca_fbtl_ime_nonblocking_op (ompio_file_t *fh,
                 ompi_request_t *request, int io_op);

ssize_t mca_fbtl_ime_ipreadv (ompio_file_t *fh, ompi_request_t *request)
{
    return mca_fbtl_ime_nonblocking_op(fh, request, FBTL_IME_READ);
}
ssize_t  mca_fbtl_ime_ipwritev (ompio_file_t *fh, ompi_request_t *request)
{
    return mca_fbtl_ime_nonblocking_op(fh, request, FBTL_IME_WRITE);
}

static ssize_t mca_fbtl_ime_nonblocking_op (ompio_file_t *fh,
                                            ompi_request_t *request, int io_op)
{
    mca_fbtl_ime_request_data_t *data;
    mca_ompio_request_t *req = (mca_ompio_request_t *) request;
    int i=0, req_index = 0, ret;

    data = (mca_fbtl_ime_request_data_t *) malloc ( sizeof (mca_fbtl_ime_request_data_t));
    if ( NULL == data ) {
        opal_output (1,"could not allocate memory\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* We might allocate too much memory here because we don't know
       how many IME requests will be necessary.

       We will use all the iovec "slots" in the array,
       but maybe not all the request and request status slots.
       That is, because an IME request can handle several iovecs,
       not just one. */
    data->allocated_data = (void*) malloc( fh->f_num_of_io_entries *
        (sizeof(struct iovec) +
         sizeof(struct ime_aiocb) +
         sizeof(ssize_t)) );
    if (NULL == data->allocated_data) {
        opal_output(1, "OUT OF MEMORY\n");
        free(data);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    data->aio_iovecs = (struct iovec *) data->allocated_data;
    data->aio_reqs = (struct ime_aiocb *) (data->aio_iovecs +
        fh->f_num_of_io_entries);
    data->aio_req_status = (ssize_t *) (data->aio_reqs +
        fh->f_num_of_io_entries);

    /* Fill some attributes of the OMPIO request data */
    data->aio_req_type  = io_op;    /* The correctness of io_op will be checked later */
    data->aio_req_chunks = mca_fbtl_ime_aio_reqs_max;
    data->aio_req_fail_count = 0;
    data->aio_total_len = 0;
    data->aio_fh = fh;
    data->aio_reqs[0].iovcnt = 0;

    /* Go through all IO entries and try to aggregate them. */
    for ( i=0; i<fh->f_num_of_io_entries; i++ ) {
        data->aio_iovecs[i].iov_base = fh->f_io_array[i].memory_address;
        data->aio_iovecs[i].iov_len = fh->f_io_array[i].length;

        /* If the processed iovec will be the first in our ime_aiocb request,
           then we initialize this aio request for IME. */
        if (data->aio_reqs[req_index].iovcnt == 0) {
            data->aio_reqs[req_index].iov = &data->aio_iovecs[i];
            data->aio_reqs[req_index].iovcnt = 1;
            data->aio_reqs[req_index].file_offset  = (off_t)
                fh->f_io_array[i].offset;
            data->aio_reqs[req_index].fd  = fh->fd;
            data->aio_reqs[req_index].complete_cb = &mca_fbtl_ime_complete_cb;
            data->aio_reqs[req_index].user_context = (intptr_t)
                &data->aio_req_status[req_index];
            data->aio_req_status[req_index] = FBTL_IME_IN_PROGRESS;
        }

        /* Here we check if the next iovec will be appended to
           the current ime_aiocb request.
           ie: if data is contiguous 
               AND we don't exceed the advised number of iovecs for IME
           In that case, the next iovec will be appended to the IME req. */
        if (i+1 != fh->f_num_of_io_entries &&
            ((OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset +
             (ptrdiff_t)fh->f_io_array[i].length) ==
              (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i+1].offset &&
            data->aio_reqs[req_index].iovcnt < mca_fbtl_ime_iov_max ) {
            data->aio_reqs[req_index].iovcnt++;
        }

        /* Otherwise, we need to create a new request
           (except if there is no next iovec to process) */
        else if ( i+1 != fh->f_num_of_io_entries ) {
            req_index++;
            data->aio_reqs[req_index].iovcnt = 0;
        }
    }

    /* Fill the missing attributes of the OMPI request */
    data->aio_req_count = req_index + 1;
    data->aio_open_reqs = req_index + 1;
    data->aio_first_active_req = 0;
    if ( data->aio_req_count > data->aio_req_chunks ) {
        data->aio_last_active_req = data->aio_req_chunks;
    }
    else {
        data->aio_last_active_req = data->aio_req_count;
    }

    /* Actually start the requests (or at least the first batch).
       In case an error happened when one request is started, we
       don't send the next ones and mark the failing request as
       the last active one. Finally we exit as if no error happened,
       because some other requests might have already been started
       and they need to be finalized properly (via the progress function).
     */
    for (i=0; i < data->aio_last_active_req; i++) {
        switch(io_op) {

        case FBTL_IME_READ:
            ret = ime_native_aio_read(&data->aio_reqs[i]);
            if (ret < 0) {
                opal_output(1, "mca_fbtl_ime_nonblocking_op: error in "
                               "ime_native_aio_read() error ret=%d  %s",
                               ret, strerror(errno));
                data->aio_req_status[i] = FBTL_IME_REQ_ERROR;
                data->aio_last_active_req = i + 1;
                goto standard_exit;
            }
            break;

        case FBTL_IME_WRITE:
            ret = ime_native_aio_write(&data->aio_reqs[i]);
            if (ret < 0) {
                opal_output(1, "mca_fbtl_ime_nonblocking_op: error in "
                               "ime_native_aio_write() error ret=%d  %s",
                               ret, strerror(errno));
                data->aio_req_status[i] = FBTL_IME_REQ_ERROR;
                data->aio_last_active_req = i + 1;
                goto standard_exit;
            }
            break;

        default:
            opal_output(1, "mca_fbtl_ime_nonblocking_op: an unsupported "
                           "IO operation was requested. io_op=%d", io_op);
            goto error_exit;
        }
    }

standard_exit:
    req->req_data = data;
    req->req_progress_fn = mca_fbtl_ime_progress;
    req->req_free_fn     = mca_fbtl_ime_request_free;

    return OMPI_SUCCESS;

error_exit:
    free(data->allocated_data);
    free(data);
    return OMPI_ERROR;
}
