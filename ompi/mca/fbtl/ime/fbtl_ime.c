/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/ime/fbtl_ime.h"

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
static mca_fbtl_base_module_1_0_0_t ime =  {
    mca_fbtl_ime_module_init,     /* initalise after being selected */
    mca_fbtl_ime_module_finalize, /* close a module on a communicator */
    mca_fbtl_ime_preadv,          /* blocking read */
    mca_fbtl_ime_ipreadv,         /* non-blocking read*/
    mca_fbtl_ime_pwritev,         /* blocking write */
    mca_fbtl_ime_ipwritev,        /* non-blocking write */
    mca_fbtl_ime_progress,        /* module specific progress */
    mca_fbtl_ime_request_free     /* free module specific data items on the request */
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

int mca_fbtl_ime_component_init_query(bool enable_progress_threads,
                                      bool enable_mpi_threads)
{
    /* Nothing to do */
   return OMPI_SUCCESS;
}

struct mca_fbtl_base_module_1_0_0_t *
mca_fbtl_ime_component_file_query (ompio_file_t *fh, int *priority)
{
   *priority = mca_fbtl_ime_priority;

    /* Do the same as the FS component:
       Only return a non-null component if IME
       can handle the IO operations. */
    if (IME == fh->f_fstype) {
        if (*priority < FBTL_IME_INCREASED_PRIORITY) {
            *priority = FBTL_IME_INCREASED_PRIORITY;
        }
        return &ime;
    }

   return NULL;
}

int mca_fbtl_ime_component_file_unquery (ompio_file_t *file)
{
   /* This function might be needed for some purposes later. for now it
    * does not have anything to do since there are no steps which need
    * to be undone if this module is not selected */

   return OMPI_SUCCESS;
}

int mca_fbtl_ime_module_init (ompio_file_t *file)
{
    return OMPI_SUCCESS;
}


int mca_fbtl_ime_module_finalize (ompio_file_t *file)
{
    return OMPI_SUCCESS;
}

bool mca_fbtl_ime_progress ( mca_ompio_request_t *req)
{
    int i=0, lcount=0, ret_code=0;
    mca_fbtl_ime_request_data_t *data=(mca_fbtl_ime_request_data_t *)req->req_data;

    /* Go through all the requests in the current batch to check
     * if they have finished. */
    for (i=data->aio_first_active_req; i < data->aio_last_active_req; i++ ) {
        if ( data->aio_req_status[i] == FBTL_IME_REQ_CLOSED ) {
            lcount++;
        }
        else if ( data->aio_req_status[i] >= 0 ) {
            /* request has finished */
            data->aio_open_reqs--;
            lcount++;
            data->aio_total_len += data->aio_req_status[i];
            data->aio_req_status[i] = FBTL_IME_REQ_CLOSED;
        }
        else if ( data->aio_req_status[i] == FBTL_IME_REQ_ERROR ) {
            /* an error occured. */
            data->aio_open_reqs--;
            lcount++;
            data->aio_req_fail_count++;
            data->aio_req_status[i] = FBTL_IME_REQ_CLOSED;
        }
        else {
            /* not yet done */
        }
    }

    /* In case the current batch of requests terminated, exit if an error
     * happened for any request.
     */
    if ( data->aio_req_fail_count > 0 &&
         lcount == data->aio_last_active_req - data->aio_first_active_req ) {
        goto error_exit;
    }

    /* In case some requests are pending, and no error happened in any of the
     * previous requests, then the next batch of operations should be prepared.
     */
    if ( (lcount == data->aio_req_chunks) && (0 != data->aio_open_reqs) ) {

        /* prepare the next batch of operations */
        data->aio_first_active_req = data->aio_last_active_req;
        if ( (data->aio_req_count-data->aio_last_active_req) > data->aio_req_chunks ) {
            data->aio_last_active_req += data->aio_req_chunks;
        }
        else {
            data->aio_last_active_req = data->aio_req_count;
        }

        /* Send the requests. */
        for ( i=data->aio_first_active_req; i< data->aio_last_active_req; i++ ) {
            if ( FBTL_IME_READ == data->aio_req_type &&
                 ime_native_aio_read(&data->aio_reqs[i]) < 0 ) {
                opal_output(1, "mca_fbtl_ime_progress: error in aio_read()");
                data->aio_req_status[i] = FBTL_IME_REQ_ERROR;
                data->aio_last_active_req = i + 1;
                break;
            }
            else if ( FBTL_IME_WRITE == data->aio_req_type &&
                      ime_native_aio_write(&data->aio_reqs[i]) < 0 ) {
                opal_output(1, "mca_fbtl_ime_progress: error in aio_write()");
                data->aio_req_status[i] = FBTL_IME_REQ_ERROR;
                data->aio_last_active_req = i + 1;
                break;
            }
        }
    }

    if ( 0 == data->aio_open_reqs ) {
        /* all pending operations are finished for this request */
        req->req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
        req->req_ompi.req_status._ucount = data->aio_total_len;
        return true;
    }
    return false;

error_exit:
    req->req_ompi.req_status.MPI_ERROR = OMPI_ERROR;
    req->req_ompi.req_status._ucount = data->aio_total_len;
    return true;
}

void mca_fbtl_ime_request_free ( mca_ompio_request_t *req)
{
    /* Free the fbtl specific data structures */
    mca_fbtl_ime_request_data_t *data=(mca_fbtl_ime_request_data_t *)req->req_data;
    if (NULL != data) {
        free (data->allocated_data);
        free (data);
        req->req_data = NULL;
    }
}

void mca_fbtl_ime_complete_cb  (struct ime_aiocb *aiocb, int err, ssize_t bytes)
{
    ssize_t *req_status = (ssize_t *) aiocb->user_context;
    *req_status = err == 0 ? bytes : FBTL_IME_REQ_ERROR;
}