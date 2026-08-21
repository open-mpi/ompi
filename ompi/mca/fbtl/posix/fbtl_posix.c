/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2021 University of Houston. All rights reserved.
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics. Since linkers generally pull in symbols by object fules,
 * keeping these symbols as the only symbols in this file prevents
 * utility programs such as "ompi_info" from having to import entire
 * modules just to query their version and parameters
 */

#include "ompi_config.h"
#include "mpi.h"

#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/uio.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#if defined(__APPLE__) && defined(HAVE_SYS_SYSCTL_H)
#include <sys/sysctl.h>
#endif
#if HAVE_AIO_H
#include <aio.h>
#endif

int ompi_fbtl_posix_max_prd_active_reqs=2048;

#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/posix/fbtl_posix.h"

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
static mca_fbtl_base_module_1_0_0_t posix =  {
    mca_fbtl_posix_module_init,     /* initialise after being selected */
    mca_fbtl_posix_module_finalize, /* close a module on a communicator */
    mca_fbtl_posix_preadv,          /* blocking read */
#if defined (FBTL_POSIX_HAVE_AIO)
    mca_fbtl_posix_ipreadv,         /* non-blocking read*/
#else
    NULL,                           /* non-blocking read */
#endif
    mca_fbtl_posix_pwritev,         /* blocking write */
#if defined (FBTL_POSIX_HAVE_AIO)
    mca_fbtl_posix_ipwritev,        /* non-blocking write */
    mca_fbtl_posix_progress,        /* module specific progress */
    mca_fbtl_posix_request_free,    /* free module specific data items on the request */
#else
    NULL,                           /* non-blocking write */
    NULL,                           /* module specific progress */
    NULL,                           /* free module specific data items on the request */
#endif
    mca_fbtl_posix_check_atomicity  /* check whether atomicity is supported on this fs */
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

int mca_fbtl_posix_component_init_query(bool enable_progress_threads,
                                      bool enable_mpi_threads) {
    /* Nothing to do */

   return OMPI_SUCCESS;
}

struct mca_fbtl_base_module_1_0_0_t *
mca_fbtl_posix_component_file_query (ompio_file_t *fh, int *priority) {
   *priority = mca_fbtl_posix_priority;

   if (UFS == fh->f_fstype) {
       if (*priority < 50) {
           *priority = 50;
       }
   }

   return &posix;
}

int mca_fbtl_posix_component_file_unquery (ompio_file_t *file) {
   /* This function might be needed for some purposes later. for now it
    * does not have anything to do since there are no steps which need
    * to be undone if this module is not selected */

   return OMPI_SUCCESS;
}

int mca_fbtl_posix_module_init (ompio_file_t *file) {

#if defined (FBTL_POSIX_HAVE_AIO)
    /* An explicit fbtl_posix_max_aio_reqs wins; otherwise ask the system.
     * The number wanted is what *one process* may have outstanding, and
     * sysconf(_SC_AIO_MAX) is not always that: on Darwin it reports
     * kern.aiomax, the limit across all processes on the machine (90 by
     * default), where a single process is held to kern.aioprocmax (16). Sizing
     * a batch of concurrent aio_write() calls from the former overruns the
     * latter by 5x on a stock machine, so read the per-process sysctl there.
     */
    if ( 0 >= mca_fbtl_posix_max_aio_reqs ) {
        long val = -1;
#if defined(__APPLE__) && defined(HAVE_SYS_SYSCTL_H)
        int procmax = 0;
        size_t procmax_len = sizeof(procmax);
        if ( 0 == sysctlbyname("kern.aioprocmax", &procmax, &procmax_len, NULL, 0) &&
             0 < procmax ) {
            val = (long)procmax;
        }
#endif
        if ( 0 >= val ) {
            val = sysconf(_SC_AIO_MAX);
        }
        if ( 0 < val ) {
            ompi_fbtl_posix_max_prd_active_reqs = (int)val;
        }
    }
    else {
        ompi_fbtl_posix_max_prd_active_reqs = mca_fbtl_posix_max_aio_reqs;
    }
#endif
    return OMPI_SUCCESS;
}

#if defined (FBTL_POSIX_HAVE_AIO)
/* Hand requests [first, last) to the kernel. *posted comes back as one past the
 * last request accepted, so that the caller can make it prd_last_active_req; it
 * is set whether this returns success or not, since the caller has to know what
 * is in flight either way.
 *
 * EAGAIN is not a failure. It means this process already has as many aio
 * operations outstanding as it is allowed, which is transient -- a slot comes
 * back when a request is aio_return()ed -- so the requests that did not fit are
 * simply left for a later call, and *posted says where to resume. Retrying here
 * cannot work: nothing in this process reaps anything until the request this
 * batch belongs to is registered with mca_common_ompio_progress, which happens
 * only once the caller returns.
 */
int mca_fbtl_posix_post_reqs ( mca_fbtl_posix_request_data_t *data,
                               int first, int last, int *posted )
{
    int i, ret = OMPI_SUCCESS;

    for ( i = first; i < last; i++ ) {
        int rc;
        if ( FBTL_POSIX_AIO_WRITE == data->prd_req_type ) {
            rc = aio_write ( &data->prd_aio.aio_reqs[i] );
        }
        else {
            rc = aio_read ( &data->prd_aio.aio_reqs[i] );
        }
        if ( -1 == rc ) {
            if ( EAGAIN != errno ) {
                opal_output(1, "mca_fbtl_posix_post_reqs: error in aio_%s(): errno %d %s",
                            FBTL_POSIX_AIO_WRITE == data->prd_req_type ? "write" : "read",
                            errno, strerror(errno));
                ret = OMPI_ERROR;
            }
            break;
        }
    }

    *posted = i;
    return ret;
}

/* Wait for the requests in [first, last) that are still in flight and reap
 * them. Every aio operation that has been posted has to be aio_return()ed even
 * once it has completed, because that is what releases its slot in the queue --
 * an abandoned request costs the process a slot for the rest of its life. Used
 * on the error paths, which would otherwise free the aiocbs from under the
 * kernel as well, or leave the rest of the active window in flight.
 *
 * prd_aio.aio_req_status is what says whether an operation is still in flight:
 * mca_fbtl_posix_progress overwrites the entry with the aio_error() result once
 * it has reaped that operation, so a window that an earlier progress call
 * partly reaped must not be handed to aio_error() or aio_return() a second
 * time.
 *
 * The wait is aio_suspend() rather than mca_common_ompio_progress(). This
 * function is called from mca_fbtl_posix_progress, which ompio progress
 * reaches through req_progress_fn; ompio progress has no re-entrancy guard,
 * and OPAL_THREAD_TRYLOCK is a compile-time 0 unless opal_using_threads(), so
 * calling it here would walk the pending request list again and re-enter
 * mca_fbtl_posix_progress on the very request being drained. aio_suspend waits
 * on the one operation and calls back into nothing.
 */
void mca_fbtl_posix_drain_reqs ( mca_fbtl_posix_request_data_t *data,
                                 int first, int last )
{
    int i;

    for ( i = first; i < last; i++ ) {
        const struct aiocb *cb = &data->prd_aio.aio_reqs[i];
        ssize_t len;

        if ( EINPROGRESS != data->prd_aio.aio_req_status[i] ) {
            /* Already reaped, by this or by an earlier progress call. */
            continue;
        }
        while ( EINPROGRESS == aio_error ( cb ) ) {
            /* aio_suspend() may return early, e.g. on a signal; aio_error() is
             * the condition that decides whether to wait again.
             */
            (void) aio_suspend ( &cb, 1, NULL );
        }
        data->prd_aio.aio_req_status[i] = aio_error ( cb );
        len = aio_return ( &data->prd_aio.aio_reqs[i] );
        if ( 0 < len ) {
            /* Whatever did land counts towards what the request reports. */
            data->prd_total_len += len;
        }
    }
}
#endif


int mca_fbtl_posix_module_finalize (ompio_file_t *file) {
    return OMPI_SUCCESS;
}

bool mca_fbtl_posix_progress ( mca_ompio_request_t *req)
{
    bool ret=false;
#if defined (FBTL_POSIX_HAVE_AIO)
    int i=0, lcount=0, ret_code=0;
    mca_fbtl_posix_request_data_t *data=(mca_fbtl_posix_request_data_t *)req->req_data;
    off_t start_offset, end_offset, total_length;

    for (i=data->prd_first_active_req; i < data->prd_last_active_req; i++ ) {
        if ( EINPROGRESS == data->prd_aio.aio_req_status[i] ) {
            data->prd_aio.aio_req_status[i] = aio_error ( &data->prd_aio.aio_reqs[i]);
            if ( 0 == data->prd_aio.aio_req_status[i]){
                /* assuming right now that aio_return will return
                ** the number of bytes written/read and not an error code,
                ** since aio_error should have returned an error in that
                ** case and not 0 ( which means request is complete)
                */
                ssize_t ret2 = aio_return (&data->prd_aio.aio_reqs[i]);
                data->prd_total_len += ret2;
                if ( data->prd_aio.aio_reqs[i].aio_nbytes != (size_t)ret2 ) {
                    /* Partial completion */
                    data->prd_aio.aio_reqs[i].aio_offset += ret2;
                    data->prd_aio.aio_reqs[i].aio_buf    = (char*)data->prd_aio.aio_reqs[i].aio_buf + ret2;
                    data->prd_aio.aio_reqs[i].aio_nbytes -= ret2;
                    data->prd_aio.aio_reqs[i].aio_reqprio = 0;
                    data->prd_aio.aio_reqs[i].aio_sigevent.sigev_notify = SIGEV_NONE;
                    data->prd_aio.aio_req_status[i]        = EINPROGRESS;
                    start_offset = data->prd_aio.aio_reqs[i].aio_offset;
                    total_length = data->prd_aio.aio_reqs[i].aio_nbytes;
                    /* release previous lock */
                    mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
                    
                    if ( data->prd_req_type == FBTL_POSIX_AIO_WRITE ) {
                        ret_code = mca_fbtl_posix_lock( &data->prd_lock, data->prd_fh, F_WRLCK, start_offset, total_length,
                                                        OMPIO_LOCK_ENTIRE_REGION, &data->prd_lock_counter );
                        if ( 0 < ret_code ) {
                            opal_output(1, "mca_fbtl_posix_progress: error in mca_fbtl_posix_lock() %d", ret_code);
                            /* Just in case some part of the lock actually succeeded. */
                            mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
                            return false;
                        }
                        if (-1 == aio_write(&data->prd_aio.aio_reqs[i])) {
                            opal_output(1, "mca_fbtl_posix_progress: error in aio_write()");
                            mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
                            return false;
                        }
                    }
                    else if (  data->prd_req_type == FBTL_POSIX_AIO_READ ) {
                        ret_code = mca_fbtl_posix_lock( &data->prd_lock, data->prd_fh, F_RDLCK, start_offset, total_length,
                                                        OMPIO_LOCK_ENTIRE_REGION, &data->prd_lock_counter );
                        if ( 0 < ret_code ) {
                            opal_output(1, "mca_fbtl_posix_progress: error in mca_fbtl_posix_lock() %d", ret_code);
                            /* Just in case some part of the lock actually succeeded. */
                            mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
                            return false;
                        }
                        if (-1 == aio_read(&data->prd_aio.aio_reqs[i])) {
                            opal_output(1, "mca_fbtl_posix_progress: error in aio_read()");
                            mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
                            return false;
                        }
                    }
                }
                else {
                    data->prd_open_reqs--;
                    lcount++;
                }
            }
            else if ( EINPROGRESS == data->prd_aio.aio_req_status[i]){
                /* not yet done */
                continue;
            }
            else {
                /* An error occurred. Reap this operation and the rest of the
                 * active window before giving up on the request: an operation
                 * that is never aio_return()ed costs the process a queue slot
                 * for the rest of its life, and the aiocbs are released when
                 * the request is freed. Reaping the failed operation is a bare
                 * aio_return -- aio_error has already said it is no longer in
                 * flight, and its recorded status is the error code, which is
                 * how mca_fbtl_posix_drain_reqs knows to leave it alone. That
                 * call also lets prd_total_len account for whatever landed in
                 * the rest of the window. Then release the lock the batch was
                 * posted under, mark the request done, and set an error code
                 * in the status.
                 *
                 * prd_open_reqs is deliberately left alone: this request is
                 * still counted in it, so the "all pending operations are
                 * finished" tail below, which would unlock a second time and
                 * overwrite MPI_ERROR with OMPI_SUCCESS, is correctly skipped.
                 */
                (void) aio_return ( &data->prd_aio.aio_reqs[i] );
                mca_fbtl_posix_drain_reqs ( data, i + 1, data->prd_last_active_req );
                mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
                req->req_ompi.req_status.MPI_ERROR = OMPI_ERROR;
                req->req_ompi.req_status._ucount = data->prd_total_len;
                ret = true;
                break;
            }
        }
        else {
            lcount++;
        }
    }
#if 0
    printf("lcount=%d open_reqs=%d\n", lcount, data->prd_open_reqs );
#endif
    /* Every request in the active window is accounted for. If any remain
     * unfinished, the next batch goes out -- which includes the case of an empty
     * window, left behind when the process's aio queue was full and would take
     * none of the previous batch. Comparing lcount against the width of the
     * window rather than against prd_req_chunks is what lets that work: a window
     * is only as wide as the queue allowed, which need not be a whole chunk.
     */
    if ( (lcount == (data->prd_last_active_req - data->prd_first_active_req)) &&
         (0 != data->prd_open_reqs )) {
        int want, posted;

        /* release the lock of the previous operations */
        mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );

        /* post the next batch of operations */
        data->prd_first_active_req = data->prd_last_active_req;
        if ( (data->prd_req_count-data->prd_first_active_req) > data->prd_req_chunks ) {
            want = data->prd_first_active_req + data->prd_req_chunks;
        }
        else {
            want = data->prd_req_count;
        }
        if ( want <= data->prd_first_active_req ) {
            /* Requests are outstanding but none are left to post, which should
             * not happen. Leave the request alone rather than index outside the
             * array below.
             */
            return ret;
        }

        start_offset = data->prd_aio.aio_reqs[data->prd_first_active_req].aio_offset;
        end_offset   = data->prd_aio.aio_reqs[want-1].aio_offset +
                       data->prd_aio.aio_reqs[want-1].aio_nbytes;
        total_length = (end_offset - start_offset);

        if ( FBTL_POSIX_AIO_READ == data->prd_req_type ) {
            ret_code = mca_fbtl_posix_lock( &data->prd_lock, data->prd_fh, F_RDLCK, start_offset, total_length,
                                            OMPIO_LOCK_ENTIRE_REGION, &data->prd_lock_counter );
        }
        else if ( FBTL_POSIX_AIO_WRITE == data->prd_req_type ) {
            ret_code = mca_fbtl_posix_lock( &data->prd_lock, data->prd_fh, F_WRLCK, start_offset, total_length,
                                            OMPIO_LOCK_ENTIRE_REGION, &data->prd_lock_counter );
        }
        if ( 0 < ret_code ) {
            opal_output(1, "mca_fbtl_posix_progress: error in mca_fbtl_posix_lock() %d", ret_code);
            /* Just in case some part of the lock actually succeeded. */
            mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
            return false;
        }
        
        if ( OMPI_SUCCESS != mca_fbtl_posix_post_reqs ( data, data->prd_first_active_req,
                                                        want, &posted ) ) {
            /* A real error rather than a full queue. Reap the part of the batch
             * that did go out, then complete the request with an error instead
             * of leaving the caller waiting on operations that will never be
             * posted.
             */
            mca_fbtl_posix_drain_reqs ( data, data->prd_first_active_req, posted );
            mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
            data->prd_last_active_req = data->prd_first_active_req;
            data->prd_open_reqs = 0;
            req->req_ompi.req_status.MPI_ERROR = OMPI_ERROR;
            req->req_ompi.req_status._ucount = data->prd_total_len;
            return true;
        }
        /* posted may fall short of want, the queue being full; the remainder
         * waits for the next call, which finds a short or empty window and comes
         * back here.
         */
        data->prd_last_active_req = posted;
#if 0
        printf("posting new batch: first=%d last=%d\n", data->prd_first_active_req, data->prd_last_active_req );
#endif
    }

    if ( 0 == data->prd_open_reqs ) {
        /* all pending operations are finished for this request */
        req->req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
        req->req_ompi.req_status._ucount = data->prd_total_len;
        mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );

        if ( data->prd_fh->f_atomicity ) {
            mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter );
        }

        ret = true;
    }
#endif
    return ret;
}

void mca_fbtl_posix_request_free ( mca_ompio_request_t *req)
{
#if defined (FBTL_POSIX_HAVE_AIO)
    /* Free the fbtl specific data structures */
    mca_fbtl_posix_request_data_t *data=(mca_fbtl_posix_request_data_t *)req->req_data;
    if (NULL != data ) {
            
        if ( NULL != data->prd_aio.aio_reqs ) {
            free ( data->prd_aio.aio_reqs);
        }
        if ( NULL != data->prd_aio.aio_req_status ) {
            free ( data->prd_aio.aio_req_status );
        }
        free (data);
        req->req_data = NULL;
    }
#endif
  return;
}

bool mca_fbtl_posix_check_atomicity ( ompio_file_t *file)
{    
    struct flock lock;
    
    lock.l_type   = F_WRLCK;
    lock.l_whence = SEEK_SET;
    lock.l_start  = 0;
    lock.l_len    = 0;
    lock.l_pid    = 0;
    
    if (fcntl(file->fd, F_GETLK, &lock) < 0)
    {
#ifdef VERBOSE
        printf("Failed to get lock info for '%s': %s\n", filename, strerror(errno));
#endif
        return false;
    }

#ifdef VERBOSE
    printf("Lock would have worked, l_type=%d\n", (int)lock.l_type);
#endif
    return true;
}
