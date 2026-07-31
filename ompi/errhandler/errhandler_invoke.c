/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2025      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"
#include "ompi/file/file.h"
#include "ompi/request/request.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"
#include "ompi/runtime/ompi_mpit_events.h"


int ompi_errhandler_invoke(ompi_errhandler_t *errhandler, void *mpi_object,
                           int object_type, int err_code, const char *message)
{
    MPI_Fint fortran_handle, fortran_err_code = OMPI_INT_2_FINT(err_code);
    ompi_communicator_t *comm;
    ompi_win_t *win;
    ompi_file_t *file;
    ompi_instance_t *instance;

    /* Raise the MPI_T errhandler-invoked event (no-op when no tool is
       listening or the producer is disabled).  The payload carries the
       MPI_Errhandler handle and the handle of the MPI object the handler is
       invoked on (both as opaque handle values; either is 0 when not available,
       e.g. routing to a predefined handler before MPI_INIT). */
    if (NULL != ompi_event_errhandler_invoked) {
        struct {
            int32_t  err_code;
            int32_t  object_type;
            uint64_t errhandler_handle;
            uint64_t object_handle;
        } payload;
        /* Report object_type as the MPI_T_BIND_* binding kind of the object the
           handler is invoked on, rather than the internal errhandler-type enum. */
        int32_t object_bind;
        switch (object_type) {
        case OMPI_ERRHANDLER_TYPE_COMM:     object_bind = MPI_T_BIND_MPI_COMM;    break;
        case OMPI_ERRHANDLER_TYPE_WIN:      object_bind = MPI_T_BIND_MPI_WIN;     break;
        case OMPI_ERRHANDLER_TYPE_FILE:     object_bind = MPI_T_BIND_MPI_FILE;    break;
        case OMPI_ERRHANDLER_TYPE_INSTANCE: object_bind = MPI_T_BIND_MPI_SESSION; break;
        default:                            object_bind = MPI_T_BIND_NO_OBJECT;   break;
        }
        payload.err_code = (int32_t) err_code;
        payload.object_type = object_bind;
        /* XXX ABI: the MPI_Errhandler handle and the invoking object's handle
           must match the registering MPI_T tool's ABI (ompi_mpit_callback_abi). */
        if (OMPI_MPIT_ABI_OMPI == ompi_mpit_callback_abi) {
            payload.errhandler_handle = (uint64_t) (uintptr_t) errhandler;
            payload.object_handle = (uint64_t) (uintptr_t) mpi_object;
        } else {
            /* TODO ABI (#13280): set the MPI Standard ABI handle values -- the
               MPI_Errhandler, and mpi_object converted per object_type
               (MPI_Comm / MPI_Win / MPI_File / MPI_Session). */
            payload.errhandler_handle = 0;
            payload.object_handle = 0;
        }
        mca_base_event_raise(ompi_event_errhandler_invoked, NULL, &payload);
    }

    /* If we got no errorhandler, then route the error to the appropriate
     * predefined error handler */
    if (NULL == errhandler) {
        int32_t state = ompi_mpi_state;
        if (state >= OMPI_MPI_STATE_INIT_COMPLETED &&
            state < OMPI_MPI_STATE_FINALIZE_PAST_COMM_SELF_DESTRUCT) {
            comm = (ompi_mpi_compat_mpi3)? &ompi_mpi_comm_world.comm: &ompi_mpi_comm_self.comm;
            switch (comm->error_handler->eh_lang) {
               case OMPI_ERRHANDLER_LANG_C:
                 comm->error_handler->eh_comm_fn(&comm, &err_code, message, NULL);
                 break;

               case OMPI_ERRHANDLER_LANG_FORTRAN:
                  fortran_handle = OMPI_INT_2_FINT(comm->c_f_to_c_index);
                  comm->error_handler->eh_fort_fn(&fortran_handle, &fortran_err_code);
                  err_code = OMPI_FINT_2_INT(fortran_err_code);
                  break;
            }
        }
        else {
            if(NULL == ompi_initial_error_handler) {
                int rc = ompi_initial_errhandler_init();
                if(OMPI_SUCCESS != rc) {
                    /* don't know what else to do... */
                    ompi_mpi_errors_are_fatal_comm_handler(NULL, NULL, message);
                }
            }
            ompi_initial_error_handler(NULL, NULL, message);
        }
        return err_code;
    }

    /* Figure out what kind of errhandler it is, figure out if it's
       fortran or C, and then invoke it */

    switch (object_type) {
    case OMPI_ERRHANDLER_TYPE_COMM:
        comm = (ompi_communicator_t *) mpi_object;
        switch (errhandler->eh_lang) {
        case OMPI_ERRHANDLER_LANG_C:
            errhandler->eh_comm_fn(&comm, &err_code, message, NULL);
            break;

        case OMPI_ERRHANDLER_LANG_FORTRAN:
            fortran_handle = OMPI_INT_2_FINT(comm->c_f_to_c_index);
            errhandler->eh_fort_fn(&fortran_handle, &fortran_err_code);
            err_code = OMPI_FINT_2_INT(fortran_err_code);
            break;
        }
        break;

    case OMPI_ERRHANDLER_TYPE_WIN:
        win = (ompi_win_t *) mpi_object;
        switch (errhandler->eh_lang) {
        case OMPI_ERRHANDLER_LANG_C:
            errhandler->eh_win_fn(&win, &err_code, message, NULL);
            break;

        case OMPI_ERRHANDLER_LANG_FORTRAN:
            fortran_handle = OMPI_INT_2_FINT(win->w_f_to_c_index);
            errhandler->eh_fort_fn(&fortran_handle, &fortran_err_code);
            err_code = OMPI_FINT_2_INT(fortran_err_code);
            break;
        }
        break;

    case OMPI_ERRHANDLER_TYPE_FILE:
        file = (ompi_file_t *) mpi_object;
        switch (errhandler->eh_lang) {
        case OMPI_ERRHANDLER_LANG_C:
            errhandler->eh_file_fn(&file, &err_code, message, NULL);
            break;

        case OMPI_ERRHANDLER_LANG_FORTRAN:
            fortran_handle = OMPI_INT_2_FINT(file->f_f_to_c_index);
            errhandler->eh_fort_fn(&fortran_handle, &fortran_err_code);
            err_code = OMPI_FINT_2_INT(fortran_err_code);
            break;
        }
        break;

    case OMPI_ERRHANDLER_TYPE_INSTANCE:
        instance = (ompi_instance_t *) mpi_object;
        switch (errhandler->eh_lang) {
        case OMPI_ERRHANDLER_LANG_C:
            errhandler->eh_instance_fn(&instance, &err_code, message, NULL);
            break;

        case OMPI_ERRHANDLER_LANG_FORTRAN:
            fortran_handle = OMPI_INT_2_FINT(instance->i_f_to_c_index);
            errhandler->eh_fort_fn(&fortran_handle, &fortran_err_code);
            err_code = OMPI_FINT_2_INT(fortran_err_code);
            break;
        }
        break;

    }

    /* All done */
    return err_code;
}


int ompi_errhandler_request_invoke(int count,
                                   struct ompi_request_t **requests,
                                   const char *message)
{
    int i, ec, type;
    ompi_mpi_object_t mpi_object;

    /* Find the *first* request that has an error -- that's the one
       that we'll invoke the error on.  In an error condition, the
       request will not have been reset back to MPI_REQUEST_NULL, so
       there's no need to cache values from before we call
       ompi_request_test(). */
    for (i = 0; i < count; ++i) {
        if (MPI_REQUEST_NULL != requests[i] &&
            MPI_SUCCESS != requests[i]->req_status.MPI_ERROR) {
            break;
        }
    }
    /* If there were no errors, return SUCCESS */
    if (i >= count) {
        return MPI_SUCCESS;
    }

    ec = ompi_errcode_get_mpi_code(requests[i]->req_status.MPI_ERROR);
    mpi_object = requests[i]->req_mpi_object;
    type = requests[i]->req_type;

    /* Since errors on requests cause them to not be freed (until we
       can examine them here), go through and free all requests with
       errors.  We only invoke the error on the *first* request
       that had an error. */
    for (; i < count; ++i) {
        if (MPI_REQUEST_NULL != requests[i] &&
            !requests[i]->req_persistent &&
            MPI_SUCCESS != requests[i]->req_status.MPI_ERROR) {
#if OPAL_ENABLE_FT_MPI
            /* Special case for MPI_ANY_SOURCE when marked as
             * MPI_ERR_PROC_FAILED_PENDING,
             * This request should not be freed since it is still active. */
            if( MPI_ERR_PROC_FAILED_PENDING != requests[i]->req_status.MPI_ERROR ) {
                ompi_request_free(&(requests[i]));
            }
#else
            /* Ignore the error -- what are we going to do?  We're
               already going to invoke an error */
            ompi_request_free(&(requests[i]));
#endif /* OPAL_ENABLE_FT_MPI */
        }
    }

    /* Invoke the error */
    switch (type) {
    case OMPI_REQUEST_PML:
    case OMPI_REQUEST_COLL:
        return ompi_errhandler_invoke(mpi_object.comm->error_handler,
                                      mpi_object.comm,
                                      mpi_object.comm->errhandler_type,
                                      ec, message);
        break;
    case OMPI_REQUEST_IO:
        return ompi_errhandler_invoke(mpi_object.file->error_handler,
                                      mpi_object.file,
                                      mpi_object.file->errhandler_type,
                                      ec, message);
        break;
    case OMPI_REQUEST_WIN:
        return ompi_errhandler_invoke(mpi_object.win->error_handler,
                                      mpi_object.win,
                                      mpi_object.win->errhandler_type,
                                      ec, message);
        break;
    default:
        /* Covers REQUEST_GEN, REQUEST_NULL, REQUEST_MAX */
        return ompi_errhandler_invoke(NULL,
                                      NULL,
                                      0,
                                      ec, message);
        break;
    }
}
