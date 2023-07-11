/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2018-2021 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/errhandler/errhandler_predefined.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/string_copy.h"
#include "opal/mca/backtrace/backtrace.h"
#include "ompi/runtime/mpiruntime.h"

/*
 * Table for Fortran <-> C errhandler handle conversion
 */
opal_pointer_array_t ompi_errhandler_f_to_c_table = {{0}};

/*
 * default errhandler id
 */
static size_t default_errhandler_id = SIZE_MAX;

/*
 * Class information
 */
static void ompi_errhandler_construct(ompi_errhandler_t *eh);
static void ompi_errhandler_destruct(ompi_errhandler_t *eh);


/*
 * Class instance
 */
OBJ_CLASS_INSTANCE(ompi_errhandler_t, opal_object_t, ompi_errhandler_construct,
                   ompi_errhandler_destruct);


/*
 * _addr flavors are for F03 bindings
 */
ompi_predefined_errhandler_t ompi_mpi_errhandler_null = {{{0}}};
ompi_predefined_errhandler_t *ompi_mpi_errhandler_null_addr =
    &ompi_mpi_errhandler_null;
ompi_predefined_errhandler_t ompi_mpi_errors_are_fatal = {{{0}}};
ompi_predefined_errhandler_t *ompi_mpi_errors_are_fatal_addr =
    &ompi_mpi_errors_are_fatal;
ompi_predefined_errhandler_t ompi_mpi_errors_abort = {{{0}}};
ompi_predefined_errhandler_t *ompi_mpi_errors_abort_addr =
    &ompi_mpi_errors_abort;
ompi_predefined_errhandler_t ompi_mpi_errors_return = {{{0}}};
ompi_predefined_errhandler_t *ompi_mpi_errors_return_addr =
    &ompi_mpi_errors_return;

static opal_mutex_t errhandler_init_lock = OPAL_MUTEX_STATIC_INIT;
ompi_errhandler_t* ompi_initial_error_handler_eh = NULL;
void (*ompi_initial_error_handler)(struct ompi_communicator_t **comm, int *error_code, ...) = NULL;

#if OPAL_ENABLE_FT_MPI
/*
 * A mutex to prevent altering the proc/comm status from an
 * RTE progress thread at the same time as from an OMPI
 * thread.
 */
static opal_mutex_t errhandler_ftmpi_lock = OPAL_MUTEX_STATIC_INIT;
#endif /* OPAL_ENABLE_FT_MPI */

/*
 * Initialize the initial errhandler infrastructure only.
 * This does not allocate any memory and does not require a corresponding fini.
 */
int ompi_initial_errhandler_init(void) {
    opal_mutex_lock(&errhandler_init_lock);
    if ( NULL != ompi_initial_error_handler ) {
        /* Already initialized (presumably by an API call before MPI_init) */
        opal_mutex_unlock(&errhandler_init_lock);
        return OMPI_SUCCESS;
    }

    /* If it has been requested from the launch keys, set the initial
     * error handler that will be attached by default with predefined
     * communicators. We use an env because that can be obtained before
     * OPAL and PMIx initialization.
     */
    char *env = getenv("OMPI_MCA_mpi_initial_errhandler");
    if( NULL != env ) {
        if( 0 == strcasecmp(env, "mpi_errors_are_fatal") ) {
            ompi_initial_error_handler = &ompi_mpi_errors_are_fatal_comm_handler;
            ompi_initial_error_handler_eh = &ompi_mpi_errors_are_fatal.eh;
        }
        else if( 0 == strcasecmp(env, "mpi_errors_abort") ) {
            ompi_initial_error_handler = &ompi_mpi_errors_abort_comm_handler;
            ompi_initial_error_handler_eh = &ompi_mpi_errors_abort.eh;
        }
        else if( 0 == strcasecmp(env, "mpi_errors_return") ) {
            ompi_initial_error_handler = &ompi_mpi_errors_return_comm_handler;
            ompi_initial_error_handler_eh = &ompi_mpi_errors_return.eh;
        }
        else {
            /* invalid entry detected, ignore it, set fatal by default */
            opal_output(0, "WARNING: invalid value for launch key 'mpi_initial_errhandler'; defaulting to 'mpi_errors_are_fatal'.");
            ompi_initial_error_handler = &ompi_mpi_errors_are_fatal_comm_handler;
            ompi_initial_error_handler_eh = &ompi_mpi_errors_are_fatal.eh;
        }
    }
    else {
        ompi_initial_error_handler = &ompi_mpi_errors_are_fatal_comm_handler;
        ompi_initial_error_handler_eh = &ompi_mpi_errors_are_fatal.eh;
    }
    opal_mutex_unlock(&errhandler_init_lock);
    return OMPI_SUCCESS;
}

static int ompi_errhandler_finalize (void);

/*
 * Initialize OMPI errhandler infrastructure
 */
int ompi_errhandler_init(void)
{
    OBJ_CONSTRUCT( &ompi_errhandler_f_to_c_table, opal_pointer_array_t);
    if( OPAL_SUCCESS != opal_pointer_array_init(&ompi_errhandler_f_to_c_table, 8,
                                                OMPI_FORTRAN_HANDLE_MAX, 16) ) {
        return OMPI_ERROR;
    }

    /* Initialize the predefined error handlers */
    OBJ_CONSTRUCT( &ompi_mpi_errhandler_null.eh, ompi_errhandler_t );
    if( ompi_mpi_errhandler_null.eh.eh_f_to_c_index != OMPI_ERRHANDLER_NULL_FORTRAN ) {
        return OMPI_ERROR;
    }

    ompi_mpi_errhandler_null.eh.eh_mpi_object_type = OMPI_ERRHANDLER_TYPE_PREDEFINED;
    ompi_mpi_errhandler_null.eh.eh_lang = OMPI_ERRHANDLER_LANG_C;
    ompi_mpi_errhandler_null.eh.eh_comm_fn = NULL;
    ompi_mpi_errhandler_null.eh.eh_file_fn = NULL;
    ompi_mpi_errhandler_null.eh.eh_win_fn  = NULL ;
    ompi_mpi_errhandler_null.eh.eh_fort_fn = NULL;
    opal_string_copy (ompi_mpi_errhandler_null.eh.eh_name, "MPI_ERRHANDLER_NULL",
                      sizeof(ompi_mpi_errhandler_null.eh.eh_name));

    OBJ_CONSTRUCT( &ompi_mpi_errors_are_fatal.eh, ompi_errhandler_t );
    if( ompi_mpi_errors_are_fatal.eh.eh_f_to_c_index != OMPI_ERRORS_ARE_FATAL_FORTRAN )
        return OMPI_ERROR;
    ompi_mpi_errors_are_fatal.eh.eh_mpi_object_type = OMPI_ERRHANDLER_TYPE_PREDEFINED;
    ompi_mpi_errors_are_fatal.eh.eh_lang = OMPI_ERRHANDLER_LANG_C;
    ompi_mpi_errors_are_fatal.eh.eh_comm_fn = ompi_mpi_errors_are_fatal_comm_handler;
    ompi_mpi_errors_are_fatal.eh.eh_file_fn = ompi_mpi_errors_are_fatal_file_handler;
    ompi_mpi_errors_are_fatal.eh.eh_win_fn  = ompi_mpi_errors_are_fatal_win_handler;
    ompi_mpi_errors_are_fatal.eh.eh_instance_fn = ompi_mpi_errors_are_fatal_instance_handler;
    ompi_mpi_errors_are_fatal.eh.eh_fort_fn = NULL;
    opal_string_copy(ompi_mpi_errors_are_fatal.eh.eh_name,
                     "MPI_ERRORS_ARE_FATAL",
                     sizeof(ompi_mpi_errors_are_fatal.eh.eh_name));

    OBJ_CONSTRUCT( &ompi_mpi_errors_return.eh, ompi_errhandler_t );
    if( ompi_mpi_errors_return.eh.eh_f_to_c_index != OMPI_ERRORS_RETURN_FORTRAN )
        return OMPI_ERROR;
    ompi_mpi_errors_return.eh.eh_mpi_object_type  = OMPI_ERRHANDLER_TYPE_PREDEFINED;
    ompi_mpi_errors_return.eh.eh_lang = OMPI_ERRHANDLER_LANG_C;
    ompi_mpi_errors_return.eh.eh_comm_fn = ompi_mpi_errors_return_comm_handler;
    ompi_mpi_errors_return.eh.eh_file_fn = ompi_mpi_errors_return_file_handler;
    ompi_mpi_errors_return.eh.eh_win_fn  = ompi_mpi_errors_return_win_handler;
    ompi_mpi_errors_return.eh.eh_instance_fn = ompi_mpi_errors_return_instance_handler;
    ompi_mpi_errors_return.eh.eh_fort_fn = NULL;
    opal_string_copy(ompi_mpi_errors_return.eh.eh_name, "MPI_ERRORS_RETURN",
                     sizeof(ompi_mpi_errors_return.eh.eh_name));

  OBJ_CONSTRUCT( &ompi_mpi_errors_abort.eh, ompi_errhandler_t );
  if( ompi_mpi_errors_abort.eh.eh_f_to_c_index != OMPI_ERRORS_ABORT_FORTRAN )
      return OMPI_ERROR;
  ompi_mpi_errors_abort.eh.eh_mpi_object_type = OMPI_ERRHANDLER_TYPE_PREDEFINED;
  ompi_mpi_errors_abort.eh.eh_lang = OMPI_ERRHANDLER_LANG_C;
  ompi_mpi_errors_abort.eh.eh_comm_fn = ompi_mpi_errors_abort_comm_handler;
  ompi_mpi_errors_abort.eh.eh_file_fn = ompi_mpi_errors_abort_file_handler;
  ompi_mpi_errors_abort.eh.eh_win_fn  = ompi_mpi_errors_abort_win_handler ;
  ompi_mpi_errors_abort.eh.eh_fort_fn = NULL;
  opal_string_copy(ompi_mpi_errors_abort.eh.eh_name,
                   "MPI_ERRORS_ABORT",
                   sizeof(ompi_mpi_errors_abort.eh.eh_name));

  /* Lets initialize the initial error handler if not already done */
  char *env = getenv("OMPI_MCA_mpi_initial_errhandler");
  if( NULL != env ) {
    ompi_process_info.initial_errhandler = strndup(env, MPI_MAX_INFO_VAL);
  }

  ompi_initial_errhandler_init();
  ompi_mpi_instance_append_finalize (ompi_errhandler_finalize);

  return OMPI_SUCCESS;
}


/**
 * Finalize the error handler interface.
 *
 * @returns OMPI_SUCCESS Always
 *
 * Invoked on instance teardown if ompi_errhandler_init() was called; tears down the error handler
 * interface, and destroys the F2C translation table.
 */
static int ompi_errhandler_finalize (void)
{
    OBJ_DESTRUCT(&ompi_mpi_errhandler_null.eh);
    OBJ_DESTRUCT(&ompi_mpi_errors_return.eh);
    OBJ_DESTRUCT(&ompi_mpi_errors_are_fatal.eh);

    /* JMS Add stuff here checking for unreleased errorhandlers,
       similar to communicators, info handles, etc. */
    PMIx_Deregister_event_handler(default_errhandler_id, NULL, NULL);

    /* Remove errhandler F2C table */

    OBJ_DESTRUCT(&ompi_errhandler_f_to_c_table);

    /* All done */

    return OMPI_SUCCESS;
}

void ompi_errhandler_free (ompi_errhandler_t *errhandler)
{
    OBJ_RELEASE(errhandler);
    ompi_mpi_instance_release ();
}

ompi_errhandler_t *ompi_errhandler_create(ompi_errhandler_type_t object_type,
                                          ompi_errhandler_generic_handler_fn_t *func,
                                          ompi_errhandler_lang_t lang)
{
    ompi_errhandler_t *new_errhandler;
    int ret;

    /* make sure the infrastructure is initialized */
    ret = ompi_mpi_instance_retain ();
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return NULL;
    }

    /* Create a new object and ensure that it's valid */

    new_errhandler = OBJ_NEW(ompi_errhandler_t);
    if (NULL != new_errhandler) {
        if (0 > new_errhandler->eh_f_to_c_index) {
            OBJ_RELEASE(new_errhandler);
            new_errhandler = NULL;
        } else {

        /* We cast the user's callback function to any one of the
           function pointer types in the union; it doesn't matter which.
           It only matters that we dereference/use the right member when
           invoking the callback. */

            new_errhandler->eh_mpi_object_type = object_type;
            new_errhandler->eh_lang = lang;
            switch (object_type ) {
            case OMPI_ERRHANDLER_TYPE_COMM:
                new_errhandler->eh_comm_fn = (MPI_Comm_errhandler_function *)func;
                break;
            case OMPI_ERRHANDLER_TYPE_FILE:
                new_errhandler->eh_file_fn = (ompi_file_errhandler_function *)func;
                break;
            case OMPI_ERRHANDLER_TYPE_WIN:
                new_errhandler->eh_win_fn = (MPI_Win_errhandler_function *)func;
                break;
            case OMPI_ERRHANDLER_TYPE_INSTANCE:
                new_errhandler->eh_instance_fn = (MPI_Session_errhandler_function *)func;
                break;
            default:
                break;
            }
        }

        if (NULL != new_errhandler) {
            new_errhandler->eh_fort_fn = (ompi_errhandler_fortran_handler_fn_t *)func;
        }

    }

  /* All done */

  return new_errhandler;
}

#if OPAL_ENABLE_FT_MPI
#include "opal/mca/threads/wait_sync.h"

int ompi_errhandler_proc_failed_internal(ompi_proc_t* ompi_proc, int status, bool forward)
{
    int rc = OMPI_SUCCESS, max_num_comm = 0, i, proc_rank;
    ompi_communicator_t *comm = NULL;
    ompi_group_t *group = NULL;
    bool remote = false;

    /* Mutual exclusion (we are going to manipulate global group objects etc). This function
     * may be invoked from the RTE thread.
     *
     * Note that this may be called from within the event-loop, hence this
     * function must NOT call recursively opal_progress().
     *
     * Note that we could use atomic CAS on field proc->proc_active instead. We decided to keep
     * the existing code because it is outside of the critical path and a lock
     * makes it easier to deal with the sentinel proc case.
     */
    opal_mutex_lock(&errhandler_ftmpi_lock);
    /* If we have already reported this error, ignore */
    if( !ompi_proc_is_active(ompi_proc) ) {
        opal_mutex_unlock(&errhandler_ftmpi_lock);
        return rc;
    }
    /* Process State:
     * Update process state to failed */
    ompi_proc_mark_as_failed(ompi_proc);
    opal_mutex_unlock(&errhandler_ftmpi_lock);

    opal_output_verbose(1, ompi_ftmpi_output_handle,
                        "%s ompi: Process %s failed (state = %d %s).",
                        OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                        OMPI_NAME_PRINT(&ompi_proc->super.proc_name),
                        status, PMIx_Error_string(status) );

    if(90 < opal_output_get_verbosity(ompi_ftmpi_output_handle)) {
        /* how did we get there? */
        opal_backtrace_print(stderr, NULL, 0);
    }

    /* Communicator State:
     * Let them know about the failure. */
    max_num_comm = opal_pointer_array_get_size(&ompi_mpi_communicators);
    for( i = 0; i < max_num_comm; ++i ) {
        comm = (ompi_communicator_t *)opal_pointer_array_get_item(&ompi_mpi_communicators, i);
        if( NULL == comm ) {
            continue;
        }

        /* Look in both the local and remote group for this process */
        proc_rank = ompi_group_proc_lookup_rank(comm->c_local_group, ompi_proc);
        remote = false;
        if( (proc_rank < 0) && (comm->c_local_group != comm->c_remote_group) ) {
            proc_rank = ompi_group_proc_lookup_rank(comm->c_remote_group, ompi_proc);
            remote = true;
        }
        if( proc_rank < 0 ) {
            continue;  /* Not in this communicator, continue */
        }

        /* Notify the communicator to update as necessary */
        ompi_comm_set_rank_failed(comm, proc_rank, remote);

        if( NULL == group ) {  /* Build a group with the failed process */
            rc = ompi_group_incl((remote ? comm->c_remote_group : comm->c_local_group),
                                 1, &proc_rank,
                                 &group);
            if(OPAL_UNLIKELY( OMPI_SUCCESS != rc )) goto cleanup;
        }
        OPAL_OUTPUT_VERBOSE((10, ompi_ftmpi_output_handle,
                             "%s ompi: Process %s is in comm (%s) with rank %d. [%s]",
                             OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                             OMPI_NAME_PRINT(&ompi_proc->super.proc_name),
                             ompi_comm_print_cid(comm),
                             proc_rank,
                             (OMPI_ERRHANDLER_TYPE_PREDEFINED == comm->errhandler_type ? "P" :
                              (OMPI_ERRHANDLER_TYPE_COMM == comm->errhandler_type ? "C" :
                               (OMPI_ERRHANDLER_TYPE_WIN == comm->errhandler_type ? "W" :
                                (OMPI_ERRHANDLER_TYPE_FILE == comm->errhandler_type ? "F" : "U") ) ) )
                             ));
    }

    /* Group State:
     * Add the failed process to the global group of failed processes. */
    if( group != NULL ) {
        opal_mutex_lock(&ompi_group_afp_mutex);
        ompi_group_t *afp = ompi_group_all_failed_procs;
        rc = ompi_group_union(afp, group, &ompi_group_all_failed_procs);
        opal_mutex_unlock(&ompi_group_afp_mutex);
        if(OPAL_UNLIKELY( OMPI_SUCCESS != rc )) {
            goto cleanup;
        }
        OBJ_RELEASE(afp);
    }

    /* Point-to-Point:
     * Let the active request know of the process state change.
     * The wait function has a check, so all we need to do here is
     * signal it so it will check again.
     */
    wait_sync_global_wakeup(PMIX_ERR_PROC_ABORTED == status? MPI_ERR_PROC_ABORTED: MPI_ERR_PROC_FAILED);

    /* Collectives:
     * Propagate the error (this has been selected rather than the "roll
     * forward through errors in collectives" as this is less intrusive to the
     * code base.) */
    if( forward ) {
        /* TODO: this to become redundant when pmix has rbcast */
        ompi_comm_failure_propagate(&ompi_mpi_comm_world.comm, ompi_proc, status);
        /* Let pmix know: flush modex information, propagate to connect/accept
         * jobs; we will tell our local daemon, and it will do the proper thing */
        bool active = true;
        pmix_proc_t pmix_source;
        pmix_proc_t pmix_proc;
        pmix_info_t pmix_info[1];
        pmix_status_t prc;

        OPAL_PMIX_CONVERT_NAME(&pmix_source, OMPI_PROC_MY_NAME);
        OPAL_PMIX_CONVERT_NAME(&pmix_proc, &ompi_proc->super.proc_name);
        PMIX_INFO_CONSTRUCT(&pmix_info[0]);
        PMIX_INFO_LOAD(&pmix_info[0], PMIX_EVENT_AFFECTED_PROC, &pmix_proc, PMIX_PROC);
        prc = PMIx_Notify_event(PMIX_ERR_PROC_TERM_WO_SYNC, &pmix_source, PMIX_RANGE_LOCAL,
                                pmix_info, 1, NULL, &active);
        if( PMIX_SUCCESS != prc &&
            PMIX_OPERATION_SUCCEEDED != prc ) {
            /* Lets hope for the best: maybe someone else will succeed in
             * reporting, or PRTE will figure it out itself.
             * Complain but keep going. */
            OMPI_ERROR_LOG(prc);
        }
        PMIX_INFO_DESTRUCT(&pmix_info[0]);
    }

 cleanup:
    return rc;
}
#endif /* OPAL_ENABLE_FT_MPI */

/* helper to move the error report back from the RTE thread to the MPI thread */
typedef struct ompi_errhandler_event_s {
    opal_event_t super;
    int status;
    opal_process_name_t source;
    int nvalue;
    pmix_info_t info[];
} ompi_errhandler_event_t;

static void *ompi_errhandler_event_cb(int fd, int flags, void *context) {
    ompi_errhandler_event_t *event = (ompi_errhandler_event_t*) context;
    int status = event->status;
    opal_process_name_t source = event->source;
    opal_process_name_t prc;
    int rc;
#if OPAL_ENABLE_FT_MPI
    switch( status ) {
    case PMIX_ERR_PROC_TERM_WO_SYNC:
    case PMIX_ERR_PROC_ABORTED_BY_SIG:
    case PMIX_ERR_PROC_ABORTED: /* that is, proc aborted by pmix_abort */
        for(int i = 0; i < event->nvalue; i++) {
            if (PMIX_PROC != event->info[i].value.type) {
                OPAL_OUTPUT_VERBOSE((70, ompi_ftmpi_output_handle,
                    "%s ompi: ignoring the following key for a PMIx fault event: %s",
                    OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
                    event->info[i].key));
                continue;
            }
            OPAL_PMIX_CONVERT_PROCT(rc, &prc, event->info[i].value.data.proc);
            if (OPAL_SUCCESS != rc) {
                OPAL_ERROR_LOG(rc);
                break;
            }
            ompi_proc_t *proc = (ompi_proc_t*)ompi_proc_for_name(prc);
            if( NULL == proc ) {
                continue; /* we are not 'MPI connected' with this proc. */
            }
            assert( !ompi_proc_is_sentinel(proc) );
            ompi_errhandler_proc_failed_internal(proc, status, false);
        }
        opal_event_del(&event->super);
        free(event);
        return NULL;
    case PMIX_ERR_LOST_CONNECTION:
        opal_output_verbose(1, ompi_ftmpi_output_handle,
            "%s ompi: Error event PMIX_ERR_LOST_CONNECTION reported, that usually means that my daemon died thus I need to go away.",
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME));
        break;
    default:
        /* An unmanaged type of failure, let it do its thing. */
        opal_output_verbose(1, ompi_ftmpi_output_handle,
            "%s ompi: Error event reported through PMIx from %s (state = %d). "
            "This error type is not handled by the fault tolerant layer "
            "and the application will now presumably abort.",
            OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),
            OPAL_NAME_PRINT(source),
            status );
    }
#endif /* OPAL_ENABLE_FT_MPI */
    opal_event_del(&event->super);
    free(event);
    /* our default action is to abort */
    /* TODO: this error should store the error and later invoke an
     * error handler from an impacted MPI API call.
     * For now, it is fatal. */
    ompi_mpi_errors_are_fatal_comm_handler(NULL, &status, "PMIx Event Notification");
    return NULL;
}

/* registration callback */
void ompi_errhandler_registration_callback(int status,
                                           size_t errhandler_ref,
                                           void *cbdata)
{
    ompi_errhandler_errtrk_t *errtrk = (ompi_errhandler_errtrk_t*)cbdata;

    default_errhandler_id = errhandler_ref;
    errtrk->status = status;
    errtrk->active = false;
}

/**
 * Default errhandler callback for RTE reported errors
 */
void ompi_errhandler_callback(size_t refid, pmix_status_t status,
                              const pmix_proc_t *source,
                              pmix_info_t *info, size_t ninfo,
                              pmix_info_t *results, size_t nresults,
                              pmix_event_notification_cbfunc_fn_t cbfunc,
                              void *cbdata)
{
    int rc;
    size_t i;
    /* an error has been found, report to the MPI layer and let it take
     * further action. */
    /* transition this from the RTE thread to the MPI progress engine */
    ompi_errhandler_event_t *event = malloc(sizeof(*event)+ninfo*sizeof(pmix_info_t));
    if(NULL == event) {
        OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
        goto error;
    }
    event->status = status;
    OPAL_PMIX_CONVERT_PROCT(rc, &event->source, (pmix_proc_t*)source);
    if(OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        OMPI_ERROR_LOG(rc);
        free(event);
        goto error;
    }
    event->nvalue = ninfo;
    for (i = 0; i < ninfo; i++) {
        PMIX_INFO_XFER(&event->info[i], &info[i]);
    }
    opal_event_set(opal_sync_event_base, &event->super, -1, OPAL_EV_READ,
                   ompi_errhandler_event_cb, event);
    opal_event_active(&event->super, OPAL_EV_READ, 1);
    /* tell the event chain engine to go no further - we
     * will handle this */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    return;

error:
    if (NULL != cbfunc) {
        /* We can't handle this, let the default action abort. */
        cbfunc(PMIX_EVENT_NO_ACTION_TAKEN, NULL, 0, NULL, NULL, cbdata);
    }
}



/**************************************************************************
 *
 * Static functions
 *
 **************************************************************************/

/**
 * Errhandler constructor
 */
static void ompi_errhandler_construct(ompi_errhandler_t *new_errhandler)
{
  int ret_val;

  /* assign entry in fortran <-> c translation array */

  ret_val = opal_pointer_array_add(&ompi_errhandler_f_to_c_table,
                                   new_errhandler);
  new_errhandler->eh_f_to_c_index = ret_val;

  new_errhandler->eh_lang = OMPI_ERRHANDLER_LANG_C;

  new_errhandler->eh_comm_fn      = NULL;
  new_errhandler->eh_win_fn       = NULL;
  new_errhandler->eh_file_fn      = NULL;
  new_errhandler->eh_fort_fn      = NULL;

  memset (new_errhandler->eh_name, 0, MPI_MAX_OBJECT_NAME);
}


/**
 * Errhandler destructor
 */
static void ompi_errhandler_destruct(ompi_errhandler_t *errhandler)
{
  /* reset the ompi_errhandler_f_to_c_table entry - make sure that the
     entry is in the table */

  if (NULL!= opal_pointer_array_get_item(&ompi_errhandler_f_to_c_table,
                                        errhandler->eh_f_to_c_index)) {
    opal_pointer_array_set_item(&ompi_errhandler_f_to_c_table,
                                errhandler->eh_f_to_c_index, NULL);
  }
}
