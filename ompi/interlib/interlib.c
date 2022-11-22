/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/mca/pmix/pmix-internal.h"
#include "opal/util/proc.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/interlib/interlib.h"

#include "mpi.h"

static void model_callback(size_t refid, pmix_status_t status,
                           const pmix_proc_t *source,
                           pmix_info_t *info, size_t ninfo,
                           pmix_info_t *results, size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc,
                           void *cbdata)
{
    size_t n;

    if (NULL != getenv("OMPI_SHOW_MODEL_CALLBACK")) {
        /* we can ignore our own callback as we obviously
         * know that we are MPI */
        if (NULL != info) {
            for (n=0; n < ninfo; n++) {
                if (PMIX_CHECK_KEY(&info[n], PMIX_PROGRAMMING_MODEL) &&
                    0 == strcmp(info[n].value.data.string, "MPI")) {
                    goto cback;
                }
                if (PMIX_STRING == info[n].value.type) {
                        opal_output(0, "OMPI Model Callback Key: %s Val %s", info[n].key, info[n].value.data.string);
                }
            }
        }
    }
    /* otherwise, do something clever here */

  cback:
    /* we must NOT tell the event handler state machine that we
     * are the last step as that will prevent it from notifying
     * anyone else that might be listening for declarations */
    if (NULL != cbfunc) {
        cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
    }
}

static void evhandler_reg_callbk(pmix_status_t status,
                                 size_t evhandler_ref,
                                 void *cbdata)
{
    opal_pmix_lock_t *lock = (opal_pmix_lock_t*)cbdata;

    lock->status = status;
    OPAL_PMIX_WAKEUP_THREAD(lock);
}

int ompi_interlib_declare(int threadlevel, char *version)
{
    pmix_info_t info[4], directives;
    int ret;
    pmix_status_t rc;
    pmix_status_t code = PMIX_MODEL_DECLARED;
    opal_pmix_lock_t mylock;

    /* Register an event handler for library model declarations  */
    /* give it a name so we can distinguish it */
    PMIX_INFO_LOAD(&directives, PMIX_EVENT_HDLR_NAME, "MPI-Model-Declarations", PMIX_STRING);
    /* we could constrain the range to proc_local - technically, this
     * isn't required so long as the code that generates
     * the event stipulates its range as proc_local. We rely
     * on that here */
    OPAL_PMIX_CONSTRUCT_LOCK(&mylock);
    PMIx_Register_event_handler(&code, 1, &directives, 1, model_callback, evhandler_reg_callbk, (void*)&mylock);
    OPAL_PMIX_WAIT_THREAD(&mylock);
    PMIX_INFO_DESTRUCT(&directives);
    rc = mylock.status;
    OPAL_PMIX_DESTRUCT_LOCK(&mylock);
    if (PMIX_SUCCESS != rc) {
        return OMPI_ERROR;
    }

    /* declare that we are present and active */
    PMIX_INFO_LOAD(&info[0], PMIX_PROGRAMMING_MODEL, "MPI", PMIX_STRING);
    PMIX_INFO_LOAD(&info[1], PMIX_MODEL_LIBRARY_NAME, "OpenMPI", PMIX_STRING);
    PMIX_INFO_LOAD(&info[2], PMIX_MODEL_LIBRARY_VERSION, version, PMIX_STRING);
    if (MPI_THREAD_SINGLE == threadlevel) {
        PMIX_INFO_LOAD(&info[3], PMIX_THREADING_MODEL, "NONE", PMIX_STRING);
    } else {
        PMIX_INFO_LOAD(&info[3], PMIX_THREADING_MODEL, "PTHREAD", PMIX_STRING);
    }

    /* call pmix to initialize these values */
    rc = PMIx_Init(NULL, info, 4);
    PMIX_INFO_DESTRUCT(&info[0]);
    PMIX_INFO_DESTRUCT(&info[1]);
    PMIX_INFO_DESTRUCT(&info[2]);
    PMIX_INFO_DESTRUCT(&info[3]);
    /* account for our refcount on pmix_init */
    PMIx_Finalize(NULL, 0);
    if (opal_process_info.is_singleton && PMIX_ERR_UNREACH == rc) {
        ret = OMPI_SUCCESS;
    } else {
        ret = opal_pmix_convert_status(rc);
    }
    return ret;
}
