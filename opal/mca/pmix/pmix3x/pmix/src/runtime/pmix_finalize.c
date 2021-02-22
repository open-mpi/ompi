/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "src/include/pmix_config.h"

#include "src/class/pmix_object.h"
#include "src/client/pmix_client_ops.h"
#include "src/util/output.h"
#include "src/util/keyval_parse.h"
#include "src/util/show_help.h"
#include "src/mca/base/base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/pcompress/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/pif/base/base.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/mca/plog/base/base.h"
#include "src/mca/pnet/base/base.h"
#include "src/mca/preg/base/base.h"
#include "src/mca/psec/base/base.h"
#include "src/mca/psquash/base/base.h"
#include "src/mca/ptl/base/base.h"
#include PMIX_EVENT_HEADER

#include "src/runtime/pmix_rte.h"
#include "src/runtime/pmix_progress_threads.h"

extern int pmix_initialized;
extern bool pmix_init_called;

void pmix_rte_finalize(void)
{
    int i;
    pmix_notify_caddy_t *cd;
    pmix_iof_req_t *req;

    if( --pmix_initialized != 0 ) {
        if( pmix_initialized < 0 ) {
            fprintf(stderr, "PMIx Finalize called too many times\n");
            return;
        }
        return;
    }


    /* close plog */
    (void)pmix_mca_base_framework_close(&pmix_plog_base_framework);

    /* close preg */
    (void)pmix_mca_base_framework_close(&pmix_preg_base_framework);

    /* cleanup communications */
    (void)pmix_mca_base_framework_close(&pmix_ptl_base_framework);

    /* close the security framework */
    (void)pmix_mca_base_framework_close(&pmix_psec_base_framework);

    /* close bfrops */
    (void)pmix_mca_base_framework_close(&pmix_bfrops_base_framework);

    /* close the psquash framework */
    pmix_psquash.finalize();
    pmix_mca_base_framework_close(&pmix_psquash_base_framework);

    /* close compress */
    (void)pmix_mca_base_framework_close(&pmix_pcompress_base_framework);

    /* close GDS */
    (void)pmix_mca_base_framework_close(&pmix_gds_base_framework);

    /* finalize the mca */
    /* Clear out all the registered MCA params */
    pmix_deregister_params();
    pmix_mca_base_var_finalize();

    /* keyval lex-based parser */
    pmix_util_keyval_parse_finalize();

    (void)pmix_mca_base_framework_close(&pmix_pinstalldirs_base_framework);
    (void)pmix_mca_base_framework_close(&pmix_pif_base_framework);
    (void)pmix_mca_base_close();

    /* finalize the show_help system */
    pmix_show_help_finalize();

    /* finalize the output system.  This has to come *after* the
       malloc code, as the malloc code needs to call into this, but
       the malloc code turning off doesn't affect pmix_output that
       much */
    pmix_output_finalize();

    /* clean out the globals */
    PMIX_RELEASE(pmix_globals.mypeer);
    PMIX_DESTRUCT(&pmix_globals.events);
    PMIX_LIST_DESTRUCT(&pmix_globals.cached_events);
    /* clear any notifications */
    for (i=0; i < pmix_globals.max_events; i++) {
        pmix_hotel_checkout_and_return_occupant(&pmix_globals.notifications, i, (void**)&cd);
        if (NULL != cd) {
            PMIX_RELEASE(cd);
        }
    }
    PMIX_DESTRUCT(&pmix_globals.notifications);
    for (i=0; i < pmix_globals.iof_requests.size; i++) {
        if (NULL != (req = (pmix_iof_req_t*)pmix_pointer_array_get_item(&pmix_globals.iof_requests, i))) {
            PMIX_RELEASE(req);
        }
    }
    PMIX_DESTRUCT(&pmix_globals.iof_requests);
    PMIX_LIST_DESTRUCT(&pmix_globals.stdin_targets);
    if (NULL != pmix_globals.hostname) {
        free(pmix_globals.hostname);
        pmix_globals.hostname = NULL;
    }
    PMIX_LIST_DESTRUCT(&pmix_globals.nspaces);

    /* now safe to release the event base */
    if (!pmix_globals.external_evbase) {
        (void)pmix_progress_thread_stop(NULL);
    }

}
