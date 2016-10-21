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
 * Copyright (c) 2013-2016 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include <src/include/pmix_config.h>

#include "src/class/pmix_object.h"
#include "src/client/pmix_client_ops.h"
#include "src/usock/usock.h"
#include "src/util/output.h"
#include "src/util/keyval_parse.h"
#include "src/util/show_help.h"
#include "src/mca/base/base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/mca/psec/base/base.h"
#include "src/dstore/pmix_dstore.h"
#include PMIX_EVENT_HEADER

#include "src/runtime/pmix_rte.h"
#include "src/runtime/pmix_progress_threads.h"

extern int pmix_initialized;
extern bool pmix_init_called;

static void __pmix_attribute_destructor__ pmix_cleanup (void)
{
    if (!pmix_initialized) {
        /* nothing to do */
        return;
    }

    /* finalize the class/object system */
    pmix_class_finalize();
}

void pmix_rte_finalize(void)
{
    if( --pmix_initialized != 0 ) {
        if( pmix_initialized < 0 ) {
            fprintf(stderr, "PMIx Finalize called too many times\n");
            return;
        }
        return;
    }

    if (!pmix_globals.external_evbase) {
        /* stop the progress thread */
        (void)pmix_progress_thread_finalize(NULL);
    }

    /* cleanup communications */
    pmix_usock_finalize();
    if (PMIX_PROC_SERVER != pmix_globals.proc_type &&
        0 <= pmix_client_globals.myserver.sd) {
        CLOSE_THE_SOCKET(pmix_client_globals.myserver.sd);
    }
    #if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
        pmix_dstore_finalize();
    #endif /* PMIX_ENABLE_DSTORE */

    /* close the security framework */
    (void)pmix_mca_base_framework_close(&pmix_psec_base_framework);

    /* Clear out all the registered MCA params */
    pmix_deregister_params();
    pmix_mca_base_var_finalize();

    /* keyval lex-based parser */
    pmix_util_keyval_parse_finalize();

    (void)pmix_mca_base_framework_close(&pmix_pinstalldirs_base_framework);

    /* finalize the show_help system */
    pmix_show_help_finalize();

    /* finalize the output system.  This has to come *after* the
       malloc code, as the malloc code needs to call into this, but
       the malloc code turning off doesn't affect pmix_output that
       much */
    pmix_output_finalize();

#if 0
    /* close the bfrops */
    (void)pmix_mca_base_framework_close(&pmix_bfrops_base_framework);
#endif

    /* clean out the globals */
    PMIX_RELEASE(pmix_globals.mypeer);
    PMIX_LIST_DESTRUCT(&pmix_globals.nspaces);
    if (NULL != pmix_globals.cache_local) {
        PMIX_RELEASE(pmix_globals.cache_local);
    }
    if (NULL != pmix_globals.cache_remote) {
        PMIX_RELEASE(pmix_globals.cache_remote);
    }
    PMIX_DESTRUCT(&pmix_globals.events);

    #if PMIX_NO_LIB_DESTRUCTOR
        pmix_cleanup();
    #endif
}
