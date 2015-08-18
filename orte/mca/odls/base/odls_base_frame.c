/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "opal/class/opal_ring_buffer.h"
#include "orte/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/util/parse_options.h"
#include "orte/mca/ess/ess.h"

#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/odls/base/static-components.h"

/*
 * Instantiate globals
 */
orte_odls_base_module_t orte_odls = {0};

/*
 * Framework global variables
 */
orte_odls_globals_t orte_odls_globals = {0};

static int orte_odls_base_register(mca_base_register_flag_t flags)
{
    orte_odls_globals.timeout_before_sigkill = 1;
    (void) mca_base_var_register("orte", "odls", "base", "sigkill_timeout",
                                 "Time to wait for a process to die after issuing a kill signal to it",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &orte_odls_globals.timeout_before_sigkill);

    return ORTE_SUCCESS;
}

static int orte_odls_base_close(void)
{
    int i;
    orte_proc_t *proc;
    opal_list_item_t *item;

    /* cleanup ODLS globals */
    while (NULL != (item = opal_list_remove_first(&orte_odls_globals.xterm_ranks))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_odls_globals.xterm_ranks);

    /* cleanup the global list of local children and job data */
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            OBJ_RELEASE(proc);
        }
    }
    OBJ_RELEASE(orte_local_children);

    return mca_base_framework_components_close(&orte_odls_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int orte_odls_base_open(mca_base_open_flag_t flags)
{
    char **ranks=NULL, *tmp;
    int rc, i, rank;
    orte_namelist_t *nm;
    bool xterm_hold;

    /* initialize the global array of local children */
    orte_local_children = OBJ_NEW(opal_pointer_array_t);
    if (OPAL_SUCCESS != (rc = opal_pointer_array_init(orte_local_children,
                                                      1,
                                                      ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                                      1))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* initialize ODLS globals */
    OBJ_CONSTRUCT(&orte_odls_globals.xterm_ranks, opal_list_t);
    orte_odls_globals.xtermcmd = NULL;

    /* check if the user requested that we display output in xterms */
    if (NULL != orte_xterm) {
        /* construct a list of ranks to be displayed */
        xterm_hold = false;
        orte_util_parse_range_options(orte_xterm, &ranks);
        for (i=0; i < opal_argv_count(ranks); i++) {
            if (0 == strcmp(ranks[i], "BANG")) {
                xterm_hold = true;
                continue;
            }
            nm = OBJ_NEW(orte_namelist_t);
            rank = strtol(ranks[i], NULL, 10);
            if (-1 == rank) {
                /* wildcard */
                nm->name.vpid = ORTE_VPID_WILDCARD;
            } else if (rank < 0) {
                /* error out on bozo case */
                orte_show_help("help-orte-odls-base.txt",
                               "orte-odls-base:xterm-neg-rank",
                               true, rank);
                return ORTE_ERROR;
            } else {
                /* we can't check here if the rank is out of
                 * range as we don't yet know how many ranks
                 * will be in the job - we'll check later
                 */
                nm->name.vpid = rank;
            }
            opal_list_append(&orte_odls_globals.xterm_ranks, &nm->super);
        }
        opal_argv_free(ranks);
        /* construct the xtermcmd */
        orte_odls_globals.xtermcmd = NULL;
        tmp = opal_find_absolute_path("xterm");
        if (NULL == tmp) {
            return ORTE_ERROR;
        }
        opal_argv_append_nosize(&orte_odls_globals.xtermcmd, tmp);
        free(tmp);
        opal_argv_append_nosize(&orte_odls_globals.xtermcmd, "-T");
        opal_argv_append_nosize(&orte_odls_globals.xtermcmd, "save");
        if (xterm_hold) {
            opal_argv_append_nosize(&orte_odls_globals.xtermcmd, "-hold");
        }
        opal_argv_append_nosize(&orte_odls_globals.xtermcmd, "-e");
    }

     /* Open up all available components */
    return mca_base_framework_components_open(&orte_odls_base_framework, flags);
}

MCA_BASE_FRAMEWORK_DECLARE(orte, odls, "ORTE Daemon Launch Subsystem",
                           orte_odls_base_register, orte_odls_base_open, orte_odls_base_close,
                           mca_odls_base_static_components, 0);

static void launch_local_const(orte_odls_launch_local_t *ptr)
{
    ptr->ev = opal_event_alloc();
    ptr->job = ORTE_JOBID_INVALID;
    ptr->fork_local = NULL;
    ptr->retries = 0;
}
static void launch_local_dest(orte_odls_launch_local_t *ptr)
{
    opal_event_free(ptr->ev);
}
OBJ_CLASS_INSTANCE(orte_odls_launch_local_t,
                   opal_object_t,
                   launch_local_const,
                   launch_local_dest);
