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
 * Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#if !ORTE_DISABLE_FULL_SUPPORT
#include "opal/class/opal_ring_buffer.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
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

#endif

#include "orte/mca/odls/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/odls/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_odls_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Instantiate globals
 */
orte_odls_base_module_t orte_odls;

/*
 * Framework global variables
 */
orte_odls_base_t orte_odls_base;
orte_odls_globals_t orte_odls_globals;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_odls_base_open(void)
{
    char **ranks=NULL, *tmp;
    int rc, i, rank;
    orte_namelist_t *nm;
    bool xterm_hold;
    
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_odls_globals.output = opal_output_open(NULL);

    mca_base_param_reg_int_name("odls", "base_sigkill_timeout",
                                "Time to wait for a process to die after issuing a kill signal to it",
                                false, false, 1, &orte_odls_globals.timeout_before_sigkill);

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
                orte_show_help("help-odls-base.txt",
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
    if (ORTE_SUCCESS != 
        mca_base_components_open("odls", orte_odls_globals.output,
                                 mca_odls_base_static_components, 
                                 &orte_odls_base.available_components, true)) {
        return ORTE_ERROR;
    }

    /* are there components available for use ?  - 
     * orte_odls_base.available_components is always initialized */
    if(0 < opal_list_get_size(&(orte_odls_base.available_components))) {
        orte_odls_base.components_available = true;
    } else {
        orte_odls_base.components_available = false;
    }
    
    /* All done */
    
    return ORTE_SUCCESS;
}

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

#endif
