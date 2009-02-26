/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#if !ORTE_DISABLE_FULL_SUPPORT
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/trace.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/argv.h"
#include "opal/class/opal_value_array.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"
#include "orte/util/parse_options.h"

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

/* instance the child list object */
static void orte_odls_child_constructor(orte_odls_child_t *ptr)
{
    ptr->name = NULL;
    ptr->pid = 0;
    ptr->app_idx = -1;
    ptr->alive = false;
    ptr->coll_recvd = false;
    /* set the default state to "failed to start" so
     * we can correctly report should something
     * go wrong during launch
     */
    ptr->state = ORTE_PROC_STATE_FAILED_TO_START;
    ptr->exit_code = 0;
    ptr->rml_uri = NULL;
    ptr->slot_list = NULL;
    ptr->waitpid_recvd = false;
    ptr->iof_complete = false;
}
static void orte_odls_child_destructor(orte_odls_child_t *ptr)
{
    if (NULL != ptr->name) free(ptr->name);
    if (NULL != ptr->rml_uri) free(ptr->rml_uri);
    if (NULL != ptr->slot_list) free(ptr->slot_list);
}
OBJ_CLASS_INSTANCE(orte_odls_child_t,
                   opal_list_item_t,
                   orte_odls_child_constructor,
                   orte_odls_child_destructor);

static void orte_odls_job_constructor(orte_odls_job_t *ptr)
{
    ptr->jobid = ORTE_JOBID_INVALID;
    ptr->launch_msg_processed = false;
    ptr->apps = NULL;
    ptr->num_apps = 0;
    ptr->controls = 0;
    ptr->stdin_target = ORTE_VPID_INVALID;
    ptr->total_slots_alloc = 0;
    ptr->num_procs = 0;
    ptr->num_local_procs = 0;
    ptr->pmap = NULL;
    OBJ_CONSTRUCT(&ptr->collection_bucket, opal_buffer_t);
    OBJ_CONSTRUCT(&ptr->local_collection, opal_buffer_t);
    ptr->collective_type = ORTE_GRPCOMM_COLL_NONE;
    ptr->num_contributors = 0;
    ptr->num_participating = -1;
    ptr->num_collected = 0;
}
static void orte_odls_job_destructor(orte_odls_job_t *ptr)
{
    orte_std_cntr_t i;
    
    if (NULL != ptr->apps) {
        for (i=0; i < ptr->num_apps; i++) {
            OBJ_RELEASE(ptr->apps[i]);
        }
        if (NULL != ptr->apps) {
            free(ptr->apps);
        }
    }
    
    if (NULL != ptr->pmap && NULL != ptr->pmap->bytes) {
        free(ptr->pmap->bytes);
        free(ptr->pmap);
    }
    
    OBJ_DESTRUCT(&ptr->collection_bucket);
    OBJ_DESTRUCT(&ptr->local_collection);
}
OBJ_CLASS_INSTANCE(orte_odls_job_t,
                   opal_list_item_t,
                   orte_odls_job_constructor,
                   orte_odls_job_destructor);

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
    int i, rank;
    orte_namelist_t *nm;
    bool xterm_hold;
    
    /* Debugging / verbose output.  Always have stream open, with
        verbose set by the mca open system... */
    orte_odls_globals.output = opal_output_open(NULL);

    mca_base_param_reg_int_name("odls", "base_sigkill_timeout",
                                "Time to wait for a process to die after issuing a kill signal to it",
                                false, false, 1, &orte_odls_globals.timeout_before_sigkill);

    /* initialize ODLS globals */
    OBJ_CONSTRUCT(&orte_odls_globals.mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_odls_globals.cond, opal_condition_t);
    OBJ_CONSTRUCT(&orte_odls_globals.xterm_ranks, opal_list_t);
    orte_odls_globals.xtermcmd = NULL;
    orte_odls_globals.dmap = NULL;
    orte_odls_globals.debugger = NULL;
    orte_odls_globals.debugger_launched = false;
    
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
            opal_list_append(&orte_odls_globals.xterm_ranks, &nm->item);
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
#endif
