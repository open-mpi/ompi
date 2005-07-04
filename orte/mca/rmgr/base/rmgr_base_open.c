/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "include/orte_constants.h"

#include "dps/dps.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "mca/rmgr/base/base.h"



/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rmgr/base/static-components.h"

/*
 * globals
 */

orte_rmgr_base_t orte_rmgr_base;
orte_rmgr_base_module_t orte_rmgr = {
    orte_rmgr_base_query_not_available,
    orte_rmgr_base_create_not_available,
    orte_rmgr_base_allocate_not_available,
    orte_rmgr_base_deallocate_not_available,
    orte_rmgr_base_map_not_available,
    orte_rmgr_base_launch_not_available,
    orte_rmgr_base_terminate_job_not_available,
    orte_rmgr_base_terminate_proc_not_available,
    orte_rmgr_base_spawn_not_available,
    orte_rmgr_base_proc_stage_gate_init,
    orte_rmgr_base_proc_stage_gate_mgr,
    orte_rmgr_base_finalize_not_available
};

/*
 * Setup up app_context_t class so we can use it
 */

/*
 * orte_app_context_t constructor
 */

static void orte_app_context_construct(orte_app_context_t* app_context)
{
    app_context->idx=0;
    app_context->app=NULL;
    app_context->num_procs=0;
    app_context->argc=0;
    app_context->argv=NULL;
    app_context->num_env=0;
    app_context->env=NULL;
    app_context->cwd=NULL;
    app_context->num_map = 0;
    app_context->map_data = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_app_context_destructor(orte_app_context_t* app_context)
{
    size_t i;

    if (NULL != app_context->app) {
        free (app_context->app);
    }

    /* argv and env lists created by util/argv copy functions */
    if (NULL != app_context->argv) {
       opal_argv_free(app_context->argv);
    }

    if (NULL != app_context->env) {
       opal_argv_free(app_context->env);
    }

    if (NULL != app_context->cwd) {
        free (app_context->cwd);
    }

    if (NULL != app_context->map_data) {
        for (i = 0; i < app_context->num_map; ++i) {
            if (NULL != app_context->map_data[i]) {
                OBJ_RELEASE(app_context->map_data[i]);
            }
        }
        free(app_context->map_data);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
           orte_app_context_t,  /* type name */
           opal_object_t, /* parent "class" name */
           orte_app_context_construct, /* constructor */
           orte_app_context_destructor); /* destructor */


static void orte_app_context_map_construct(orte_app_context_map_t *a) 
{
    a->map_type = ORTE_APP_CONTEXT_MAP_INVALID;
    a->map_data = NULL;
}

static void orte_app_context_map_destruct(orte_app_context_map_t *a) 
{
    if (NULL != a->map_data) {
        free(a->map_data);
    }
}


OBJ_CLASS_INSTANCE(orte_app_context_map_t, 
                   opal_object_t,
                   orte_app_context_map_construct,
                   orte_app_context_map_destruct);

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */

int orte_rmgr_base_open(void)
{
    int param, value, rc;
    orte_data_type_t tmp;

    /* Debugging / verbose output */

    orte_rmgr_base.rmgr_output = opal_output_open(NULL);
    param = mca_base_param_register_int("rmgr", "base", "verbose", NULL, 0);
    mca_base_param_lookup_int(param, &value);
    if (value != 0) {
        orte_rmgr_base.rmgr_output = opal_output_open(NULL);
    } else {
        orte_rmgr_base.rmgr_output = -1;
    }

    /* register the base system types with the DPS */
    tmp = ORTE_APP_CONTEXT;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_rmgr_base_pack_app_context,
                                          orte_rmgr_base_unpack_app_context,
                                          "ORTE_APP_CONTEXT", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_APP_CONTEXT_MAP;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_rmgr_base_pack_app_context_map,
                                          orte_rmgr_base_unpack_app_context_map,
                                          "ORTE_APP_CONTEXT_MAP", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* Open up all available components */

    if (ORTE_SUCCESS != 
        mca_base_components_open("rmgr", 0, mca_rmgr_base_static_components, 
                                 &orte_rmgr_base.rmgr_components, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}

