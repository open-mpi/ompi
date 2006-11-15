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
#include "orte/orte_constants.h"

#include "orte/dss/dss.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/trace.h"

#include "orte/mca/rmgr/base/rmgr_private.h"
#include "orte/mca/rmgr/base/base.h"



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
    NULL,
    orte_rmgr_base_setup_job_not_available,
    orte_rmgr_base_spawn_not_available,
    orte_rmgr_base_connect,
    orte_rmgr_base_disconnect,
    orte_rmgr_base_finalize_not_available,
    /**   SUPPORT FUNCTIONS   ***/
    orte_rmgr_base_find_attribute,
    orte_rmgr_base_add_attribute,
    orte_rmgr_base_merge_attributes,
    orte_rmgr_base_delete_attribute,
    orte_rmgr_base_get_app_context,
    orte_rmgr_base_put_app_context,
    orte_rmgr_base_check_context_cwd,
    orte_rmgr_base_check_context_app,
    orte_rmgr_base_set_vpid_range,
    orte_rmgr_base_get_vpid_range
    
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
    app_context->argv=NULL;
    app_context->env=NULL;
    app_context->cwd=NULL;
    app_context->num_map = 0;
    app_context->map_data = NULL;
    app_context->prefix_dir = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_app_context_destructor(orte_app_context_t* app_context)
{
    orte_std_cntr_t i;

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
        if (NULL != app_context->map_data) {
            free(app_context->map_data);
        }
    }
    
    if (NULL != app_context->prefix_dir) {
        free(app_context->prefix_dir);
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

/*
 * ATTRIBUTE
 */
static void orte_attribute_construct(orte_attribute_t *a)
{
    a->key = NULL;
    a->value = NULL;
}

static void orte_attribute_destruct(orte_attribute_t *a)
{
    if (NULL != a->key) free(a->key);
    if (NULL != a->value) OBJ_RELEASE(a->value);
}


OBJ_CLASS_INSTANCE(orte_attribute_t,
                   opal_list_item_t,
                   orte_attribute_construct,
                   orte_attribute_destruct);

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */

int orte_rmgr_base_open(void)
{
    int param, value, rc;
    orte_data_type_t tmp;

    OPAL_TRACE(5);

    /* Debugging / verbose output */

    orte_rmgr_base.rmgr_output = opal_output_open(NULL);
    param = mca_base_param_reg_int_name("rmgr_base", "verbose",
                                        "Verbosity level for the rmgr framework",
                                        false, false, 0, &value);
    if (value != 0) {
        orte_rmgr_base.rmgr_output = opal_output_open(NULL);
    } else {
        orte_rmgr_base.rmgr_output = -1;
    }

    /* register the base system types with the DSS */
    tmp = ORTE_APP_CONTEXT;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_rmgr_base_pack_app_context,
                                        orte_rmgr_base_unpack_app_context,
                                        (orte_dss_copy_fn_t)orte_rmgr_base_copy_app_context,
                                        (orte_dss_compare_fn_t)orte_rmgr_base_compare_app_context,
                                        (orte_dss_size_fn_t)orte_rmgr_base_size_app_context,
                                        (orte_dss_print_fn_t)orte_rmgr_base_print_app_context,
                                        (orte_dss_release_fn_t)orte_rmgr_base_std_obj_release,
                                        ORTE_DSS_STRUCTURED,
                                        "ORTE_APP_CONTEXT", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

    tmp = ORTE_APP_CONTEXT_MAP;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_rmgr_base_pack_app_context_map,
                                        orte_rmgr_base_unpack_app_context_map,
                                        (orte_dss_copy_fn_t)orte_rmgr_base_copy_app_context_map,
                                        (orte_dss_compare_fn_t)orte_rmgr_base_compare_app_context_map,
                                        (orte_dss_size_fn_t)orte_rmgr_base_size_app_context_map,
                                        (orte_dss_print_fn_t)orte_rmgr_base_print_app_context_map,
                                        (orte_dss_release_fn_t)orte_rmgr_base_std_obj_release,
                                        ORTE_DSS_STRUCTURED,
                                        "ORTE_APP_CONTEXT_MAP", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_ATTRIBUTE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_rmgr_base_pack_attribute,
                                                     orte_rmgr_base_unpack_attribute,
                                                     (orte_dss_copy_fn_t)orte_rmgr_base_copy_attribute,
                                                     (orte_dss_compare_fn_t)orte_rmgr_base_compare_attribute,
                                                     (orte_dss_size_fn_t)orte_rmgr_base_size_attribute,
                                                     (orte_dss_print_fn_t)orte_rmgr_base_print_attribute,
                                                     (orte_dss_release_fn_t)orte_rmgr_base_std_obj_release,
                                                     ORTE_DSS_STRUCTURED,
                                                     "ORTE_ATTRIBUTE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_ATTR_LIST;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_rmgr_base_pack_attr_list,
                                                     orte_rmgr_base_unpack_attr_list,
                                                     (orte_dss_copy_fn_t)orte_rmgr_base_copy_attr_list,
                                                     (orte_dss_compare_fn_t)orte_rmgr_base_compare_attr_list,
                                                     (orte_dss_size_fn_t)orte_rmgr_base_size_attr_list,
                                                     (orte_dss_print_fn_t)orte_rmgr_base_print_attr_list,
                                                     (orte_dss_release_fn_t)orte_rmgr_base_std_obj_release,
                                                     ORTE_DSS_STRUCTURED,
                                                     "ORTE_ATTR_LIST", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("rmgr", orte_rmgr_base.rmgr_output,
                                 mca_rmgr_base_static_components,
                                 &orte_rmgr_base.rmgr_components, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}

