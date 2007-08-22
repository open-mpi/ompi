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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "opal/util/output.h"

#include "orte/dss/dss.h"

#include "orte/mca/ns/base/base.h"
#include "orte/mca/ns/base/ns_private.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/ns/base/static-components.h"

/*
 * globals
 */

orte_process_name_t orte_ns_name_wildcard = {ORTE_CELLID_WILDCARD, ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD};
orte_process_name_t orte_ns_name_invalid = {ORTE_CELLID_INVALID, ORTE_JOBID_INVALID, ORTE_VPID_INVALID}; 
orte_process_name_t orte_ns_name_my_hnp = {0, 0, 0}; 

/*
 * Global variables
 */
int mca_ns_base_output = -1;
mca_ns_base_module_t orte_ns = {
    /* init */
    orte_ns_base_module_init_not_available,
    /* cell functions */
    orte_ns_base_create_cellid_not_available,
    orte_ns_base_get_cell_info_not_available,
    orte_ns_base_get_cellid_string,
    orte_ns_base_convert_cellid_to_string,
    orte_ns_base_convert_string_to_cellid,
    /* node functions */
    orte_ns_base_create_nodeids_not_available,
    orte_ns_base_get_node_info_not_available,
    orte_ns_base_convert_nodeid_to_string,
    orte_ns_base_convert_string_to_nodeid,
    /* jobid functions */
    orte_ns_base_create_jobid_not_available,
    orte_ns_base_get_job_descendants_not_available,
    orte_ns_base_get_job_children_not_available,
    orte_ns_base_get_root_job_not_available,
    orte_ns_base_get_parent_job_not_available,
    orte_ns_base_get_jobid_string,
    orte_ns_base_convert_jobid_to_string,
    orte_ns_base_convert_string_to_jobid,
    orte_ns_base_get_vpid_range_not_available,
    /* vpid functions */
    orte_ns_base_get_vpid_string,
    orte_ns_base_convert_vpid_to_string,
    orte_ns_base_convert_string_to_vpid,
    /* name functions */
    orte_ns_base_create_process_name,
    orte_ns_base_create_my_name_not_available,
    orte_ns_base_convert_string_to_process_name,
    orte_ns_base_get_proc_name_string,
    orte_ns_base_compare_fields,
    /* peer functions */
    orte_ns_base_get_peers_not_available,
    /* tag server functions */
    orte_ns_base_assign_rml_tag_not_available,
    /* data type functions */
    orte_ns_base_define_data_type_not_available,
    /* diagnostic functions */
    orte_ns_base_dump_cells_not_available,
    orte_ns_base_dump_jobs_not_available,
    orte_ns_base_dump_tags_not_available,
    orte_ns_base_dump_datatypes_not_available
};

bool mca_ns_base_selected = false;
opal_list_t mca_ns_base_components_available;
mca_ns_base_component_t mca_ns_base_selected_component;


/* constructor - used to initialize namelist instance */
static void orte_namelist_construct(orte_namelist_t* list)
{
    list->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_namelist_destructor(orte_namelist_t* list)
{
    if (NULL != list->name) {
    free(list->name);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
           orte_namelist_t,              /* type name */
           opal_list_item_t,                        /* parent "class" name */
           orte_namelist_construct,    /* constructor */
           orte_namelist_destructor);  /* destructor */



/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_ns_base_open(void)
{
    int param, value, rc;
    orte_data_type_t tmp;
    opal_output_stream_t kill_prefix;

    /* Debugging / verbose output */
    /** setup the structure to kill the blasted prefix that opal_output
     * now defaults to including so the output can be legible again!
     */
    OBJ_CONSTRUCT(&kill_prefix, opal_output_stream_t);
    kill_prefix.lds_want_stderr = true;
    kill_prefix.lds_prefix = NULL;
    
    param = mca_base_param_reg_int_name("ns", "base_verbose",
                                        "Verbosity level for the ns framework",
                                        false, false, 0, &value);
    if (value != 0) {
        kill_prefix.lds_verbose_level = value;
    }
    mca_ns_base_output = opal_output_open(&kill_prefix);

    /* register the base system types with the DPS */
    tmp = ORTE_NAME;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_ns_base_pack_name,
                                        orte_ns_base_unpack_name,
                                        (orte_dss_copy_fn_t)orte_ns_base_copy_name,
                                        (orte_dss_compare_fn_t)orte_ns_base_compare_name,
                                        (orte_dss_size_fn_t)orte_ns_base_std_size,
                                        (orte_dss_print_fn_t)orte_ns_base_print_name,
                                        (orte_dss_release_fn_t)orte_ns_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_NAME", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

    tmp = ORTE_VPID;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_ns_base_pack_vpid,
                                        orte_ns_base_unpack_vpid,
                                        (orte_dss_copy_fn_t)orte_ns_base_copy_vpid,
                                        (orte_dss_compare_fn_t)orte_ns_base_compare_vpid,
                                        (orte_dss_size_fn_t)orte_ns_base_std_size,
                                        (orte_dss_print_fn_t)orte_ns_base_std_print,
                                        (orte_dss_release_fn_t)orte_ns_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_VPID", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

    tmp = ORTE_JOBID;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_ns_base_pack_jobid,
                                        orte_ns_base_unpack_jobid,
                                        (orte_dss_copy_fn_t)orte_ns_base_copy_jobid,
                                        (orte_dss_compare_fn_t)orte_ns_base_compare_jobid,
                                        (orte_dss_size_fn_t)orte_ns_base_std_size,
                                        (orte_dss_print_fn_t)orte_ns_base_std_print,
                                        (orte_dss_release_fn_t)orte_ns_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_JOBID", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

    tmp = ORTE_CELLID;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_ns_base_pack_cellid,
                                        orte_ns_base_unpack_cellid,
                                        (orte_dss_copy_fn_t)orte_ns_base_copy_cellid,
                                        (orte_dss_compare_fn_t)orte_ns_base_compare_cellid,
                                        (orte_dss_size_fn_t)orte_ns_base_std_size,
                                        (orte_dss_print_fn_t)orte_ns_base_std_print,
                                        (orte_dss_release_fn_t)orte_ns_base_std_release,
                                        ORTE_DSS_UNSTRUCTURED,
                                        "ORTE_CELLID", &tmp))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

                    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("ns", mca_ns_base_output,
                                 mca_ns_base_static_components,
                                 &mca_ns_base_components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}
