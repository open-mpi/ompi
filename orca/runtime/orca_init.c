/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orca_config.h"
#include "orca/constants.h"

#include "orca/include/rte_orca.h"
#include "orca/runtime/runtime.h"
#include "orca/runtime/orca_dt.h"
#include "orca/mca/stems/base/base.h"

#include "opal/runtime/opal.h"

orca_process_name_t orca_name_wildcard = {ORCA_JOBID_WILDCARD, ORCA_VPID_WILDCARD};
orca_process_name_t orca_name_invalid  = {ORCA_JOBID_INVALID,  ORCA_VPID_INVALID };

int orca_init_counter = 0;
bool orca_initialized = false;

orca_proc_info_t *orca_process_info = NULL;

int orca_init(int *pargc, char *** pargv)
{
    int ret, exit_status = ORCA_SUCCESS;
    opal_data_type_t tmp;

    /*
     * Only initialize once
     */
    if( orca_init_counter != 0 ) {
        ++orca_init_counter;
        return ORCA_SUCCESS;
    }
    ++orca_init_counter;
    orca_initialized = true;

    /*
     * Initialize the OPAL layer
     */
    if( ORCA_SUCCESS != (ret = opal_init(pargc, pargv)) ) {
        opal_output(0, "Error: opal_init() failed (%d)", ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Register MCA parameters
     */
    if( ORCA_SUCCESS != (ret = orca_register_params()) ) {
        opal_output(0, "Error: orca_register_params() failed (%d)", ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Stem framework
     */
    if( ORCA_SUCCESS != (ret = orca_stems_base_open()) ) {
        opal_output(0, "Error: orca_stems_base_open() failed (%d)", ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORCA_SUCCESS != (ret = orca_stems_base_select()) ) {
        opal_output(0, "Error: orca_stems_base_select() failed (%d)", ret);
        exit_status = ret;
        goto cleanup;
    }

    if( NULL != orca_stems.stems_init ) {
        if( ORCA_SUCCESS != (ret = orca_stems.stems_init(pargc, pargv)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Data Type Registration
     */
    tmp = ORCA_DSS_VPID;
    ret = opal_dss.register_type(orca_dt_pack_vpid,
                                 orca_dt_unpack_vpid,
                                 (opal_dss_copy_fn_t)orca_dt_copy_vpid,
                                 (opal_dss_compare_fn_t)orca_dt_compare_vpid,
                                 (opal_dss_print_fn_t)orca_dt_print_vpid,
                                 OPAL_DSS_UNSTRUCTURED,
                                 "ORCA_DSS_VPID",
                                 &tmp);
    if( OPAL_SUCCESS != ret ) {
        exit_status = ret;
        goto cleanup;
    }

    tmp = ORCA_DSS_JOBID;
    ret = opal_dss.register_type(orca_dt_pack_jobid,
                                 orca_dt_unpack_jobid,
                                 (opal_dss_copy_fn_t)orca_dt_copy_jobid,
                                 (opal_dss_compare_fn_t)orca_dt_compare_jobid,
                                 (opal_dss_print_fn_t)orca_dt_print_jobid,
                                 OPAL_DSS_UNSTRUCTURED,
                                 "ORCA_DSS_JOBID",
                                 &tmp);
    if( OPAL_SUCCESS != ret ) {
        exit_status = ret;
        goto cleanup;
    }

    tmp = ORCA_DSS_NAME;
    ret = opal_dss.register_type(orca_dt_pack_name,
                                 orca_dt_unpack_name,
                                 (opal_dss_copy_fn_t)orca_dt_copy_name,
                                 (opal_dss_compare_fn_t)orca_dt_compare_name,
                                 (opal_dss_print_fn_t)orca_dt_print_name,
                                 OPAL_DSS_UNSTRUCTURED,
                                 "ORCA_DSS_NAME",
                                 &tmp);
    if( OPAL_SUCCESS != ret ) {
        exit_status = ret;
        goto cleanup;
    }


 cleanup:
    return exit_status;
}
