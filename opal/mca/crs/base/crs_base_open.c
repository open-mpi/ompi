/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/constants.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/util/output.h"

#include "opal/mca/crs/base/static-components.h"

/*
 * Globals
 */
int  opal_crs_base_output  = -1;
opal_crs_base_module_t opal_crs = {
    NULL, /* crs_init               */
    NULL, /* crs_finalize           */
    NULL, /* crs_checkpoint         */
    NULL, /* crs_restart_cmd        */
    NULL, /* crs_disable_checkpoint */
    NULL, /* crs_enable_checkpoint  */
    NULL, /* crs_prelaunch          */
    NULL  /* crs_reg_thread         */
};
opal_list_t opal_crs_base_components_available;
opal_crs_base_component_t opal_crs_base_selected_component;

bool crs_base_do_not_select = false;

static int crs_base_verbose = 0;

static int opal_crs_base_register(int flags)
{
    /*
     * Note: If we are a tool, then we will manually run the selection routine 
     *       for the checkpointer.  The tool will set the MCA parameter 
     *       'crs_base_do_not_select' before opal_init and then reset it after to 
     *       disable the selection logic.
     *       This is useful for opal_restart because it reads the metadata file
     *       that indicates the checkpointer to be used after calling opal_init.
     *       Therefore it would need to select a specific module, but it doesn't
     *       know which one until later. It will set the MCA parameter 'crs' 
     *       before calling this function.
     */
    crs_base_do_not_select = false;
    (void) mca_base_var_register("opal", "crs", "base", "do_not_select",
                                 "Do not do the selection of the CRS component",
                                 MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_INTERNAL,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY, &crs_base_do_not_select);


    /* Debugging/Verbose output */
    crs_base_verbose = 0;
    (void) mca_base_var_register("opal", "crs", "base", "verbose",
                                 "Verbosity level of the CRS framework",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &crs_base_verbose);

    return OPAL_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int opal_crs_base_open(void)
{
    int ret, exit_status = OPAL_SUCCESS;

    (void) opal_crs_base_register(0);

    if(0 != crs_base_verbose) {
        opal_crs_base_output = opal_output_open(NULL);
    } else {
        opal_crs_base_output = -1;
    }
    opal_output_set_verbosity(opal_crs_base_output, crs_base_verbose);

    if( !opal_cr_is_enabled ) {
        opal_output_verbose(10, opal_crs_base_output,
                            "crs:open: FT is not enabled, skipping!");
        return OPAL_SUCCESS;
    }

    /* Open up all available components */
    if (OPAL_SUCCESS != (ret = mca_base_components_open("crs", 
                                                        opal_crs_base_output, 
                                                        mca_crs_base_static_components,
                                                        &opal_crs_base_components_available,
                                                        true)) ) {
        exit_status = OPAL_ERROR;
        if( OPAL_ERR_NOT_FOUND == ret) {
            const char **str_value = NULL;

            ret = mca_base_var_find("opal", "crs", NULL, NULL);
            mca_base_var_get_value(ret, &str_value, NULL, NULL);
            if (NULL != str_value && NULL != str_value[0] &&
                0 == strncmp(str_value[0], "none", strlen("none"))) {
                exit_status = OPAL_SUCCESS;
            }
        }
    }

    return exit_status;
}
