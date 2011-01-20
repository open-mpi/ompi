/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/include/opal/constants.h"
#include "opal/mca/compress/compress.h"
#include "opal/mca/compress/base/base.h"
#include "opal/util/output.h"
#include "opal/util/opal_sos.h"

#include "opal/mca/compress/base/static-components.h"

/*
 * Globals
 */
int  opal_compress_base_output  = -1;
opal_compress_base_module_t opal_compress = {
    NULL, /* init             */
    NULL, /* finalize         */
    NULL, /* compress         */
    NULL, /* compress_nb      */
    NULL, /* decompress       */
    NULL  /* decompress_nb    */
};
opal_list_t opal_compress_base_components_available;
opal_compress_base_component_t opal_compress_base_selected_component;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int opal_compress_base_open(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    int value;
    char *str_value = NULL;

    /* Debugging/Verbose output */
    mca_base_param_reg_int_name("compress",
                                "base_verbose",
                                "Verbosity level of the COMPRESS framework",
                                false, false,
                                0, &value);
    if(0 != value) {
        opal_compress_base_output = opal_output_open(NULL);
    } else {
        opal_compress_base_output = -1;
    }
    opal_output_set_verbosity(opal_compress_base_output, value);

    /* 
     * Which COMPRESS component to open
     *  - NULL or "" = auto-select
     *  - "none" = Empty component
     *  - ow. select that specific component
     */
    mca_base_param_reg_string_name("compress", NULL,
                                   "Which COMPRESS component to use (empty = auto-select)",
                                   false, false,
                                   NULL, &str_value);

    /* Compression currently only used with C/R */
    if( !opal_cr_is_enabled ) {
        opal_output_verbose(10, opal_compress_base_output,
                            "compress:open: FT is not enabled, skipping!");
        return OPAL_SUCCESS;
    }

    /* Open up all available components */
    if (OPAL_SUCCESS != (ret = mca_base_components_open("compress", 
                                                        opal_compress_base_output, 
                                                        mca_compress_base_static_components,
                                                        &opal_compress_base_components_available,
                                                        true)) ) {
        if( OPAL_ERR_NOT_FOUND == OPAL_SOS_GET_ERROR_CODE(ret) &&
            NULL != str_value &&
            0 == strncmp(str_value, "none", strlen("none")) ) {
            exit_status = OPAL_SUCCESS;
        } else {
            exit_status = OPAL_ERROR;
        }
    }

    if( NULL != str_value ) {
        free(str_value);
    }
    return exit_status;
}
