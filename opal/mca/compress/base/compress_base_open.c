/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
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

static int opal_compress_base_verbose = 0;

static int opal_compress_base_register(int flags)
{
    /* Debugging/Verbose output */
    opal_compress_base_verbose = 0;
    (void) mca_base_var_register("opal", "compress", "base", "verbose",
                                 "Verbosity level of the COMPRESS framework",
                                 MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &opal_compress_base_verbose);

    return OPAL_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int opal_compress_base_open(void)
{
    int ret, exit_status = OPAL_SUCCESS;

    (void) opal_compress_base_register(0);

    if(0 != opal_compress_base_verbose) {
        opal_compress_base_output = opal_output_open(NULL);
    } else {
        opal_compress_base_output = -1;
    }
    opal_output_set_verbosity(opal_compress_base_output, opal_compress_base_verbose);

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
        exit_status = OPAL_ERROR;
        if( OPAL_ERR_NOT_FOUND == ret) {
            const char **str_value = NULL;

            ret = mca_base_var_find("opal", "compress", NULL, NULL);
            mca_base_var_get_value(ret, &str_value, NULL, NULL);
            if (NULL != str_value && NULL != str_value[0] &&
                0 == strncmp(str_value[0], "none", strlen("none"))) {
                exit_status = OPAL_SUCCESS;
            }
        }
    }

    return exit_status;
}
