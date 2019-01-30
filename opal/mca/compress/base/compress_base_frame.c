/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/mca/base/base.h"
#include "opal/mca/compress/base/base.h"

#include "opal/mca/compress/base/static-components.h"

/*
 * Globals
 */
static bool compress_block(uint8_t *inbytes,
                           size_t inlen,
                           uint8_t **outbytes,
                           size_t *olen)
{
    return false;
}

static bool decompress_block(uint8_t **outbytes, size_t olen,
                             uint8_t *inbytes, size_t len)
{
    return false;
}

opal_compress_base_module_t opal_compress = {
    NULL, /* init             */
    NULL, /* finalize         */
    NULL, /* compress         */
    NULL, /* compress_nb      */
    NULL, /* decompress       */
    NULL,  /* decompress_nb    */
    compress_block,
    decompress_block
};
opal_compress_base_t opal_compress_base = {0};

opal_compress_base_component_t opal_compress_base_selected_component = {{0}};

static int opal_compress_base_register(mca_base_register_flag_t flags);

MCA_BASE_FRAMEWORK_DECLARE(opal, compress, "COMPRESS MCA",
                           opal_compress_base_register, opal_compress_base_open,
                           opal_compress_base_close, mca_compress_base_static_components, 0);

static int opal_compress_base_register(mca_base_register_flag_t flags)
{
    opal_compress_base.compress_limit = 4096;
    (void) mca_base_var_register("opal", "compress", "base", "limit",
                                 "Threshold beyond which data will be compressed",
                                 MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0, OPAL_INFO_LVL_3,
                                 MCA_BASE_VAR_SCOPE_READONLY, &opal_compress_base.compress_limit);

    return OPAL_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int opal_compress_base_open(mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return mca_base_framework_components_open(&opal_compress_base_framework, flags);
}

int opal_compress_base_close(void)
{
    /* Call the component's finalize routine */
    if( NULL != opal_compress.finalize ) {
        opal_compress.finalize();
    }

    /* Close all available modules that are open */
    return mca_base_framework_components_close (&opal_compress_base_framework, NULL);
}
