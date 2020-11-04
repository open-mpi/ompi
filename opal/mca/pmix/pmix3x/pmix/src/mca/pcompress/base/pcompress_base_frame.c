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

#include "pmix_config.h"

#include "src/mca/base/base.h"
#include "src/mca/pcompress/base/base.h"

#include "src/mca/pcompress/base/static-components.h"

/*
 * Globals
 */
static bool compress_block(char *instring,
                           uint8_t **outbytes,
                           size_t *nbytes)
{
    (void)instring;
    (void)outbytes;
    (void)nbytes;
    return false;
}

static bool decompress_block(char **outstring,
                             uint8_t *inbytes, size_t len)
{
    (void)outstring;
    (void)inbytes;
    (void)len;
    return false;
}

pmix_compress_base_module_t pmix_compress = {
    NULL, /* init             */
    NULL, /* finalize         */
    NULL, /* compress         */
    NULL, /* compress_nb      */
    NULL, /* decompress       */
    NULL,  /* decompress_nb    */
    compress_block,
    decompress_block
};
pmix_compress_base_t pmix_compress_base = {0};

pmix_compress_base_component_t pmix_compress_base_selected_component = {{0}};

static int pmix_compress_base_register(pmix_mca_base_register_flag_t flags)
{
    (void)flags;
    pmix_compress_base.compress_limit = 4096;
    (void) pmix_mca_base_var_register("pmix", "compress", "base", "limit",
                                      "Threshold beyond which data will be compressed",
                                      PMIX_MCA_BASE_VAR_TYPE_SIZE_T, NULL, 0, 0, PMIX_INFO_LVL_3,
                                      PMIX_MCA_BASE_VAR_SCOPE_READONLY, &pmix_compress_base.compress_limit);

    return PMIX_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int pmix_compress_base_open(pmix_mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_pcompress_base_framework, flags);
}

static int pmix_compress_base_close(void)
{
    pmix_compress_base.selected = false;
    /* Call the component's finalize routine */
    if( NULL != pmix_compress.finalize ) {
        pmix_compress.finalize();
    }

    /* Close all available modules that are open */
    return pmix_mca_base_framework_components_close (&pmix_pcompress_base_framework, NULL);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, pcompress, "PCOMPRESS MCA",
                                pmix_compress_base_register, pmix_compress_base_open,
                                pmix_compress_base_close, mca_pcompress_base_static_components, 0);

