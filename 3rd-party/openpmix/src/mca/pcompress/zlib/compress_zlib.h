/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * ZLIB COMPRESS component
 *
 * Uses the zlib library
 */

#ifndef MCA_COMPRESS_ZLIB_EXPORT_H
#define MCA_COMPRESS_ZLIB_EXPORT_H

#include "pmix_config.h"

#include "src/util/pmix_output.h"

#include "src/mca/mca.h"
#include "src/mca/pcompress/pcompress.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_mca_base_component_t pmix_mca_pcompress_zlib_component;
extern pmix_compress_base_module_t pmix_pcompress_zlib_module;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_COMPRESS_ZLIB_EXPORT_H */
