/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_PREG_compress_H
#define PMIX_PREG_compress_H

#include <src/include/pmix_config.h>


#include "src/mca/preg/preg.h"

BEGIN_C_DECLS

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_mca_base_component_t mca_preg_compress_component;
extern pmix_preg_module_t pmix_preg_compress_module;

END_C_DECLS

#endif
