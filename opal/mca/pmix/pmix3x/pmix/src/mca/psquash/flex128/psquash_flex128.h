/*
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_NATIVE_H
#define PMIX_NATIVE_H

#include "src/include/pmix_config.h"


#include "src/mca/psquash/psquash.h"

BEGIN_C_DECLS

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_psquash_base_component_t mca_psquash_flex128_component;
extern pmix_psquash_base_module_t pmix_flex128_module;

END_C_DECLS

#endif
