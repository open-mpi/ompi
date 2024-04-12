/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_NATIVE_H
#define PMIX_NATIVE_H

#include "src/include/pmix_config.h"

#include "src/mca/psec/psec.h"

BEGIN_C_DECLS

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_psec_base_component_t pmix_mca_psec_native_component;
extern pmix_psec_module_t pmix_native_module;

END_C_DECLS

#endif
