/*
 * Copyright (c) 2015-2016 Intel, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_MUNGE_H
#define PMIX_MUNGE_H

BEGIN_C_DECLS

#include <src/include/pmix_config.h>


/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_psec_base_component_t mca_psec_munge_component;
extern pmix_psec_module_t pmix_munge_module;

END_C_DECLS

#endif
