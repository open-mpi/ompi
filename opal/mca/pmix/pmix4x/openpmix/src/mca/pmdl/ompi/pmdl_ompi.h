/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_PMDL_ompi_H
#define PMIX_PMDL_ompi_H

#include <src/include/pmix_config.h>


#include "src/mca/pmdl/pmdl.h"

BEGIN_C_DECLS

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_pmdl_base_component_t mca_pmdl_ompi_component;
extern pmix_pmdl_module_t pmix_pmdl_ompi_module;

END_C_DECLS

#endif
