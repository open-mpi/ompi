/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_H
#define PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_H

#include "src/include/pmix_config.h"

#include "pmix_common.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/mca/pinstalldirs/pinstalldirs_types.h"

BEGIN_C_DECLS
/**
 * Expand out path variables (such as ${prefix}) in the input string
 * using the current pmix_pinstall_dirs structure */
PMIX_EXPORT char *pmix_pinstall_dirs_expand(const char *input);

/* optional initialization function */
typedef void (*pmix_install_dirs_init_fn_t)(pmix_info_t info[], size_t ninfo);

/**
 * Structure for pinstalldirs components.
 */
struct pmix_pinstalldirs_base_component_2_0_0_t {
    /** MCA base component */
    pmix_mca_base_component_t component;
    /** install directories provided by the given component */
    pmix_pinstall_dirs_t install_dirs_data;
    /* optional init function */
    pmix_install_dirs_init_fn_t init;
};
/**
 * Convenience typedef
 */
typedef struct pmix_pinstalldirs_base_component_2_0_0_t pmix_pinstalldirs_base_component_t;

/*
 * Macro for use in components that are of type pinstalldirs
 */
#define PMIX_PINSTALLDIRS_BASE_VERSION_1_0_0 PMIX_MCA_BASE_VERSION_1_0_0("pinstalldirs", 1, 0, 0)

END_C_DECLS

#endif /* PMIX_MCA_PINSTALLDIRS_PINSTALLDIRS_H */
