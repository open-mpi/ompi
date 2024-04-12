/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_DL_BASE_H
#define PRTE_DL_BASE_H

#include "prte_config.h"
#include "src/mca/prtedl/prtedl.h"
#include "src/util/pmix_environ.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_framework.h"

BEGIN_C_DECLS

/**
 * Globals
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_prtedl_base_framework;
PRTE_EXPORT extern prte_prtedl_base_component_t *prte_prtedl_base_selected_component;
PRTE_EXPORT extern prte_prtedl_base_module_t *prte_prtedl;

/**
 * Initialize the DL MCA framework
 *
 * @retval PRTE_SUCCESS Upon success
 * @retval PRTE_ERROR   Upon failures
 *
 * This function is invoked during prte_init();
 */
PRTE_EXPORT int prte_dl_base_open(pmix_mca_base_open_flag_t flags);

/**
 * Select an available component.
 *
 * @retval PRTE_SUCCESS Upon Success
 * @retval PRTE_NOT_FOUND If no component can be selected
 * @retval PRTE_ERROR Upon other failure
 *
 */
PRTE_EXPORT int prte_dl_base_select(void);

/**
 * Finalize the DL MCA framework
 *
 * @retval PRTE_SUCCESS Upon success
 * @retval PRTE_ERROR   Upon failures
 *
 * This function is invoked during prte_finalize();
 */
PRTE_EXPORT int prte_dl_base_close(void);

/**
 * Open a DSO
 *
 * (see prte_prtedl_base_module_open_ft_t in src/mca/prtedl/prtedl.h for
 * documentation of this function)
 */
PRTE_EXPORT int prte_dl_open(const char *fname, bool use_ext, bool private_namespace,
                             prte_dl_handle_t **handle, char **err_msg);

/**
 * Lookup a symbol in a DSO
 *
 * (see prte_prtedl_base_module_lookup_ft_t in src/mca/prtedl/prtedl.h for
 * documentation of this function)
 */
PRTE_EXPORT int prte_dl_lookup(prte_dl_handle_t *handle, const char *symbol, void **ptr,
                               char **err_msg);

/**
 * Close a DSO
 *
 * (see prte_prtedl_base_module_close_ft_t in src/mca/prtedl/prtedl.h for
 * documentation of this function)
 */
PRTE_EXPORT int prte_dl_close(prte_dl_handle_t *handle);

/**
 * Iterate over files in a path
 *
 * (see prte_prtedl_base_module_foreachfile_ft_t in src/mca/prtedl/prtedl.h for
 * documentation of this function)
 */
PRTE_EXPORT int prte_dl_foreachfile(const char *search_path,
                                    int (*cb_func)(const char *filename, void *context),
                                    void *context);

END_C_DECLS

#endif /* PRTE_DL_BASE_H */
