/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_UCT_DISCOVER_H)
#define MCA_BTL_UCT_DISCOVER_H

#include "btl_uct.h"
#include "opal/class/opal_list.h"

/**
 * @brief Query UCT for the available memory domains. This list will be limited by 
 */
int mca_btl_uct_component_discover_mds(void);

/**
 * @brief Create BTL modules from the memory domain list.
 *
 * The modules are registered with MCA and must be shut down using
 * mca_btl_module_finalize.
 */
int mca_btl_uct_component_generate_modules(opal_list_t *md_list);

int mca_btl_uct_enable_modules(mca_btl_uct_module_t **modules, int module_count);

/**
 * @brief Scan detected transports and find a connection transport (if needed).
 */
int mca_btl_uct_component_maybe_setup_conn_tl(void);

/**
 * @brief Clean out unused memory domains and transport layers.
 */
int mca_btl_uct_component_filter_mds(void);


#endif  /* !defined(MCA_BTL_UCT_DISCOVER_H) */
