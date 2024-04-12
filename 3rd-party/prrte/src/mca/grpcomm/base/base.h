/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_GRPCOMM_BASE_H
#define MCA_GRPCOMM_BASE_H

/*
 * includes
 */
#include "prte_config.h"

#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/grpcomm/grpcomm.h"
#include "src/mca/mca.h"
#include "src/mca/odls/odls_types.h"
#include "src/rml/rml_types.h"
#include "src/pmix/pmix-internal.h"

/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

/*
 * MCA framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_grpcomm_base_framework;
/*
 * Select an available component.
 */
PRTE_EXPORT int prte_grpcomm_base_select(void);

/*
 * globals that might be needed
 */
typedef struct {
    pmix_list_item_t super;
    int pri;
    prte_grpcomm_base_module_t *module;
    pmix_mca_base_component_t *component;
} prte_grpcomm_base_active_t;
PMIX_CLASS_DECLARATION(prte_grpcomm_base_active_t);

typedef struct {
    pmix_list_t actives;
    pmix_list_t ongoing;
    pmix_hash_table_t sig_table;
    char *transports;
    uint32_t context_id;
} prte_grpcomm_base_t;

PRTE_EXPORT extern prte_grpcomm_base_t prte_grpcomm_base;

/* Public API stubs */
PRTE_EXPORT int prte_grpcomm_API_xcast(prte_grpcomm_signature_t *sig, prte_rml_tag_t tag,
                                       pmix_data_buffer_t *buf);

PRTE_EXPORT int prte_grpcomm_API_allgather(prte_pmix_mdx_caddy_t *cd);

PRTE_EXPORT prte_grpcomm_coll_t *prte_grpcomm_base_get_tracker(prte_grpcomm_signature_t *sig,
                                                               bool create);

PRTE_EXPORT int prte_pack_ctrl_options(pmix_byte_object_t *bo,
                                       const pmix_info_t *info, size_t ninfo);

END_C_DECLS
#endif
