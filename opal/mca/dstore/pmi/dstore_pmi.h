/*
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_DSTORE_PMI_H
#define OPAL_DSTORE_PMI_H

#include "opal/util/error.h"
#include "opal/mca/dstore/dstore.h"

BEGIN_C_DECLS


OPAL_MODULE_DECLSPEC extern opal_dstore_base_component_t mca_dstore_pmi_component;

typedef struct {
    opal_dstore_base_module_t api;
    char *pmi_kvs_name;
    int pmi_vallen_max;
    int pmi_keylen_max;
    char *pmi_packed_data;
    int pmi_pack_key;
    int pmi_packed_data_off;
    opal_hash_table_t hash_data;
} mca_dstore_pmi_module_t;
OPAL_MODULE_DECLSPEC extern mca_dstore_pmi_module_t opal_dstore_pmi_module;


END_C_DECLS

#endif /* OPAL_DSTORE_PMI_H */
