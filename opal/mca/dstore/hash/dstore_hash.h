/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_DSTORE_HASH_H
#define OPAL_DSTORE_HASH_H

#include "opal/class/opal_hash_table.h"
#include "opal/mca/dstore/dstore.h"

BEGIN_C_DECLS


OPAL_MODULE_DECLSPEC extern opal_dstore_base_component_t mca_dstore_hash_component;

typedef struct {
    opal_dstore_base_module_t api;
    opal_proc_table_t ptable;
} mca_dstore_hash_module_t;
OPAL_MODULE_DECLSPEC extern mca_dstore_hash_module_t opal_dstore_hash_module;

END_C_DECLS

#endif /* OPAL_DSTORE_HASH_H */
