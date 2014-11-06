/* Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_DSTORE_SM_H
#define OPAL_DSTORE_SM_H

#include "opal/mca/dstore/dstore.h"
#include "opal/mca/shmem/shmem_types.h"

BEGIN_C_DECLS

OPAL_MODULE_DECLSPEC extern opal_dstore_base_component_t mca_dstore_sm_component;

typedef struct {
    opal_shmem_ds_t *seg_ds;
    char* addr;
} segment_info;

typedef struct {
    opal_list_item_t super;
    uint32_t jobid;
    opal_shmem_ds_t seg_ds;
    uint8_t *addr;
} opal_sm_tracker_t;
OBJ_CLASS_DECLARATION(opal_sm_tracker_t);

typedef struct {
    opal_dstore_base_module_t api;
    opal_list_t               tracklist;
} mca_dstore_sm_module_t;
OPAL_MODULE_DECLSPEC extern mca_dstore_sm_module_t opal_dstore_sm_module;

typedef struct {
    pid_t seg_cpid;
    int seg_id;
    size_t seg_size;
    char file_name[256];
} seg_info_short;


END_C_DECLS

#endif /* OPAL_DSTORE_SM_H */
