/*
 * Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_DS12_H
#define PMIX_DS12_H

#include <src/include/pmix_config.h>


#include "src/mca/gds/gds.h"
#include "src/mca/pshmem/pshmem.h"

BEGIN_C_DECLS

#include <src/include/pmix_config.h>
#include "src/class/pmix_value_array.h"

#define INITIAL_SEG_SIZE 4096
#define NS_META_SEG_SIZE (1<<22)
#define NS_DATA_SEG_SIZE (1<<22)

#define PMIX_DSTORE_ESH_BASE_PATH "PMIX_DSTORE_ESH_BASE_PATH"

#define ESH_REGION_EXTENSION        "EXTENSION_SLOT"
#define ESH_REGION_INVALIDATED      "INVALIDATED"
#define ESH_ENV_INITIAL_SEG_SIZE    "INITIAL_SEG_SIZE"
#define ESH_ENV_NS_META_SEG_SIZE    "NS_META_SEG_SIZE"
#define ESH_ENV_NS_DATA_SEG_SIZE    "NS_DATA_SEG_SIZE"
#define ESH_ENV_LINEAR              "SM_USE_LINEAR_SEARCH"

#define ESH_MIN_KEY_LEN             (sizeof(ESH_REGION_INVALIDATED))

#ifdef HAVE_PTHREAD_SHARED
#define ESH_PTHREAD_LOCK
#elif defined HAVE_FCNTL_FLOCK
#define ESH_FCNTL_LOCK
#else
#error No locking mechanism was found
#endif

/* this structs are used to store information about
 * shared segments addresses locally at each process,
 * so they are common for different types of segments
 * and don't have a specific content (namespace's info,
 * rank's meta info, ranks's data). */

typedef enum {
    INITIAL_SEGMENT,
    NS_META_SEGMENT,
    NS_DATA_SEGMENT
} segment_type;

typedef struct seg_desc_t seg_desc_t;
struct seg_desc_t {
    segment_type type;
    pmix_pshmem_seg_t seg_info;
    uint32_t id;
    seg_desc_t *next;
};

typedef struct ns_map_data_s ns_map_data_t;
typedef struct session_s session_t;
typedef struct ns_map_s ns_map_t;
typedef struct dstore_mod_s dstore_mod_t;

struct session_s {
    int in_use;
    uid_t jobuid;
    char setjobuid;
    char *nspace_path;
    char *lockfile;
#ifdef ESH_PTHREAD_LOCK
    pmix_pshmem_seg_t *rwlock_seg;
    pthread_rwlock_t *rwlock;
#endif
    int lockfd;
    seg_desc_t *sm_seg_first;
    seg_desc_t *sm_seg_last;
    dstore_mod_t *dstor;
};

struct ns_map_data_s {
    char name[PMIX_MAX_NSLEN+1];
    size_t tbl_idx;
    int track_idx;
};

struct ns_map_s {
    int in_use;
    ns_map_data_t data;
};

/* initial segment format:
 * size_t num_elems;
 * size_t full; //indicate to client that it needs to attach to the next segment
 * ns_seg_info_t ns_seg_info[max_ns_num];
 */

typedef struct {
    ns_map_data_t ns_map;
    size_t num_meta_seg;/* read by clients to attach to this number of segments. */
    size_t num_data_seg;
} ns_seg_info_t;

/* meta segment format:
 * size_t num_elems;
 * rank_meta_info meta_info[max_meta_elems];
 */

typedef struct {
    size_t rank;
    size_t offset;
    size_t count;
} rank_meta_info;

typedef struct {
    pmix_value_array_t super;
    ns_map_data_t ns_map;
    size_t num_meta_seg;
    size_t num_data_seg;
    seg_desc_t *meta_seg;
    seg_desc_t *data_seg;
    bool in_use;
} ns_track_elem_t;

/* functions of dstore operations */
typedef size_t (*pmix_gds_ds_base_kv_size_fn_t)(uint8_t *addr);
typedef size_t (*pmix_gds_ds_base_key_size_fn_t)(const char *key, size_t data_size);
typedef char* (*pmix_gds_ds_base_key_name_ptr_fn_t)(uint8_t *addr);
typedef size_t (*pmix_gds_ds_base_key_len_fn_t)(const char *key);
typedef uint8_t* (*pmix_gds_ds_base_data_ptr_fn_t)(uint8_t *addr);
typedef size_t (*pmix_gds_ds_base_data_size_fn_t)(uint8_t *addr, uint8_t *data_ptr);
typedef size_t (*pmix_gds_ds_base_slot_size_fn_t)(void);
typedef void (*pmix_gds_ds_base_put_key_fn_t)(uint8_t *data, char *key, void *buf, size_t size);

/* structure for dstore operations */
struct dstore_mod_s {
    const char                         *name;
    pmix_gds_ds_base_kv_size_fn_t      kv_size;
    pmix_gds_ds_base_key_size_fn_t     key_size;
    pmix_gds_ds_base_key_name_ptr_fn_t key_ptr;
    pmix_gds_ds_base_key_len_fn_t      key_len;
    pmix_gds_ds_base_data_ptr_fn_t     data_ptr;
    pmix_gds_ds_base_data_size_fn_t    data_size;
    pmix_gds_ds_base_slot_size_fn_t    slot_size;
    pmix_gds_ds_base_put_key_fn_t      put_key;
};

extern dstore_mod_t dstore_v12_module;
extern dstore_mod_t dstore_v20_module;

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_gds_base_component_t mca_gds_ds12_component;
extern pmix_gds_base_module_t pmix_ds12_module;

END_C_DECLS

#endif
