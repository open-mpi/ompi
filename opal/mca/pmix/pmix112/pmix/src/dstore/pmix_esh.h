/*
 * Copyright (c) 2015-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_DSTORE_ESH_H
#define PMIX_DSTORE_ESH_H

#include <src/include/pmix_config.h>
#include "src/class/pmix_value_array.h"
#include "pmix_dstore.h"
#include "src/sm/pmix_sm.h"

BEGIN_C_DECLS

#define INITIAL_SEG_SIZE 4096
#define NS_META_SEG_SIZE (1<<22)
#define NS_DATA_SEG_SIZE (1<<22)

#define PMIX_DSTORE_ESH_BASE_PATH "PMIX_DSTORE_ESH_BASE_PATH"

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
    pmix_sm_seg_t seg_info;
    uint32_t id;
    seg_desc_t *next;
};

typedef struct ns_map_data_s ns_map_data_t;
typedef struct session_s session_t;
typedef struct ns_map_s ns_map_t;

struct session_s {
    int in_use;
    uid_t jobuid;
    char setjobuid;
    char *nspace_path;
    char *lockfile;
#ifdef ESH_PTHREAD_LOCK
    pmix_sm_seg_t *rwlock_seg;
    pthread_rwlock_t *rwlock;
#endif
    int lockfd;
    seg_desc_t *sm_seg_first;
    seg_desc_t *sm_seg_last;
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
} ns_track_elem_t;

extern pmix_dstore_base_module_t pmix_dstore_esh_module;

END_C_DECLS

#endif /* PMIX_DSTORE_ESH_H */
