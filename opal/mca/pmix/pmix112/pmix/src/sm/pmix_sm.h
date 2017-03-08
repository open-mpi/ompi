/*
 * Copyright (c) 2015-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_SM_H
#define PMIX_SM_H

#include <src/include/pmix_config.h>

#include <pmix/pmix_common.h>
#include <src/include/pmix_config.h>

BEGIN_C_DECLS

#if !defined(MAP_FAILED)
#    define MAP_FAILED ((char*)-1)
#endif /* MAP_FAILED */

#define PMIX_SHMEM_DS_ID_INVALID -1

typedef enum {
    PMIX_SM_RONLY,
    PMIX_SM_RW
} pmix_sm_access_mode_t;

typedef struct pmix_sm_seg_t {
    /* pid of the shared memory segment creator */
    pid_t seg_cpid;
    /* ds id */
    int seg_id;
    /* size of shared memory segment */
    size_t seg_size;
    /* base address of shared memory segment */
    unsigned char *seg_base_addr;
    char seg_name[PMIX_PATH_MAX];
} pmix_sm_seg_t;

int pmix_sm_init(void);
void pmix_sm_finalize(void);
int pmix_sm_segment_create(pmix_sm_seg_t *sm_seg, const char *file_name, size_t size);
int pmix_sm_segment_attach(pmix_sm_seg_t *sm_seg, pmix_sm_access_mode_t sm_mode);
int pmix_sm_segment_detach(pmix_sm_seg_t *sm_seg);
int pmix_sm_segment_unlink(pmix_sm_seg_t *sm_seg);

static inline void _segment_ds_reset(pmix_sm_seg_t *sm_seg)
{
    sm_seg->seg_cpid = 0;
    sm_seg->seg_id = PMIX_SHMEM_DS_ID_INVALID;
    sm_seg->seg_size = 0;
    memset(sm_seg->seg_name, '\0', PMIX_PATH_MAX);
    sm_seg->seg_base_addr = (unsigned char *)MAP_FAILED;
}


/**
* create a new shared memory segment and initialize members in structure
* pointed to by sm_seg.
*
* @param sm_seg   pointer to pmix_sm_seg_t structure
*
* @param file_name unique string identifier that must be a valid,
*                 writable path (IN).
*
* @param size     size of the shared memory segment.
*
* @return PMIX_SUCCESS on success.
*/
typedef int (*pmix_sm_base_module_segment_create_fn_t)(pmix_sm_seg_t *sm_seg, const char *file_name, size_t size);

/**
* attach to an existing shared memory segment initialized by segment_create.
*
* @param sm_seg  pointer to initialized pmix_sm_seg_t typedef'd
*                structure (IN/OUT).
*
* @return        base address of shared memory segment on success. returns
*                NULL otherwise.
*/
typedef int (*pmix_sm_base_module_segment_attach_fn_t)(pmix_sm_seg_t *sm_seg, pmix_sm_access_mode_t sm_mode);

/**
* detach from an existing shared memory segment.
*
* @param sm_seg  pointer to initialized pmix_sm_seg_t typedef'd structure
*                (IN/OUT).
*
* @return PMIX_SUCCESS on success.
*/
typedef int (*pmix_sm_base_module_segment_detach_fn_t)(pmix_sm_seg_t *sm_seg);

/**
* unlink an existing shared memory segment.
*
* @param sm_seg  pointer to initialized pmix_sm_seg_t typedef'd structure
*                (IN/OUT).
*
* @return PMIX_SUCCESS on success.
*/
typedef int (*pmix_sm_base_module_unlink_fn_t)(pmix_sm_seg_t *sm_seg);


/**
* structure for sm modules
*/
typedef struct {
    const char *name;
    pmix_sm_base_module_segment_create_fn_t  segment_create;
    pmix_sm_base_module_segment_attach_fn_t  segment_attach;
    pmix_sm_base_module_segment_detach_fn_t  segment_detach;
    pmix_sm_base_module_unlink_fn_t          segment_unlink;
} pmix_sm_base_module_t;


END_C_DECLS

#endif /* PMIX_SM_H */
