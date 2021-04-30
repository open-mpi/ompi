/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/* This file contains "pre" definitions and declarations for the OS wrappers.
 * That is, things that shouldn't depend on much more than the mpichconf.h
 * values. */

#ifndef MPL_SHM_H_INCLUDED
#define MPL_SHM_H_INCLUDED

#include "mplconfig.h"

#ifdef MPL_USE_SYSV_SHM
#include "mpl_shm_sysv.h"
#elif defined MPL_USE_MMAP_SHM
#include "mpl_shm_mmap.h"
#elif defined MPL_USE_NT_SHM
#include "mpl_shm_win.h"
#endif

#define MPLI_SHM_FLAG_CLR         0x0
#define MPLI_SHM_FLAG_SHM_CREATE  0x1
#define MPLI_SHM_FLAG_SHM_ATTACH  0x10
#define MPLI_SHM_FLAG_GHND_STATIC 0x100
#define MPLI_SHM_FLAG_FIXED_ADDR  0x1000

#define MPL_SHM_HND_INVALID    NULL
#define MPLI_SHM_GHND_INVALID  NULL
#define MPLI_SHM_GHND_INIT_VAL '\0'
#define MPL_SHM_HND_SZ         (sizeof(MPLI_shm_lghnd_t))
#define MPL_SHM_GHND_SZ        MPLI_SHM_GHND_SZ

/* A Handle is valid if it is initialized/init and has a value
 * different from the default/invalid value assigned during init
 */
#define MPLI_shm_hnd_is_valid(hnd) (\
    ((hnd) &&                                                       \
        MPLI_shm_lhnd_is_valid(hnd) &&                             \
        MPLI_shm_ghnd_is_valid(hnd))                               \
)

/* With MMAP_SHM, NT_SHM & SYSV_SHM local handle is always init'ed */
#define MPLI_shm_hnd_is_init(hnd) (\
    ((hnd) && /* MPL_shm_lhnd_is_init(hnd) && */                  \
        MPLI_shm_ghnd_is_init(hnd))                                \
)

/* These macros are the setters/getters for the shm handle */
#define MPLI_shm_lhnd_get(hnd)      ((hnd)->lhnd)
#define MPLI_shm_lhnd_set(hnd, val) ((hnd)->lhnd=val)
#define MPLI_shm_lhnd_is_valid(hnd) (((hnd)->lhnd != MPLI_SHM_LHND_INVALID))
#define MPLI_shm_lhnd_is_init(hnd)  1

/* Allocate mem for references within the handle */
/* Returns MPL_SUCCESS on success, MPL_ERR_SHM_NOMEM on error */
#define MPL_shm_hnd_ref_alloc(hnd)(\
    ((hnd)->ghnd = (MPLI_shm_ghnd_t)                               \
                    MPL_malloc(MPLI_SHM_GHND_SZ, MPL_MEM_SHM)) ? MPL_SUCCESS : MPL_ERR_SHM_NOMEM \
)


/* These macros are the setters/getters for the shm handle */
#define MPLI_shm_ghnd_get_by_ref(hnd)   ((hnd)->ghnd)

/* Returns -1 on error, 0 on success */
#define MPLI_shm_ghnd_get_by_val(hnd, str, strlen)  (\
    (MPL_snprintf(str, strlen, "%s",                                       \
        MPLI_shm_ghnd_get_by_ref(hnd))) ? MPL_SUCCESS : MPL_ERR_SHM_INTERN \
)
#define MPLI_shm_ghnd_set_by_ref(hnd, val) ((hnd)->ghnd = val)
/* Returns -1 on error, 0 on success */
/* FIXME: What if val is a non-null terminated string ? */
#define MPLI_shm_ghnd_set_by_val(hnd, fmt, val) (\
    (MPL_snprintf(MPLI_shm_ghnd_get_by_ref(hnd),                         \
        MPLI_SHM_GHND_SZ, fmt, val)) ? MPL_SUCCESS : MPL_ERR_SHM_INTERN  \
)

#define MPLI_shm_ghnd_is_valid(hnd) (\
    (((hnd)->ghnd == MPLI_SHM_GHND_INVALID) ||                     \
        (strlen((hnd)->ghnd) == 0)) ? 0 : 1                         \
)
#define MPLI_shm_ghnd_is_init(hnd) (\
    ((hnd)->flag & MPLI_SHM_FLAG_GHND_STATIC) ?                    \
    1 :                                                             \
    (((hnd)->ghnd != MPLI_SHM_GHND_INVALID) ? 1 : 0)               \
)

/* Allocate mem for global handle.
 * Returns 0 on success, -1 on failure
 */
static inline int MPLI_shm_ghnd_alloc(MPL_shm_hnd_t hnd, MPL_memory_class class)
{
    if (!(hnd->ghnd)) {
        hnd->ghnd = (MPLI_shm_ghnd_t) MPL_malloc(MPLI_SHM_GHND_SZ, class);
        if (!(hnd->ghnd)) {
            return MPL_ERR_SHM_NOMEM;
        }
    }
    /* Global handle is no longer static */
    hnd->flag &= ~MPLI_SHM_FLAG_GHND_STATIC;
    return MPL_SUCCESS;
}


/* Allocate mem for handle. Lazy allocation for global handle */
/* Returns 0 on success, -1 on error */
static inline int MPLI_shm_hnd_alloc(MPL_shm_hnd_t * hnd_ptr, MPL_memory_class class)
{
    *hnd_ptr = (MPL_shm_hnd_t) MPL_malloc(MPL_SHM_HND_SZ, class);
    if (*hnd_ptr) {
        (*hnd_ptr)->flag = MPLI_SHM_FLAG_GHND_STATIC;
    } else {
        return MPL_ERR_SHM_NOMEM;
    }
    return MPL_SUCCESS;
}

/* Close Handle */
#define MPLI_shm_hnd_close(hnd) MPLI_shm_lhnd_close(hnd)

static inline void MPLI_shm_hnd_reset_val(MPL_shm_hnd_t hnd)
{
    MPLI_shm_lhnd_set(hnd, MPLI_SHM_LHND_INIT_VAL);
    if (hnd->flag & MPLI_SHM_FLAG_GHND_STATIC) {
        hnd->ghnd = MPLI_SHM_GHND_INVALID;
    } else {
        (hnd->ghnd)[0] = MPLI_SHM_GHND_INIT_VAL;
    }
}

static inline void MPLI_shm_hnd_free(MPL_shm_hnd_t hnd)
{
    if (MPLI_shm_hnd_is_init(hnd)) {
        if (!(hnd->flag & MPLI_SHM_FLAG_GHND_STATIC)) {
            MPL_free(hnd->ghnd);
        }
        MPL_free(hnd);
    }
}

/* interfaces */
int MPL_shm_hnd_serialize(char *str, MPL_shm_hnd_t hnd, int str_len);
int MPL_shm_hnd_deserialize(MPL_shm_hnd_t hnd, const char *str_hnd, size_t str_hnd_len);
int MPL_shm_hnd_get_serialized_by_ref(MPL_shm_hnd_t hnd, char **str_ptr);
int MPL_shm_hnd_deserialize_by_ref(MPL_shm_hnd_t hnd, char **ser_hnd_ptr);
int MPL_shm_hnd_init(MPL_shm_hnd_t * hnd_ptr);
int MPL_shm_hnd_finalize(MPL_shm_hnd_t * hnd_ptr);
int MPL_shm_seg_create(MPL_shm_hnd_t hnd, intptr_t seg_sz);
int MPL_shm_seg_open(MPL_shm_hnd_t hnd, intptr_t seg_sz);
int MPL_shm_seg_create_and_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz,
                                  void **shm_addr_ptr, int offset);
int MPL_shm_seg_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz, void **shm_addr_ptr, int offset);
int MPL_shm_fixed_seg_create_and_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz,
                                        void **shm_addr_ptr, int offset);
int MPL_shm_fixed_seg_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz, void **shm_addr_ptr, int offset);
int MPL_shm_seg_detach(MPL_shm_hnd_t hnd, void **shm_addr_ptr, intptr_t seg_sz);
int MPL_shm_seg_remove(MPL_shm_hnd_t hnd);

#endif /* MPL_SHM_H_INCLUDED */
