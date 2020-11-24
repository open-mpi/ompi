/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* vim: set ft=c.mpich : */
/*
 *  (C) 2016 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#ifndef MPL_SHM_WIN_H_INCLUDED
#define MPL_SHM_WIN_H_INCLUDED

typedef HANDLE MPL_shm_lhnd_t;

typedef char *MPLI_shm_ghnd_t;
/* The local handle, lhnd, is valid only for the current process,
 * The global handle, ghnd, is valid across multiple processes
 * The handle flag, flag, is used to set various attributes of the
 *  handle.
 */
typedef struct MPLI_shm_lghnd_t {
    MPLI_shm_lhnd_t lhnd;
    MPLI_shm_ghnd_t ghnd;
    int flag;
} MPLI_shm_lghnd_t;

typedef MPLI_shm_lghnd_t *MPL_shm_hnd_t;

#define MPL_SHM_SEG_NAME_LEN   70
#define MPLI_SHM_GHND_SZ       MPL_SHM_SEG_NAME_LEN
#define MPLI_SHM_LHND_INVALID  INVALID_HANDLE_VALUE
#define MPLI_SHM_LHND_INIT_VAL INVALID_HANDLE_VALUE

#define MPL_shm_SEG_ALREADY_EXISTS ERROR_ALREADY_EXISTS

/* Returns MPL_SHM_SUCCESS on success, MPL_SHM_EINTERN on error */
#define MPLI_shm_lhnd_close(hnd)(\
    (CloseHandle(MPLI_shm_lhnd_get(hnd)) != 0) ? MPL_SHM_SUCCESS : MPL_SHM_EINTERN  \
)

#if defined (HAVE_QUERYPERFORMANCECOUNTER)
/*
 * Returns size of uniqStr, 0 on error
 */
static inline int MPL_shm_get_uniq_str(char *str, int strlen)
{
    LARGE_INTEGER perfCnt;
    QueryPerformanceCounter(&perfCnt);
    return (MPL_snprintf(str, strlen, "MPICH_NEM_%d_%I64d",
                         GetCurrentThreadId(), (perfCnt.QuadPart)));
}
#endif

/* Returns MPL_SHM_SUCCESS on success, MPL_SHM_EINTERN on error */
static inline int MPLI_shm_ghnd_set_uniq(MPL_shm_hnd_t hnd)
{
    if (MPL_shm_hnd_ref_alloc(hnd) == MPL_SHM_SUCCESS) {
        if (MPLI_shm_get_uniq_str(hnd->ghnd, MPLI_SHM_GHND_SZ) != 0) {
            return MPL_SHM_EINTERN;
        }
    } else {
        return MPL_SHM_EINTERN;
    }
    return MPL_SHM_SUCCESS;
}

/* Nothing to be done when removing an SHM segment */
static inline int MPL_shm_seg_remove(MPL_shm_hnd_t hnd)
{
    return MPL_SHM_SUCCESS;
}

#endif /* MPL_SHM_WIN_H_INCLUDED */
