/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_SHM_SYSV_H_INCLUDED
#define MPL_SHM_SYSV_H_INCLUDED

#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/shm.h>

typedef int MPLI_shm_lhnd_t;

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

#define MPL_SHM_MAX_INT_STR_LEN 20
#define MPL_SHM_SEG_KEY_LEN     MPL_SHM_MAX_INT_STR_LEN
#define MPLI_SHM_GHND_SZ        MPL_SHM_SEG_KEY_LEN
#define MPLI_SHM_LHND_INVALID   (-1)
#define MPLI_SHM_LHND_INIT_VAL  (-1)
#define MPL_SHM_SER_HND_SZ      MPLI_SHM_GHND_SZ

#define MPL_SHM_SEG_ALREADY_EXISTS EEXIST

/* Nothing to be done at close */
#define MPLI_shm_lhnd_close(hnd)    0

#endif /* MPL_SHM_SYSV_H_INCLUDED */
