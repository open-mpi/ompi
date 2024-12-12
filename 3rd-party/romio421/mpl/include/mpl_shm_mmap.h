/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_SHM_MMAP_H_INCLUDED
#define MPL_SHM_MMAP_H_INCLUDED

#include <fcntl.h>

#ifdef MPL_HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

typedef intptr_t MPLI_shm_lhnd_t;

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

#define MPL_SHM_FNAME_LEN      50
#define MPLI_SHM_GHND_SZ       MPL_SHM_FNAME_LEN
#define MPLI_SHM_LHND_INVALID  (-1)
#define MPLI_SHM_LHND_INIT_VAL (-1)

#define MPLI_SHM_SEG_ALREADY_EXISTS EEXIST

/* Returns MPL_SUCCESS on success, MPL_ERR_SHM_INTERN on error */
int MPLI_shm_lhnd_close(MPL_shm_hnd_t hnd);

#endif /* MPL_SHM_MMAP_H_INCLUDED */
