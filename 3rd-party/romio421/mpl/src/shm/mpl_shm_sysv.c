/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#ifdef MPL_USE_SYSV_SHM

#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/shm.h>

/* A template function which creates/attaches shm seg handle
 * to the shared memory. Used by user-exposed functions below
 */
static inline int MPL_shm_seg_create_attach_templ(MPL_shm_hnd_t hnd, intptr_t seg_sz,
                                                  void **shm_addr_ptr, int offset, int flag)
{
    int rc = MPL_SUCCESS;
    int lhnd = -1;

    if (flag & MPLI_SHM_FLAG_SHM_CREATE) {
        lhnd = shmget(IPC_PRIVATE, seg_sz, IPC_CREAT | S_IRWXU);
        MPLI_shm_lhnd_set(hnd, lhnd);
        rc = MPLI_shm_ghnd_alloc(hnd, MPL_MEM_SHM);
        if (rc) {
            goto fn_exit;
        }
        rc = MPLI_shm_ghnd_set_by_val(hnd, "%d", lhnd);
        if (rc != MPL_SUCCESS) {
            goto fn_exit;
        }
    } else {
        /* Open an existing shared memory seg */
        if (!MPLI_shm_lhnd_is_valid(hnd)) {
            lhnd = atoi(MPLI_shm_ghnd_get_by_ref(hnd));
            MPLI_shm_lhnd_set(hnd, lhnd);
        }
    }

    if (flag & MPLI_SHM_FLAG_SHM_ATTACH) {
        const void *start_addr = NULL;

        /* Caller ensures that shmaddr must be a page-aligned address
         * at which the attach occurs. EINVAL error would result if a
         * mapping already exists in this address range or the address
         * is not page-aligned. */
        if (flag & MPLI_SHM_FLAG_FIXED_ADDR)
            start_addr = (const void *) *shm_addr_ptr;

        /* Attach to shared mem seg */
        *shm_addr_ptr = shmat(MPLI_shm_lhnd_get(hnd), start_addr, 0x0);
        if (*shm_addr_ptr == (void *) -1) {
            rc = MPL_ERR_SHM_INVAL;
        }
    }

  fn_exit:
    return rc;
}

/* Create new SHM segment
 * hnd : A "init"ed shared memory handle
 * seg_sz : Size of shared memory segment to be created
 */
int MPL_shm_seg_create(MPL_shm_hnd_t hnd, intptr_t seg_sz)
{
    return MPL_shm_seg_create_attach_templ(hnd, seg_sz, NULL, 0, MPLI_SHM_FLAG_SHM_CREATE);
}

/* Open an existing SHM segment
 * hnd : A shm handle with a valid global handle
 * seg_sz : Size of shared memory segment to open
 * Currently only using internally within wrapper funcs
 */
int MPL_shm_seg_open(MPL_shm_hnd_t hnd, intptr_t seg_sz)
{
    return MPL_shm_seg_create_attach_templ(hnd, seg_sz, NULL, 0, MPLI_SHM_FLAG_CLR);
}

/* Create new SHM segment and attach to it
 * hnd : A "init"ed shared mem handle
 * seg_sz: Size of shared mem segment
 * shm_addr_ptr : Pointer to shared memory address to attach
 *                  the shared mem segment
 * offset : Offset to attach the shared memory address to
 */
int MPL_shm_seg_create_and_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz,
                                  void **shm_addr_ptr, int offset)
{
    return MPL_shm_seg_create_attach_templ(hnd, seg_sz, shm_addr_ptr, offset,
                                           MPLI_SHM_FLAG_SHM_CREATE | MPLI_SHM_FLAG_SHM_ATTACH);
}

/* Attach to an existing SHM segment
 * hnd : A "init"ed shared mem handle
 * seg_sz: Size of shared mem segment
 * shm_addr_ptr : Pointer to shared memory address to attach
 *                  the shared mem segment
 * offset : Offset to attach the shared memory address to
 */
int MPL_shm_seg_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz, void **shm_addr_ptr, int offset)
{
    return MPL_shm_seg_create_attach_templ(hnd, seg_sz, shm_addr_ptr, offset,
                                           MPLI_SHM_FLAG_SHM_ATTACH);
}

/* Create new SHM segment and attach to it with specified starting address
 * hnd : A "init"ed shared mem handle
 * seg_sz: Size of shared mem segment
 * shm_addr_ptr (inout): Pointer to specified starting address, the address cannot be NULL.
 *                       The actual attached memory address is updated at return.
 * offset : Offset to attach the shared memory address to
 */
int MPL_shm_fixed_seg_create_and_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz,
                                        void **shm_addr_ptr, int offset)
{
    return MPL_shm_seg_create_attach_templ(hnd, seg_sz, shm_addr_ptr, offset,
                                           MPLI_SHM_FLAG_SHM_CREATE | MPLI_SHM_FLAG_SHM_ATTACH |
                                           MPLI_SHM_FLAG_FIXED_ADDR);
}

/* Attach to an existing SHM segment with specified starting address
 * hnd : A "init"ed shared mem handle
 * seg_sz: Size of shared mem segment
 * shm_addr_ptr (inout): Pointer to specified starting address, the address cannot be NULL.
 *                       The actual attached memory address is updated at return.
 * offset : Offset to attach the shared memory address to
 */
int MPL_shm_fixed_seg_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz, void **shm_addr_ptr, int offset)
{
    return MPL_shm_seg_create_attach_templ(hnd, seg_sz, shm_addr_ptr, offset,
                                           MPLI_SHM_FLAG_SHM_ATTACH | MPLI_SHM_FLAG_FIXED_ADDR);
}

/* Detach from an attached SHM segment
 * hnd : Handle to the shm segment
 * shm_addr_ptr : Pointer to the shm address to detach
 * seg_sz : Size of shm segment
 */
int MPL_shm_seg_detach(MPL_shm_hnd_t hnd, void **shm_addr_ptr, intptr_t seg_sz)
{
    int rc = -1;

    rc = shmdt(*shm_addr_ptr);
    *shm_addr_ptr = NULL;

    return (rc == 0) ? MPL_SUCCESS : MPL_ERR_SHM_INTERN;
}

/* Remove a shared memory segment
 * hnd : Handle to the shared memory segment to be removed
 */
int MPL_shm_seg_remove(MPL_shm_hnd_t hnd)
{
    struct shmid_ds ds;
    int rc = -1;

    rc = shmctl(MPLI_shm_lhnd_get(hnd), IPC_RMID, &ds);

    return (rc == 0) ? MPL_SUCCESS : MPL_ERR_SHM_INTERN;
}

#endif /* MPL_USE_SYSV_SHM */
