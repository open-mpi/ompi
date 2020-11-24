/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* vim: set ft=c.mpich : */
/*
 *  (C) 2016 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#ifdef MPL_USE_NT_SHM

#include <winsock2.h>
#include <windows.h>

/* A template function which creates/attaches shm seg handle
 * to the shared memory. Used by user-exposed functions below
 */
#undef FUNCNAME
#define FUNCNAME MPL_shm_seg_create_attach_templ
#undef FCNAME
#define FCNAME MPL_QUOTE(FUNCNAME)
static inline int MPL_shm_seg_create_attach_templ(MPL_shm_hnd_t hnd, intptr_t seg_sz,
                                                  void **shm_addr_ptr, int offset, int flag)
{
    HANDLE lhnd = INVALID_HANDLE_VALUE;
    int rc = MPL_SHM_SUCCESS;
    ULARGE_INTEGER seg_sz_large;
    seg_sz_large.QuadPart = seg_sz;

    if (!MPLI_shm_ghnd_is_valid(hnd)) {
        rc = MPLI_shm_ghnd_set_uniq(hnd);
        if (rc) {
            goto fn_exit;
        }
    }

    if (flag & MPLI_SHM_FLAG_SHM_CREATE) {
        lhnd = CreateFileMapping(INVALID_HANDLE_VALUE, NULL,
                                 PAGE_READWRITE, seg_sz_large.HighPart, seg_sz_large.LowPart,
                                 MPLI_shm_ghnd_get_by_ref(hnd));
        if (lhnd == NULL) {
            rc = MPL_SHM_EINTERN;
            goto fn_exit;
        }
        MPLI_shm_lhnd_set(hnd, lhnd);
    } else {
        if (!MPLI_shm_lhnd_is_valid(hnd)) {
            /* Strangely OpenFileMapping() returns NULL on error! */
            lhnd = OpenFileMapping(FILE_MAP_WRITE, FALSE, MPLI_shm_ghnd_get_by_ref(hnd));
            if (lhnd == NULL) {
                rc = MPL_SHM_EINTERN;
                goto fn_exit;
            }

            MPLI_shm_lhnd_set(hnd, lhnd);
        }
    }

    if (flag & MPLI_SHM_FLAG_SHM_ATTACH) {
        if (flag & MPLI_SHM_FLAG_FIXED_ADDR) {
            void *start_addr = (void *) *shm_addr_ptr;
            /* The start_addr must be a multiple of the system's memory allocation granularity,
             * or the function fails. To determine the memory allocation granularity of the system,
             * use the GetSystemInfo function. If there is not enough address space at the
             * specified address, the function fails.
             * If the function fails, the return value is NULL.*/
            *shm_addr_ptr = MapViewOfFileEx(MPLI_shm_lhnd_get(hnd),
                                            FILE_MAP_WRITE, 0, offset, 0, start_addr);
        } else {
            *shm_addr_ptr = MapViewOfFile(MPLI_shm_lhnd_get(hnd), FILE_MAP_WRITE, 0, offset, 0);
        }
        if (*shm_addr_ptr == NULL) {
            rc = MPL_SHM_EINVAL;
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
                                           MPLI_SHM_FLAG_FIXED_ADDR, MPL_MEM_SHM);
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

/* Detach from an attached SHM segment */
#undef FUNCNAME
#define FUNCNAME MPL_shm_seg_detach
#undef FCNAME
#define FCNAME MPL_QUOTE(FUNCNAME)
static inline int MPL_shm_seg_detach(MPL_shm_hnd_t hnd, void **shm_addr_ptr, intptr_t seg_sz)
{
    int rc = -1;

    rc = UnmapViewOfFile(*shm_addr_ptr);
    *shm_addr_ptr = NULL;

    /* If the function succeeds, the return value is nonzero,
     * otherwise the return value is zero. */
    return (rc != 0) ? MPL_SHM_SUCCESS : MPL_SHM_EINTERN;
}


#endif /* MPL_USE_NT_SHM */
