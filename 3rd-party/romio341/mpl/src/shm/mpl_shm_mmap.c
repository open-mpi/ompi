/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpl.h"

MPL_SUPPRESS_OSX_HAS_NO_SYMBOLS_WARNING;

#ifdef MPL_USE_MMAP_SHM

#include <fcntl.h>

#ifdef MPL_HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#if defined (MPL_HAVE_MKSTEMP) && defined (MPL_NEEDS_MKSTEMP_DECL)
extern int mkstemp(char *template);
#endif

inline int MPLI_shm_lhnd_close(MPL_shm_hnd_t hnd)
{
    MPLI_shm_lhnd_t lhnd = MPLI_SHM_LHND_INVALID;
    lhnd = MPLI_shm_lhnd_get(hnd);
    if (lhnd != MPLI_SHM_LHND_INVALID) {
        if (close(lhnd) == 0) {
            MPLI_shm_lhnd_set(hnd, MPLI_SHM_LHND_INIT_VAL);
        } else {
            /* close() failed */
            return MPL_ERR_SHM_INTERN;
        }
    }
    return MPL_SUCCESS;
}

static inline int check_valid_fixed_mmap_range(void *shm_addr, intptr_t seg_sz)
{
    int rc = 0, is_valid = 1;
    size_t page_sz = 0, mapsize = 0, num_pages = 0, i;

    if (shm_addr == NULL)
        return 0;       /* NULL is not a valid maprage */

    page_sz = sysconf(_SC_PAGESIZE);
    mapsize = (seg_sz + (page_sz - 1)) & (~(page_sz - 1));
    num_pages = mapsize / page_sz;

    char *ptr = (char *) shm_addr;
    for (i = 0; i < num_pages; i++) {
        /* return ENOMEM if the page is not mapped */
        rc = msync(ptr, page_sz, 0);
        if (rc != -1 || errno != ENOMEM) {
            is_valid = 0;
            break;
        }
        ptr += page_sz;
    }
    return is_valid;
}

/* A template function which creates/attaches shm seg handle
 * to the shared memory. Used by user-exposed functions below
 */
static inline int MPL_shm_seg_create_attach_templ(MPL_shm_hnd_t hnd, intptr_t seg_sz,
                                                  void **shm_addr_ptr, int offset, int flag)
{
    MPLI_shm_lhnd_t lhnd = -1;
    int rc = MPL_SUCCESS, rc_close = MPL_SUCCESS;

    if (flag & MPLI_SHM_FLAG_SHM_CREATE) {
        char dev_shm_fname[] = "/dev/shm/mpich_shar_tmpXXXXXX";
        char tmp_fname[] = "/tmp/mpich_shar_tmpXXXXXX";
        char *chosen_fname = NULL;

        chosen_fname = dev_shm_fname;
        lhnd = mkstemp(chosen_fname);
        if (lhnd == -1) {
            chosen_fname = tmp_fname;
            lhnd = mkstemp(chosen_fname);
            if (lhnd == -1) {
                rc = MPL_ERR_SHM_INTERN;
                goto fn_fail;
            }
        }

        MPLI_shm_lhnd_set(hnd, lhnd);
        rc = (MPLI_shm_lhnd_t) lseek(lhnd, seg_sz - 1, SEEK_SET);
        do {
            rc = (int) write(lhnd, "", 1);
        } while ((rc == -1) && (errno == EINTR));

        rc = MPLI_shm_ghnd_alloc(hnd, MPL_MEM_SHM);
        if (rc != MPL_SUCCESS) {
            goto fn_fail;
        }
        rc = MPLI_shm_ghnd_set_by_val(hnd, "%s", chosen_fname);
        if (rc != MPL_SUCCESS) {
            goto fn_fail;
        }
    } else {
        /* Open an existing shared memory seg */
        if (!MPLI_shm_lhnd_is_valid(hnd)) {
            lhnd = open(MPLI_shm_ghnd_get_by_ref(hnd), O_RDWR);
            if (lhnd == -1) {
                rc = MPL_ERR_SHM_INTERN;
                goto fn_fail;
            }
            MPLI_shm_lhnd_set(hnd, lhnd);
        }
    }

    if (flag & MPLI_SHM_FLAG_SHM_ATTACH) {
        if (flag & MPLI_SHM_FLAG_FIXED_ADDR) {
            void *start_addr = *shm_addr_ptr;
            /* mmap with MAP_FIXED discards any overlapped part of the existing mapping.
             * Thus, we need manually check if the entire range is valid. */
            if (check_valid_fixed_mmap_range(start_addr, seg_sz)) {
                *shm_addr_ptr = MPL_mmap(start_addr, seg_sz, PROT_READ | PROT_WRITE,
                                         MAP_SHARED | MAP_FIXED, MPLI_shm_lhnd_get(hnd), 0,
                                         MPL_MEM_SHM);
            } else
                rc = MPL_ERR_SHM_INVAL;
        } else {
            *shm_addr_ptr = MPL_mmap(NULL, seg_sz, PROT_READ | PROT_WRITE,
                                     MAP_SHARED, MPLI_shm_lhnd_get(hnd), 0, MPL_MEM_SHM);
        }

        if (*shm_addr_ptr == MAP_FAILED || *shm_addr_ptr == NULL) {
            rc = MPL_ERR_SHM_INVAL;
            goto fn_fail;
        }
    }

  fn_exit:
    /* FIXME: Close local handle only when closing the shm handle */
    if (MPLI_shm_lhnd_is_valid(hnd)) {
        rc_close = MPLI_shm_lhnd_close(hnd);
    }
    return (rc != MPL_SUCCESS) ? rc : rc_close;
  fn_fail:
    goto fn_exit;
}

/* Create new SHM segment
 * hnd : A "init"ed shared memory handle
 * seg_sz : Size of shared memory segment to be created
 */
int MPL_shm_seg_create(MPL_shm_hnd_t hnd, intptr_t seg_sz)
{
    int rc = MPL_SUCCESS;
    rc = MPL_shm_seg_create_attach_templ(hnd, seg_sz, NULL, 0, MPLI_SHM_FLAG_SHM_CREATE);
    return rc;
}

/* Open an existing SHM segment
 * hnd : A shm handle with a valid global handle
 * seg_sz : Size of shared memory segment to open
 * Currently only using internally within wrapper funcs
 */
int MPL_shm_seg_open(MPL_shm_hnd_t hnd, intptr_t seg_sz)
{
    int rc = MPL_SUCCESS;
    rc = MPL_shm_seg_create_attach_templ(hnd, seg_sz, NULL, 0, MPLI_SHM_FLAG_CLR);
    return rc;
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
 *                       The attached memory address is updated at return.
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
 *                       The attached memory address is updated at return.
 * offset : Offset to attach the shared memory address to
 */
int MPL_shm_fixed_seg_attach(MPL_shm_hnd_t hnd, intptr_t seg_sz, void **shm_addr_ptr, int offset)
{
    return MPL_shm_seg_create_attach_templ(hnd, seg_sz, shm_addr_ptr, offset,
                                           MPLI_SHM_FLAG_SHM_ATTACH | MPLI_SHM_FLAG_FIXED_ADDR);
}

/* Detach from an attached SHM segment */
int MPL_shm_seg_detach(MPL_shm_hnd_t hnd, void **shm_addr_ptr, intptr_t seg_sz)
{
    int rc = -1;

    rc = munmap(*shm_addr_ptr, seg_sz);
    *shm_addr_ptr = NULL;

    return (rc == 0) ? MPL_SUCCESS : MPL_ERR_SHM_INTERN;
}

/* Remove an existing SHM segment */
int MPL_shm_seg_remove(MPL_shm_hnd_t hnd)
{
    int rc = -1;

    rc = unlink(MPLI_shm_ghnd_get_by_ref(hnd));

    return (rc == 0) ? MPL_SUCCESS : MPL_ERR_SHM_INTERN;
}

#endif /* MPL_USE_MMAP_SHM */
