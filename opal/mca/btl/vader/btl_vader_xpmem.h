/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_VADER_XPMEM_H)
#define MCA_BTL_VADER_XPMEM_H

#if OPAL_BTL_VADER_HAVE_XPMEM

#if defined(HAVE_XPMEM_H)
  #include <xpmem.h>

  typedef struct xpmem_addr xpmem_addr_t;
#elif defined(HAVE_SN_XPMEM_H)
  #include <sn/xpmem.h>

  typedef int64_t xpmem_segid_t;
  typedef int64_t xpmem_apid_t;
#endif

/* look up the remote pointer in the peer rcache and attach if
 * necessary */

/* largest address we can attach to using xpmem */
#define VADER_MAX_ADDRESS ((uintptr_t)0x7ffffffff000ul)

int mca_btl_vader_xpmem_init (void);

mca_mpool_base_registration_t *vader_get_registation (struct mca_btl_base_endpoint_t *endpoint, void *rem_ptr,
 						      size_t size, int flags, void **local_ptr);

void vader_return_registration (mca_mpool_base_registration_t *reg, struct mca_btl_base_endpoint_t *endpoint);

#else

static inline mca_mpool_base_registration_t *vader_get_registation (struct mca_btl_base_endpoint_t *endpoint, void *rem_ptr,
                                                                        size_t size, int flags, void **local_ptr)
{
    (void) endpoint;
    (void) rem_ptr;
    (void) size;
    (void) flags;
    (void) local_ptr;
    return NULL;
}

static inline void vader_return_registration (mca_mpool_base_registration_t *reg, struct mca_btl_base_endpoint_t *endpoint)
{
    (void) reg;
    (void) endpoint;
}

#endif /* OPAL_BTL_VADER_HAVE_XPMEM */

#endif
