/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * LICENSE_END
 *
 *
 */

#ifndef _KCOMPAT_PRIV_H_
#define _KCOMPAT_PRIV_H_

#include <stdint.h>
#include <stdio.h>

struct pci_dev;
typedef uint64_t dma_addr_t;
struct usd_device;

int usd_alloc_mr(struct usd_device *dev, size_t size, void **vaddr_o);
int usd_free_mr(void *);
char *pci_name(struct pci_dev *pdev);

static inline void *pci_alloc_consistent(struct pci_dev *hwdev,
                                         size_t size,
                                         dma_addr_t * dma_handle)
{
    int ret;
    void *va;

    ret = usd_alloc_mr((struct usd_device *) hwdev, size, &va);
    if (ret == 0) {
        *dma_handle = (dma_addr_t) va;
        return va;
    } else {
        return NULL;
    }
}

static inline void pci_free_consistent( __attribute__ ((unused))
                                       struct pci_dev *pdev,
                                       __attribute__ ((unused)) size_t
                                       size, void *vaddr,
                                       __attribute__ ((unused)) dma_addr_t
                                       dma)
{
    (void) usd_free_mr(vaddr);
}

#define usd_err(args...) fprintf(stderr, args)
#define pr_err usd_err
#define pr_warning(args...)

#ifndef wmb
#define wmb() asm volatile("" ::: "memory")
#endif

#ifndef rmb
#define rmb() asm volatile("" ::: "memory")
#endif

#endif /* _KCOMPAT_PRIV_H_ */
