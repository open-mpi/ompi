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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>

#include "usnic_direct.h"
#include "usd.h"
#include "usd_ib_cmd.h"

/*
 * Issue driver command to register memory region
 */
int
usd_reg_mr(
    struct usd_device *dev,
    void *vaddr,
    size_t length,
    struct usd_mr **mr_o)
{
    struct usd_mr *mr;
    int ret;

    mr = calloc(sizeof(*mr), 1);
    if (mr == NULL) {
        return -errno;
    }

    ret = usd_ib_cmd_reg_mr(dev, vaddr, length, mr);

    if (ret == 0) {
        mr->umr_dev = dev;
        mr->umr_vaddr = vaddr;
        mr->umr_length = length;
        *mr_o = mr;
    } else {
        free(mr);
    }

    return ret;
}

/*
 * Issue driver command to de-register memory region
 */
int
usd_dereg_mr(
    struct usd_mr *mr)
{
    int ret;

    ret = usd_ib_cmd_dereg_mr(mr->umr_dev, mr);
    if (ret == 0)
        free(mr);

    return ret;
}

/*
 * Used to allocate memory and an mr to go with it all in one go.  Used
 * to provide memory to the vnic_* functions that call pci_alloc_consistant
 * We want to return a nicely aligned chunk of memory preceded by struct usd_mr.
 * We don't know the alignment of the memory we get back, so allocate a big
 * enough chunk to hold the following:
 *   struct usd_mr
 *   N pad bytes
 *   true length and pointer to usd_mr
 *   page aligned buffer for user
 */
int
usd_alloc_mr(
    struct usd_device *dev,
    size_t size,
    void **vaddr_o)
{
    void *vaddr;
    void *base_addr;
    struct usd_mr *mr;
    size_t true_size;
    int ret;

    true_size = size + sizeof(struct usd_mr) + 2 * sizeof(uintptr_t) +
        sysconf(_SC_PAGESIZE) - 1;
    base_addr = mmap(NULL, true_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base_addr == NULL || base_addr == MAP_FAILED) {
        usd_err("Failed to mmap region of size %lu\n", true_size);
        return -errno;
    }
    mr = base_addr;
    vaddr =
        (void *) ALIGN((uintptr_t) base_addr + sizeof(*mr) + sizeof(mr),
                       sysconf(_SC_PAGESIZE));
    ((uintptr_t *) vaddr)[-1] = (uintptr_t) mr;
    ((uintptr_t *) vaddr)[-2] = true_size;

    ret = usd_ib_cmd_reg_mr(dev, vaddr, size, mr);
    if (ret == 0) {
        mr->umr_dev = dev;
    } else {
        munmap(base_addr, true_size);
        return ret;
    }

    *vaddr_o = vaddr;
    return 0;
}

/*
 * See usd_alloc_mr() for explanation of:
 *  mr = (struct usd_mr *)((uintptr_t *)vaddr)[-1];
 */
int
usd_free_mr(
    void *vaddr)
{
    struct usd_mr *mr;
    size_t true_size;
    int ret;

    mr = (struct usd_mr *) ((uintptr_t *) vaddr)[-1];
    true_size = ((uintptr_t *) vaddr)[-2];

    ret = usd_ib_cmd_dereg_mr(mr->umr_dev, mr);
    if (ret == 0)
        munmap(mr, true_size);

    return ret;
}

/*
 * Utility function for vnic_* routines
 */
char *
pci_name(
    struct pci_dev *pdev)
{
    struct usd_device *dev;

    dev = (struct usd_device *) pdev;

    return dev->ud_ib_dev->id_usnic_name;
}
