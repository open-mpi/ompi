/*
 * Copyright 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright 2007 Nuova Systems, Inc.  All rights reserved.
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

#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/pci.h>
#include <linux/delay.h>
#include <linux/if_ether.h>

#ifndef FOR_UPSTREAM_KERNEL
#include "kcompat.h"
#endif
#include "vnic_resource.h"
#include "vnic_devcmd.h"
#include "vnic_dev.h"
#include "vnic_stats.h"
#include "vnic_wq.h"

struct devcmd2_controller {
	struct vnic_wq_ctrl *wq_ctrl;
	struct vnic_dev_ring results_ring;
	struct vnic_wq wq;
	struct vnic_devcmd2 *cmd_ring;
	struct devcmd2_result *result;
	u16 next_result;
	u16 result_size;
	int color;
};

enum vnic_proxy_type {
	PROXY_NONE,
	PROXY_BY_BDF,
	PROXY_BY_INDEX,
};

struct vnic_res {
	void __iomem *vaddr;
	dma_addr_t bus_addr;
	unsigned int count;
	u8 bar_num;
	u32 bar_offset;
	unsigned long len;
};

struct vnic_intr_coal_timer_info {
	u32 mul;
	u32 div;
	u32 max_usec;
};

struct vnic_dev {
	void *priv;
	struct pci_dev *pdev;
	struct vnic_res res[RES_TYPE_MAX];
	enum vnic_dev_intr_mode intr_mode;
	struct vnic_devcmd __iomem *devcmd;
	struct vnic_devcmd_notify *notify;
	struct vnic_devcmd_notify notify_copy;
	dma_addr_t notify_pa;
	u32 notify_sz;
	dma_addr_t linkstatus_pa;
	struct vnic_stats *stats;
	dma_addr_t stats_pa;
	struct vnic_devcmd_fw_info *fw_info;
	dma_addr_t fw_info_pa;
	enum vnic_proxy_type proxy;
	u32 proxy_index;
	u64 args[VNIC_DEVCMD_NARGS];
	struct vnic_intr_coal_timer_info intr_coal_timer_info;
	struct devcmd2_controller *devcmd2;
	int (*devcmd_rtn)(struct vnic_dev *vdev, enum vnic_devcmd_cmd cmd, int wait);
};

#define VNIC_MAX_RES_HDR_SIZE \
	(sizeof(struct vnic_resource_header) + \
	sizeof(struct vnic_resource) * RES_TYPE_MAX)
#define VNIC_RES_STRIDE	128

void *vnic_dev_priv(struct vnic_dev *vdev)
{
	return vdev->priv;
}

#ifndef NOT_FOR_OPEN_SOURCE
int vnic_dev_get_size(void)
{
	return sizeof(struct vnic_dev);
}

#endif

static int vnic_dev_discover_res(struct vnic_dev *vdev,
	struct vnic_dev_bar *bar, unsigned int num_bars)
{
	struct vnic_resource_header __iomem *rh;
	struct mgmt_barmap_hdr __iomem *mrh;
	struct vnic_resource __iomem *r;
	u8 type;

	if (num_bars == 0)
		return -EINVAL;

	if (bar->len < VNIC_MAX_RES_HDR_SIZE) {
		pr_err("vNIC BAR0 res hdr length error\n");
		return -EINVAL;
	}

	rh  = bar->vaddr;
	mrh = bar->vaddr;
	if (!rh) {
		pr_err("vNIC BAR0 res hdr not mem-mapped\n");
		return -EINVAL;
	}

	/* Check for mgmt vnic in addition to normal vnic */
	if ((ioread32(&rh->magic) != VNIC_RES_MAGIC) ||
		(ioread32(&rh->version) != VNIC_RES_VERSION)) {
		if ((ioread32(&mrh->magic) != MGMTVNIC_MAGIC) ||
			(ioread32(&mrh->version) != MGMTVNIC_VERSION)) {
			pr_err("vNIC BAR0 res magic/version error "
			"exp (%lx/%lx) or (%lx/%lx), curr (%x/%x)\n",
			VNIC_RES_MAGIC, VNIC_RES_VERSION,
			MGMTVNIC_MAGIC, MGMTVNIC_VERSION,
			ioread32(&rh->magic), ioread32(&rh->version));
			return -EINVAL;
		}
	}

	if (ioread32(&mrh->magic) == MGMTVNIC_MAGIC)
		r = (struct vnic_resource __iomem *)(mrh + 1);
	else
		r = (struct vnic_resource __iomem *)(rh + 1);

	while ((type = ioread8(&r->type)) != RES_TYPE_EOL) {

		u8 bar_num = ioread8(&r->bar);
		u32 bar_offset = ioread32(&r->bar_offset);
		u32 count = ioread32(&r->count);
		u32 len;

		r++;

		if (bar_num >= num_bars)
			continue;

		if (!bar[bar_num].len || !bar[bar_num].vaddr)
			continue;

		switch (type) {
		case RES_TYPE_WQ:
		case RES_TYPE_RQ:
		case RES_TYPE_CQ:
		case RES_TYPE_INTR_CTRL:
			/* each count is stride bytes long */
			len = count * VNIC_RES_STRIDE;
			if (len + bar_offset > bar[bar_num].len) {
				pr_err("vNIC BAR0 resource %d "
					"out-of-bounds, offset 0x%x + "
					"size 0x%x > bar len 0x%lx\n",
					type, bar_offset,
					len,
					bar[bar_num].len);
				return -EINVAL;
			}
			break;
		case RES_TYPE_MEM:
		case RES_TYPE_INTR_PBA_LEGACY:
#ifdef CONFIG_MIPS
		case RES_TYPE_DEV:
#endif
		case RES_TYPE_DEVCMD2:
		case RES_TYPE_DEVCMD:
#ifdef ENIC_UPT
		case RES_TYPE_PASS_THRU_PAGE:
#endif
			len = count;
			break;
		default:
			continue;
		}

		vdev->res[type].count = count;
		vdev->res[type].vaddr = (char __iomem *)bar[bar_num].vaddr +
			bar_offset;
		vdev->res[type].bus_addr = bar[bar_num].bus_addr + bar_offset;
		vdev->res[type].bar_num = bar_num;
		vdev->res[type].bar_offset = bar_offset;
		vdev->res[type].len = len;
	}

	return 0;
}

/*
 * Assign virtual addresses to all resources whose bus address falls
 * within the specified map.
 * vnic_dev_discover_res assigns res vaddrs based on the assumption that
 * the entire bar is mapped once. When memory regions on the bar
 * are mapped seperately, the vnic res for those regions need to be updated
 * with new virutal addresses.
 * Notice that the mapping and virtual address update need to be done before
 * other VNIC APIs that might use the old virtual address,
 * such as vdev->devcmd
 */
void vnic_dev_upd_res_vaddr(struct vnic_dev *vdev,
		struct vnic_dev_iomap_info *map)
{
	int i;

	for (i = RES_TYPE_EOL; i < RES_TYPE_MAX; i++) {
		if (i == RES_TYPE_EOL)
			continue;
		if (vdev->res[i].bus_addr >= map->bus_addr &&
			vdev->res[i].bus_addr < map->bus_addr + map->len)
			vdev->res[i].vaddr = map->vaddr +
					(vdev->res[i].bus_addr - map->bus_addr);
	}
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_upd_res_vaddr);
#endif

unsigned int vnic_dev_get_res_count(struct vnic_dev *vdev,
	enum vnic_res_type type)
{
	return vdev->res[type].count;
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_get_res_count);
#endif

void __iomem *vnic_dev_get_res(struct vnic_dev *vdev, enum vnic_res_type type,
	unsigned int index)
{
	if (!vdev->res[type].vaddr)
		return NULL;

	switch (type) {
	case RES_TYPE_WQ:
	case RES_TYPE_RQ:
	case RES_TYPE_CQ:
	case RES_TYPE_INTR_CTRL:
		return (char __iomem *)vdev->res[type].vaddr +
			index * VNIC_RES_STRIDE;
	default:
		return (char __iomem *)vdev->res[type].vaddr;
	}
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_get_res);
#endif

dma_addr_t vnic_dev_get_res_bus_addr(struct vnic_dev *vdev,
	enum vnic_res_type type, unsigned int index)
{
	switch (type) {
	case RES_TYPE_WQ:
	case RES_TYPE_RQ:
	case RES_TYPE_CQ:
	case RES_TYPE_INTR_CTRL:
		return vdev->res[type].bus_addr +
			index * VNIC_RES_STRIDE;
	default:
		return vdev->res[type].bus_addr;
	}
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_get_res_bus_addr);
#endif

uint8_t vnic_dev_get_res_bar(struct vnic_dev *vdev,
	enum vnic_res_type type)
{
	return vdev->res[type].bar_num;
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_get_res_bar);
#endif

uint32_t vnic_dev_get_res_offset(struct vnic_dev *vdev,
	enum vnic_res_type type, unsigned int index)
{
	switch (type) {
	case RES_TYPE_WQ:
	case RES_TYPE_RQ:
	case RES_TYPE_CQ:
	case RES_TYPE_INTR_CTRL:
		return vdev->res[type].bar_offset +
			index * VNIC_RES_STRIDE;
	default:
		return vdev->res[type].bar_offset;
	}
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_get_res_offset);
#endif

/*
 * Get the length of the res type
 */
unsigned long vnic_dev_get_res_type_len(struct vnic_dev *vdev,
					enum vnic_res_type type)
{
	return vdev->res[type].len;
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_get_res_type_len);
#endif

#ifdef FOR_UPSTREAM_KERNEL
static unsigned int vnic_dev_desc_ring_size(struct vnic_dev_ring *ring,
#else
unsigned int vnic_dev_desc_ring_size(struct vnic_dev_ring *ring,
#endif
	unsigned int desc_count, unsigned int desc_size)
{
	/* The base address of the desc rings must be 512 byte aligned.
	 * Descriptor count is aligned to groups of 32 descriptors.  A
	 * count of 0 means the maximum 4096 descriptors.  Descriptor
	 * size is aligned to 16 bytes.
	 */

	unsigned int count_align = 32;
	unsigned int desc_align = 16;

	ring->base_align = 512;

	if (desc_count == 0)
		desc_count = 4096;

	ring->desc_count = ALIGN(desc_count, count_align);

	ring->desc_size = ALIGN(desc_size, desc_align);

	ring->size = ring->desc_count * ring->desc_size;
	ring->size_unaligned = ring->size + ring->base_align;

	return ring->size_unaligned;
}

void vnic_dev_clear_desc_ring(struct vnic_dev_ring *ring)
{
	memset(ring->descs, 0, ring->size);
}

int vnic_dev_alloc_desc_ring(struct vnic_dev *vdev, struct vnic_dev_ring *ring,
	unsigned int desc_count, unsigned int desc_size)
{
	vnic_dev_desc_ring_size(ring, desc_count, desc_size);

	ring->descs_unaligned = pci_alloc_consistent(vdev->pdev,
		ring->size_unaligned,
		&ring->base_addr_unaligned);

	if (!ring->descs_unaligned) {
		pr_err("Failed to allocate ring (size=%d), aborting\n",
			(int)ring->size);
		return -ENOMEM;
	}

	ring->base_addr = ALIGN(ring->base_addr_unaligned,
		ring->base_align);
	ring->descs = (u8 *)ring->descs_unaligned +
		(ring->base_addr - ring->base_addr_unaligned);

	vnic_dev_clear_desc_ring(ring);

	ring->desc_avail = ring->desc_count - 1;

	return 0;
}

void vnic_dev_free_desc_ring(struct vnic_dev *vdev, struct vnic_dev_ring *ring)
{
	if (ring->descs) {
		pci_free_consistent(vdev->pdev,
			ring->size_unaligned,
			ring->descs_unaligned,
			ring->base_addr_unaligned);
		ring->descs = NULL;
	}
}

static int _vnic_dev_cmd(struct vnic_dev *vdev, enum vnic_devcmd_cmd cmd,
	int wait)
{
#if defined(CONFIG_MIPS) || defined(MGMT_VNIC)
	return 0;
#else
	struct vnic_devcmd __iomem *devcmd = vdev->devcmd;
	unsigned int i;
	int delay;
	u32 status;
	int err;

	status = ioread32(&devcmd->status);
	if (status == 0xFFFFFFFF) {
		/* PCI-e target device is gone */
		return -ENODEV;
	}
	if (status & STAT_BUSY) {
#ifndef __WINDOWS__
		pr_err("%s: Busy devcmd %d\n",
			pci_name(vdev->pdev), _CMD_N(cmd));
#else
		pr_err("Busy devcmd %d\n", _CMD_N(cmd));
#endif
		return -EBUSY;
	}

	if (_CMD_DIR(cmd) & _CMD_DIR_WRITE) {
		for (i = 0; i < VNIC_DEVCMD_NARGS; i++)
			writeq(vdev->args[i], &devcmd->args[i]);
		wmb();
	}

	iowrite32(cmd, &devcmd->cmd);

	if ((_CMD_FLAGS(cmd) & _CMD_FLAGS_NOWAIT))
		return 0;

	for (delay = 0; delay < wait; delay++) {

		udelay(100);

		status = ioread32(&devcmd->status);
		if (status == 0xFFFFFFFF) {
			/* PCI-e target device is gone */
			return -ENODEV;
		}

		if (!(status & STAT_BUSY)) {
			if (status & STAT_ERROR) {
				err = -(int)readq(&devcmd->args[0]);
				if (cmd != CMD_CAPABILITY)
#ifndef __WINDOWS__
					pr_err("%s: Devcmd %d failed "
						"with error code %d\n",
						pci_name(vdev->pdev),
						_CMD_N(cmd), err);
#else
					pr_err("Devcmd %d failed "
						"with error code %d\n",
						_CMD_N(cmd), err);
#endif
				return err;
			}

			if (_CMD_DIR(cmd) & _CMD_DIR_READ) {
				rmb();
				for (i = 0; i < VNIC_DEVCMD_NARGS; i++)
					vdev->args[i] = readq(&devcmd->args[i]);
			}

			return 0;
		}
	}

#ifndef __WINDOWS__
	pr_err("%s: Timedout devcmd %d\n",
		pci_name(vdev->pdev), _CMD_N(cmd));
#else
	pr_err("Timedout devcmd %d\n", _CMD_N(cmd));
#endif
	return -ETIMEDOUT;
#endif
}

static int _vnic_dev_cmd2(struct vnic_dev *vdev, enum vnic_devcmd_cmd cmd,
	int wait)
{
#if defined(CONFIG_MIPS) || defined(MGMT_VNIC)
	return 0;
#else
	struct devcmd2_controller *dc2c = vdev->devcmd2;
	struct devcmd2_result *result = dc2c->result + dc2c->next_result;
	unsigned int i;
	int delay;
	int err;
	u32 fetch_index;
	u32 posted;
	u32 new_posted;

	posted = ioread32(&dc2c->wq_ctrl->posted_index);
	fetch_index = ioread32(&dc2c->wq_ctrl->fetch_index);

	if (posted == 0xFFFFFFFF || fetch_index == 0xFFFFFFFF) { /* check for hardware gone  */
		/* Hardware surprise removal: return error */
		return -ENODEV;

	}
	new_posted = (posted + 1) % DEVCMD2_RING_SIZE;

	if (new_posted == fetch_index) {
		pr_err("%s: wq is full while issuing devcmd2 command %d, "
			"fetch index: %u, posted index: %u\n",
			 pci_name(vdev->pdev),
			 _CMD_N(cmd),
			 fetch_index, posted);
		return -EBUSY;

	}
	dc2c->cmd_ring[posted].cmd = cmd;
	dc2c->cmd_ring[posted].flags = 0;

	if ((_CMD_FLAGS(cmd) & _CMD_FLAGS_NOWAIT))
		dc2c->cmd_ring[posted].flags |= DEVCMD2_FNORESULT;
	if (_CMD_DIR(cmd) & _CMD_DIR_WRITE) {
		for (i = 0; i < VNIC_DEVCMD_NARGS; i++)
			dc2c->cmd_ring[posted].args[i] = vdev->args[i];

	}

	/* Adding write memory barrier prevents compiler and/or CPU
 	 * reordering, thus avoiding descriptor posting before
	 * descriptor is initialized. Otherwise, hardware can read
	 * stale descriptor fields.
	 */
	wmb();
	iowrite32(new_posted, &dc2c->wq_ctrl->posted_index);

	if (dc2c->cmd_ring[posted].flags & DEVCMD2_FNORESULT)
		return 0;

	for (delay = 0; delay < wait; delay++) {
		udelay(100);
		if (result->color == dc2c->color) {
			dc2c->next_result++;
			if (dc2c->next_result == dc2c->result_size) {
				dc2c->next_result = 0;
				dc2c->color = dc2c->color ? 0 : 1;
			}
			if (result->error) {
				err = -(int) result->error;
				if (err != ERR_ECMDUNKNOWN || cmd != CMD_CAPABILITY)
					pr_err("%s:Error %d devcmd %d\n",
						 pci_name(vdev->pdev),
						 err, _CMD_N(cmd));
				return err;
			}
			if (_CMD_DIR(cmd) & _CMD_DIR_READ) {
				rmb();
				for (i = 0; i < VNIC_DEVCMD_NARGS; i++)
					vdev->args[i] = result->results[i];
			}
			return 0;
		}
	}
	
	pr_err("%s:Timed out devcmd %d\n", pci_name(vdev->pdev),
			_CMD_N(cmd));

	return -ETIMEDOUT;
#endif
}

int vnic_dev_init_devcmdorig(struct vnic_dev *vdev)
{
#if !defined(CONFIG_MIPS) && !defined(MGMT_VNIC)
	vdev->devcmd = vnic_dev_get_res(vdev, RES_TYPE_DEVCMD, 0);
	if (!vdev->devcmd)
		return -ENODEV;

	vdev->devcmd_rtn = &_vnic_dev_cmd;
	return 0;
#else
	return 0;
#endif
}

static int vnic_dev_init_devcmd2(struct vnic_dev *vdev)
{
#if !defined(CONFIG_MIPS) && !defined(MGMT_VNIC)
	int err;
	unsigned int fetch_index;
	
	if (vdev->devcmd2)
		return 0;

	vdev->devcmd2 = kzalloc(sizeof(*vdev->devcmd2), GFP_ATOMIC);
	if (!vdev->devcmd2)
		return -ENOMEM;
	
	vdev->devcmd2->color = 1;
	vdev->devcmd2->result_size = DEVCMD2_RING_SIZE;
	err = vnic_wq_devcmd2_alloc(vdev, &vdev->devcmd2->wq,
				DEVCMD2_RING_SIZE, DEVCMD2_DESC_SIZE);
	if (err)
		goto err_free_devcmd2;
	
	fetch_index = ioread32(&vdev->devcmd2->wq.ctrl->fetch_index);
	if (fetch_index == 0xFFFFFFFF) { /* check for hardware gone  */
		pr_err("Fatal error in devcmd2 init - hardware surprise removal");
		return -ENODEV;
	}
	
	/*
	 * Don't change fetch_index ever and
	 * set posted_index same as fetch_index
	 * when setting up the WQ for devmcd2.
	 */
	vnic_wq_init_start(&vdev->devcmd2->wq, 0, fetch_index, fetch_index, 0, 0);
	vnic_wq_enable(&vdev->devcmd2->wq);
	
	err = vnic_dev_alloc_desc_ring(vdev, &vdev->devcmd2->results_ring,
			DEVCMD2_RING_SIZE, DEVCMD2_DESC_SIZE);
	if (err)
		goto err_free_wq;
	
	vdev->devcmd2->result =
		(struct devcmd2_result *) vdev->devcmd2->results_ring.descs;
	vdev->devcmd2->cmd_ring =
		(struct vnic_devcmd2 *) vdev->devcmd2->wq.ring.descs;
	vdev->devcmd2->wq_ctrl = vdev->devcmd2->wq.ctrl;
	vdev->args[0] = (u64) vdev->devcmd2->results_ring.base_addr |
				VNIC_PADDR_TARGET;
	vdev->args[1] = DEVCMD2_RING_SIZE;

	err = _vnic_dev_cmd2(vdev, CMD_INITIALIZE_DEVCMD2, 1000);
	if (err)
		goto err_free_desc_ring;
	
	vdev->devcmd_rtn = &_vnic_dev_cmd2;

	return 0;

err_free_desc_ring:
	vnic_dev_free_desc_ring(vdev, &vdev->devcmd2->results_ring);
err_free_wq:
	vnic_wq_disable(&vdev->devcmd2->wq);
	vnic_wq_free(&vdev->devcmd2->wq);
err_free_devcmd2:
	kfree(vdev->devcmd2);
	vdev->devcmd2 = NULL;
	
	return err;
#else
	return 0;
#endif
}

static void vnic_dev_deinit_devcmd2(struct vnic_dev *vdev)
{
#if !defined(CONFIG_MIPS) && !defined(MGMT_VNIC)
	vnic_dev_free_desc_ring(vdev, &vdev->devcmd2->results_ring);
	vnic_wq_disable(&vdev->devcmd2->wq);
	vnic_wq_free(&vdev->devcmd2->wq);
	kfree(vdev->devcmd2);
	vdev->devcmd2 = NULL;
	vdev->devcmd_rtn = &_vnic_dev_cmd;
#endif
}

int vnic_dev_cmd_args(struct vnic_dev *vdev, enum vnic_devcmd_cmd cmd,
               u64 *args, int nargs, int wait)
{
       int err;
       int i;

       if (nargs > VNIC_DEVCMD_NARGS) {
               nargs = VNIC_DEVCMD_NARGS;
       }
       for (i = 0; i < nargs; ++i) {
               vdev->args[i] = args[i];
       }

       err = _vnic_dev_cmd(vdev, cmd, wait);

       for (i = 0; i < nargs; ++i) {
               args[i] = vdev->args[i];
       }
       return err;
}

static int vnic_dev_cmd_proxy(struct vnic_dev *vdev,
	enum vnic_devcmd_cmd proxy_cmd, enum vnic_devcmd_cmd cmd,
	u64 *a0, u64 *a1, int wait)
{
	u32 status;
	int err;

	memset(vdev->args, 0, sizeof(vdev->args));

	vdev->args[0] = vdev->proxy_index;
	vdev->args[1] = cmd;
	vdev->args[2] = *a0;
	vdev->args[3] = *a1;

	err = (*vdev->devcmd_rtn)(vdev, proxy_cmd, wait);
	if (err)
		return err;

	status = (u32)vdev->args[0];
	if (status & STAT_ERROR) {
		err = (int)vdev->args[1];
		if (err != ERR_ECMDUNKNOWN ||
		    cmd != CMD_CAPABILITY)
			pr_err("Error %d proxy devcmd %d\n", err, _CMD_N(cmd));
		return err;
	}

	*a0 = vdev->args[1];
	*a1 = vdev->args[2];

	return 0;
}

static int vnic_dev_cmd_no_proxy(struct vnic_dev *vdev,
	enum vnic_devcmd_cmd cmd, u64 *a0, u64 *a1, int wait)
{
	int err;

	vdev->args[0] = *a0;
	vdev->args[1] = *a1;

	err = (*vdev->devcmd_rtn)(vdev, cmd, wait);

	*a0 = vdev->args[0];
	*a1 = vdev->args[1];

	return err;
}

void vnic_dev_cmd_proxy_by_index_start(struct vnic_dev *vdev, u16 index)
{
	vdev->proxy = PROXY_BY_INDEX;
	vdev->proxy_index = index;
}

#ifndef FOR_UPSTREAM_KERNEL
void vnic_dev_cmd_proxy_by_bdf_start(struct vnic_dev *vdev, u16 bdf)
{
	vdev->proxy = PROXY_BY_BDF;
	vdev->proxy_index = bdf;
}

#endif
void vnic_dev_cmd_proxy_end(struct vnic_dev *vdev)
{
	vdev->proxy = PROXY_NONE;
	vdev->proxy_index = 0;
}

int vnic_dev_cmd(struct vnic_dev *vdev, enum vnic_devcmd_cmd cmd,
	u64 *a0, u64 *a1, int wait)
{
	memset(vdev->args, 0, sizeof(vdev->args));

	switch (vdev->proxy) {
	case PROXY_BY_INDEX:
		return vnic_dev_cmd_proxy(vdev, CMD_PROXY_BY_INDEX, cmd,
				a0, a1, wait);
	case PROXY_BY_BDF:
		return vnic_dev_cmd_proxy(vdev, CMD_PROXY_BY_BDF, cmd,
				a0, a1, wait);
	case PROXY_NONE:
	default:
		return vnic_dev_cmd_no_proxy(vdev, cmd, a0, a1, wait);
	}
}

static int vnic_dev_capable(struct vnic_dev *vdev, enum vnic_devcmd_cmd cmd)
{
	u64 a0 = (u32)cmd, a1 = 0;
	int wait = 1000;
	int err;

	err = vnic_dev_cmd(vdev, CMD_CAPABILITY, &a0, &a1, wait);

	return !(err || a0);
}

int vnic_dev_fw_info(struct vnic_dev *vdev,
	struct vnic_devcmd_fw_info **fw_info)
{
	u64 a0, a1 = 0;
	int wait = 1000;
	int err = 0;

	if (!vdev->fw_info) {
		vdev->fw_info = pci_alloc_consistent(vdev->pdev,
			sizeof(struct vnic_devcmd_fw_info),
			&vdev->fw_info_pa);
		if (!vdev->fw_info)
			return -ENOMEM;

		memset(vdev->fw_info, 0, sizeof(struct vnic_devcmd_fw_info));

		a0 = vdev->fw_info_pa;
		a1 = sizeof(struct vnic_devcmd_fw_info);

		/* only get fw_info once and cache it */
		if (vnic_dev_capable(vdev, CMD_MCPU_FW_INFO))
			err = vnic_dev_cmd(vdev, CMD_MCPU_FW_INFO,
				&a0, &a1, wait);
		else
			err = vnic_dev_cmd(vdev, CMD_MCPU_FW_INFO_OLD,
				&a0, &a1, wait);
	}

	*fw_info = vdev->fw_info;

	return err;
}

#ifndef FOR_UPSTREAM_KERNEL
int vnic_dev_asic_info(struct vnic_dev *vdev, u16 *asic_type, u16 *asic_rev)
{
	struct vnic_devcmd_fw_info *fw_info;
	int err;

	err = vnic_dev_fw_info(vdev, &fw_info);
	if (err)
		return err;

	*asic_type = fw_info->asic_type;
	*asic_rev = fw_info->asic_rev;

	return 0;
}

#endif
int vnic_dev_spec(struct vnic_dev *vdev, unsigned int offset, unsigned int size,
	void *value)
{
#ifdef CONFIG_MIPS
	u8 *v = vnic_dev_get_res(vdev, RES_TYPE_DEV, 0);
	if (!v) {
		pr_err("vNIC device-specific region not found.\n");
		return -EINVAL;
	}

	switch (size) {
	case 1:
		*(u8 *)value = ioread8(v + offset);
		break;
	case 2:
		*(u16 *)value = ioread16(v + offset);
		break;
	case 4:
		*(u32 *)value = ioread32(v + offset);
		break;
	case 8:
		*(u64 *)value = readq(v + offset);
		break;
	default:
		BUG();
		break;
	}

	return 0;
#else
	u64 a0, a1;
	int wait = 1000;
	int err;

	a0 = offset;
	a1 = size;

	err = vnic_dev_cmd(vdev, CMD_DEV_SPEC, &a0, &a1, wait);

	switch (size) {
	case 1:
		*(u8 *)value = (u8)a0;
		break;
	case 2:
		*(u16 *)value = (u16)a0;
		break;
	case 4:
		*(u32 *)value = (u32)a0;
		break;
	case 8:
		*(u64 *)value = a0;
		break;
	default:
		BUG();
		break;
	}

	return err;
#endif
}

#ifndef FOR_UPSTREAM_KERNEL
int vnic_dev_stats_clear(struct vnic_dev *vdev)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	return vnic_dev_cmd(vdev, CMD_STATS_CLEAR, &a0, &a1, wait);
}

#endif
int vnic_dev_stats_dump(struct vnic_dev *vdev, struct vnic_stats **stats)
{
	u64 a0, a1;
	int wait = 1000;

	if (!vdev->stats) {
		vdev->stats = pci_alloc_consistent(vdev->pdev,
			sizeof(struct vnic_stats), &vdev->stats_pa);
		if (!vdev->stats)
			return -ENOMEM;
	}

	*stats = vdev->stats;
	a0 = vdev->stats_pa;
	a1 = sizeof(struct vnic_stats);

	return vnic_dev_cmd(vdev, CMD_STATS_DUMP, &a0, &a1, wait);
}

int vnic_dev_close(struct vnic_dev *vdev)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	return vnic_dev_cmd(vdev, CMD_CLOSE, &a0, &a1, wait);
}

#ifndef FOR_UPSTREAM_KERNEL
/** Deprecated.  @see vnic_dev_enable_wait */
int vnic_dev_enable(struct vnic_dev *vdev)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	return vnic_dev_cmd(vdev, CMD_ENABLE, &a0, &a1, wait);
}

#endif
int vnic_dev_enable_wait(struct vnic_dev *vdev)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;

	if (vnic_dev_capable(vdev, CMD_ENABLE_WAIT))
		return vnic_dev_cmd(vdev, CMD_ENABLE_WAIT, &a0, &a1, wait);
	else
		return vnic_dev_cmd(vdev, CMD_ENABLE, &a0, &a1, wait);
}

int vnic_dev_disable(struct vnic_dev *vdev)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	return vnic_dev_cmd(vdev, CMD_DISABLE, &a0, &a1, wait);
}

int vnic_dev_open(struct vnic_dev *vdev, int arg)
{
	u64 a0 = (u32)arg, a1 = 0;
	int wait = 1000;
	return vnic_dev_cmd(vdev, CMD_OPEN, &a0, &a1, wait);
}

int vnic_dev_open_done(struct vnic_dev *vdev, int *done)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	int err;

	*done = 0;

	err = vnic_dev_cmd(vdev, CMD_OPEN_STATUS, &a0, &a1, wait);
	if (err)
		return err;

	*done = (a0 == 0);

	return 0;
}

#ifdef FOR_UPSTREAM_KERNEL
static int vnic_dev_soft_reset(struct vnic_dev *vdev, int arg)
#else
int vnic_dev_soft_reset(struct vnic_dev *vdev, int arg)
#endif
{
	u64 a0 = (u32)arg, a1 = 0;
	int wait = 1000;
	return vnic_dev_cmd(vdev, CMD_SOFT_RESET, &a0, &a1, wait);
}

#ifdef FOR_UPSTREAM_KERNEL
static int vnic_dev_soft_reset_done(struct vnic_dev *vdev, int *done)
#else
int vnic_dev_soft_reset_done(struct vnic_dev *vdev, int *done)
#endif
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	int err;

	*done = 0;

	err = vnic_dev_cmd(vdev, CMD_SOFT_RESET_STATUS, &a0, &a1, wait);
	if (err)
		return err;

	*done = (a0 == 0);

	return 0;
}

int vnic_dev_hang_reset(struct vnic_dev *vdev, int arg)
{
	u64 a0 = (u32)arg, a1 = 0;
	int wait = 1000;
	int err;

	if (vnic_dev_capable(vdev, CMD_HANG_RESET)) {
		return vnic_dev_cmd(vdev, CMD_HANG_RESET,
				&a0, &a1, wait);
	} else {
		err = vnic_dev_soft_reset(vdev, arg);
		if (err)
			return err;
		return vnic_dev_init(vdev, 0);
	}
}

int vnic_dev_hang_reset_done(struct vnic_dev *vdev, int *done)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	int err;

	*done = 0;

	if (vnic_dev_capable(vdev, CMD_HANG_RESET_STATUS)) {
		err = vnic_dev_cmd(vdev, CMD_HANG_RESET_STATUS,
				&a0, &a1, wait);
		if (err)
			return err;
	} else {
		return vnic_dev_soft_reset_done(vdev, done);
	}

	*done = (a0 == 0);

	return 0;
}

int vnic_dev_hang_notify(struct vnic_dev *vdev)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	return vnic_dev_cmd(vdev, CMD_HANG_NOTIFY, &a0, &a1, wait);
}

int vnic_dev_get_mac_addr(struct vnic_dev *vdev, u8 *mac_addr)
{
#if defined(CONFIG_MIPS) || defined(MGMT_VNIC)
	u64 laa = 0x02;
	memcpy(mac_addr, &laa, ETH_ALEN);
	return 0;
#else
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	int err, i;

	for (i = 0; i < ETH_ALEN; i++)
		mac_addr[i] = 0;

	err = vnic_dev_cmd(vdev, CMD_GET_MAC_ADDR, &a0, &a1, wait);
	if (err)
		return err;

	for (i = 0; i < ETH_ALEN; i++)
		mac_addr[i] = ((u8 *)&a0)[i];

	return 0;
#endif
}

int vnic_dev_packet_filter(struct vnic_dev *vdev, int directed, int multicast,
	int broadcast, int promisc, int allmulti)
{
	u64 a0, a1 = 0;
	int wait = 1000;
	int err;

	a0 = (directed ? CMD_PFILTER_DIRECTED : 0) |
	     (multicast ? CMD_PFILTER_MULTICAST : 0) |
	     (broadcast ? CMD_PFILTER_BROADCAST : 0) |
	     (promisc ? CMD_PFILTER_PROMISCUOUS : 0) |
	     (allmulti ? CMD_PFILTER_ALL_MULTICAST : 0);

	err = vnic_dev_cmd(vdev, CMD_PACKET_FILTER, &a0, &a1, wait);
	if (err)
		pr_err("Can't set packet filter\n");

	return err;
}

#ifndef FOR_UPSTREAM_KERNEL
int vnic_dev_packet_filter_all(struct vnic_dev *vdev, int directed,
	int multicast, int broadcast, int promisc, int allmulti)
{
	u64 a0, a1 = 0;
	int wait = 1000;
	int err;

	a0 = (directed ? CMD_PFILTER_DIRECTED : 0) |
	     (multicast ? CMD_PFILTER_MULTICAST : 0) |
	     (broadcast ? CMD_PFILTER_BROADCAST : 0) |
	     (promisc ? CMD_PFILTER_PROMISCUOUS : 0) |
	     (allmulti ? CMD_PFILTER_ALL_MULTICAST : 0);

	err = vnic_dev_cmd(vdev, CMD_PACKET_FILTER_ALL, &a0, &a1, wait);
	if (err)
		pr_err("Can't set packet filter\n");

	return err;
}

#endif
int vnic_dev_add_addr(struct vnic_dev *vdev, u8 *addr)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	int err;
	int i;

	for (i = 0; i < ETH_ALEN; i++)
		((u8 *)&a0)[i] = addr[i];

	err = vnic_dev_cmd(vdev, CMD_ADDR_ADD, &a0, &a1, wait);
	if (err)
		pr_err("Can't add addr [%02x:%02x:%02x:%02x:%02x:%02x], %d\n",
			addr[0], addr[1], addr[2], addr[3], addr[4], addr[5],
			err);

	return err;
}

int vnic_dev_del_addr(struct vnic_dev *vdev, u8 *addr)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	int err;
	int i;

	for (i = 0; i < ETH_ALEN; i++)
		((u8 *)&a0)[i] = addr[i];

	err = vnic_dev_cmd(vdev, CMD_ADDR_DEL, &a0, &a1, wait);
	if (err)
		pr_err("Can't del addr [%02x:%02x:%02x:%02x:%02x:%02x], %d\n",
			addr[0], addr[1], addr[2], addr[3], addr[4], addr[5],
			err);

	return err;
}

int vnic_dev_set_ig_vlan_rewrite_mode(struct vnic_dev *vdev,
	u8 ig_vlan_rewrite_mode)
{
	u64 a0 = ig_vlan_rewrite_mode, a1 = 0;
	int wait = 1000;

	if (vnic_dev_capable(vdev, CMD_IG_VLAN_REWRITE_MODE))
		return vnic_dev_cmd(vdev, CMD_IG_VLAN_REWRITE_MODE,
				&a0, &a1, wait);
	else
		return 0;
}

#ifndef FOR_UPSTREAM_KERNEL
int vnic_dev_raise_intr(struct vnic_dev *vdev, u16 intr)
{
	u64 a0 = intr, a1 = 0;
	int wait = 1000;
	int err;

	err = vnic_dev_cmd(vdev, CMD_IAR, &a0, &a1, wait);
	if (err)
		pr_err("Failed to raise INTR[%d], err %d\n", intr, err);

	return err;
}

#endif
#ifdef FOR_UPSTREAM_KERNEL
static int vnic_dev_notify_setcmd(struct vnic_dev *vdev,
#else
int vnic_dev_notify_setcmd(struct vnic_dev *vdev,
#endif
	void *notify_addr, dma_addr_t notify_pa, u16 intr)
{
	u64 a0, a1;
	int wait = 1000;
	int r;

	memset(notify_addr, 0, sizeof(struct vnic_devcmd_notify));
	vdev->notify = notify_addr;
	vdev->notify_pa = notify_pa;

	a0 = (u64)notify_pa;
	a1 = ((u64)intr << 32) & 0x0000ffff00000000ULL;
	a1 += sizeof(struct vnic_devcmd_notify);

	r = vnic_dev_cmd(vdev, CMD_NOTIFY, &a0, &a1, wait);
	vdev->notify_sz = (r == 0) ? (u32)a1 : 0;
	return r;
}

int vnic_dev_notify_set(struct vnic_dev *vdev, u16 intr)
{
	void *notify_addr;
	dma_addr_t notify_pa;

	if (vdev->notify || vdev->notify_pa) {
		pr_err("notify block %p still allocated", vdev->notify);
		return -EINVAL;
	}

	notify_addr = pci_alloc_consistent(vdev->pdev,
			sizeof(struct vnic_devcmd_notify),
			&notify_pa);
	if (!notify_addr)
		return -ENOMEM;

	return vnic_dev_notify_setcmd(vdev, notify_addr, notify_pa, intr);
}

#ifdef FOR_UPSTREAM_KERNEL
static int vnic_dev_notify_unsetcmd(struct vnic_dev *vdev)
#else
int vnic_dev_notify_unsetcmd(struct vnic_dev *vdev)
#endif
{
	u64 a0, a1;
	int wait = 1000;
	int err;

	a0 = 0;  /* paddr = 0 to unset notify buffer */
	a1 = 0x0000ffff00000000ULL; /* intr num = -1 to unreg for intr */
	a1 += sizeof(struct vnic_devcmd_notify);

	err = vnic_dev_cmd(vdev, CMD_NOTIFY, &a0, &a1, wait);
	vdev->notify = NULL;
	vdev->notify_pa = 0;
	vdev->notify_sz = 0;

	return err;
}

int vnic_dev_notify_unset(struct vnic_dev *vdev)
{
#ifdef __VMKLNX__	
	struct vnic_devcmd_notify *notify_tmp = vdev->notify;
	dma_addr_t notify_pa_tmp = vdev->notify_pa;
	int r;
	
	r = vnic_dev_notify_unsetcmd(vdev);
	if (notify_tmp) {
		pci_free_consistent(vdev->pdev,
			sizeof(struct vnic_devcmd_notify),
			notify_tmp,
			notify_pa_tmp);
	}

	return r;
#else 
	if (vdev->notify) {
		pci_free_consistent(vdev->pdev,
			sizeof(struct vnic_devcmd_notify),
			vdev->notify,
			vdev->notify_pa);
	}

	return vnic_dev_notify_unsetcmd(vdev);
#endif       	
}

static int vnic_dev_notify_ready(struct vnic_dev *vdev)
{
	u32 *words;
	unsigned int nwords = vdev->notify_sz / 4;
	unsigned int i;
	u32 csum;

	if (!vdev->notify || !vdev->notify_sz)
		return 0;

	do {
		csum = 0;
		memcpy(&vdev->notify_copy, vdev->notify, vdev->notify_sz);
		words = (u32 *)&vdev->notify_copy;
		for (i = 1; i < nwords; i++)
			csum += words[i];
	} while (csum != words[0]);

	return 1;
}

int vnic_dev_init(struct vnic_dev *vdev, int arg)
{
	u64 a0 = (u32)arg, a1 = 0;
	int wait = 1000;
	int r = 0;

	if (vnic_dev_capable(vdev, CMD_INIT))
		r = vnic_dev_cmd(vdev, CMD_INIT, &a0, &a1, wait);
	else {
		vnic_dev_cmd(vdev, CMD_INIT_v1, &a0, &a1, wait);
		if (a0 & CMD_INITF_DEFAULT_MAC) {
			/* Emulate these for old CMD_INIT_v1 which
			 * didn't pass a0 so no CMD_INITF_*.
			 */
			vnic_dev_cmd(vdev, CMD_GET_MAC_ADDR, &a0, &a1, wait);
			vnic_dev_cmd(vdev, CMD_ADDR_ADD, &a0, &a1, wait);
		}
	}
	return r;
}

#ifndef FOR_UPSTREAM_KERNEL
int vnic_dev_init_done(struct vnic_dev *vdev, int *done, int *err)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	int ret;

	*done = 0;

	ret = vnic_dev_cmd(vdev, CMD_INIT_STATUS, &a0, &a1, wait);
	if (ret)
		return ret;

	*done = (a0 == 0);

	*err = (a0 == 0) ? (int)a1 : 0;

	return 0;
}

int vnic_dev_init_prov(struct vnic_dev *vdev, u8 *buf, u32 len)
{
	u64 a0, a1 = len;
	int wait = 1000;
	dma_addr_t prov_pa;
	void *prov_buf;
	int ret;

	prov_buf = pci_alloc_consistent(vdev->pdev, len, &prov_pa);
	if (!prov_buf)
		return -ENOMEM;

	memcpy(prov_buf, buf, len);

	a0 = prov_pa;

	ret = vnic_dev_cmd(vdev, CMD_INIT_PROV_INFO, &a0, &a1, wait);

	pci_free_consistent(vdev->pdev, len, prov_buf, prov_pa);

	return ret;
}

#endif
int vnic_dev_deinit(struct vnic_dev *vdev)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;

	return vnic_dev_cmd(vdev, CMD_DEINIT, &a0, &a1, wait);
}

void vnic_dev_intr_coal_timer_info_default(struct vnic_dev *vdev)
{
	/* Default: hardware intr coal timer is in units of 1.5 usecs */
	vdev->intr_coal_timer_info.mul = 2;
	vdev->intr_coal_timer_info.div = 3;
	vdev->intr_coal_timer_info.max_usec =
		vnic_dev_intr_coal_timer_hw_to_usec(vdev, 0xffff);
}

int vnic_dev_intr_coal_timer_info(struct vnic_dev *vdev)
{
	int wait = 1000;
	int err;

	memset(vdev->args, 0, sizeof(vdev->args));

	if (vnic_dev_capable(vdev, CMD_INTR_COAL_CONVERT))
		err = (*vdev->devcmd_rtn)(vdev, CMD_INTR_COAL_CONVERT, wait);
	else
		err = ERR_ECMDUNKNOWN;

	/* Use defaults when firmware doesn't support the devcmd at all or
	 * supports it for only specific hardware
	 */
	if ((err == ERR_ECMDUNKNOWN) ||
		(!err && !(vdev->args[0] && vdev->args[1] && vdev->args[2]))) {
		pr_warning("Using default conversion factor for "
			"interrupt coalesce timer\n");
		vnic_dev_intr_coal_timer_info_default(vdev);
		return 0;
	}

	if (!err) {
		vdev->intr_coal_timer_info.mul = (u32) vdev->args[0];
		vdev->intr_coal_timer_info.div = (u32) vdev->args[1];
		vdev->intr_coal_timer_info.max_usec = (u32) vdev->args[2];
	}

	return err;
}

int vnic_dev_link_status(struct vnic_dev *vdev)
{
#ifdef CONFIG_MIPS
	return 1;
#else
	if (!vnic_dev_notify_ready(vdev))
		return 0;

	return vdev->notify_copy.link_state;
#endif
}

u32 vnic_dev_port_speed(struct vnic_dev *vdev)
{
	if (!vnic_dev_notify_ready(vdev))
		return 0;

	return vdev->notify_copy.port_speed;
}

u32 vnic_dev_msg_lvl(struct vnic_dev *vdev)
{
	if (!vnic_dev_notify_ready(vdev))
		return 0;

	return vdev->notify_copy.msglvl;
}

u32 vnic_dev_mtu(struct vnic_dev *vdev)
{
#if defined(CONFIG_MIPS) || defined(MGMT_VNIC)
	return 1500;
#else
	if (!vnic_dev_notify_ready(vdev))
		return 0;

	return vdev->notify_copy.mtu;
#endif
}

#ifndef FOR_UPSTREAM_KERNEL
u32 vnic_dev_link_down_cnt(struct vnic_dev *vdev)
{
	if (!vnic_dev_notify_ready(vdev))
		return 0;

	return vdev->notify_copy.link_down_cnt;
}

u32 vnic_dev_notify_status(struct vnic_dev *vdev)
{
	if (!vnic_dev_notify_ready(vdev))
		return 0;

	return vdev->notify_copy.status;
}

u32 vnic_dev_uif(struct vnic_dev *vdev)
{
	if (!vnic_dev_notify_ready(vdev))
		return 0;

	return vdev->notify_copy.uif;
}

#endif
#ifndef NOT_FOR_OPEN_SOURCE
u32 vnic_dev_perbi_rebuild_cnt(struct vnic_dev *vdev)
{
	if (!vnic_dev_notify_ready(vdev))
		return 0;

	return vdev->notify_copy.perbi_rebuild_cnt;
}

#endif
void vnic_dev_set_intr_mode(struct vnic_dev *vdev,
	enum vnic_dev_intr_mode intr_mode)
{
	vdev->intr_mode = intr_mode;
}

enum vnic_dev_intr_mode vnic_dev_get_intr_mode(
	struct vnic_dev *vdev)
{
	return vdev->intr_mode;
}

u32 vnic_dev_intr_coal_timer_usec_to_hw(struct vnic_dev *vdev, u32 usec)
{
	return (usec * vdev->intr_coal_timer_info.mul) /
		vdev->intr_coal_timer_info.div;
}

u32 vnic_dev_intr_coal_timer_hw_to_usec(struct vnic_dev *vdev, u32 hw_cycles)
{
	return (hw_cycles * vdev->intr_coal_timer_info.div) /
		vdev->intr_coal_timer_info.mul;
}

u32 vnic_dev_get_intr_coal_timer_max(struct vnic_dev *vdev)
{
	return vdev->intr_coal_timer_info.max_usec;
}

void vnic_dev_unregister(struct vnic_dev *vdev)
{
	if (vdev) {
		if (vdev->notify)
			pci_free_consistent(vdev->pdev,
				sizeof(struct vnic_devcmd_notify),
				vdev->notify,
				vdev->notify_pa);
		if (vdev->stats)
			pci_free_consistent(vdev->pdev,
				sizeof(struct vnic_stats),
				vdev->stats, vdev->stats_pa);
		if (vdev->fw_info)
			pci_free_consistent(vdev->pdev,
				sizeof(struct vnic_devcmd_fw_info),
				vdev->fw_info, vdev->fw_info_pa);
		if (vdev->devcmd2)
			vnic_dev_deinit_devcmd2(vdev);

		kfree(vdev);
	}
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_unregister);
#endif

struct vnic_dev *vnic_dev_alloc_discover(struct vnic_dev *vdev,
	void *priv, struct pci_dev *pdev, struct vnic_dev_bar *bar,
	unsigned int num_bars)
{
	if (!vdev) {
		vdev = kzalloc(sizeof(struct vnic_dev), GFP_ATOMIC);
		if (!vdev)
			return NULL;
	}

	vdev->priv = priv;
	vdev->pdev = pdev;

	if (vnic_dev_discover_res(vdev, bar, num_bars))
		goto err_out;

	return vdev;

err_out:
	vnic_dev_unregister(vdev);
	return NULL;
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_alloc_discover);
#endif

struct vnic_dev *vnic_dev_register(struct vnic_dev *vdev,
	void *priv, struct pci_dev *pdev, struct vnic_dev_bar *bar,
	unsigned int num_bars)
{
	vdev = vnic_dev_alloc_discover(vdev, priv, pdev, bar, num_bars);
	if (!vdev)
		goto err_out;

	if (vnic_dev_init_devcmdorig(vdev))
		goto err_free;

	return vdev;

err_free:
	vnic_dev_unregister(vdev);
err_out:
	return NULL;
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_register);
#endif

struct pci_dev *vnic_dev_get_pdev(struct vnic_dev *vdev)
{
	return vdev->pdev;
}
#ifdef EXPORT_SYMBOL_FOR_USNIC
EXPORT_SYMBOL(vnic_dev_get_pdev);
#endif

int vnic_dev_cmd_init(struct vnic_dev *vdev, int fallback)
{
#if !defined(CONFIG_MIPS) && !defined(MGMT_VNIC)
	int err;
 	void *p;
    
	p = vnic_dev_get_res(vdev, RES_TYPE_DEVCMD2, 0);
	if (p)
		err = vnic_dev_init_devcmd2(vdev);
	else if (fallback) {
		pr_warning("DEVCMD2 resource not found, fall back to devcmd\n");
		err = vnic_dev_init_devcmdorig(vdev);
	} else {
		pr_err("DEVCMD2 resource not found, no fall back to devcmd allowed\n");
		err = -ENODEV;
	}
	
	return err;
#else
	return 0;
#endif
}

#ifndef NOT_FOR_OPEN_SOURCE
int vnic_dev_int13(struct vnic_dev *vdev, u64 arg, u32 op)
{
	u64 a0 = arg, a1 = op;
	int wait = 1000;
	int r = 0;

	r = vnic_dev_cmd(vdev, CMD_INT13, &a0, &a1, wait);
	return r;
}

int vnic_dev_perbi(struct vnic_dev *vdev, u64 arg, u32 op)
{
	u64 a0 = arg, a1 = op;
	int wait = 5000;
	int r = 0;

	r = vnic_dev_cmd(vdev, CMD_PERBI, &a0, &a1, wait);

	return r;
}

#endif
int vnic_dev_init_prov2(struct vnic_dev *vdev, u8 *buf, u32 len)
{
	u64 a0, a1 = len;
	int wait = 1000;
	dma_addr_t prov_pa;
	void *prov_buf;
	int ret;

	prov_buf = pci_alloc_consistent(vdev->pdev, len, &prov_pa);
	if (!prov_buf)
		return -ENOMEM;

	memcpy(prov_buf, buf, len);

	a0 = prov_pa;

	ret = vnic_dev_cmd(vdev, CMD_INIT_PROV_INFO2, &a0, &a1, wait);

	pci_free_consistent(vdev->pdev, len, prov_buf, prov_pa);

	return ret;
}

int vnic_dev_enable2(struct vnic_dev *vdev, int active)
{
	u64 a0, a1 = 0;
	int wait = 1000;

	a0 = (active ? CMD_ENABLE2_ACTIVE : 0);

	return vnic_dev_cmd(vdev, CMD_ENABLE2, &a0, &a1, wait);
}

static int vnic_dev_cmd_status(struct vnic_dev *vdev, enum vnic_devcmd_cmd cmd,
	int *status)
{
	u64 a0 = cmd, a1 = 0;
	int wait = 1000;
	int ret;

	ret = vnic_dev_cmd(vdev, CMD_STATUS, &a0, &a1, wait);
	if (!ret)
		*status = (int)a0;

	return ret;
}

int vnic_dev_enable2_done(struct vnic_dev *vdev, int *status)
{
	return vnic_dev_cmd_status(vdev, CMD_ENABLE2, status);
}

int vnic_dev_deinit_done(struct vnic_dev *vdev, int *status)
{
	return vnic_dev_cmd_status(vdev, CMD_DEINIT, status);
}

int vnic_dev_set_mac_addr(struct vnic_dev *vdev, u8 *mac_addr)
{
	u64 a0 = 0, a1 = 0;
	int wait = 1000;
	int i;

	for (i = 0; i < ETH_ALEN; i++)
		((u8 *)&a0)[i] = mac_addr[i];

	return vnic_dev_cmd(vdev, CMD_SET_MAC_ADDR, &a0, &a1, wait);
}

/*
 *  vnic_dev_classifier: Add/Delete classifier entries
 *  @vdev: vdev of the device
 *  @cmd: CLSF_ADD for Add filter
 *        CLSF_DEL for Delete filter
 *  @entry: In case of ADD filter, the caller passes the RQ number in this variable.
 *             This function stores the filter_id returned by the
 *             firmware in the same variable before return;
 *
 *            In case of DEL filter, the caller passes the RQ number. Return
 *            value is irrelevant.
 * @data: filter data
 */
int vnic_dev_classifier(struct vnic_dev *vdev, u8 cmd, u16 *entry, struct filter *data)
{
	u64 a0, a1;
	int wait = 1000;
	dma_addr_t tlv_pa;
	int ret = -EINVAL;
	struct filter_tlv *tlv, *tlv_va;
	struct filter_action *action;
	u64 tlv_size;

	if (cmd == CLSF_ADD) {
		tlv_size = sizeof(struct filter) +
			   sizeof(struct filter_action) +
			   2*sizeof(struct filter_tlv);
		tlv_va = pci_alloc_consistent(vdev->pdev, tlv_size, &tlv_pa);
		if (!tlv_va)
			return -ENOMEM;
		tlv = tlv_va;
		a0 = tlv_pa;
		a1 = tlv_size;
		memset(tlv, 0, tlv_size);
		tlv->type = CLSF_TLV_FILTER;
		tlv->length = sizeof(struct filter);
		*(struct filter *)&tlv->val = *data;

		tlv = (struct filter_tlv *)((char *)tlv +
					 sizeof(struct filter_tlv) +
					 sizeof(struct filter));

		tlv->type = CLSF_TLV_ACTION;
		tlv->length = sizeof (struct filter_action);
		action = (struct filter_action *)&tlv->val;
		action->type = FILTER_ACTION_RQ_STEERING;
		action->u.rq_idx = *entry;

		ret = vnic_dev_cmd(vdev, CMD_ADD_FILTER, &a0, &a1, wait);
		*entry = (u16)a0;
		pci_free_consistent(vdev->pdev, tlv_size, tlv_va, tlv_pa);
	} else if (cmd == CLSF_DEL) {
		a0 = *entry;
		ret = vnic_dev_cmd(vdev, CMD_DEL_FILTER, &a0, &a1, wait);
	}

	return ret;
}
#ifdef ENIC_VXLAN
int vnic_dev_overlay_offload_ctrl(struct vnic_dev *vdev, u8 overlay,
	u8 config)
{
	u64 a0, a1;
	int wait = 1000;
	int ret = -EINVAL;
	a0 = overlay;
	a1 = config;
	ret = vnic_dev_cmd(vdev, CMD_OVERLAY_OFFLOAD_CTRL,
		&a0, &a1, wait);

	return ret;
}

int vnic_dev_overlay_offload_cfg(struct vnic_dev *vdev, u8 overlay,
	u16 vxlan_udp_port_number)
{
	u64 a0, a1;
	int wait = 1000;
	int ret = -EINVAL;
	a0 = overlay;
	a1 = vxlan_udp_port_number;
	ret = vnic_dev_cmd(vdev, CMD_OVERLAY_OFFLOAD_CFG, &a0, &a1, wait);

	return ret;
}

int vnic_dev_get_supported_feature_ver(struct vnic_dev *vdev, u8 feature,
	u64 *supported_versions)
{
	u64 a0 = feature, a1 = 0;
	int wait = 1000;
	int ret = -EINVAL;
	ret = vnic_dev_cmd(vdev, CMD_GET_SUPP_FEATURE_VER, &a0, &a1, wait);
	if (!ret)
		*supported_versions = a0;
	return ret;
}
#endif
