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
#ident "$Id$"

#ifndef _VNIC_RESOURCE_H_
#define _VNIC_RESOURCE_H_

#define VNIC_RES_MAGIC		0x766E6963L	/* 'vnic' */
#define VNIC_RES_VERSION	0x00000000L
#define MGMTVNIC_MAGIC		0x544d474dL	/* 'MGMT' */
#define MGMTVNIC_VERSION	0x00000000L

/* The MAC address assigned to the CFG vNIC is fixed. */
#define MGMTVNIC_MAC		{ 0x02, 0x00, 0x54, 0x4d, 0x47, 0x4d }

/* vNIC resource types */
enum vnic_res_type {
	RES_TYPE_EOL,			/* End-of-list */
	RES_TYPE_WQ,			/* Work queues */
	RES_TYPE_RQ,			/* Receive queues */
	RES_TYPE_CQ,			/* Completion queues */
	RES_TYPE_MEM,			/* Window to dev memory */
	RES_TYPE_NIC_CFG,		/* Enet NIC config registers */
#ifndef NOT_FOR_OPEN_SOURCE
	RES_TYPE_RSS_KEY,		/* Enet RSS secret key */
	RES_TYPE_RSS_CPU,		/* Enet RSS indirection table */
	RES_TYPE_TX_STATS,		/* Netblock Tx statistic regs */
	RES_TYPE_RX_STATS,		/* Netblock Rx statistic regs */
#else
	RES_TYPE_RSVD2,
	RES_TYPE_RSVD3,
	RES_TYPE_RSVD4,
	RES_TYPE_RSVD5,
#endif
	RES_TYPE_INTR_CTRL,		/* Interrupt ctrl table */
	RES_TYPE_INTR_TABLE,		/* MSI/MSI-X Interrupt table */
	RES_TYPE_INTR_PBA,		/* MSI/MSI-X PBA table */
	RES_TYPE_INTR_PBA_LEGACY,	/* Legacy intr status */
#ifndef NOT_FOR_OPEN_SOURCE
	RES_TYPE_DEBUG,			/* Debug-only info */
	RES_TYPE_DEV,			/* Device-specific region */
#else
	RES_TYPE_RSVD6,
	RES_TYPE_RSVD7,
#endif
	RES_TYPE_DEVCMD,		/* Device command region */
	RES_TYPE_PASS_THRU_PAGE,	/* Pass-thru page */
	RES_TYPE_SUBVNIC,               /* subvnic resource type */
	RES_TYPE_MQ_WQ,                 /* MQ Work queues */
	RES_TYPE_MQ_RQ,                 /* MQ Receive queues */
	RES_TYPE_MQ_CQ,                 /* MQ Completion queues */
	RES_TYPE_DEPRECATED1,           /* Old version of devcmd 2 */
	RES_TYPE_DEVCMD2,               /* Device control region */
	RES_TYPE_MAX,			/* Count of resource types */
};

struct vnic_resource_header {
	u32 magic;
	u32 version;
};

struct mgmt_barmap_hdr {
	u32 magic;			/* magic number */
	u32 version;			/* header format version */
	u16 lif;			/* loopback lif for mgmt frames */
	u16 pci_slot;			/* installed pci slot */
	char serial[16];		/* card serial number */
};

struct vnic_resource {
	u8 type;
	u8 bar;
	u8 pad[2];
	u32 bar_offset;
	u32 count;
};

#endif /* _VNIC_RESOURCE_H_ */
