/**
 * axon_ioctl - provides an io control interface to the axon driver
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * Copyright (C) IBM Corporation, 2008
 *
 * Authors: H Brett Bolen <hbbolen@us.ibm.com>,
 *	    Tim Schimke	  <tschimke@us.ibm.com>,
 *	    Jesse Arroyo  <arroyoj@us.ibm.com>,
 *	    Murali N Iyer <mniyer@us.ibm.com>
 *
 */


#ifndef __AXON_IOCTL_H__
#define __AXON_IOCTL_H__

#define AXONIO_IOC_MAGIC     'x'

/* DMA Memory Registration */
#define AXONIO_DMA_REGISTER	_IOWR(AXONIO_IOC_MAGIC, 0x7,  \
				      struct AXON_MR_registration)
#define AXONIO_DMA_DEREGISTER	_IOWR(AXONIO_IOC_MAGIC, 0x8,  \
				      struct AXON_MR_deregistration)
#define AXONIO_DMA_EXTREGISTER	_IOWR(AXONIO_IOC_MAGIC, 0x10,  \
				      struct AXON_MR_ext_registration)
#define AXONIO_ISSUE_DMA_FAST _IOWR(AXONIO_IOC_MAGIC, 0x11, __u32)

/* Wakeup Notificaton of remote system */
#define AXONIO_NOTIFY	       _IOR(AXONIO_IOC_MAGIC, 0x13, __u32)


/*
 * mmap offsets
 */
#define LOCAL_SMA_OFFSET   0x0
#define REMOTE_SMA_OFFSET  0x0100000
#define DMA_COMMAND_BUFFER_OFFSET  0x0800000


/*
 * opaque handles
 */
typedef	 __u64 AXON_memory_region_handle;

/**
 *  Supports Memory Registration
 *     AXON_DMA_REGISTER
 *     AXON_DMA_DEREGISTER
 *
 *    permissions bitmask
 *
 *	  0x01 - allow local access ( always true)
 *	  0x02 - allow local read
 *	  0x04 - allow local write
 *	  0x10 - allow remote access
 *	  0x20 - allow remote read
 *	  0x40 - allow remote write
 */

enum {
	AXON_MR_LOCAL_ACCESS   = 0x00000001,
	AXON_MR_LOCAL_READ     = 0x00000002,
	AXON_MR_LOCAL_WRITE    = 0x00000004,
	AXON_MR_REMOTE_ACCESS  = 0x00000010,
	AXON_MR_REMOTE_READ    = 0x00000020,
	AXON_MR_REMOTE_WRITE   = 0x00000040,
};


struct AXON_MR_registration{
	AXON_memory_region_handle	     memory_region_handle;
	__u64					local_dma_memory;
	__u64				     local_dma_memory_size;
	__u64				     permissions;
};

struct AXON_MR_deregistration{
	AXON_memory_region_handle	    memory_region_handle;
};

struct AXON_MR_ext_registration{
	AXON_memory_region_handle	    memory_region_handle;
	__u64 				    permissions;
};

/**
 *  Supports DMA GET/PUT status queries
 *
 *  NOTE: AXON_dma_request required to be within SMA area
 */

struct AXON_dma_list_entry {
	AXON_memory_region_handle     src_memory_region_handle;
	__u64			      src_address;
	__u64			      transfer_size;
	/* total size 0x18 */
};

enum {
	AXON_DMATYPE_PUT  = 0x01, /* dma local to remote */
	AXON_DMATYPE_GET  = 0x02, /* dma remote to local */
};

enum {
	AXON_DMAFLAG_WRITE_REMOTE_STATUS     = 0x00000001,
	AXON_DMAFLAG_LOCAL_COMPLETION_SIGNAL = 0x00000002,
};

struct	AXON_dma_request {
	__u32		    dma_type;
	__u32		    flags;
	__u32		    localDmaStatusOffset;
	__u32		    remoteDmaStatusOffset;
	__u64		    transfer_size; /* bytes */
	__u32		    local_descriptor_count;
	__u32		    remote_descriptor_count;
	__u64		    rsvd1;
	struct AXON_dma_list_entry   local_descriptor[10];
	struct AXON_dma_list_entry   remote_descriptor[10];
};
struct AXON_dma_command_list_fast {
	__u32 dma_requests_available;
	__u32 dma_requests_started;
	__u32 dma_req_offset; /* offset into command block mmap area */
};

/**
 * Wakeup Notificaton
 */
struct AXON_WAKEUP {
	__u32    type;
};


#endif /* __AXON_IOCTL_H__ */
