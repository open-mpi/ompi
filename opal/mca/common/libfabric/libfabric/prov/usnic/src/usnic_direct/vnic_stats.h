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

#ifndef _VNIC_STATS_H_
#define _VNIC_STATS_H_

/* Tx statistics */
struct vnic_tx_stats {
	u64 tx_frames_ok;
	u64 tx_unicast_frames_ok;
	u64 tx_multicast_frames_ok;
	u64 tx_broadcast_frames_ok;
	u64 tx_bytes_ok;
	u64 tx_unicast_bytes_ok;
	u64 tx_multicast_bytes_ok;
	u64 tx_broadcast_bytes_ok;
	u64 tx_drops;
	u64 tx_errors;
	u64 tx_tso;
	u64 rsvd[16];
};

/* Rx statistics */
struct vnic_rx_stats {
	u64 rx_frames_ok;
	u64 rx_frames_total;
	u64 rx_unicast_frames_ok;
	u64 rx_multicast_frames_ok;
	u64 rx_broadcast_frames_ok;
	u64 rx_bytes_ok;
	u64 rx_unicast_bytes_ok;
	u64 rx_multicast_bytes_ok;
	u64 rx_broadcast_bytes_ok;
	u64 rx_drop;
	u64 rx_no_bufs;
	u64 rx_errors;
	u64 rx_rss;
	u64 rx_crc_errors;
	u64 rx_frames_64;
	u64 rx_frames_127;
	u64 rx_frames_255;
	u64 rx_frames_511;
	u64 rx_frames_1023;
	u64 rx_frames_1518;
	u64 rx_frames_to_max;
	u64 rsvd[16];
};

#ifndef __VMKLNX__
/* Generic statistics */
struct vnic_gen_stats {
	u64 dma_map_error;
};
#endif

struct vnic_stats {
	struct vnic_tx_stats tx;
	struct vnic_rx_stats rx;
};

#endif /* _VNIC_STATS_H_ */
