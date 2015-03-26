/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef _FI_EXT_USNIC_H_
#define _FI_EXT_USNIC_H_

/*
 * See the fi_usnic.7 man page for information about the usnic provider
 * extensions provided in this header.
 */

#include <stdint.h>
#include <net/if.h>

#define FI_PROTO_RUDP 100

#define FI_EXT_USNIC_INFO_VERSION 1

/*
 * usNIC specific info
 */
struct fi_usnic_info_v1 {
	uint32_t ui_link_speed;
	uint32_t ui_netmask_be;
	char ui_ifname[IFNAMSIZ];

	uint32_t ui_num_vf;
	uint32_t ui_qp_per_vf;
	uint32_t ui_cq_per_vf;
};

struct fi_usnic_info {
	uint32_t ui_version;
	union {
		struct fi_usnic_info_v1 v1;
	} ui;
};

/*
 * usNIC-specific fabric ops
 */
#define FI_USNIC_FABRIC_OPS_1 "fabric_ops 1"
struct fi_usnic_ops_fabric {
	size_t size;
	int (*getinfo)(uint32_t version, struct fid_fabric *fabric,
				struct fi_usnic_info *info);
};

/*
 * usNIC-specific AV ops
 */
#define FI_USNIC_AV_OPS_1 "av_ops 1"
struct fi_usnic_ops_av {
	size_t size;
	int (*get_distance)(struct fid_av *av, void *addr, int *metric);
};

#endif /* _FI_EXT_USNIC_H_ */
