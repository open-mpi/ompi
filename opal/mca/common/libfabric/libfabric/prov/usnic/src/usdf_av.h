/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
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
 */
#ifndef _USDF_AV_H_
#define _USDF_AV_H_

#include "usd_dest.h"

#define USDF_AV_MAX_ARPS 3
#define USDF_AV_ARP_INTERVAL 1000

struct usdf_rdm_connection;

/*
 * libfabric version of dest
 */
struct usdf_dest {
	struct usd_dest ds_dest;

	SLIST_HEAD(,usdf_rdm_connection) ds_rdm_rdc_list;
};

/* struct used to track async insert requests */
struct usdf_av_req {
	fi_addr_t *avr_fi_addr;
	struct usdf_dest *avr_dest;
	int avr_status;

	uint32_t avr_daddr_be;

	TAILQ_ENTRY(usdf_av_req) avr_link;
};

struct usdf_av_insert {
	struct usdf_av *avi_av;
	void *avi_context;

	struct usdf_timer_entry *avi_timer;

	uint32_t avi_successes;
	TAILQ_HEAD(,usdf_av_req) avi_req_list;
	uint32_t avi_arps_left;
	uint64_t avi_last_arp_time;
};

struct usdf_av {
	struct fid_av av_fid;
	struct usdf_domain *av_domain;
	uint64_t av_flags;
	struct usdf_eq *av_eq;
	atomic_t av_refcnt;
	int av_closing;
	atomic_t av_active_inserts;
	pthread_spinlock_t av_lock;
};
#define av_ftou(FAV) container_of(FAV, struct usdf_av, av_fid)
#define av_fidtou(FID) container_of(FID, struct usdf_av, av_fid.fid)
#define av_utof(AV) (&(AV)->av_fid)

#endif /* _USDF_AV_H_ */
