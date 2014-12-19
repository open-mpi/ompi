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
#ifndef _USDF_TIMER_H_
#define _USDF_TIMER_H_

#include <time.h>

struct usdf_timer_entry;

static inline uint64_t
usdf_get_ms(void)
{
	struct timespec now;
	uint64_t ms;

	clock_gettime(CLOCK_MONOTONIC, &now);
	ms = now.tv_sec * 1000 + now.tv_nsec / 1000000;

	return ms;
}

typedef void (*usdf_timer_callback_t)(void *);

int usdf_timer_alloc(usdf_timer_callback_t cb, void *arg,
		struct usdf_timer_entry **entry);

void usdf_timer_free(struct usdf_fabric *fp, struct usdf_timer_entry *entry);

int usdf_timer_set(struct usdf_fabric *fp, struct usdf_timer_entry *entry,
		uint32_t timeout);

void usdf_timer_cancel(struct usdf_fabric *fp, struct usdf_timer_entry *entry);

void usdf_timer_progress(struct usdf_fabric *fp);

int usdf_timer_init(struct usdf_fabric *fp);
void usdf_timer_deinit(struct usdf_fabric *fp);

#endif /* _USDF_TIMER_H_ */
