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

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/epoll.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"

#include "usnic_direct.h"
#include "usdf.h"
#include "usdf_progress.h"
#include "usdf_timer.h"

int
usdf_fabric_wake_thread(struct usdf_fabric *fp)
{
	uint64_t val;
	int n;

	val = 1;
	n = write(fp->fab_eventfd, &val, sizeof(val));
	if (n != sizeof(val)) {
		return -FI_EIO;
	}
	return 0;
}

int
usdf_fabric_progression_cb(void *v)
{
	struct usdf_fabric *fp;
	uint64_t val;
	int n;

	fp = v;
	n = read(fp->fab_eventfd, &val, sizeof(val));
	if (n != sizeof(val)) {
		return -FI_EIO;
	}
	return 0;
}

void *
usdf_fabric_progression_thread(void *v)
{
	struct usdf_fabric *fp;
	struct epoll_event ev;
	struct usdf_poll_item *pip;
	int sleep_time;
	int epfd;
	int ret;
	int n;

	fp = v;
	epfd = fp->fab_epollfd;

	while (1) {

		/* sleep inifinitely if nothing to do */
		if (fp->fab_active_timer_count > 0) {
			sleep_time = 1;
		} else {
			sleep_time = -1;
		}

		n = epoll_wait(epfd, &ev, 1, sleep_time);
		if (n == -1) {
			pthread_exit(NULL);
		}

		/* consume event if there was one */
		if (n == 1) {
			pip = ev.data.ptr;
			ret = pip->pi_rtn(pip->pi_context);
			if (ret != 0) {
				pthread_exit(NULL);
			}
		}

		/* call timer progress each wakeup */
		usdf_timer_progress(fp);

		if (fp->fab_exit) {
			pthread_exit(NULL);
		}
	}
}
