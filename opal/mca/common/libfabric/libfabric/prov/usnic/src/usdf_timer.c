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

#include <stdlib.h>
#include <time.h>
#include <sys/queue.h>

#include "rdma/fabric.h"
#include "rdma/fi_errno.h"
#include "fi.h"

#include "usnic_direct.h"

#include "usdf.h"
#include "usdf_timer.h"

enum {
	USDF_TF_QUEUED = (1 << 0),
	USDF_TF_ACTIVE = (1 << 1),
	USDF_TF_FREED = (1 << 2)
};

#define USDF_NUM_TIMER_BUCKETS (16 * 1024) /* roughly 16 seconds max delay */

struct usdf_timer_entry {
	struct usdf_fabric *te_fabric;

	usdf_timer_callback_t te_callback;
	void *te_context;

	uint32_t te_flags;
	LIST_ENTRY(usdf_timer_entry) te_link;
};

/*
 * Create a timer entry, registering a callback and argument.
 */
int
usdf_timer_alloc(usdf_timer_callback_t cb, void *context,
		struct usdf_timer_entry **entry_o)
{
	struct usdf_timer_entry *entry;

	entry = calloc(1, sizeof(*entry));
	if (entry == NULL) {
		return -FI_ENOMEM;
	}

	entry->te_callback = cb;
	entry->te_context = context;
	entry->te_flags = 0;

	*entry_o = entry;
	return 0;
}

void
usdf_timer_free(struct usdf_fabric *fp, struct usdf_timer_entry *entry)
{
	pthread_spin_lock(&fp->fab_timer_lock);
	
	if (entry->te_flags & USDF_TF_ACTIVE) {
		entry->te_flags |= USDF_TF_FREED;
	} else {
		if (entry->te_flags & USDF_TF_QUEUED) {
			LIST_REMOVE(entry, te_link);
		}
		free(entry);
	}

	pthread_spin_unlock(&fp->fab_timer_lock);
}

void
usdf_timer_cancel(struct usdf_fabric *fp, struct usdf_timer_entry *entry)
{
	pthread_spin_lock(&fp->fab_timer_lock);

	if (entry->te_flags & USDF_TF_QUEUED) {
		LIST_REMOVE(entry, te_link);
		entry->te_flags &= ~USDF_TF_QUEUED;
		--fp->fab_active_timer_count;
	}

	pthread_spin_unlock(&fp->fab_timer_lock);
}

/*
 * Set this timer to fire "ms" milliseconds from now.  If the timer is already
 * queued, previous timeout will be discarded.
 *
 * When timer expires, the registered timer callback will be called and
 * the timer entry removed from the queued list.  The timer routine will not
 * be called again until usdf_timer_set() is called again to re-set it.
 * usdf_timer_set() is safe to call from timer service routine.
 */
int
usdf_timer_set(struct usdf_fabric *fp, struct usdf_timer_entry *entry,
		uint32_t ms)
{
	int ret;
	unsigned bucket;

	pthread_spin_lock(&fp->fab_timer_lock);

	/* If no timers active, cur_bucket_ms may need catchup */
	if (fp->fab_active_timer_count == 0) {
		fp->fab_cur_bucket_ms = usdf_get_ms();
		ret = usdf_fabric_wake_thread(fp);
		if (ret != 0) {
			goto out;
		}
	}

	if (entry->te_flags & USDF_TF_QUEUED) {
		LIST_REMOVE(entry, te_link);
		--fp->fab_active_timer_count;
	}

	// we could make "overflow" bucket...
	if (ms >= USDF_NUM_TIMER_BUCKETS) {
		ret = -FI_EINVAL;
		goto out;
	}
	bucket = (fp->fab_cur_bucket + ms) & (USDF_NUM_TIMER_BUCKETS - 1);

	LIST_INSERT_HEAD(&fp->fab_timer_buckets[bucket], entry, te_link);
	entry->te_flags |= USDF_TF_QUEUED;
	++fp->fab_active_timer_count;
	ret = 0;

out:
	pthread_spin_unlock(&fp->fab_timer_lock);
	return ret;
}

static inline void
usdf_run_bucket(struct usdf_fabric *fp, struct usdf_timer_bucket *bp)
{
	struct usdf_timer_entry *entry;

	while (!LIST_EMPTY(bp)) {
		entry = LIST_FIRST(bp);
		LIST_REMOVE(entry, te_link);
		entry->te_flags |= USDF_TF_ACTIVE;
		entry->te_flags &= ~USDF_TF_QUEUED;
		--fp->fab_active_timer_count;

		/* call timer service routine without lock */
		pthread_spin_unlock(&fp->fab_timer_lock);
		entry->te_callback(entry->te_context);
		pthread_spin_lock(&fp->fab_timer_lock);
	}
}

/*
 * Called only from fabric progression thread
 */
void
usdf_timer_progress(struct usdf_fabric *fp)
{
	pthread_spin_lock(&fp->fab_timer_lock);

	while (fp->fab_cur_bucket_ms < usdf_get_ms()) {
		usdf_run_bucket(fp,
			&fp->fab_timer_buckets[fp->fab_cur_bucket]);

		++fp->fab_cur_bucket_ms;
		fp->fab_cur_bucket = (fp->fab_cur_bucket + 1) &
			(USDF_NUM_TIMER_BUCKETS - 1);
	}

	pthread_spin_unlock(&fp->fab_timer_lock);
}

/*
 * Initialize timer data
 */
int
usdf_timer_init(struct usdf_fabric *fp)
{
	int i;

	pthread_spin_init(&fp->fab_timer_lock, PTHREAD_PROCESS_PRIVATE);

	fp->fab_timer_buckets = calloc(USDF_NUM_TIMER_BUCKETS,
			sizeof(struct usdf_timer_bucket));
	if (fp->fab_timer_buckets == NULL) {
		return -FI_ENOMEM;
	}

	for (i = 0; i < USDF_NUM_TIMER_BUCKETS; ++i) {
		LIST_INIT(&fp->fab_timer_buckets[i]);
	}

	fp->fab_cur_bucket = 0;
	fp->fab_cur_bucket_ms = usdf_get_ms();
	return 0;
}

void
usdf_timer_deinit(struct usdf_fabric *fp)
{
	if (fp->fab_timer_buckets != NULL) {
		free(fp->fab_timer_buckets);
	}
}
