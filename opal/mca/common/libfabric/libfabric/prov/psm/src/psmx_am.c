/*
 * Copyright (c) 2013 Intel Corporation. All rights reserved.
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

#include "psmx.h"

struct psm_am_parameters psmx_am_param;

static psm_am_handler_fn_t psmx_am_handlers[3] = {
	psmx_am_rma_handler,
	psmx_am_msg_handler,
	psmx_am_atomic_handler,
};

static int psmx_am_handlers_idx[3];
static int psmx_am_handlers_initialized = 0;

int psmx_am_progress(struct psmx_fid_domain *domain)
{
	struct slist_entry *item;
	struct psmx_am_request *req;

#if PSMX_AM_USE_SEND_QUEUE
	pthread_mutex_lock(&domain->send_queue.lock);
	while (!slist_empty(&domain->send_queue.list)) {
		item = slist_remove_head(&domain->send_queue.list);
		req = container_of(item, struct psmx_am_request, list_entry);
		if (req->state == PSMX_AM_STATE_DONE) {
			free(req);
		}
		else {
			pthread_mutex_unlock(&domain->send_queue.lock);
			psmx_am_process_send(domain, req);
			pthread_mutex_lock(&domain->send_queue.lock);
		}
	}
	pthread_mutex_unlock(&domain->send_queue.lock);
#endif

	if (psmx_env.tagged_rma) {
		pthread_mutex_lock(&domain->rma_queue.lock);
		while (!slist_empty(&domain->rma_queue.list)) {
			item = slist_remove_head(&domain->rma_queue.list);
			req = container_of(item, struct psmx_am_request, list_entry);
			pthread_mutex_unlock(&domain->rma_queue.lock);
			psmx_am_process_rma(domain, req);
			pthread_mutex_lock(&domain->rma_queue.lock);
		}
		pthread_mutex_unlock(&domain->rma_queue.lock);
	}

	return 0;
}

#if PSMX_AM_USE_SEND_QUEUE
static void *psmx_am_async_progress(void *args)
{
	struct psmx_fid_domain *domain = args;
	struct timespec timeout;

	timeout.tv_sec = 1;
	timeout.tv_nsec = 1000;

	pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
	pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

	while (1) {
		pthread_mutex_lock(&domain->progress_mutex);
		pthread_cond_wait(&domain->progress_cond, &domain->progress_mutex);
		pthread_mutex_unlock(&domain->progress_mutex);
		pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);

		psmx_am_progress(domain);

		pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
	}

	return NULL;
}
#endif

int psmx_am_init(struct psmx_fid_domain *domain)
{
	psm_ep_t psm_ep = domain->psm_ep;
	size_t size;
	int err = 0;

	if (!psmx_am_handlers_initialized) {
		err = psm_am_get_parameters(psm_ep, &psmx_am_param,
						sizeof(psmx_am_param), &size);
		if (err)
			return psmx_errno(err);

		err = psm_am_register_handlers(psm_ep, psmx_am_handlers, 3,
						psmx_am_handlers_idx);
		if (err)
			return psmx_errno(err);

		assert(psmx_am_handlers_idx[0] == PSMX_AM_RMA_HANDLER);
		assert(psmx_am_handlers_idx[1] == PSMX_AM_MSG_HANDLER);
		assert(psmx_am_handlers_idx[2] == PSMX_AM_ATOMIC_HANDLER);

		psmx_am_handlers_initialized = 1;
	}

	slist_init(&domain->rma_queue.list);
	slist_init(&domain->recv_queue.list);
	slist_init(&domain->unexp_queue.list);
	pthread_mutex_init(&domain->rma_queue.lock, NULL);
	pthread_mutex_init(&domain->recv_queue.lock, NULL);
	pthread_mutex_init(&domain->unexp_queue.lock, NULL);
#if PSMX_AM_USE_SEND_QUEUE
	slist_init(&domain->send_queue.list);
	pthread_mutex_init(&domain->send_queue.lock, NULL);
	pthread_mutex_init(&domain->progress_mutex, NULL);
	pthread_cond_init(&domain->progress_cond, NULL);
	err = pthread_create(&domain->progress_thread, NULL, psmx_am_async_progress, (void *)domain);
#endif

	return err;
}

int psmx_am_fini(struct psmx_fid_domain *domain)
{
#if PSMX_AM_USE_SEND_QUEUE
        if (domain->progress_thread) {
                pthread_cancel(domain->progress_thread);
                pthread_join(domain->progress_thread, NULL);
		pthread_mutex_destroy(&domain->progress_mutex);
		pthread_cond_destroy(&domain->progress_cond);
        }
#endif

	return 0;
}

