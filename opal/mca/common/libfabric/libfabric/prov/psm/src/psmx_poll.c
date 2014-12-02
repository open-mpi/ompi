/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
 *
 * This software is waitailable to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, waitailable from the file
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

int psmx_poll_add(struct fid_poll *pollset, struct fid *event_fid, uint64_t flags)
{

	struct psmx_fid_poll *poll_priv;
	struct psmx_poll_list *list_item;

	poll_priv = container_of(pollset, struct psmx_fid_poll, poll.fid);

	list_item = calloc(1, sizeof(*list_item));
	if (!list_item)
		return -FI_ENOMEM;

	list_item->fid = event_fid;
	dlist_insert_after(&list_item->entry, &poll_priv->poll_list_head);
	
	return 0;
}

int psmx_poll_del(struct fid_poll *pollset, struct fid *event_fid, uint64_t flags)
{
	struct psmx_fid_poll *poll_priv;
	struct psmx_poll_list *list_item;
	struct dlist_entry *p, *head;

	poll_priv = container_of(pollset, struct psmx_fid_poll, poll.fid);

	head = &poll_priv->poll_list_head;
	for (p = head->next; p != head; p = p->next) {
		list_item = container_of(p, struct psmx_poll_list, entry);
		if (list_item->fid == event_fid) {
			dlist_remove(p);
			free(list_item);
			break;
		}
	}

	return 0;
}

static int psmx_poll_poll(struct fid_poll *pollset, void **context, int count)
{
	struct psmx_fid_poll *poll_priv;
	int err = 0;
	
	poll_priv = container_of(pollset, struct psmx_fid_poll, poll.fid);

	/* TODO: poll them all! */

	return err;
}

static int psmx_poll_close(fid_t fid)
{
	struct psmx_fid_poll *poll;
	struct psmx_poll_list *list_item;
	struct dlist_entry *p, *head;

	poll = container_of(fid, struct psmx_fid_poll, poll.fid);

	head = &poll->poll_list_head;
	while (!dlist_empty(head)) {
		p = head->next;
		list_item = container_of(p, struct psmx_poll_list, entry);
		dlist_remove(p);
		free(list_item);
	}

	free(poll);
	return 0;
}

static struct fi_ops psmx_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_poll_close,
	.bind = fi_no_bind,
	.sync = fi_no_sync,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_poll psmx_poll_ops = {
	.size = sizeof(struct fi_ops_poll),
	.poll = psmx_poll_poll,
};

int psmx_poll_open(struct fid_domain *domain, struct fi_poll_attr *attr,
		   struct fid_poll **pollset)
{
	struct psmx_fid_domain *domain_priv;
	struct psmx_fid_poll *poll_priv;

	domain_priv = container_of(domain, struct psmx_fid_domain, domain);

	poll_priv = calloc(1, sizeof(*poll_priv));
	if (!poll_priv)
		return -FI_ENOMEM;
	
	dlist_init(&poll_priv->poll_list_head);
	poll_priv->poll.fid.fclass = FI_CLASS_POLL;
	poll_priv->poll.fid.context = 0;
	poll_priv->poll.fid.ops = &psmx_fi_ops;
	poll_priv->poll.ops = &psmx_poll_ops;

	*pollset = &poll_priv->poll;
	return 0;
}

