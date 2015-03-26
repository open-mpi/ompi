/*
 * Copyright (c) 2011 Intel Corporation.  All rights reserved.
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
 *
 */

#if !defined(LIST_H)
#define LIST_H

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#include <fi.h>
#include <rdma/fi_errno.h>

/*
 * Double-linked list
 */
struct dlist_entry {
	struct dlist_entry	*next;
	struct dlist_entry	*prev;
};

static inline void dlist_init(struct dlist_entry *head)
{
	head->next = head;
	head->prev = head;
}

static inline int dlist_empty(struct dlist_entry *head)
{
	return head->next == head;
}

static inline void
dlist_insert_after(struct dlist_entry *item, struct dlist_entry *head)
{
	item->next = head->next;
	item->prev = head;
	head->next->prev = item;
	head->next = item;
}

static inline void
dlist_insert_before(struct dlist_entry *item, struct dlist_entry *head)
{
	dlist_insert_after(item, head->prev);
}

#define dlist_insert_head dlist_insert_after
#define dlist_insert_tail dlist_insert_before

static inline void dlist_remove(struct dlist_entry *item)
{
	item->prev->next = item->next;
	item->next->prev = item->prev;
}


typedef int dlist_match_func_t(struct dlist_entry *item, const void *arg);

static inline struct dlist_entry *
dlist_remove_first_match(struct dlist_entry *head, dlist_match_func_t *match,
			 const void *arg)
{
	struct dlist_entry *item;

	for (item = head->next; item != head; item = item->next) {
		if (match(item, arg)) {
			dlist_remove(item);
			return item;
		}
	}

	return NULL;
}

/*
 * Single-linked list
 */
struct slist_entry {
	struct slist_entry	*next;
};

struct slist {
	struct slist_entry	*head;
	struct slist_entry	*tail;
};

static inline void slist_init(struct slist *list)
{
	list->head = NULL;
}

static inline int slist_empty(struct slist *list)
{
	return !list->head;
}

static inline void slist_insert_head(struct slist_entry *item, struct slist *list)
{
	if (slist_empty(list))
		list->tail = item;
	else
		item->next = list->head;

	list->head = item;
}

static inline void slist_insert_tail(struct slist_entry *item, struct slist *list)
{
	if (slist_empty(list))
		list->head = item;
	else
		list->tail->next = item;

	list->tail = item;
}

static inline struct slist_entry *slist_remove_head(struct slist *list)
{
	struct slist_entry *item;

	item = list->head;
	if (list->head == list->tail)
		slist_init(list);
	else
		list->head = item->next;
	return item;
}

typedef int slist_match_func_t(struct slist_entry *item, const void *arg);

static inline struct slist_entry *
slist_remove_first_match(struct slist *list, slist_match_func_t *match, const void *arg)
{
	struct slist_entry *item, *prev;

	for (prev = NULL, item = list->head; item; prev = item, item = item->next) {
		if (match(item, arg)) {
			if (prev)
				prev->next = item->next;
			else
				list->head = item->next;

			if (!item->next)
				list->tail = prev;

			return item;
		}
	}

	return NULL;
}

/*
 * Double-linked list with blocking wait-until-avail support
 */

enum {
	LIST_READ_FD = 0,
	LIST_WRITE_FD
};

struct dlistfd_head {
	struct dlist_entry list;
	int		fdrcnt;
	int		fdwcnt;
	int		fd[2];
};

static inline int dlistfd_head_init(struct dlistfd_head *head)
{
	int ret;

	dlist_init(&head->list);

	ret = socketpair(AF_UNIX, SOCK_STREAM, 0, head->fd);
	if (ret < 0)
		return -errno;

	ret = fcntl(head->fd[LIST_READ_FD], F_SETFL, O_NONBLOCK);
	if (ret < 0)
		goto err;

	return 0;

err:
	close(head->fd[0]);
	close(head->fd[1]);
	return -errno;
}

static inline void dlistfd_head_free(struct dlistfd_head *head)
{
	close(head->fd[0]);
	close(head->fd[1]);
}

static inline int dlistfd_empty(struct dlistfd_head *head)
{
	return dlist_empty(&head->list);
}

static inline void dlistfd_signal(struct dlistfd_head *head)
{
	if (head->fdwcnt == head->fdrcnt) {
		if (write(head->fd[LIST_WRITE_FD], head, sizeof head) == sizeof head)
			head->fdwcnt++;
	}
}

static inline void dlistfd_reset(struct dlistfd_head *head)
{
	void *buf;
	if (dlistfd_empty(head) && (head->fdrcnt < head->fdwcnt)) {
		if (read(head->fd[LIST_READ_FD], &buf, sizeof buf) == sizeof buf)
			head->fdrcnt++;
	}
}

static inline void
dlistfd_insert_head(struct dlist_entry *item, struct dlistfd_head *head)
{
	dlist_insert_after(item, &head->list);
	dlistfd_signal(head);
}

static inline void
dlistfd_insert_tail(struct dlist_entry *item, struct dlistfd_head *head)
{
	dlist_insert_before(item, &head->list);
	dlistfd_signal(head);
}

static inline void dlistfd_remove(struct dlist_entry *item, struct dlistfd_head *head)
{
	dlist_remove(item);
	dlistfd_reset(head);
}

static inline int dlistfd_wait_avail(struct dlistfd_head *head, int timeout)
{
	int ret;

	if(!dlistfd_empty(head))
		return 1;
	
	ret = fi_poll_fd(head->fd[LIST_READ_FD], timeout);
	if(ret < 0)
		return ret;

	return (ret == 0) ? -FI_ETIMEDOUT : !dlistfd_empty(head);
}

#endif /* LIST_H */
