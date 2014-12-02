/*
 * Copyright (c) 2014 Intel Corporation, Inc.  All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * OpenIB.org BSD license below:
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

#ifndef _LIST_H_
#define _LIST_H_

#include "fi.h"

typedef struct _list_t list_t;
typedef struct _list_element_t
{
	void *data;
	size_t len;
	list_t *list;
	struct _list_element_t *next;
}list_element_t;

struct _list_t
{
	list_element_t *head, *tail;
	list_element_t *free_head, *free_tail;
	size_t curr_len;
	size_t max_len;
	fastlock_t lock;
};

list_t *new_list(size_t length);
void free_list(list_t *list);

int enqueue_item(list_t *list, void *item);
void *peek_item(list_t *list);
void *dequeue_item(list_t *list);
int find_item(list_t *list, void *item);
int delete_item(list_t *list, void *item);
ssize_t list_length(list_t *list);

#endif /* _LIST_H_ */
