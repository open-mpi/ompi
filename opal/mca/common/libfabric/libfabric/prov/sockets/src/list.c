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

#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "list.h"

#define LIST_DEF_NUM_ENTRIES (128)

#define ENQUEUE_LIST(_head, _tail, _elem) do{		\
		(_elem)->next = NULL;			\
		if(NULL == (_head)){			\
			(_head) = (_tail) = (_elem);	\
		}else{					\
			(_tail)->next = (_elem);	\
		}					\
	}while(0)

#define DEQUEUE_LIST(_head, _tail, _elem) do{	\
		if(NULL == _head){		\
			_elem = NULL;		\
		}else{				\
			_elem = _head;		\
			_head = _head->next;	\
			if(_head == NULL)	\
				_tail = NULL;	\
		}				\
	}while(0)

static int _list_enqueue(list_element_t *element)
{
	if(!element)
		return -1;
	ENQUEUE_LIST(element->list->head,
		     element->list->tail, element);
	return 0;
}

static list_element_t *_list_dequeue(list_t *list)
{
	list_element_t *element;
	DEQUEUE_LIST(list->head, list->tail, element);
	return element;
}

static int _list_enqueue_free_list(list_element_t *element)
{
	if(!element)
		return -1;
	ENQUEUE_LIST(element->list->free_head,
		     element->list->free_tail, element);
	return 0;
}

static list_element_t *_list_dequeue_free_list(list_t *list)
{
	list_element_t *element;
	DEQUEUE_LIST(list->free_head, list->free_tail, element);
	return element;
}

list_t *new_list(size_t length)
{
	int i;
	list_t *list = (list_t *)malloc(sizeof(list_t) + 
			      length * sizeof(list_element_t));

	memset(list, 0, sizeof(list_t) + 
	       length * sizeof(list_element_t));

	list->curr_len = 0;
	list->max_len = length;
	list->head = list->tail = NULL;
	list->free_head = list->free_tail = NULL;

	if(0 != fastlock_init(&(list->lock)))
		goto err;

	list_element_t *elements = (list_element_t *)
		((char*)list + sizeof(list_t));

	for(i=0; i<length; i++){
		list_element_t *element = (list_element_t *)((char *)elements + 
							     sizeof(list_element_t) * i);
		element->list = list;
		if(0 != _list_enqueue_free_list(element))
			goto err1;
	}
	return list;

err1:
	fastlock_destroy(&(list->lock));

err:
	free(list);
	return NULL;
}

void free_list(list_t *list)
{
	fastlock_destroy(&(list->lock));
	free((void *)list);
}

int enqueue_item(list_t *list, void *data)
{
	int ret;
	fastlock_acquire(&(list->lock));
	list_element_t *elem = _list_dequeue_free_list(list);
	if(!elem){
		int i;
		list_element_t *elements;

		if(list->curr_len == list->max_len){
			list = realloc(list, 
				       sizeof(list_t) + list->max_len * sizeof(list_element_t) +
				       sizeof(list_element_t) * LIST_DEF_NUM_ENTRIES);
			if(!list){
				fastlock_release(&(list->lock));
				return -1;
			}

			elements = (list_element_t *) ((char*)list + sizeof(list_t) + 
						       sizeof(list_element_t) * list->max_len);
			memset(elements, 0, sizeof(list_element_t) * 
			       LIST_DEF_NUM_ENTRIES);

			for(i=0; i<LIST_DEF_NUM_ENTRIES; i++){
				list_element_t *element = (list_element_t *)((char *)elements + 
									     sizeof(list_element_t) * i);
				if(0 != _list_enqueue_free_list(element)){
					fastlock_release(&(list->lock));
					return -1;
				}
			}
			list->max_len += LIST_DEF_NUM_ENTRIES;
			elem = _list_dequeue_free_list(list);
			if(!elem){
				fastlock_release(&(list->lock));
				return -1;
			}
		}
	}
	
	elem->next = NULL;
	elem->data = data;
	elem->len = 0;
	ret = _list_enqueue(elem);
	if(!ret)
		list->curr_len++;
	fastlock_release(&(list->lock));
	return ret;
}

void *dequeue_item(list_t *list)
{
	fastlock_acquire(&(list->lock));
	if(list->curr_len > 0){
		void *data;
		list_element_t *element = _list_dequeue(list);
		
		list->curr_len--;
		data = element->data;
		_list_enqueue_free_list(element);
		fastlock_release(&(list->lock));
		return data;
	}
	fastlock_release(&(list->lock));
	return NULL;
}

void *peek_item(list_t *list)
{
	fastlock_acquire(&(list->lock));
	if(list->curr_len > 0){
		list_element_t *element = _list_dequeue(list);
		fastlock_release(&(list->lock));
	        return element->data;
	}
	fastlock_release(&(list->lock));
	return NULL;
}

int delete_item(list_t *list, void *item)
{
	fastlock_acquire(&(list->lock));
	list_element_t *curr;
	list_element_t *prev = NULL;
	
	for(curr = list->head; curr != NULL; curr = curr->next){
		if(curr->data == item) {
			if(prev == NULL) {
				list->head = curr->next;
			} else {
				prev->next = curr->next;
			}
			
			if(list->tail == curr)
				list->tail = NULL;

			_list_enqueue_free_list(curr);
			list->curr_len--;
			fastlock_release(&(list->lock));
			return 0;
		}
		prev = curr;
	}
	fastlock_release(&(list->lock));
	return -1;
}

int find_item(list_t *list, void *item)
{
	fastlock_acquire(&(list->lock));
	list_element_t *curr = list->head;

	while(curr){
		if(curr->data == item){
			fastlock_release(&(list->lock));
			return 0;
		}
		curr=curr->next;
	}
	fastlock_release(&(list->lock));
	return -1;
}

ssize_t list_length(list_t *list)
{
	ssize_t len;
	fastlock_acquire(&(list->lock));
	len = list->curr_len;
	fastlock_release(&(list->lock));
	return len;
}
