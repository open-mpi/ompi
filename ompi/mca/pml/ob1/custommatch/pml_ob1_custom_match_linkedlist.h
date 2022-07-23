/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Sandia National Laboratories.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_OB1_CUSTOM_MATCH_LINKEDLIST_H
#define PML_OB1_CUSTOM_MATCH_LINKEDLIST_H

#include "../pml_ob1_recvreq.h"
#include "../pml_ob1_recvfrag.h"

typedef struct custom_match_prq_node
{
    int tag;
    int tmask;
    int src;
    int smask;
    struct custom_match_prq_node* next;
    void* value;
} custom_match_prq_node;

typedef struct custom_match_prq
{
    custom_match_prq_node* head;
    custom_match_prq_node* tail;
    custom_match_prq_node* pool;
    int size;
} custom_match_prq;

static inline int custom_match_prq_cancel(custom_match_prq* list, void* req)
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_prq_cancel - list: %x req: %x\n", list, req);
#endif
    custom_match_prq_node* prev = 0;
    custom_match_prq_node* elem = list->head;
    int i;
    while(elem)
    {
        if(elem->value == req)
        {
            //    printf("Canceled!");// %x %x %x\n", req, req->req_tag, req->req_peer);
            elem->tag = ~0;
            elem->tmask = ~0;
            elem->src = ~0;
            elem->smask = ~0;
            elem->value = 0;
            if(prev)
            {
                prev->next = elem->next;
            }
            else
            {
                list->head = elem->next;
            }
            if(!elem->next)
            {
                list->tail = prev;
            }
            elem->next = list->pool;
            list->pool = elem;
            list->size--;
            return 1;
        }
        prev = elem;
        elem = elem->next;
    }
    return 0;
}

static inline void* custom_match_prq_find_verify(custom_match_prq* list, int tag, int peer)
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_prq_find_verify list: %x tag: %x peer: %x\n", list, tag, peer);
#endif
    custom_match_prq_node* elem = list->head;
    int result;

    while(elem)
    {
        result = ((elem->tag & elem->tmask) == (tag & elem->tmask)) &&
            ((elem->src & elem->smask) == (peer & elem->smask));
        if(result)
        {
            return elem->value;
        }
        elem = elem->next;
    }
    return 0;
}

static inline void* custom_match_prq_find_dequeue_verify(custom_match_prq* list, int tag, int peer)
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_prq_find_dequeue_verify list: %x:%d tag: %x peer: %x\n", list, list->size, tag, peer);
#endif
    custom_match_prq_node* prev = 0;
    custom_match_prq_node* elem = list->head;
    int result;

    while(elem)
    {
        result = ((elem->tag & elem->tmask) == (tag & elem->tmask)) &&
            ((elem->src & elem->smask) == (peer & elem->smask));
        if(result)
        {
            void* payload = elem->value;
            elem->tag = ~0;
            elem->tmask = ~0;
            elem->src = ~0;
            elem->smask = ~0;
            elem->value = 0;
            if(prev)
            {
                prev->next = elem->next;
            }
            else
            {
                list->head = elem->next;
            }
            if(!elem->next)
            {
                list->tail = prev;
            }
            elem->next = list->pool;
            list->pool = elem;
#if CUSTOM_MATCH_DEBUG_VERBOSE
            printf("%x == %x added to the pool\n", elem, list->pool);
#endif
            list->size--;
            mca_pml_base_request_t *req = (mca_pml_base_request_t *)payload;
#if CUSTOM_MATCH_DEBUG_VERBOSE
            printf("Found list: %x tag: %x peer: %x\n", list, req->req_tag, req->req_peer);
#endif
            return payload;
        }
        prev = elem;
        elem = elem->next;
    }
    return 0;
}


static inline void custom_match_prq_append(custom_match_prq* list, void* payload, int tag, int source)
{
    int32_t mask_tag, mask_src;
    if(source == OMPI_ANY_SOURCE)
    {
        mask_src = 0;
    }
    else
    {
        mask_src = ~0;
    }
    if(tag == OMPI_ANY_TAG)
    {
        mask_tag = 0;
    }
    else
    {
        mask_tag = ~0;
    }
    mca_pml_base_request_t *req = (mca_pml_base_request_t *)payload;
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_prq_append list: %x tag: %x source: %x tag: %x peer: %x\n", list, tag, source, req->req_tag, req->req_peer);
#endif
    int i;
    custom_match_prq_node* elem;
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("%x next elem in the pool\n", list->pool);
#endif
    if(list->pool)
    {
        elem = list->pool;
        list->pool = list->pool->next;
    }
    else
    {
        elem = malloc(sizeof(custom_match_prq_node));
    }
    elem->next = 0;
    if(list->tail)
    {
        list->tail->next = elem;
        list->tail = elem;
    }
    else
    {
        list->head = elem;
        list->tail = elem;
    }

    elem = list->tail;
    elem->tag = tag;
    elem->tmask = mask_tag;
    elem->src = source;
    elem->smask = mask_src;
    elem->value = payload;
    list->size++;
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("Exiting custom_match_prq_append\n");
#endif
}

static inline int custom_match_prq_size(custom_match_prq* list)
{
    return list->size;
}

static inline custom_match_prq* custom_match_prq_init()
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_prq_init\n");
#endif
    custom_match_prq* list = malloc(sizeof(custom_match_prq));
    list->head = 0;
    list->tail = 0;
    list->pool = 0;
    list->size = 0;
    return list;
}

static inline void custom_match_prq_destroy(custom_match_prq* list)
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_prq_destroy\n");
#endif
    custom_match_prq_node* elem;
    int i = 0;
    int j = 0;
    while(list->head)
    {
        elem = list->head;
        list->head = list->head->next;
        free(elem);
        i++;
    }
    while(list->pool)
    {
        elem = list->pool;
        list->pool = list->pool->next;
        free(elem);
        j++;
    }
    free(list);
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("Number of prq elements destroyed = %d %d\n", i, j);
#endif
}

static inline void custom_match_print(custom_match_prq* list)
{
    custom_match_prq_node* elem;
    int i = 0;
    int j = 0;
    printf("Elements in the list (this is currently only partially implemented):\n");
    for(elem = list->head; elem; elem = elem->next)
    {
        printf("This is the %d linked list element\n", ++i);
        printf("%d The key is %d, the mask is %d, the value is %ld\n", i, elem->tag, elem->tmask, elem->value);
        i++;
    }
}

static inline void custom_match_prq_dump(custom_match_prq* list)
{
    opal_list_item_t* item;
    char cpeer[64], ctag[64];

    custom_match_prq_node* elem;
    int i = 0;
    int j = 0;
    printf("Elements in the list:\n");
    for(elem = list->head; elem; elem = elem->next)
    {
        printf("This is the %d linked list element\n", ++i);
        if(elem->value)
        {
            mca_pml_base_request_t *req = (mca_pml_base_request_t *)elem->value;
            if( OMPI_ANY_SOURCE == req->req_peer ) snprintf(cpeer, 64, "%s", "ANY_SOURCE");
            else snprintf(cpeer, 64, "%d", req->req_peer);
            if( OMPI_ANY_TAG == req->req_tag ) snprintf(ctag, 64, "%s", "ANY_TAG");
            else snprintf(ctag, 64, "%d", req->req_tag);
            opal_output(0, "req %p peer %s tag %s addr %p count %lu datatype %s [%p] [%s %s] req_seq %" PRIu64,
                        (void*) req, cpeer, ctag,
                        (void*) req->req_addr, req->req_count,
                        (0 != req->req_count ? req->req_datatype->name : "N/A"),
                        (void*) req->req_datatype,
                        (req->req_pml_complete ? "pml_complete" : ""),
                        (req->req_free_called ? "freed" : ""),
                        req->req_sequence);

        }
    }
}


// UMQ below.

typedef struct custom_match_umq_node
{
    int tag;
    int src;
    struct custom_match_umq_node* next;
    void* value;
} custom_match_umq_node;

typedef struct custom_match_umq
{
    custom_match_umq_node* head;
    custom_match_umq_node* tail;
    custom_match_umq_node* pool;
    int size;
} custom_match_umq;

static inline void custom_match_umq_dump(custom_match_umq* list);

static inline void* custom_match_umq_find_verify_hold(custom_match_umq* list, int tag, int peer, custom_match_umq_node** hold_prev, custom_match_umq_node** hold_elem, int* hold_index)
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_umq_find_verify_hold list: %x:%d tag: %x peer: %x\n", list, list->size, tag, peer);
    custom_match_umq_dump(list);
#endif
    custom_match_umq_node* prev = 0;
    custom_match_umq_node* elem = list->head;
    int result;

    int tmask = ~0;
    int smask = ~0;
    if(peer == OMPI_ANY_SOURCE)
    {
        smask = 0;
    }

    if(tag == OMPI_ANY_TAG)
    {
        tmask = 0;
    }

    tag = tag & tmask;
    peer = peer & smask;

    while(elem)
    {
        result = ((elem->tag & tmask) == tag) &&
            ((elem->src & smask) == peer);
        if(result)
        {
#if CUSTOM_MATCH_DEBUG_VERBOSE
            printf("Found list: %x tag: %x peer: %x\n", list, tag, peer);
#endif
            *hold_prev = prev;
            *hold_elem = elem;
            *hold_index = 0;
            return elem->value;
        }
        prev = elem;
        elem = elem->next;
    }
    return 0;
}


static inline void custom_match_umq_remove_hold(custom_match_umq* list, custom_match_umq_node* prev, custom_match_umq_node* elem, int i)
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_umq_find_remove_hold %x %x %x\n", prev, elem, i);
#endif
    elem->tag = ~0;
    elem->src = ~0;
    elem->value = 0;
    if(prev)
    {
        prev->next = elem->next;
    }
    else
    {
        list->head = elem->next;
    }
    if(!elem->next)
    {
        list->tail = prev;
    }
    elem->next = list->pool;
    list->pool = elem;
    list->size--;
}

static inline void custom_match_umq_append(custom_match_umq* list, int tag, int source, void* payload)
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_umq_append list: %x payload: %x tag: %d src: %d\n", list,  payload, tag, source);
#endif
    int i;
    custom_match_umq_node* elem;
    list->size++;
    if(list->pool)
    {
#if CUSTOM_MATCH_DEBUG_VERBOSE
        printf("Grab an element from the pool\n");
#endif
        elem = list->pool;
        list->pool = list->pool->next;
    }
    else
    {
#if CUSTOM_MATCH_DEBUG_VERBOSE
        printf("Make a new element\n");
#endif
        elem = malloc(sizeof(custom_match_umq_node));
    }
    elem->next = 0;
    if(list->tail)
    {
#if CUSTOM_MATCH_DEBUG_VERBOSE
        printf("Append to list of elems\n");
#endif
        list->tail->next = elem;
        list->tail = elem;
    }
    else
    {
#if CUSTOM_MATCH_DEBUG_VERBOSE
        printf("New Elem is only Elem\n");
#endif
        list->head = elem;
        list->tail = elem;
    }

    elem = list->tail;
    elem->tag = tag;
    elem->src = source;
    elem->value = payload;
#if CUSTOM_MATCH_DEBUG_VERBOSE
    custom_match_umq_dump(list);
#endif
}

static inline custom_match_umq* custom_match_umq_init()
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_umq_init\n");
#endif
    custom_match_umq* list = malloc(sizeof(custom_match_umq));
    list->head = 0;
    list->tail = 0;
    list->pool = 0;
    list->size = 0;
    return list;
}

static inline void custom_match_umq_destroy(custom_match_umq* list)
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_umq_destroy\n");
#endif
    custom_match_umq_node* elem;
    int i = 0;
    int j = 0;
    while(list->head)
    {
        elem = list->head;
        list->head = list->head->next;
        free(elem);
        i++;
    }
    while(list->pool)
    {
        elem = list->pool;
        list->pool = list->pool->next;
        free(elem);
        j++;
    }
    free(list);
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("Number of umq elements destroyed = %d %d\n", i, j);
#endif
}

static inline int custom_match_umq_size(custom_match_umq* list)
{
    return list->size;
}

static inline void custom_match_umq_dump(custom_match_umq* list)
{
    char cpeer[64], ctag[64];

    custom_match_umq_node* elem;
    int i = 0;
    int j = 0;
    printf("Elements in the list:\n");
    for(elem = list->head; elem; elem = elem->next)
    {
        printf("This is the %d linked list element\n", ++i);
        if(elem->value)
        {
            mca_pml_ob1_recv_frag_t *req = (mca_pml_ob1_recv_frag_t *)elem->value;
            printf("%x %x %x\n", elem->value, req->hdr.hdr_match.hdr_tag, req->hdr.hdr_match.hdr_src);
            if( OMPI_ANY_SOURCE == req->hdr.hdr_match.hdr_src ) snprintf(cpeer, 64, "%s", "ANY_SOURCE");
            else snprintf(cpeer, 64, "%d", req->hdr.hdr_match.hdr_src);
            if( OMPI_ANY_TAG == req->hdr.hdr_match.hdr_tag ) snprintf(ctag, 64, "%s", "ANY_TAG");
            else snprintf(ctag, 64, "%d", req->hdr.hdr_match.hdr_tag);
            // opal_output(0, "peer %s tag %s",// addr %p count %lu datatype %s [%p] [%s %s] req_seq %" PRIu64,
            //         /*(void*) req,*/ cpeer, ctag,
            //(void*) req->req_addr, req->req_count,
            //(0 != req->req_count ? req->req_datatype->name : "N/A"),
            //(void*) req->req_datatype,
            //(req->req_pml_complete ? "pml_complete" : ""),
            //(req->req_free_called ? "freed" : ""),
            //req->req_sequence);
            //           );

        }
    }
}

#endif
