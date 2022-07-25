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

#ifndef PML_OB1_CUSTOM_MATCH_ARRAYS_H
#define PML_OB1_CUSTOM_MATCH_ARRAYS_H

#include <immintrin.h>

#include "../pml_ob1_recvreq.h"
#include "../pml_ob1_recvfrag.h"

#define PRQ_SIZE 2

typedef struct custom_match_prq_node
{
    int32_t tags[PRQ_SIZE];
    int32_t tmask[PRQ_SIZE];
    int32_t srcs[PRQ_SIZE];
    int32_t smask[PRQ_SIZE];
    struct custom_match_prq_node* next;
    int8_t start, end;
    void* value[PRQ_SIZE];
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
#if CUSTOM_MATCH_DEBUG
    printf("custom_match_prq_cancel - list: %x req: %x\n", list, req);
#endif
    custom_match_prq_node* prev = 0;
    custom_match_prq_node* elem = list->head;
    int i;
    while(elem)
    {
        for(i = elem->start; i <= elem->end; i++)
        {
            if(elem->value[i] == req)
            {
#if CUSTOM_MATCH_DEBUG
                printf("Canceled!");// %x %x %x\n", req, req->req_tag, req->req_peer);
#endif
                elem->tags[i] = ~0;
                elem->tmask[i] = ~0;
                elem->srcs[i] = ~0;
                elem->smask[i] = ~0;
                elem->value[i] = 0;
                if(i == elem->start || i == elem->end)
                {
                    while((elem->start <= elem->end) && (!(elem->value[elem->start]))) elem->start++;
                    while((elem->start <= elem->end) && (!(elem->value[elem->end])))   elem->end--;
                    if(elem->start > elem->end)
                    {
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
                    }
                }
                list->size--;
                return 1;
            }
        }
        prev = elem;
        elem = elem->next;
    }
    return 0;
}

static inline void* custom_match_prq_find_verify(custom_match_prq* list, int tag, int peer)
{
    int result;
#if CUSTOM_MATCH_DEBUG
    // printf("custom_match_prq_find_verify list: %x tag: %x peer: %x\n", list, tag, peer);
#endif
    custom_match_prq_node* elem = list->head;
    int i;

    while(elem)
    {
        for(i = elem->start; i <= elem->end; i++)
        {
            if(elem->value[i])
            {
                result = ((elem->tags[i] & elem->tmask[i]) ==  (tag & elem->tmask[i])) && ((elem->srcs[i] & elem->smask[i]) == (peer & elem->smask[i]));
                if(result)
                {
                    return elem->value[i];
                }
            }
            elem = elem->next;
        }
    }
    return 0;
}

static inline void* custom_match_prq_find_dequeue_verify(custom_match_prq* list, int tag, int peer)
{
    int result;
#if CUSTOM_MATCH_DEBUG
    // printf("custom_match_prq_find_dequeue_verify list: %x:%d tag: %x peer: %x\n", list, list->size, tag, peer);
#endif
    custom_match_prq_node* prev = 0;
    custom_match_prq_node* elem = list->head;
    int i;
    while(elem)
    {
        for(i = elem->start; i <= elem->end; i++)
        {
            if(elem->value[i])
            {
                result = ((elem->tags[i] & elem->tmask[i]) ==  (tag & elem->tmask[i])) && ((elem->srcs[i] & elem->smask[i]) == (peer & elem->smask[i]));
                if(result)
                {
                    void* payload = elem->value[i];
                    elem->tags[i] = ~0;
                    elem->tmask[i] = ~0;
                    elem->srcs[i] = ~0;
                    elem->smask[i] = ~0;
                    elem->value[i] = 0;
                    if(i == elem->start || i == elem->end)
                    {
                        while((elem->start <= elem->end) && (!(elem->value[elem->start]))) elem->start++;
                        while((elem->start <= elem->end) && (!(elem->value[elem->end])))   elem->end--;
                        if(elem->start > elem->end)
                        {
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
                        }
                    }
                    list->size--;
                    //printf("Found list: %x tag: %x peer: %x\n", list, req->req_tag, req->req_peer);
                    return payload;
                }
            }
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
#if CUSTOM_MATCH_DEBUG
    printf("custom_match_prq_append list: %x mask: %x tag: %x peer: %x\n", list, mask_tag, tag, source);
#endif
    int i;
    custom_match_prq_node* elem;
    if((!list->tail) || list->tail->end == PRQ_SIZE-1)
    {
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
        elem->start = 0;
        elem->end = -1; // we don't have an element yet
        for(i = 0; i < PRQ_SIZE; i++)
        {
            elem->value[i] = 0;
            elem->tags[i] = ~0; // TODO: we only have to do this type of initialization for freshly malloc'd entries.
            elem->tmask[i] = ~0;
            elem->srcs[i] = ~0;
            elem->smask[i] = ~0;
        }

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
    }

    elem = list->tail;
    elem->end++;
    elem->tags[elem->end] = tag;
    elem->tmask[elem->end] = mask_tag;
    elem->srcs[elem->end] = source;
    elem->smask[elem->end] = mask_src;
    elem->value[elem->end] = payload;
    list->size++;
#if CUSTOM_MATCH_DEBUG
    printf("Exiting custom_match_prq_append\n");
#endif
}

static inline int custom_match_prq_size(custom_match_prq* list)
{
    return list->size;
}

static inline custom_match_prq* custom_match_prq_init()
{
#if CUSTOM_MATCH_DEBUG
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
#if CUSTOM_MATCH_DEBUG
    printf("custom_match_prq_destroy\n");
#endif
    custom_match_prq_node* elem;
    while(list->head)
    {
        elem = list->head;
        list->head = list->head->next;
        free(elem);
    }
    while(list->pool)
    {
        elem = list->pool;
        list->pool = list->pool->next;
        free(elem);
    }
    free(list);
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
        for(j = 0; j < PRQ_SIZE; j++)
        {
            printf("%d:%d The key is %d, the mask is %d, the value is %ld\n", i, j, elem->tags[j], elem->tmask[j], elem->value[j]);
        }
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
        for(j = 0; j < PRQ_SIZE; j++)
        {
            if(elem->value[j])
            {
                mca_pml_base_request_t *req = (mca_pml_base_request_t *)elem->value[j];
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
}


// UMQ below.

#define UMQ_SIZE 3

typedef struct custom_match_umq_node
{
    int32_t tags[UMQ_SIZE];
    int32_t srcs[UMQ_SIZE];
    struct custom_match_umq_node* next;
    int8_t start, end;
    void* value[UMQ_SIZE];
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
    int result;
#if CUSTOM_MATCH_DEBUG
    printf("custom_match_umq_find_verify_hold list: %x:%d tag: %x peer: %x\n", list, list->size, tag, peer);
#if CUSTOM_MATCH_DEBUG_VERBOSE
    custom_match_umq_dump(list);
#endif
#endif
    custom_match_umq_node* prev = 0;
    custom_match_umq_node* elem = list->head;
    int i;

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
        for(i = elem->start; i <= elem->end; i++)
        {
            if(elem->value[i])
            {
                result = (tag == (elem->tags[i] & tmask))  && (peer == (elem->srcs[i] & smask));
                if(result)
                {
                    *hold_prev = prev;
                    *hold_elem = elem;
                    *hold_index = i;
                    return elem->value[i];
                }
            }
        }
        prev = elem;
        elem = elem->next;
    }
    return 0;
}


static inline void custom_match_umq_remove_hold(custom_match_umq* list, custom_match_umq_node* prev, custom_match_umq_node* elem, int i)
{
#if CUSTOM_MATCH_DEBUG
    printf("custom_match_umq_find_remove_hold %x %x %x\n", prev, elem, i);
#endif
    elem->tags[i] = ~0;
    elem->srcs[i] = ~0;
    elem->value[i] = 0;
    if(i == elem->start || i == elem->end)
    {
        while((elem->start <= elem->end) && (!(elem->value[elem->start]))) elem->start++;
        while((elem->start <= elem->end) && (!(elem->value[elem->end])))   elem->end--;
        if(elem->start > elem->end)
        {
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
        }
    }
    list->size--;
}

static inline void custom_match_umq_append(custom_match_umq* list, int tag, int source, void* payload)
{
#if CUSTOM_MATCH_DEBUG
    printf("custom_match_umq_append list: %x payload: %x tag: %d src: %d\n", list, payload, tag, source);
#endif
    int i;
    custom_match_umq_node* elem;
    list->size++;
    if((!list->tail) || list->tail->end == UMQ_SIZE-1)
    {
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
        elem->start = 0;
        elem->end = -1; // we don't have an element yet
        for(i = 0; i < UMQ_SIZE; i++)
        {
            elem->tags[i] = 0;
            elem->srcs[i] = 0;
            elem->value[i] = 0;
        }
        if(list->tail)
        {
            //printf("Append to list of elems\n");
            list->tail->next = elem;
            list->tail = elem;
        }
        else
        {
            //printf("New Elem is only Elem\n");
            list->head = elem;
            list->tail = elem;
        }
    }

    elem = list->tail;
    elem->end++;
    elem->tags[elem->end] = tag;
    elem->srcs[elem->end] = source;
    elem->value[elem->end] = payload;
#if CUSTOM_MATCH_DEBUG_VERBOSE
    custom_match_umq_dump(list);
#endif
}

static inline custom_match_umq* custom_match_umq_init()
{
#if CUSTOM_MATCH_DEBUG
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
#if CUSTOM_MATCH_DEBUG
    printf("custom_match_umq_destroy\n");
#endif
    custom_match_umq_node* elem;
    while(list->head)
    {
        elem = list->head;
        list->head = list->head->next;
        free(elem);
    }
    while(list->pool)
    {
        elem = list->pool;
        list->pool = list->pool->next;
        free(elem);
    }
    free(list);
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
        for(j = 0; j < UMQ_SIZE; j++)
        {
            if(elem->value[j])
            {
                mca_pml_ob1_recv_frag_t *req = (mca_pml_ob1_recv_frag_t *)elem->value[j];
                printf("%x %x %x\n", elem->value[j], req->hdr.hdr_match.hdr_tag, req->hdr.hdr_match.hdr_src);
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
}

#endif
