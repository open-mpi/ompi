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

#ifndef PML_OB1_CUSTOM_MATCH_FUZZY512_SHORT_H
#define PML_OB1_CUSTOM_MATCH_FUZZY512_SHORT_H

#include <immintrin.h>

#include "ompi/mca/pml/ob1/pml_ob1_recvfrag.h"
#include "ompi/mca/pml/ob1/pml_ob1_recvreq.h"

typedef struct custom_match_prq_node
{
    __m512i keys;
    __m512i mask;
    struct custom_match_prq_node* next;
    int start, end;
    void* value[16];
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
        for(i = elem->start; i <= elem->end; i++)
        {
            if(elem->value[i] == req)
            {
#if CUSTOM_MATCH_DEBUG_VERBOSE
                printf("Canceled!");// %x %x %x\n", req, req->req_tag, req->req_peer);
#endif
                ((int*)(&(elem->keys)))[i] = ~0;
                ((int*)(&(elem->mask)))[i] = ~0;
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
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_prq_find_verify list: %x tag: %x peer: %x\n", list, tag, peer);
#endif
    __mmask16 result = 0;
    custom_match_prq_node* elem = list->head;
    int i;
    int32_t key = peer;
    ((int8_t*)&key)[3] = (int8_t) tag; // MGFD TODO verify this set higher order bits...
    __m512i search = _mm512_set1_epi32(key);
    while(elem)
    {
        result = _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->keys, elem->mask), _mm512_and_epi32(search, elem->mask));
        if(result)
        {
            for(i = elem->start; i <= elem->end; i++)
            {
                if((0x1 << i & result) && elem->value[i])
                {
                    mca_pml_base_request_t *req = (mca_pml_base_request_t *)elem->value[i];
                    if((req->req_peer == peer || req->req_peer == OMPI_ANY_SOURCE) && (req->req_tag == tag || req->req_tag == OMPI_ANY_TAG))
                    {
#if CUSTOM_MATCH_DEBUG_VERBOSE
                        printf("Found list: %x tag: %x peer: %x\n", list, req->req_tag, req->req_peer);
#endif
                        return elem->value[i];
                    }
                }
            }
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
    __mmask16 result = 0;
    custom_match_prq_node* prev = 0;
    custom_match_prq_node* elem = list->head;
    int i;
    int32_t key = peer;
    ((int8_t*)&key)[3] = (int8_t) tag; // MGFD TODO verify this set higher order bits...
    __m512i search = _mm512_set1_epi32(key);
    while(elem)
    {
#if CUSTOM_MATCH_DEBUG_VERBOSE
        for(int iter = elem->start; iter <= elem->end; iter++)
        {
            printf("Search = %x, Element Key = %x, Element mask = %x", ((int32_t*) &search)[iter], ((int32_t*) &elem->keys)[iter], ((int32_t*) &elem->mask)[iter]);
        }
#endif
        result = _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->keys, elem->mask), _mm512_and_epi32(search, elem->mask));
        if(result)
        {
            for(i = elem->start; i <= elem->end; i++)
            {
                mca_pml_base_request_t *req = (mca_pml_base_request_t *)elem->value[i];
                if((0x1 << i & result) && req && ((req->req_peer == peer || req->req_peer == OMPI_ANY_SOURCE) && (req->req_tag == tag || req->req_tag == OMPI_ANY_TAG)))
                {
                    void* payload = elem->value[i];
                    ((int*)(&(elem->keys)))[i] = ~0;
                    ((int*)(&(elem->mask)))[i] = ~0;
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
#if CUSTOM_MATCH_DEBUG_VERBOSE
                    printf("Found list: %x tag: %x peer: %x\n", list, req->req_tag, req->req_peer);
#endif
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
    int32_t key, mask;
    if(source == OMPI_ANY_SOURCE)
    {
        key = source;
        mask = 0;
    }
    else
    {
        key = source;
        mask = ~0;
    }
    if(tag == OMPI_ANY_TAG)
    {
        ((int8_t*)&key)[3] = (int8_t)tag;
        ((int8_t*)&mask)[3] = (int8_t)0;
    }
    else
    {
        ((int8_t*)&key)[3] = (int8_t)tag;
        ((int8_t*)&mask)[3] = (int8_t)~0;
    }
#if CUSTOM_MATCH_DEBUG_VERBOSE
    mca_pml_base_request_t *req = (mca_pml_base_request_t *)payload;
    printf("custom_match_prq_append list: %x key: %x mask: %x tag: %x peer: %x\n", list, key, mask, req->req_tag, req->req_peer);
#endif
    int i;
    custom_match_prq_node* elem;
    if((!list->tail) || list->tail->end == 15)
    {
        if(list->pool)
        {
            elem = list->pool;
            list->pool = list->pool->next;
        }
        else
        {
            elem = _mm_malloc(sizeof(custom_match_prq_node),64);
        }
        elem->keys = _mm512_set1_epi32(~0);
        elem->mask = _mm512_set1_epi32(~0);
        elem->next = 0;
        elem->start = 0;
        elem->end = -1; // we don't have an element yet
        for(i = 0; i < 16; i++) elem->value[i] = 0;
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
    ((int*)(&(elem->keys)))[elem->end] = key;
    ((int*)(&(elem->mask)))[elem->end] = mask;
    elem->value[elem->end] = payload;
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
    custom_match_prq* list = _mm_malloc(sizeof(custom_match_prq),64);
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
    while(list->head)
    {
        elem = list->head;
        list->head = list->head->next;
        _mm_free(elem);
    }
    while(list->pool)
    {
        elem = list->pool;
        list->pool = list->pool->next;
        _mm_free(elem);
    }
    _mm_free(list);
}

static inline void custom_match_print(custom_match_prq* list)
{
    custom_match_prq_node* elem;
    int i = 0;
    int j = 0;
    printf("Elements in the list:\n");
    for(elem = list->head; elem; elem = elem->next)
    {
        printf("This is the %d linked list element\n", ++i);
        for(j = 0; j < 16; j++)
        {
            printf("%d:%d The key is %d, the mask is %d, the value is %lu\n", i, j, ((int*)(&(elem->keys)))[j], ((int*)(&(elem->mask)))[j], (uintptr_t) elem->value[j]);
        }
        i++;
    }
}

static inline void custom_match_prq_dump(custom_match_prq* list)
{
    char cpeer[64], ctag[64];

    custom_match_prq_node* elem;
    int i = 0;
    int j = 0;
    printf("Elements in the list:\n");
    for(elem = list->head; elem; elem = elem->next)
    {
        printf("This is the %d linked list element\n", ++i);
        for(j = 0; j < 16; j++)
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

typedef struct custom_match_umq_node
{
    __m512i keys;
    struct custom_match_umq_node* next;
    int start, end;
    void* value[16];
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
    __mmask16 result = 0;
    custom_match_umq_node* prev = 0;
    custom_match_umq_node* elem = list->head;
    int i;
    int32_t key = peer;
    ((int8_t*)&key)[3] = (int8_t) tag; // MGFD TODO verify this set higher order bits...
    __m512i search = _mm512_set1_epi32(key);

    int32_t mask = ~0;
    if(peer == OMPI_ANY_SOURCE)
    {
        mask = 0;
    }
    else
    {
        mask = ~0;
    }
    if(tag == OMPI_ANY_TAG)
    {
        ((int8_t*)&mask)[3] = (int8_t)0;
    }
    else
    {
        ((int8_t*)&mask)[3] = (int8_t)~0;
    }
    __m512i msearch = _mm512_set1_epi32(mask);
    search = _mm512_and_epi32(search, msearch);

    while(elem)
    {
        result = _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->keys,msearch), search);
        if(result)
        {
            for(i = elem->start; i <= elem->end; i++)
            {
                if((0x1 << i & result) && elem->value[i])
                {
                    mca_pml_ob1_recv_frag_t *req = (mca_pml_ob1_recv_frag_t *)elem->value[i];
                    if((req->hdr.hdr_match.hdr_src == peer || peer == OMPI_ANY_SOURCE) && (req->hdr.hdr_match.hdr_tag == tag || tag == OMPI_ANY_TAG))
                    {
#if CUSTOM_MATCH_DEBUG_VERBOSE
                        printf("Found list: %x tag: %x peer: %x\n", list, req->hdr.hdr_match.hdr_tag, req->hdr.hdr_match.hdr_src);
#endif
                        *hold_prev = prev;
                        *hold_elem = elem;
                        *hold_index = i;
                        return elem->value[i];
                    }
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
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_umq_find_remove_hold %x %x %x\n", prev, elem, i);
#endif
    ((int*)(&(elem->keys)))[i] = ~0;
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
    int32_t key = source;
    ((int8_t*)&key)[3] = (int8_t) tag; // MGFD TODO verify this set higher order bits...
#if CUSTOM_MATCH_DEBUG_VERBOSE
    mca_pml_ob1_recv_frag_t *req = (mca_pml_ob1_recv_frag_t *)payload;
    printf("custom_match_umq_append list: %x key: %x payload: %x tag: %d src: %d\n", list, key, payload, req->hdr.hdr_match.hdr_tag, req->hdr.hdr_match.hdr_src);
#endif
    int i;
    custom_match_umq_node* elem;
    list->size++;
    if((!list->tail) || list->tail->end == 15)
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
            elem = _mm_malloc(sizeof(custom_match_umq_node),64);
        }
        elem->keys = _mm512_set1_epi32(~0); // TODO: we only have to do this type of initialization for freshly malloc'd entries.
        elem->next = 0;
        elem->start = 0;
        elem->end = -1; // we don't have an element yet
        for(i = 0; i < 16; i++) elem->value[i] = 0;
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
    }

    elem = list->tail;
    elem->end++;
    ((int*)(&(elem->keys)))[elem->end] = key;
    elem->value[elem->end] = payload;
#if CUSTOM_MATCH_DEBUG_VERBOSE
    custom_match_umq_dump(list);
#endif
}

static inline custom_match_umq* custom_match_umq_init()
{
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_umq_init\n");
#endif
    custom_match_umq* list = _mm_malloc(sizeof(custom_match_umq),64);
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
    while(list->head)
    {
        elem = list->head;
        list->head = list->head->next;
        _mm_free(elem);
    }
    while(list->pool)
    {
        elem = list->pool;
        list->pool = list->pool->next;
        _mm_free(elem);
    }
    _mm_free(list);
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
        for(j = 0; j < 16; j++)
        {
            if(elem->value[j])
            {
                mca_pml_ob1_recv_frag_t *req = (mca_pml_ob1_recv_frag_t *)elem->value[j];
                printf("%lx %x %x\n", (uintptr_t) elem->value[j], req->hdr.hdr_match.hdr_tag, req->hdr.hdr_match.hdr_src);
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
