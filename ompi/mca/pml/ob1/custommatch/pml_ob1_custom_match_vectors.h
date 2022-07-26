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

#ifndef PML_OB1_CUSTOM_MATCH_VECTORS_H
#define PML_OB1_CUSTOM_MATCH_VECTORS_H

#include <immintrin.h>

#include "../pml_ob1_recvreq.h"
#include "../pml_ob1_recvfrag.h"

typedef struct custom_match_prq_node
{
    __m512i tags;
    __m512i tmask;
    __m512i srcs;
    __m512i smask;
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
    printf("custom_match_prq_cancel - list: %p req: %p\n", (void *) list, req);
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
                ((int*)(&(elem->tags)))[i] = ~0;
                ((int*)(&(elem->tmask)))[i] = ~0;
                ((int*)(&(elem->srcs)))[i] = ~0;
                ((int*)(&(elem->smask)))[i] = ~0;
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
    printf("custom_match_prq_find_verify list: %p tag: %x peer: %x\n", (void *) list, tag, peer);
#endif
    __mmask16 result = 0;
    custom_match_prq_node* elem = list->head;
    int i;
    __m512i tsearch = _mm512_set1_epi32(tag);
    __m512i ssearch = _mm512_set1_epi32(peer);

    while(elem)
    {
        result = _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->tags, elem->tmask), _mm512_and_epi32(tsearch, elem->tmask)) &
            _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->srcs, elem->smask), _mm512_and_epi32(ssearch, elem->smask));
        if(result)
        {
            for(i = elem->start; i <= elem->end; i++)
            {
                if((0x1 << i & result) && elem->value[i])
                {
#if CUSTOM_MATCH_DEBUG_VERBOSE
                    mca_pml_base_request_t *req = (mca_pml_base_request_t *)elem->value[i];
                    printf("Found list: %p tag: %x peer: %x\n", (void *) list, req->req_tag, req->req_peer);
#endif
                    return elem->value[i];
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
    printf("custom_match_prq_find_dequeue_verify list: %p:%d tag: %x peer: %x\n", (void *) list, list->size, tag, peer);
#endif
    __mmask16 result = 0;
    custom_match_prq_node* prev = 0;
    custom_match_prq_node* elem = list->head;
    int i;
    __m512i tsearch = _mm512_set1_epi32(tag);
    __m512i ssearch = _mm512_set1_epi32(peer);
    while(elem)
    {
#if CUSTOM_MATCH_DEBUG_VERBOSE
        for(int iter = elem->start; iter <= elem->end; iter++)
        {
            //printf("Search = %x, Element Key = %x, Element mask = %x", ((int32_t*) &search)[iter], ((int32_t*) &elem->keys)[iter], ((int32_t*) &elem->mask)[iter]);
        }
#endif
        result = _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->tags, elem->tmask), _mm512_and_epi32(tsearch, elem->tmask)) &
            _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->srcs, elem->smask), _mm512_and_epi32(ssearch, elem->smask));
        if(result)
        {
            for(i = elem->start; i <= elem->end; i++)
            {
                mca_pml_base_request_t *req = (mca_pml_base_request_t *)elem->value[i];
                if((0x1 << i & result) && req && ((req->req_peer == peer || req->req_peer == OMPI_ANY_SOURCE) && (req->req_tag == tag || req->req_tag == OMPI_ANY_TAG)))
                {
                    void* payload = elem->value[i];
                    ((int*)(&(elem->tags)))[i] = ~0;
                    ((int*)(&(elem->tmask)))[i] = ~0;
                    ((int*)(&(elem->srcs)))[i] = ~0;
                    ((int*)(&(elem->smask)))[i] = ~0;
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
                    printf("Found list: %p tag: %x peer: %x\n", (void *) list, req->req_tag, req->req_peer);
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
#if CUSTOM_MATCH_DEBUG_VERBOSE
    mca_pml_base_request_t *req = (mca_pml_base_request_t *)payload;
    printf("custom_match_prq_append list: %p mask_src: %x mask_tag: %x tag: %x peer: %x\n", (void *) list,
           mask_src, mask_tag, req->req_tag, req->req_peer);
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
            //if(!elem)
            //{
            // printf("Error: Couldn't create memory\n");
            //}
        }
        elem->tags = _mm512_set1_epi32(~0); // TODO: we only have to do this type of initialization for freshly malloc'd entries.
        elem->tmask = _mm512_set1_epi32(~0);
        elem->srcs = _mm512_set1_epi32(~0);
        elem->smask = _mm512_set1_epi32(~0);
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
    ((int*)(&(elem->tags)))[elem->end] = tag;
    ((int*)(&(elem->tmask)))[elem->end] = mask_tag;
    ((int*)(&(elem->srcs)))[elem->end] = source;
    ((int*)(&(elem->smask)))[elem->end] = mask_src;
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
    printf("Elements in the list (this is currently only partially implemented):\n");
    for(elem = list->head; elem; elem = elem->next)
    {
        printf("This is the %d linked list element\n", ++i);
        for(j = 0; j < 16; j++)
        {
            printf("%d:%d The key is %d, the mask is %d, the value is %lu\n", i, j, ((int*)(&(elem->tags)))[j],
                   ((int*)(&(elem->tmask)))[j], (uintptr_t) elem->value[j]);
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
    __m512i tags;
    __m512i srcs;
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
    printf("custom_match_umq_find_verify_hold list: %p:%d tag: %x peer: %x\n", (void *) list, list->size, tag, peer);
    custom_match_umq_dump(list);
#endif
    __mmask16 result = 0;
    custom_match_umq_node* prev = 0;
    custom_match_umq_node* elem = list->head;
    int i;
    __m512i tsearch = _mm512_set1_epi32(tag);
    __m512i ssearch = _mm512_set1_epi32(peer);

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

    __m512i tmasks = _mm512_set1_epi32(tmask);
    __m512i smasks = _mm512_set1_epi32(smask);

    tsearch = _mm512_and_epi32(tsearch, tmasks);
    ssearch = _mm512_and_epi32(ssearch, smasks);

    while(elem)
    {
        result = _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->tags,tmasks), tsearch) &
            _mm512_cmpeq_epi32_mask(_mm512_and_epi32(elem->srcs,smasks), ssearch);
        if(result)
        {
            for(i = elem->start; i <= elem->end; i++)
            {
                if((0x1 << i & result) && elem->value[i])
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
#if CUSTOM_MATCH_DEBUG_VERBOSE
    printf("custom_match_umq_find_remove_hold %p %p %x\n", (void *) prev, (void *) elem, i);
#endif
    ((int*)(&(elem->tags)))[i] = ~0;
    ((int*)(&(elem->srcs)))[i] = ~0;
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
#if CUSTOM_MATCH_DEBUG_VERBOSE
    int32_t key = source;
    ((int8_t*)&key)[3] = (int8_t) tag; // MGFD TODO verify this set higher order bits...
#endif
#if CUSTOM_MATCH_DEBUG_VERBOSE
    mca_pml_ob1_recv_frag_t *req = (mca_pml_ob1_recv_frag_t *)payload;
    printf("custom_match_umq_append list: %p payload: %p tag: %d src: %d\n", (void *) list, payload,
           req->hdr.hdr_match.hdr_tag, req->hdr.hdr_match.hdr_src);
#endif
    int i;
    custom_match_umq_node* elem;
    list->size++;
    if((!list->tail) || list->tail->end == 15)
    {
        if(list->pool)
        {
            elem = list->pool;
            list->pool = list->pool->next;
        }
        else
        {
            elem = _mm_malloc(sizeof(custom_match_umq_node),64);
        }
        elem->tags = _mm512_set1_epi32(~0); // TODO: we only have to do this type of initialization for freshly malloc'd entries.
        elem->srcs = _mm512_set1_epi32(~0);
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
    ((int*)(&(elem->tags)))[elem->end] = tag;
    ((int*)(&(elem->srcs)))[elem->end] = source;
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

    //printf("Elements in the list:\n");
    for (custom_match_umq_node *elem = list->head; elem; elem = elem->next) {
        //printf("This is the %d linked list element\n", ++i);
        for (int j = 0; j < 16; j++) {
            if (elem->value[j]) {
                mca_pml_ob1_recv_frag_t *req = (mca_pml_ob1_recv_frag_t *)elem->value[j];
                //printf("%x %x %x\n", elem->value[j], req->hdr.hdr_match.hdr_tag, req->hdr.hdr_match.hdr_src);
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
