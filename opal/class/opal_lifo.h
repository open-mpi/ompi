/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Voltaire All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reseved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_LIFO_H_HAS_BEEN_INCLUDED
#define OPAL_LIFO_H_HAS_BEEN_INCLUDED

#include "opal_config.h"
#include "opal/class/opal_list.h"

#include "opal/sys/atomic.h"
#include "opal/threads/mutex.h"

BEGIN_C_DECLS

/* NTH: temporarily suppress warnings about this not being defined */
#if !defined(OPAL_HAVE_ATOMIC_CMPSET_128)
#define OPAL_HAVE_ATOMIC_CMPSET_128 0
#endif

/**
 * Counted pointer to avoid the ABA problem.
 */
union opal_counted_pointer_t {
    struct {
        /** update counter used when cmpset_128 is available */
        uint64_t counter;
        /** list item pointer */
        opal_list_item_t *item;
    } data;
#if OPAL_HAVE_ATOMIC_CMPSET_128 && HAVE_OPAL_INT128_T
    /** used for atomics when there is a cmpset that can operate on
     * two 64-bit values */
    opal_int128_t value;
#endif
};
typedef union opal_counted_pointer_t opal_counted_pointer_t;


#if OPAL_HAVE_ATOMIC_CMPSET_128

/* Add one element to the FIFO. We will return the last head of the list
 * to allow the upper level to detect if this element is the first one in the
 * list (if the list was empty before this operation).
 */
static inline bool opal_update_counted_pointer (volatile opal_counted_pointer_t *addr, opal_counted_pointer_t old,
                                                opal_list_item_t *item)
{
    opal_counted_pointer_t new = {.data = {.item = item, .counter = old.data.counter + 1}};
    return opal_atomic_cmpset_128 (&addr->value, old.value, new.value);
}

#endif

/* Atomic Last In First Out lists. If we are in a multi-threaded environment then the
 * atomicity is insured via the compare-and-swap operation, if not we simply do a read
 * and/or a write.
 *
 * There is a trick. The ghost element at the end of the list. This ghost element has
 * the next pointer pointing to itself, therefore we cannot go past the end of the list.
 * With this approach we will never have a NULL element in the list, so we never have
 * to test for the NULL.
 */
struct opal_lifo_t {
    opal_object_t super;

    /** head element of the lifo. points to opal_lifo_ghost if the lifo is empty */
    volatile opal_counted_pointer_t opal_lifo_head;

    /** list sentinel (always points to self) */
    opal_list_item_t opal_lifo_ghost;
};

typedef struct opal_lifo_t opal_lifo_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_lifo_t);


/* The ghost pointer will never change. The head will change via an atomic
 * compare-and-swap. On most architectures the reading of a pointer is an
 * atomic operation so we don't have to protect it.
 */
static inline bool opal_lifo_is_empty( opal_lifo_t* lifo )
{
    return (opal_list_item_t *) lifo->opal_lifo_head.data.item == &lifo->opal_lifo_ghost;
}


#if OPAL_HAVE_ATOMIC_CMPSET_128

/* Add one element to the LIFO. We will return the last head of the list
 * to allow the upper level to detect if this element is the first one in the
 * list (if the list was empty before this operation).
 */
static inline opal_list_item_t *opal_lifo_push_atomic (opal_lifo_t *lifo,
                                                       opal_list_item_t *item)
{
    do {
        opal_list_item_t *next = (opal_list_item_t *) lifo->opal_lifo_head.data.item;

        item->opal_list_next = next;
        opal_atomic_wmb ();

        /* to protect against ABA issues it is sufficient to only update the counter in pop */
        if (opal_atomic_cmpset_ptr (&lifo->opal_lifo_head.data.item, next, item)) {
            return next;
        }
        /* DO some kind of pause to release the bus */
    } while (1);
}

/* Retrieve one element from the LIFO. If we reach the ghost element then the LIFO
 * is empty so we return NULL.
 */
static inline opal_list_item_t *opal_lifo_pop_atomic (opal_lifo_t* lifo)
{
    opal_list_item_t *item;

    do {
        opal_counted_pointer_t old_head;

        old_head.data.counter = lifo->opal_lifo_head.data.counter;
        opal_atomic_rmb ();
        item = old_head.data.item = lifo->opal_lifo_head.data.item;

        if (item == &lifo->opal_lifo_ghost) {
            return NULL;
        }

        if (opal_update_counted_pointer (&lifo->opal_lifo_head, old_head,
                                         (opal_list_item_t *) item->opal_list_next)) {
            opal_atomic_wmb ();
            item->opal_list_next = NULL;
            return item;
        }
    } while (1);
}

#else

/* Add one element to the LIFO. We will return the last head of the list
 * to allow the upper level to detect if this element is the first one in the
 * list (if the list was empty before this operation).
 */
static inline opal_list_item_t *opal_lifo_push_atomic (opal_lifo_t *lifo,
                                                       opal_list_item_t *item)
{
    /* item free acts as a mini lock to avoid ABA problems */
    item->item_free = 1;
    do {
        opal_list_item_t *next = (opal_list_item_t *) lifo->opal_lifo_head.data.item;
        item->opal_list_next = next;
        opal_atomic_wmb();
        if (opal_atomic_cmpset_ptr (&lifo->opal_lifo_head.data.item, next, item)) {
            opal_atomic_wmb ();
            /* now safe to pop this item */
            item->item_free = 0;
            return next;
        }
        /* DO some kind of pause to release the bus */
    } while (1);
}

/* Retrieve one element from the LIFO. If we reach the ghost element then the LIFO
 * is empty so we return NULL.
 */
static inline opal_list_item_t *opal_lifo_pop_atomic (opal_lifo_t* lifo)
{
    opal_list_item_t *item;
    while ((item = (opal_list_item_t *) lifo->opal_lifo_head.data.item) != &lifo->opal_lifo_ghost) {
        opal_atomic_rmb();

        /* ensure it is safe to pop the head */
        if (opal_atomic_swap_32((volatile int32_t *) &item->item_free, 1)) {
            continue;
        }

        /* try to swap out the head pointer */
        if (opal_atomic_cmpset_ptr (&lifo->opal_lifo_head.data.item, item,
                                   (void *) item->opal_list_next)) {
            break;
        }
        /* NTH: don't need another atomic here */
        item->item_free = 0;
        /* Do some kind of pause to release the bus */
    }

    if (item == &lifo->opal_lifo_ghost) {
        return NULL;
    }

    opal_atomic_wmb ();

    item->opal_list_next = NULL;
    return item;
}

#endif

/* single-threaded versions of the lifo functions */
static inline opal_list_item_t *opal_lifo_push_st (opal_lifo_t *lifo,
                                                   opal_list_item_t *item)
{
    item->opal_list_next = (opal_list_item_t *) lifo->opal_lifo_head.data.item;
    item->item_free = 0;
    lifo->opal_lifo_head.data.item = item;
    return (opal_list_item_t *) item->opal_list_next;
}

static inline opal_list_item_t *opal_lifo_pop_st (opal_lifo_t *lifo)
{
    opal_list_item_t *item;
    item = (opal_list_item_t *) lifo->opal_lifo_head.data.item;
    lifo->opal_lifo_head.data.item = (opal_list_item_t *) item->opal_list_next;
    if (item == &lifo->opal_lifo_ghost) {
        return NULL;
    }

    item->opal_list_next = NULL;
    item->item_free = 1;
    return item;
}

/* conditional versions of lifo functions. use atomics if opal_using_threads is set */
static inline opal_list_item_t *opal_lifo_push (opal_lifo_t *lifo,
                                                opal_list_item_t *item)
{
    if (opal_using_threads ()) {
        return opal_lifo_push_atomic (lifo, item);
    }

    return opal_lifo_push_st (lifo, item);
}

static inline opal_list_item_t *opal_lifo_pop (opal_lifo_t *lifo)
{
    if (opal_using_threads ()) {
        return opal_lifo_pop_atomic (lifo);
    }

    return opal_lifo_pop_st (lifo);
}

END_C_DECLS

#endif  /* OPAL_LIFO_H_HAS_BEEN_INCLUDED */
