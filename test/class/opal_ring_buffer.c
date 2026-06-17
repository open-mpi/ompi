/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for opal_ring_buffer_t.  Exercises the full public API with
 * self-checking assertions (via the support harness).  Note: the
 * library is compiled with -DNDEBUG, so assert() is a no-op here --
 * all verification must go through test_verify()/test_failure().
 */

#include "opal_config.h"

#include <stddef.h>
#include <stdio.h>

#include "support.h"

#include "opal/class/opal_ring_buffer.h"
#include "opal/constants.h"

/* Use addresses of static ints as opaque void* payloads -- pointer
   identity only, no heap allocation needed.  A large pool covers all
   tests without reuse confusion. */
static int items[32];

/* Shorthand: item pointer */
#define ITEM(n) ((void *)&items[(n)])

static void test_init_error(void);
static void test_push_and_pop_basic(void);
static void test_push_eviction_wraparound(void);
static void test_poke(void);
static void test_pop_empty(void);

int main(int argc, char *argv[])
{
    test_init("opal_ring_buffer_t");

    test_init_error();
    test_push_and_pop_basic();
    test_push_eviction_wraparound();
    test_poke();
    test_pop_empty();

    return test_finalize();
}

/* --------------------------------------------------------------------------
 * init error paths
 * -------------------------------------------------------------------------- */
static void test_init_error(void)
{
    /* NULL ring pointer must return OPAL_ERR_BAD_PARAM */
    test_verify("init(NULL) returns BAD_PARAM",
                OPAL_ERR_BAD_PARAM == opal_ring_buffer_init(NULL, 4));
}

/* --------------------------------------------------------------------------
 * Basic push-pop FIFO ordering (no wraparound / eviction)
 * -------------------------------------------------------------------------- */
static void test_push_and_pop_basic(void)
{
    opal_ring_buffer_t rb;
    void *ret;

    OBJ_CONSTRUCT(&rb, opal_ring_buffer_t);
    test_verify("init(3) ok", OPAL_SUCCESS == opal_ring_buffer_init(&rb, 3));

    /* Push three items into a 3-slot ring -- no slot is overwritten yet,
       so push must return NULL (no evicted occupant). */
    ret = opal_ring_buffer_push(&rb, ITEM(0));
    test_verify("push A into empty slot -> NULL", NULL == ret);

    ret = opal_ring_buffer_push(&rb, ITEM(1));
    test_verify("push B into empty slot -> NULL", NULL == ret);

    ret = opal_ring_buffer_push(&rb, ITEM(2));
    test_verify("push C into empty slot -> NULL", NULL == ret);

    /* Pop should return items FIFO -- oldest first: A, B, C */
    test_verify("pop returns oldest (A)", ITEM(0) == opal_ring_buffer_pop(&rb));
    test_verify("pop returns next   (B)", ITEM(1) == opal_ring_buffer_pop(&rb));
    test_verify("pop returns newest (C)", ITEM(2) == opal_ring_buffer_pop(&rb));

    /* Ring is now empty */
    test_verify("pop on empty -> NULL", NULL == opal_ring_buffer_pop(&rb));

    OBJ_DESTRUCT(&rb);
}

/* --------------------------------------------------------------------------
 * Push-when-full: eviction and FIFO ordering after wrap-around
 *
 * After filling a 3-slot ring (A, B, C), pushing D must evict A (the
 * oldest / tail element) and return it.  The ring then contains B, C, D
 * oldest-to-newest.
 *
 * NOTE: The function header comment says push "returns error if ring cannot
 * take another entry", yet the C signature is void* and the implementation
 * returns the evicted pointer (not an error code).  The void* return is the
 * authoritative contract; the header @return text is misleading.
 *
 * SUSPECTED BUG: header @return text says "returns error if ring cannot
 * take another entry" but the function signature is void* and the
 * implementation returns the evicted occupant (or NULL for a free slot).
 * The text contradicts the actual void* contract.
 * -------------------------------------------------------------------------- */
static void test_push_eviction_wraparound(void)
{
    opal_ring_buffer_t rb;
    void *evicted;

    OBJ_CONSTRUCT(&rb, opal_ring_buffer_t);
    test_verify("init(3) ok", OPAL_SUCCESS == opal_ring_buffer_init(&rb, 3));

    /* Fill the ring: A at slot0, B at slot1, C at slot2. */
    opal_ring_buffer_push(&rb, ITEM(10)); /* A */
    opal_ring_buffer_push(&rb, ITEM(11)); /* B */
    opal_ring_buffer_push(&rb, ITEM(12)); /* C */

    /* Pushing D into a full ring must evict and return A (oldest). */
    evicted = opal_ring_buffer_push(&rb, ITEM(13)); /* D evicts A */
    test_verify("push into full -> evicts oldest (A)", ITEM(10) == evicted);

    /* Ring now: B, C, D (oldest to newest). */
    test_verify("pop after eviction -> B", ITEM(11) == opal_ring_buffer_pop(&rb));
    test_verify("pop after eviction -> C", ITEM(12) == opal_ring_buffer_pop(&rb));
    test_verify("pop after eviction -> D", ITEM(13) == opal_ring_buffer_pop(&rb));
    test_verify("pop on empty after eviction -> NULL", NULL == opal_ring_buffer_pop(&rb));

    /* Verify multiple wrap-arounds: push 6 items into an empty size-3 ring.
       After 6 pushes the ring should hold items 23,24,25 (newest three). */
    opal_ring_buffer_push(&rb, ITEM(20)); /* evicts nothing (empty) */
    opal_ring_buffer_push(&rb, ITEM(21));
    opal_ring_buffer_push(&rb, ITEM(22));
    opal_ring_buffer_push(&rb, ITEM(23)); /* evicts ITEM(20) */
    opal_ring_buffer_push(&rb, ITEM(24)); /* evicts ITEM(21) */
    opal_ring_buffer_push(&rb, ITEM(25)); /* evicts ITEM(22) */

    test_verify("wrap: pop -> item 23", ITEM(23) == opal_ring_buffer_pop(&rb));
    test_verify("wrap: pop -> item 24", ITEM(24) == opal_ring_buffer_pop(&rb));
    test_verify("wrap: pop -> item 25", ITEM(25) == opal_ring_buffer_pop(&rb));
    test_verify("wrap: pop on empty -> NULL", NULL == opal_ring_buffer_pop(&rb));

    OBJ_DESTRUCT(&rb);
}

/* --------------------------------------------------------------------------
 * poke(): non-destructive index into the ring
 *
 * poke(i>=0): return element i positions forward from the tail (oldest).
 * poke(i<0):  return the element at the head - 1, i.e., the newest item.
 * poke(i>=size): return NULL.
 * poke on empty ring: return NULL regardless of index.
 * -------------------------------------------------------------------------- */
static void test_poke(void)
{
    opal_ring_buffer_t rb;

    OBJ_CONSTRUCT(&rb, opal_ring_buffer_t);
    test_verify("init(4) ok", OPAL_SUCCESS == opal_ring_buffer_init(&rb, 4));

    /* poke on an empty ring must return NULL for any index */
    test_verify("poke(0) on empty -> NULL", NULL == opal_ring_buffer_poke(&rb, 0));
    test_verify("poke(-1) on empty -> NULL", NULL == opal_ring_buffer_poke(&rb, -1));
    test_verify("poke(3) on empty -> NULL", NULL == opal_ring_buffer_poke(&rb, 3));

    /* Push 4 items: A=ITEM(0), B=ITEM(1), C=ITEM(2), D=ITEM(3) */
    opal_ring_buffer_push(&rb, ITEM(0)); /* A, oldest */
    opal_ring_buffer_push(&rb, ITEM(1)); /* B */
    opal_ring_buffer_push(&rb, ITEM(2)); /* C */
    opal_ring_buffer_push(&rb, ITEM(3)); /* D, newest */

    /* poke by non-negative offset from tail (oldest) */
    test_verify("poke(0) -> oldest (A)", ITEM(0) == opal_ring_buffer_poke(&rb, 0));
    test_verify("poke(1) -> B",          ITEM(1) == opal_ring_buffer_poke(&rb, 1));
    test_verify("poke(2) -> C",          ITEM(2) == opal_ring_buffer_poke(&rb, 2));
    test_verify("poke(3) -> newest (D)", ITEM(3) == opal_ring_buffer_poke(&rb, 3));

    /* poke(-1) returns the element at head-1 = newest */
    test_verify("poke(-1) -> newest (D)", ITEM(3) == opal_ring_buffer_poke(&rb, -1));

    /* poke with index >= size returns NULL */
    test_verify("poke(4) -> NULL (i >= size)", NULL == opal_ring_buffer_poke(&rb, 4));
    test_verify("poke(99) -> NULL (i >> size)", NULL == opal_ring_buffer_poke(&rb, 99));

    /* After push-eviction the ring wraps; poke must still work correctly.
       Push E (evicts A): ring is B,C,D,E oldest-to-newest. */
    opal_ring_buffer_push(&rb, ITEM(4)); /* E evicts A */

    test_verify("poke(0) after wrap -> B", ITEM(1) == opal_ring_buffer_poke(&rb, 0));
    test_verify("poke(3) after wrap -> E", ITEM(4) == opal_ring_buffer_poke(&rb, 3));
    test_verify("poke(-1) after wrap -> E (newest)", ITEM(4) == opal_ring_buffer_poke(&rb, -1));

    /* poke does NOT remove; pop should still deliver items in FIFO order */
    test_verify("poke does not remove: pop -> B", ITEM(1) == opal_ring_buffer_pop(&rb));
    test_verify("poke does not remove: pop -> C", ITEM(2) == opal_ring_buffer_pop(&rb));
    test_verify("poke does not remove: pop -> D", ITEM(3) == opal_ring_buffer_pop(&rb));
    test_verify("poke does not remove: pop -> E", ITEM(4) == opal_ring_buffer_pop(&rb));

    OBJ_DESTRUCT(&rb);
}

/* --------------------------------------------------------------------------
 * pop from empty ring always returns NULL
 * -------------------------------------------------------------------------- */
static void test_pop_empty(void)
{
    opal_ring_buffer_t rb;

    OBJ_CONSTRUCT(&rb, opal_ring_buffer_t);
    test_verify("init(1) ok", OPAL_SUCCESS == opal_ring_buffer_init(&rb, 1));

    /* Freshly initialised, tail == -1 -> empty */
    test_verify("pop from fresh empty ring -> NULL", NULL == opal_ring_buffer_pop(&rb));

    /* Push one item, pop it, then pop again -- ring is empty */
    opal_ring_buffer_push(&rb, ITEM(5));
    opal_ring_buffer_pop(&rb);
    test_verify("pop from drained ring -> NULL", NULL == opal_ring_buffer_pop(&rb));

    OBJ_DESTRUCT(&rb);
}
