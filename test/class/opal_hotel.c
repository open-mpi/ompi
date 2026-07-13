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
 * Unit test for opal_hotel_t.  Exercises the full public API with
 * self-checking assertions (via the support harness).  Note: the
 * library is compiled with -DNDEBUG, so assert() is a no-op here --
 * all verification must go through test_verify()/test_failure().
 *
 * We pass evbase=NULL throughout so that no libevent timers are armed.
 * With evbase=NULL, checkin/checkout work entirely synchronously.
 * A non-NULL evict_callback_fn is still required by opal_hotel_init().
 */

#include "opal_config.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "support.h"

#include "opal/class/opal_hotel.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

/* Trivial eviction callback -- never actually called in these tests
   because evbase==NULL means no timer fires.  It must be non-NULL to
   satisfy the bozo check inside opal_hotel_init(). */
static void evict_cb(struct opal_hotel_t *hotel, int room_num, void *occupant)
{
    (void) hotel;
    (void) room_num;
    (void) occupant;
    /* Should never be reached in these tests (evbase==NULL). */
    test_failure("evict_cb: unexpected eviction callback invoked");
}

/* Opaque occupant payloads -- pointer identity only. */
static int guests[16];
#define GUEST(n) ((void *)&guests[(n)])

static void test_init_errors(void);
static void test_basic_checkin_checkout(void);
static void test_checkout_and_return(void);
static void test_is_empty(void);
static void test_full_hotel(void);
static void test_checkin_with_res(void);
static void test_knock(void);
static void test_checkout_empty_room(void);

int main(int argc, char *argv[])
{
    /* Some opal_hotel error paths call opal_output(), which needs the
       opal output/finalize infrastructure initialized. */
    opal_init_util(&argc, &argv);

    test_init("opal_hotel_t");

    test_init_errors();
    test_basic_checkin_checkout();
    test_checkout_and_return();
    test_is_empty();
    test_full_hotel();
    test_checkin_with_res();
    test_knock();
    test_checkout_empty_room();

    int ret = test_finalize();
    opal_finalize_util();
    return ret;
}

/* --------------------------------------------------------------------------
 * opal_hotel_init() error paths
 * -------------------------------------------------------------------------- */
static void test_init_errors(void)
{
    opal_hotel_t h;
    int rc;

    /* num_rooms == 0 is rejected */
    OBJ_CONSTRUCT(&h, opal_hotel_t);
    rc = opal_hotel_init(&h, 0, NULL, 0, 0, evict_cb);
    test_verify("init(num_rooms=0) -> BAD_PARAM", OPAL_ERR_BAD_PARAM == rc);
    OBJ_DESTRUCT(&h);

    /* num_rooms < 0 is rejected */
    OBJ_CONSTRUCT(&h, opal_hotel_t);
    rc = opal_hotel_init(&h, -1, NULL, 0, 0, evict_cb);
    test_verify("init(num_rooms=-1) -> BAD_PARAM", OPAL_ERR_BAD_PARAM == rc);
    OBJ_DESTRUCT(&h);

    /* NULL evict_callback_fn is rejected */
    OBJ_CONSTRUCT(&h, opal_hotel_t);
    rc = opal_hotel_init(&h, 3, NULL, 0, 0, NULL);
    test_verify("init(evict_fn=NULL) -> BAD_PARAM", OPAL_ERR_BAD_PARAM == rc);
    OBJ_DESTRUCT(&h);

    /* Valid parameters succeed */
    OBJ_CONSTRUCT(&h, opal_hotel_t);
    rc = opal_hotel_init(&h, 3, NULL, 0, 0, evict_cb);
    test_verify("init(3, NULL, 0, 0, cb) -> SUCCESS", OPAL_SUCCESS == rc);
    OBJ_DESTRUCT(&h);
}

/* --------------------------------------------------------------------------
 * Basic checkin / checkout sequence
 * -------------------------------------------------------------------------- */
static void test_basic_checkin_checkout(void)
{
    opal_hotel_t h;
    int room[3];
    int i, rc;

    OBJ_CONSTRUCT(&h, opal_hotel_t);
    test_verify("init(3) ok", OPAL_SUCCESS == opal_hotel_init(&h, 3, NULL, 0, 0, evict_cb));

    /* Check in three occupants; each must get a distinct, in-range room. */
    for (i = 0; i < 3; ++i) {
        room[i] = -1;
        rc = opal_hotel_checkin(&h, GUEST(i), &room[i]);
        test_verify("checkin -> SUCCESS", OPAL_SUCCESS == rc);
        test_verify("room num in range [0, num_rooms)",
                    room[i] >= 0 && room[i] < 3);
    }

    /* All three room numbers must be distinct. */
    test_verify("room 0 != room 1", room[0] != room[1]);
    test_verify("room 0 != room 2", room[0] != room[2]);
    test_verify("room 1 != room 2", room[1] != room[2]);

    /* Check out each room; afterwards a fresh checkin must succeed
       (verifying the room was returned to the free pool). */
    for (i = 0; i < 3; ++i) {
        opal_hotel_checkout(&h, room[i]);
    }

    int new_room = -1;
    rc = opal_hotel_checkin(&h, GUEST(5), &new_room);
    test_verify("checkin after all checkouts -> SUCCESS", OPAL_SUCCESS == rc);
    test_verify("room from fresh checkin in range", new_room >= 0 && new_room < 3);
    opal_hotel_checkout(&h, new_room);

    OBJ_DESTRUCT(&h);
}

/* --------------------------------------------------------------------------
 * checkout_and_return_occupant: verify occupant pointer is returned
 * -------------------------------------------------------------------------- */
static void test_checkout_and_return(void)
{
    opal_hotel_t h;
    int room = -1;
    void *out = NULL;

    OBJ_CONSTRUCT(&h, opal_hotel_t);
    test_verify("init(2) ok", OPAL_SUCCESS == opal_hotel_init(&h, 2, NULL, 0, 0, evict_cb));

    opal_hotel_checkin(&h, GUEST(7), &room);
    test_verify("room is valid", room >= 0 && room < 2);

    opal_hotel_checkout_and_return_occupant(&h, room, &out);
    test_verify("returned occupant matches checked-in guest", GUEST(7) == out);

    /* A second checkout from the same (now-empty) room must return NULL. */
    out = GUEST(15); /* non-NULL sentinel distinct from GUEST(7) */
    opal_hotel_checkout_and_return_occupant(&h, room, &out);
    test_verify("checkout_and_return on empty room -> NULL occupant", NULL == out);

    OBJ_DESTRUCT(&h);
}

/* --------------------------------------------------------------------------
 * is_empty transitions
 * -------------------------------------------------------------------------- */
static void test_is_empty(void)
{
    opal_hotel_t h;
    int r0 = -1, r1 = -1;

    OBJ_CONSTRUCT(&h, opal_hotel_t);
    test_verify("init(2) ok", OPAL_SUCCESS == opal_hotel_init(&h, 2, NULL, 0, 0, evict_cb));

    /* Fresh hotel is empty */
    test_verify("fresh hotel is_empty == true", opal_hotel_is_empty(&h));

    /* After first checkin, hotel is no longer empty */
    opal_hotel_checkin(&h, GUEST(0), &r0);
    test_verify("after first checkin is_empty == false", !opal_hotel_is_empty(&h));

    /* After second checkin (all rooms occupied) still not empty */
    opal_hotel_checkin(&h, GUEST(1), &r1);
    test_verify("after both checkins is_empty == false", !opal_hotel_is_empty(&h));

    /* Checkout one; not yet empty */
    opal_hotel_checkout(&h, r0);
    test_verify("after one checkout is_empty == false", !opal_hotel_is_empty(&h));

    /* Checkout the other; now empty */
    opal_hotel_checkout(&h, r1);
    test_verify("after all checkouts is_empty == true", opal_hotel_is_empty(&h));

    OBJ_DESTRUCT(&h);
}

/* --------------------------------------------------------------------------
 * Checkin beyond capacity returns OPAL_ERR_TEMP_OUT_OF_RESOURCE
 * -------------------------------------------------------------------------- */
static void test_full_hotel(void)
{
    opal_hotel_t h;
    int rooms[3];
    int i, rc;

    OBJ_CONSTRUCT(&h, opal_hotel_t);
    test_verify("init(3) ok", OPAL_SUCCESS == opal_hotel_init(&h, 3, NULL, 0, 0, evict_cb));

    for (i = 0; i < 3; ++i) {
        rooms[i] = -1;
        rc = opal_hotel_checkin(&h, GUEST(i), &rooms[i]);
        test_verify("fill: checkin -> SUCCESS", OPAL_SUCCESS == rc);
    }

    /* Hotel is full; one more checkin must fail. */
    int overflow_room = -1;
    rc = opal_hotel_checkin(&h, GUEST(9), &overflow_room);
    test_verify("checkin when full -> TEMP_OUT_OF_RESOURCE",
                OPAL_ERR_TEMP_OUT_OF_RESOURCE == rc);

    /* Room variable must be untouched on failure (remains -1) */
    test_verify("overflow room_num unchanged on failure", -1 == overflow_room);

    /* Free one room; the next checkin must succeed. */
    opal_hotel_checkout(&h, rooms[0]);
    rc = opal_hotel_checkin(&h, GUEST(10), &rooms[0]);
    test_verify("checkin after partial checkout -> SUCCESS", OPAL_SUCCESS == rc);
    test_verify("new room in range after partial checkout",
                rooms[0] >= 0 && rooms[0] < 3);

    /* Clean up remaining occupants */
    for (i = 0; i < 3; ++i) {
        opal_hotel_checkout(&h, rooms[i]);
    }

    OBJ_DESTRUCT(&h);
}

/* --------------------------------------------------------------------------
 * checkin_with_res: optimized path for a caller that knows a room is free
 * -------------------------------------------------------------------------- */
static void test_checkin_with_res(void)
{
    opal_hotel_t h;
    int room = -1;
    void *out = NULL;

    OBJ_CONSTRUCT(&h, opal_hotel_t);
    test_verify("init(2) ok", OPAL_SUCCESS == opal_hotel_init(&h, 2, NULL, 0, 0, evict_cb));

    /* Use the with_res variant (caller asserts a room is available). */
    opal_hotel_checkin_with_res(&h, GUEST(3), &room);
    test_verify("checkin_with_res: room in range", room >= 0 && room < 2);

    /* Verify occupant is actually present via checkout_and_return */
    opal_hotel_checkout_and_return_occupant(&h, room, &out);
    test_verify("checkin_with_res: occupant correct", GUEST(3) == out);

    OBJ_DESTRUCT(&h);
}

/* --------------------------------------------------------------------------
 * knock(): peek at a room without checking the occupant out
 * -------------------------------------------------------------------------- */
static void test_knock(void)
{
    opal_hotel_t h;
    int room = -1;
    void *out = NULL;

    OBJ_CONSTRUCT(&h, opal_hotel_t);
    test_verify("init(2) ok", OPAL_SUCCESS == opal_hotel_init(&h, 2, NULL, 0, 0, evict_cb));

    opal_hotel_checkin(&h, GUEST(6), &room);
    test_verify("knock: room in range", room >= 0 && room < 2);

    /* knock peeks at occupant but does NOT check them out */
    opal_hotel_knock(&h, room, &out);
    test_verify("knock returns correct occupant", GUEST(6) == out);

    /* Occupant is still there -- a second knock sees the same value */
    out = NULL;
    opal_hotel_knock(&h, room, &out);
    test_verify("knock is non-destructive (second knock)", GUEST(6) == out);

    /* And checkout_and_return still works after two knocks */
    out = NULL;
    opal_hotel_checkout_and_return_occupant(&h, room, &out);
    test_verify("occupant still present after knocks", GUEST(6) == out);

    OBJ_DESTRUCT(&h);
}

/* --------------------------------------------------------------------------
 * Checkout an already-empty room: must be a no-op (not crash or corrupt state)
 * -------------------------------------------------------------------------- */
static void test_checkout_empty_room(void)
{
    opal_hotel_t h;
    int room = -1;
    int rc;

    OBJ_CONSTRUCT(&h, opal_hotel_t);
    test_verify("init(2) ok", OPAL_SUCCESS == opal_hotel_init(&h, 2, NULL, 0, 0, evict_cb));

    /* Checkin then checkout to get a valid room number in range */
    rc = opal_hotel_checkin(&h, GUEST(0), &room);
    test_verify("setup checkin ok", OPAL_SUCCESS == rc);
    opal_hotel_checkout(&h, room);

    /* Checkout same room again (already empty) -- the inline code has an
       OPAL_LIKELY(room->occupant != NULL) guard, so it simply does nothing.
       Verify the hotel state remains consistent: we can still checkin. */
    opal_hotel_checkout(&h, room); /* no-op */

    int new_room = -1;
    rc = opal_hotel_checkin(&h, GUEST(1), &new_room);
    test_verify("checkin after double-checkout still works", OPAL_SUCCESS == rc);
    test_verify("room from checkin after double-checkout in range",
                new_room >= 0 && new_room < 2);
    opal_hotel_checkout(&h, new_room);

    OBJ_DESTRUCT(&h);
}
