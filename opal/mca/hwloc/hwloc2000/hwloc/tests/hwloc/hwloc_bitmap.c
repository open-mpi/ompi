/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009, 2012 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <assert.h>

/* check misc bitmap stuff */

int main(void)
{
  hwloc_bitmap_t set;

  /* check an empty bitmap */
  set = hwloc_bitmap_alloc();
  assert(hwloc_bitmap_weight(set) == 0);
  assert(hwloc_bitmap_first(set) == -1);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_to_ulong(set) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 23) == 0UL);
  /* check a non-empty bitmap */
  hwloc_bitmap_from_ith_ulong(set, 4, 0xff);
  assert(hwloc_bitmap_to_ith_ulong(set, 4) == 0xff);
  assert(hwloc_bitmap_to_ulong(set) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 23) == 0UL);
  /* check a two-long bitmap */
  hwloc_bitmap_from_ith_ulong(set, 4, 0xfe);
  hwloc_bitmap_set_ith_ulong(set, 6, 0xef);
  assert(hwloc_bitmap_to_ith_ulong(set, 4) == 0xfe);
  assert(hwloc_bitmap_to_ith_ulong(set, 6) == 0xef);
  assert(hwloc_bitmap_to_ulong(set) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 23) == 0UL);
  /* check a zeroed bitmap */
  hwloc_bitmap_zero(set);
  assert(hwloc_bitmap_weight(set) == 0);
  assert(hwloc_bitmap_first(set) == -1);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_to_ulong(set) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 4) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 23) == 0UL);
  hwloc_bitmap_free(set);

  /* check a full bitmap */
  set = hwloc_bitmap_alloc_full();
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_to_ulong(set) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 23) == ~0UL);
  /* check a almost full bitmap */
  hwloc_bitmap_set_ith_ulong(set, 4, 0xff);
  assert(hwloc_bitmap_to_ith_ulong(set, 4) == 0xff);
  assert(hwloc_bitmap_to_ulong(set) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 23) == ~0UL);
  /* check a almost empty bitmap */
  hwloc_bitmap_from_ith_ulong(set, 4, 0xff);
  assert(hwloc_bitmap_to_ith_ulong(set, 4) == 0xff);
  assert(hwloc_bitmap_to_ulong(set) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == 0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 23) == 0UL);
  /* check a filled bitmap */
  hwloc_bitmap_fill(set);
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_to_ith_ulong(set, 4) == ~0UL);
  assert(hwloc_bitmap_to_ulong(set) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == ~0UL);
  assert(hwloc_bitmap_to_ith_ulong(set, 23) == ~0UL);
  hwloc_bitmap_free(set);

  /* check ranges */
  set = hwloc_bitmap_alloc();
  assert(hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 0);
  assert(hwloc_bitmap_first(set) == -1);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, -1) == 0);
  /* 20-39 */
  hwloc_bitmap_set_range(set, 20, 39);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 20);
  assert(hwloc_bitmap_first(set) == 20);
  assert(hwloc_bitmap_last(set) == 39);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 19) == 40);
  /* 20-39,80- */
  hwloc_bitmap_set_range(set, 80, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 20);
  assert(hwloc_bitmap_next(set, 39) == 80);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == 79);
  assert(hwloc_bitmap_next_unset(set, 79) == -1);
  /* 20-39,80-359 */
  hwloc_bitmap_clr_range(set, 360, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 300);
  assert(hwloc_bitmap_first(set) == 20);
  assert(hwloc_bitmap_next(set, 39) == 80);
  assert(hwloc_bitmap_last(set) == 359);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 100) == 360);
  /* 20-39,80-179,280-359 */
  hwloc_bitmap_clr_range(set, 180, 279);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 200);
  assert(hwloc_bitmap_first(set) == 20);
  assert(hwloc_bitmap_next(set, 39) == 80);
  assert(hwloc_bitmap_next(set, 179) == 280);
  assert(hwloc_bitmap_last(set) == 359);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 100) == 180);
  /* 20- */
  hwloc_bitmap_set_range(set, 35, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 20);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == 19);
  assert(hwloc_bitmap_next_unset(set, 100) == -1);
  /* 20-419 */
  hwloc_bitmap_clr_range(set, 420, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 400);
  assert(hwloc_bitmap_first(set) == 20);
  assert(hwloc_bitmap_last(set) == 419);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 100) == 420);
  /* 20-419,1000- */
  hwloc_bitmap_set_range(set, 1000, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 20);
  assert(hwloc_bitmap_next(set, 419) == 1000);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == 999);
  assert(hwloc_bitmap_next_unset(set, 1000) == -1);
  /* 20- */
  hwloc_bitmap_set_range(set, 420, 999);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 20);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == 19);
  assert(hwloc_bitmap_next_unset(set, 1000) == -1);
  /* 0- */
  hwloc_bitmap_set_range(set, 0, 25);
  assert(!hwloc_bitmap_iszero(set));
  assert(hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == -1);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 1000) == -1);
  /* 0-99,1500- */
  hwloc_bitmap_clr_range(set, 100, 1499);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 1500);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == 1499);
  assert(hwloc_bitmap_next_unset(set, 99) == 100);
  /* 0-99,1500-1999 */
  hwloc_bitmap_clr_range(set, 2000, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 600);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 1500);
  assert(hwloc_bitmap_last(set) == 1999);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 1800) == 2000);
  /* 0-99,1500- */
  hwloc_bitmap_set_range(set, 1500, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 1500);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == 1499);
  assert(hwloc_bitmap_next_unset(set, 1000) == 1001);
  /* 0-99,1500-2199 */
  hwloc_bitmap_clr_range(set, 2200, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 800);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 1500);
  assert(hwloc_bitmap_last(set) == 2199);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 1800) == 2200);
  /* 0-99,1500-1999 */
  hwloc_bitmap_clr_range(set, 2000, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 600);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 1500);
  assert(hwloc_bitmap_last(set) == 1999);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 1800) == 2000);
  /* 0-99,1500- */
  hwloc_bitmap_set_range(set, 2000, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == -1);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 1500);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == 1499);
  assert(hwloc_bitmap_next_unset(set, 1800) == -1);
  /* 0-99,1500-1999 */
  hwloc_bitmap_clr_range(set, 2000, -1);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 600);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 1500);
  assert(hwloc_bitmap_last(set) == 1999);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 1800) == 2000);
  /* 0-99,200-1999 */
  hwloc_bitmap_set_range(set, 200, 1499);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 1900);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 200);
  assert(hwloc_bitmap_last(set) == 1999);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 200) == 2000);
  /* 0-99,1999 */
  hwloc_bitmap_clr_range(set, 200, 1998);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 101);
  assert(hwloc_bitmap_first(set) == 0);
  assert(hwloc_bitmap_next(set, 99) == 1999);
  assert(hwloc_bitmap_last(set) == 1999);
  assert(hwloc_bitmap_first_unset(set) == 100);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, 1999) == 2000);
  /* 1999 */
  hwloc_bitmap_clr_range(set, 0, 100);
  assert(!hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 1);
  assert(hwloc_bitmap_first(set) == 1999);
  assert(hwloc_bitmap_last(set) == 1999);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, -1) == 0);
  assert(hwloc_bitmap_next_unset(set, 1999) == 2000);
  /* empty */
  hwloc_bitmap_clr(set, 1999);
  assert(hwloc_bitmap_iszero(set));
  assert(!hwloc_bitmap_isfull(set));
  assert(hwloc_bitmap_weight(set) == 0);
  assert(hwloc_bitmap_first(set) == -1);
  assert(hwloc_bitmap_last(set) == -1);
  assert(hwloc_bitmap_first_unset(set) == 0);
  assert(hwloc_bitmap_last_unset(set) == -1);
  assert(hwloc_bitmap_next_unset(set, -1) == 0);
  assert(hwloc_bitmap_next_unset(set, 1999) == 2000);
  hwloc_bitmap_free(set);

  /* check miscellaneous other functions */
  set = hwloc_bitmap_alloc();
  /* from_ulong */
  hwloc_bitmap_from_ulong(set, 0x0ff0);
  assert(hwloc_bitmap_first(set) == 4);
  assert(hwloc_bitmap_last(set) == 11);
  assert(hwloc_bitmap_weight(set) == 8);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == 0xff0);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == 0);
  /* from_ith_ulong */
  hwloc_bitmap_zero(set);
  assert(hwloc_bitmap_weight(set) == 0);
  hwloc_bitmap_from_ith_ulong(set, 2, 0xff00);
  assert(hwloc_bitmap_weight(set) == 8);
  assert(hwloc_bitmap_to_ith_ulong(set, 0) == 0);
  assert(hwloc_bitmap_to_ith_ulong(set, 1) == 0);
  assert(hwloc_bitmap_to_ith_ulong(set, 2) == 0xff00);
  assert(hwloc_bitmap_to_ith_ulong(set, 3) == 0);
  /* allbut and not */
  hwloc_bitmap_allbut(set, 153);
  assert(hwloc_bitmap_weight(set) == -1);
  hwloc_bitmap_not(set, set);
  assert(hwloc_bitmap_weight(set) == 1);
  assert(hwloc_bitmap_first(set) == 153);
  assert(hwloc_bitmap_last(set) == 153);
  /* clr_range */
  hwloc_bitmap_fill(set);
  hwloc_bitmap_clr_range(set, 178, 3589);
  hwloc_bitmap_not(set, set);
  assert(hwloc_bitmap_weight(set) == 3589-178+1);
  assert(hwloc_bitmap_first(set) == 178);
  assert(hwloc_bitmap_last(set) == 3589);
  /* singlify */
  hwloc_bitmap_zero(set);
  hwloc_bitmap_set_range(set, 0, 127);
  assert(hwloc_bitmap_weight(set) == 128);
  hwloc_bitmap_not(set, set);
  assert(hwloc_bitmap_weight(set) == -1);
  hwloc_bitmap_singlify(set);
  assert(hwloc_bitmap_weight(set) == 1);
  assert(hwloc_bitmap_first(set) == 128);
  assert(hwloc_bitmap_last(set) == 128);

  hwloc_bitmap_free(set);

  return 0;
}
