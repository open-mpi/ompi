/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2010 inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* check hwloc_bitmap_singlify() */

int main(void)
{
  hwloc_bitmap_t orig, expected;

  orig = hwloc_bitmap_alloc();
  expected = hwloc_bitmap_alloc();

  /* empty set gives empty set */
  hwloc_bitmap_singlify(orig);
  assert(hwloc_bitmap_iszero(orig));

  /* full set gives first bit only */
  hwloc_bitmap_fill(orig);
  hwloc_bitmap_singlify(orig);
  hwloc_bitmap_zero(expected);
  hwloc_bitmap_set(expected, 0);
  assert(hwloc_bitmap_isequal(orig, expected));
  assert(!hwloc_bitmap_compare(orig, expected));

  /* actual non-trivial set */
  hwloc_bitmap_zero(orig);
  hwloc_bitmap_set(orig, 45);
  hwloc_bitmap_set(orig, 46);
  hwloc_bitmap_set(orig, 517);
  hwloc_bitmap_singlify(orig);
  hwloc_bitmap_zero(expected);
  hwloc_bitmap_set(expected, 45);
  assert(hwloc_bitmap_isequal(orig, expected));
  assert(!hwloc_bitmap_compare(orig, expected));

  hwloc_bitmap_free(orig);
  hwloc_bitmap_free(expected);

  return 0;
}
