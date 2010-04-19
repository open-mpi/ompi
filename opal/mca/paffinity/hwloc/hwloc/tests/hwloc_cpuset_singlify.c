/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* check hwloc_cpuset_singlify() */

int main(void)
{
  hwloc_cpuset_t orig, expected;

  orig = hwloc_cpuset_alloc();
  expected = hwloc_cpuset_alloc();

  /* empty set gives empty set */
  hwloc_cpuset_singlify(orig);
  assert(hwloc_cpuset_iszero(orig));

  /* full set gives first bit only */
  hwloc_cpuset_fill(orig);
  hwloc_cpuset_singlify(orig);
  hwloc_cpuset_zero(expected);
  hwloc_cpuset_set(expected, 0);
  assert(!hwloc_cpuset_compare(orig, expected));

  /* actual non-trivial set */
  hwloc_cpuset_zero(orig);
  hwloc_cpuset_set(orig, 45);
  hwloc_cpuset_set(orig, 46);
  hwloc_cpuset_set(orig, 517);
  hwloc_cpuset_singlify(orig);
  hwloc_cpuset_zero(expected);
  hwloc_cpuset_set(expected, 45);
  assert(!hwloc_cpuset_compare(orig, expected));

  hwloc_cpuset_free(orig);
  hwloc_cpuset_free(expected);

  return 0;
}
