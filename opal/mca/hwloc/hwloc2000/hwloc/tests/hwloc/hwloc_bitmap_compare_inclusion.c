/*
 * Copyright © 2015-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>
#include <private/misc.h> /* for hwloc_bitmap_compare_inclusion() */

#include <assert.h>

/* check hwloc_bitmap_compare_inclusion() */

static int check_compare(hwloc_const_bitmap_t set1, hwloc_const_bitmap_t set2)
{
  int res = hwloc_bitmap_compare_inclusion(set1, set2);

  if (hwloc_bitmap_iszero(set1)) {
    if (hwloc_bitmap_iszero(set2)) {
      assert(res == HWLOC_BITMAP_EQUAL);
    } else {
      assert(res == HWLOC_BITMAP_INCLUDED);
    }
  } else if (hwloc_bitmap_iszero(set2)) {
    assert(res == HWLOC_BITMAP_CONTAINS);

  } else if (hwloc_bitmap_isequal(set1, set2)) {
    assert(res == HWLOC_BITMAP_EQUAL);
  } else if (hwloc_bitmap_isincluded(set1, set2)) {
    assert(res == HWLOC_BITMAP_INCLUDED);
  } else if (hwloc_bitmap_isincluded(set2, set1)) {
    assert(res == HWLOC_BITMAP_CONTAINS);
  } else if (hwloc_bitmap_intersects(set1, set2)) {
    assert(res == HWLOC_BITMAP_INTERSECTS);
  } else {
    assert(res == HWLOC_BITMAP_DIFFERENT);
  }

  return res;
}

int main(void)
{
#define N 10
  hwloc_bitmap_t sets[N];
  unsigned i,j;
  unsigned stats[5];

  memset(stats, 0, sizeof(stats));
  
  sets[0] = hwloc_bitmap_alloc();
  
  sets[1] = hwloc_bitmap_alloc_full();

  sets[2] = hwloc_bitmap_alloc_full();
  hwloc_bitmap_clr_range(sets[2], 56, 129);

  sets[3] = hwloc_bitmap_alloc_full();
  hwloc_bitmap_clr_range(sets[3], 0, 33);
  hwloc_bitmap_clr_range(sets[3], 50, 135);

  sets[4] = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(sets[4], 0, 24);

  sets[5] = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(sets[5], 0, 178);

  sets[6] = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(sets[6], 0, 191);
  hwloc_bitmap_set_range(sets[6], 1031, 2035);

  sets[7] = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(sets[7], 324, 456);
  
  sets[8] = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(sets[8], 323, 455);
  hwloc_bitmap_set_range(sets[8], 136, 177);
  
  sets[9] = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(sets[9], 3, 6);

  for(i=0; i<N; i++) {
    for(j=0; j<N; j++) {
      int res = check_compare(sets[i], sets[j]);
      stats[res]++;
    }
  }

  for(i=0; i<N; i++)
    hwloc_bitmap_free(sets[i]);

  printf("got %u EQUAL\n", stats[0]);
  printf("got %u INCLUDED\n", stats[1]);
  printf("got %u CONTAINS\n", stats[2]);
  printf("got %u INTERSECTS\n", stats[3]);
  printf("got %u DIFFERENT\n", stats[4]);
  
  return 0;
}
