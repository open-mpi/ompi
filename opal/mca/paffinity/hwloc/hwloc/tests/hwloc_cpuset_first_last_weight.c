/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>

#include <assert.h>

/* check hwloc_cpuset_first(), _last(), _next() and _weight() */

int main(void)
{
  hwloc_cpuset_t set;
  int i, cpu, expected_cpu;

  /* empty set */
  set = hwloc_cpuset_alloc();
  assert(hwloc_cpuset_first(set) == -1);
  assert(hwloc_cpuset_last(set) == -1);
  assert(hwloc_cpuset_next(set, 0) == -1);
  assert(hwloc_cpuset_weight(set) == 0);

  /* full set */
  hwloc_cpuset_fill(set);
  assert(hwloc_cpuset_first(set) == 0);
  assert(hwloc_cpuset_last(set) == HWLOC_NBMAXCPUS-1);
  assert(hwloc_cpuset_next(set, 0) == 1);
  assert(hwloc_cpuset_next(set, 1) == 2);
  assert(hwloc_cpuset_next(set, 2) == 3);
  assert(hwloc_cpuset_next(set, 30) == 31);
  assert(hwloc_cpuset_next(set, 31) == 32);
  assert(hwloc_cpuset_next(set, 32) == 33);
  assert(hwloc_cpuset_next(set, 62) == 63);
  assert(hwloc_cpuset_next(set, 63) == 64);
  assert(hwloc_cpuset_next(set, 64) == 65);
  assert(hwloc_cpuset_next(set, HWLOC_NBMAXCPUS-3) == HWLOC_NBMAXCPUS-2);
  assert(hwloc_cpuset_next(set, HWLOC_NBMAXCPUS-2) == HWLOC_NBMAXCPUS-1);
  assert(hwloc_cpuset_next(set, HWLOC_NBMAXCPUS-1) == -1);
  assert(hwloc_cpuset_weight(set) == HWLOC_NBMAXCPUS);

  /* custom sets */
  hwloc_cpuset_zero(set);
  hwloc_cpuset_set_range(set, 36, 59);
  assert(hwloc_cpuset_first(set) == 36);
  assert(hwloc_cpuset_last(set) == 59);
  assert(hwloc_cpuset_next(set, 0) == 36);
  assert(hwloc_cpuset_next(set, 36) == 37);
  assert(hwloc_cpuset_next(set, 59) == -1);
  assert(hwloc_cpuset_weight(set) == 24);
  hwloc_cpuset_set_range(set, 136, 259);
  assert(hwloc_cpuset_first(set) == 36);
  assert(hwloc_cpuset_last(set) == 259);
  assert(hwloc_cpuset_next(set, 59) == 136);
  assert(hwloc_cpuset_next(set, 259) == -1);
  assert(hwloc_cpuset_weight(set) == 148);
  hwloc_cpuset_clr(set, 199);
  assert(hwloc_cpuset_first(set) == 36);
  assert(hwloc_cpuset_last(set) == 259);
  assert(hwloc_cpuset_next(set, 198) == 200);
  assert(hwloc_cpuset_next(set, 199) == 200);
  assert(hwloc_cpuset_weight(set) == 147);

  i = 0;
  hwloc_cpuset_foreach_begin(cpu, set) {
    if (0 <= i && i < 24)
      expected_cpu = i + 36;
    else if (24 <= i && i < 87)
      expected_cpu = i + 112;
    else if (87 <= i && i < 147)
      expected_cpu = i + 113;

    assert(expected_cpu == cpu);

    i++;
  } hwloc_cpuset_foreach_end();

  hwloc_cpuset_free(set);

  return 0;
}
