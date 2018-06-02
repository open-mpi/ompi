/*
 * Copyright © 2012 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>

int main(void)
{
  hwloc_topology_t topology;

  putenv("HWLOC_COMPONENTS_VERBOSE=1");

  hwloc_topology_init(&topology);
  /* no load, to avoid spurious "enable" messages */
  hwloc_topology_destroy(topology);
  return 0;
}
