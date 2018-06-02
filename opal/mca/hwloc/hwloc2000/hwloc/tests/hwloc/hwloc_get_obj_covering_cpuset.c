/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* check hwloc_get_obj_covering_cpuset() */

#define SYNTHETIC_TOPOLOGY_DESCRIPTION "6 5 4 3 2" /* 736bits wide topology */

#define GIVEN_CPUSET_STRING "0x0,0x0fff,0xf0000000"
#define EXPECTED_CPUSET_STRING "0x0000ffff,0xff000000"
#define GIVEN_LARGESPLIT_CPUSET_STRING "0x8000,,,,,,,,,,,,,,,,,,,,,,0x1" /* first and last(735th) bit set */
#define GIVEN_TOOLARGE_CPUSET_STRING "0x10000,,,,,,,,,,,,,,,,,,,,,,0x0" /* 736th bit is too large for the 720-wide topology */

int main(void)
{
  hwloc_topology_t topology;
  char *string = NULL;
  hwloc_obj_t obj;
  hwloc_bitmap_t set;

  hwloc_topology_init(&topology);
  hwloc_topology_set_synthetic(topology, SYNTHETIC_TOPOLOGY_DESCRIPTION);
  hwloc_topology_load(topology);
  set = hwloc_bitmap_alloc();

  hwloc_bitmap_sscanf(set, GIVEN_CPUSET_STRING);
  obj = hwloc_get_obj_covering_cpuset(topology, set);

  assert(obj);
  fprintf(stderr, "found covering object type %s covering cpuset %s\n",
	  hwloc_obj_type_string(obj->type), GIVEN_CPUSET_STRING);
  assert(hwloc_bitmap_isincluded(set, obj->cpuset));

  hwloc_bitmap_asprintf(&string, obj->cpuset);
  fprintf(stderr, "covering object of %s is %s, expected %s\n",
	  GIVEN_CPUSET_STRING, string, EXPECTED_CPUSET_STRING);
  if (strcmp(EXPECTED_CPUSET_STRING, string))
    assert(0);
  free(string);

  hwloc_bitmap_sscanf(set, GIVEN_LARGESPLIT_CPUSET_STRING);
  obj = hwloc_get_obj_covering_cpuset(topology, set);
  assert(obj == hwloc_get_root_obj(topology));
  fprintf(stderr, "found system as covering object of first+last cpus cpuset %s\n",
	  GIVEN_LARGESPLIT_CPUSET_STRING);

  hwloc_bitmap_sscanf(set, GIVEN_TOOLARGE_CPUSET_STRING);
  obj = hwloc_get_obj_covering_cpuset(topology, set);
  assert(!obj);
  fprintf(stderr, "found no covering object for too-large cpuset %s\n",
	  GIVEN_TOOLARGE_CPUSET_STRING);

  hwloc_bitmap_free(set);
  hwloc_topology_destroy(topology);

  return EXIT_SUCCESS;
}
