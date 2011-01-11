/*
 * Copyright © 2010 INRIA
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <assert.h>
#include <libxml/xmlstring.h>
#include <libxml/xmlmemory.h>
#include <private/config.h>
#include <hwloc.h>

/* check the CUDA Driver API helpers */

int main(void)
{
  hwloc_topology_t topology;
  int size1, size2;
  char *buf1, *buf2;
  int err = 0;

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);
  assert(hwloc_topology_is_thissystem(topology));
  hwloc_topology_export_xmlbuffer(topology, &buf1, &size1);
  hwloc_topology_destroy(topology);
  printf("exported to buffer %p length %d\n", buf1, size1);

  hwloc_topology_init(&topology);
  hwloc_topology_set_xmlbuffer(topology, buf1, size1);
  hwloc_topology_load(topology);
  assert(!hwloc_topology_is_thissystem(topology));
  hwloc_topology_export_xmlbuffer(topology, &buf2, &size2);
  hwloc_topology_destroy(topology);
  printf("re-exported to buffer %p length %d\n", buf2, size2);

  if (strcmp(buf1, buf2)) {
    printf("### First exported buffer is:\n");
    printf("%s", buf1);
    printf("### End of first export buffer\n");
    printf("### Second exported buffer is:\n");
    printf("%s", buf2);
    printf("### End of second export buffer\n");
    err = 1;
  }

  xmlFree(BAD_CAST buf1);
  xmlFree(BAD_CAST buf2);

  return err;
}
