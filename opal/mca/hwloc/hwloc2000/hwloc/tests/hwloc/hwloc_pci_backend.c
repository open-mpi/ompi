/*
 * Copyright © 2011-2016 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

/* check whether the PCI backend behaves as expected wrt to thissystem, XML, flags, ... */

static int get_nb_pcidev(int iolevel, int thissystem,
			 const char *xmlbuf, int xmlbuflen)
{
  int nb;
  hwloc_topology_t topology;
  enum hwloc_type_filter_e filter;
  if (iolevel == 0)
    filter = HWLOC_TYPE_FILTER_KEEP_NONE;
  else if (iolevel == 1)
    filter = HWLOC_TYPE_FILTER_KEEP_IMPORTANT;
  else
    filter = HWLOC_TYPE_FILTER_KEEP_ALL;

  if (thissystem)
    putenv("HWLOC_THISSYSTEM=1");
  else
    putenv("HWLOC_THISSYSTEM=0");
  hwloc_topology_init(&topology);
  hwloc_topology_set_io_types_filter(topology, filter);
  if (xmlbuf)
    hwloc_topology_set_xmlbuffer(topology, xmlbuf, xmlbuflen);
  hwloc_topology_load(topology);

  nb = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_PCI_DEVICE);

  hwloc_topology_destroy(topology);

  return nb;
}

int main(void)
{
  hwloc_topology_t topology;
  char *xmlbuf;
  int xmlbuflen;
  int nb, nbnormal, nbwhole;

  hwloc_topology_init(&topology);
  hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_ALL);
  hwloc_topology_load(topology);
  if (hwloc_topology_export_xmlbuffer(topology, &xmlbuf, &xmlbuflen, 0) < 0)
    printf("XML buffer export failed (%s), ignoring\n", strerror(errno));

  /* with HWLOC_THISSYSTEM=1 */
  nb = get_nb_pcidev(0, 1, NULL, 0);
  assert(!nb);
  nbnormal = get_nb_pcidev(1, 1, NULL, 0);
  assert(nbnormal >= 0); /* may get more objects */
  nbwhole = get_nb_pcidev(2, 1, NULL, 0);
  assert(nbwhole >= nbnormal); /* will get at least as much objects */

  /* XML with with HWLOC_THISSYSTEM=1, should get as many object as a native load */
  nb = get_nb_pcidev(0, 1, xmlbuf, xmlbuflen);
  assert(!nb);
  nb = get_nb_pcidev(1, 1, xmlbuf, xmlbuflen);
  assert(nb == nbnormal);
  nb = get_nb_pcidev(2, 1, xmlbuf, xmlbuflen);
  assert(nb == nbwhole);

  /* XML with with HWLOC_THISSYSTEM=0,  should get as many object as a native load */
  nb = get_nb_pcidev(0, 0, xmlbuf, xmlbuflen);
  assert(!nb);
  nb = get_nb_pcidev(1, 0, xmlbuf, xmlbuflen);
  assert(nb == nbnormal);
  nb = get_nb_pcidev(2, 0, xmlbuf, xmlbuflen);
  assert(nb == nbwhole);

  hwloc_free_xmlbuffer(topology, xmlbuf);
  hwloc_topology_destroy(topology);

  return 0;
}
