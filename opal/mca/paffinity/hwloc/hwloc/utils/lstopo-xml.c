/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>

#ifdef HWLOC_HAVE_XML

#include <hwloc.h>
#include <string.h>

#include "lstopo.h"

void output_xml(hwloc_topology_t topology, const char *filename, int logical __hwloc_attribute_unused, int verbose_mode __hwloc_attribute_unused)
{
  if (!strcasecmp(filename, "-.xml"))
    filename = "-";

  hwloc_topology_export_xml(topology, filename);
}

#endif /* HWLOC_HAVE_XML */
