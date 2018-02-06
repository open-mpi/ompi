/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>

#include <hwloc.h>
#include <string.h>
#include <sys/stat.h>

#include "lstopo.h"

int
output_xml(struct lstopo_output *loutput, const char *filename)
{
  struct stat st;

  if (!filename || !strcasecmp(filename, "-.xml"))
    filename = "-";
  /* hwloc_topology_export_xml() writes to stdout if "-" is given */

  if (strcmp(filename, "-") && !stat(filename, &st) && !loutput->overwrite) {
    fprintf(stderr, "Failed to export XML to %s (%s)\n", filename, strerror(EEXIST));
    return -1;
  }

  if (hwloc_topology_export_xml(loutput->topology, filename, loutput->export_xml_flags) < 0) {
    fprintf(stderr, "Failed to export XML to %s (%s)\n", filename, strerror(errno));
    return -1;
  }

  return 0;
}
