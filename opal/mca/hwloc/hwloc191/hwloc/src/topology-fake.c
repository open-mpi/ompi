/*
 * Copyright © 2012 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>

#include <stdlib.h>

static struct hwloc_backend *
hwloc_fake_component_instantiate(struct hwloc_disc_component *component __hwloc_attribute_unused,
				 const void *_data1 __hwloc_attribute_unused,
				 const void *_data2 __hwloc_attribute_unused,
				 const void *_data3 __hwloc_attribute_unused)
{
  if (hwloc_plugin_check_namespace("fake", "hwloc_backend_alloc") < 0)
    return NULL;
  if (getenv("HWLOC_DEBUG_FAKE_COMPONENT"))
    printf("fake component instantiated\n");
  return NULL;
}

static struct hwloc_disc_component hwloc_fake_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC, /* so that it's always enabled when using the OS discovery */
  "fake",
  0, /* nothing to exclude */
  hwloc_fake_component_instantiate,
  100, /* make sure it's loaded before anything conflicting excludes it */
  NULL
};

HWLOC_DECLSPEC extern const struct hwloc_component hwloc_fake_component; /* never linked statically in the core */

const struct hwloc_component hwloc_fake_component = {
  HWLOC_COMPONENT_ABI,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_fake_disc_component
};
