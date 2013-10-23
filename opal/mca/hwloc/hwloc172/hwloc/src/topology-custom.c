/*
 * Copyright © 2011-2012 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>

hwloc_obj_t
hwloc_custom_insert_group_object_by_parent(struct hwloc_topology *topology, hwloc_obj_t parent, int groupdepth)
{
  hwloc_obj_t obj;

  /* must be called between set_custom() and load(), so there's a single backend, the custom one */
  if (topology->is_loaded || !topology->backends || !topology->backends->is_custom) {
    errno = EINVAL;
    return NULL;
  }

  obj = hwloc_alloc_setup_object(HWLOC_OBJ_GROUP, -1);
  obj->attr->group.depth = groupdepth;
  hwloc_insert_object_by_parent(topology, parent, obj);
  /* insert_object_by_parent() doesn't merge during insert, so obj is still valid */

  return obj;
}

int
hwloc_custom_insert_topology(struct hwloc_topology *newtopology,
			     struct hwloc_obj *newparent,
			     struct hwloc_topology *oldtopology,
			     struct hwloc_obj *oldroot)
{
  /* must be called between set_custom() and load(), so there's a single backend, the custom one */
  if (newtopology->is_loaded || !newtopology->backends || !newtopology->backends->is_custom) {
    errno = EINVAL;
    return -1;
  }

  if (!oldtopology->is_loaded) {
    errno = EINVAL;
    return -1;
  }

  hwloc__duplicate_objects(newtopology, newparent, oldroot ? oldroot : oldtopology->levels[0][0]);
  return 0;
}

static int
hwloc_look_custom(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;

  assert(!topology->levels[0][0]->cpuset);

  if (!topology->levels[0][0]->first_child) {
    errno = EINVAL;
    return -1;
  }

  topology->levels[0][0]->type = HWLOC_OBJ_SYSTEM;
  return 1;
}

static struct hwloc_backend *
hwloc_custom_component_instantiate(struct hwloc_disc_component *component,
				   const void *_data1 __hwloc_attribute_unused,
				   const void *_data2 __hwloc_attribute_unused,
				   const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_custom;
  backend->is_custom = 1;
  backend->is_thissystem = 0;
  return backend;
}

static struct hwloc_disc_component hwloc_custom_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  "custom",
  ~0,
  hwloc_custom_component_instantiate,
  30,
  NULL
};

const struct hwloc_component hwloc_custom_component = {
  HWLOC_COMPONENT_ABI,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_custom_disc_component
};
