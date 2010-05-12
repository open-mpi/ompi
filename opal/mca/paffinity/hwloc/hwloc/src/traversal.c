/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/misc.h>
#include <private/debug.h>

int
hwloc_get_type_depth (struct hwloc_topology *topology, hwloc_obj_type_t type)
{
  return topology->type_depth[type];
}

hwloc_obj_type_t
hwloc_get_depth_type (hwloc_topology_t topology, unsigned depth)
{
  if (depth >= topology->nb_levels)
    return (hwloc_obj_type_t) -1;
  return topology->levels[depth][0]->type;
}

unsigned
hwloc_get_nbobjs_by_depth (struct hwloc_topology *topology, unsigned depth)
{
  if (depth >= topology->nb_levels)
    return 0;
  return topology->level_nbobjects[depth];
}

struct hwloc_obj *
hwloc_get_obj_by_depth (struct hwloc_topology *topology, unsigned depth, unsigned idx)
{
  if (depth >= topology->nb_levels)
    return NULL;
  if (idx >= topology->level_nbobjects[depth])
    return NULL;
  return topology->levels[depth][idx];
}

unsigned hwloc_get_closest_objs (struct hwloc_topology *topology, struct hwloc_obj *src, struct hwloc_obj **objs, unsigned max)
{
  struct hwloc_obj *parent, *nextparent, **src_objs;
  int i,src_nbobjects;
  unsigned stored = 0;

  src_nbobjects = topology->level_nbobjects[src->depth];
  src_objs = topology->levels[src->depth];

  parent = src;
  while (stored < max) {
    while (1) {
      nextparent = parent->parent;
      if (!nextparent)
	goto out;
      if (!hwloc_cpuset_isequal(parent->cpuset, nextparent->cpuset))
	break;
      parent = nextparent;
    }

    /* traverse src's objects and find those that are in nextparent and were not in parent */
    for(i=0; i<src_nbobjects; i++) {
      if (hwloc_cpuset_isincluded(src_objs[i]->cpuset, nextparent->cpuset)
	  && !hwloc_cpuset_isincluded(src_objs[i]->cpuset, parent->cpuset)) {
	objs[stored++] = src_objs[i];
	if (stored == max)
	  goto out;
      }
    }
    parent = nextparent;
  }

 out:
  return stored;
}

static int
hwloc__get_largest_objs_inside_cpuset (struct hwloc_obj *current, hwloc_const_cpuset_t set,
				       struct hwloc_obj ***res, int *max)
{
  int gotten = 0;
  unsigned i;

  /* the caller must ensure this */
  if (*max <= 0)
    return 0;

  if (hwloc_cpuset_isequal(current->cpuset, set)) {
    **res = current;
    (*res)++;
    (*max)--;
    return 1;
  }

  for (i=0; i<current->arity; i++) {
    hwloc_cpuset_t subset = hwloc_cpuset_dup(set);
    int ret;

    /* split out the cpuset part corresponding to this child and see if there's anything to do */
    hwloc_cpuset_and(subset, subset, current->children[i]->cpuset);
    if (hwloc_cpuset_iszero(subset)) {
      hwloc_cpuset_free(subset);
      continue;
    }

    ret = hwloc__get_largest_objs_inside_cpuset (current->children[i], subset, res, max);
    gotten += ret;
    hwloc_cpuset_free(subset);

    /* if no more room to store remaining objects, return what we got so far */
    if (!*max)
      break;
  }

  return gotten;
}

int
hwloc_get_largest_objs_inside_cpuset (struct hwloc_topology *topology, hwloc_const_cpuset_t set,
				      struct hwloc_obj **objs, int max)
{
  struct hwloc_obj *current = topology->levels[0][0];

  if (!hwloc_cpuset_isincluded(set, current->cpuset))
    return -1;

  if (max <= 0)
    return 0;

  return hwloc__get_largest_objs_inside_cpuset (current, set, &objs, &max);
}

const char *
hwloc_obj_type_string (hwloc_obj_type_t obj)
{
  switch (obj)
    {
    case HWLOC_OBJ_SYSTEM: return "System";
    case HWLOC_OBJ_MACHINE: return "Machine";
    case HWLOC_OBJ_MISC: return "Misc";
    case HWLOC_OBJ_GROUP: return "Group";
    case HWLOC_OBJ_NODE: return "NUMANode";
    case HWLOC_OBJ_SOCKET: return "Socket";
    case HWLOC_OBJ_CACHE: return "Cache";
    case HWLOC_OBJ_CORE: return "Core";
    case HWLOC_OBJ_PU: return "PU";
    default: return "Unknown";
    }
}

hwloc_obj_type_t
hwloc_obj_type_of_string (const char * string)
{
  if (!strcasecmp(string, "System")) return HWLOC_OBJ_SYSTEM;
  if (!strcasecmp(string, "Machine")) return HWLOC_OBJ_MACHINE;
  if (!strcasecmp(string, "Misc")) return HWLOC_OBJ_MISC;
  if (!strcasecmp(string, "Group")) return HWLOC_OBJ_GROUP;
  if (!strcasecmp(string, "NUMANode") || !strcasecmp(string, "Node")) return HWLOC_OBJ_NODE;
  if (!strcasecmp(string, "Socket")) return HWLOC_OBJ_SOCKET;
  if (!strcasecmp(string, "Cache")) return HWLOC_OBJ_CACHE;
  if (!strcasecmp(string, "Core")) return HWLOC_OBJ_CORE;
  if (!strcasecmp(string, "PU") || !strcasecmp(string, "proc") /* backward compatiliby with 0.9 */) return HWLOC_OBJ_PU;
  return (hwloc_obj_type_t) -1;
}

#define hwloc_memory_size_printf_value(_size, _verbose) \
  ((_size) < (10ULL<<20) || _verbose ? (((_size)>>9)+1)>>1 : (_size) < (10ULL<<30) ? (((_size)>>19)+1)>>1 : (((_size)>>29)+1)>>1)
#define hwloc_memory_size_printf_unit(_size, _verbose) \
  ((_size) < (10ULL<<20) || _verbose ? "KB" : (_size) < (10ULL<<30) ? "MB" : "GB")

int
hwloc_obj_type_snprintf(char * __hwloc_restrict string, size_t size, hwloc_obj_t obj, int verbose)
{
  hwloc_obj_type_t type = obj->type;
  switch (type) {
  case HWLOC_OBJ_MISC:
  case HWLOC_OBJ_SYSTEM:
  case HWLOC_OBJ_MACHINE:
  case HWLOC_OBJ_NODE:
  case HWLOC_OBJ_SOCKET:
  case HWLOC_OBJ_CORE:
  case HWLOC_OBJ_PU:
    return hwloc_snprintf(string, size, "%s", hwloc_obj_type_string(type));
  case HWLOC_OBJ_CACHE:
    return hwloc_snprintf(string, size, "L%u%s", obj->attr->cache.depth, verbose ? hwloc_obj_type_string(type): "");
  case HWLOC_OBJ_GROUP:
	  /* TODO: more pretty presentation? */
    return hwloc_snprintf(string, size, "%s%u", hwloc_obj_type_string(type), obj->attr->group.depth);
  default:
    *string = '\0';
    return 0;
  }
}

int
hwloc_obj_attr_snprintf(char * __hwloc_restrict string, size_t size, hwloc_obj_t obj, const char * separator, int verbose)
{
  char memory[64] = "";
  char specific[64] = "";
  const char *specificseparator;

  if (verbose) {
    if (obj->memory.local_memory)
      hwloc_snprintf(memory, sizeof(memory), "local=%lu%s%stotal=%lu%s",
		     (unsigned long) hwloc_memory_size_printf_value(obj->memory.total_memory, verbose),
		     hwloc_memory_size_printf_unit(obj->memory.total_memory, verbose),
		     separator,
		     (unsigned long) hwloc_memory_size_printf_value(obj->memory.local_memory, verbose),
		     hwloc_memory_size_printf_unit(obj->memory.local_memory, verbose));
    else if (obj->memory.total_memory)
      hwloc_snprintf(memory, sizeof(memory), "total=%lu%s",
		     (unsigned long) hwloc_memory_size_printf_value(obj->memory.total_memory, verbose),
		     hwloc_memory_size_printf_unit(obj->memory.total_memory, verbose));
  } else {
    if (obj->memory.total_memory)
      hwloc_snprintf(memory, sizeof(memory), "%lu%s",
		     (unsigned long) hwloc_memory_size_printf_value(obj->memory.total_memory, verbose),
		     hwloc_memory_size_printf_unit(obj->memory.total_memory, verbose));
  }

  switch (obj->type) {
  case HWLOC_OBJ_MACHINE:
    if (verbose)
      hwloc_snprintf(specific, sizeof(specific), "%s%s%s",
		     obj->attr->machine.dmi_board_vendor ? obj->attr->machine.dmi_board_vendor : "",
		     obj->attr->machine.dmi_board_vendor && obj->attr->machine.dmi_board_name ? separator : "",
		     obj->attr->machine.dmi_board_name ? obj->attr->machine.dmi_board_name : "");
    break;
  case HWLOC_OBJ_CACHE:
    hwloc_snprintf(specific, sizeof(specific), "%lu%s",
		   (unsigned long) hwloc_memory_size_printf_value(obj->attr->cache.size, verbose),
		   hwloc_memory_size_printf_unit(obj->attr->cache.size, verbose));
    break;
  default:
    break;
  }

  /* does the type-specific attribute string need separator prefix ? */
  specificseparator = *memory && *specific ? separator : "";

  return hwloc_snprintf(string, size, "%s%s%s",
			memory,
			specificseparator, specific);
}


int
hwloc_obj_snprintf(char *string, size_t size,
    struct hwloc_topology *topology __hwloc_attribute_unused, struct hwloc_obj *l, const char *_indexprefix, int verbose)
{
  const char *indexprefix = _indexprefix ? _indexprefix : "#";
  char os_index[12] = "";
  char type[64];
  char attr[128];
  int attrlen;

  if (l->os_index != (unsigned) -1) {
    hwloc_snprintf(os_index, 12, "%s%u", indexprefix, l->os_index);
  }

  hwloc_obj_type_snprintf(type, sizeof(type), l, verbose);
  attrlen = hwloc_obj_attr_snprintf(attr, sizeof(attr), l, " ", verbose);

  if (attrlen > 0)
    return hwloc_snprintf(string, size, "%s%s(%s)", type, os_index, attr);
  else
    return hwloc_snprintf(string, size, "%s%s", type, os_index);
}

int hwloc_obj_cpuset_snprintf(char *str, size_t size, size_t nobj, struct hwloc_obj * const *objs)
{
  hwloc_cpuset_t set = hwloc_cpuset_alloc();
  int res;
  unsigned i;

  hwloc_cpuset_zero(set);
  for(i=0; i<nobj; i++)
    hwloc_cpuset_or(set, set, objs[i]->cpuset);

  res = hwloc_cpuset_snprintf(str, size, set);
  hwloc_cpuset_free(set);
  return res;
}
