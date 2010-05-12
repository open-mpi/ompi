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

#include <limits.h>
#include <assert.h>
#include <strings.h>

/* Read from DESCRIPTION a series of integers describing a symmetrical
   topology and update `topology->synthetic_description' accordingly.  On
   success, return zero.  */
int
hwloc_backend_synthetic_init(struct hwloc_topology *topology, const char *description)
{
  const char *pos, *next_pos;
  unsigned long item, count;
  unsigned i;
  int cache_depth = 0, group_depth = 0;
  int nb_machine_levels = 0, nb_node_levels = 0;
  int nb_pu_levels = 0;
  int nb_pu = 1;

  assert(topology->backend_type == HWLOC_BACKEND_NONE);

  for (pos = description, count = 1; *pos; pos = next_pos) {
#define HWLOC_OBJ_TYPE_UNKNOWN ((unsigned) -1)
    hwloc_obj_type_t type = HWLOC_OBJ_TYPE_UNKNOWN;

    while (*pos == ' ')
      pos++;

    if (!*pos)
      break;

    if (*pos < '0' || *pos > '9') {
      if (!hwloc_namecoloncmp(pos, "machines", 2)) {
	type = HWLOC_OBJ_MACHINE;
      } else if (!hwloc_namecoloncmp(pos, "nodes", 1))
	type = HWLOC_OBJ_NODE;
      else if (!hwloc_namecoloncmp(pos, "sockets", 1))
	type = HWLOC_OBJ_SOCKET;
      else if (!hwloc_namecoloncmp(pos, "cores", 2))
	type = HWLOC_OBJ_CORE;
      else if (!hwloc_namecoloncmp(pos, "caches", 2))
	type = HWLOC_OBJ_CACHE;
      else if (!hwloc_namecoloncmp(pos, "pus", 1) || !hwloc_namecoloncmp(pos, "procs", 1) /* backward compatiblity with 0.9 */)
	type = HWLOC_OBJ_PU;
      else if (!hwloc_namecoloncmp(pos, "misc", 2))
	type = HWLOC_OBJ_MISC;
      else if (!hwloc_namecoloncmp(pos, "group", 2))
	type = HWLOC_OBJ_GROUP;
      else
        fprintf(stderr, "Unknown object type `%s'\n", pos);

      next_pos = strchr(pos, ':');
      if (!next_pos) {
	fprintf(stderr,"synthetic string doesn't have a `:' after object type at '%s'\n", pos);
	return -1;
      }
      pos = next_pos + 1;
    }
    item = strtoul(pos, (char **)&next_pos, 0);
    if (next_pos == pos) {
      fprintf(stderr,"synthetic string doesn't have a number of objects at '%s'\n", pos);
      return -1;
    }

    if (count + 1 >= HWLOC_SYNTHETIC_MAX_DEPTH) {
      fprintf(stderr,"Too many synthetic levels, max %d\n", HWLOC_SYNTHETIC_MAX_DEPTH);
      return -1;
    }
    if (item > UINT_MAX) {
      fprintf(stderr,"Too big arity, max %u\n", UINT_MAX);
      return -1;
    }

    nb_pu *= item;
    if (nb_pu > HWLOC_NBMAXCPUS) {
      fprintf(stderr, "To many PUs, max %d\n", HWLOC_NBMAXCPUS);
      return -1;
    }

    topology->backend_params.synthetic.arity[count-1] = (unsigned)item;
    topology->backend_params.synthetic.type[count] = type;
    topology->backend_params.synthetic.id[count] = 0;
    count++;
  }

  if (count <= 0) {
    fprintf(stderr,"synthetic string doesn't contain any object\n");
    return -1;
  }

  for(i=count-1; i>0; i--) {
    hwloc_obj_type_t type;

    type = topology->backend_params.synthetic.type[i];

    if (type == HWLOC_OBJ_TYPE_UNKNOWN) {
      if (i == count-1)
	type = HWLOC_OBJ_PU;
      else {
	switch (topology->backend_params.synthetic.type[i+1]) {
	case HWLOC_OBJ_PU: type = HWLOC_OBJ_CORE; break;
	case HWLOC_OBJ_CORE: type = HWLOC_OBJ_CACHE; break;
	case HWLOC_OBJ_CACHE: type = HWLOC_OBJ_SOCKET; break;
	case HWLOC_OBJ_SOCKET: type = HWLOC_OBJ_NODE; break;
	case HWLOC_OBJ_NODE:
	case HWLOC_OBJ_GROUP: type = HWLOC_OBJ_GROUP; break;
	case HWLOC_OBJ_MACHINE:
	case HWLOC_OBJ_MISC: type = HWLOC_OBJ_MISC; break;
	default:
	  assert(0);
	}
      }
      topology->backend_params.synthetic.type[i] = type;
    }
    switch (type) {
      case HWLOC_OBJ_PU:
	if (nb_pu_levels) {
	    fprintf(stderr,"synthetic string can not have several PU levels\n");
	    return -1;
	}
	nb_pu_levels++;
	break;
      case HWLOC_OBJ_CACHE:
	cache_depth++;
	break;
      case HWLOC_OBJ_GROUP:
	group_depth++;
	break;
      case HWLOC_OBJ_NODE:
	nb_node_levels++;
	break;
      case HWLOC_OBJ_MACHINE:
	nb_machine_levels++;
	break;
      default:
	break;
    }
  }

  if (nb_pu_levels > 1) {
    fprintf(stderr,"synthetic string can not have several PU levels\n");
    return -1;
  }
  if (nb_node_levels > 1) {
    fprintf(stderr,"synthetic string can not have several NUMA node levels\n");
    return -1;
  }
  if (nb_machine_levels > 1) {
    fprintf(stderr,"synthetic string can not have several machine levels\n");
    return -1;
  }

  if (nb_machine_levels)
    topology->backend_params.synthetic.type[0] = HWLOC_OBJ_SYSTEM;
  else {
    topology->backend_params.synthetic.type[0] = HWLOC_OBJ_MACHINE;
    nb_machine_levels++;
  }
  topology->backend_params.synthetic.id[0] = 0;

  if (cache_depth == 1)
    /* if there is a single cache level, make it L2 */
    cache_depth = 2;

  for (i=0; i<count; i++) {
    hwloc_obj_type_t type = topology->backend_params.synthetic.type[i];

    if (type == HWLOC_OBJ_GROUP)
      topology->backend_params.synthetic.depth[i] = group_depth--;
    else if (type == HWLOC_OBJ_CACHE)
      topology->backend_params.synthetic.depth[i] = cache_depth--;
  }

  topology->backend_type = HWLOC_BACKEND_SYNTHETIC;
  topology->backend_params.synthetic.arity[count-1] = 0;
  topology->is_thissystem = 0;

  return 0;
}

void
hwloc_backend_synthetic_exit(struct hwloc_topology *topology)
{
  assert(topology->backend_type == HWLOC_BACKEND_SYNTHETIC);
  topology->backend_type = HWLOC_BACKEND_NONE;
}

/*
 * Recursively build objects whose cpu start at first_cpu
 * - level gives where to look in the type, arity and id arrays
 * - the id array is used as a variable to get unique IDs for a given level.
 * - generated memory should be added to *memory_kB.
 * - generated cpus should be added to parent_cpuset.
 * - next cpu number to be used should be returned.
 */
static unsigned
hwloc__look_synthetic(struct hwloc_topology *topology,
    int level, unsigned first_cpu,
    hwloc_cpuset_t parent_cpuset)
{
  hwloc_obj_t obj;
  unsigned i;
  hwloc_obj_type_t type = topology->backend_params.synthetic.type[level];

  /* pre-hooks */
  switch (type) {
    case HWLOC_OBJ_MISC:
      break;
    case HWLOC_OBJ_GROUP:
      break;
    case HWLOC_OBJ_SYSTEM:
      /* Shouldn't happen.  */
      abort();
      break;
    case HWLOC_OBJ_MACHINE:
      break;
    case HWLOC_OBJ_NODE:
      break;
    case HWLOC_OBJ_SOCKET:
      break;
    case HWLOC_OBJ_CACHE:
      break;
    case HWLOC_OBJ_CORE:
      break;
    case HWLOC_OBJ_PU:
      break;
  }

  obj = hwloc_alloc_setup_object(type, topology->backend_params.synthetic.id[level]++);
  obj->cpuset = hwloc_cpuset_alloc();

  if (!topology->backend_params.synthetic.arity[level]) {
    hwloc_cpuset_set(obj->cpuset, first_cpu++);
  } else {
    for (i = 0; i < topology->backend_params.synthetic.arity[level]; i++)
      first_cpu = hwloc__look_synthetic(topology, level + 1, first_cpu, obj->cpuset);
  }

  if (type == HWLOC_OBJ_NODE) {
    obj->nodeset = hwloc_cpuset_alloc();
    hwloc_cpuset_set(obj->nodeset, obj->os_index);
  }

  hwloc_insert_object_by_cpuset(topology, obj);

  hwloc_cpuset_or(parent_cpuset, parent_cpuset, obj->cpuset);

  /* post-hooks */
  switch (type) {
    case HWLOC_OBJ_MISC:
      break;
    case HWLOC_OBJ_GROUP:
      obj->attr->group.depth = topology->backend_params.synthetic.depth[level];
      break;
    case HWLOC_OBJ_SYSTEM:
      abort();
      break;
    case HWLOC_OBJ_MACHINE:
      break;
    case HWLOC_OBJ_NODE:
      /* 1GB in memory nodes, 256k 4k-pages.  */
      obj->memory.local_memory = 1024*1024*1024;
      obj->memory.page_types_len = 1;
      obj->memory.page_types = malloc(sizeof(*obj->memory.page_types));
      memset(obj->memory.page_types, 0, sizeof(*obj->memory.page_types));
      obj->memory.page_types[0].size = 4096;
      obj->memory.page_types[0].count = 256*1024;
      break;
    case HWLOC_OBJ_SOCKET:
      break;
    case HWLOC_OBJ_CACHE:
      obj->attr->cache.depth = topology->backend_params.synthetic.depth[level];
      if (obj->attr->cache.depth == 1)
	/* 32Kb in L1 */
	obj->attr->cache.size = 32*1024;
      else
	/* *4 at each level, starting from 1MB for L2 */
	obj->attr->cache.size = 256*1024 << (2*obj->attr->cache.depth);
      break;
    case HWLOC_OBJ_CORE:
      break;
    case HWLOC_OBJ_PU:
      break;
  }

  return first_cpu;
}

void
hwloc_look_synthetic(struct hwloc_topology *topology)
{
  hwloc_cpuset_t cpuset = hwloc_cpuset_alloc();
  unsigned first_cpu = 0, i;

  topology->support.discovery->pu = 1;

  /* update first level type according to the synthetic type array */
  topology->levels[0][0]->type = topology->backend_params.synthetic.type[0];

  for (i = 0; i < topology->backend_params.synthetic.arity[0]; i++)
    first_cpu = hwloc__look_synthetic(topology, 1, first_cpu, cpuset);

  hwloc_cpuset_free(cpuset);
}

