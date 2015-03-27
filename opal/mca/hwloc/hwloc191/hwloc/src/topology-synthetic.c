/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2014 Inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux 1
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/misc.h>
#include <private/debug.h>

#include <limits.h>
#include <assert.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

struct hwloc_synthetic_level_data_s {
  unsigned arity;
  hwloc_obj_type_t type;
  unsigned depth; /* For caches/groups */
  hwloc_obj_cache_type_t cachetype; /* For caches */

  /* used while filling the topology */
  unsigned next_os_index; /* id of the next object for that level */
};

struct hwloc_synthetic_backend_data_s {
  /* synthetic backend parameters */
  char *string;
#define HWLOC_SYNTHETIC_MAX_DEPTH 128
  struct hwloc_synthetic_level_data_s level[HWLOC_SYNTHETIC_MAX_DEPTH];
};

/* Read from description a series of integers describing a symmetrical
   topology and update the hwloc_synthetic_backend_data_s accordingly.  On
   success, return zero.  */
static int
hwloc_backend_synthetic_init(struct hwloc_synthetic_backend_data_s *data,
			     const char *description)
{
  const char *pos, *next_pos;
  unsigned long item, count;
  unsigned i;
  int cache_depth = 0, group_depth = 0;
  int nb_machine_levels = 0, nb_node_levels = 0;
  int nb_pu_levels = 0;
  int verbose = 0;
  char *env = getenv("HWLOC_SYNTHETIC_VERBOSE");

  if (env)
    verbose = atoi(env);

  for (pos = description, count = 1; *pos; pos = next_pos) {
#define HWLOC_OBJ_TYPE_UNKNOWN ((hwloc_obj_type_t) -1)
    hwloc_obj_type_t type = HWLOC_OBJ_TYPE_UNKNOWN;
    int typedepth = -1;
    hwloc_obj_cache_type_t cachetype = (hwloc_obj_cache_type_t) -1;

    while (*pos == ' ')
      pos++;

    if (!*pos)
      break;

    if (*pos < '0' || *pos > '9') {
      if (hwloc_obj_type_sscanf(pos, &type, &typedepth, &cachetype, sizeof(cachetype)) < 0) {
	if (verbose)
	  fprintf(stderr, "Synthetic string with unknown object type at '%s'\n", pos);
	errno = EINVAL;
	return -1;
      }
      if (type == HWLOC_OBJ_MISC) {
	if (verbose)
	  fprintf(stderr, "Synthetic string with disallow object type at '%s'\n", pos);
	errno = EINVAL;
	return -1;
      }

      next_pos = strchr(pos, ':');
      if (!next_pos) {
	if (verbose)
	  fprintf(stderr,"Synthetic string doesn't have a `:' after object type at '%s'\n", pos);
	errno = EINVAL;
	return -1;
      }
      pos = next_pos + 1;
    }
    item = strtoul(pos, (char **)&next_pos, 0);
    if (next_pos == pos) {
      if (verbose)
	fprintf(stderr,"Synthetic string doesn't have a number of objects at '%s'\n", pos);
      errno = EINVAL;
      return -1;
    }

    if (count + 1 >= HWLOC_SYNTHETIC_MAX_DEPTH) {
      if (verbose)
	fprintf(stderr,"Too many synthetic levels, max %d\n", HWLOC_SYNTHETIC_MAX_DEPTH);
      errno = EINVAL;
      return -1;
    }
    if (item > UINT_MAX) {
      if (verbose)
	fprintf(stderr,"Too big arity, max %u\n", UINT_MAX);
      errno = EINVAL;
      return -1;
    }

    data->level[count-1].arity = (unsigned)item;
    data->level[count].type = type;
    data->level[count].depth = (unsigned) typedepth;
    data->level[count].cachetype = cachetype;
    count++;
  }

  if (count <= 0) {
    if (verbose)
      fprintf(stderr, "Synthetic string doesn't contain any object\n");
    errno = EINVAL;
    return -1;
  }

  for(i=count-1; i>0; i--) {
    struct hwloc_synthetic_level_data_s *curlevel = &data->level[i];
    hwloc_obj_type_t type;

    type = curlevel->type;

    if (type == HWLOC_OBJ_TYPE_UNKNOWN) {
      if (i == count-1)
	type = HWLOC_OBJ_PU;
      else {
	switch (data->level[i+1].type) {
	case HWLOC_OBJ_PU: type = HWLOC_OBJ_CORE; break;
	case HWLOC_OBJ_CORE: type = HWLOC_OBJ_CACHE; break;
	case HWLOC_OBJ_CACHE: type = HWLOC_OBJ_SOCKET; break;
	case HWLOC_OBJ_SOCKET: type = HWLOC_OBJ_NODE; break;
	case HWLOC_OBJ_NODE:
	case HWLOC_OBJ_MACHINE:
	case HWLOC_OBJ_GROUP: type = HWLOC_OBJ_GROUP; break;
	default:
	  assert(0);
	}
      }
      curlevel->type = type;
    }
    switch (type) {
      case HWLOC_OBJ_PU:
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

  if (!nb_pu_levels) {
    if (verbose)
      fprintf(stderr, "Synthetic string missing ending number of PUs\n");
    errno = EINVAL;
    return -1;
  }
  if (nb_pu_levels > 1) {
    if (verbose)
      fprintf(stderr, "Synthetic string can not have several PU levels\n");
    errno = EINVAL;
    return -1;
  }
  if (nb_node_levels > 1) {
    if (verbose)
      fprintf(stderr, "Synthetic string can not have several NUMA node levels\n");
    errno = EINVAL;
    return -1;
  }
  if (nb_machine_levels > 1) {
    if (verbose)
      fprintf(stderr, "Synthetic string can not have several machine levels\n");
    errno = EINVAL;
    return -1;
  }

  if (nb_machine_levels)
    data->level[0].type = HWLOC_OBJ_SYSTEM;
  else {
    data->level[0].type = HWLOC_OBJ_MACHINE;
    nb_machine_levels++;
  }

  if (cache_depth == 1)
    /* if there is a single cache level, make it L2 */
    cache_depth = 2;

  for (i=0; i<count; i++) {
    struct hwloc_synthetic_level_data_s *curlevel = &data->level[i];
    hwloc_obj_type_t type = curlevel->type;

    if (type == HWLOC_OBJ_GROUP) {
      if (curlevel->depth == (unsigned)-1)
	curlevel->depth = group_depth--;
    } else if (type == HWLOC_OBJ_CACHE) {
      if (curlevel->depth == (unsigned)-1)
	curlevel->depth = cache_depth--;
      if (curlevel->cachetype == (hwloc_obj_cache_type_t) -1)
	curlevel->cachetype = curlevel->depth == 1 ? HWLOC_OBJ_CACHE_DATA : HWLOC_OBJ_CACHE_UNIFIED;
    }
  }

  data->string = strdup(description);
  data->level[count-1].arity = 0;

  return 0;
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
		      struct hwloc_synthetic_backend_data_s *data,
		      int level, unsigned first_cpu,
		      hwloc_bitmap_t parent_cpuset)
{
  hwloc_obj_t obj;
  unsigned i;
  struct hwloc_synthetic_level_data_s *curlevel = &data->level[level];
  hwloc_obj_type_t type = curlevel->type;

  /* pre-hooks */
  switch (type) {
    case HWLOC_OBJ_GROUP:
      break;
    case HWLOC_OBJ_SYSTEM:
    case HWLOC_OBJ_BRIDGE:
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_OS_DEVICE:
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
    case HWLOC_OBJ_MISC:
    case HWLOC_OBJ_TYPE_MAX:
      /* Should never happen */
      assert(0);
      break;
  }

  obj = hwloc_alloc_setup_object(type, curlevel->next_os_index++);
  obj->cpuset = hwloc_bitmap_alloc();

  if (!curlevel->arity) {
    hwloc_bitmap_set(obj->cpuset, first_cpu++);
  } else {
    for (i = 0; i < curlevel->arity; i++)
      first_cpu = hwloc__look_synthetic(topology, data, level + 1, first_cpu, obj->cpuset);
  }

  if (type == HWLOC_OBJ_NODE) {
    obj->nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_set(obj->nodeset, obj->os_index);
  }

  hwloc_bitmap_or(parent_cpuset, parent_cpuset, obj->cpuset);

  /* post-hooks */
  switch (type) {
    case HWLOC_OBJ_GROUP:
      obj->attr->group.depth = curlevel->depth;
      break;
    case HWLOC_OBJ_SYSTEM:
    case HWLOC_OBJ_BRIDGE:
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_OS_DEVICE:
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
      obj->attr->cache.depth = curlevel->depth;
      obj->attr->cache.linesize = 64;
      obj->attr->cache.type = curlevel->cachetype;
      if (obj->attr->cache.depth == 1) {
	/* 32Kb in L1d */
	obj->attr->cache.size = 32*1024;
      } else {
	/* *4 at each level, starting from 1MB for L2, unified */
	obj->attr->cache.size = 256*1024 << (2*obj->attr->cache.depth);
      }
      break;
    case HWLOC_OBJ_CORE:
      break;
    case HWLOC_OBJ_PU:
      break;
    case HWLOC_OBJ_MISC:
    case HWLOC_OBJ_TYPE_MAX:
      /* Should never happen */
      assert(0);
      break;
  }

  hwloc_insert_object_by_cpuset(topology, obj);

  return first_cpu;
}

static int
hwloc_look_synthetic(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_synthetic_backend_data_s *data = backend->private_data;
  hwloc_bitmap_t cpuset = hwloc_bitmap_alloc();
  unsigned first_cpu = 0, i;

  assert(!topology->levels[0][0]->cpuset);

  hwloc_alloc_obj_cpusets(topology->levels[0][0]);

  topology->support.discovery->pu = 1;

  /* start with os_index 0 for each level */
  for (i = 0; data->level[i].arity > 0; i++)
    data->level[i].next_os_index = 0;
  /* ... including the last one */
  data->level[i].next_os_index = 0;

  /* update first level type according to the synthetic type array */
  topology->levels[0][0]->type = data->level[0].type;

  for (i = 0; i < data->level[0].arity; i++)
    first_cpu = hwloc__look_synthetic(topology, data, 1, first_cpu, cpuset);

  hwloc_bitmap_free(cpuset);

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "Synthetic");
  hwloc_obj_add_info(topology->levels[0][0], "SyntheticDescription", data->string);
  return 1;
}

static void
hwloc_synthetic_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_synthetic_backend_data_s *data = backend->private_data;
  free(data->string);
  free(data);
}

static struct hwloc_backend *
hwloc_synthetic_component_instantiate(struct hwloc_disc_component *component,
				      const void *_data1,
				      const void *_data2 __hwloc_attribute_unused,
				      const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  struct hwloc_synthetic_backend_data_s *data;
  int err;

  if (!_data1) {
    errno = EINVAL;
    goto out;
  }

  backend = hwloc_backend_alloc(component);
  if (!backend)
    goto out;

  data = malloc(sizeof(*data));
  if (!data) {
    errno = ENOMEM;
    goto out_with_backend;
  }

  err = hwloc_backend_synthetic_init(data, (const char *) _data1);
  if (err < 0)
    goto out_with_data;

  backend->private_data = data;
  backend->discover = hwloc_look_synthetic;
  backend->disable = hwloc_synthetic_backend_disable;
  backend->is_thissystem = 0;

  return backend;

 out_with_data:
  free(data);
 out_with_backend:
  free(backend);
 out:
  return NULL;
}

static struct hwloc_disc_component hwloc_synthetic_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  "synthetic",
  ~0,
  hwloc_synthetic_component_instantiate,
  30,
  NULL
};

const struct hwloc_component hwloc_synthetic_component = {
  HWLOC_COMPONENT_ABI,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_synthetic_disc_component
};
