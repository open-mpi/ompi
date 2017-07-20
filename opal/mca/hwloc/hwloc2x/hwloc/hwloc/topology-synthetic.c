/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux
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
  unsigned long totalwidth;
  hwloc_obj_type_t type;
  unsigned depth; /* For caches/groups */
  hwloc_obj_cache_type_t cachetype; /* For caches */
  hwloc_uint64_t memorysize; /* For caches/memory */

  /* the indexes= attribute before parsing */
  const char *index_string;
  unsigned long index_string_length;
  /* the array of explicit indexes after parsing */
  unsigned *index_array;

  /* used while filling the topology */
  unsigned next_os_index; /* id of the next object for that level */
};

struct hwloc_synthetic_backend_data_s {
  /* synthetic backend parameters */
  char *string;
#define HWLOC_SYNTHETIC_MAX_DEPTH 128
  struct hwloc_synthetic_level_data_s level[HWLOC_SYNTHETIC_MAX_DEPTH];
};

struct hwloc_synthetic_intlv_loop_s {
  unsigned step;
  unsigned nb;
  unsigned level_depth;
};

static void
hwloc_synthetic_process_level_indexes(struct hwloc_synthetic_backend_data_s *data,
				      unsigned curleveldepth,
				      int verbose)
{
  struct hwloc_synthetic_level_data_s *curlevel = &data->level[curleveldepth];
  unsigned long total = curlevel->totalwidth;
  const char *attr = curlevel->index_string;
  unsigned long length = curlevel->index_string_length;
  unsigned *array = NULL;
  struct hwloc_synthetic_intlv_loop_s * loops = NULL;
  size_t i;

  if (!attr)
    return;

  array = calloc(total, sizeof(*array));
  if (!array) {
    if (verbose)
      fprintf(stderr, "Failed to allocate synthetic index array of size %lu\n", total);
    goto out;
  }

  i = strspn(attr, "0123456789,");
  if (i == length) {
    /* explicit array of indexes */

    for(i=0; i<total; i++) {
      const char *next;
      unsigned idx = strtoul(attr, (char **) &next, 10);
      if (next == attr) {
	if (verbose)
	  fprintf(stderr, "Failed to read synthetic index #%lu at '%s'\n", (unsigned long) i, attr);
	goto out_with_array;
      }

      array[i] = idx;
      if (i != total-1) {
	if (*next != ',') {
	  if (verbose)
	    fprintf(stderr, "Missing comma after synthetic index #%lu at '%s'\n", (unsigned long) i, attr);
	  goto out_with_array;
	}
	attr = next+1;
      } else {
	attr = next;
      }
    }
    curlevel->index_array = array;

  } else {
    /* interleaving */
    unsigned nr_loops = 1, cur_loop;
    unsigned minstep = total;
    unsigned long nbs = 1;
    unsigned j, mul;
    const char *tmp;

    tmp = attr;
    while (tmp) {
      tmp = strchr(tmp, ':');
      if (!tmp || tmp >= attr+length)
	break;
      nr_loops++;
      tmp++;
    }
    /* nr_loops colon-separated fields, but we may need one more at the end */
    loops = malloc((nr_loops+1)*sizeof(*loops));
    if (!loops) {
      if (verbose)
	fprintf(stderr, "Failed to allocate synthetic index interleave loop array of size %u\n", nr_loops);
      goto out_with_array;
    }

    if (*attr >= '0' && *attr <= '9') {
      /* interleaving as x*y:z*t:... */
      unsigned step, nb;

      tmp = attr;
      cur_loop = 0;
      while (tmp) {
	char *tmp2, *tmp3;
	step = (unsigned) strtol(tmp, &tmp2, 0);
	if (tmp2 == tmp || *tmp2 != '*') {
	  if (verbose)
	    fprintf(stderr, "Failed to read synthetic index interleaving loop '%s' without number before '*'\n", tmp);
	  goto out_with_loops;
	}
	if (!step) {
	  if (verbose)
	    fprintf(stderr, "Invalid interleaving loop with step 0 at '%s'\n", tmp);
	  goto out_with_loops;
	}
	tmp2++;
	nb = (unsigned) strtol(tmp2, &tmp3, 0);
	if (tmp3 == tmp2 || (*tmp3 && *tmp3 != ':' && *tmp3 != ')' && *tmp3 != ' ')) {
	  if (verbose)
	    fprintf(stderr, "Failed to read synthetic index interleaving loop '%s' without number between '*' and ':'\n", tmp);
	  goto out_with_loops;
	}
	if (!nb) {
	  if (verbose)
	    fprintf(stderr, "Invalid interleaving loop with number 0 at '%s'\n", tmp2);
	  goto out_with_loops;
	}
	loops[cur_loop].step = step;
	loops[cur_loop].nb = nb;
	if (step < minstep)
	  minstep = step;
	nbs *= nb;
	cur_loop++;
	if (*tmp3 == ')' || *tmp3 == ' ')
	  break;
	tmp = (const char*) (tmp3+1);
      }

    } else {
      /* interleaving as type1:type2:... */
      hwloc_obj_type_t type;
      union hwloc_obj_attr_u attrs;
      int err;

      /* find level depths for each interleaving loop */
      tmp = attr;
      cur_loop = 0;
      while (tmp) {
	err = hwloc_type_sscanf(tmp, &type, &attrs, sizeof(attrs));
	if (err < 0) {
	  if (verbose)
	    fprintf(stderr, "Failed to read synthetic index interleaving loop type '%s'\n", tmp);
	  goto out_with_loops;
	}
	if (type == HWLOC_OBJ_MISC || type == HWLOC_OBJ_BRIDGE || type == HWLOC_OBJ_PCI_DEVICE || type == HWLOC_OBJ_OS_DEVICE) {
	  if (verbose)
	    fprintf(stderr, "Misc object type disallowed in synthetic index interleaving loop type '%s'\n", tmp);
	  goto out_with_loops;
	}
	for(i=0; i<curleveldepth; i++) {
	  if (type != data->level[i].type)
	    continue;
	  if (type == HWLOC_OBJ_GROUP
	      && attrs.group.depth != (unsigned) -1
	      && attrs.group.depth != data->level[i].depth)
	    continue;
	  loops[cur_loop].level_depth = (unsigned)i;
	  break;
	}
	if (i == curleveldepth) {
	  if (verbose)
	    fprintf(stderr, "Failed to find level for synthetic index interleaving loop type '%s' above '%s'\n",
		    tmp, hwloc_type_name(curlevel->type));
	  goto out_with_loops;
	}
	tmp = strchr(tmp, ':');
	if (!tmp || tmp > attr+length)
	  break;
	tmp++;
	cur_loop++;
      }

      /* compute actual loop step/nb */
      for(cur_loop=0; cur_loop<nr_loops; cur_loop++) {
	unsigned mydepth = loops[cur_loop].level_depth;
	unsigned prevdepth = 0;
	unsigned step, nb;
	for(i=0; i<nr_loops; i++) {
	  if (loops[i].level_depth == mydepth && i != cur_loop) {
	    if (verbose)
	      fprintf(stderr, "Invalid duplicate interleaving loop type in synthetic index '%s'\n", attr);
	    goto out_with_loops;
	  }
	  if (loops[i].level_depth < mydepth
	      && loops[i].level_depth > prevdepth)
	    prevdepth = loops[i].level_depth;
	}
	step = curlevel->totalwidth / data->level[mydepth].totalwidth; /* number of objects below us */
	nb = data->level[mydepth].totalwidth / data->level[prevdepth].totalwidth; /* number of us within parent */

	loops[cur_loop].step = step;
	loops[cur_loop].nb = nb;
	assert(nb);
	assert(step);
	if (step < minstep)
	  minstep = step;
	nbs *= nb;
      }
    }
    assert(nbs);

    if (nbs != total) {
      /* one loop of total/nbs steps is missing, add it if it's just the smallest one */
      if (minstep == total/nbs) {
	loops[nr_loops].step = 1;
	loops[nr_loops].nb = total/nbs;
	nr_loops++;
      } else {
	if (verbose)
	  fprintf(stderr, "Invalid index interleaving total width %lu instead of %lu\n", nbs, total);
	goto out_with_loops;
      }
    }

    /* generate the array of indexes */
    mul = 1;
    for(i=0; i<nr_loops; i++) {
      unsigned step = loops[i].step;
      unsigned nb = loops[i].nb;
      for(j=0; j<total; j++)
	array[j] += ((j / step) % nb) * mul;
      mul *= nb;
    }

    /* check that we have the right values (cannot pass total, cannot give duplicate 0) */
    for(j=0; j<total; j++) {
      if (array[j] >= total) {
	if (verbose)
	  fprintf(stderr, "Invalid index interleaving generates out-of-range index %u\n", array[j]);
	goto out_with_loops;
      }
      if (!array[j] && j) {
	if (verbose)
	  fprintf(stderr, "Invalid index interleaving generates duplicate index values\n");
	goto out_with_loops;
      }
    }

    free(loops);
    curlevel->index_array = array;
  }

  return;

 out_with_loops:
  free(loops);
 out_with_array:
  free(array);
 out:
  return;
}

static hwloc_uint64_t
hwloc_synthetic_parse_memory_attr(const char *attr, const char **endp)
{
  const char *endptr;
  hwloc_uint64_t size;
  size = strtoull(attr, (char **) &endptr, 0);
  if (!hwloc_strncasecmp(endptr, "TB", 2)) {
    size <<= 40;
    endptr += 2;
  } else if (!hwloc_strncasecmp(endptr, "GB", 2)) {
    size <<= 30;
    endptr += 2;
  } else if (!hwloc_strncasecmp(endptr, "MB", 2)) {
    size <<= 20;
    endptr += 2;
  } else if (!hwloc_strncasecmp(endptr, "kB", 2)) {
    size <<= 10;
    endptr += 2;
  }
  *endp = endptr;
  return size;
}

static int
hwloc_synthetic_parse_level_attrs(const char *attrs, const char **next_posp,
				  struct hwloc_synthetic_level_data_s *curlevel,
				  int verbose)
{
  hwloc_obj_type_t type = curlevel->type;
  const char *next_pos;
  hwloc_uint64_t memorysize = 0;
  const char *index_string = NULL;
  size_t index_string_length = 0;

  next_pos = (const char *) strchr(attrs, ')');
  if (!next_pos) {
    if (verbose)
      fprintf(stderr, "Missing attribute closing bracket in synthetic string doesn't have a number of objects at '%s'\n", attrs);
    errno = EINVAL;
    return -1;
  }

  while (')' != *attrs) {
    int iscache = hwloc_obj_type_is_cache(type);

    if (iscache && !strncmp("size=", attrs, 5)) {
      memorysize = hwloc_synthetic_parse_memory_attr(attrs+5, &attrs);

    } else if (!iscache && !strncmp("memory=", attrs, 7)) {
      memorysize = hwloc_synthetic_parse_memory_attr(attrs+7, &attrs);

    } else if (!strncmp("indexes=", attrs, 8)) {
      index_string = attrs+8;
      attrs += 8;
      index_string_length = strcspn(attrs, " )");
      attrs += index_string_length;

    } else {
      if (verbose)
	fprintf(stderr, "Unknown attribute at '%s'\n", attrs);
      errno = EINVAL;
      return -1;
    }

    if (' ' == *attrs)
      attrs++;
    else if (')' != *attrs) {
      if (verbose)
	fprintf(stderr, "Missing parameter separator at '%s'\n", attrs);
      errno = EINVAL;
      return -1;
    }
  }

  curlevel->memorysize = memorysize;
  curlevel->index_string = index_string;
  curlevel->index_string_length = (unsigned long)index_string_length;
  *next_posp = next_pos+1;
  return 0;
}

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
  int type_count[HWLOC_OBJ_TYPE_MAX];
  unsigned unset;
  int verbose = 0;
  const char *env = getenv("HWLOC_SYNTHETIC_VERBOSE");
  int err;
  unsigned long totalarity = 1;

  if (env)
    verbose = atoi(env);

  /* default values before we add root attributes */
  data->level[0].totalwidth = 1;
  data->level[0].type = HWLOC_OBJ_MACHINE;
  data->level[0].index_string = NULL;
  data->level[0].index_array = NULL;
  data->level[0].memorysize = 0;
  if (*description == '(') {
    err = hwloc_synthetic_parse_level_attrs(description+1, &description, &data->level[0], verbose);
    if (err < 0)
      return err;
  }

  for (pos = description, count = 1; *pos; pos = next_pos) {
    hwloc_obj_type_t type = HWLOC_OBJ_TYPE_NONE;
    union hwloc_obj_attr_u attrs;

    /* initialize parent arity to 0 so that the levels are not infinite */
    data->level[count-1].arity = 0;

    while (*pos == ' ')
      pos++;

    if (!*pos)
      break;

    if (*pos < '0' || *pos > '9') {
      if (hwloc_type_sscanf(pos, &type, &attrs, sizeof(attrs)) < 0) {
	/* FIXME: allow generic "Cache" string? would require to deal with possibly duplicate cache levels */
	if (verbose)
	  fprintf(stderr, "Synthetic string with unknown object type at '%s'\n", pos);
	errno = EINVAL;
	goto error;
      }
      if (type == HWLOC_OBJ_SYSTEM || type == HWLOC_OBJ_MISC || type == HWLOC_OBJ_BRIDGE || type == HWLOC_OBJ_PCI_DEVICE || type == HWLOC_OBJ_OS_DEVICE) {
	if (verbose)
	  fprintf(stderr, "Synthetic string with disallowed object type at '%s'\n", pos);
	errno = EINVAL;
	goto error;
      }

      next_pos = strchr(pos, ':');
      if (!next_pos) {
	if (verbose)
	  fprintf(stderr,"Synthetic string doesn't have a `:' after object type at '%s'\n", pos);
	errno = EINVAL;
	goto error;
      }
      pos = next_pos + 1;
    }
    data->level[count].type = type;
    data->level[count].depth = (unsigned) -1;
    data->level[count].cachetype = (hwloc_obj_cache_type_t) -1;
    if (hwloc_obj_type_is_cache(type)) {
      /* these are always initialized */
      data->level[count].depth = attrs.cache.depth;
      data->level[count].cachetype = attrs.cache.type;
    } else if (type == HWLOC_OBJ_GROUP) {
      /* could be -1 but will be set below */
      data->level[count].depth = attrs.group.depth;
    }

    item = strtoul(pos, (char **)&next_pos, 0);
    if (next_pos == pos) {
      if (verbose)
	fprintf(stderr,"Synthetic string doesn't have a number of objects at '%s'\n", pos);
      errno = EINVAL;
      goto error;
    }
    if (!item) {
      if (verbose)
	fprintf(stderr,"Synthetic string with disallow 0 number of objects at '%s'\n", pos);
      errno = EINVAL;
      goto error;
    }
    data->level[count-1].arity = (unsigned)item;

    totalarity *= item;
    data->level[count].totalwidth = totalarity;
    data->level[count].index_string = NULL;
    data->level[count].index_array = NULL;
    data->level[count].memorysize = 0;
    if (*next_pos == '(') {
      err = hwloc_synthetic_parse_level_attrs(next_pos+1, &next_pos, &data->level[count], verbose);
      if (err < 0)
	goto error;
    }

    if (count + 1 >= HWLOC_SYNTHETIC_MAX_DEPTH) {
      if (verbose)
	fprintf(stderr,"Too many synthetic levels, max %d\n", HWLOC_SYNTHETIC_MAX_DEPTH);
      errno = EINVAL;
      goto error;
    }
    if (item > UINT_MAX) {
      if (verbose)
	fprintf(stderr,"Too big arity, max %u\n", UINT_MAX);
      errno = EINVAL;
      goto error;
    }

    count++;
  }

  if (count <= 0) {
    if (verbose)
      fprintf(stderr, "Synthetic string doesn't contain any object\n");
    errno = EINVAL;
    goto error;
  }

  if (data->level[count-1].type != HWLOC_OBJ_TYPE_NONE && data->level[count-1].type != HWLOC_OBJ_PU) {
    if (verbose)
      fprintf(stderr, "Synthetic string cannot use non-PU type for last level\n");
    errno = EINVAL;
    return -1;
  }
  data->level[count-1].type = HWLOC_OBJ_PU;

  for(i=0; i<HWLOC_OBJ_TYPE_MAX; i++) {
    type_count[i] = 0;
  }
  for(i=count-1; i>0; i--) {
    hwloc_obj_type_t type = data->level[i].type;
    if (type != HWLOC_OBJ_TYPE_NONE) {
      type_count[type]++;
    }
  }

  /* sanity checks */
  if (!type_count[HWLOC_OBJ_PU]) {
    if (verbose)
      fprintf(stderr, "Synthetic string missing ending number of PUs\n");
    errno = EINVAL;
    return -1;
  } else if (type_count[HWLOC_OBJ_PU] > 1) {
    if (verbose)
      fprintf(stderr, "Synthetic string cannot have several PU levels\n");
    errno = EINVAL;
    return -1;
  }
  if (type_count[HWLOC_OBJ_PACKAGE] > 1) {
    if (verbose)
      fprintf(stderr, "Synthetic string cannot have several package levels\n");
    errno = EINVAL;
    return -1;
  }
  if (type_count[HWLOC_OBJ_NUMANODE] > 1) {
    if (verbose)
      fprintf(stderr, "Synthetic string cannot have several NUMA node levels\n");
    errno = EINVAL;
    return -1;
  }
  if (type_count[HWLOC_OBJ_CORE] > 1) {
    if (verbose)
      fprintf(stderr, "Synthetic string cannot have several core levels\n");
    errno = EINVAL;
    return -1;
  }
  if (type_count[HWLOC_OBJ_MACHINE] > 1) {
    if (verbose)
      fprintf(stderr, "Synthetic string cannot have several machine levels\n");
    errno = EINVAL;
    return -1;
  }

  /* initialize the top level (not specified in the string) */
  if (type_count[HWLOC_OBJ_MACHINE] == 1) {
    data->level[0].type = HWLOC_OBJ_SYSTEM;
    type_count[HWLOC_OBJ_SYSTEM] = 1;
  } else {
    data->level[0].type = HWLOC_OBJ_MACHINE;
    type_count[HWLOC_OBJ_MACHINE] = 1;
  }

  /* deal with missing intermediate levels */
  unset = 0;
  for(i=1; i<count-1; i++) {
    if (data->level[i].type == HWLOC_OBJ_TYPE_NONE)
      unset++;
  }
  if (unset && unset != count-2) {
    if (verbose)
      fprintf(stderr, "Synthetic string cannot mix unspecified and specified types for levels\n");
    errno = EINVAL;
    return -1;
  }
  if (unset) {
    /* we want in priority: numa, package, core, up to 3 caches, groups */
    unsigned neednuma = count >= 3;
    unsigned needpack = count >= 4;
    unsigned needcore = count >= 5;
    unsigned needcaches = count <= 5 ? 0 : count >= 9 ? 4 : count-5;
    unsigned needgroups = count-2-neednuma-needpack-needcore-needcaches;
    /* we place them in order: groups, package, numa, caches, core */
    for(i = 0; i < needgroups; i++) {
      unsigned depth = 1 + i;
      data->level[depth].type = HWLOC_OBJ_GROUP;
      type_count[HWLOC_OBJ_GROUP]++;
    }
    if (needpack) {
      unsigned depth = 1 + needgroups;
      data->level[depth].type = HWLOC_OBJ_PACKAGE;
      type_count[HWLOC_OBJ_PACKAGE] = 1;
    }
    if (neednuma) {
      unsigned depth = 1 + needgroups + needpack;
      data->level[depth].type = HWLOC_OBJ_NUMANODE;
      type_count[HWLOC_OBJ_NUMANODE] = 1;
    }
    if (needcaches) {
      /* priority: l2, l1, l3, l1i */
      /* order: l3, l2, l1, l1i */
      unsigned l3depth = 1 + needgroups + needpack + neednuma;
      unsigned l2depth = l3depth + (needcaches >= 3);
      unsigned l1depth = l2depth + 1;
      unsigned l1idepth = l1depth + 1;
      if (needcaches >= 3) {
	data->level[l3depth].type = HWLOC_OBJ_L3CACHE;
	data->level[l3depth].depth = 3;
	data->level[l3depth].cachetype = HWLOC_OBJ_CACHE_UNIFIED;
	type_count[HWLOC_OBJ_L3CACHE] = 1;
      }
      data->level[l2depth].type = HWLOC_OBJ_L2CACHE;
      data->level[l2depth].depth = 2;
      data->level[l2depth].cachetype = HWLOC_OBJ_CACHE_UNIFIED;
      type_count[HWLOC_OBJ_L2CACHE] = 1;
      if (needcaches >= 2) {
	data->level[l1depth].type = HWLOC_OBJ_L1CACHE;
	data->level[l1depth].depth = 1;
	data->level[l1depth].cachetype = HWLOC_OBJ_CACHE_DATA;
	type_count[HWLOC_OBJ_L1CACHE] = 1;
      }
      if (needcaches >= 4) {
	data->level[l1idepth].type = HWLOC_OBJ_L1ICACHE;
	data->level[l1idepth].depth = 1;
	data->level[l1idepth].cachetype = HWLOC_OBJ_CACHE_INSTRUCTION;
	type_count[HWLOC_OBJ_L1ICACHE] = 1;
      }
    }
    if (needcore) {
      unsigned depth = 1 + needgroups + needpack + neednuma + needcaches;
      data->level[depth].type = HWLOC_OBJ_CORE;
      type_count[HWLOC_OBJ_CORE] = 1;
    }
  }

  /* enforce a NUMA level */
  if (!type_count[HWLOC_OBJ_NUMANODE]) {
    /* insert a NUMA level and the machine level */
    if (data->level[1].type == HWLOC_OBJ_MACHINE)
      /* there's an explicit machine level after the automatic system root, insert below both */
      i = 2;
    else
      /* insert below the automatic machine root */
      i = 1;
    if (verbose)
      fprintf(stderr, "Inserting a NUMA level with a single object at depth %u\n", i);
    /* move existing levels by one */
    memmove(&data->level[i+1], &data->level[i], (count*i)*sizeof(struct hwloc_synthetic_level_data_s));
    data->level[i].type = HWLOC_OBJ_NUMANODE;
    data->level[i].index_string = NULL;
    data->level[i].index_array = NULL;
    data->level[i].memorysize = 0;
    data->level[i].totalwidth = data->level[i-1].totalwidth;
    /* update arity to insert a single NUMA node per parent */
    data->level[i].arity = data->level[i-1].arity;
    data->level[i-1].arity = 1;
    count++;
  }

  for (i=0; i<count; i++) {
    struct hwloc_synthetic_level_data_s *curlevel = &data->level[i];
    hwloc_obj_type_t type = curlevel->type;

    if (type == HWLOC_OBJ_GROUP) {
      if (curlevel->depth == (unsigned)-1)
	curlevel->depth = type_count[HWLOC_OBJ_GROUP]--;

    } else if (hwloc_obj_type_is_cache(type)) {
      if (!curlevel->memorysize) {
	if (1 == curlevel->depth)
	  /* 32Kb in L1 */
	  curlevel->memorysize = 32*1024;
	else
	  /* *4 at each level, starting from 1MB for L2, unified */
	  curlevel->memorysize = 256*1024 << (2*curlevel->depth);
      }

    } else if (type == HWLOC_OBJ_NUMANODE && !curlevel->memorysize) {
      /* 1GB in memory nodes. */
      curlevel->memorysize = 1024*1024*1024;
    }

    hwloc_synthetic_process_level_indexes(data, i, verbose);
  }

  data->string = strdup(description);
  data->level[count-1].arity = 0;
  return 0;

 error:
  for(i=0; i<HWLOC_SYNTHETIC_MAX_DEPTH; i++) {
    struct hwloc_synthetic_level_data_s *curlevel = &data->level[i];
    free(curlevel->index_array);
    if (!curlevel->arity)
      break;
  }
  return -1;
}

static void
hwloc_synthetic__post_look_hooks(struct hwloc_synthetic_level_data_s *curlevel,
				 hwloc_obj_t obj)
{
  switch (obj->type) {
  case HWLOC_OBJ_GROUP:
    obj->attr->group.kind = HWLOC_GROUP_KIND_SYNTHETIC;
    obj->attr->group.subkind = curlevel->depth-1;
    break;
  case HWLOC_OBJ_SYSTEM:
    break;
  case HWLOC_OBJ_MACHINE:
    break;
  case HWLOC_OBJ_NUMANODE:
    break;
  case HWLOC_OBJ_PACKAGE:
    break;
  case HWLOC_OBJ_L1CACHE:
  case HWLOC_OBJ_L2CACHE:
  case HWLOC_OBJ_L3CACHE:
  case HWLOC_OBJ_L4CACHE:
  case HWLOC_OBJ_L5CACHE:
  case HWLOC_OBJ_L1ICACHE:
  case HWLOC_OBJ_L2ICACHE:
  case HWLOC_OBJ_L3ICACHE:
    obj->attr->cache.depth = curlevel->depth;
    obj->attr->cache.linesize = 64;
    obj->attr->cache.type = curlevel->cachetype;
    obj->attr->cache.size = curlevel->memorysize;
    break;
  case HWLOC_OBJ_CORE:
    break;
  case HWLOC_OBJ_PU:
    break;
  case HWLOC_OBJ_BRIDGE:
  case HWLOC_OBJ_PCI_DEVICE:
  case HWLOC_OBJ_OS_DEVICE:
  case HWLOC_OBJ_MISC:
  case HWLOC_OBJ_TYPE_MAX:
    /* Should never happen */
    assert(0);
    break;
  }
  if (curlevel->memorysize && !hwloc_obj_type_is_cache(obj->type)) {
    obj->memory.local_memory = curlevel->memorysize;
    obj->memory.page_types_len = 1;
    obj->memory.page_types = malloc(sizeof(*obj->memory.page_types));
    memset(obj->memory.page_types, 0, sizeof(*obj->memory.page_types));
    obj->memory.page_types[0].size = 4096;
    obj->memory.page_types[0].count = curlevel->memorysize / 4096;
  }
}

/*
 * Recursively build objects whose cpu start at first_cpu
 * - level gives where to look in the type, arity and id arrays
 * - the id array is used as a variable to get unique IDs for a given level.
 * - generated memory should be added to *memory_kB.
 * - generated cpus should be added to parent_cpuset.
 * - next cpu number to be used should be returned.
 */
static void
hwloc__look_synthetic(struct hwloc_topology *topology,
		      struct hwloc_synthetic_backend_data_s *data,
		      int level,
		      hwloc_bitmap_t parent_cpuset)
{
  hwloc_obj_t obj;
  unsigned i;
  struct hwloc_synthetic_level_data_s *curlevel = &data->level[level];
  hwloc_obj_type_t type = curlevel->type;
  hwloc_bitmap_t set;
  unsigned os_index;

  /* pre-hooks */
  switch (type) {
    case HWLOC_OBJ_GROUP:
      break;
    case HWLOC_OBJ_MACHINE:
      break;
    case HWLOC_OBJ_NUMANODE:
      break;
    case HWLOC_OBJ_PACKAGE:
      break;
    case HWLOC_OBJ_L1CACHE:
    case HWLOC_OBJ_L2CACHE:
    case HWLOC_OBJ_L3CACHE:
    case HWLOC_OBJ_L4CACHE:
    case HWLOC_OBJ_L5CACHE:
    case HWLOC_OBJ_L1ICACHE:
    case HWLOC_OBJ_L2ICACHE:
    case HWLOC_OBJ_L3ICACHE:
      break;
    case HWLOC_OBJ_CORE:
      break;
    case HWLOC_OBJ_PU:
      break;
    case HWLOC_OBJ_SYSTEM:
    case HWLOC_OBJ_BRIDGE:
    case HWLOC_OBJ_PCI_DEVICE:
    case HWLOC_OBJ_OS_DEVICE:
    case HWLOC_OBJ_MISC:
    case HWLOC_OBJ_TYPE_MAX:
      /* Should never happen */
      assert(0);
      break;
  }

  os_index = curlevel->next_os_index++;
  if (curlevel->index_array)
    os_index = curlevel->index_array[os_index];
  else if (hwloc_obj_type_is_cache(type) || type == HWLOC_OBJ_GROUP)
    /* don't enforce useless os_indexes for Caches and Groups */
    os_index = -1;

  set = hwloc_bitmap_alloc();
  if (!curlevel->arity) {
    hwloc_bitmap_set(set, os_index);
  } else {
    for (i = 0; i < curlevel->arity; i++)
      hwloc__look_synthetic(topology, data, level + 1, set);
  }

  hwloc_bitmap_or(parent_cpuset, parent_cpuset, set);

  if (hwloc_filter_check_keep_object_type(topology, type)) {
    obj = hwloc_alloc_setup_object(topology, type, os_index);
    obj->cpuset = set;

    if (type == HWLOC_OBJ_NUMANODE) {
      obj->nodeset = hwloc_bitmap_alloc();
      hwloc_bitmap_set(obj->nodeset, os_index);
    }

    hwloc_synthetic__post_look_hooks(curlevel, obj);

    hwloc_insert_object_by_cpuset(topology, obj);
  } else
    hwloc_bitmap_free(set);
}

static int
hwloc_look_synthetic(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_synthetic_backend_data_s *data = backend->private_data;
  hwloc_bitmap_t cpuset = hwloc_bitmap_alloc();
  unsigned i;

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
  hwloc_synthetic__post_look_hooks(&data->level[0], topology->levels[0][0]);

  for (i = 0; i < data->level[0].arity; i++)
    hwloc__look_synthetic(topology, data, 1, cpuset);

  hwloc_bitmap_free(cpuset);

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "Synthetic");
  hwloc_obj_add_info(topology->levels[0][0], "SyntheticDescription", data->string);
  return 0;
}

static void
hwloc_synthetic_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_synthetic_backend_data_s *data = backend->private_data;
  unsigned i;
  for(i=0; i<HWLOC_SYNTHETIC_MAX_DEPTH; i++) {
    struct hwloc_synthetic_level_data_s *curlevel = &data->level[i];
    free(curlevel->index_array);
    if (!curlevel->arity)
      break;
  }
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
    const char *env = getenv("HWLOC_SYNTHETIC");
    if (env) {
      /* 'synthetic' was given in HWLOC_COMPONENTS without a description */
      _data1 = env;
    } else {
      errno = EINVAL;
      goto out;
    }
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
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_synthetic_disc_component
};

static int hwloc_topology_export_synthetic_indexes(struct hwloc_topology * topology,
						   hwloc_obj_t obj,
						   char *buffer, size_t buflen)
{
  unsigned depth = obj->depth;
  unsigned total = topology->level_nbobjects[depth];
  unsigned step = 1;
  unsigned nr_loops = 0;
  struct hwloc_synthetic_intlv_loop_s *loops = NULL, *tmploops;
  hwloc_obj_t cur;
  unsigned i, j;
  ssize_t tmplen = buflen;
  char *tmp = buffer;
  int res, ret = 0;

  /* must start with 0 */
  if (obj->os_index)
    goto exportall;

  while (step != total) {
    /* must be a divider of the total */
    if (total % step)
      goto exportall;

    /* look for os_index == step */
    for(i=1; i<total; i++)
      if (topology->levels[depth][i]->os_index == step)
	break;
    if (i == total)
      goto exportall;
    for(j=2; j<total/i; j++)
      if (topology->levels[depth][i*j]->os_index != step*j)
	break;

    nr_loops++;
    tmploops = realloc(loops, nr_loops*sizeof(*loops));
    if (!tmploops)
      goto exportall;
    loops = tmploops;
    loops[nr_loops-1].step = i;
    loops[nr_loops-1].nb = j;
    step *= j;
  }

  /* check this interleaving */
  for(i=0; i<total; i++) {
    unsigned ind = 0;
    unsigned mul = 1;
    for(j=0; j<nr_loops; j++) {
      ind += (i / loops[j].step) % loops[j].nb * mul;
      mul *= loops[j].nb;
    }
    if (topology->levels[depth][i]->os_index != ind)
      goto exportall;
  }

  /* success, print it */
  for(j=0; j<nr_loops; j++) {
    res = hwloc_snprintf(tmp, tmplen, "%u*%u%s", loops[j].step, loops[j].nb,
			 j == nr_loops-1 ? ")" : ":");
    if (res < 0) {
      free(loops);
      return -1;
    }
    ret += res;
    if (res >= tmplen)
      res = tmplen>0 ? (int)tmplen - 1 : 0;
    tmp += res;
    tmplen -= res;
  }

  free(loops);
  return ret;

 exportall:
  free(loops);

  /* dump all indexes */
  cur = obj;
  while (cur) {
    res = snprintf(tmp, tmplen, "%u%s", cur->os_index,
		   cur->next_cousin ? "," : ")");
    if (res < 0)
      return -1;
    ret += res;
    if (res >= tmplen)
      res = tmplen>0 ? (int)tmplen - 1 : 0;
    tmp += res;
    tmplen -= res;
    cur = cur->next_cousin;
  }
  return ret;
}

static int hwloc_topology_export_synthetic_obj_attr(struct hwloc_topology * topology,
						    hwloc_obj_t obj,
						    char *buffer, size_t buflen)
{
  const char * separator = " ";
  const char * prefix = "(";
  char cachesize[64] = "";
  char memsize[64] = "";
  int needindexes = 0;

  if (hwloc_obj_type_is_cache(obj->type) && obj->attr->cache.size) {
    snprintf(cachesize, sizeof(cachesize), "%ssize=%llu",
	     prefix, (unsigned long long) obj->attr->cache.size);
    prefix = separator;
  }
  if (obj->memory.local_memory) {
    snprintf(memsize, sizeof(memsize), "%smemory=%llu",
	     prefix, (unsigned long long) obj->memory.local_memory);
    prefix = separator;
  }
  if (obj->type == HWLOC_OBJ_PU || obj->type == HWLOC_OBJ_NUMANODE) {
    hwloc_obj_t cur = obj;
    while (cur) {
      if (cur->os_index != cur->logical_index) {
	needindexes = 1;
	break;
      }
      cur = cur->next_cousin;
    }
  }
  if (*cachesize || *memsize || needindexes) {
    ssize_t tmplen = buflen;
    char *tmp = buffer;
    int res, ret = 0;

    res = hwloc_snprintf(tmp, tmplen, "%s%s%s", cachesize, memsize, needindexes ? "" : ")");
    if (res < 0)
      return -1;
    ret += res;
    if (res >= tmplen)
      res = tmplen>0 ? (int)tmplen - 1 : 0;
    tmp += res;
    tmplen -= res;

    if (needindexes) {
      res = snprintf(tmp, tmplen, "%sindexes=", prefix);
      if (res < 0)
	return -1;
      ret += res;
      if (res >= tmplen)
	res = tmplen>0 ? (int)tmplen - 1 : 0;
      tmp += res;
      tmplen -= res;

      res = hwloc_topology_export_synthetic_indexes(topology, obj, tmp, tmplen);
      if (res < 0)
	return -1;
      ret += res;
      if (res >= tmplen)
	res = tmplen>0 ? (int)tmplen - 1 : 0;
      tmp += res;
      tmplen -= res;
    }
    return ret;
  } else {
    return 0;
  }
}

int
hwloc_topology_export_synthetic(struct hwloc_topology * topology,
				char *buffer, size_t buflen,
				unsigned long flags)
{
  hwloc_obj_t obj = hwloc_get_root_obj(topology);
  ssize_t tmplen = buflen;
  char *tmp = buffer;
  int res, ret = 0;
  unsigned arity;
  const char * separator = " ";
  const char * prefix = "";

  if (!topology->is_loaded) {
    errno = EINVAL;
    return -1;
  }

  if (flags & ~(HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_EXTENDED_TYPES|HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_ATTRS)) {
    errno = EINVAL;
    return -1;
  }

  /* TODO: add a flag to ignore symmetric_subtree and I/Os.
   * just assume things are symmetric with the left branches of the tree.
   * but the number of objects per level may be wrong, what to do with OS index array in this case?
   * only allow ignoring symmetric_subtree if the level width remains OK?
   */

  /* TODO: add a root object by default, with a prefix such as tree=
   * so that we can backward-compatibly recognize whether there's a root or not.
   * and add a flag to disable it.
   */

  /* TODO: flag to force all indexes, not only for PU and NUMA? */

  if (!obj->symmetric_subtree) {
    errno = EINVAL;
    return -1;
  }

  if (!(flags & HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_ATTRS)) {
    /* root attributes */
    res = hwloc_topology_export_synthetic_obj_attr(topology, obj, tmp, tmplen);
    if (res < 0)
      return -1;
    ret += res;
    if (ret > 0)
      prefix = separator;
    if (res >= tmplen)
      res = tmplen>0 ? (int)tmplen - 1 : 0;
    tmp += res;
    tmplen -= res;
  }

  arity = obj->arity;
  while (arity) {
    /* for each level */
    obj = obj->first_child;
    if (flags & HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_EXTENDED_TYPES) {
      res = hwloc_snprintf(tmp, tmplen, "%s%s:%u", prefix, hwloc_type_name(obj->type), arity);
    } else {
      char types[64];
      hwloc_obj_type_snprintf(types, sizeof(types), obj, 1);
      res = hwloc_snprintf(tmp, tmplen, "%s%s:%u", prefix, types, arity);
    }
    if (res < 0)
      return -1;
    ret += res;
    if (res >= tmplen)
      res = tmplen>0 ? (int)tmplen - 1 : 0;
    tmp += res;
    tmplen -= res;

    if (!(flags & HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_ATTRS)) {
      /* obj attributes */
      res = hwloc_topology_export_synthetic_obj_attr(topology, obj, tmp, tmplen);
      if (res < 0)
	return -1;
      ret += res;
      if (res >= tmplen)
	res = tmplen>0 ? (int)tmplen - 1 : 0;
      tmp += res;
      tmplen -= res;
    }

    /* next level */
    prefix = separator;
    arity = obj->arity;
  }

  return ret;
}
