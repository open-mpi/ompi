/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_CALC_H
#define HWLOC_CALC_H

#include <hwloc.h>
#include <private/misc.h> /* for HWLOC_OBJ_TYPE_NONE */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <ctype.h>
#include <assert.h>

struct hwloc_calc_location_context_s {
  hwloc_topology_t topology;
  int topodepth;
  int only_hbm; /* -1 for everything, 0 for only non-HBM, 1 for only HBM numa nodes */
  int logical;
  int verbose;
};

typedef enum hwloc_calc_append_mode_e {
  HWLOC_CALC_APPEND_ADD,
  HWLOC_CALC_APPEND_CLR,
  HWLOC_CALC_APPEND_AND,
  HWLOC_CALC_APPEND_XOR
} hwloc_calc_append_mode_t;

static __hwloc_inline int
hwloc_calc_append_set(hwloc_bitmap_t set, hwloc_const_bitmap_t newset,
		      hwloc_calc_append_mode_t mode, int verbose)
{
  char *s1, *s2;
  hwloc_bitmap_asprintf(&s1, newset);
  hwloc_bitmap_asprintf(&s2, set);
  switch (mode) {
  case HWLOC_CALC_APPEND_ADD:
    if (verbose > 0)
      fprintf(stderr, "adding %s to %s\n",
          s1, s2);
    hwloc_bitmap_or(set, set, newset);
    break;
  case HWLOC_CALC_APPEND_CLR:
    if (verbose > 0)
      fprintf(stderr, "clearing %s from %s\n",
          s1, s2);
    hwloc_bitmap_andnot(set, set, newset);
    break;
  case HWLOC_CALC_APPEND_AND:
    if (verbose > 0)
      fprintf(stderr, "and'ing %s from %s\n",
          s1, s2);
    hwloc_bitmap_and(set, set, newset);
    break;
  case HWLOC_CALC_APPEND_XOR:
    if (verbose > 0)
      fprintf(stderr, "xor'ing %s from %s\n",
          s1, s2);
    hwloc_bitmap_xor(set, set, newset);
    break;
  default:
    assert(0);
  }
  free(s1);
  free(s2);
  return 0;
}

static __hwloc_inline unsigned
hwloc_calc_get_nbobjs_inside_sets_by_depth(struct hwloc_calc_location_context_s *lcontext,
					   hwloc_const_bitmap_t cpuset, hwloc_const_bitmap_t nodeset,
					   int depth)
{
  hwloc_topology_t topology = lcontext->topology;
  int only_hbm = lcontext->only_hbm;
  hwloc_obj_t obj = NULL;
  unsigned n = 0;
  while ((obj = hwloc_get_next_obj_by_depth(topology, depth, obj)) != NULL) {
    if (!hwloc_bitmap_isincluded(obj->cpuset, cpuset))
      continue;
    if (!hwloc_bitmap_isincluded(obj->nodeset, nodeset))
      continue;
    if (hwloc_bitmap_iszero(obj->cpuset) && hwloc_bitmap_iszero(obj->nodeset))
      /* ignore objects with empty sets (both can be empty when outside of cgroup) */
      continue;
    if (only_hbm >= 0 && obj->type == HWLOC_OBJ_NUMANODE) {
      /* filter on hbm */
      int obj_is_hbm = obj->subtype && !strcmp(obj->subtype, "MCDRAM");
      if (only_hbm != obj_is_hbm)
	continue;
    }
    n++;
  }
  return n;
}

static __hwloc_inline hwloc_obj_t
hwloc_calc_get_obj_inside_sets_by_depth(struct hwloc_calc_location_context_s *lcontext,
					hwloc_const_bitmap_t cpuset, hwloc_const_bitmap_t nodeset,
					int depth, unsigned ind)
{
  hwloc_topology_t topology = lcontext->topology;
  int only_hbm = lcontext->only_hbm;
  int logical = lcontext->logical;
  hwloc_obj_t obj = NULL;
  unsigned i = 0;
  while ((obj = hwloc_get_next_obj_by_depth(topology, depth, obj)) != NULL) {
    if (!hwloc_bitmap_isincluded(obj->cpuset, cpuset))
      continue;
    if (!hwloc_bitmap_isincluded(obj->nodeset, nodeset))
      continue;
    if (hwloc_bitmap_iszero(obj->cpuset) && hwloc_bitmap_iszero(obj->nodeset))
      /* ignore objects with empty sets (both can be empty when outside of cgroup) */
      continue;
    if (only_hbm >= 0 && obj->type == HWLOC_OBJ_NUMANODE) {
      /* filter on hbm */
      int obj_is_hbm = obj->subtype && !strcmp(obj->subtype, "MCDRAM");
      if (only_hbm != obj_is_hbm)
	continue;
    }
    if (logical) {
      if (i == ind)
	return obj;
      i++;
    } else {
      if (obj->os_index == ind)
	return obj;
    }
  }
  return NULL;
}

static __hwloc_inline int
hwloc_calc_parse_depth_prefix(struct hwloc_calc_location_context_s *lcontext,
			      const char *string, size_t typelen,
			      hwloc_obj_type_t *typep)
{
  hwloc_topology_t topology = lcontext->topology;
  int topodepth = lcontext->topodepth;
  int verbose = lcontext->verbose;
  char typestring[20+1]; /* large enough to store all type names, even with a depth attribute */
  hwloc_obj_type_t type;
  int depth;
  char *end;
  int err;

  if (typelen >= sizeof(typestring)) {
    if (verbose >= 0)
      fprintf(stderr, "invalid type name %s\n", string);
    return -1;
  }
  strncpy(typestring, string, typelen);
  typestring[typelen] = '\0';

  /* try to match a type name */
  err = hwloc_type_sscanf_as_depth(typestring, &type, topology, &depth);
  if (!err) {
    *typep = type;
    return depth;
  }
  if (!strcasecmp(typestring, "HBM") || !strcasecmp(typestring, "MCDRAM")) {
    if (lcontext->only_hbm == -1)
      lcontext->only_hbm = 1;
    *typep = HWLOC_OBJ_NUMANODE;
    depth = HWLOC_TYPE_DEPTH_NUMANODE;
    return depth;
  }

  /* try to match a numeric depth */
  depth = strtol(string, &end, 0);
  if (end != &string[typelen]) {
    if (verbose >= 0)
      fprintf(stderr, "invalid type name %s\n", string);
    return -1;
  }
  if (depth >= topodepth) {
    if (verbose >= 0)
      fprintf(stderr, "ignoring invalid depth %d\n", depth);
    return -1;
  }
  *typep = HWLOC_OBJ_TYPE_NONE;
  return depth;
}

static __hwloc_inline int
hwloc_calc_parse_range(const char *_string,
		       int *firstp, int *amountp, int *stepp, int *wrapp,
		       const char **dotp)
{
  char string[65];
  size_t len;
  char *dot, *end, *end2;
  long first, last, amount;
  int wrap;

  dot = strchr(_string, '.');
  *dotp = dot;
  if (dot) {
    len = dot - _string;
  } else {
    len = strlen(_string);
  }
  if (len >= sizeof(string)) {
    fprintf(stderr, "invalid range `%s', too long\n", _string);
    return -1;
  }
  memcpy(string, _string, len);
  string[len] = '\0';

  if (!isdigit(*string)) {
    if (!strncmp(string, "all", 3)) {
      *firstp = 0;
      *amountp = -1;
      *stepp = 1;
      *wrapp = 0;
      return 0;
    } else if (!strncmp(string, "odd", 3)) {
      *firstp = 1;
      *amountp = -1;
      *stepp = 2;
      *wrapp = 0;
      return 0;
    } else if (!strncmp(string, "even", 4)) {
      *firstp = 0;
      *amountp = -1;
      *stepp = 2;
      *wrapp = 0;
      return 0;
    } else {
      fprintf(stderr, "unrecognized range keyword `%s'\n", string);
      return -1;
    }
  }

  first = strtol(string, &end, 10);
  amount = 1;
  wrap = 0;

  if (*end == '-') {
    last = strtol(end+1, &end2, 10);
    if (*end2) {
      fprintf(stderr, "invalid character at `%s' after range at `%s'\n", end2, string);
      return -1;
    } else if (end2 == end+1) {
      /* X- */
      amount = -1;
    } else {
      /* X-Y */
      amount = last-first+1;
    }

  } else if (*end == ':') {
    /* X:Y */
    wrap = 1;
    amount = strtol(end+1, &end2, 10);
    if (*end2) {
      fprintf(stderr, "invalid character at `%s' after range at `%s'\n", end2, string);
      return -1;
    } else if (end2 == end+1) {
      fprintf(stderr, "missing width at `%s' in range at `%s'\n", end2, string);
      return -1;
    }

  } else if (*end) {
    fprintf(stderr, "invalid character at `%s' after index at `%s'\n", end, string);
    return -1;
  }

  *firstp = first;
  *amountp = amount;
  *stepp = 1;
  *wrapp = wrap;
  return 0;
}

static __hwloc_inline int
hwloc_calc_append_object_range(struct hwloc_calc_location_context_s *lcontext,
			       hwloc_const_bitmap_t rootcpuset, hwloc_const_bitmap_t rootnodeset, int depth,
			       const char *string, /* starts with indexes following the colon */
			       void (*cbfunc)(struct hwloc_calc_location_context_s *, void *, hwloc_obj_t), void *cbdata)
{
  int verbose = lcontext->verbose;
  hwloc_obj_t obj;
  unsigned width;
  const char *dot, *nextsep = NULL;
  int nextdepth = -1;
  int first, wrap, amount, step;
  unsigned i,j;
  int err;

  err = hwloc_calc_parse_range(string,
			       &first, &amount, &step, &wrap,
			       &dot);
  if (err < 0)
    return -1;
  assert(amount != -1 || !wrap);

  if (dot) {
    /* parse the next string before calling ourself recursively */
    size_t typelen;
    hwloc_obj_type_t type;
    const char *nextstring = dot+1;
    typelen = strspn(nextstring, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
    if (!typelen || nextstring[typelen] != ':')
      return -1;
    nextsep = &nextstring[typelen];

    nextdepth = hwloc_calc_parse_depth_prefix(lcontext,
					      nextstring, typelen,
					      &type);
    if (nextdepth == HWLOC_TYPE_DEPTH_UNKNOWN || nextdepth == HWLOC_TYPE_DEPTH_MULTIPLE)
      return -1;
    /* we need an object with a cpuset, that's depth>=0 or memory */
    if (nextdepth < 0 && nextdepth != HWLOC_TYPE_DEPTH_NUMANODE) {
      if (verbose >= 0)
	fprintf(stderr, "hierarchical location %s only supported with normal object types\n", string);
      return -1;
    }
  }

  width = hwloc_calc_get_nbobjs_inside_sets_by_depth(lcontext, rootcpuset, rootnodeset, depth);
  if (amount == -1)
    amount = (width-first+step-1)/step;

  for(i=first, j=0; j<(unsigned)amount; i+=step, j++) {
    if (wrap && i>=width)
      i = 0;

    obj = hwloc_calc_get_obj_inside_sets_by_depth(lcontext, rootcpuset, rootnodeset, depth, i);
    if (verbose > 0 || (!obj && verbose >= 0)) {
      char *sc, *sn;
      hwloc_bitmap_asprintf(&sc, rootcpuset);
      hwloc_bitmap_asprintf(&sn, rootnodeset);
      if (obj)
	printf("using object #%u depth %d below cpuset %s nodeset %s\n",
	       i, depth, sc, sn);
      else
	fprintf(stderr, "object #%u depth %d below cpuset %s nodeset %s does not exist\n",
		i, depth, sc, sn);
      free(sc);
      free(sn);
    }
    if (obj) {
      if (dot) {
	hwloc_calc_append_object_range(lcontext, obj->cpuset, obj->nodeset, nextdepth, nextsep+1, cbfunc, cbdata);
      } else {
	/* add to the temporary cpuset
	 * and let the caller add/clear/and/xor for the actual final cpuset depending on cmdline options
	 */
        cbfunc(lcontext, cbdata, obj);
      }
    }
  }

  return 0;
}

static __hwloc_inline int
hwloc_calc_append_iodev(struct hwloc_calc_location_context_s *lcontext,
			void (*cbfunc)(struct hwloc_calc_location_context_s *, void *, hwloc_obj_t), void *cbdata,
			hwloc_obj_t obj)
{
  cbfunc(lcontext, cbdata, obj);
  return 0;
}

static __hwloc_inline int
hwloc_calc_append_iodev_by_index(struct hwloc_calc_location_context_s *lcontext,
				 hwloc_obj_type_t type, int depth, const char *string,
				 void (*cbfunc)(struct hwloc_calc_location_context_s *, void *, hwloc_obj_t), void *cbdata)
{
  hwloc_topology_t topology = lcontext->topology;
  int verbose = lcontext->verbose;
  hwloc_obj_t obj, prev = NULL;
  int pcivendor = -1, pcidevice = -1;
  const char *current, *dot;
  char *endp;
  int first = 0, step = 1, amount = 1, wrap = 0; /* assume the index suffix is `:0' by default */
  int err, i, max;

  if (*string == '[') {
    /* matching */
    current = string+1;

    if (type == HWLOC_OBJ_PCI_DEVICE) {
      /* try to match by [vendor:device] */
      pcivendor = strtoul(current, &endp, 16);
      if (*endp != ':') {
	if (verbose >= 0)
	  fprintf(stderr, "invalid PCI vendor:device matching specification %s\n", string);
	return -1;
      }
      if (endp == current)
	pcivendor = -1;
      current = endp+1;

      pcidevice = strtoul(current, &endp, 16);
      if (*endp != ']') {
	if (verbose >= 0)
	  fprintf(stderr, "invalid PCI vendor:device matching specification %s\n", string);
      	return -1;
      }
      if (endp == current)
	pcidevice = -1;
      current = endp+1;

      if (*current != ':' && *current != '\0') {
	if (verbose >= 0)
	  fprintf(stderr, "invalid PCI vendor:device matching specification %s\n", string);
      	return -1;
      }

    } else {
      /* no matching for non-PCI devices */
      if (verbose >= 0)
	fprintf(stderr, "invalid matching specification %s\n", string);
      return -1;
    }

  } else {
    /* no matching */
    current = string;
  }

  if (*current != '\0') {
    current++;
    err = hwloc_calc_parse_range(current,
				 &first, &amount, &step, &wrap,
				 &dot);
    if (dot) {
      fprintf(stderr, "hierarchical location %s only supported with normal object types\n", string);
      return -1;
    }
    if (err < 0)
      return -1;
  }

  max = hwloc_get_nbobjs_by_depth(topology, depth);

  for(i=0; i < max*(wrap+1); i++) {
    if (i == max && wrap) {
      i = 0;
      wrap = 0;
    }

    obj = hwloc_get_obj_by_depth(topology, depth, i);
    assert(obj);

    if (obj == prev) /* already used that object, stop wrapping around */
      break;

    if (type == HWLOC_OBJ_PCI_DEVICE) {
      if (pcivendor != -1 && (int) obj->attr->pcidev.vendor_id != pcivendor)
	continue;
      if (pcidevice != -1 && (int) obj->attr->pcidev.device_id != pcidevice)
	continue;
    }

    if (first--)
      continue;

    /* ok, got one object */
    if (verbose > 0)
      printf("using matching PCI object #%d bus id %04x:%02x:%02x.%01x\n", i,
	     obj->attr->pcidev.domain, obj->attr->pcidev.bus, obj->attr->pcidev.dev, obj->attr->pcidev.func);
    hwloc_calc_append_iodev(lcontext, cbfunc, cbdata, obj);

    if (!prev)
      prev = obj;

    amount--;
    if (!amount)
      break;

    first = step-1;
  }

  return 0;
}

static __hwloc_inline int
hwloc_calc_process_location(struct hwloc_calc_location_context_s *lcontext,
			    const char *arg, size_t typelen,
			    void (*cbfunc)(struct hwloc_calc_location_context_s *, void *, hwloc_obj_t), void *cbdata)
{
  hwloc_topology_t topology = lcontext->topology;
  int verbose = lcontext->verbose;
  const char *sep = &arg[typelen];
  hwloc_obj_type_t type = HWLOC_OBJ_TYPE_NONE;
  int depth;

  depth = hwloc_calc_parse_depth_prefix(lcontext,
					arg, typelen,
					&type);
  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN || depth == HWLOC_TYPE_DEPTH_MULTIPLE) {
    return -1;

  } else if (depth < 0 && depth != HWLOC_TYPE_DEPTH_NUMANODE) {
    /* special object without cpusets */

    /* if we didn't find a depth but found a type, handle special cases */
    hwloc_obj_t obj = NULL;

    if (*sep == ':' || *sep == '[') {
      return hwloc_calc_append_iodev_by_index(lcontext, type, depth, sep, cbfunc, cbdata);

    } else if (*sep == '=' && type == HWLOC_OBJ_PCI_DEVICE) {
      /* try to match a busid */
      obj = hwloc_get_pcidev_by_busidstring(topology, sep+1);
      if (obj)
	return hwloc_calc_append_iodev(lcontext, cbfunc, cbdata, obj);
      if (verbose >= 0)
	fprintf(stderr, "invalid PCI device %s\n", sep+1);
      return -1;

    } else if (*sep == '=' && type == HWLOC_OBJ_OS_DEVICE) {
      /* try to match a OS device name */
      while ((obj = hwloc_get_next_osdev(topology, obj)) != NULL) {
	if (!strcmp(obj->name, sep+1))
	  return hwloc_calc_append_iodev(lcontext, cbfunc, cbdata, obj);
      }
      if (verbose >= 0)
	fprintf(stderr, "invalid OS device %s\n", sep+1);
      return -1;

    } else if (*sep == '=' && type == HWLOC_OBJ_MISC) {
      /* try to match a Misc device name */
      obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_MISC, 0);
      while (obj) {
	if (!strcmp(obj->name, sep+1))
	  return hwloc_calc_append_iodev(lcontext, cbfunc, cbdata, obj);
	obj = obj->next_cousin;
      }
      if (verbose >= 0)
	fprintf(stderr, "invalid Misc object %s\n", sep+1);
      return -1;

    } else
      return -1;
  }

  /* look at indexes following this type/depth */
  return hwloc_calc_append_object_range(lcontext,
					hwloc_topology_get_complete_cpuset(topology),
					hwloc_topology_get_complete_nodeset(topology),
					depth, sep+1, cbfunc, cbdata);
}

struct hwloc_calc_set_context_s {
  int nodeset_input;
  int nodeset_output;
  hwloc_bitmap_t output_set;
};

struct hwloc_calc_process_location_set_cbdata_s {
  struct hwloc_calc_set_context_s *scontext;
  hwloc_bitmap_t set;
};

static __hwloc_inline void
hwloc_calc_process_location_set_cb(struct hwloc_calc_location_context_s *lcontext, void *_data, hwloc_obj_t obj)
{
  int verbose = lcontext->verbose;
  struct hwloc_calc_process_location_set_cbdata_s *cbdata = _data;
  hwloc_bitmap_t set = cbdata->set;
  int nodeset_output = cbdata->scontext->nodeset_output;
  /* walk up out of I/O objects */
  while (obj && !obj->cpuset)
    obj = obj->parent;
  if (!obj)
    /* do nothing */
    return;
  hwloc_calc_append_set(set,
			nodeset_output ? obj->nodeset : obj->cpuset,
			HWLOC_CALC_APPEND_ADD, verbose);
}

static __hwloc_inline int
hwloc_calc_process_location_as_set(struct hwloc_calc_location_context_s *lcontext,
				   struct hwloc_calc_set_context_s *scontext,
				   const char *arg)
{
  hwloc_topology_t topology = lcontext->topology;
  int verbose = lcontext->verbose;
  int nodeset_output = scontext->nodeset_output;
  int nodeset_input = scontext->nodeset_input;
  hwloc_bitmap_t output_set = scontext->output_set;
  hwloc_calc_append_mode_t mode = HWLOC_CALC_APPEND_ADD;
  size_t typelen;
  int err;

  if (*arg == '~') {
    mode = HWLOC_CALC_APPEND_CLR;
    arg++;
  } else if (*arg == 'x') {
    mode = HWLOC_CALC_APPEND_AND;
    arg++;
  } else if (*arg == '^') {
    mode = HWLOC_CALC_APPEND_XOR;
    arg++;
  }

  if (!strcmp(arg, "all") || !strcmp(arg, "root"))
    return hwloc_calc_append_set(output_set,
				 nodeset_output ? hwloc_topology_get_topology_nodeset(topology) : hwloc_topology_get_topology_cpuset(topology),
				 mode, verbose);

  /* try to match a type/depth followed by a special character */
  typelen = strspn(arg, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
  if (typelen && (arg[typelen] == ':' || arg[typelen] == '=' || arg[typelen] == '[')) {
    /* process type/depth */
    struct hwloc_calc_process_location_set_cbdata_s cbdata;
    cbdata.set = hwloc_bitmap_alloc();
    cbdata.scontext = scontext;
    err = hwloc_calc_process_location(lcontext, arg, typelen,
				      hwloc_calc_process_location_set_cb, &cbdata);
    if (!err)
      err = hwloc_calc_append_set(output_set, cbdata.set, mode, verbose);
    hwloc_bitmap_free(cbdata.set);

  } else {
    /* try to match a cpuset */
    char *tmp = (char*) arg;
    hwloc_bitmap_t newset;
    int taskset = ( strchr(tmp, ',') == NULL );

    /* check the infinite prefix */
    if (hwloc_strncasecmp(tmp, "0xf...f,", 7+!taskset) == 0) {
      tmp += 7+!taskset;
      if (0 == *tmp) {
        err = -1;
        goto out;
      }
    }

    if (taskset) {
      /* check that the remaining is 0x followed by a huge hexadecimal number */
      if (hwloc_strncasecmp(tmp, "0x", 2) != 0) {
        err = -1;
        goto out;
      }
      tmp += 2;
      if (0 == *tmp) {
        err = -1;
        goto out;
      }
      if (strlen(tmp) != strspn(tmp, "0123456789abcdefABCDEF")) {
        err = -1;
        goto out;
      }

    } else {
      /* check that the remaining is a comma-separated list of hexadecimal integer with 0x as an optional prefix */
      while (1) {
	char *next = strchr(tmp, ',');
	size_t len;
        if (hwloc_strncasecmp(tmp, "0x", 2) == 0) {
	  tmp += 2;
	  if (',' == *tmp || 0 == *tmp) {
	    err = -1;
	    goto out;
	  }
	}
	len = next ? (size_t) (next-tmp) : strlen(tmp);
	if (len != strspn(tmp, "0123456789abcdefABCDEF")) {
	  err = -1;
	  goto out;
	}
	if (!next)
	  break;
	tmp = next+1;
      }
    }

    newset = hwloc_bitmap_alloc();
    if (taskset)
      hwloc_bitmap_taskset_sscanf(newset, arg);
    else
      hwloc_bitmap_sscanf(newset, arg);
    if (nodeset_output && !nodeset_input) {
      hwloc_bitmap_t newnset = hwloc_bitmap_alloc();
      hwloc_cpuset_to_nodeset(topology, newset, newnset);
      err = hwloc_calc_append_set(output_set, newnset, mode, verbose);
      hwloc_bitmap_free(newnset);
    } else if (nodeset_input && !nodeset_output) {
      hwloc_bitmap_t newcset = hwloc_bitmap_alloc();
      hwloc_cpuset_from_nodeset(topology, newcset, newset);
      err = hwloc_calc_append_set(output_set, newcset, mode, verbose);
      hwloc_bitmap_free(newcset);
    } else {
      err = hwloc_calc_append_set(output_set, newset, mode, verbose);
    }
    hwloc_bitmap_free(newset);
  }

 out:
  return err;
}

static __hwloc_inline void
hwloc_calc_locations_usage(FILE *where)
{
  fprintf (where, "    core:2-3        for the second and third core\n");
  fprintf (where, "    node:1.pu:2       the third PU of the second NUMA node\n");
  fprintf (where, "    0x12345678        a CPU set given a bitmask string\n");
  fprintf (where, "    os=eth0           the operating system device named eth0\n");
  fprintf (where, "    pci=0000:01:02.0  the PCI device with the given bus ID\n");
  fprintf (where, "  with prefix ~ to remove, ^ for xor and x for intersection\n");
  fprintf (where, "  (see Location Specification in hwloc(7) for details).\n");
}

#endif /* HWLOC_CALC_H */
