/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_MASK_H
#define HWLOC_MASK_H

#include <private/config.h>
#include <private/private.h>
#include <private/misc.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <assert.h>

typedef enum hwloc_mask_append_mode_e {
  HWLOC_MASK_APPEND_ADD,
  HWLOC_MASK_APPEND_CLR,
  HWLOC_MASK_APPEND_AND,
  HWLOC_MASK_APPEND_XOR,
} hwloc_mask_append_mode_t;

static inline int
hwloc_mask_append_cpuset(hwloc_cpuset_t set, hwloc_const_cpuset_t newset,
		       hwloc_mask_append_mode_t mode, int verbose)
{
  char *s1 = hwloc_cpuset_printf_value(newset);
  char *s2 = hwloc_cpuset_printf_value(set);
  switch (mode) {
  case HWLOC_MASK_APPEND_ADD:
    if (verbose)
      fprintf(stderr, "adding %s to %s\n",
          s1, s2);
    hwloc_cpuset_or(set, set, newset);
    break;
  case HWLOC_MASK_APPEND_CLR:
    if (verbose)
      fprintf(stderr, "clearing %s from %s\n",
          s1, s2);
    hwloc_cpuset_andnot(set, set, newset);
    break;
  case HWLOC_MASK_APPEND_AND:
    if (verbose)
      fprintf(stderr, "and'ing %s from %s\n",
          s1, s2);
    hwloc_cpuset_and(set, set, newset);
    break;
  case HWLOC_MASK_APPEND_XOR:
    if (verbose)
      fprintf(stderr, "xor'ing %s from %s\n",
          s1, s2);
    hwloc_cpuset_xor(set, set, newset);
    break;
  default:
    assert(0);
  }
  free(s1);
  free(s2);
  return 0;
}

static inline hwloc_obj_t __hwloc_attribute_pure
hwloc_mask_get_obj_inside_cpuset_by_depth(hwloc_topology_t topology, hwloc_const_cpuset_t rootset,
					 unsigned depth, unsigned i, int logical)
{
  if (logical) {
    return hwloc_get_obj_inside_cpuset_by_depth(topology, rootset, depth, i);
  } else {
    hwloc_obj_t obj = NULL;
    while ((obj = hwloc_get_next_obj_inside_cpuset_by_depth(topology, rootset, depth, obj)) != NULL) {
      if (obj->os_index == i)
        return obj;
    }
    return NULL;
  }
}

static inline int
hwloc_mask_append_object(hwloc_topology_t topology, unsigned topodepth,
		       hwloc_const_cpuset_t rootset, const char *string, int logical,
		       hwloc_cpuset_t set, int verbose)
{
  hwloc_obj_t obj;
  unsigned depth, width;
  char *sep, *sep2, *sep3;
  unsigned first, wrap, amount, step;
  unsigned i,j;

  if (!hwloc_namecoloncmp(string, "system", 2))
    depth = hwloc_get_type_or_above_depth(topology, HWLOC_OBJ_SYSTEM);
  else if (!hwloc_namecoloncmp(string, "machine", 1))
    depth = hwloc_get_type_or_above_depth(topology, HWLOC_OBJ_MACHINE);
  else if (!hwloc_namecoloncmp(string, "node", 1))
    depth = hwloc_get_type_or_above_depth(topology, HWLOC_OBJ_NODE);
  else if (!hwloc_namecoloncmp(string, "socket", 2))
    depth = hwloc_get_type_or_above_depth(topology, HWLOC_OBJ_SOCKET);
  else if (!hwloc_namecoloncmp(string, "core", 1))
    depth = hwloc_get_type_or_above_depth(topology, HWLOC_OBJ_CORE);
  else if (!hwloc_namecoloncmp(string, "pu", 1) || !hwloc_namecoloncmp(string, "proc", 1) /* backward compat with 0.9 */)
    depth = hwloc_get_type_or_above_depth(topology, HWLOC_OBJ_PU);
  else {
    char *end;
    depth = strtol(string, &end, 0);
    if (end == string) {
      if (verbose)
        fprintf(stderr, "invalid object name %s\n", string);
      return -1;
    }
  }

  if (depth >= topodepth) {
    if (verbose)
      fprintf(stderr, "ignoring invalid depth %u\n", depth);
    return -1;
  }
  width = hwloc_get_nbobjs_by_depth(topology, depth);

  sep = strchr(string, ':');
  if (!sep) {
    if (verbose)
      fprintf(stderr, "missing colon separator in argument %s\n", string);
    return -1;
  }

  first = atoi(sep+1);
  amount = 1;
  step = 1;
  wrap = 0;
  if (!isdigit(*(sep+1))) {
    if (!strncmp(sep+1, "all", 3)) {
      first = 0;
      amount = width;
    } else if (!strncmp(sep+1, "odd", 3)) {
      first = 1;
      step = 2;
      amount = (width+1)/2;
    } else if (!strncmp(sep+1, "even", 4)) {
      first = 0;
      step = 2;
      amount = (width+1)/2;
    }
  }

  sep3 = strchr(sep+1, '.');

  sep2 = strchr(sep+1, '-');
  if (sep2 && (sep2 < sep3 || !sep3)) {
    if (*(sep2+1) == '\0')
      amount = width-first;
    else
      amount = atoi(sep2+1)-first+1;
  } else {
    sep2 = strchr(sep+1, ':');
    if (sep2 && (sep2 < sep3 || !sep3)) {
      amount = atoi(sep2+1);
      wrap = 1;
    }
  }

  for(i=first, j=0; j<amount; i+=step, j++) {
    if (wrap && i==width)
      i = 0;

    obj = hwloc_mask_get_obj_inside_cpuset_by_depth(topology, rootset, depth, i, logical);
    if (verbose) {
      char * s = hwloc_cpuset_printf_value(rootset);
      if (obj)
	printf("object #%u depth %u below cpuset %s found\n",
	       i, depth, s);
      else
	printf("object #%u depth %u below cpuset %s does not exist\n",
	       i, depth, s);
      free(s);
    }
    if (obj) {
      if (sep3)
	hwloc_mask_append_object(topology, topodepth, obj->cpuset, sep3+1, logical, set, verbose);
      else
	/* add to the temporary cpuset
	 * and let the caller add/clear/and/xor for the actual final cpuset depending on cmdline options
	 */
        hwloc_mask_append_cpuset(set, obj->cpuset, HWLOC_MASK_APPEND_ADD, verbose);
    }
  }

  return 0;
}

static inline int
hwloc_mask_process_arg(hwloc_topology_t topology, unsigned topodepth,
		     const char *arg, int logical, hwloc_cpuset_t set,
		     int verbose)
{
  char *colon;
  hwloc_mask_append_mode_t mode = HWLOC_MASK_APPEND_ADD;
  int err;

  if (*arg == '~') {
    mode = HWLOC_MASK_APPEND_CLR;
    arg++;
  } else if (*arg == 'x') {
    mode = HWLOC_MASK_APPEND_AND;
    arg++;
  } else if (*arg == '^') {
    mode = HWLOC_MASK_APPEND_XOR;
    arg++;
  }

  colon = strchr(arg, ':');
  if (colon) {
    hwloc_cpuset_t newset = hwloc_cpuset_alloc();
    err = hwloc_mask_append_object(topology, topodepth, hwloc_topology_get_complete_cpuset(topology), arg, logical, newset, verbose);
    if (!err)
      err = hwloc_mask_append_cpuset(set, newset, mode, verbose);
    hwloc_cpuset_free(newset);
  } else {
    /* try to parse as a comma-separated list of integer with 0x as an optional prefix */
    char *tmp = (char*) arg;
    hwloc_cpuset_t newset;
    while (1) {
      char *next = strchr(tmp, ',');
      size_t len;
      if (strncasecmp(tmp, "0x", 2) == 0) {
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
    newset = hwloc_cpuset_alloc();
    hwloc_cpuset_from_string(newset, arg);
    err = hwloc_mask_append_cpuset(set, newset, mode, verbose);
    hwloc_cpuset_free(newset);
  }

 out:
  return err;
}

#endif /* HWLOC_MASK_H */
