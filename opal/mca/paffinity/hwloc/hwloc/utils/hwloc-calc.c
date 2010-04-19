/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/private.h>
#include <hwloc-calc.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void usage(FILE *where)
{
  fprintf(where, "Usage: hwloc-calc [options] <location> ...\n");
  fprintf(where, " <location> may be a space-separated list of cpusets or objects\n");
  fprintf(where, "            as supported by the hwloc-bind utility.\n");
  fprintf(where, "Options:\n");
  fprintf(where, "  -l --logical\tuse logical object indexes (default)\n");
  fprintf(where, "  -p --physical\tuse physical object indexes\n");
  fprintf(where, "  --li --logical-input\tuse logical indexes for input (default)\n");
  fprintf(where, "  --lo --logical-output\tuse logical indexes for output (default)\n");
  fprintf(where, "  --pi --physical-input\tuse physical indexes for input\n");
  fprintf(where, "  --po --physical-output\tuse physical indexes for output\n");
  fprintf(where, "  --PUlist\treport the list of processing units' indexes in the CPU set\n");
  fprintf(where, "  --nodelist\treport the list of memory nodes' indexes near the CPU set\n");
  fprintf(where, "  --objects\treport the list of largest objects in the CPU set\n");
  fprintf(where, "  -v\t\tverbose messages\n");
  fprintf(where, "  --version\treport version and exit\n");
}

int main(int argc, char *argv[])
{
  hwloc_topology_t topology;
  unsigned depth;
  hwloc_cpuset_t set;
  int verbose = 0;
  int logicali = 1;
  int logicalo = 1;
  int nodelist = 0;
  int pulist = 0;
  int showobjs = 0;
  char **orig_argv = argv;

  set = hwloc_cpuset_alloc();

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);
  depth = hwloc_topology_get_depth(topology);

  while (argc >= 2) {
    if (*argv[1] == '-') {
      if (!strcmp(argv[1], "-v")) {
        verbose = 1;
        goto next;
      }
      if (!strcmp(argv[1], "--help")) {
	usage(stdout);
	return EXIT_SUCCESS;
      }
      if (!strcasecmp(argv[1], "--pulist") || !strcmp(argv[1], "--proclist") /* backward compat with 0.9 */) {
	pulist = 1;
        goto next;
      }
      if (!strcmp(argv[1], "--nodelist")) {
	nodelist = 1;
        goto next;
      }
      if (!strcmp(argv[1], "--objects")) {
	showobjs = 1;
        goto next;
      }
      if (!strcmp(argv[1], "--version")) {
        printf("%s %s\n", orig_argv[0], VERSION);
        exit(EXIT_SUCCESS);
      }
      if (!strcmp(argv[1], "-l") || !strcmp(argv[1], "--logical")) {
	logicali = 1;
	logicalo = 1;
	goto next;
      }
      if (!strcmp(argv[1], "--li") || !strcmp(argv[1], "--logical-input")) {
	logicali = 1;
	goto next;
      }
      if (!strcmp(argv[1], "--lo") || !strcmp(argv[1], "--logical-output")) {
	logicalo = 1;
	goto next;
      }
      if (!strcmp(argv[1], "-p") || !strcmp(argv[1], "--physical")) {
	logicali = 0;
	logicalo = 0;
	goto next;
      }
      if (!strcmp(argv[1], "--pi") || !strcmp(argv[1], "--physical-input")) {
	logicali = 0;
	goto next;
      }
      if (!strcmp(argv[1], "--po") || !strcmp(argv[1], "--physical-output")) {
	logicalo = 0;
	goto next;
      }
      usage(stderr);
      return EXIT_FAILURE;
    }

    if (hwloc_mask_process_arg(topology, depth, argv[1], logicali, set, verbose) < 0) {
      if (verbose)
	fprintf(stderr, "ignored unrecognized argument %s\n", argv[1]);
    }

 next:
    argc--;
    argv++;
  }

  if (showobjs) {
    hwloc_cpuset_t remaining = hwloc_cpuset_dup(set);
    int first = 1;
    while (!hwloc_cpuset_iszero(remaining)) {
      char type[64];
      unsigned index;
      hwloc_obj_t obj = hwloc_get_first_largest_obj_inside_cpuset(topology, remaining);
      hwloc_obj_type_snprintf(type, sizeof(type), obj, 1);
      index = logicalo ? obj->logical_index : obj->os_index;
      printf("%s%s:%u", first ? "" : " ", type, index);
      hwloc_cpuset_andnot(remaining, remaining, obj->cpuset);
      first = 0;
    }
    printf("\n");
    hwloc_cpuset_free(remaining);
  } else if (pulist) {
    hwloc_obj_t proc, prev = NULL;
    while ((proc = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_PU, prev)) != NULL) {
      if (prev)
	printf(",");
      printf("%u", logicalo ? proc->logical_index : proc->os_index);
      prev = proc;
    }
    printf("\n");
  } else if (nodelist) {
    hwloc_obj_t node, prev = NULL;
    while ((node = hwloc_get_next_obj_covering_cpuset_by_type(topology, set, HWLOC_OBJ_NODE, prev)) != NULL) {
      if (prev)
	printf(",");
      printf("%u", logicalo ? node->logical_index : node->os_index);
      prev = node;
    }
    printf("\n");
  } else {
    char *string = NULL;
    hwloc_cpuset_asprintf(&string, set);
    printf("%s\n", string);
    free(string);
  }

  hwloc_topology_destroy(topology);

  hwloc_cpuset_free(set);

  return EXIT_SUCCESS;
}
