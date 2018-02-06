/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <assert.h>

#include "misc.h"
#include "hwloc-calc.h"

static int pid_number = -1;
static hwloc_pid_t pid;
static int verbose_mode = 0;
static int logical = 1;
static int show_ancestors = 0;
static int show_ancestor_depth = HWLOC_TYPE_DEPTH_UNKNOWN;
static int show_children = 0;
static int show_descendants_depth = HWLOC_TYPE_DEPTH_UNKNOWN;
static int show_index_prefix = 0;
static unsigned current_obj;

void usage(const char *name, FILE *where)
{
  fprintf (where, "Usage: %s [ options ] [ locations ]\n", name);
  fprintf (where, "\nOutput options:\n");
  fprintf (where, "  --objects             Report information about specific objects\n");
  fprintf (where, "  --topology            Report information the topology\n");
  fprintf (where, "  --support             Report information about supported features\n");
  fprintf (where, "  -v --verbose          Include additional details\n");
  fprintf (where, "  -s --silent           Reduce the amount of details to show\n");
  fprintf (where, "  --ancestors           Display the chain of ancestor objects up to the root\n");
  fprintf (where, "  --ancestor <type>     Only display the ancestor of the given type\n");
  fprintf (where, "  --children            Display all children\n");
  fprintf (where, "  --descendants <type>  Only display descendants of the given type\n");
  fprintf (where, "  -n                    Prefix each line with the index of the considered object\n");
  fprintf (where, "Object filtering options:\n");
  fprintf (where, "  --restrict <cpuset>   Restrict the topology to processors listed in <cpuset>\n");
  fprintf (where, "  --restrict binding    Restrict the topology to the current process binding\n");
  fprintf (where, "  --filter <type>:<knd> Filter objects of the given type, or all.\n");
  fprintf (where, "     <knd> may be `all' (keep all), `none' (remove all), `structure' or `basic'\n");
  fprintf (where, "  --no-icaches          Do not show instruction caches\n");
  fprintf (where, "  --no-io               Do not show any I/O device or bridge\n");
  fprintf (where, "  --no-bridges          Do not any I/O bridge except hostbridges\n");
  fprintf (where, "  --whole-io            Show all I/O devices and bridges\n");
  fprintf (where, "Input options:\n");
  hwloc_utils_input_format_usage(where, 6);
  fprintf (where, "  --thissystem          Assume that the input topology provides the topology\n"
		  "                        for the system on which we are running\n");
  fprintf (where, "  --pid <pid>           Detect topology as seen by process <pid>\n");
  fprintf (where, "  --whole-system        Do not consider administration limitations\n");
  fprintf (where, "  -l --logical          Use logical object indexes for input (default)\n");
  fprintf (where, "  -p --physical         Use physical object indexes for input\n");
  fprintf (where, "Miscellaneous options:\n");
  fprintf (where, "  --version             Report version and exit\n");
}

static void
hwloc_info_show_obj(hwloc_topology_t topology, hwloc_obj_t obj, const char *type, const char *prefix, int verbose)
{
  char s[128];
  unsigned i;
  if (verbose < 0)
    return;
  printf("%s type = %s\n", prefix, hwloc_obj_type_string(obj->type));
  printf("%s full type = %s\n", prefix, type);
  printf("%s logical index = %u\n", prefix, obj->logical_index);
  if (obj->os_index != (unsigned) -1)
    printf("%s os index = %u\n", prefix, obj->os_index);
  printf("%s gp index = %llu\n", prefix, (unsigned long long) obj->gp_index);
  if (obj->name)
    printf("%s name = %s\n", prefix, obj->name);
  printf("%s depth = %d\n", prefix, obj->depth);
  printf("%s sibling rank = %u\n", prefix, obj->sibling_rank);
  printf("%s children = %u\n", prefix, obj->arity);
  printf("%s memory children = %u\n", prefix, obj->memory_arity);
  printf("%s i/o children = %u\n", prefix, obj->io_arity);
  printf("%s misc children = %u\n", prefix, obj->misc_arity);

  if (obj->type == HWLOC_OBJ_NUMANODE) {
    printf("%s local memory = %llu\n", prefix, (unsigned long long) obj->attr->numanode.local_memory);
  }
  if (obj->total_memory)
    printf("%s total memory = %llu\n", prefix, (unsigned long long) obj->total_memory);

  if (obj->cpuset) {
    hwloc_bitmap_snprintf(s, sizeof(s), obj->cpuset);
    printf("%s cpuset = %s\n", prefix, s);

    hwloc_bitmap_snprintf(s, sizeof(s), obj->complete_cpuset);
    printf("%s complete cpuset = %s\n", prefix, s);

    {
      hwloc_bitmap_t allowed_cpuset = hwloc_bitmap_dup(obj->cpuset);
      hwloc_bitmap_and(allowed_cpuset, allowed_cpuset, hwloc_topology_get_allowed_cpuset(topology));
      hwloc_bitmap_snprintf(s, sizeof(s), allowed_cpuset);
      hwloc_bitmap_free(allowed_cpuset);
      printf("%s allowed cpuset = %s\n", prefix, s);
    }

    hwloc_bitmap_snprintf(s, sizeof(s), obj->nodeset);
    printf("%s nodeset = %s\n", prefix, s);

    hwloc_bitmap_snprintf(s, sizeof(s), obj->complete_nodeset);
    printf("%s complete nodeset = %s\n", prefix, s);

    {
      hwloc_bitmap_t allowed_nodeset = hwloc_bitmap_dup(obj->nodeset);
      hwloc_bitmap_and(allowed_nodeset, allowed_nodeset, hwloc_topology_get_allowed_nodeset(topology));
      hwloc_bitmap_snprintf(s, sizeof(s), allowed_nodeset);
      hwloc_bitmap_free(allowed_nodeset);
      printf("%s allowed nodeset = %s\n", prefix, s);
    }
  }

  switch (obj->type) {
  case HWLOC_OBJ_L1CACHE:
  case HWLOC_OBJ_L2CACHE:
  case HWLOC_OBJ_L3CACHE:
  case HWLOC_OBJ_L4CACHE:
  case HWLOC_OBJ_L5CACHE:
  case HWLOC_OBJ_L1ICACHE:
  case HWLOC_OBJ_L2ICACHE:
  case HWLOC_OBJ_L3ICACHE:
    printf("%s attr cache depth = %u\n", prefix, obj->attr->cache.depth);
    switch (obj->attr->cache.type) {
    case HWLOC_OBJ_CACHE_UNIFIED: printf("%s attr cache type = Unified\n", prefix); break;
    case HWLOC_OBJ_CACHE_DATA: printf("%s attr cache type = Data\n", prefix); break;
    case HWLOC_OBJ_CACHE_INSTRUCTION: printf("%s attr cache type = Instruction\n", prefix); break;
    }
    printf("%s attr cache size = %llu\n", prefix, (unsigned long long) obj->attr->cache.size);
    printf("%s attr cache line size = %u\n", prefix, obj->attr->cache.linesize);
    if (obj->attr->cache.associativity == -1)
      printf("%s attr cache ways = Fully-associative\n", prefix);
    else if (obj->attr->cache.associativity != 0)
      printf("%s attr cache ways = %d\n", prefix, obj->attr->cache.associativity);
    break;
  case HWLOC_OBJ_GROUP:
    printf("%s attr group depth = %u\n", prefix, obj->attr->group.depth);
    break;
  case HWLOC_OBJ_BRIDGE:
    switch (obj->attr->bridge.upstream_type) {
    case HWLOC_OBJ_BRIDGE_HOST:
      printf("%s attr bridge upstream type = Host\n", prefix);
      break;
    case HWLOC_OBJ_BRIDGE_PCI:
      printf("%s attr bridge upstream type = PCI\n", prefix);
      printf("%s attr PCI bus id = %04x:%02x:%02x.%01x\n",
	     prefix, obj->attr->pcidev.domain, obj->attr->pcidev.bus, obj->attr->pcidev.dev, obj->attr->pcidev.func);
      printf("%s attr PCI class = %04x\n",
	     prefix, obj->attr->pcidev.class_id);
      printf("%s attr PCI id = %04x:%04x\n",
	     prefix, obj->attr->pcidev.vendor_id, obj->attr->pcidev.device_id);
      if (obj->attr->pcidev.linkspeed)
	printf("%s attr PCI linkspeed = %f GB/s\n", prefix, obj->attr->pcidev.linkspeed);
      break;
    }
    switch (obj->attr->bridge.downstream_type) {
    case HWLOC_OBJ_BRIDGE_HOST:
      assert(0);
    case HWLOC_OBJ_BRIDGE_PCI:
      printf("%s attr bridge downstream type = PCI\n", prefix);
      printf("%s attr PCI secondary bus = %02x\n",
	     prefix, obj->attr->bridge.downstream.pci.secondary_bus);
      printf("%s attr PCI subordinate bus = %02x\n",
	     prefix, obj->attr->bridge.downstream.pci.subordinate_bus);
      break;
    }
    break;
  case HWLOC_OBJ_PCI_DEVICE:
    printf("%s attr PCI bus id = %04x:%02x:%02x.%01x\n",
	   prefix, obj->attr->pcidev.domain, obj->attr->pcidev.bus, obj->attr->pcidev.dev, obj->attr->pcidev.func);
    printf("%s attr PCI class = %04x\n",
	   prefix, obj->attr->pcidev.class_id);
    printf("%s attr PCI id = %04x:%04x\n",
	   prefix, obj->attr->pcidev.vendor_id, obj->attr->pcidev.device_id);
    if (obj->attr->pcidev.linkspeed)
      printf("%s attr PCI linkspeed = %f GB/s\n", prefix, obj->attr->pcidev.linkspeed);
    break;
  case HWLOC_OBJ_OS_DEVICE:
    printf("%s attr osdev type = %s\n", prefix, type);
    break;
  default:
    /* nothing to show */
    break;
  }

  printf("%s symmetric subtree = %d\n", prefix, obj->symmetric_subtree);

  for(i=0; i<obj->infos_count; i++) {
    struct hwloc_info_s *info = &obj->infos[i];
    printf("%s info %s = %s\n", prefix, info->name, info->value);
  }
}

static void
hwloc_calc_process_location_info_cb(struct hwloc_calc_location_context_s *lcontext,
				    void *_data __hwloc_attribute_unused,
				    hwloc_obj_t obj)
{
  hwloc_topology_t topology = lcontext->topology;
  int verbose = lcontext->verbose;
  char prefix[32];
  char objs[128];

  prefix[0] = '\0';
  if (show_index_prefix)
    snprintf(prefix, sizeof(prefix), "%u: ", current_obj);

  hwloc_obj_type_snprintf(objs, sizeof(objs), obj, 1);

  if (show_ancestors) {
    char parents[128];
    unsigned level = 0;
    hwloc_obj_t parent = obj;
    while (parent) {
      if (show_index_prefix)
	snprintf(prefix, sizeof(prefix), "%u.%u: ", current_obj, level);
      hwloc_obj_type_snprintf(parents, sizeof(parents), parent, 1);
      if (verbose < 0)
	printf("%s%s:%u\n", prefix, parents, parent->logical_index);
      else if (level)
	printf("%s%s L#%u = parent #%u of %s L#%u\n",
	       prefix, parents, parent->logical_index, level, objs, obj->logical_index);
      else
	printf("%s%s L#%u\n", prefix, parents, parent->logical_index);
      hwloc_info_show_obj(topology, parent, parents, prefix, verbose);
      parent = parent->parent;
      level++;
    }
  } else if (show_ancestor_depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    char parents[128];
    hwloc_obj_t parent = obj;
    while (parent) {
      if (parent->depth == show_ancestor_depth) {
	hwloc_obj_type_snprintf(parents, sizeof(parents), parent, 1);
	if (verbose < 0)
	  printf("%s%s:%u\n", prefix, parents, parent->logical_index);
	else
	  printf("%s%s L#%u = parent of %s L#%u\n",
		 prefix, parents, parent->logical_index, objs, obj->logical_index);
	hwloc_info_show_obj(topology, parent, parents, prefix, verbose);
	break;
      }
      parent = parent->parent;
    }
  } else if (show_children) {
    unsigned i = 0;
    hwloc_obj_t child = NULL;
    while ((child = hwloc_get_next_child(topology, obj, child)) != NULL) {
      char childs[128];
      if (show_index_prefix)
	snprintf(prefix, sizeof(prefix), "%u.%u: ", current_obj, i);
      hwloc_obj_type_snprintf(childs, sizeof(childs), child, 1);
      if (verbose < 0)
	printf("%s%s:%u\n", prefix, childs, child->logical_index);
      else
	printf("%s%s L#%u = child #%u of %s L#%u\n",
	       prefix, childs, child->logical_index, i, objs, obj->logical_index);
      hwloc_info_show_obj(topology, child, childs, prefix, verbose);
      i++;
    }
  } else if (show_descendants_depth != HWLOC_TYPE_DEPTH_UNKNOWN) {
    if (show_descendants_depth >= 0) {
      /* normal level */
      unsigned i = 0;
      unsigned n = hwloc_calc_get_nbobjs_inside_sets_by_depth(lcontext, obj->cpuset, obj->nodeset, show_descendants_depth);
      for(i=0; i<n; i++) {
	hwloc_obj_t child = hwloc_calc_get_obj_inside_sets_by_depth(lcontext, obj->cpuset, obj->nodeset, show_descendants_depth, i);
	char childs[128];
	if (show_index_prefix)
	  snprintf(prefix, sizeof(prefix), "%u.%u: ", current_obj, i);
	hwloc_obj_type_snprintf(childs, sizeof(childs), child, 1);
	if (verbose < 0)
	  printf("%s%s:%u\n", prefix, childs, child->logical_index);
	else
	  printf("%s%s L#%u = descendant #%u of %s L#%u\n",
		 prefix, childs, child->logical_index, i, objs, obj->logical_index);
	hwloc_info_show_obj(topology, child, childs, prefix, verbose);
      }
    } else {
      /* custom level */
      unsigned i = 0;
      hwloc_obj_t child = NULL;
      while ((child = hwloc_get_next_obj_by_depth(topology, show_descendants_depth, child)) != NULL) {
	char childs[128];
	hwloc_obj_t parent = child->parent;
	if (obj->cpuset) {
	  while (parent && !parent->cpuset)
	    parent = parent->parent;
	  if (!parent)
	    continue;
	  if (!hwloc_bitmap_isincluded(parent->cpuset, obj->cpuset)
	      || !hwloc_bitmap_isincluded(parent->nodeset, obj->nodeset))
	    continue;
	} else {
	  while (parent && parent != obj)
	    parent = parent->parent;
	  if (!parent)
	    continue;
	}
	if (show_index_prefix)
	  snprintf(prefix, sizeof(prefix), "%u.%u: ", current_obj, i);
	hwloc_obj_type_snprintf(childs, sizeof(childs), child, 1);
	if (verbose < 0)
	  printf("%s%s:%u\n", prefix, childs, child->logical_index);
	else
	  printf("%s%s L#%u = descendant #%u of %s L#%u\n",
		 prefix, childs, child->logical_index, i, objs, obj->logical_index);
	hwloc_info_show_obj(topology, child, childs, prefix, verbose);
	i++;
      }
    }
  } else {
    if (verbose < 0)
      printf("%s%s:%u\n", prefix, objs, obj->logical_index);
    else
      printf("%s%s L#%u\n", prefix, objs, obj->logical_index);
    hwloc_info_show_obj(topology, obj, objs, prefix, verbose);
  }

  current_obj++;
}

int
main (int argc, char *argv[])
{
  int err;
  hwloc_topology_t topology;
  int topodepth;
  unsigned long flags = 0;
  char * callname;
  char * input = NULL;
  enum hwloc_utils_input_format input_format = HWLOC_UTILS_INPUT_DEFAULT;
  const char *show_ancestor_type = NULL;
  const char *show_descendants_type = NULL;
  char *restrictstring = NULL;
  size_t typelen;
  int opt;
  enum hwloc_info_mode { HWLOC_INFO_MODE_UNKNOWN, HWLOC_INFO_MODE_TOPOLOGY, HWLOC_INFO_MODE_OBJECTS, HWLOC_INFO_MODE_SUPPORT } mode = HWLOC_INFO_MODE_UNKNOWN;

  callname = strrchr(argv[0], '/');
  if (!callname)
    callname = argv[0];
  else
    callname++;
  /* skip argv[0], handle options */
  argc--;
  argv++;

  hwloc_utils_check_api_version(callname);

  /* enable verbose backends */
  putenv((char *) "HWLOC_XML_VERBOSE=1");
  putenv((char *) "HWLOC_SYNTHETIC_VERBOSE=1");

  err = hwloc_topology_init (&topology);
  if (err)
    return EXIT_FAILURE;

  hwloc_topology_set_all_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_ALL);
  hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);

  while (argc >= 1) {
    opt = 0;
    if (*argv[0] == '-') {
      if (!strcmp (argv[0], "--objects"))
	mode = HWLOC_INFO_MODE_OBJECTS;
      else if (!strcmp (argv[0], "--topology"))
	mode = HWLOC_INFO_MODE_TOPOLOGY;
      else if (!strcmp (argv[0], "--support"))
	mode = HWLOC_INFO_MODE_SUPPORT;
      else if (!strcmp (argv[0], "-v") || !strcmp (argv[0], "--verbose"))
        verbose_mode++;
      else if (!strcmp (argv[0], "-s") || !strcmp (argv[0], "--silent"))
        verbose_mode--;
      else if (!strcmp (argv[0], "-h") || !strcmp (argv[0], "--help")) {
	usage(callname, stdout);
        exit(EXIT_SUCCESS);
      }
      else if (!strcmp (argv[0], "-n"))
	show_index_prefix = 1;
      else if (!strcmp (argv[0], "--ancestors"))
	show_ancestors = 1;
      else if (!strcmp (argv[0], "--ancestor")) {
	if (argc < 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	show_ancestor_type = argv[1];
	opt = 1;
      }
      else if (!strcmp (argv[0], "--children"))
	show_children = 1;
      else if (!strcmp (argv[0], "--descendants")) {
	if (argc < 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	show_descendants_type = argv[1];
	opt = 1;
      }
      else if (!strcmp (argv[0], "--filter")) {
        hwloc_obj_type_t type;
        char *colon;
        enum hwloc_type_filter_e filter = HWLOC_TYPE_FILTER_KEEP_ALL;
        int all = 0;
	int allio = 0;
	int allcaches = 0;
	int allicaches = 0;
        if (argc < 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
        colon = strchr(argv[1], ':');
        if (colon) {
          *colon = '\0';
          if (!strcmp(colon+1, "none"))
            filter = HWLOC_TYPE_FILTER_KEEP_NONE;
          else if (!strcmp(colon+1, "all"))
            filter = HWLOC_TYPE_FILTER_KEEP_ALL;
          else if (!strcmp(colon+1, "structure"))
            filter = HWLOC_TYPE_FILTER_KEEP_STRUCTURE;
	  else if (!strcmp(colon+1, "important"))
	    filter = HWLOC_TYPE_FILTER_KEEP_IMPORTANT;
	  else {
	    fprintf(stderr, "Unsupported filtering kind `%s' passed to --filter.\n", colon+1);
	    usage (callname, stderr);
	    exit(EXIT_FAILURE);
	  }
        }
        if (!strcmp(argv[1], "all"))
          all = 1;
	else if (!strcmp(argv[1], "io"))
	  allio = 1;
	else if (!strcmp(argv[1], "cache"))
	  allcaches = 1;
	else if (!strcmp(argv[1], "icache"))
	  allicaches = 1;
        else if (hwloc_type_sscanf(argv[1], &type, NULL, 0) < 0) {
          fprintf(stderr, "Unsupported type `%s' passed to --filter.\n", argv[1]);
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
        }
        if (all)
          hwloc_topology_set_all_types_filter(topology, filter);
	else if (allio)
          hwloc_topology_set_io_types_filter(topology, filter);
	else if (allcaches)
	  hwloc_topology_set_cache_types_filter(topology, filter);
	else if (allicaches)
	  hwloc_topology_set_icache_types_filter(topology, filter);
        else
          hwloc_topology_set_type_filter(topology, type, filter);
        opt = 1;
      }
      else if (!strcmp (argv[0], "--no-icaches")) {
	hwloc_topology_set_icache_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_NONE);
      } else if (!strcmp (argv[0], "--whole-system"))
	flags |= HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM;
      else if (!strcmp (argv[0], "--no-io")) {
	hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_NONE);
      } else if (!strcmp (argv[0], "--no-bridges")) {
	hwloc_topology_set_type_filter(topology, HWLOC_OBJ_BRIDGE, HWLOC_TYPE_FILTER_KEEP_NONE);
      } else if (!strcmp (argv[0], "--whole-io")) {
	hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_ALL);
      } else if (!strcmp (argv[0], "--thissystem"))
	flags |= HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM;
      else if (!strcmp (argv[0], "--restrict")) {
	if (argc < 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	restrictstring = strdup(argv[1]);
	opt = 1;
      }

      else if (hwloc_utils_lookup_input_option(argv, argc, &opt,
					       &input, &input_format,
					       callname)) {
	/* we'll enable later */
      }
      else if (!strcmp (argv[0], "--pid")) {
	if (argc < 2) {
	  usage (callname, stderr);
	  exit(EXIT_FAILURE);
	}
	pid_number = atoi(argv[1]); opt = 1;
      }
      else if (!strcmp(argv[0], "-l") || !strcmp(argv[0], "--logical"))
	logical = 1;
      else if (!strcmp(argv[0], "-p") || !strcmp(argv[0], "--physical"))
	logical = 0;
      else if (!strcmp (argv[0], "--version")) {
        printf("%s %s\n", callname, HWLOC_VERSION);
        exit(EXIT_SUCCESS);
      }
      else {
	fprintf (stderr, "Unrecognized option: %s\n", argv[0]);
	usage(callname, stderr);
	return EXIT_FAILURE;
      }
      argc -= opt+1;
      argv += opt+1;
    } else {
      /* not an option */
      break;
    }
  }

  hwloc_topology_set_flags(topology, flags);

  if (input) {
    err = hwloc_utils_enable_input_format(topology, input, &input_format, verbose_mode, callname);
    if (err)
      return err;
  }

  if (pid_number > 0) {
    pid = hwloc_pid_from_number(pid_number, 0);
    if (hwloc_topology_set_pid(topology, pid)) {
      perror("Setting target pid");
      return EXIT_FAILURE;
    }
  }

  err = hwloc_topology_load (topology);
  if (err)
    return EXIT_FAILURE;

  topodepth = hwloc_topology_get_depth(topology);

  if (show_ancestor_type) {
    err = hwloc_type_sscanf_as_depth(show_ancestor_type, NULL, topology, &show_ancestor_depth);
    if (err < 0) {
      fprintf(stderr, "unrecognized --ancestor type %s\n", show_ancestor_type);
      usage(callname, stderr);
      return EXIT_FAILURE;
    }
    if (show_ancestor_depth == HWLOC_TYPE_DEPTH_UNKNOWN) {
      fprintf(stderr, "unavailable --ancestor type %s\n", show_ancestor_type);
      return EXIT_FAILURE;
    }
    if (show_ancestor_depth == HWLOC_TYPE_DEPTH_MULTIPLE) {
      fprintf(stderr, "multiple --ancestor type %s\n", show_ancestor_type);
      return EXIT_FAILURE;
    }
  }
  if (show_descendants_type) {
    err = hwloc_type_sscanf_as_depth(show_descendants_type, NULL, topology, &show_descendants_depth);
    if (err < 0) {
      fprintf(stderr, "unrecognized --descendants type %s\n", show_descendants_type);
      usage(callname, stderr);
      return EXIT_FAILURE;
    }
    if (show_descendants_depth == HWLOC_TYPE_DEPTH_UNKNOWN) {
      fprintf(stderr, "unavailable --descendants type %s\n", show_descendants_type);
      return EXIT_FAILURE;
    }
    if (show_descendants_depth == HWLOC_TYPE_DEPTH_MULTIPLE) {
      fprintf(stderr, "multiple --descendants type %s\n", show_descendants_type);
      return EXIT_FAILURE;
    }
  }

  if (restrictstring) {
    hwloc_bitmap_t restrictset = hwloc_bitmap_alloc();
    if (!strcmp (restrictstring, "binding")) {
      if (pid_number > 0)
	hwloc_get_proc_cpubind(topology, pid, restrictset, HWLOC_CPUBIND_PROCESS);
      else
	hwloc_get_cpubind(topology, restrictset, HWLOC_CPUBIND_PROCESS);
    } else {
      hwloc_bitmap_sscanf(restrictset, restrictstring);
    }
    err = hwloc_topology_restrict (topology, restrictset, 0);
    if (err) {
      perror("Restricting the topology");
      /* FALLTHRU */
    }
    hwloc_bitmap_free(restrictset);
    free(restrictstring);
  }

  if (mode == HWLOC_INFO_MODE_UNKNOWN) {
    if (argc)
      mode = HWLOC_INFO_MODE_OBJECTS;
    else
      mode = HWLOC_INFO_MODE_TOPOLOGY;
  }

  if (mode == HWLOC_INFO_MODE_TOPOLOGY) {
    hwloc_lstopo_show_summary(stdout, topology);

  } else if (mode == HWLOC_INFO_MODE_SUPPORT) {
    const struct hwloc_topology_support *support = hwloc_topology_get_support(topology);
#define DO(x,y) printf(#x ":" #y " = %u\n", (unsigned char) support->x->y);
    DO(discovery, pu);
    DO(discovery, numa);
    DO(discovery, numa_memory);

    DO(cpubind, set_thisproc_cpubind);
    DO(cpubind, get_thisproc_cpubind);
    DO(cpubind, set_proc_cpubind);
    DO(cpubind, get_proc_cpubind);
    DO(cpubind, set_thisthread_cpubind);
    DO(cpubind, get_thisthread_cpubind);
    DO(cpubind, set_thread_cpubind);
    DO(cpubind, get_thread_cpubind);
    DO(cpubind, get_thisproc_last_cpu_location);
    DO(cpubind, get_proc_last_cpu_location);
    DO(cpubind, get_thisthread_last_cpu_location);

    DO(membind, set_thisproc_membind);
    DO(membind, get_thisproc_membind);
    DO(membind, set_proc_membind);
    DO(membind, get_proc_membind);
    DO(membind, set_thisthread_membind);
    DO(membind, get_thisthread_membind);
    DO(membind, set_area_membind);
    DO(membind, get_area_membind);
    DO(membind, alloc_membind);
    DO(membind, firsttouch_membind);
    DO(membind, bind_membind);
    DO(membind, interleave_membind);
    DO(membind, nexttouch_membind);
    DO(membind, migrate_membind);
    DO(membind, get_area_memlocation);

  } else if (mode == HWLOC_INFO_MODE_OBJECTS) {
    struct hwloc_calc_location_context_s lcontext;
    lcontext.topology = topology;
    lcontext.topodepth = topodepth;
    lcontext.only_hbm = -1;
    lcontext.logical = logical;
    lcontext.verbose = verbose_mode;
    current_obj = 0;
    while (argc >= 1) {
      if (!strcmp(argv[0], "all") || !strcmp(argv[0], "root")) {
	hwloc_calc_process_location_info_cb(&lcontext, NULL, hwloc_get_root_obj(topology));
      } else {
	/* try to match a type/depth followed by a special character */
	typelen = strspn(argv[0], "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");
	if (typelen && (argv[0][typelen] == ':' || argv[0][typelen] == '=' || argv[0][typelen] == '[')) {
	  err = hwloc_calc_process_location(&lcontext, argv[0], typelen,
					    hwloc_calc_process_location_info_cb, NULL);
	}
      }
      argc--; argv++;
    }

  } else assert(0);

  hwloc_topology_destroy (topology);

  return EXIT_SUCCESS;
}
