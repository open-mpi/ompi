/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
 * Copyright © 2009-2010 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>

#include "misc.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void usage(const char *callname __hwloc_attribute_unused, FILE *where)
{
  fprintf(where, "Usage: hwloc-distrib [options] number\n");
  fprintf(where, "Distribution options:\n");
  fprintf(where, "  --ignore <type>  Ignore objects of the given type\n");
  fprintf(where, "  --from <type>    Distribute starting from objects of the given type\n");
  fprintf(where, "  --to <type>      Distribute down to objects of the given type\n");
  fprintf(where, "  --at <type>      Distribute among objects of the given type\n");
  fprintf(where, "  --reverse        Distribute by starting from last objects\n");
  fprintf(where, "Input topology options:\n");
  fprintf(where, "  --restrict <set> Restrict the topology to processors listed in <set>\n");
  fprintf(where, "  --whole-system   Do not consider administration limitations\n");
  hwloc_utils_input_format_usage(where, 0);
  fprintf(where, "Formatting options:\n");
  fprintf(where, "  --single         Singlify each output to a single CPU\n");
  fprintf(where, "  --taskset        Show taskset-specific cpuset strings\n");
  fprintf(where, "Miscellaneous options:\n");
  fprintf(where, "  -v --verbose     Show verbose messages\n");
  fprintf(where, "  --version        Report version and exit\n");
}

int main(int argc, char *argv[])
{
  long n = -1;
  char *callname;
  char *input = NULL;
  enum hwloc_utils_input_format input_format = HWLOC_UTILS_INPUT_DEFAULT;
  int taskset = 0;
  int singlify = 0;
  int verbose = 0;
  char *restrictstring = NULL;
  const char *from_type = NULL, *to_type = NULL;
  hwloc_topology_t topology;
  unsigned long flags = 0;
  unsigned long dflags = 0;
  int opt;
  int err;

  callname = argv[0];
  /* skip argv[0], handle options */
  argv++;
  argc--;

  hwloc_utils_check_api_version(callname);

  /* enable verbose backends */
  putenv((char *) "HWLOC_XML_VERBOSE=1");
  putenv((char *) "HWLOC_SYNTHETIC_VERBOSE=1");

  hwloc_topology_init(&topology);

  while (argc >= 1) {
    if (!strcmp(argv[0], "--")) {
      argc--;
      argv++;
      break;
    }

    if (*argv[0] == '-') {
      if (!strcmp(argv[0], "--single")) {
	singlify = 1;
	goto next;
      }
      if (!strcmp(argv[0], "--taskset")) {
	taskset = 1;
	goto next;
      }
      if (!strcmp(argv[0], "-v") || !strcmp(argv[0], "--verbose")) {
	verbose = 1;
	goto next;
      }
      if (!strcmp (argv[0], "--whole-system")) {
	flags |= HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM;
	goto next;
      }
      if (!strcmp(argv[0], "--help")) {
	usage(callname, stdout);
	return EXIT_SUCCESS;
      }
      if (hwloc_utils_lookup_input_option(argv, argc, &opt,
					  &input, &input_format,
					  callname)) {
	argv += opt;
	argc -= opt;
	goto next;
      }
      else if (!strcmp (argv[0], "--ignore")) {
	hwloc_obj_type_t type;
	if (argc < 2) {
	  usage(callname, stdout);
	  exit(EXIT_FAILURE);
	}
	if (hwloc_type_sscanf(argv[1], &type, NULL, 0) < 0)
	  fprintf(stderr, "Unsupported type `%s' passed to --ignore, ignoring.\n", argv[1]);
	else
	  hwloc_topology_set_type_filter(topology, type, HWLOC_TYPE_FILTER_KEEP_NONE);
	argc--;
	argv++;
	goto next;
      }
      else if (!strcmp (argv[0], "--from")) {
	if (argc < 2) {
	  usage(callname, stdout);
	  exit(EXIT_FAILURE);
	}
	from_type = argv[1];
	argc--;
	argv++;
	goto next;
      }
      else if (!strcmp (argv[0], "--to")) {
	if (argc < 2) {
	  usage(callname, stdout);
	  exit(EXIT_FAILURE);
	}
	to_type = argv[1];
	argc--;
	argv++;
	goto next;
      }
      else if (!strcmp (argv[0], "--at")) {
	if (argc < 2) {
	  usage(callname, stdout);
	  exit(EXIT_FAILURE);
	}
	from_type = to_type = argv[1];
	argc--;
	argv++;
	goto next;
      }
      else if (!strcmp (argv[0], "--reverse")) {
	dflags |= HWLOC_DISTRIB_FLAG_REVERSE;
	goto next;
      }
      else if (!strcmp (argv[0], "--restrict")) {
	if (argc < 2) {
	  usage (callname, stdout);
	  exit(EXIT_FAILURE);
	}
	restrictstring = strdup(argv[1]);
	argc--;
	argv++;
	goto next;
      }
      else if (!strcmp (argv[0], "--version")) {
          printf("%s %s\n", callname, HWLOC_VERSION);
          exit(EXIT_SUCCESS);
      }

      fprintf (stderr, "Unrecognized option: %s\n", argv[0]);
      usage(callname, stderr);
      return EXIT_FAILURE;
    }

    if (n != -1) {
      fprintf(stderr,"duplicate number\n");
      usage(callname, stderr);
      return EXIT_FAILURE;
    }
    n = atol(argv[0]);

  next:
    argc--;
    argv++;
  }

  if (n == -1) {
    fprintf(stderr,"need a number\n");
    usage(callname, stderr);
    return EXIT_FAILURE;
  }

  if (verbose)
    fprintf(stderr, "distributing %ld\n", n);

  {
    unsigned i;
    int from_depth, to_depth;
    unsigned chunks;
    hwloc_bitmap_t *cpuset;

    cpuset = malloc(n * sizeof(hwloc_bitmap_t));

    if (input) {
      err = hwloc_utils_enable_input_format(topology, input, &input_format, verbose, callname);
      if (err) {
	free(cpuset);
	return EXIT_FAILURE;
      }
    }
    hwloc_topology_set_flags(topology, flags);
    hwloc_topology_load(topology);

    if (restrictstring) {
      hwloc_bitmap_t restrictset = hwloc_bitmap_alloc();
      hwloc_bitmap_sscanf(restrictset, restrictstring);
      err = hwloc_topology_restrict (topology, restrictset, 0);
      if (err) {
	perror("Restricting the topology");
	/* FALLTHRU */
      }
      hwloc_bitmap_free(restrictset);
      free(restrictstring);
    }

    from_depth = 0;
    if (from_type) {
      if (hwloc_type_sscanf_as_depth(from_type, NULL, topology, &from_depth) < 0 || from_depth < 0) {
	fprintf(stderr, "Unsupported or unavailable type `%s' passed to --from, ignoring.\n", from_type);
	return EXIT_FAILURE;
      }
    }

    to_depth = INT_MAX;
    if (to_type) {
      if (hwloc_type_sscanf_as_depth(to_type, NULL, topology, &to_depth) < 0 || to_depth < 0) {
	fprintf(stderr, "Unsupported or unavailable type `%s' passed to --to, ignoring.\n", to_type);
	return EXIT_FAILURE;
      }
    }

    chunks =  hwloc_get_nbobjs_by_depth(topology, from_depth);
    {
      hwloc_obj_t *roots;

      roots = malloc(chunks * sizeof(hwloc_obj_t));

      for (i = 0; i < chunks; i++)
        roots[i] = hwloc_get_obj_by_depth(topology, from_depth, i);

      hwloc_distrib(topology, roots, chunks, cpuset, n, to_depth, dflags);

      for (i = 0; (long) i < n; i++) {
	char *str = NULL;
	if (singlify) {
	  if (dflags & HWLOC_DISTRIB_FLAG_REVERSE) {
	    unsigned last = hwloc_bitmap_last(cpuset[i]);
	    hwloc_bitmap_only(cpuset[i], last);
	  } else {
	    hwloc_bitmap_singlify(cpuset[i]);
	  }
	}
	if (taskset)
	  hwloc_bitmap_taskset_asprintf(&str, cpuset[i]);
	else
	  hwloc_bitmap_asprintf(&str, cpuset[i]);
	printf("%s\n", str);
	free(str);
	hwloc_bitmap_free(cpuset[i]);
      }

      free(roots);
    }

   free(cpuset);
  }

  hwloc_topology_destroy(topology);

  return EXIT_SUCCESS;
}
