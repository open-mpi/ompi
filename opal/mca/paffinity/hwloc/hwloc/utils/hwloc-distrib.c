/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/private.h>
#include <hwloc.h>

#include <unistd.h>

static void usage(FILE *where)
{
  fprintf(where, "Usage: hwloc-distrib [options] number\n");
  fprintf(where, "Options:\n");
  fprintf(where, "   --single\tsinglify each output to a single CPU\n");
  fprintf(where, "   -v\t\t\tverbose messages\n");
  fprintf(where, "   --synthetic \"2 2\"\tsimulate a fake hierarchy\n");
#ifdef HWLOC_HAVE_XML
  fprintf(where, "   --xml <path>\t\tread topology from XML file <path>\n");
#endif
  fprintf(where, "   --version\t\treport version and exit\n");
}

int main(int argc, char *argv[])
{
  long n = -1;
  char * synthetic = NULL;
  const char * xmlpath = NULL;
  int singlify = 0;
  int verbose = 0;
  char **orig_argv = argv;

  /* skip argv[0], handle options */
  argv++;
  argc--;

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
      if (!strcmp(argv[0], "-v")) {
	verbose = 1;
	goto next;
      }
      if (!strcmp(argv[0], "--help")) {
	usage(stdout);
	return EXIT_SUCCESS;
      }
      if (!strcmp (argv[0], "--synthetic")) {
	if (argc <= 2) {
	  usage(stdout);
	  exit(EXIT_FAILURE);
	}
	synthetic = argv[1];
	argv++;
	argc--;
	goto next;
      }
      else if (!strcmp (argv[0], "--version")) {
          printf("%s %s\n", orig_argv[0], VERSION);
          exit(EXIT_SUCCESS);
      }
#ifdef HWLOC_HAVE_XML
      if (!strcmp (argv[0], "--xml")) {
	if (argc <= 2) {
	  usage(stdout);
	  exit(EXIT_FAILURE);
	}
	xmlpath = argv[1];
	argc--;
	argv++;
	if (!strcmp(xmlpath, "-"))
	  xmlpath = "/dev/stdin";
	goto next;
      }
#endif /* HWLOC_HAVE_XML */

      usage(stderr);
      return EXIT_FAILURE;
    }

    if (n != -1) {
      fprintf(stderr,"duplicate number\n");
      usage(stderr);
      return EXIT_FAILURE;
    }
    n = atol(argv[0]);

  next:
    argc--;
    argv++;
  }

  if (n == -1) {
    fprintf(stderr,"need a number\n");
    usage(stderr);
    return EXIT_FAILURE;
  }

  if (verbose)
    fprintf(stderr, "distributing %ld\n", n);

  {
    long i;
    hwloc_cpuset_t cpuset[n];
    hwloc_topology_t topology;

    hwloc_topology_init(&topology);
    if (synthetic)
      hwloc_topology_set_synthetic(topology, synthetic);
    if (xmlpath)
      hwloc_topology_set_xml(topology, xmlpath);
    hwloc_topology_load(topology);

    hwloc_distribute(topology, hwloc_get_root_obj(topology), cpuset, n);
    for (i = 0; i < n; i++) {
      char *str = NULL;
      if (singlify)
	hwloc_cpuset_singlify(cpuset[i]);
      hwloc_cpuset_asprintf(&str, cpuset[i]);
      printf("%s\n", str);
      free(str);
      hwloc_cpuset_free(cpuset[i]);
    }
    hwloc_topology_destroy(topology);
  }

  return EXIT_SUCCESS;
}
