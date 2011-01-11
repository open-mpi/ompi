/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2010 INRIA
 * Copyright © 2009-2010 Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <assert.h>

extern void usage(const char *name, FILE *where);

static inline void
hwloc_utils_input_format_usage(FILE *where, int addspaces)
{
#ifdef HWLOC_HAVE_XML
  fprintf (where, "  --input <XML file>\n");
  fprintf (where, "  -i <XML file>   %*sRead topology from XML file <path>\n",
	   addspaces, " ");
#endif
#ifdef HWLOC_LINUX_SYS
  fprintf (where, "  --input <directory>\n");
  fprintf (where, "  -i <directory>  %*sRead topology from chroot containing the /proc and /sys\n",
	   addspaces, " ");
  fprintf (where, "                  %*sof another system\n",
	   addspaces, " ");
#endif
  fprintf (where, "  --input \"n:2 2\"\n");
  fprintf (where, "  -i \"n:2 2\"      %*sSimulate a fake hierarchy, here with 2 NUMA nodes of 2\n",
	   addspaces, " ");
  fprintf (where, "                  %*sprocessors\n",
	   addspaces, " ");
  fprintf (where, "  --input-format <format>\n");
  fprintf (where, "  --if <format>   %*sEnforce input format among "
#ifdef HWLOC_HAVE_XML
	   "xml, "
#endif
#ifdef HWLOC_LINUX_SYS
	   "fsroot, "
#endif
	   "synthetic\n",
	   addspaces, " ");
}

enum hwloc_utils_input_format {
  HWLOC_UTILS_INPUT_DEFAULT,
  HWLOC_UTILS_INPUT_XML,
  HWLOC_UTILS_INPUT_FSROOT,
  HWLOC_UTILS_INPUT_SYNTHETIC
};

static inline enum hwloc_utils_input_format
hwloc_utils_parse_input_format(const char *name, const char *callname)
{
  if (!strncasecmp(name, "default", 3))
    return HWLOC_UTILS_INPUT_DEFAULT;
  else if (!strncasecmp(name, "xml", 1))
    return HWLOC_UTILS_INPUT_XML;
  else if (!strncasecmp(name, "fsroot", 1))
    return HWLOC_UTILS_INPUT_FSROOT;
  else if (!strncasecmp(name, "synthetic", 1))
    return HWLOC_UTILS_INPUT_SYNTHETIC;

  fprintf(stderr, "input format `%s' not supported\n", name);
  usage(callname, stderr);
  exit(EXIT_FAILURE);
}

static inline int
hwloc_utils_lookup_input_option(char *argv[], int argc, int *consumed_opts,
				char **inputp, enum hwloc_utils_input_format *input_formatp,
				const char *callname)
{
  if (!strcmp (argv[0], "--input")
	       || !strcmp (argv[0], "-i")) {
    if (argc <= 1) {
      usage (callname, stderr);
      exit(EXIT_FAILURE);
    }
    if (strlen(argv[1]))
      *inputp = argv[1];
    else
      *inputp = NULL;
    *consumed_opts = 1;
    return 1;
  }
  else if (!strcmp (argv[0], "--input-format")
	   || !strcmp (argv[0], "--if")) {
    if (argc <= 1) {
      usage (callname, stderr);
      exit(EXIT_FAILURE);
    }
    *input_formatp = hwloc_utils_parse_input_format (argv[1], callname);
    *consumed_opts = 1;
    return 1;
  }

  /* backward compat with 1.0 */
  else if (!strcmp (argv[0], "--synthetic")) {
    if (argc <= 1) {
      usage (callname, stderr);
      exit(EXIT_FAILURE);
    }
    *inputp = argv[1];
    *input_formatp = HWLOC_UTILS_INPUT_SYNTHETIC;
    *consumed_opts = 1;
    return 1;
  } else if (!strcmp (argv[0], "--xml")) {
    if (argc <= 1) {
      usage (callname, stderr);
      exit(EXIT_FAILURE);
    }
    *inputp = argv[1];
    *input_formatp = HWLOC_UTILS_INPUT_XML;
    *consumed_opts = 1;
    return 1;
  } else if (!strcmp (argv[0], "--fsys-root")) {
    if (argc <= 1) {
      usage (callname, stderr);
      exit(EXIT_FAILURE);
    }
    *inputp = argv[1];
    *input_formatp = HWLOC_UTILS_INPUT_FSROOT;
    *consumed_opts = 1;
    return 1;
  }

  return 0;
}

static inline int
hwloc_utils_enable_input_format(struct hwloc_topology *topology,
				const char *input,
				enum hwloc_utils_input_format input_format,
				int verbose, const char *callname)
{
  if (input_format == HWLOC_UTILS_INPUT_DEFAULT) {
    struct stat inputst;
    int err;
    err = stat(input, &inputst);
    if (err < 0) {
      if (verbose)
	printf("assuming `%s' is a synthetic topology description\n", input);
      input_format = HWLOC_UTILS_INPUT_SYNTHETIC;
    } else if (S_ISDIR(inputst.st_mode)) {
      if (verbose)
	printf("assuming `%s' is a file-system root\n", input);
      input_format = HWLOC_UTILS_INPUT_FSROOT;
    } else if (S_ISREG(inputst.st_mode)) {
      if (verbose)
	printf("assuming `%s' is a XML file\n", input);
      input_format = HWLOC_UTILS_INPUT_XML;
    } else {
      fprintf (stderr, "Unrecognized input file: %s\n", input);
      usage (callname, stderr);
    }
  }

  switch (input_format) {
  case HWLOC_UTILS_INPUT_XML:
#ifdef HWLOC_HAVE_XML
    if (!strcmp(input, "-"))
      input = "/dev/stdin";
    if (hwloc_topology_set_xml(topology, input)) {
      perror("Setting target XML file");
      return EXIT_FAILURE;
    }
#else /* HWLOC_HAVE_XML */
    fprintf(stderr, "This installation of hwloc does not support XML, sorry.\n");
    exit(EXIT_FAILURE);
#endif /* HWLOC_HAVE_XML */
    break;

  case HWLOC_UTILS_INPUT_FSROOT:
#ifdef HWLOC_LINUX_SYS
    if (hwloc_topology_set_fsroot(topology, input)) {
      perror("Setting target filesystem root");
      return EXIT_FAILURE;
    }
#else /* HWLOC_LINUX_SYS */
    fprintf(stderr, "This installation of hwloc does not support changing the file-system root, sorry.\n");
    exit(EXIT_FAILURE);
#endif /* HWLOC_LINUX_SYS */
    break;

  case HWLOC_UTILS_INPUT_SYNTHETIC:
    if (hwloc_topology_set_synthetic(topology, input))
      return EXIT_FAILURE;
    break;

  case HWLOC_UTILS_INPUT_DEFAULT:
    assert(0);
  }

  return 0;
}

