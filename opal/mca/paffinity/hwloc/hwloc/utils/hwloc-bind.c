/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/private.h>
#include <hwloc-calc.h>
#include <hwloc.h>

#include <unistd.h>
#include <errno.h>

static void usage(FILE *where)
{
  fprintf(where, "Usage: hwloc-bind [options] <location> -- command ...\n");
  fprintf(where, " <location> may be a space-separated list of cpusets or objects\n");
  fprintf(where, "            as supported by the hwloc-calc utility.\n");
  fprintf(where, "Options:\n");
  fprintf(where, "  -l --logical\ttake logical object indexes (default)\n");
  fprintf(where, "  -p --physical\ttake physical object indexes\n");
  fprintf(where, "  --single\tbind on a single CPU to prevent migration\n");
  fprintf(where, "  --strict\trequire strict binding\n");
  fprintf(where, "  --get\t\tretrieve current process binding\n");
  fprintf(where, "  --pid <pid>\toperate on process <pid>\n");
  fprintf(where, "  -v\t\tverbose messages\n");
  fprintf(where, "  --version\treport version and exit\n");
}

int main(int argc, char *argv[])
{
  hwloc_topology_t topology;
  unsigned depth;
  hwloc_cpuset_t cpu_set; /* invalid until bind_cpus is set */
  int get_binding = 0;
  int bind_cpus = 0;
  int single = 0;
  int verbose = 0;
  int logical = 1;
  int flags = 0;
  int opt;
  int ret;
  hwloc_pid_t pid = 0;
  char **orig_argv = argv;

  cpu_set = hwloc_cpuset_alloc();

  hwloc_topology_init(&topology);
  hwloc_topology_load(topology);
  depth = hwloc_topology_get_depth(topology);

  /* skip argv[0], handle options */
  argv++;
  argc--;

  while (argc >= 1) {
    if (!strcmp(argv[0], "--")) {
      argc--;
      argv++;
      break;
    }

    opt = 0;

    if (*argv[0] == '-') {
      if (!strcmp(argv[0], "-v")) {
	verbose = 1;
	goto next;
      }
      else if (!strcmp(argv[0], "--help")) {
	usage(stdout);
	return EXIT_SUCCESS;
      }
      else if (!strcmp(argv[0], "--single")) {
	single = 1;
	goto next;
      }
      else if (!strcmp(argv[0], "--strict")) {
	flags |= HWLOC_CPUBIND_STRICT;
	goto next;
      }
      else if (!strcmp(argv[0], "--pid")) {
        if (argc < 2) {
          usage (stderr);
          exit(EXIT_FAILURE);
        }
        pid = atoi(argv[1]);
        opt = 1;
        goto next;
      }
      else if (!strcmp (argv[0], "--version")) {
          printf("%s %s\n", orig_argv[0], VERSION);
          exit(EXIT_SUCCESS);
      }
      if (!strcmp(argv[0], "-l") || !strcmp(argv[0], "--logical")) {
        logical = 1;
        goto next;
      }
      if (!strcmp(argv[0], "-p") || !strcmp(argv[0], "--physical")) {
        logical = 0;
        goto next;
      }
      else if (!strcmp (argv[0], "--get")) {
	  get_binding = 1;
	  goto next;
      }

      usage(stderr);
      return EXIT_FAILURE;
    }

    ret = hwloc_mask_process_arg(topology, depth, argv[0], logical, cpu_set, verbose);
    if (ret < 0) {
      if (verbose)
	fprintf(stderr, "assuming the command starts at %s\n", argv[0]);
      break;
    }

    /* we found at least one binding argument */
    bind_cpus = 1;

  next:
    argc -= opt+1;
    argv += opt+1;
  }

  if (get_binding) {
    char *s;
    int err;
    if (pid)
      err = hwloc_get_proc_cpubind(topology, pid, cpu_set, 0);
    else
      err = hwloc_get_cpubind(topology, cpu_set, 0);
    if (err) {
      const char *errmsg = strerror(errno);
      fprintf(stderr, "hwloc_get_cpubind failed (errno %d %s)\n", errno, errmsg);
      return EXIT_FAILURE;
    }
    s = hwloc_cpuset_printf_value(cpu_set);
    printf("%s\n", s);
    free(s);
    return EXIT_SUCCESS;
  }

  if (bind_cpus) {
    if (verbose) {
      char *s = hwloc_cpuset_printf_value(cpu_set);
      fprintf(stderr, "binding on cpu set %s\n", s);
      free(s);
    }
    if (single)
      hwloc_cpuset_singlify(cpu_set);
    if (pid)
      ret = hwloc_set_proc_cpubind(topology, pid, cpu_set, flags);
    else
      ret = hwloc_set_cpubind(topology, cpu_set, flags);
    if (ret) {
      int bind_errno = errno;
      const char *errmsg = strerror(bind_errno);
      char *s = hwloc_cpuset_printf_value(cpu_set);
      fprintf(stderr, "hwloc_set_cpubind %s failed (errno %d %s)\n", s, bind_errno, errmsg);
      free(s);
    }
  }

  hwloc_cpuset_free(cpu_set);

  hwloc_topology_destroy(topology);

  if (pid)
    return EXIT_SUCCESS;

  if (0 == argc) {
    fprintf(stderr, "%s: nothing to do!\n", orig_argv[0]);
    return EXIT_FAILURE;
  }

  ret = execvp(argv[0], argv);
  if (ret) {
      fprintf(stderr, "%s: Failed to launch executable \"%s\"\n", 
              orig_argv[0], argv[0]);
      perror("execvp");
  }
  return EXIT_FAILURE;
}
