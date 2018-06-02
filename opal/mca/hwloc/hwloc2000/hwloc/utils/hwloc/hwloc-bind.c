/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
 * Copyright © 2009-2010, 2012 Université Bordeaux
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc-calc.h>
#include <hwloc.h>

#ifdef HWLOC_LINUX_SYS
#include <hwloc/linux.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>

#include "misc.h"

#if defined(HWLOC_WIN_SYS) && !defined(__CYGWIN__)
#include <process.h>
#define execvp(a,b) (int)_execvp((a), (const char * const *)(b))
#endif

void usage(const char *name, FILE *where)
{
  fprintf(where, "Usage: %s [options] <location> -- command ...\n", name);
  fprintf(where, " <location> may be a space-separated list of cpusets or objects\n");
  fprintf(where, "            as supported by the hwloc-calc utility, e.g:\n");
  hwloc_calc_locations_usage(where);
  fprintf(where, "Options:\n");
  fprintf(where, "  --cpubind      Use following arguments for cpu binding (default)\n");
  fprintf(where, "  --membind      Use following arguments for memory binding\n");
  fprintf(where, "  --mempolicy <default|firsttouch|bind|interleave|nexttouch>\n"
		 "                 Change policy that --membind applies (default is bind)\n");
  fprintf(where, "  -l --logical   Take logical object indexes (default)\n");
  fprintf(where, "  -p --physical  Take physical object indexes\n");
  fprintf(where, "  --single       Bind on a single CPU to prevent migration\n");
  fprintf(where, "  --strict       Require strict binding\n");
  fprintf(where, "  --get          Retrieve current process binding\n");
  fprintf(where, "  -e --get-last-cpu-location\n"
		 "                 Retrieve the last processors where the current process ran\n");
  fprintf(where, "  --nodeset      Display (and parse) cpusets as nodesets\n");
  fprintf(where, "  --pid <pid>    Operate on process <pid>\n");
#ifdef HWLOC_LINUX_SYS
  fprintf(where, "  --tid <tid>    Operate on thread <tid>\n");
#endif
  fprintf(where, "  --taskset      Use taskset-specific format when displaying cpuset strings\n");
  fprintf(where, "Input topology options:\n");
  fprintf(where, "  --restrict <set> Restrict the topology to processors listed in <set>\n");
  fprintf(where, "  --whole-system   Do not consider administration limitations\n");
  fprintf(where, "  --hbm          Only consider high bandwidth memory nodes\n");
  fprintf(where, "  --no-hbm       Ignore high-bandwidth memory nodes\n");
  fprintf(where, "Miscellaneous options:\n");
  fprintf(where, "  -f --force     Launch the command even if binding failed\n");
  fprintf(where, "  -q --quiet     Hide non-fatal error messages\n");
  fprintf(where, "  -v --verbose   Show verbose messages\n");
  fprintf(where, "  --version      Report version and exit\n");
}

int main(int argc, char *argv[])
{
  hwloc_topology_t topology;
  int loaded = 0;
  int depth;
  hwloc_bitmap_t cpubind_set, membind_set;
  int got_cpubind = 0, got_membind = 0;
  int working_on_cpubind = 1; /* membind if 0 */
  int get_binding = 0;
  int use_nodeset = 0;
  int get_last_cpu_location = 0;
  unsigned long flags = 0;
  int force = 0;
  int single = 0;
  int verbose = 0;
  int only_hbm = -1;
  int logical = 1;
  int taskset = 0;
  unsigned cpubind_flags = 0;
  hwloc_membind_policy_t membind_policy = HWLOC_MEMBIND_BIND;
  int got_mempolicy = 0;
  unsigned membind_flags = 0;
  int opt;
  int ret;
  int pid_number = -1;
  int tid_number = -1;
  hwloc_pid_t pid = 0; /* only valid when pid_number > 0, but gcc-4.8 still reports uninitialized warnings */
  char *callname;
  struct hwloc_calc_location_context_s lcontext;
  struct hwloc_calc_set_context_s scontext;

  callname = argv[0];
  /* skip argv[0], handle options */
  argv++;
  argc--;

  hwloc_utils_check_api_version(callname);

  cpubind_set = hwloc_bitmap_alloc();
  membind_set = hwloc_bitmap_alloc();

  /* don't load now, in case some options change the config before the topology is actually used */
#define LOADED() (loaded)
#define ENSURE_LOADED() do { \
  if (!loaded) { \
    hwloc_topology_init(&topology); \
    hwloc_topology_set_all_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_ALL); \
    hwloc_topology_set_flags(topology, flags); \
    hwloc_topology_load(topology); \
    depth = hwloc_topology_get_depth(topology); \
    loaded = 1; \
  } \
} while (0)

  while (argc >= 1) {
    if (!strcmp(argv[0], "--")) {
      argc--;
      argv++;
      break;
    }

    opt = 0;

    if (*argv[0] == '-') {
      if (!strcmp(argv[0], "-v") || !strcmp(argv[0], "--verbose")) {
	verbose++;
	goto next;
      }
      if (!strcmp(argv[0], "-q") || !strcmp(argv[0], "--quiet")) {
	verbose--;
	goto next;
      }
      if (!strcmp(argv[0], "--help")) {
        usage("hwloc-bind", stdout);
	return EXIT_SUCCESS;
      }
      if (!strcmp(argv[0], "--single")) {
	single = 1;
	goto next;
      }
      if (!strcmp(argv[0], "-f") || !strcmp(argv[0], "--force")) {
	force = 1;
	goto next;
      }
      if (!strcmp(argv[0], "--strict")) {
	cpubind_flags |= HWLOC_CPUBIND_STRICT;
	membind_flags |= HWLOC_MEMBIND_STRICT;
	goto next;
      }
      if (!strcmp(argv[0], "--pid")) {
        if (argc < 2) {
          usage ("hwloc-bind", stderr);
          exit(EXIT_FAILURE);
        }
        pid_number = atoi(argv[1]);
        opt = 1;
        goto next;
      }
#ifdef HWLOC_LINUX_SYS
      if (!strcmp(argv[0], "--tid")) {
        if (argc < 2) {
          usage ("hwloc-bind", stderr);
          exit(EXIT_FAILURE);
        }
        tid_number = atoi(argv[1]);
        opt = 1;
        goto next;
      }
#endif
      if (!strcmp (argv[0], "--version")) {
	printf("%s %s\n", callname, HWLOC_VERSION);
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
      if (!strcmp(argv[0], "--taskset")) {
        taskset = 1;
        goto next;
      }
      if (!strcmp (argv[0], "-e") || !strncmp (argv[0], "--get-last-cpu-location", 10)) {
	get_last_cpu_location = 1;
	goto next;
      }
      if (!strcmp (argv[0], "--get")) {
	get_binding = 1;
	goto next;
      }
      if (!strcmp (argv[0], "--nodeset")) {
	use_nodeset = 1;
	goto next;
      }
      if (!strcmp (argv[0], "--cpubind")) {
	working_on_cpubind = 1;
	goto next;
      }
      if (!strcmp (argv[0], "--membind")) {
	working_on_cpubind = 0;
	goto next;
      }
      if (!strcmp (argv[0], "--mempolicy")) {
	if (!strncmp(argv[1], "default", 2))
	  membind_policy = HWLOC_MEMBIND_DEFAULT;
	else if (!strncmp(argv[1], "firsttouch", 2))
	  membind_policy = HWLOC_MEMBIND_FIRSTTOUCH;
	else if (!strncmp(argv[1], "bind", 2))
	  membind_policy = HWLOC_MEMBIND_BIND;
	else if (!strncmp(argv[1], "interleave", 2))
	  membind_policy = HWLOC_MEMBIND_INTERLEAVE;
	else if (!strncmp(argv[1], "nexttouch", 2))
	  membind_policy = HWLOC_MEMBIND_NEXTTOUCH;
	else {
	  fprintf(stderr, "Unrecognized memory binding policy %s\n", argv[1]);
          usage ("hwloc-bind", stderr);
          exit(EXIT_FAILURE);
	}
	got_mempolicy = 1;
	opt = 1;
	goto next;
      }
      if (!strcmp(argv[0], "--hbm")) {
	only_hbm = 1;
	goto next;
      }
      if (!strcmp(argv[0], "--no-hbm")) {
	only_hbm = 0;
	goto next;
      }
      if (!strcmp (argv[0], "--whole-system")) {
	if (loaded) {
	  fprintf(stderr, "Input option %s disallowed after options using the topology\n", argv[0]);
	  exit(EXIT_FAILURE);
	}
	flags |= HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM;
	goto next;
      }
      if (!strcmp (argv[0], "--restrict")) {
	hwloc_bitmap_t restrictset;
	int err;
	if (argc < 2) {
	  usage (callname, stdout);
	  exit(EXIT_FAILURE);
	}
	restrictset = hwloc_bitmap_alloc();
	hwloc_bitmap_sscanf(restrictset, argv[1]);
	ENSURE_LOADED();
	err = hwloc_topology_restrict (topology, restrictset, 0);
	if (err) {
	  perror("Restricting the topology");
	  /* FALLTHRU */
	}
	hwloc_bitmap_free(restrictset);
	argc--;
	argv++;
	goto next;
      }

      fprintf (stderr, "Unrecognized option: %s\n", argv[0]);
      usage("hwloc-bind", stderr);
      return EXIT_FAILURE;
    }

    ENSURE_LOADED();

    lcontext.topology = topology;
    lcontext.topodepth = depth;
    lcontext.only_hbm = only_hbm;
    lcontext.logical = logical;
    lcontext.verbose = verbose;
    scontext.nodeset_input = use_nodeset;
    scontext.nodeset_output = working_on_cpubind ? 0 : 1;
    scontext.output_set = working_on_cpubind ? cpubind_set : membind_set;
    ret = hwloc_calc_process_location_as_set(&lcontext, &scontext, argv[0]);
    if (ret < 0) {
      if (verbose > 0)
	fprintf(stderr, "assuming the command starts at %s\n", argv[0]);
      break;
    }
    if (working_on_cpubind)
      got_cpubind = 1;
    else
      got_membind = 1;

  next:
    argc -= opt+1;
    argv += opt+1;
  }

  ENSURE_LOADED();

  if (pid_number > 0 && tid_number > 0) {
    fprintf(stderr, "cannot operate both on tid and pid\n");
    return EXIT_FAILURE;
  }

  if (pid_number > 0) {
    pid = hwloc_pid_from_number(pid_number, !(get_binding || get_last_cpu_location));
    /* no need to set_pid()
     * the doc just says we're operating on pid, not that we're retrieving the topo/cpuset as seen from inside pid
     */
  }

  if (get_last_cpu_location && !working_on_cpubind) {
    fprintf(stderr, "Options --membind and --get-last-cpu-location cannot be combined.\n");
    return EXIT_FAILURE;
  }
  if ((get_binding || get_last_cpu_location) && (got_cpubind || got_membind)) {
    /* doesn't work because get_binding/get_last_cpu_location overwrites cpubind_set */
    fprintf(stderr, "Cannot display and set binding at the same time.\n");
    return EXIT_FAILURE;
  }

  if (get_binding || get_last_cpu_location) {
    char *s;
    const char *policystr = NULL;
    int err;
    if (working_on_cpubind) {
      if (get_last_cpu_location) {
	if (pid_number > 0)
	  err = hwloc_get_proc_last_cpu_location(topology, pid, cpubind_set, 0);
#ifdef HWLOC_LINUX_SYS
	else if (tid_number > 0)
	  err = hwloc_linux_get_tid_last_cpu_location(topology, tid_number, cpubind_set);
#endif
	else
	  err = hwloc_get_last_cpu_location(topology, cpubind_set, 0);
      } else {
	if (pid_number > 0)
	  err = hwloc_get_proc_cpubind(topology, pid, cpubind_set, 0);
#ifdef HWLOC_LINUX_SYS
	else if (tid_number > 0)
	  err = hwloc_linux_get_tid_cpubind(topology, tid_number, cpubind_set);
#endif
	else
	  err = hwloc_get_cpubind(topology, cpubind_set, 0);
      }
      if (err) {
	const char *errmsg = strerror(errno);
	if (pid_number > 0)
	  fprintf(stderr, "hwloc_get_proc_%s %d failed (errno %d %s)\n", get_last_cpu_location ? "last_cpu_location" : "cpubind", pid_number, errno, errmsg);
	else if (tid_number > 0)
	  fprintf(stderr, "hwloc_get_tid_%s %d failed (errno %d %s)\n", get_last_cpu_location ? "last_cpu_location" : "cpubind", tid_number, errno, errmsg);
	else
	  fprintf(stderr, "hwloc_get_%s failed (errno %d %s)\n", get_last_cpu_location ? "last_cpu_location" : "cpubind", errno, errmsg);
	return EXIT_FAILURE;
      }
      if (use_nodeset) {
	hwloc_bitmap_t nset = hwloc_bitmap_alloc();
	hwloc_cpuset_to_nodeset(topology, cpubind_set, nset);
	if (taskset)
	  hwloc_bitmap_taskset_asprintf(&s, nset);
	else
	  hwloc_bitmap_asprintf(&s, nset);
	hwloc_bitmap_free(nset);
      } else {
	if (taskset)
	  hwloc_bitmap_taskset_asprintf(&s, cpubind_set);
	else
	  hwloc_bitmap_asprintf(&s, cpubind_set);
      }

      } else {
      hwloc_membind_policy_t policy;
      if (pid_number > 0) {
	err = hwloc_get_proc_membind(topology, pid, membind_set, &policy, use_nodeset ? HWLOC_MEMBIND_BYNODESET : 0);
      } else if (tid_number > 0) {
	err = -1; errno = ENOSYS;
      } else {
	err = hwloc_get_membind(topology, membind_set, &policy, use_nodeset ? HWLOC_MEMBIND_BYNODESET : 0);
      }
      if (err) {
	const char *errmsg = strerror(errno);
        if (pid_number > 0)
          fprintf(stderr, "hwloc_get_proc_membind %d failed (errno %d %s)\n", pid_number, errno, errmsg);
        else
	  fprintf(stderr, "hwloc_get_membind failed (errno %d %s)\n", errno, errmsg);
	return EXIT_FAILURE;
      }
      if (taskset)
	hwloc_bitmap_taskset_asprintf(&s, membind_set);
      else
	hwloc_bitmap_asprintf(&s, membind_set);
      switch (policy) {
      case HWLOC_MEMBIND_FIRSTTOUCH: policystr = "firsttouch"; break;
      case HWLOC_MEMBIND_BIND: policystr = "bind"; break;
      case HWLOC_MEMBIND_INTERLEAVE: policystr = "interleave"; break;
      case HWLOC_MEMBIND_NEXTTOUCH: policystr = "nexttouch"; break;
      default: fprintf(stderr, "unknown memory policy %d\n", policy); assert(0); break;
      }
    }
    if (policystr)
      printf("%s (%s)\n", s, policystr);
    else
      printf("%s\n", s);
    free(s);
  }

  if (got_membind) {
    if (hwloc_bitmap_iszero(membind_set)) {
      if (verbose >= 0)
	fprintf(stderr, "cannot membind to empty set\n");
      if (!force)
	goto failed_binding;
    }
    if (verbose > 0) {
      char *s;
      hwloc_bitmap_asprintf(&s, membind_set);
      fprintf(stderr, "binding on memory set %s\n", s);
      free(s);
    }
    if (single)
      hwloc_bitmap_singlify(membind_set);
    if (pid_number > 0)
      ret = hwloc_set_proc_membind(topology, pid, membind_set, membind_policy, membind_flags | HWLOC_MEMBIND_BYNODESET);
    else if (tid_number > 0) {
      ret = -1; errno = ENOSYS;
    } else
      ret = hwloc_set_membind(topology, membind_set, membind_policy, membind_flags | HWLOC_MEMBIND_BYNODESET);
    if (ret && verbose >= 0) {
      int bind_errno = errno;
      const char *errmsg = strerror(bind_errno);
      char *s;
      hwloc_bitmap_asprintf(&s, membind_set);
      if (pid_number > 0)
        fprintf(stderr, "hwloc_set_proc_membind %s (policy %d flags %x) PID %d failed (errno %d %s)\n",
		s, membind_policy, membind_flags, pid_number, bind_errno, errmsg);
      else
        fprintf(stderr, "hwloc_set_membind %s (policy %d flags %x) failed (errno %d %s)\n",
		s, membind_policy, membind_flags, bind_errno, errmsg);
      free(s);
    }
    if (ret && !force)
      goto failed_binding;
  } else {
    if (got_mempolicy)
      fprintf(stderr, "--mempolicy ignored unless memory binding is also requested with --membind.\n");
  }

  if (got_cpubind) {
    if (hwloc_bitmap_iszero(cpubind_set)) {
      if (verbose >= 0)
	fprintf(stderr, "cannot cpubind to empty set\n");
      if (!force)
	goto failed_binding;
    }
    if (verbose > 0) {
      char *s;
      hwloc_bitmap_asprintf(&s, cpubind_set);
      fprintf(stderr, "binding on cpu set %s\n", s);
      free(s);
    }
    if (got_membind && !hwloc_bitmap_isequal(membind_set, cpubind_set)) {
      if (verbose)
	fprintf(stderr, "Conflicting CPU and memory binding requested, adding HWLOC_CPUBIND_NOMEMBIND flag.\n");
      cpubind_flags |= HWLOC_CPUBIND_NOMEMBIND;
    }
    if (single)
      hwloc_bitmap_singlify(cpubind_set);
    if (pid_number > 0)
      ret = hwloc_set_proc_cpubind(topology, pid, cpubind_set, cpubind_flags);
#ifdef HWLOC_LINUX_SYS
    else if (tid_number > 0)
      ret = hwloc_linux_set_tid_cpubind(topology, tid_number, cpubind_set);
#endif
    else
      ret = hwloc_set_cpubind(topology, cpubind_set, cpubind_flags);
    if (ret && verbose >= 0) {
      int bind_errno = errno;
      const char *errmsg = strerror(bind_errno);
      char *s;
      hwloc_bitmap_asprintf(&s, cpubind_set);
      if (pid_number > 0)
        fprintf(stderr, "hwloc_set_proc_cpubind %s (flags %x) PID %d failed (errno %d %s)\n",
		s, cpubind_flags, pid_number, bind_errno, errmsg);
      else if (tid_number > 0)
        fprintf(stderr, "hwloc_set_tid_cpubind %s (flags %x) PID %d failed (errno %d %s)\n",
		s, cpubind_flags, tid_number, bind_errno, errmsg);
      else
        fprintf(stderr, "hwloc_set_cpubind %s (flags %x) failed (errno %d %s)\n",
		s, cpubind_flags, bind_errno, errmsg);
      free(s);
    }
    if (ret && !force)
      goto failed_binding;
  }

  hwloc_bitmap_free(cpubind_set);
  hwloc_bitmap_free(membind_set);

  hwloc_topology_destroy(topology);

  if (pid_number > 0 || tid_number > 0)
    return EXIT_SUCCESS;

  if (0 == argc) {
    if (get_binding || get_last_cpu_location)
      return EXIT_SUCCESS;
    fprintf(stderr, "%s: nothing to do!\n", callname);
    return EXIT_FAILURE;
  }

  /* FIXME: check whether Windows execvp() passes INHERIT_PARENT_AFFINITY to CreateProcess()
   * because we need to propagate processor group affinity. However process-wide affinity
   * isn't supported with processor groups so far.
   */
  ret = execvp(argv[0], argv);
  if (ret) {
      fprintf(stderr, "%s: Failed to launch executable \"%s\"\n",
              callname, argv[0]);
      perror("execvp");
  }
  return EXIT_FAILURE;


failed_binding:
  hwloc_bitmap_free(cpubind_set);
  hwloc_bitmap_free(membind_set);
  hwloc_topology_destroy(topology);
  return EXIT_FAILURE;
}
