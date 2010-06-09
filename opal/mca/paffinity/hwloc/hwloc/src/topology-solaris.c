/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

#include <private/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

#include <stdio.h>
#include <errno.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/processor.h>
#include <sys/procset.h>

/* Note: get_cpubind not available on Solaris */
/* TODO: try to use pset (restricted to super-user) to support cpusets with weigth > 1? */
static int
hwloc_solaris_set_sth_cpubind(hwloc_topology_t topology, idtype_t idtype, id_t id, hwloc_const_cpuset_t hwloc_set, int policy __hwloc_attribute_unused)
{
  unsigned target;

  /* The resulting binding is always strict */

  if (hwloc_cpuset_isequal(hwloc_set, hwloc_topology_get_complete_cpuset(topology))) {
    if (processor_bind(idtype, id, PBIND_NONE, NULL) != 0)
      return -1;
    return 0;
  }

  if (hwloc_cpuset_weight(hwloc_set) != 1) {
    errno = EXDEV;
    return -1;
  }

  target = hwloc_cpuset_first(hwloc_set);

  if (processor_bind(idtype, id,
		     (processorid_t) (target), NULL) != 0)
    return -1;

  return 0;
}

static int
hwloc_solaris_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t hwloc_set, int policy)
{
  return hwloc_solaris_set_sth_cpubind(topology, P_PID, pid, hwloc_set, policy);
}

static int
hwloc_solaris_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set, int policy)
{
  return hwloc_solaris_set_sth_cpubind(topology, P_PID, P_MYID, hwloc_set, policy);
}

static int
hwloc_solaris_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set, int policy)
{
  return hwloc_solaris_set_sth_cpubind(topology, P_LWPID, P_MYID, hwloc_set, policy);
}

/* TODO: thread, maybe not easy because of the historical n:m implementation */

#ifdef HAVE_LIBLGRP
#      include <sys/lgrp_user.h>
static void
browse(struct hwloc_topology *topology, lgrp_cookie_t cookie, lgrp_id_t lgrp, hwloc_obj_t *glob_lgrps, unsigned *curlgrp)
{
  int n;
  hwloc_obj_t obj;
  lgrp_mem_size_t mem_size;

  n = lgrp_cpus(cookie, lgrp, NULL, 0, LGRP_CONTENT_HIERARCHY);
  if (n == -1)
    return;

  if ((mem_size = lgrp_mem_size(cookie, lgrp, LGRP_MEM_SZ_INSTALLED, LGRP_CONTENT_DIRECT)) > 0)
  {
    int i;
    processorid_t cpuids[n];

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_NODE, lgrp);
    obj->nodeset = hwloc_cpuset_alloc();
    hwloc_cpuset_set(obj->nodeset, lgrp);
    obj->cpuset = hwloc_cpuset_alloc();
    glob_lgrps[(*curlgrp)++] = obj;

    lgrp_cpus(cookie, lgrp, cpuids, n, LGRP_CONTENT_HIERARCHY);
    for (i = 0; i < n ; i++) {
      hwloc_debug("node %ld's cpu %d is %d\n", lgrp, i, cpuids[i]);
      hwloc_cpuset_set(obj->cpuset, cpuids[i]);
    }
    hwloc_debug_1arg_cpuset("node %ld has cpuset %s\n",
	lgrp, obj->cpuset);

    /* or LGRP_MEM_SZ_FREE */
    hwloc_debug("node %ld has %lldkB\n", lgrp, mem_size/1024);
    obj->memory.local_memory = mem_size;
    obj->memory.page_types_len = 2;
    obj->memory.page_types = malloc(2*sizeof(*obj->memory.page_types));
    memset(obj->memory.page_types, 0, 2*sizeof(*obj->memory.page_types));
    obj->memory.page_types[0].size = getpagesize();
#ifdef HAVE__SC_LARGE_PAGESIZE
    obj->memory.page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
    hwloc_insert_object_by_cpuset(topology, obj);
  }

  n = lgrp_children(cookie, lgrp, NULL, 0);
  {
    lgrp_id_t lgrps[n];
    int i;

    lgrp_children(cookie, lgrp, lgrps, n);
    hwloc_debug("lgrp %ld has %d children\n", lgrp, n);
    for (i = 0; i < n ; i++)
      {
	browse(topology, cookie, lgrps[i], glob_lgrps, curlgrp);
      }
    hwloc_debug("lgrp %ld's children done\n", lgrp);
  }
}

static void
hwloc_look_lgrp(struct hwloc_topology *topology)
{
  lgrp_cookie_t cookie;
  unsigned curlgrp = 0;
  int nlgrps;
  lgrp_id_t root;

  if ((topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM))
    cookie = lgrp_init(LGRP_VIEW_OS);
  else
    cookie = lgrp_init(LGRP_VIEW_CALLER);
  if (cookie == LGRP_COOKIE_NONE)
    {
      hwloc_debug("lgrp_init failed: %s\n", strerror(errno));
      return;
    }
  nlgrps = lgrp_nlgrps(cookie);
  root = lgrp_root(cookie);
  {
    hwloc_obj_t glob_lgrps[nlgrps];
    browse(topology, cookie, root, glob_lgrps, &curlgrp);
#ifdef HAVE_LGRP_LATENCY_COOKIE
    {
      unsigned distances[curlgrp][curlgrp];
      unsigned i, j;
      for (i = 0; i < curlgrp; i++)
	for (j = 0; j < curlgrp; j++)
	  distances[i][j] = lgrp_latency_cookie(cookie, glob_lgrps[i]->os_index, glob_lgrps[j]->os_index, LGRP_LAT_CPU_TO_MEM);
      hwloc_setup_misc_level_from_distances(topology, curlgrp, glob_lgrps, (unsigned*) distances);
    }
#endif /* HAVE_LGRP_LATENCY_COOKIE */
  }
  lgrp_fini(cookie);
}
#endif /* LIBLGRP */

#ifdef HAVE_LIBKSTAT
#include <kstat.h>
static int
hwloc_look_kstat(struct hwloc_topology *topology)
{
  kstat_ctl_t *kc = kstat_open();
  kstat_t *ksp;
  kstat_named_t *stat;
  unsigned look_cores = 1, look_chips = 1;

  unsigned numsockets = 0;
  unsigned proc_physids[HWLOC_NBMAXCPUS];
  unsigned proc_osphysids[HWLOC_NBMAXCPUS];
  unsigned osphysids[HWLOC_NBMAXCPUS];

  unsigned numcores = 0;
  unsigned proc_coreids[HWLOC_NBMAXCPUS];
  unsigned oscoreids[HWLOC_NBMAXCPUS];

  unsigned core_osphysids[HWLOC_NBMAXCPUS];

  unsigned numprocs = 0;
  unsigned proc_procids[HWLOC_NBMAXCPUS];
  unsigned osprocids[HWLOC_NBMAXCPUS];

  unsigned physid, coreid, cpuid;
  unsigned procid_max = 0;
  unsigned i;

  for (cpuid = 0; cpuid < HWLOC_NBMAXCPUS; cpuid++)
    {
      proc_procids[cpuid] = -1;
      proc_physids[cpuid] = -1;
      proc_osphysids[cpuid] = -1;
      proc_coreids[cpuid] = -1;
    }

  if (!kc)
    {
      hwloc_debug("kstat_open failed: %s\n", strerror(errno));
      return 0;
    }

  for (ksp = kc->kc_chain; ksp; ksp = ksp->ks_next)
    {
      if (strncmp("cpu_info", ksp->ks_module, 8))
	continue;

      cpuid = ksp->ks_instance;
      if (cpuid > HWLOC_NBMAXCPUS)
	{
	  fprintf(stderr,"CPU id too big: %u\n", cpuid);
	  continue;
	}

      if (kstat_read(kc, ksp, NULL) == -1)
	{
	  fprintf(stderr, "kstat_read failed for CPU%u: %s\n", cpuid, strerror(errno));
	  continue;
	}

      hwloc_debug("cpu%u\n", cpuid);
      proc_procids[cpuid] = numprocs;
      osprocids[numprocs] = cpuid;
      numprocs++;

      if (cpuid >= procid_max)
        procid_max = cpuid + 1;

      stat = (kstat_named_t *) kstat_data_lookup(ksp, "state");
      if (!stat)
          hwloc_debug("could not read state for CPU%u: %s\n", cpuid, strerror(errno));
      else if (stat->data_type != KSTAT_DATA_CHAR)
          hwloc_debug("unknown kstat type %d for cpu state\n", stat->data_type);
      else
        {
          hwloc_debug("cpu%u's state is %s\n", cpuid, stat->value.c);
          if (strcmp(stat->value.c, "on-line"))
            /* not online */
            hwloc_cpuset_clr(topology->levels[0][0]->online_cpuset, cpuid);
        }

      if (look_chips) do {
	/* Get Chip ID */
	stat = (kstat_named_t *) kstat_data_lookup(ksp, "chip_id");
	if (!stat)
	  {
	    if (numsockets)
	      fprintf(stderr, "could not read socket id for CPU%u: %s\n", cpuid, strerror(errno));
	    else
	      hwloc_debug("could not read socket id for CPU%u: %s\n", cpuid, strerror(errno));
	    look_chips = 0;
	    continue;
	  }
	switch (stat->data_type) {
	  case KSTAT_DATA_INT32:
	    physid = stat->value.i32;
	    break;
	  case KSTAT_DATA_UINT32:
	    physid = stat->value.ui32;
	    break;
#ifdef _INT64_TYPE
	  case KSTAT_DATA_UINT64:
	    physid = stat->value.ui64;
	    break;
	  case KSTAT_DATA_INT64:
	    physid = stat->value.i64;
	    break;
#endif
	  default:
	    fprintf(stderr, "chip_id type %d unknown\n", stat->data_type);
	    look_chips = 0;
	    continue;
	}
	proc_osphysids[cpuid] = physid;
	for (i = 0; i < numsockets; i++)
	  if (physid == osphysids[i])
	    break;
	proc_physids[cpuid] = i;
	hwloc_debug("%u on socket %u (%u)\n", cpuid, i, physid);
	if (i == numsockets)
	  osphysids[numsockets++] = physid;
      } while(0);

      if (look_cores) do {
	/* Get Core ID */
	stat = (kstat_named_t *) kstat_data_lookup(ksp, "core_id");
	if (!stat)
	  {
	    if (numcores)
	      fprintf(stderr, "could not read core id for CPU%u: %s\n", cpuid, strerror(errno));
	    else
	      hwloc_debug("could not read core id for CPU%u: %s\n", cpuid, strerror(errno));
	    look_cores = 0;
	    continue;
	  }
	switch (stat->data_type) {
	  case KSTAT_DATA_INT32:
	    coreid = stat->value.i32;
	    break;
	  case KSTAT_DATA_UINT32:
	    coreid = stat->value.ui32;
	    break;
#ifdef _INT64_TYPE
	  case KSTAT_DATA_UINT64:
	    coreid = stat->value.ui64;
	    break;
	  case KSTAT_DATA_INT64:
	    coreid = stat->value.i64;
	    break;
#endif
	  default:
	    fprintf(stderr, "core_id type %d unknown\n", stat->data_type);
	    look_cores = 0;
	    continue;
	}
	for (i = 0; i < numcores; i++)
	  if (coreid == oscoreids[i] && proc_osphysids[cpuid] == core_osphysids[i])
	    break;
	proc_coreids[cpuid] = i;
	hwloc_debug("%u on core %u (%u)\n", cpuid, i, coreid);
	if (i == numcores)
	  {
	    core_osphysids[numcores] = proc_osphysids[cpuid];
	    oscoreids[numcores++] = coreid;
	  }
      } while(0);

      /* Note: there is also clog_id for the Thread ID (not unique) and
       * pkg_core_id for the core ID (not unique).  They are not useful to us
       * however. */
    }

  if (look_chips)
    hwloc_setup_level(procid_max, numsockets, osphysids, proc_physids, topology, HWLOC_OBJ_SOCKET);

  if (look_cores)
    hwloc_setup_level(procid_max, numcores, oscoreids, proc_coreids, topology, HWLOC_OBJ_CORE);

  if (numprocs)
    hwloc_setup_level(procid_max, numprocs, osprocids, proc_procids, topology, HWLOC_OBJ_PU);

  kstat_close(kc);

  return numprocs > 0;
}
#endif /* LIBKSTAT */

void
hwloc_look_solaris(struct hwloc_topology *topology)
{
  unsigned nbprocs = hwloc_fallback_nbprocessors (topology);
#ifdef HAVE_LIBLGRP
  hwloc_look_lgrp(topology);
#endif /* HAVE_LIBLGRP */
#ifdef HAVE_LIBKSTAT
  nbprocs = 0;
  if (hwloc_look_kstat(topology))
    return;
#endif /* HAVE_LIBKSTAT */
  hwloc_setup_pu_level(topology, nbprocs);
}

void
hwloc_set_solaris_hooks(struct hwloc_topology *topology)
{
  topology->set_proc_cpubind = hwloc_solaris_set_proc_cpubind;
  topology->set_thisproc_cpubind = hwloc_solaris_set_thisproc_cpubind;
  topology->set_thisthread_cpubind = hwloc_solaris_set_thisthread_cpubind;
}

/* TODO:
 * memory binding: lgrp_affinity_set
 * madvise(MADV_ACCESS_LWP / ACCESS_MANY)
 */
