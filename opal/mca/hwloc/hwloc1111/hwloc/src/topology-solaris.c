/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2014 Inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright © 2011      Oracle and/or its affiliates.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>
#include <private/solaris-chiptype.h>

#include <stdio.h>
#include <errno.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/processor.h>
#include <sys/procset.h>
#include <sys/types.h>
#include <sys/mman.h>

#ifdef HAVE_LIBLGRP
#  include <sys/lgrp_user.h>
#endif

/* TODO: use psets? (only for root)
 * TODO: get cache info from prtdiag? (it is setgid sys to be able to read from
 * crw-r-----   1 root     sys       88,  0 nov   3 14:35 /devices/pseudo/devinfo@0:devinfo
 * and run (apparently undocumented) ioctls on it.
 */

static int
hwloc_solaris_set_sth_cpubind(hwloc_topology_t topology, idtype_t idtype, id_t id, hwloc_const_bitmap_t hwloc_set, int flags)
{
  unsigned target_cpu;

  /* The resulting binding is always strict */

  if (hwloc_bitmap_isequal(hwloc_set, hwloc_topology_get_complete_cpuset(topology))) {
    if (processor_bind(idtype, id, PBIND_NONE, NULL) != 0)
      return -1;
#ifdef HAVE_LIBLGRP
    if (!(flags & HWLOC_CPUBIND_NOMEMBIND)) {
      int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
      if (depth >= 0) {
	int n = hwloc_get_nbobjs_by_depth(topology, depth);
	int i;

	for (i = 0; i < n; i++) {
	  hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, i);
	  lgrp_affinity_set(idtype, id, obj->os_index, LGRP_AFF_NONE);
	}
      }
    }
#endif /* HAVE_LIBLGRP */
    return 0;
  }

#ifdef HAVE_LIBLGRP
  if (!(flags & HWLOC_CPUBIND_NOMEMBIND)) {
    int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
    if (depth >= 0) {
      int n = hwloc_get_nbobjs_by_depth(topology, depth);
      int i;
      int ok;
      hwloc_bitmap_t target = hwloc_bitmap_alloc();

      for (i = 0; i < n; i++) {
	hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, i);
        if (hwloc_bitmap_isincluded(obj->cpuset, hwloc_set))
          hwloc_bitmap_or(target, target, obj->cpuset);
      }

      ok = hwloc_bitmap_isequal(target, hwloc_set);
      hwloc_bitmap_free(target);

      if (ok) {
        /* Ok, managed to achieve hwloc_set by just combining NUMA nodes */

        for (i = 0; i < n; i++) {
          hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, i);

          if (hwloc_bitmap_isincluded(obj->cpuset, hwloc_set)) {
            lgrp_affinity_set(idtype, id, obj->os_index, LGRP_AFF_STRONG);
          } else {
            if (flags & HWLOC_CPUBIND_STRICT)
              lgrp_affinity_set(idtype, id, obj->os_index, LGRP_AFF_NONE);
            else
              lgrp_affinity_set(idtype, id, obj->os_index, LGRP_AFF_WEAK);
          }
        }

        return 0;
      }
    }
  }
#endif /* HAVE_LIBLGRP */

  if (hwloc_bitmap_weight(hwloc_set) != 1) {
    errno = EXDEV;
    return -1;
  }

  target_cpu = hwloc_bitmap_first(hwloc_set);

  if (processor_bind(idtype, id,
		     (processorid_t) (target_cpu), NULL) != 0)
    return -1;

  return 0;
}

static int
hwloc_solaris_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_bitmap_t hwloc_set, int flags)
{
  return hwloc_solaris_set_sth_cpubind(topology, P_PID, pid, hwloc_set, flags);
}

static int
hwloc_solaris_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags)
{
  return hwloc_solaris_set_sth_cpubind(topology, P_PID, P_MYID, hwloc_set, flags);
}

static int
hwloc_solaris_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags)
{
  return hwloc_solaris_set_sth_cpubind(topology, P_LWPID, P_MYID, hwloc_set, flags);
}

#ifdef HAVE_LIBLGRP
static int
hwloc_solaris_get_sth_cpubind(hwloc_topology_t topology, idtype_t idtype, id_t id, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  processorid_t binding;
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  int n;
  int i;

  if (depth < 0) {
    errno = ENOSYS;
    return -1;
  }

  /* first check if processor_bind() was used to bind to a single processor rather than to an lgroup */
  if ( processor_bind(idtype, id, PBIND_QUERY, &binding) == 0 && binding != PBIND_NONE ) {
    hwloc_bitmap_only(hwloc_set, binding);
    return 0;
  }

  /* if not, check lgroups */
  hwloc_bitmap_zero(hwloc_set);
  n = hwloc_get_nbobjs_by_depth(topology, depth);
  for (i = 0; i < n; i++) {
    hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, i);
    lgrp_affinity_t aff = lgrp_affinity_get(idtype, id, obj->os_index);

    if (aff == LGRP_AFF_STRONG)
      hwloc_bitmap_or(hwloc_set, hwloc_set, obj->cpuset);
  }

  if (hwloc_bitmap_iszero(hwloc_set))
    hwloc_bitmap_copy(hwloc_set, hwloc_topology_get_complete_cpuset(topology));

  return 0;
}

static int
hwloc_solaris_get_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_bitmap_t hwloc_set, int flags)
{
  return hwloc_solaris_get_sth_cpubind(topology, P_PID, pid, hwloc_set, flags);
}

static int
hwloc_solaris_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags)
{
  return hwloc_solaris_get_sth_cpubind(topology, P_PID, P_MYID, hwloc_set, flags);
}

static int
hwloc_solaris_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags)
{
  return hwloc_solaris_get_sth_cpubind(topology, P_LWPID, P_MYID, hwloc_set, flags);
}
#endif /* HAVE_LIBLGRP */

/* TODO: given thread, probably not easy because of the historical n:m implementation */
#ifdef HAVE_LIBLGRP
static int
hwloc_solaris_set_sth_membind(hwloc_topology_t topology, idtype_t idtype, id_t id, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  int depth;
  int n, i;

  switch (policy) {
    case HWLOC_MEMBIND_DEFAULT:
    case HWLOC_MEMBIND_BIND:
      break;
    default:
      errno = ENOSYS;
      return -1;
  }

  if (flags & HWLOC_MEMBIND_NOCPUBIND) {
    errno = ENOSYS;
    return -1;
  }

  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  if (depth < 0) {
    errno = EXDEV;
    return -1;
  }
  n = hwloc_get_nbobjs_by_depth(topology, depth);

  for (i = 0; i < n; i++) {
    hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, i);
    if (hwloc_bitmap_isset(nodeset, obj->os_index)) {
      lgrp_affinity_set(idtype, id, obj->os_index, LGRP_AFF_STRONG);
    } else {
      if (flags & HWLOC_CPUBIND_STRICT)
	lgrp_affinity_set(idtype, id, obj->os_index, LGRP_AFF_NONE);
      else
	lgrp_affinity_set(idtype, id, obj->os_index, LGRP_AFF_WEAK);
    }
  }

  return 0;
}

static int
hwloc_solaris_set_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  return hwloc_solaris_set_sth_membind(topology, P_PID, pid, nodeset, policy, flags);
}

static int
hwloc_solaris_set_thisproc_membind(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  return hwloc_solaris_set_sth_membind(topology, P_PID, P_MYID, nodeset, policy, flags);
}

static int
hwloc_solaris_set_thisthread_membind(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  return hwloc_solaris_set_sth_membind(topology, P_LWPID, P_MYID, nodeset, policy, flags);
}

static int
hwloc_solaris_get_sth_membind(hwloc_topology_t topology, idtype_t idtype, id_t id, hwloc_nodeset_t nodeset, hwloc_membind_policy_t *policy, int flags __hwloc_attribute_unused)
{
  int depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  int n;
  int i;

  if (depth < 0) {
    errno = ENOSYS;
    return -1;
  }

  hwloc_bitmap_zero(nodeset);
  n = hwloc_get_nbobjs_by_depth(topology, depth);

  for (i = 0; i < n; i++) {
    hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, i);
    lgrp_affinity_t aff = lgrp_affinity_get(idtype, id, obj->os_index);

    if (aff == LGRP_AFF_STRONG)
      hwloc_bitmap_set(nodeset, obj->os_index);
  }

  if (hwloc_bitmap_iszero(nodeset))
    hwloc_bitmap_copy(nodeset, hwloc_topology_get_complete_nodeset(topology));

  *policy = HWLOC_MEMBIND_DEFAULT;
  return 0;
}

static int
hwloc_solaris_get_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_nodeset_t nodeset, hwloc_membind_policy_t *policy, int flags)
{
  return hwloc_solaris_get_sth_membind(topology, P_PID, pid, nodeset, policy, flags);
}

static int
hwloc_solaris_get_thisproc_membind(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t *policy, int flags)
{
  return hwloc_solaris_get_sth_membind(topology, P_PID, P_MYID, nodeset, policy, flags);
}

static int
hwloc_solaris_get_thisthread_membind(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t *policy, int flags)
{
  return hwloc_solaris_get_sth_membind(topology, P_LWPID, P_MYID, nodeset, policy, flags);
}
#endif /* HAVE_LIBLGRP */


#ifdef MADV_ACCESS_LWP
static int
hwloc_solaris_set_area_membind(hwloc_topology_t topology, const void *addr, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags __hwloc_attribute_unused)
{
  int advice;
  size_t remainder;

  /* Can not give a set of nodes just for an area.  */
  if (!hwloc_bitmap_isequal(nodeset, hwloc_topology_get_complete_nodeset(topology))) {
    errno = EXDEV;
    return -1;
  }

  switch (policy) {
    case HWLOC_MEMBIND_DEFAULT:
    case HWLOC_MEMBIND_BIND:
      advice = MADV_ACCESS_DEFAULT;
      break;
    case HWLOC_MEMBIND_FIRSTTOUCH:
    case HWLOC_MEMBIND_NEXTTOUCH:
      advice = MADV_ACCESS_LWP;
      break;
    case HWLOC_MEMBIND_INTERLEAVE:
      advice = MADV_ACCESS_MANY;
      break;
    default:
      errno = ENOSYS;
      return -1;
  }

  remainder = (uintptr_t) addr & (sysconf(_SC_PAGESIZE)-1);
  addr = (char*) addr - remainder;
  len += remainder;
  return madvise((void*) addr, len, advice);
}
#endif

#ifdef HAVE_LIBLGRP
static void
browse(struct hwloc_topology *topology, lgrp_cookie_t cookie, lgrp_id_t lgrp, hwloc_obj_t *glob_lgrps, unsigned *curlgrp)
{
  int n;
  hwloc_obj_t obj;
  lgrp_mem_size_t mem_size;

  n = lgrp_cpus(cookie, lgrp, NULL, 0, LGRP_CONTENT_HIERARCHY);
  if (n == -1)
    return;

  /* Is this lgrp a NUMA node? */
  if ((mem_size = lgrp_mem_size(cookie, lgrp, LGRP_MEM_SZ_INSTALLED, LGRP_CONTENT_DIRECT)) > 0)
  {
    int i;
    processorid_t *cpuids;
    cpuids = malloc(sizeof(processorid_t) * n);
    assert(cpuids != NULL);

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_NUMANODE, lgrp);
    obj->nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_set(obj->nodeset, lgrp);
    obj->cpuset = hwloc_bitmap_alloc();
    glob_lgrps[(*curlgrp)++] = obj;

    lgrp_cpus(cookie, lgrp, cpuids, n, LGRP_CONTENT_HIERARCHY);
    for (i = 0; i < n ; i++) {
      hwloc_debug("node %ld's cpu %d is %d\n", lgrp, i, cpuids[i]);
      hwloc_bitmap_set(obj->cpuset, cpuids[i]);
    }
    hwloc_debug_1arg_bitmap("node %ld has cpuset %s\n",
	lgrp, obj->cpuset);

    /* or LGRP_MEM_SZ_FREE */
    hwloc_debug("node %ld has %lldkB\n", lgrp, mem_size/1024);
    obj->memory.local_memory = mem_size;
    obj->memory.page_types_len = 2;
    obj->memory.page_types = malloc(2*sizeof(*obj->memory.page_types));
    memset(obj->memory.page_types, 0, 2*sizeof(*obj->memory.page_types));
    obj->memory.page_types[0].size = hwloc_getpagesize();
#ifdef HAVE__SC_LARGE_PAGESIZE
    obj->memory.page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
    hwloc_insert_object_by_cpuset(topology, obj);
    free(cpuids);
  }

  n = lgrp_children(cookie, lgrp, NULL, 0);
  {
    lgrp_id_t *lgrps;
    int i;

    lgrps = malloc(sizeof(lgrp_id_t) * n);
    assert(lgrps != NULL);
    lgrp_children(cookie, lgrp, lgrps, n);
    hwloc_debug("lgrp %ld has %d children\n", lgrp, n);
    for (i = 0; i < n ; i++)
      {
	browse(topology, cookie, lgrps[i], glob_lgrps, curlgrp);
      }
    hwloc_debug("lgrp %ld's children done\n", lgrp);
    free(lgrps);
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
    hwloc_obj_t *glob_lgrps = calloc(nlgrps, sizeof(hwloc_obj_t));
    browse(topology, cookie, root, glob_lgrps, &curlgrp);
#ifdef HAVE_LGRP_LATENCY_COOKIE
    {
      float *distances = calloc(curlgrp*curlgrp, sizeof(float));
      unsigned *indexes = calloc(curlgrp,sizeof(unsigned));
      unsigned i, j;
      for (i = 0; i < curlgrp; i++) {
	indexes[i] = glob_lgrps[i]->os_index;
	for (j = 0; j < curlgrp; j++)
          distances[i*curlgrp+j] = (float) lgrp_latency_cookie(cookie, glob_lgrps[i]->os_index, glob_lgrps[j]->os_index, LGRP_LAT_CPU_TO_MEM);
      }
      hwloc_distances_set(topology, HWLOC_OBJ_NUMANODE, curlgrp, indexes, glob_lgrps, distances, 0 /* OS cannot force */);
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
  /* FIXME this assumes that all packages are identical */
  char *CPUType = hwloc_solaris_get_chip_type();
  char *CPUModel = hwloc_solaris_get_chip_model();

  kstat_ctl_t *kc = kstat_open();
  kstat_t *ksp;
  kstat_named_t *stat;
  unsigned look_cores = 1, look_chips = 1;

  unsigned Pproc_max = 0;
  unsigned Pproc_alloc = 256;
  struct hwloc_solaris_Pproc {
    unsigned Lpkg, Ppkg, Lcore, Lproc;
  } * Pproc = malloc(Pproc_alloc * sizeof(*Pproc));

  unsigned Lproc_num = 0;
  unsigned Lproc_alloc = 256;
  struct hwloc_solaris_Lproc {
    unsigned Pproc;
  } * Lproc = malloc(Lproc_alloc * sizeof(*Lproc));

  unsigned Lcore_num = 0;
  unsigned Lcore_alloc = 256;
  struct hwloc_solaris_Lcore {
    unsigned Pcore, Ppkg;
  } * Lcore = malloc(Lcore_alloc * sizeof(*Lcore));

  unsigned Lpkg_num = 0;
  unsigned Lpkg_alloc = 256;
  struct hwloc_solaris_Lpkg {
    unsigned Ppkg;
  } * Lpkg = malloc(Lpkg_alloc * sizeof(*Lpkg));

  unsigned pkgid, coreid, cpuid;
  unsigned i;

  for (i = 0; i < Pproc_alloc; i++) {
    Pproc[i].Lproc = -1;
    Pproc[i].Lpkg = -1;
    Pproc[i].Ppkg = -1;
    Pproc[i].Lcore = -1;
  }

  if (!kc) {
    hwloc_debug("kstat_open failed: %s\n", strerror(errno));
    free(Pproc);
    free(Lproc);
    free(Lcore);
    free(Lpkg);
    return 0;
  }

  for (ksp = kc->kc_chain; ksp; ksp = ksp->ks_next)
    {
      if (strncmp("cpu_info", ksp->ks_module, 8))
	continue;

      cpuid = ksp->ks_instance;

      if (kstat_read(kc, ksp, NULL) == -1)
	{
	  fprintf(stderr, "kstat_read failed for CPU%u: %s\n", cpuid, strerror(errno));
	  continue;
	}

      hwloc_debug("cpu%u\n", cpuid);

      if (cpuid >= Pproc_alloc) {
	Pproc_alloc *= 2;
	Pproc = realloc(Pproc, Pproc_alloc * sizeof(*Pproc));
	for(i = Pproc_alloc/2; i < Pproc_alloc; i++) {
	  Pproc[i].Lproc = -1;
	  Pproc[i].Lpkg = -1;
	  Pproc[i].Ppkg = -1;
	  Pproc[i].Lcore = -1;
	}
      }
      Pproc[cpuid].Lproc = Lproc_num;

      if (Lproc_num >= Lproc_alloc) {
	Lproc_alloc *= 2;
	Lproc = realloc(Lproc, Lproc_alloc * sizeof(*Lproc));
      }
      Lproc[Lproc_num].Pproc = cpuid;
      Lproc_num++;

      if (cpuid >= Pproc_max)
        Pproc_max = cpuid + 1;

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
            hwloc_bitmap_clr(topology->levels[0][0]->online_cpuset, cpuid);
        }

      if (look_chips) do {
	/* Get Chip ID */
	stat = (kstat_named_t *) kstat_data_lookup(ksp, "chip_id");
	if (!stat)
	  {
	    if (Lpkg_num)
	      fprintf(stderr, "could not read package id for CPU%u: %s\n", cpuid, strerror(errno));
	    else
	      hwloc_debug("could not read package id for CPU%u: %s\n", cpuid, strerror(errno));
	    look_chips = 0;
	    continue;
	  }
	switch (stat->data_type) {
	  case KSTAT_DATA_INT32:
	    pkgid = stat->value.i32;
	    break;
	  case KSTAT_DATA_UINT32:
	    pkgid = stat->value.ui32;
	    break;
#ifdef _INT64_TYPE
	  case KSTAT_DATA_UINT64:
	    pkgid = stat->value.ui64;
	    break;
	  case KSTAT_DATA_INT64:
	    pkgid = stat->value.i64;
	    break;
#endif
	  default:
	    fprintf(stderr, "chip_id type %d unknown\n", stat->data_type);
	    look_chips = 0;
	    continue;
	}
	Pproc[cpuid].Ppkg = pkgid;
	for (i = 0; i < Lpkg_num; i++)
	  if (pkgid == Lpkg[i].Ppkg)
	    break;
	Pproc[cpuid].Lpkg = i;
	hwloc_debug("%u on package %u (%u)\n", cpuid, i, pkgid);
	if (i == Lpkg_num) {
	  if (Lpkg_num == Lpkg_alloc) {
	    Lpkg_alloc *= 2;
	    Lpkg = realloc(Lpkg, Lpkg_alloc * sizeof(*Lpkg));
	  }
	  Lpkg[Lpkg_num++].Ppkg = pkgid;
	}
      } while(0);

      if (look_cores) do {
	/* Get Core ID */
	stat = (kstat_named_t *) kstat_data_lookup(ksp, "core_id");
	if (!stat)
	  {
	    if (Lcore_num)
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
	for (i = 0; i < Lcore_num; i++)
	  if (coreid == Lcore[i].Pcore && Pproc[cpuid].Ppkg == Lcore[i].Ppkg)
	    break;
	Pproc[cpuid].Lcore = i;
	hwloc_debug("%u on core %u (%u)\n", cpuid, i, coreid);
	if (i == Lcore_num) {
	  if (Lcore_num == Lcore_alloc) {
	    Lcore_alloc *= 2;
	    Lcore = realloc(Lcore, Lcore_alloc * sizeof(*Lcore));
	  }
	  Lcore[Lcore_num].Ppkg = Pproc[cpuid].Ppkg;
	  Lcore[Lcore_num++].Pcore = coreid;
	}
      } while(0);

      /* Note: there is also clog_id for the Thread ID (not unique) and
       * pkg_core_id for the core ID (not unique).  They are not useful to us
       * however. */
    }

  if (look_chips) {
    struct hwloc_obj *obj;
    unsigned j,k;
    hwloc_debug("%d Packages\n", Lpkg_num);
    for (j = 0; j < Lpkg_num; j++) {
      obj = hwloc_alloc_setup_object(HWLOC_OBJ_PACKAGE, Lpkg[j].Ppkg);
      if (CPUType)
	hwloc_obj_add_info(obj, "CPUType", CPUType);
      if (CPUModel)
	hwloc_obj_add_info(obj, "CPUModel", CPUModel);
      obj->cpuset = hwloc_bitmap_alloc();
      for(k=0; k<Pproc_max; k++)
	if (Pproc[k].Lpkg == j)
	  hwloc_bitmap_set(obj->cpuset, k);
      hwloc_debug_1arg_bitmap("Package %d has cpuset %s\n", j, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    hwloc_debug("%s", "\n");
  }

  if (look_cores) {
    struct hwloc_obj *obj;
    unsigned j,k;
    hwloc_debug("%d Cores\n", Lcore_num);
    for (j = 0; j < Lcore_num; j++) {
      obj = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, Lcore[j].Pcore);
      obj->cpuset = hwloc_bitmap_alloc();
      for(k=0; k<Pproc_max; k++)
	if (Pproc[k].Lcore == j)
	  hwloc_bitmap_set(obj->cpuset, k);
      hwloc_debug_1arg_bitmap("Core %d has cpuset %s\n", j, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    hwloc_debug("%s", "\n");
  }
  if (Lproc_num) {
    struct hwloc_obj *obj;
    unsigned j,k;
    hwloc_debug("%d PUs\n", Lproc_num);
    for (j = 0; j < Lproc_num; j++) {
      obj = hwloc_alloc_setup_object(HWLOC_OBJ_PU, Lproc[j].Pproc);
      obj->cpuset = hwloc_bitmap_alloc();
      for(k=0; k<Pproc_max; k++)
	if (Pproc[k].Lproc == j)
	  hwloc_bitmap_set(obj->cpuset, k);
      hwloc_debug_1arg_bitmap("PU %d has cpuset %s\n", j, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    hwloc_debug("%s", "\n");
  }

  kstat_close(kc);

  free(Pproc);
  free(Lproc);
  free(Lcore);
  free(Lpkg);

  return Lproc_num > 0;
}
#endif /* LIBKSTAT */

static int
hwloc_look_solaris(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  unsigned nbprocs = hwloc_fallback_nbprocessors (topology);
  int alreadypus = 0;

  if (topology->levels[0][0]->cpuset)
    /* somebody discovered things */
    return 0;

  hwloc_alloc_obj_cpusets(topology->levels[0][0]);

#ifdef HAVE_LIBLGRP
  hwloc_look_lgrp(topology);
#endif /* HAVE_LIBLGRP */
#ifdef HAVE_LIBKSTAT
  if (hwloc_look_kstat(topology) > 0)
    alreadypus = 1;
#endif /* HAVE_LIBKSTAT */
  if (!alreadypus)
    hwloc_setup_pu_level(topology, nbprocs);

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "Solaris");
  if (topology->is_thissystem)
    hwloc_add_uname_info(topology, NULL);
  return 1;
}

void
hwloc_set_solaris_hooks(struct hwloc_binding_hooks *hooks,
			struct hwloc_topology_support *support __hwloc_attribute_unused)
{
  hooks->set_proc_cpubind = hwloc_solaris_set_proc_cpubind;
  hooks->set_thisproc_cpubind = hwloc_solaris_set_thisproc_cpubind;
  hooks->set_thisthread_cpubind = hwloc_solaris_set_thisthread_cpubind;
#ifdef HAVE_LIBLGRP
  hooks->get_proc_cpubind = hwloc_solaris_get_proc_cpubind;
  hooks->get_thisproc_cpubind = hwloc_solaris_get_thisproc_cpubind;
  hooks->get_thisthread_cpubind = hwloc_solaris_get_thisthread_cpubind;
  hooks->set_proc_membind = hwloc_solaris_set_proc_membind;
  hooks->set_thisproc_membind = hwloc_solaris_set_thisproc_membind;
  hooks->set_thisthread_membind = hwloc_solaris_set_thisthread_membind;
  hooks->get_proc_membind = hwloc_solaris_get_proc_membind;
  hooks->get_thisproc_membind = hwloc_solaris_get_thisproc_membind;
  hooks->get_thisthread_membind = hwloc_solaris_get_thisthread_membind;
#endif /* HAVE_LIBLGRP */
#ifdef MADV_ACCESS_LWP
  hooks->set_area_membind = hwloc_solaris_set_area_membind;
  support->membind->firsttouch_membind = 1;
  support->membind->bind_membind = 1;
  support->membind->interleave_membind = 1;
  support->membind->nexttouch_membind = 1;
#endif
}

static struct hwloc_backend *
hwloc_solaris_component_instantiate(struct hwloc_disc_component *component,
				    const void *_data1 __hwloc_attribute_unused,
				    const void *_data2 __hwloc_attribute_unused,
				    const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_solaris;
  return backend;
}

static struct hwloc_disc_component hwloc_solaris_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_CPU,
  "solaris",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_solaris_component_instantiate,
  50,
  NULL
};

const struct hwloc_component hwloc_solaris_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_solaris_disc_component
};
