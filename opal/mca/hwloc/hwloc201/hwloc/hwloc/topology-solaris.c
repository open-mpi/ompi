/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
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
#include <sys/systeminfo.h>
#include <sys/types.h>
#include <sys/mman.h>

#ifdef HAVE_LIBLGRP
#  include <sys/lgrp_user.h>
#endif

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
      int n, i;
      n = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);
      for (i = 0; i < n; i++) {
	hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE, i);
	lgrp_affinity_set(idtype, id, obj->os_index, LGRP_AFF_NONE);
      }
    }
#endif /* HAVE_LIBLGRP */
    return 0;
  }

#ifdef HAVE_LIBLGRP
  if (!(flags & HWLOC_CPUBIND_NOMEMBIND)) {
    int n, i, ok;
    n = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);
    hwloc_bitmap_t target = hwloc_bitmap_alloc();
    for (i = 0; i < n; i++) {
      hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE, i);
      if (hwloc_bitmap_isincluded(obj->cpuset, hwloc_set))
	hwloc_bitmap_or(target, target, obj->cpuset);
    }

    ok = hwloc_bitmap_isequal(target, hwloc_set);
    hwloc_bitmap_free(target);

    if (ok) {
      /* Ok, managed to achieve hwloc_set by just combining NUMA nodes */

      for (i = 0; i < n; i++) {
        hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE, i);

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
  int n;
  int i;

  /* first check if processor_bind() was used to bind to a single processor rather than to an lgroup */
  if ( processor_bind(idtype, id, PBIND_QUERY, &binding) == 0 && binding != PBIND_NONE ) {
    hwloc_bitmap_only(hwloc_set, binding);
    return 0;
  }

  /* if not, check lgroups */
  hwloc_bitmap_zero(hwloc_set);
  n = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);
  for (i = 0; i < n; i++) {
    hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE, i);
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
hwloc_solaris_set_sth_membind(hwloc_topology_t topology, idtype_t idtype, id_t id, hwloc_const_nodeset_t _nodeset, hwloc_membind_policy_t policy, int flags)
{
  int n, i;
  hwloc_const_nodeset_t nodeset;

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

  if (policy == HWLOC_MEMBIND_DEFAULT)
    nodeset = hwloc_topology_get_complete_nodeset(topology);
  else
    nodeset = _nodeset;

  n = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);

  for (i = 0; i < n; i++) {
    hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE, i);
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
  int n;
  int i;

  hwloc_bitmap_zero(nodeset);
  n = hwloc_get_nbobjs_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE);

  for (i = 0; i < n; i++) {
    hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, HWLOC_TYPE_DEPTH_NUMANODE, i);
    lgrp_affinity_t aff = lgrp_affinity_get(idtype, id, obj->os_index);

    if (aff == LGRP_AFF_STRONG)
      hwloc_bitmap_set(nodeset, obj->os_index);
  }

  if (hwloc_bitmap_iszero(nodeset))
    hwloc_bitmap_copy(nodeset, hwloc_topology_get_complete_nodeset(topology));

  *policy = HWLOC_MEMBIND_BIND;
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
  if (policy != HWLOC_MEMBIND_DEFAULT
      && !hwloc_bitmap_isequal(nodeset, hwloc_topology_get_complete_nodeset(topology))) {
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

/* list the allowed PUs and NUMA Nodes using LGRP_VIEW_CALLER */
static void
lgrp_list_allowed(struct hwloc_topology *topology)
{
  lgrp_cookie_t cookie;
  lgrp_id_t root;
  int npids, nnids;
  int i, n;
  processorid_t *pids;
  lgrp_id_t *nids;

  cookie = lgrp_init(LGRP_VIEW_CALLER);
  if (cookie == LGRP_COOKIE_NONE) {
    hwloc_debug("lgrp_init LGRP_VIEW_CALLER failed: %s\n", strerror(errno));
    goto out;
  }
  root = lgrp_root(cookie);

  /* list allowed PUs */
  npids = lgrp_cpus(cookie, root, NULL, 0, LGRP_CONTENT_HIERARCHY);
  if (npids < 0) {
    hwloc_debug("lgrp_cpus failed: %s\n", strerror(errno));
    goto out_with_cookie;
  }
  hwloc_debug("root lgrp contains %d allowed PUs\n", npids);
  assert(npids > 0);

  pids = malloc(npids * sizeof(*pids));
  if (!pids)
    goto out_with_cookie;

  n = lgrp_cpus(cookie, root, pids, npids, LGRP_CONTENT_HIERARCHY);
  assert(n == npids);

  hwloc_bitmap_zero(topology->allowed_cpuset);

  for(i=0; i<npids; i++) {
    hwloc_debug("root lgrp contains allowed PU #%d = P#%d\n", i, pids[i]);
    hwloc_bitmap_set(topology->allowed_cpuset, pids[i]);
  }
  free(pids);

  /* list allowed NUMA nodes */
  nnids = lgrp_resources(cookie, root, NULL, 0, LGRP_RSRC_MEM);
  if (nnids < 0) {
    hwloc_debug("lgrp_resources failed: %s\n", strerror(errno));
    goto out_with_cookie;
  }
  hwloc_debug("root lgrp contains %d allowed NUMA nodes\n", nnids);
  assert(nnids > 0);

  nids = malloc(nnids * sizeof(*nids));
  if (!nids)
    goto out_with_cookie;

  n = lgrp_resources(cookie, root, nids, nnids, LGRP_RSRC_MEM);
  assert(n == nnids);

  hwloc_bitmap_zero(topology->allowed_nodeset);

  for(i=0; i<nnids; i++) {
    hwloc_debug("root lgrp contains allowed NUMA node #%d = P#%ld\n", i, nids[i]);
    hwloc_bitmap_set(topology->allowed_nodeset, nids[i]);
  }
  free(nids);

 out_with_cookie:
  lgrp_fini(cookie);
 out:
  return;
}

/* build all NUMAs (even if disallowed) and get global cpuset+nodeset using LGRP_VIEW_OS */
static void
lgrp_build_numanodes(struct hwloc_topology *topology,
		     lgrp_cookie_t cookie, lgrp_id_t root,
		     hwloc_obj_t *nodes, unsigned *nr_nodes)
{
  int npids, nnids;
  int i, j, n;
  processorid_t *pids;
  lgrp_id_t *nids;

  /* get the max number of PUs */
  npids = lgrp_cpus(cookie, root, NULL, 0, LGRP_CONTENT_HIERARCHY);
  if (npids < 0) {
    hwloc_debug("lgrp_cpus failed: %s\n", strerror(errno));
    goto out;
  }
  hwloc_debug("root lgrp contains %d PUs\n", npids);
  assert(npids > 0);

  /* allocate a single array that will be large enough for lgroup cpus below */
  pids = malloc(npids * sizeof(*pids));
  if (!pids)
    goto out;

  /* list NUMA nodes */
  nnids = lgrp_resources(cookie, root, NULL, 0, LGRP_RSRC_MEM);
  if (nnids < 0) {
    hwloc_debug("lgrp_resources failed: %s\n", strerror(errno));
    goto out_with_pids;
  }
  hwloc_debug("root lgrp contains %d NUMA nodes\n", nnids);
  assert(nnids > 0);

  nids = malloc(nnids * sizeof(*nids));
  if (!nids)
    goto out_with_pids;

  n = lgrp_resources(cookie, root, nids, nnids, LGRP_RSRC_MEM);
  assert(n == nnids);

  for(i=0; i<nnids; i++) {
    hwloc_obj_t obj;
    lgrp_mem_size_t mem_size;
    hwloc_debug("root lgrp contains NUMA node #%d = P#%ld\n", i, nids[i]);
    mem_size = lgrp_mem_size(cookie, nids[i], LGRP_MEM_SZ_INSTALLED, LGRP_CONTENT_DIRECT);
    /* or LGRP_MEM_SZ_FREE */

    obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_NUMANODE, (unsigned) nids[i]);
    obj->nodeset = hwloc_bitmap_alloc();
    hwloc_bitmap_set(obj->nodeset, nids[i]);
    obj->cpuset = hwloc_bitmap_alloc();
    nodes[(*nr_nodes)++] = obj;

    hwloc_debug("NUMA node %ld has %lldkB\n", nids[i], mem_size/1024);
    obj->attr->numanode.local_memory = mem_size;
    obj->attr->numanode.page_types_len = 2;
    obj->attr->numanode.page_types = malloc(2*sizeof(*obj->attr->numanode.page_types));
    memset(obj->attr->numanode.page_types, 0, 2*sizeof(*obj->attr->numanode.page_types));
    obj->attr->numanode.page_types[0].size = hwloc_getpagesize();
#if HAVE_DECL__SC_LARGE_PAGESIZE
    obj->attr->numanode.page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif

    n = lgrp_cpus(cookie, nids[i], pids, npids, LGRP_CONTENT_HIERARCHY);
    if (n < 0) {
      hwloc_debug("lgrp_cpus on NUMA node failed: %s\n", strerror(errno));
    } else {
      hwloc_debug("NUMA node %ld contains %d PUs\n", nids[i], n);
      for (j = 0; j < n ; j++) {
	hwloc_debug("node %ld's cpu %d is %d\n", nids[i], j, pids[j]);
	hwloc_bitmap_set(obj->cpuset, pids[j]);
      }
      hwloc_debug_1arg_bitmap("node %ld has cpuset %s\n",
			      nids[i], obj->cpuset);
    }

    hwloc_insert_object_by_cpuset(topology, obj);
  }
  topology->support.discovery->numa = 1;
  topology->support.discovery->numa_memory = 1;

 out_with_pids:
  free(pids);
 out:
  return;
}

static void
hwloc_look_lgrp(struct hwloc_topology *topology)
{
  lgrp_cookie_t cookie;
  unsigned curlgrp = 0;
  int nlgrps;
  lgrp_id_t root;

  lgrp_list_allowed(topology);

  cookie = lgrp_init(LGRP_VIEW_OS);
  if (cookie == LGRP_COOKIE_NONE)
    {
      hwloc_debug("lgrp_init failed: %s\n", strerror(errno));
      return;
    }
  nlgrps = lgrp_nlgrps(cookie);
  root = lgrp_root(cookie);
  if (nlgrps > 0) {
    hwloc_obj_t *glob_lgrps = calloc(nlgrps, sizeof(hwloc_obj_t));

    lgrp_build_numanodes(topology, cookie, root, glob_lgrps, &curlgrp);

#if HAVE_DECL_LGRP_LATENCY_COOKIE
    if (nlgrps > 1) {
      uint64_t *distances = calloc(curlgrp*curlgrp, sizeof(uint64_t));
      unsigned i, j;
      if (distances) {
	for (i = 0; i < curlgrp; i++)
	  for (j = 0; j < curlgrp; j++) {
	    int latency = lgrp_latency_cookie(cookie, glob_lgrps[i]->os_index, glob_lgrps[j]->os_index, LGRP_LAT_CPU_TO_MEM);
	    if (latency < 0) {
	      /* FIXME: if errno = ESRCH because some NUMA nodes are unavailable, we could reduce the matrix instead of ignoring */
	      free(distances);
	      goto done;
            }
            distances[i*curlgrp+j] = (uint64_t) latency;
        }
	hwloc_internal_distances_add(topology, curlgrp, glob_lgrps, distances,
				     HWLOC_DISTANCES_KIND_FROM_OS|HWLOC_DISTANCES_KIND_MEANS_LATENCY,
				     HWLOC_DISTANCES_ADD_FLAG_GROUP);
	glob_lgrps = NULL; /* dont free it below */
      }
    }
#endif /* HAVE_DECL_LGRP_LATENCY_COOKIE */
done:
    free(glob_lgrps);
  }
  lgrp_fini(cookie);
}
#endif /* LIBLGRP */

#ifdef HAVE_LIBKSTAT
#include <kstat.h>
static int
hwloc_look_kstat(struct hwloc_topology *topology)
{
  struct hwloc_solaris_chip_info_s chip_info;
  static char architecture[6] = "";
  int is_sparc = 0;
  int l1i_from_core = 0;
  int l1d_from_core = 0;
  int ret;

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

  ret = sysinfo(SI_ARCHITECTURE, architecture, sizeof architecture);
  if (ret == 6 && !strcmp(architecture, "sparc"))
    is_sparc = 1;

  hwloc_solaris_get_chip_info(&chip_info);

  /* mark unneeded caches as size -1 */
  if (!hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L1ICACHE))
    chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L1I] = -1;
  if (!hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L1CACHE))
    chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L1D] = -1;
  if (!hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L2ICACHE))
    chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L2I] = -1;
  if (!hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L2CACHE))
    chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L2D] = -1;
  if (!hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L3CACHE))
    chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L3] = -1;

  /* mark empty caches as unneeded on !sparc since we have the x86 backend to better get them. */
  if (!is_sparc) {
    for(i=0; i<sizeof(chip_info.cache_size)/sizeof(*chip_info.cache_size); i++)
      if (!chip_info.cache_size[i])
	chip_info.cache_size[i] = -1;
  }

  /* on sparc, assume l1d and l1i have same sharing as the core.
   * on !sparc, we don't know the sharing of these caches, hence we ignore them.
   * on x86, the x86-backend will take care of these caches again.
   */
  if (is_sparc && chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L1D] >= 0) {
    hwloc_debug("Will generate L1d caches from cores and PICL cache index #%u\n", HWLOC_SOLARIS_CHIP_INFO_L1D);
    l1d_from_core = 1;
  }
  if (is_sparc && chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L1I] >= 0) {
    hwloc_debug("Will generate L1i caches from cores and PICL cache index #%u\n", HWLOC_SOLARIS_CHIP_INFO_L1I);
    l1i_from_core = 1;
  }

  for (ksp = kc->kc_chain; ksp; ksp = ksp->ks_next) {
    if (!strncmp("cpu_info", ksp->ks_module, 8)) {
      cpuid = ksp->ks_instance;

      if (kstat_read(kc, ksp, NULL) == -1)
	{
	  fprintf(stderr, "kstat_read failed for CPU%u: %s\n", cpuid, strerror(errno));
	  continue;
	}

      hwloc_debug("cpu%u\n", cpuid);
      hwloc_bitmap_set(topology->levels[0][0]->complete_cpuset, cpuid);

      stat = (kstat_named_t *) kstat_data_lookup(ksp, "state");
      if (!stat)
          hwloc_debug("could not read state for CPU%u: %s\n", cpuid, strerror(errno));
      else if (stat->data_type != KSTAT_DATA_CHAR)
          hwloc_debug("unknown kstat type %d for cpu state\n", stat->data_type);
      else
        {
          hwloc_debug("cpu%u's state is %s\n", cpuid, stat->value.c);
          if (strcmp(stat->value.c, "on-line")) {
            /* Not online.
	     * It was marked as existing in complete_cpuset above, ignore everything else.
	     * We wouldn't get the all topology information about parents anyway.
	     */
	    continue;
	  }
        }

      if (cpuid >= Pproc_alloc) {
	struct hwloc_solaris_Pproc *tmp = realloc(Pproc, 2*Pproc_alloc * sizeof(*Pproc));
	if (!tmp)
	  goto err;
	Pproc = tmp;
	Pproc_alloc *= 2;
	for(i = Pproc_alloc/2; i < Pproc_alloc; i++) {
	  Pproc[i].Lproc = -1;
	  Pproc[i].Lpkg = -1;
	  Pproc[i].Ppkg = -1;
	  Pproc[i].Lcore = -1;
	}
      }
      Pproc[cpuid].Lproc = Lproc_num;

      if (Lproc_num >= Lproc_alloc) {
	struct hwloc_solaris_Lproc *tmp = realloc(Lproc, 2*Lproc_alloc * sizeof(*Lproc));
	if (!tmp)
	  goto err;
	Lproc = tmp;
	Lproc_alloc *= 2;
      }
      Lproc[Lproc_num].Pproc = cpuid;
      Lproc_num++;

      if (cpuid >= Pproc_max)
        Pproc_max = cpuid + 1;

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
	    fprintf(stderr, "chip_id type %u unknown\n", (unsigned) stat->data_type);
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
	    struct hwloc_solaris_Lpkg *tmp = realloc(Lpkg, 2*Lpkg_alloc * sizeof(*Lpkg));
	    if (!tmp)
	      goto err;
	    Lpkg = tmp;
	    Lpkg_alloc *= 2;
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
	    fprintf(stderr, "core_id type %u unknown\n", (unsigned) stat->data_type);
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
	    struct hwloc_solaris_Lcore *tmp = realloc(Lcore, 2*Lcore_alloc * sizeof(*Lcore));
	    if (!tmp)
	      goto err;
	    Lcore = tmp;
	    Lcore_alloc *= 2;
	  }
	  Lcore[Lcore_num].Ppkg = Pproc[cpuid].Ppkg;
	  Lcore[Lcore_num++].Pcore = coreid;
	}
      } while(0);

      /* Note: there is also clog_id for the Thread ID (not unique) and
       * pkg_core_id for the core ID (not unique).  They are not useful to us
       * however. */

    } else if (!strcmp("pg_hw_perf", ksp->ks_module)) {
      if (kstat_read(kc, ksp, NULL) == -1) {
	fprintf(stderr, "kstat_read failed for module %s name %s instance %d: %s\n", ksp->ks_module, ksp->ks_name, ksp->ks_instance, strerror(errno));
	continue;
      }
      stat = (kstat_named_t *) kstat_data_lookup(ksp, "cpus");
      if (stat) {
	hwloc_debug("found kstat module %s name %s instance %d cpus type %d\n", ksp->ks_module, ksp->ks_name, ksp->ks_instance, stat->data_type);
	if (stat->data_type == KSTAT_DATA_STRING) {
	  hwloc_bitmap_t cpuset = hwloc_bitmap_alloc();
	  hwloc_bitmap_list_sscanf(cpuset, stat->value.str.addr.ptr);

	  if (!strcmp(ksp->ks_name, "L3_Cache")) {
	    if (chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L3] >= 0) {
	      hwloc_obj_t l3 = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L3CACHE, HWLOC_UNKNOWN_INDEX);
	      l3->cpuset = cpuset;
	      l3->attr->cache.depth = 3;
	      l3->attr->cache.size = chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L3];
	      l3->attr->cache.linesize = chip_info.cache_linesize[HWLOC_SOLARIS_CHIP_INFO_L3];
	      l3->attr->cache.associativity = chip_info.cache_associativity[HWLOC_SOLARIS_CHIP_INFO_L3];
	      l3->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
	      hwloc_insert_object_by_cpuset(topology, l3);
	      cpuset = NULL; /* don't free below */
	    }
	  }
	  else if (!strcmp(ksp->ks_name, "L2_Cache")) {
	    if (!chip_info.l2_unified && chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L2I] >= 0) {
	      hwloc_obj_t l2i = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L2ICACHE, HWLOC_UNKNOWN_INDEX);
	      l2i->cpuset = hwloc_bitmap_dup(cpuset);
	      l2i->attr->cache.depth = 2;
	      l2i->attr->cache.size = chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L2I];
	      l2i->attr->cache.linesize = chip_info.cache_linesize[HWLOC_SOLARIS_CHIP_INFO_L2I];
	      l2i->attr->cache.associativity = chip_info.cache_associativity[HWLOC_SOLARIS_CHIP_INFO_L2I];
	      l2i->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
	      hwloc_insert_object_by_cpuset(topology, l2i);
	    }
	    if (chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L2D] >= 0) {
	      hwloc_obj_t l2 = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L2CACHE, HWLOC_UNKNOWN_INDEX);
	      l2->cpuset = cpuset;
	      l2->attr->cache.depth = 2;
	      l2->attr->cache.size = chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L2D];
	      l2->attr->cache.linesize = chip_info.cache_linesize[HWLOC_SOLARIS_CHIP_INFO_L2D];
	      l2->attr->cache.associativity = chip_info.cache_associativity[HWLOC_SOLARIS_CHIP_INFO_L2D];
	      l2->attr->cache.type = chip_info.l2_unified ? HWLOC_OBJ_CACHE_UNIFIED : HWLOC_OBJ_CACHE_DATA;
	      hwloc_insert_object_by_cpuset(topology, l2);
	      cpuset = NULL; /* don't free below */
	    }
	  }
	  else if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_GROUP)) {
	    hwloc_obj_t group = hwloc_alloc_setup_object(topology, HWLOC_OBJ_GROUP, HWLOC_UNKNOWN_INDEX);
	    group->cpuset = cpuset;
	    group->attr->group.kind = HWLOC_GROUP_KIND_SOLARIS_PG_HW_PERF;
	    group->attr->group.subkind = hwloc_bitmap_weight(cpuset);
	    if (ksp->ks_name[0])
	      hwloc_obj_add_info(group, "SolarisProcessorGroup", ksp->ks_name);
	    hwloc_insert_object_by_cpuset(topology, group);
	    cpuset = NULL; /* don't free below */
	  }
	  hwloc_bitmap_free(cpuset);
	}
      }
    }
  }

  if (look_chips
      && hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_PACKAGE)) {
    struct hwloc_obj *obj;
    unsigned j,k;
    hwloc_debug("%u Packages\n", Lpkg_num);
    for (j = 0; j < Lpkg_num; j++) {
      obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_PACKAGE, Lpkg[j].Ppkg);
      if (chip_info.type && chip_info.type[0])
	hwloc_obj_add_info(obj, "CPUType", chip_info.type);
      if (chip_info.model && chip_info.model[0])
	hwloc_obj_add_info(obj, "CPUModel", chip_info.model);
      obj->cpuset = hwloc_bitmap_alloc();
      for(k=0; k<Pproc_max; k++)
	if (Pproc[k].Lpkg == j)
	  hwloc_bitmap_set(obj->cpuset, k);
      hwloc_debug_1arg_bitmap("Package %u has cpuset %s\n", j, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    hwloc_debug("%s", "\n");
  }

  if (look_cores || l1i_from_core || l1d_from_core) {
    unsigned j;
    hwloc_debug("%u Cores\n", Lcore_num);
    for (j = 0; j < Lcore_num; j++) {
      /* Build the core cpuset */
      unsigned k;
      hwloc_bitmap_t cpuset = hwloc_bitmap_alloc();
      for(k=0; k<Pproc_max; k++)
	if (Pproc[k].Lcore == j)
	  hwloc_bitmap_set(cpuset, k);
      hwloc_debug_1arg_bitmap("Core %u has cpuset %s\n", j, cpuset);

      /* Sparcs have per-core L1's. If we got their sizes from PICL, create those objects.
       *
       * On x86, let the x86 backend handle things.
       * At least AMD Fam15h L1i isn't per core (shared by dual-core compute unit).
       */
      if (l1d_from_core) {
	struct hwloc_obj *l1 = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L1CACHE, HWLOC_UNKNOWN_INDEX);
	l1->cpuset = hwloc_bitmap_dup(cpuset);
	l1->attr->cache.depth = 1;
	l1->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
	l1->attr->cache.size = chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L1D];
	l1->attr->cache.linesize = chip_info.cache_linesize[HWLOC_SOLARIS_CHIP_INFO_L1D];
	l1->attr->cache.associativity = chip_info.cache_associativity[HWLOC_SOLARIS_CHIP_INFO_L1D];
	hwloc_insert_object_by_cpuset(topology, l1);
      }
      if (l1i_from_core) {
	struct hwloc_obj *l1i = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L1ICACHE, HWLOC_UNKNOWN_INDEX);
	l1i->cpuset = hwloc_bitmap_dup(cpuset);
	l1i->attr->cache.depth = 1;
	l1i->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
	l1i->attr->cache.size = chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L1I];
	l1i->attr->cache.linesize = chip_info.cache_linesize[HWLOC_SOLARIS_CHIP_INFO_L1I];
	l1i->attr->cache.associativity = chip_info.cache_associativity[HWLOC_SOLARIS_CHIP_INFO_L1I];
	hwloc_insert_object_by_cpuset(topology, l1i);
      }
      if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_CORE)) {
	struct hwloc_obj *obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_CORE, Lcore[j].Pcore);
	obj->cpuset = cpuset;
	hwloc_insert_object_by_cpuset(topology, obj);
      } else {
	hwloc_bitmap_free(cpuset);
      }
    }
    hwloc_debug("%s", "\n");
  }

  if (Lproc_num) {
    struct hwloc_obj *obj;
    unsigned j,k;
    hwloc_debug("%u PUs\n", Lproc_num);
    for (j = 0; j < Lproc_num; j++) {
      obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_PU, Lproc[j].Pproc);
      obj->cpuset = hwloc_bitmap_alloc();
      for(k=0; k<Pproc_max; k++)
	if (Pproc[k].Lproc == j)
	  hwloc_bitmap_set(obj->cpuset, k);
      hwloc_debug_1arg_bitmap("PU %u has cpuset %s\n", j, obj->cpuset);
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    hwloc_debug("%s", "\n");
    topology->support.discovery->pu = 1;
  }

  kstat_close(kc);

  free(Pproc);
  free(Lproc);
  free(Lcore);
  free(Lpkg);
  return Lproc_num > 0;

 err:
  kstat_close(kc);

  free(Pproc);
  free(Lproc);
  free(Lcore);
  free(Lpkg);
  return 0;
}
#endif /* LIBKSTAT */

static int
hwloc_look_solaris(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  int alreadypus = 0;

  if (topology->levels[0][0]->cpuset)
    /* somebody discovered things */
    return -1;

  hwloc_alloc_root_sets(topology->levels[0][0]);

#ifdef HAVE_LIBLGRP
  hwloc_look_lgrp(topology);
#endif /* HAVE_LIBLGRP */
#ifdef HAVE_LIBKSTAT
  if (hwloc_look_kstat(topology) > 0)
    alreadypus = 1;
#endif /* HAVE_LIBKSTAT */

  if (!alreadypus) {
    int nbprocs = hwloc_fallback_nbprocessors (topology);
    if (nbprocs >= 1)
      topology->support.discovery->pu = 1;
    else
      nbprocs = 1;
    hwloc_setup_pu_level(topology, nbprocs);
  }

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "Solaris");
  hwloc_add_uname_info(topology, NULL);
  return 0;
}

#ifdef HAVE_LIBLGRP
static int hwloc_solaris_get_allowed_hook(hwloc_topology_t topology)
{
  lgrp_list_allowed(topology);
  return 0;
}
#endif

static int
hwloc_solaris_get_thisthread_last_cpu_location(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  int pu = getcpuid();
  hwloc_bitmap_only(hwloc_set, pu);
  return 0;
}

void
hwloc_set_solaris_hooks(struct hwloc_binding_hooks *hooks,
			struct hwloc_topology_support *support __hwloc_attribute_unused)
{
  hooks->set_proc_cpubind = hwloc_solaris_set_proc_cpubind;
  hooks->set_thisproc_cpubind = hwloc_solaris_set_thisproc_cpubind;
  hooks->set_thisthread_cpubind = hwloc_solaris_set_thisthread_cpubind;
  hooks->get_thisthread_last_cpu_location = hwloc_solaris_get_thisthread_last_cpu_location;
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
#ifdef HAVE_LIBLGRP
  hooks->get_allowed_resources = hwloc_solaris_get_allowed_hook;
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
  1,
  NULL
};

const struct hwloc_component hwloc_solaris_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_solaris_disc_component
};
