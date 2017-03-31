/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2016 Inria.  All rights reserved.
 * Copyright © 2009-2011, 2013 Université Bordeaux
 * Copyright © 2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/* TODO: use SIGRECONFIG & dr_reconfig for state change */

#include <private/autogen/config.h>

#include <sys/types.h>
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <hwloc.h>
#include <private/private.h>
#include <private/misc.h>
#include <private/debug.h>

#include <procinfo.h>
#include <sys/types.h>
#include <sys/rset.h>
#include <sys/processor.h>
#include <sys/thread.h>
#include <sys/mman.h>
#include <sys/systemcfg.h>

#ifndef __power_pc
#define __power_pc() 0
#endif
#ifndef __power_4
#define __power_4() 0
#endif
#ifndef __power_5
#define __power_5() 0
#endif
#ifndef __power_6
#define __power_6() 0
#endif
#ifndef __power_7
#define __power_7() 0
#endif

static int
hwloc_aix_set_sth_cpubind(hwloc_topology_t topology, rstype_t what, rsid_t who, pid_t pid, hwloc_const_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  rsethandle_t rad;
  int res;
  unsigned cpu;

  if (flags & HWLOC_CPUBIND_NOMEMBIND) {
    errno = ENOSYS;
    return -1;
  }

  /* The resulting binding is always strict */

  if (hwloc_bitmap_isequal(hwloc_set, hwloc_topology_get_complete_cpuset(topology))) {
    if (ra_detachrset(what, who, 0))
      return -1;
    return 0;
  }

  rad = rs_alloc(RS_EMPTY);
  hwloc_bitmap_foreach_begin(cpu, hwloc_set)
    rs_op(RS_ADDRESOURCE, rad, NULL, R_PROCS, cpu);
  hwloc_bitmap_foreach_end();

  res = ra_attachrset(what, who, rad, 0);
  if (res < 0 && errno == EPERM) {
    /* EPERM may mean that one thread has ben bound with bindprocessor().
     * Unbind the entire process (we can't unbind individual threads)
     * and try again.
     */
    bindprocessor(BINDPROCESS, pid, PROCESSOR_CLASS_ANY);
    res = ra_attachrset(what, who, rad, 0);
  }

  rs_free(rad);
  return res;
}

static int
hwloc_aix_get_sth_rset_cpubind(hwloc_topology_t topology, rstype_t what, rsid_t who, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused, int *boundp)
{
  rsethandle_t rset;
  unsigned cpu, maxcpus;
  int res = -1;
  int bound = 0;

  rset = rs_alloc(RS_EMPTY);

  if (ra_getrset(what, who, 0, rset) == -1)
    goto out;

  hwloc_bitmap_zero(hwloc_set);
  maxcpus = rs_getinfo(rset, R_MAXPROCS, 0);
  for (cpu = 0; cpu < maxcpus; cpu++)
    if (rs_op(RS_TESTRESOURCE, rset, NULL, R_PROCS, cpu) == 1)
      hwloc_bitmap_set(hwloc_set, cpu);
    else
      bound = 1;
  hwloc_bitmap_and(hwloc_set, hwloc_set, hwloc_topology_get_complete_cpuset(topology));
  res = 0;
  *boundp = bound;

out:
  rs_free(rset);
  return res;
}

static int
hwloc_aix_get_pid_getthrds_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, pid_t pid, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
#if HWLOC_BITS_PER_LONG == 64
  struct thrdentry64 thread_info;
  tid64_t next_thread;
#else
  struct thrdsinfo thread_info;
  tid_t next_thread;
#endif

  next_thread = 0;
  /* TODO: get multiple at once */
#if HWLOC_BITS_PER_LONG == 64
  while (getthrds64 (pid, &thread_info, sizeof (thread_info),
                     &next_thread, 1) == 1) {
#else
  while (getthrds   (pid, &thread_info, sizeof (thread_info),
                     &next_thread, 1) == 1) {
#endif
    if (PROCESSOR_CLASS_ANY != thread_info.ti_cpuid)
      hwloc_bitmap_set(hwloc_set, thread_info.ti_cpuid);
    else
      hwloc_bitmap_fill(hwloc_set);
  }
  /* TODO: what if the thread list changes and we get nothing? */

  return 0;
}

static int
hwloc_aix_get_tid_getthrds_cpubind(hwloc_topology_t topology __hwloc_attribute_unused, tid_t tid, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
#if HWLOC_BITS_PER_LONG == 64
  struct thrdentry64 thread_info;
  tid64_t next_thread;
#else
  struct thrdsinfo thread_info;
  tid_t next_thread;
#endif
  pid_t pid = getpid();

  next_thread = 0;
  /* TODO: get multiple at once */
#if HWLOC_BITS_PER_LONG == 64
  while (getthrds64 (pid, &thread_info, sizeof (thread_info),
                     &next_thread, 1) == 1) {
#else
  while (getthrds   (pid, &thread_info, sizeof (thread_info),
                     &next_thread, 1) == 1) {
#endif
    if (thread_info.ti_tid == tid) {
      if (PROCESSOR_CLASS_ANY != thread_info.ti_cpuid)
	hwloc_bitmap_set(hwloc_set, thread_info.ti_cpuid);
      else
	hwloc_bitmap_fill(hwloc_set);
      break;
    }
  }
  /* TODO: what if the thread goes away in the meantime? */

  return 0;
}

static int
hwloc_aix_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags)
{
  rsid_t who;
  who.at_pid = getpid();
  return hwloc_aix_set_sth_cpubind(topology, R_PROCESS, who, who.at_pid, hwloc_set, flags);
}

static int
hwloc_aix_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags)
{
  int ret, bound;
  rsid_t who;
  who.at_pid = getpid();
  ret = hwloc_aix_get_sth_rset_cpubind(topology, R_PROCESS, who, hwloc_set, flags, &bound);
  if (!ret && !bound) {
    hwloc_bitmap_zero(hwloc_set);
    ret = hwloc_aix_get_pid_getthrds_cpubind(topology, who.at_pid, hwloc_set, flags);
  }
  return ret;
}

#ifdef R_THREAD
static int
hwloc_aix_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags)
{
  rsid_t who;
  who.at_tid = thread_self();
  return hwloc_aix_set_sth_cpubind(topology, R_THREAD, who, getpid(), hwloc_set, flags);
}

static int
hwloc_aix_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags)
{
  int ret, bound;
  rsid_t who;
  who.at_tid = thread_self();
  ret = hwloc_aix_get_sth_rset_cpubind(topology, R_THREAD, who, hwloc_set, flags, &bound);
  if (!ret && !bound) {
    hwloc_bitmap_zero(hwloc_set);
    ret = hwloc_aix_get_tid_getthrds_cpubind(topology, who.at_tid, hwloc_set, flags);
  }
  return ret;
}
#endif /* R_THREAD */

static int
hwloc_aix_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_bitmap_t hwloc_set, int flags)
{
  rsid_t who;
  who.at_pid = pid;
  return hwloc_aix_set_sth_cpubind(topology, R_PROCESS, who, pid, hwloc_set, flags);
}

static int
hwloc_aix_get_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_bitmap_t hwloc_set, int flags)
{
  int ret, bound;
  rsid_t who;
  who.at_pid = pid;
  ret = hwloc_aix_get_sth_rset_cpubind(topology, R_PROCESS, who, hwloc_set, flags, &bound);
  if (!ret && !bound) {
    hwloc_bitmap_zero(hwloc_set);
    ret = hwloc_aix_get_pid_getthrds_cpubind(topology, who.at_pid, hwloc_set, flags);
  }
  return ret;
}

#ifdef R_THREAD
#ifdef HWLOC_HAVE_PTHREAD_GETTHRDS_NP
static int
hwloc_aix_set_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t pthread, hwloc_const_bitmap_t hwloc_set, int flags)
{
  struct __pthrdsinfo info;
  int size;
  if ((errno = pthread_getthrds_np(&pthread, PTHRDSINFO_QUERY_TID, &info, sizeof(info), NULL, &size)))
    return -1;
  {
    rsid_t who;
    who.at_tid = info.__pi_tid;
    return hwloc_aix_set_sth_cpubind(topology, R_THREAD, who, getpid(), hwloc_set, flags);
  }
}

static int
hwloc_aix_get_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t pthread, hwloc_bitmap_t hwloc_set, int flags)
{
  struct __pthrdsinfo info;
  int size;
  if (pthread_getthrds_np(&pthread, PTHRDSINFO_QUERY_TID, &info, sizeof(info), NULL, &size))
    return -1;
  {
    int ret, bound;
    rsid_t who;
    who.at_tid = info.__pi_tid;
    ret = hwloc_aix_get_sth_rset_cpubind(topology, R_THREAD, who, hwloc_set, flags, &bound);
    if (!ret && !bound) {
      hwloc_bitmap_zero(hwloc_set);
      ret = hwloc_aix_get_tid_getthrds_cpubind(topology, who.at_tid, hwloc_set, flags);
    }
    return ret;
  }
}
#endif /* HWLOC_HAVE_PTHREAD_GETTHRDS_NP */
#endif /* R_THREAD */

static int
hwloc_aix_get_thisthread_last_cpu_location(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  cpu_t cpu;

  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }

  cpu = mycpu();
  if (cpu < 0)
    return -1;

  hwloc_bitmap_only(hwloc_set, cpu);
  return 0;
}

#ifdef P_DEFAULT

static int
hwloc_aix_membind_policy_from_hwloc(uint_t *aix_policy, int policy)
{
  switch (policy) {
    case HWLOC_MEMBIND_DEFAULT:
    case HWLOC_MEMBIND_BIND:
      *aix_policy = P_DEFAULT;
      break;
    case HWLOC_MEMBIND_FIRSTTOUCH:
      *aix_policy = P_FIRST_TOUCH;
      break;
    case HWLOC_MEMBIND_INTERLEAVE:
      *aix_policy = P_BALANCED;
      break;
    default:
      errno = ENOSYS;
      return -1;
  }
  return 0;
}

static int
hwloc_aix_prepare_membind(hwloc_topology_t topology, rsethandle_t *rad, hwloc_const_nodeset_t nodeset, int flags __hwloc_attribute_unused)
{
  rsethandle_t rset, noderad;
  int MCMlevel;
  int node;

  MCMlevel = rs_getinfo(NULL, R_MCMSDL, 0);
  if ((topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM))
    rset = rs_alloc(RS_ALL);
  else
    rset = rs_alloc(RS_PARTITION);
  *rad = rs_alloc(RS_EMPTY);
  noderad = rs_alloc(RS_EMPTY);

  hwloc_bitmap_foreach_begin(node, nodeset)
    /* we used MCMlevel rad number for node->os_index during lookup */
    rs_getrad(rset, noderad, MCMlevel, node, 0);
    rs_op(RS_UNION, noderad, *rad, 0, 0);
  hwloc_bitmap_foreach_end();

  rs_free(rset);
  rs_free(noderad);

  return 0;
}

static int
hwloc_aix_set_sth_membind(hwloc_topology_t topology, rstype_t what, rsid_t who, pid_t pid, hwloc_const_bitmap_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  rsethandle_t rad;
  int res;

  if (flags & HWLOC_MEMBIND_NOCPUBIND) {
    errno = ENOSYS;
    return -1;
  }

  switch (policy) {
    case HWLOC_MEMBIND_DEFAULT:
    case HWLOC_MEMBIND_BIND:
      break;
    default:
      errno = ENOSYS;
      return -1;
  }

  if (hwloc_aix_prepare_membind(topology, &rad, nodeset, flags))
    return -1;

  res = ra_attachrset(what, who, rad, 0);
  if (res < 0 && errno == EPERM) {
    /* EPERM may mean that one thread has ben bound with bindprocessor().
     * Unbind the entire process (we can't unbind individual threads)
     * and try again.
     */
    bindprocessor(BINDPROCESS, pid, PROCESSOR_CLASS_ANY);
    res = ra_attachrset(what, who, rad, 0);
  }

  rs_free(rad);
  return res;
}

static int
hwloc_aix_get_sth_membind(hwloc_topology_t topology, rstype_t what, rsid_t who, hwloc_bitmap_t nodeset, hwloc_membind_policy_t *policy, int flags __hwloc_attribute_unused)
{
  hwloc_bitmap_t hwloc_set;
  rsethandle_t rset;
  unsigned cpu, maxcpus;
  int res = -1;
  int depth, n, i;

  depth = hwloc_get_type_depth(topology, HWLOC_OBJ_NUMANODE);
  if (depth < 0) {
    errno = EXDEV;
    return -1;
  }
  n = hwloc_get_nbobjs_by_depth(topology, depth);

  rset = rs_alloc(RS_EMPTY);

  if (ra_getrset(what, who, 0, rset) == -1)
    goto out;

  hwloc_set = hwloc_bitmap_alloc();

  maxcpus = rs_getinfo(rset, R_MAXPROCS, 0);
  for (cpu = 0; cpu < maxcpus; cpu++)
    if (rs_op(RS_TESTRESOURCE, rset, NULL, R_PROCS, cpu) == 1)
      hwloc_bitmap_set(hwloc_set, cpu);
  hwloc_bitmap_and(hwloc_set, hwloc_set, hwloc_topology_get_complete_cpuset(topology));

  hwloc_bitmap_zero(nodeset);
  for (i = 0; i < n; i++) {
    hwloc_obj_t obj = hwloc_get_obj_by_depth(topology, depth, i);
    if (hwloc_bitmap_isincluded(obj->cpuset, hwloc_set))
      hwloc_bitmap_set(nodeset, obj->os_index);
  }

  hwloc_bitmap_free(hwloc_set);

  *policy = HWLOC_MEMBIND_BIND;
  res = 0;

out:
  rs_free(rset);
  return res;
}

static int
hwloc_aix_set_thisproc_membind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, hwloc_membind_policy_t policy, int flags)
{
  rsid_t who;
  who.at_pid = getpid();
  return hwloc_aix_set_sth_membind(topology, R_PROCESS, who, who.at_pid, hwloc_set, policy, flags);
}

static int
hwloc_aix_get_thisproc_membind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, hwloc_membind_policy_t *policy, int flags)
{
  rsid_t who;
  who.at_pid = getpid();
  return hwloc_aix_get_sth_membind(topology, R_PROCESS, who, hwloc_set, policy, flags);
}

#ifdef R_THREAD
static int
hwloc_aix_set_thisthread_membind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, hwloc_membind_policy_t policy, int flags)
{
  rsid_t who;
  who.at_tid = thread_self();
  return hwloc_aix_set_sth_membind(topology, R_THREAD, who, getpid(), hwloc_set, policy, flags);
}

static int
hwloc_aix_get_thisthread_membind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, hwloc_membind_policy_t *policy, int flags)
{
  rsid_t who;
  who.at_tid = thread_self();
  return hwloc_aix_get_sth_membind(topology, R_THREAD, who, hwloc_set, policy, flags);
}
#endif /* R_THREAD */

static int
hwloc_aix_set_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_bitmap_t hwloc_set, hwloc_membind_policy_t policy, int flags)
{
  rsid_t who;
  who.at_pid = pid;
  return hwloc_aix_set_sth_membind(topology, R_PROCESS, who, pid, hwloc_set, policy, flags);
}

static int
hwloc_aix_get_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_bitmap_t hwloc_set, hwloc_membind_policy_t *policy, int flags)
{
  rsid_t who;
  who.at_pid = pid;
  return hwloc_aix_get_sth_membind(topology, R_PROCESS, who, hwloc_set, policy, flags);
}

#ifdef R_THREAD
#if 0 /* def HWLOC_HAVE_PTHREAD_GETTHRDS_NP */
static int
hwloc_aix_set_thread_membind(hwloc_topology_t topology, hwloc_thread_t pthread, hwloc_const_bitmap_t hwloc_set, hwloc_membind_policy_t policy, int flags)
{
  struct __pthrdsinfo info;
  int size;
  if ((errno = pthread_getthrds_np(&pthread, PTHRDSINFO_QUERY_TID, &info, sizeof(info), NULL, &size)))
    return -1;
  {
    rsid_t who;
    who.at_tid = info.__pi_tid;
    return hwloc_aix_set_sth_membind(topology, R_THREAD, who, getpid(), hwloc_set, policy, flags);
  }
}

static int
hwloc_aix_get_thread_membind(hwloc_topology_t topology, hwloc_thread_t pthread, hwloc_bitmap_t hwloc_set, hwloc_membind_policy_t *policy, int flags)
{
  struct __pthrdsinfo info;
  int size;
  if (pthread_getthrds_np(&pthread, PTHRDSINFO_QUERY_TID, &info, sizeof(info), NULL, &size))
    return -1;
  {
    rsid_t who;
    who.at_tid = info.__pi_tid;
    return hwloc_aix_get_sth_membind(topology, R_THREAD, who, hwloc_set, policy, flags);
  }
}
#endif /* HWLOC_HAVE_PTHREAD_GETTHRDS_NP */
#endif /* R_THREAD */

#if 0
/* TODO: seems to be right, but doesn't seem to be working (EINVAL), even after
 * aligning the range on 64K... */
static int
hwloc_aix_set_area_membind(hwloc_topology_t topology, const void *addr, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  subrange_t subrange;
  rsid_t rsid = { .at_subrange = &subrange };
  uint_t aix_policy;
  int ret;
  fprintf(stderr,"yop\n");

  if ((flags & (HWLOC_MEMBIND_MIGRATE|HWLOC_MEMBIND_STRICT))
            == (HWLOC_MEMBIND_MIGRATE|HWLOC_MEMBIND_STRICT)) {
    errno = ENOSYS;
    return -1;
  }

  subrange.su_offset = (uintptr_t) addr;
  subrange.su_length = len;
  subrange.su_rstype = R_RSET;

  if (hwloc_aix_membind_policy_from_hwloc(&aix_policy, policy))
    return -1;

  if (hwloc_aix_prepare_membind(topology, &subrange.su_rsid.at_rset, nodeset, flags))
    return -1;

  subrange.su_policy = aix_policy;

  res = ra_attachrset(R_SUBRANGE, rsid, subrange.su_rsid.at_rset, 0);
  if (res < 0 && errno == EPERM) {
    /* EPERM may mean that one thread has ben bound with bindprocessor().
     * Unbind the entire process (we can't unbind individual threads)
     * and try again.
     * FIXME: actually check that this EPERM can happen
     */
    bindprocessor(BINDPROCESS, getpid(), PROCESSOR_CLASS_ANY);
    res = ra_attachrset(R_SUBRANGE, rsid, subrange.su_rsid.at_rset, 0);
  }

  rs_free(subrange.su_rsid.at_rset);
  return ret;
}
#endif

static void *
hwloc_aix_alloc_membind(hwloc_topology_t topology, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags)
{
  void *ret;
  rsid_t rsid;
  uint_t aix_policy;

  if (hwloc_aix_membind_policy_from_hwloc(&aix_policy, policy))
    return hwloc_alloc_or_fail(topology, len, flags);

  if (hwloc_aix_prepare_membind(topology, &rsid.at_rset, nodeset, flags))
    return hwloc_alloc_or_fail(topology, len, flags);

  ret = ra_mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0, R_RSET, rsid, aix_policy);

  rs_free(rsid.at_rset);
  return ret == (void*)-1 ? NULL : ret;
}
#endif /* P_DEFAULT */

static void
look_rset(int sdl, hwloc_obj_type_t type, struct hwloc_topology *topology, int level)
{
  rsethandle_t rset, rad;
  int i,maxcpus,j;
  int nbnodes;
  struct hwloc_obj *obj;

  if ((topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM))
    rset = rs_alloc(RS_ALL);
  else
    rset = rs_alloc(RS_PARTITION);
  rad = rs_alloc(RS_EMPTY);
  nbnodes = rs_numrads(rset, sdl, 0);
  if (nbnodes == -1) {
    perror("rs_numrads");
    return;
  }

  for (i = 0; i < nbnodes; i++) {
    hwloc_bitmap_t cpuset;
    unsigned os_index = (unsigned) -1; /* no os_index except for PU and NUMANODE below */

    if (rs_getrad(rset, rad, sdl, i, 0)) {
      fprintf(stderr,"rs_getrad(%d) failed: %s\n", i, strerror(errno));
      continue;
    }
    if (!rs_getinfo(rad, R_NUMPROCS, 0))
      continue;

    maxcpus = rs_getinfo(rad, R_MAXPROCS, 0);
    cpuset = hwloc_bitmap_alloc();
    for (j = 0; j < maxcpus; j++) {
      if (rs_op(RS_TESTRESOURCE, rad, NULL, R_PROCS, j))
	hwloc_bitmap_set(cpuset, j);
    }

    if (type == HWLOC_OBJ_PU) {
      os_index = hwloc_bitmap_first(cpuset);
      hwloc_debug("Found PU #%u inside node %d for sdl %d\n", os_index, i, sdl);
      assert(hwloc_bitmap_weight(cpuset) == 1);
    } else if (type == HWLOC_OBJ_NUMANODE) {
      /* NUMA node os_index isn't used for binding, just use the rad number to get unique values.
       * Note that we'll use that fact in hwloc_aix_prepare_membind(). */
      os_index = i;
      hwloc_debug("Using os_index #%u for NUMA node inside node %d for sdl %d\n", os_index, i, sdl);
    }

    obj = hwloc_alloc_setup_object(type, os_index);
    obj->cpuset = cpuset;
    obj->os_level = sdl;

    switch(type) {
      case HWLOC_OBJ_NUMANODE:
	obj->nodeset = hwloc_bitmap_alloc();
	hwloc_bitmap_set(obj->nodeset, i);
	obj->memory.local_memory = 0; /* TODO: odd, rs_getinfo(rad, R_MEMSIZE, 0) << 10 returns the total memory ... */
	obj->memory.page_types_len = 2;
	obj->memory.page_types = malloc(2*sizeof(*obj->memory.page_types));
	memset(obj->memory.page_types, 0, 2*sizeof(*obj->memory.page_types));
	obj->memory.page_types[0].size = hwloc_getpagesize();
#if HAVE_DECL__SC_LARGE_PAGESIZE
	obj->memory.page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
	/* TODO: obj->memory.page_types[1].count = rs_getinfo(rset, R_LGPGFREE, 0) / hugepagesize */
	break;
      case HWLOC_OBJ_CACHE:
	obj->attr->cache.size = _system_configuration.L2_cache_size;
	obj->attr->cache.associativity = _system_configuration.L2_cache_asc;

	obj->attr->cache.linesize = 0; /* unknown by default */
	if (__power_pc())
	  if (__power_4() || __power_5() || __power_6() || __power_7())
	    obj->attr->cache.linesize = 128;

	obj->attr->cache.depth = 2;
	obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED; /* OK for power[4567], unknown for others */
	break;
      case HWLOC_OBJ_GROUP:
	obj->attr->group.depth = level;
	break;
      case HWLOC_OBJ_CORE:
      {
	hwloc_obj_t obj2, obj3;
	obj2 = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, i);
	obj2->cpuset = hwloc_bitmap_dup(obj->cpuset);
	obj2->attr->cache.size = _system_configuration.dcache_size;
	obj2->attr->cache.associativity = _system_configuration.dcache_asc;
	obj2->attr->cache.linesize = _system_configuration.dcache_line;
	obj2->attr->cache.depth = 1;
	if (_system_configuration.cache_attrib & (1<<30)) {
	  /* Unified cache */
	  obj2->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
	  hwloc_debug("Adding an L1u cache for core %d\n", i);
	  hwloc_insert_object_by_cpuset(topology, obj2);
	} else {
	  /* Separate Instruction and Data caches */
	  obj2->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
	  hwloc_debug("Adding an L1d cache for core %d\n", i);
	  hwloc_insert_object_by_cpuset(topology, obj2);

	  obj3 = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, i);
	  obj3->cpuset = hwloc_bitmap_dup(obj->cpuset);
	  obj3->attr->cache.size = _system_configuration.icache_size;
	  obj3->attr->cache.associativity = _system_configuration.icache_asc;
	  obj3->attr->cache.linesize = _system_configuration.icache_line;
	  obj3->attr->cache.depth = 1;
	  obj3->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
	  hwloc_debug("Adding an L1i cache for core %d\n", i);
	  hwloc_insert_object_by_cpuset(topology, obj3);
	}
	break;
      }
      default:
	break;
    }
    hwloc_debug_2args_bitmap("%s %d has cpuset %s\n",
	       hwloc_obj_type_string(type),
	       i, obj->cpuset);
    hwloc_insert_object_by_cpuset(topology, obj);
  }

  rs_free(rset);
  rs_free(rad);
}

static int
hwloc_look_aix(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  int i;

  if (topology->levels[0][0]->cpuset)
    /* somebody discovered things */
    return 0;

  hwloc_alloc_obj_cpusets(topology->levels[0][0]);

  /* TODO: R_LGPGDEF/R_LGPGFREE for large pages */

  hwloc_debug("Note: SMPSDL is at %d\n", rs_getinfo(NULL, R_SMPSDL, 0));
#ifdef R_REF1SDL
  hwloc_debug("Note: REF1SDL is at %d\n", rs_getinfo(NULL, R_REF1SDL, 0));
#endif

  for (i=0; i<=rs_getinfo(NULL, R_MAXSDL, 0); i++)
    {
      int known = 0;
#if 0
      if (i == rs_getinfo(NULL, R_SMPSDL, 0))
	/* Not enabled for now because I'm not sure what it corresponds to. On
	 * decrypthon it contains all the cpus. Is it a "machine" or a "system"
	 * level ?
	 */
	{
	  hwloc_debug("looking AIX \"SMP\" sdl %d\n", i);
	  look_rset(i, HWLOC_OBJ_MACHINE, topology, i);
	  known = 1;
	}
#endif
      if (i == rs_getinfo(NULL, R_MCMSDL, 0))
	{
	  hwloc_debug("looking AIX node sdl %d\n", i);
	  look_rset(i, HWLOC_OBJ_NUMANODE, topology, i);
	  known = 1;
	}
#      ifdef R_L2CSDL
      if (i == rs_getinfo(NULL, R_L2CSDL, 0))
	{
	  hwloc_debug("looking AIX L2 sdl %d\n", i);
	  look_rset(i, HWLOC_OBJ_CACHE, topology, i);
	  known = 1;
	}
#      endif
#      ifdef R_PCORESDL
      if (i == rs_getinfo(NULL, R_PCORESDL, 0))
	{
	  hwloc_debug("looking AIX core sdl %d\n", i);
	  look_rset(i, HWLOC_OBJ_CORE, topology, i);
	  known = 1;
	}
#      endif
      if (i == rs_getinfo(NULL, R_MAXSDL, 0))
	{
	  hwloc_debug("looking AIX max sdl %d\n", i);
	  look_rset(i, HWLOC_OBJ_PU, topology, i);
	  known = 1;
          topology->support.discovery->pu = 1;
	}

      /* Don't know how it should be rendered, make a misc object for it.  */
      if (!known)
	{
	  hwloc_debug("looking AIX unknown sdl %d\n", i);
	  look_rset(i, HWLOC_OBJ_GROUP, topology, i);
	}
    }

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "AIX");
  if (topology->is_thissystem)
    hwloc_add_uname_info(topology, NULL);
  return 1;
}

void
hwloc_set_aix_hooks(struct hwloc_binding_hooks *hooks,
		    struct hwloc_topology_support *support __hwloc_attribute_unused)
{
  hooks->set_proc_cpubind = hwloc_aix_set_proc_cpubind;
  hooks->get_proc_cpubind = hwloc_aix_get_proc_cpubind;
#ifdef R_THREAD
#ifdef HWLOC_HAVE_PTHREAD_GETTHRDS_NP
  hooks->set_thread_cpubind = hwloc_aix_set_thread_cpubind;
  hooks->get_thread_cpubind = hwloc_aix_get_thread_cpubind;
#endif /* HWLOC_HAVE_PTHREAD_GETTHRDS_NP */
#endif /* R_THREAD */
  hooks->set_thisproc_cpubind = hwloc_aix_set_thisproc_cpubind;
  hooks->get_thisproc_cpubind = hwloc_aix_get_thisproc_cpubind;
#ifdef R_THREAD
  hooks->set_thisthread_cpubind = hwloc_aix_set_thisthread_cpubind;
  hooks->get_thisthread_cpubind = hwloc_aix_get_thisthread_cpubind;
#endif /* R_THREAD */
  hooks->get_thisthread_last_cpu_location = hwloc_aix_get_thisthread_last_cpu_location;
  /* TODO: get_last_cpu_location: mycpu() only works for the current thread? */
#ifdef P_DEFAULT
  hooks->set_proc_membind = hwloc_aix_set_proc_membind;
  hooks->get_proc_membind = hwloc_aix_get_proc_membind;
#ifdef R_THREAD
#if 0 /* def HWLOC_HAVE_PTHREAD_GETTHRDS_NP */
  /* Does it really make sense to set the memory binding of another thread? */
  hooks->set_thread_membind = hwloc_aix_set_thread_membind;
  hooks->get_thread_membind = hwloc_aix_get_thread_membind;
#endif /* HWLOC_HAVE_PTHREAD_GETTHRDS_NP */
#endif /* R_THREAD */
  hooks->set_thisproc_membind = hwloc_aix_set_thisproc_membind;
  hooks->get_thisproc_membind = hwloc_aix_get_thisproc_membind;
#ifdef R_THREAD
  hooks->set_thisthread_membind = hwloc_aix_set_thisthread_membind;
  hooks->get_thisthread_membind = hwloc_aix_get_thisthread_membind;
#endif /* R_THREAD */
  /* hooks->set_area_membind = hwloc_aix_set_area_membind; */
  /* get_area_membind is not available */
  hooks->alloc_membind = hwloc_aix_alloc_membind;
  hooks->alloc = hwloc_alloc_mmap;
  hooks->free_membind = hwloc_free_mmap;
  support->membind->firsttouch_membind = 1;
  support->membind->bind_membind = 1;
  support->membind->interleave_membind = 1;
#endif /* P_DEFAULT */
}

static struct hwloc_backend *
hwloc_aix_component_instantiate(struct hwloc_disc_component *component,
				const void *_data1 __hwloc_attribute_unused,
				const void *_data2 __hwloc_attribute_unused,
				const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_aix;
  return backend;
}

static struct hwloc_disc_component hwloc_aix_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_CPU,
  "aix",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_aix_component_instantiate,
  50,
  NULL
};

const struct hwloc_component hwloc_aix_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_aix_disc_component
};
