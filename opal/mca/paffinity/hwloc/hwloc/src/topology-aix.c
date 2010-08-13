/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

/* TODO: use SIGRECONFIG & dr_reconfig for state change */

#include <private/config.h>

#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

#include <sys/rset.h>
#include <sys/processor.h>
#include <sys/thread.h>

static int
hwloc_aix_set_sth_cpubind(hwloc_topology_t topology, rstype_t what, rsid_t who, hwloc_const_cpuset_t hwloc_set, int policy __hwloc_attribute_unused)
{
  rsethandle_t rset, rad;
  hwloc_obj_t obj;
  int res = -1;

  /* The resulting binding is always strict */

  if (hwloc_cpuset_isequal(hwloc_set, hwloc_topology_get_complete_cpuset(topology))) {
    if (ra_detachrset(what, who, 0))
      return -1;
    return 0;
  }

  obj = hwloc_get_first_largest_obj_inside_cpuset(topology, hwloc_set);
  if (!hwloc_cpuset_isequal(obj->cpuset, hwloc_set) || obj->os_level == -1) {
    /* Does not correspond to exactly one radset, not possible */
    errno = EXDEV;
    return -1;
  }

  if ((topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM))
    rset = rs_alloc(RS_ALL);
  else
    rset = rs_alloc(RS_PARTITION);
  rad = rs_alloc(RS_EMPTY);
  if (rs_getrad(rset, rad, obj->os_level, obj->os_index, 0)) {
    fprintf(stderr,"rs_getrad(%d,%u) failed: %s\n", obj->os_level, obj->os_index, strerror(errno));
    goto out;
  }

  /* TODO: memory binding and policy (P_DEFAULT / P_FIRST_TOUCH / P_BALANCED)
   * ra_mmap to allocation on an rset
   */

  if (ra_attachrset(what, who, rad, 0)) {
    res = -1;
    goto out;
  }

  res = 0;

out:
  rs_free(rset);
  rs_free(rad);
  return res;
}

static int
hwloc_aix_get_sth_cpubind(hwloc_topology_t topology, rstype_t what, rsid_t who, hwloc_cpuset_t hwloc_set, int policy __hwloc_attribute_unused)
{
  rsethandle_t rset;
  unsigned cpu, maxcpus;
  int res = -1;

  rset = rs_alloc(RS_EMPTY);

  if (ra_getrset(what, who, 0, rset) == -1)
    goto out;

  hwloc_cpuset_zero(hwloc_set);
  maxcpus = rs_getinfo(rset, R_MAXPROCS, 0);
  for (cpu = 0; cpu < maxcpus; cpu++)
    if (rs_op(RS_TESTRESOURCE, rset, NULL, R_PROCS, cpu) == 1)
      hwloc_cpuset_set(hwloc_set, cpu);
  hwloc_cpuset_and(hwloc_set, hwloc_set, hwloc_topology_get_complete_cpuset(topology));
  res = 0;

out:
  rs_free(rset);
  return res;
}

static int
hwloc_aix_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set, int policy)
{
  rsid_t who = { .at_pid = getpid() };
  return hwloc_aix_set_sth_cpubind(topology, R_PROCESS, who, hwloc_set, policy);
}

static int
hwloc_aix_get_thisproc_cpubind(hwloc_topology_t topology, hwloc_cpuset_t hwloc_set, int policy)
{
  rsid_t who = { .at_pid = getpid() };
  return hwloc_aix_get_sth_cpubind(topology, R_PROCESS, who, hwloc_set, policy);
}

static int
hwloc_aix_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set, int policy)
{
  rsid_t who = { .at_tid = thread_self() };
  return hwloc_aix_set_sth_cpubind(topology, R_THREAD, who, hwloc_set, policy);
}

static int
hwloc_aix_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_cpuset_t hwloc_set, int policy)
{
  rsid_t who = { .at_tid = thread_self() };
  return hwloc_aix_get_sth_cpubind(topology, R_THREAD, who, hwloc_set, policy);
}

static int
hwloc_aix_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t hwloc_set, int policy)
{
  rsid_t who = { .at_pid = pid };
  return hwloc_aix_set_sth_cpubind(topology, R_PROCESS, who, hwloc_set, policy);
}

static int
hwloc_aix_get_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t hwloc_set, int policy)
{
  rsid_t who = { .at_pid = pid };
  return hwloc_aix_get_sth_cpubind(topology, R_PROCESS, who, hwloc_set, policy);
}

#ifdef HWLOC_HAVE_PTHREAD_GETTHRDS_NP
static int
hwloc_aix_set_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t pthread, hwloc_const_cpuset_t hwloc_set, int policy)
{
  struct __pthrdsinfo info;
  int size;
  if ((errno = pthread_getthrds_np(&pthread, PTHRDSINFO_QUERY_TID, &info, sizeof(info), NULL, &size)))
    return -1;
  {
    rsid_t who = { .at_tid = info.__pi_tid };
    return hwloc_aix_set_sth_cpubind(topology, R_THREAD, who, hwloc_set, policy);
  }
}

static int
hwloc_aix_get_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t pthread, hwloc_cpuset_t hwloc_set, int policy)
{
  struct __pthrdsinfo info;
  int size;
  if (pthread_getthrds_np(&pthread, PTHRDSINFO_QUERY_TID, &info, sizeof(info), NULL, &size))
    return -1;
  {
    rsid_t who = { .at_tid = info.__pi_tid };
    return hwloc_aix_get_sth_cpubind(topology, R_THREAD, who, hwloc_set, policy);
  }
}
#endif /* HWLOC_HAVE_PTHREAD_GETTHRDS_NP */

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
    if (rs_getrad(rset, rad, sdl, i, 0)) {
      fprintf(stderr,"rs_getrad(%d) failed: %s\n", i, strerror(errno));
      continue;
    }
    if (!rs_getinfo(rad, R_NUMPROCS, 0))
      continue;

    /* It seems logical processors are numbered from 1 here, while the
     * bindprocessor functions numbers them from 0... */
    obj = hwloc_alloc_setup_object(type, i - (type == HWLOC_OBJ_PU));
    obj->cpuset = hwloc_cpuset_alloc();
    obj->os_level = sdl;
    switch(type) {
      case HWLOC_OBJ_NODE:
	obj->nodeset = hwloc_cpuset_alloc();
	hwloc_cpuset_set(obj->nodeset, i);
	obj->memory.local_memory = 0; /* TODO: odd, rs_getinfo(rad, R_MEMSIZE, 0) << 10 returns the total memory ... */
	obj->memory.page_types_len = 2;
	obj->memory.page_types = malloc(2*sizeof(*obj->memory.page_types));
	memset(obj->memory.page_types, 0, 2*sizeof(*obj->memory.page_types));
	obj->memory.page_types[0].size = getpagesize();
#ifdef HAVE__SC_LARGE_PAGESIZE
	obj->memory.page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
	/* TODO: obj->memory.page_types[1].count = rs_getinfo(rset, R_LGPGFREE, 0) / hugepagesize */
	break;
      case HWLOC_OBJ_CACHE:
	obj->attr->cache.size = 0; /* TODO: ? */
	obj->attr->cache.depth = 2;
	break;
      case HWLOC_OBJ_GROUP:
	obj->attr->group.depth = level;
      default:
	break;
    }
    maxcpus = rs_getinfo(rad, R_MAXPROCS, 0);
    for (j = 0; j < maxcpus; j++) {
      if (rs_op(RS_TESTRESOURCE, rad, NULL, R_PROCS, j))
	hwloc_cpuset_set(obj->cpuset, j);
    }
    hwloc_debug_2args_cpuset("%s %d has cpuset %s\n",
	       hwloc_obj_type_string(type),
	       i, obj->cpuset);
    hwloc_insert_object_by_cpuset(topology, obj);
  }

  rs_free(rset);
  rs_free(rad);
}

void
hwloc_look_aix(struct hwloc_topology *topology)
{
  int i;
  /* TODO: R_LGPGDEF/R_LGPGFREE for large pages */

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
	  look_rset(i, HWLOC_OBJ_NODE, topology, i);
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
}

void
hwloc_set_aix_hooks(struct hwloc_topology *topology)
{
  topology->set_proc_cpubind = hwloc_aix_set_proc_cpubind;
  topology->get_proc_cpubind = hwloc_aix_get_proc_cpubind;
#ifdef HWLOC_HAVE_PTHREAD_GETTHRDS_NP
  topology->set_thread_cpubind = hwloc_aix_set_thread_cpubind;
  topology->get_thread_cpubind = hwloc_aix_get_thread_cpubind;
#endif /* HWLOC_HAVE_PTHREAD_GETTHRDS_NP */
  topology->set_thisproc_cpubind = hwloc_aix_set_thisproc_cpubind;
  topology->get_thisproc_cpubind = hwloc_aix_get_thisproc_cpubind;
  topology->set_thisthread_cpubind = hwloc_aix_set_thisthread_cpubind;
  topology->get_thisthread_cpubind = hwloc_aix_get_thisthread_cpubind;
}
