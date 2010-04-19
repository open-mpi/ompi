/*
 * Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
 * See COPYING in top-level directory.
 */

/* TODO: memory:
 * mmap/shmget: +MAP/IPC_MEM_INTERLEAVED, MAP/IPC_MEM_LOCAL,
   MAP_IPC/MEM_FIRST_TOUCH */

/* TODO: psets?
 * since 11i 1.6:
   _SC_PSET_SUPPORT
   pset_create/destroy/assign/setattr
   pset_ctl/getattr
   pset_bind()
   pthread_pset_bind_np()
 */

#include <private/config.h>

#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

#include <sys/mpctl.h>
#include <pthread.h>

static ldom_t
hwloc_hpux_find_ldom(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set)
{
  int has_numa = sysconf(_SC_CCNUMA_SUPPORT) == 1;
  hwloc_obj_t obj;

  if (!has_numa)
    return -1;

  obj = hwloc_get_first_largest_obj_inside_cpuset(topology, hwloc_set);
  if (!hwloc_cpuset_isequal(obj->cpuset, hwloc_set) || obj->type != HWLOC_OBJ_NODE) {
    /* Does not correspond to exactly one node */
    return -1;
  }

  return obj->os_index;
}

static spu_t
hwloc_hpux_find_spu(hwloc_topology_t topology __hwloc_attribute_unused, hwloc_const_cpuset_t hwloc_set)
{
  spu_t cpu;

  cpu = hwloc_cpuset_first(hwloc_set);
  if (cpu != -1 && hwloc_cpuset_weight(hwloc_set) == 1)
    return cpu;
  return -1;
}

/* Note: get_cpubind not available on HP-UX */
static int
hwloc_hpux_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t hwloc_set, int policy)
{
  ldom_t ldom;
  spu_t cpu;

  /* Drop previous binding */
  mpctl(MPC_SETLDOM, MPC_LDOMFLOAT, pid);
  mpctl(MPC_SETPROCESS, MPC_SPUFLOAT, pid);

  if (hwloc_cpuset_isequal(hwloc_set, hwloc_topology_get_complete_cpuset(topology)))
    return 0;

  ldom = hwloc_hpux_find_ldom(topology, hwloc_set);
  if (ldom != -1)
    return mpctl(MPC_SETLDOM, ldom, pid);

  cpu = hwloc_hpux_find_spu(topology, hwloc_set);
  if (cpu != -1)
    return mpctl(policy & HWLOC_CPUBIND_STRICT ? MPC_SETPROCESS_FORCE : MPC_SETPROCESS, cpu, pid);

  errno = EXDEV;
  return -1;
}

static int
hwloc_hpux_set_thisproc_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set, int policy)
{
  return hwloc_hpux_set_proc_cpubind(topology, MPC_SELFPID, hwloc_set, policy);
}

#ifdef hwloc_thread_t
static int
hwloc_hpux_set_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t pthread, hwloc_const_cpuset_t hwloc_set, int policy)
{
  ldom_t ldom, ldom2;
  spu_t cpu, cpu2;

  /* Drop previous binding */
  pthread_ldom_bind_np(&ldom2, PTHREAD_LDOMFLOAT_NP, pthread);
  pthread_processor_bind_np(PTHREAD_BIND_ADVISORY_NP, &cpu2, PTHREAD_SPUFLOAT_NP, pthread);

  if (hwloc_cpuset_isequal(hwloc_set, hwloc_topology_get_complete_cpuset(topology)))
    return 0;

  ldom = hwloc_hpux_find_ldom(topology, hwloc_set);
  if (ldom != -1)
    return pthread_ldom_bind_np(&ldom2, ldom, pthread);

  cpu = hwloc_hpux_find_spu(topology, hwloc_set);
  if (cpu != -1)
    return pthread_processor_bind_np(policy & HWLOC_CPUBIND_STRICT ? PTHREAD_BIND_FORCED_NP : PTHREAD_BIND_ADVISORY_NP, &cpu2, cpu, pthread);

  errno = EXDEV;
  return -1;
}

static int
hwloc_hpux_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t hwloc_set, int policy)
{
  return hwloc_hpux_set_thread_cpubind(topology, PTHREAD_SELFTID_NP, hwloc_set, policy);
}
#endif

void
hwloc_look_hpux(struct hwloc_topology *topology)
{
  int has_numa = sysconf(_SC_CCNUMA_SUPPORT) == 1;
  hwloc_obj_t *nodes = NULL, obj;
  spu_t currentcpu;
  ldom_t currentnode;
  int i, nbnodes = 0;

#ifdef HAVE__SC_LARGE_PAGESIZE
  topology->levels[0][0]->attr->machine.huge_page_size_kB = sysconf(_SC_LARGE_PAGESIZE);
#endif

  if (has_numa) {
    nbnodes = mpctl(topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM ?
      MPC_GETNUMLDOMS_SYS : MPC_GETNUMLDOMS, 0, 0);

    hwloc_debug("%d nodes\n", nbnodes);

    nodes = malloc(nbnodes * sizeof(*nodes));

    i = 0;
    currentnode = mpctl(topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM ?
      MPC_GETFIRSTLDOM_SYS : MPC_GETFIRSTLDOM, 0, 0);
    while (currentnode != -1 && i < nbnodes) {
      hwloc_debug("node %d is %d\n", i, currentnode);
      nodes[i] = obj = hwloc_alloc_setup_object(HWLOC_OBJ_NODE, currentnode);
      obj->cpuset = hwloc_cpuset_alloc();
      obj->nodeset = hwloc_cpuset_alloc();
      hwloc_cpuset_set(obj->nodeset, currentnode);
      /* TODO: obj->attr->node.memory_kB */
      /* TODO: obj->attr->node.huge_page_free */

      currentnode = mpctl(topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM ?
        MPC_GETNEXTLDOM_SYS : MPC_GETNEXTLDOM, currentnode, 0);
      i++;
    }
  }

  i = 0;
  currentcpu = mpctl(topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM ?
      MPC_GETFIRSTSPU_SYS : MPC_GETFIRSTSPU, 0,0);
  while (currentcpu != -1) {
    obj = hwloc_alloc_setup_object(HWLOC_OBJ_PU, currentcpu);
    obj->cpuset = hwloc_cpuset_alloc();
    hwloc_cpuset_set(obj->cpuset, currentcpu);

    hwloc_debug("cpu %d\n", currentcpu);

    if (nodes) {
      /* Add this cpu to its node */
      currentnode = mpctl(MPC_SPUTOLDOM, currentcpu, 0);
      if ((ldom_t) nodes[i]->os_index != currentnode)
        for (i = 0; i < nbnodes; i++)
          if ((ldom_t) nodes[i]->os_index == currentnode)
            break;
      if (i < nbnodes) {
        hwloc_cpuset_set(nodes[i]->cpuset, currentcpu);
        hwloc_debug("is in node %d\n", i);
      } else {
        hwloc_debug("%s", "is in no node?!\n");
      }
    }

    /* Add cpu */
    hwloc_insert_object_by_cpuset(topology, obj);

    currentcpu = mpctl(topology->flags & HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM ?
      MPC_GETNEXTSPU_SYS : MPC_GETNEXTSPU, currentcpu, 0);
  }

  if (nodes) {
    /* Add nodes */
    for (i = 0 ; i < nbnodes ; i++)
      hwloc_insert_object_by_cpuset(topology, nodes[i]);
    free(nodes);
  }

  topology->support.discovery->pu = 1;
}

void
hwloc_set_hpux_hooks(struct hwloc_topology *topology)
{
  topology->set_proc_cpubind = hwloc_hpux_set_proc_cpubind;
  topology->set_thisproc_cpubind = hwloc_hpux_set_thisproc_cpubind;
#ifdef hwloc_thread_t
  topology->set_thread_cpubind = hwloc_hpux_set_thread_cpubind;
  topology->set_thisthread_cpubind = hwloc_hpux_set_thisthread_cpubind;
#endif
}
