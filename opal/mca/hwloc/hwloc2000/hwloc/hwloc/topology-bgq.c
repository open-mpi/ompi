/*
 * Copyright © 2013-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>

#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

#include <stdlib.h>
#include <pthread.h>
#include <sys/utsname.h>
#include <spi/include/kernel/location.h>
#include <spi/include/kernel/process.h>

#ifndef HWLOC_DISABLE_BGQ_PORT_TEST

#define HWLOC_BGQ_CORES 17 /* spare core ignored for now */

static int
hwloc_bgq__get_allowed_resources(struct hwloc_topology *topology)
{
  const char *env;
  unsigned i;

  /* mark the 17th core (OS-reserved) as disallowed */
  hwloc_bitmap_clr_range(topology->allowed_cpuset, (HWLOC_BGQ_CORES-1)*4, HWLOC_BGQ_CORES*4-1);

  if (topology->is_thissystem) { /* don't call CNK unless thissystem */
    env = getenv("BG_THREADMODEL");
    if (!env || atoi(env) != 2) {
      /* process cannot use cores/threads outside of its Kernel_ThreadMask() unless BG_THREADMODEL=2 */
      uint64_t bgmask = Kernel_ThreadMask(Kernel_MyTcoord());
      /* the mask is reversed, manually reverse it */
	for(i=0; i<64; i++)
	if (((bgmask >> i) & 1) == 0)
	  hwloc_bitmap_clr(topology->allowed_cpuset, 63-i);
    }
  }
  return 0;
}

static int
hwloc_look_bgq(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  hwloc_bitmap_t set;
  hwloc_obj_t obj;
  unsigned i;

  if (topology->levels[0][0]->cpuset)
    /* somebody discovered things */
    return -1;

  hwloc_alloc_root_sets(topology->levels[0][0]);

  hwloc_bgq__get_allowed_resources(topology);

  /* a single memory bank */
  obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_NUMANODE, 0);
  set = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(set, 0, HWLOC_BGQ_CORES*4-1);
  obj->cpuset = set;
  set = hwloc_bitmap_alloc();
  hwloc_bitmap_set(set, 0);
  obj->nodeset = set;
  obj->attr->numanode.local_memory = 16ULL*1024*1024*1024ULL;
  hwloc_insert_object_by_cpuset(topology, obj);
  topology->support.discovery->numa = 1;
  topology->support.discovery->numa_memory = 1;

  set = hwloc_bitmap_alloc();
  hwloc_bitmap_set_range(set, 0, HWLOC_BGQ_CORES*4-1);

  /* shared L2 */
  if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L2CACHE)) {
    obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L2CACHE, HWLOC_UNKNOWN_INDEX);
    obj->cpuset = hwloc_bitmap_dup(set);
    obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
    obj->attr->cache.depth = 2;
    obj->attr->cache.size = 32*1024*1024;
    obj->attr->cache.linesize = 128;
    obj->attr->cache.associativity = 16;
    hwloc_insert_object_by_cpuset(topology, obj);
  }

  /* package */
  if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_PACKAGE)) {
    obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_PACKAGE, 0);
    obj->cpuset = set;
    hwloc_obj_add_info(obj, "CPUModel", "IBM PowerPC A2");
    hwloc_insert_object_by_cpuset(topology, obj);
  } else
    hwloc_bitmap_free(set);

  /* Cores */
  for(i=0; i<HWLOC_BGQ_CORES; i++) {
    set = hwloc_bitmap_alloc();
    hwloc_bitmap_set_range(set, i*4, i*4+3);

    /* L1d */
    if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L1CACHE)) {
      obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L1CACHE, HWLOC_UNKNOWN_INDEX);
      obj->cpuset = hwloc_bitmap_dup(set);
      obj->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
      obj->attr->cache.depth = 1;
      obj->attr->cache.size = 16*1024;
      obj->attr->cache.linesize = 64;
      obj->attr->cache.associativity = 8;
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    /* L1i */
    if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L1ICACHE)) {
      obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L1ICACHE, HWLOC_UNKNOWN_INDEX);
      obj->cpuset = hwloc_bitmap_dup(set);
      obj->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
      obj->attr->cache.depth = 1;
      obj->attr->cache.size = 16*1024;
      obj->attr->cache.linesize = 64;
      obj->attr->cache.associativity = 4;
      hwloc_insert_object_by_cpuset(topology, obj);
    }
    /* there's also a L1p "prefetch cache" of 4kB with 128B lines */

    /* Core */
    if (hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_CORE)) {
      obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_CORE, i);
      obj->cpuset = set;
      hwloc_insert_object_by_cpuset(topology, obj);
    } else
      hwloc_bitmap_free(set);
  }

  /* PUs */
  topology->support.discovery->pu = 1;
  hwloc_setup_pu_level(topology, HWLOC_BGQ_CORES*4);

  /* Add BGQ specific information */

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "BGQ");
  hwloc_add_uname_info(topology, NULL);
  return 0;
}

static int
hwloc_bgq_get_thread_cpubind(hwloc_topology_t topology, pthread_t thread, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  unsigned pu;
  cpu_set_t bg_set;
  int err;

  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }
  err = pthread_getaffinity_np(thread, sizeof(bg_set), &bg_set);
  if (err) {
    errno = err;
    return -1;
  }
  for(pu=0; pu<64; pu++)
    if (CPU_ISSET(pu, &bg_set)) {
      /* the binding cannot contain multiple PUs */
      hwloc_bitmap_only(hwloc_set, pu);
      break;
    }
  return 0;
}

static int
hwloc_bgq_get_thisthread_cpubind(hwloc_topology_t topology, hwloc_bitmap_t hwloc_set, int flags __hwloc_attribute_unused)
{
  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }
  hwloc_bitmap_only(hwloc_set, Kernel_ProcessorID());
  return 0;
}

static int
hwloc_bgq_set_thread_cpubind(hwloc_topology_t topology, pthread_t thread, hwloc_const_bitmap_t hwloc_set, int flags)
{
  unsigned pu;
  cpu_set_t bg_set;
  int err;

  if (topology->pid) {
    errno = ENOSYS;
    return -1;
  }
  /* the binding cannot contain multiple PUs.
   * keep the first PU only, and error out if STRICT.
   */
  if (hwloc_bitmap_weight(hwloc_set) != 1) {
    if ((flags & HWLOC_CPUBIND_STRICT)) {
      errno = ENOSYS;
      return -1;
    }
  }
  pu = hwloc_bitmap_first(hwloc_set);
  CPU_ZERO(&bg_set);
  CPU_SET(pu, &bg_set);
  err = pthread_setaffinity_np(thread, sizeof(bg_set), &bg_set);
  if (err) {
    errno = err;
    return -1;
  }
  return 0;
}

static int
hwloc_bgq_set_thisthread_cpubind(hwloc_topology_t topology, hwloc_const_bitmap_t hwloc_set, int flags)
{
  return hwloc_bgq_set_thread_cpubind(topology, pthread_self(), hwloc_set, flags);
}

static int
hwloc_bgq_get_allowed_resources(struct hwloc_topology *topology)
{
  /* Loading BGQ from XML isn't much useful since everything is hardwired anyway.
   * But still implement XML + this callback in case portable applications want to always use XMLs.
   */

  /* In theory, when applying local restrictions to a XML-loaded topology,
   * we should check that the current topology contains 1 NUMA nodes and 17*4 PUs.
   *
   * Just trust the user when he sets THISSYSTEM=1.
   */
  return hwloc_bgq__get_allowed_resources(topology);
}

void
hwloc_set_bgq_hooks(struct hwloc_binding_hooks *hooks __hwloc_attribute_unused,
		    struct hwloc_topology_support *support __hwloc_attribute_unused)
{
  hooks->set_thisthread_cpubind = hwloc_bgq_set_thisthread_cpubind;
  hooks->set_thread_cpubind = hwloc_bgq_set_thread_cpubind;
  hooks->get_thisthread_cpubind = hwloc_bgq_get_thisthread_cpubind;
  hooks->get_thread_cpubind = hwloc_bgq_get_thread_cpubind;
  /* threads cannot be bound to more than one PU, so get_last_cpu_location == get_cpubind */
  hooks->get_thisthread_last_cpu_location = hwloc_bgq_get_thisthread_cpubind;
  /* hooks->get_thread_last_cpu_location = hwloc_bgq_get_thread_cpubind; */

  hooks->get_allowed_resources = hwloc_bgq_get_allowed_resources;
}

static struct hwloc_backend *
hwloc_bgq_component_instantiate(struct hwloc_disc_component *component,
				const void *_data1 __hwloc_attribute_unused,
				const void *_data2 __hwloc_attribute_unused,
				const void *_data3 __hwloc_attribute_unused)
{
  struct utsname utsname;
  struct hwloc_backend *backend;
  int forced_nonbgq = 0;
  int err;

  err = uname(&utsname);
  if (err || strcmp(utsname.sysname, "CNK") || strcmp(utsname.machine, "BGQ")) {
    const char *env = getenv("HWLOC_FORCE_BGQ");
    if (!env || !atoi(env)) {
      fprintf(stderr, "*** Found unexpected uname sysname `%s' machine `%s'.\n", utsname.sysname, utsname.machine);
      fprintf(stderr, "*** The BlueGene/Q backend (bgq) is only enabled by default on compute nodes\n"
		      "*** (where uname returns sysname=CNK and machine=BGQ).\n"
		      "*** If you know you *really* want to run the bgq backend on this non-compute node,\n"
		      "*** set HWLOC_FORCE_BGQ=1 in the environment.\n"
		      "*** If you just want to discover the native topology of this non-compute node,\n"
		      "*** do not pass any BlueGene/Q-specific options on the configure command-line.\n");
      return NULL;
    } else {
      forced_nonbgq = 1;
    }
  }

  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_bgq;
  if (forced_nonbgq)
    backend->is_thissystem = 0;
  return backend;
}

static struct hwloc_disc_component hwloc_bgq_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  "bgq",
  ~0,
  hwloc_bgq_component_instantiate,
  50,
  1,
  NULL
};

const struct hwloc_component hwloc_bgq_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_bgq_disc_component
};

#endif /* !HWLOC_DISABLE_BGQ_PORT_TEST */
