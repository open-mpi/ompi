/*
 * Copyright © 2013-2015 Inria.  All rights reserved.
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

static int
hwloc_look_bgq(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  unsigned i;
  const char *env;

  if (!topology->levels[0][0]->cpuset) {
    /* Nobody created objects yet, setup everything */
    hwloc_bitmap_t set;
    hwloc_obj_t obj;

#define HWLOC_BGQ_CORES 17 /* spare core ignored for now */

    hwloc_alloc_obj_cpusets(topology->levels[0][0]);
    /* mark the 17th core (OS-reserved) as disallowed */
    hwloc_bitmap_clr_range(topology->levels[0][0]->allowed_cpuset, (HWLOC_BGQ_CORES-1)*4, HWLOC_BGQ_CORES*4-1);

    env = getenv("BG_THREADMODEL");
    if (!env || atoi(env) != 2) {
      /* process cannot use cores/threads outside of its Kernel_ThreadMask() */
      uint64_t bgmask = Kernel_ThreadMask(Kernel_MyTcoord());
      /* the mask is reversed, manually reverse it */
      for(i=0; i<64; i++)
	if (((bgmask >> i) & 1) == 0)
	  hwloc_bitmap_clr(topology->levels[0][0]->allowed_cpuset, 63-i);
    }

    /* a single memory bank */
    set = hwloc_bitmap_alloc();
    hwloc_bitmap_set(set, 0);
    topology->levels[0][0]->nodeset = set;
    topology->levels[0][0]->memory.local_memory = 16ULL*1024*1024*1024ULL;

    /* package */
    obj = hwloc_alloc_setup_object(HWLOC_OBJ_PACKAGE, 0);
    set = hwloc_bitmap_alloc();
    hwloc_bitmap_set_range(set, 0, HWLOC_BGQ_CORES*4-1);
    obj->cpuset = set;
    hwloc_obj_add_info(obj, "CPUModel", "IBM PowerPC A2");
    hwloc_insert_object_by_cpuset(topology, obj);

    /* shared L2 */
    obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
    obj->cpuset = hwloc_bitmap_dup(set);
    obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
    obj->attr->cache.depth = 2;
    obj->attr->cache.size = 32*1024*1024;
    obj->attr->cache.linesize = 128;
    obj->attr->cache.associativity = 16;
    hwloc_insert_object_by_cpuset(topology, obj);

    /* Cores */
    for(i=0; i<HWLOC_BGQ_CORES; i++) {
      /* Core */
      obj = hwloc_alloc_setup_object(HWLOC_OBJ_CORE, i);
      set = hwloc_bitmap_alloc();
      hwloc_bitmap_set_range(set, i*4, i*4+3);
      obj->cpuset = set;
      hwloc_insert_object_by_cpuset(topology, obj);
      /* L1d */
      obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
      obj->cpuset = hwloc_bitmap_dup(set);
      obj->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
      obj->attr->cache.depth = 1;
      obj->attr->cache.size = 16*1024;
      obj->attr->cache.linesize = 64;
      obj->attr->cache.associativity = 8;
      hwloc_insert_object_by_cpuset(topology, obj);
      /* L1i */
      obj = hwloc_alloc_setup_object(HWLOC_OBJ_CACHE, -1);
      obj->cpuset = hwloc_bitmap_dup(set);
      obj->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;
      obj->attr->cache.depth = 1;
      obj->attr->cache.size = 16*1024;
      obj->attr->cache.linesize = 64;
      obj->attr->cache.associativity = 4;
      hwloc_insert_object_by_cpuset(topology, obj);
      /* there's also a L1p "prefetch cache" of 4kB with 128B lines */
    }

    /* PUs */
    hwloc_setup_pu_level(topology, HWLOC_BGQ_CORES*4);
  }

  /* Add BGQ specific information */

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "BGQ");
  if (topology->is_thissystem)
    hwloc_add_uname_info(topology, NULL);
  return 1;
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
}

static struct hwloc_backend *
hwloc_bgq_component_instantiate(struct hwloc_disc_component *component,
				const void *_data1 __hwloc_attribute_unused,
				const void *_data2 __hwloc_attribute_unused,
				const void *_data3 __hwloc_attribute_unused)
{
  struct utsname utsname;
  struct hwloc_backend *backend;
  const char *env;
  int err;

  env = getenv("HWLOC_FORCE_BGQ");
  if (!env || !atoi(env)) {
    err = uname(&utsname);
    if (err || strcmp(utsname.sysname, "CNK") || strcmp(utsname.machine, "BGQ")) {
      fprintf(stderr, "*** Found unexpected uname sysname `%s' machine `%s'\n", utsname.sysname, utsname.machine);
      fprintf(stderr, "*** The BGQ backend is only enabled on compute nodes by default (sysname=CNK machine=BGQ)\n");
      fprintf(stderr, "*** Set HWLOC_FORCE_BGQ=1 in the environment to enforce the BGQ backend anyway.\n");
      return NULL;
    }
  }

  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_bgq;
  return backend;
}

static struct hwloc_disc_component hwloc_bgq_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  "bgq",
  ~0,
  hwloc_bgq_component_instantiate,
  50,
  NULL
};

const struct hwloc_component hwloc_bgq_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_bgq_disc_component
};
