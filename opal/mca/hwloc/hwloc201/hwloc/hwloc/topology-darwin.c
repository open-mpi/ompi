/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2018 Inria.  All rights reserved.
 * Copyright © 2009-2013 Université Bordeaux
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/* Detect topology change: registering for power management changes and check
 * if for example hw.activecpu changed */

/* Apparently, Darwin people do not _want_ to provide binding functions.  */

#include <private/autogen/config.h>

#include <sys/types.h>
#include <sys/sysctl.h>
#include <stdlib.h>
#include <inttypes.h>

#include <hwloc.h>
#include <private/private.h>
#include <private/debug.h>

static int
hwloc_look_darwin(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  int64_t _nprocs;
  unsigned nprocs;
  int64_t _npackages;
  unsigned i, j, cpu;
  struct hwloc_obj *obj;
  size_t size;
  int64_t l1dcachesize, l1icachesize;
  int64_t cacheways[2];
  int64_t l2cachesize;
  int64_t l3cachesize;
  int64_t cachelinesize;
  int64_t memsize;
  int64_t _tmp;
  char cpumodel[64];
  char cpuvendor[64];
  char cpufamilynumber[20], cpumodelnumber[20], cpustepping[20];
  int gotnuma = 0;
  int gotnumamemory = 0;

  if (topology->levels[0][0]->cpuset)
    /* somebody discovered things */
    return -1;

  hwloc_alloc_root_sets(topology->levels[0][0]);

  /* Don't use hwloc_fallback_nbprocessors() because it would return online cpus only,
   * while we need all cpus when computing logical_per_package, etc below.
   * We don't know which CPUs are offline, but Darwin doesn't support binding anyway.
   *
   * TODO: try hw.logicalcpu_max
   */

  if (hwloc_get_sysctlbyname("hw.logicalcpu", &_nprocs) || _nprocs <= 0)
    /* fallback to deprecated way */
    if (hwloc_get_sysctlbyname("hw.ncpu", &_nprocs) || _nprocs <= 0)
      return -1;

  nprocs = _nprocs;
  topology->support.discovery->pu = 1;

  hwloc_debug("%u procs\n", nprocs);

  size = sizeof(cpuvendor);
  if (sysctlbyname("machdep.cpu.vendor", cpuvendor, &size, NULL, 0))
    cpuvendor[0] = '\0';

  size = sizeof(cpumodel);
  if (sysctlbyname("machdep.cpu.brand_string", cpumodel, &size, NULL, 0))
    cpumodel[0] = '\0';

  if (hwloc_get_sysctlbyname("machdep.cpu.family", &_tmp))
    cpufamilynumber[0] = '\0';
  else
    snprintf(cpufamilynumber, sizeof(cpufamilynumber), "%lld", (long long) _tmp);
  if (hwloc_get_sysctlbyname("machdep.cpu.model", &_tmp))
    cpumodelnumber[0] = '\0';
  else
    snprintf(cpumodelnumber, sizeof(cpumodelnumber), "%lld", (long long) _tmp);
  /* .extfamily and .extmodel are already added to .family and .model */
  if (hwloc_get_sysctlbyname("machdep.cpu.stepping", &_tmp))
    cpustepping[0] = '\0';
  else
    snprintf(cpustepping, sizeof(cpustepping), "%lld", (long long) _tmp);

  if (!hwloc_get_sysctlbyname("hw.packages", &_npackages) && _npackages > 0) {
    unsigned npackages = _npackages;
    int64_t _cores_per_package;
    unsigned cores_per_package;
    int64_t _logical_per_package;
    unsigned logical_per_package;

    hwloc_debug("%u packages\n", npackages);

    if (!hwloc_get_sysctlbyname("machdep.cpu.thread_count", &_logical_per_package) && _logical_per_package > 0)
      /* official/modern way */
      logical_per_package = _logical_per_package;
    else if (!hwloc_get_sysctlbyname("machdep.cpu.logical_per_package", &_logical_per_package) && _logical_per_package > 0)
      /* old way, gives the max supported by this "kind" of processor,
       * can be larger than the actual number for this model.
       */
      logical_per_package = _logical_per_package;
    else
      /* Assume the trivia.  */
      logical_per_package = nprocs / npackages;

    hwloc_debug("%u threads per package\n", logical_per_package);

    if (nprocs == npackages * logical_per_package
	&& hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_PACKAGE))
      for (i = 0; i < npackages; i++) {
        obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_PACKAGE, i);
        obj->cpuset = hwloc_bitmap_alloc();
        for (cpu = i*logical_per_package; cpu < (i+1)*logical_per_package; cpu++)
          hwloc_bitmap_set(obj->cpuset, cpu);

        hwloc_debug_1arg_bitmap("package %u has cpuset %s\n",
                   i, obj->cpuset);

        if (cpuvendor[0] != '\0')
          hwloc_obj_add_info(obj, "CPUVendor", cpuvendor);
        if (cpumodel[0] != '\0')
          hwloc_obj_add_info(obj, "CPUModel", cpumodel);
        if (cpufamilynumber[0] != '\0')
          hwloc_obj_add_info(obj, "CPUFamilyNumber", cpufamilynumber);
        if (cpumodelnumber[0] != '\0')
          hwloc_obj_add_info(obj, "CPUModelNumber", cpumodelnumber);
        if (cpustepping[0] != '\0')
          hwloc_obj_add_info(obj, "CPUStepping", cpustepping);

        hwloc_insert_object_by_cpuset(topology, obj);
      }
    else {
      if (cpuvendor[0] != '\0')
        hwloc_obj_add_info(topology->levels[0][0], "CPUVendor", cpuvendor);
      if (cpumodel[0] != '\0')
        hwloc_obj_add_info(topology->levels[0][0], "CPUModel", cpumodel);
      if (cpufamilynumber[0] != '\0')
        hwloc_obj_add_info(topology->levels[0][0], "CPUFamilyNumber", cpufamilynumber);
      if (cpumodelnumber[0] != '\0')
        hwloc_obj_add_info(topology->levels[0][0], "CPUModelNumber", cpumodelnumber);
      if (cpustepping[0] != '\0')
        hwloc_obj_add_info(topology->levels[0][0], "CPUStepping", cpustepping);
    }

    if (!hwloc_get_sysctlbyname("machdep.cpu.core_count", &_cores_per_package) && _cores_per_package > 0)
      /* official/modern way */
      cores_per_package = _cores_per_package;
    else if (!hwloc_get_sysctlbyname("machdep.cpu.cores_per_package", &_cores_per_package) && _cores_per_package > 0)
      /* old way, gives the max supported by this "kind" of processor,
       * can be larger than the actual number for this model.
       */
      cores_per_package = _cores_per_package;
    else
      /* no idea */
      cores_per_package = 0;

    if (cores_per_package > 0
	&& hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_CORE)) {
      hwloc_debug("%u cores per package\n", cores_per_package);

      if (!(logical_per_package % cores_per_package))
        for (i = 0; i < npackages * cores_per_package; i++) {
          obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_CORE, i);
          obj->cpuset = hwloc_bitmap_alloc();
          for (cpu = i*(logical_per_package/cores_per_package);
               cpu < (i+1)*(logical_per_package/cores_per_package);
               cpu++)
            hwloc_bitmap_set(obj->cpuset, cpu);

          hwloc_debug_1arg_bitmap("core %u has cpuset %s\n",
                     i, obj->cpuset);
          hwloc_insert_object_by_cpuset(topology, obj);
        }
    }
  } else {
    if (cpuvendor[0] != '\0')
      hwloc_obj_add_info(topology->levels[0][0], "CPUVendor", cpuvendor);
    if (cpumodel[0] != '\0')
      hwloc_obj_add_info(topology->levels[0][0], "CPUModel", cpumodel);
    if (cpufamilynumber[0] != '\0')
      hwloc_obj_add_info(topology->levels[0][0], "CPUFamilyNumber", cpufamilynumber);
    if (cpumodelnumber[0] != '\0')
      hwloc_obj_add_info(topology->levels[0][0], "CPUModelNumber", cpumodelnumber);
    if (cpustepping[0] != '\0')
      hwloc_obj_add_info(topology->levels[0][0], "CPUStepping", cpustepping);
  }

  if (hwloc_get_sysctlbyname("hw.l1dcachesize", &l1dcachesize))
    l1dcachesize = 0;

  if (hwloc_get_sysctlbyname("hw.l1icachesize", &l1icachesize))
    l1icachesize = 0;

  if (hwloc_get_sysctlbyname("hw.l2cachesize", &l2cachesize))
    l2cachesize = 0;

  if (hwloc_get_sysctlbyname("hw.l3cachesize", &l3cachesize))
    l3cachesize = 0;

  if (hwloc_get_sysctlbyname("machdep.cpu.cache.L1_associativity", &cacheways[0]))
    cacheways[0] = 0;
  else if (cacheways[0] == 0xff)
    cacheways[0] = -1;

  if (hwloc_get_sysctlbyname("machdep.cpu.cache.L2_associativity", &cacheways[1]))
    cacheways[1] = 0;
  else if (cacheways[1] == 0xff)
    cacheways[1] = -1;

  if (hwloc_get_sysctlbyname("hw.cachelinesize", &cachelinesize))
    cachelinesize = 0;

  if (hwloc_get_sysctlbyname("hw.memsize", &memsize))
    memsize = 0;

  if (!sysctlbyname("hw.cacheconfig", NULL, &size, NULL, 0)) {
    unsigned n = size / sizeof(uint32_t);
    uint64_t cacheconfig[n];
    uint64_t cachesize[n];
    uint32_t cacheconfig32[n];

    if ((!sysctlbyname("hw.cacheconfig", cacheconfig, &size, NULL, 0))) {
      /* Yeech. Darwin seemingly has changed from 32bit to 64bit integers for
       * cacheconfig, with apparently no way for detection. Assume the machine
       * won't have more than 4 billion cpus */
      if (cacheconfig[0] > 0xFFFFFFFFUL) {
        memcpy(cacheconfig32, cacheconfig, size);
        for (i = 0 ; i < size / sizeof(uint32_t); i++)
          cacheconfig[i] = cacheconfig32[i];
      }

      memset(cachesize, 0, sizeof(uint64_t) * n);
      size = sizeof(uint64_t) * n;
      if (sysctlbyname("hw.cachesize", cachesize, &size, NULL, 0)) {
        if (n > 0)
          cachesize[0] = memsize;
        if (n > 1)
          cachesize[1] = l1dcachesize;
        if (n > 2)
          cachesize[2] = l2cachesize;
        if (n > 3)
          cachesize[3] = l3cachesize;
      }

      hwloc_debug("%s", "caches");
      for (i = 0; i < n && cacheconfig[i]; i++)
        hwloc_debug(" %"PRIu64"(%"PRIu64"kB)", cacheconfig[i], cachesize[i] / 1024);

      /* Now we know how many caches there are */
      n = i;
      hwloc_debug("\n%u cache levels\n", n - 1);

      /* For each cache level (0 is memory) */
      for (i = 0; i < n; i++) {
        /* cacheconfig tells us how many cpus share it, let's iterate on each cache */
        for (j = 0; j < (nprocs / cacheconfig[i]); j++) {
	  if (!i) {
	    obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_NUMANODE, j);
            obj->nodeset = hwloc_bitmap_alloc();
            hwloc_bitmap_set(obj->nodeset, j);
	    gotnuma++;
          } else {
	    obj = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L1CACHE+i-1, HWLOC_UNKNOWN_INDEX);
	  }
          obj->cpuset = hwloc_bitmap_alloc();
          for (cpu = j*cacheconfig[i];
               cpu < ((j+1)*cacheconfig[i]);
               cpu++)
            hwloc_bitmap_set(obj->cpuset, cpu);

          if (i == 1 && l1icachesize
	      && hwloc_filter_check_keep_object_type(topology, HWLOC_OBJ_L1ICACHE)) {
            /* FIXME assuming that L1i and L1d are shared the same way. Darwin
             * does not yet provide a way to know.  */
            hwloc_obj_t l1i = hwloc_alloc_setup_object(topology, HWLOC_OBJ_L1ICACHE, HWLOC_UNKNOWN_INDEX);
            l1i->cpuset = hwloc_bitmap_dup(obj->cpuset);
            hwloc_debug_1arg_bitmap("L1icache %u has cpuset %s\n",
                j, l1i->cpuset);
            l1i->attr->cache.depth = i;
            l1i->attr->cache.size = l1icachesize;
            l1i->attr->cache.linesize = cachelinesize;
            l1i->attr->cache.associativity = 0;
            l1i->attr->cache.type = HWLOC_OBJ_CACHE_INSTRUCTION;

            hwloc_insert_object_by_cpuset(topology, l1i);
          }
          if (i) {
            hwloc_debug_2args_bitmap("L%ucache %u has cpuset %s\n",
                i, j, obj->cpuset);
            obj->attr->cache.depth = i;
            obj->attr->cache.size = cachesize[i];
            obj->attr->cache.linesize = cachelinesize;
            if (i <= sizeof(cacheways) / sizeof(cacheways[0]))
              obj->attr->cache.associativity = cacheways[i-1];
            else
              obj->attr->cache.associativity = 0;
            if (i == 1 && l1icachesize)
              obj->attr->cache.type = HWLOC_OBJ_CACHE_DATA;
            else
              obj->attr->cache.type = HWLOC_OBJ_CACHE_UNIFIED;
          } else {
            hwloc_debug_1arg_bitmap("node %u has cpuset %s\n",
                j, obj->cpuset);
	    if (cachesize[i]) {
	      obj->attr->numanode.local_memory = cachesize[i];
	      gotnumamemory++;
	    }
	    obj->attr->numanode.page_types_len = 2;
	    obj->attr->numanode.page_types = malloc(2*sizeof(*obj->attr->numanode.page_types));
	    memset(obj->attr->numanode.page_types, 0, 2*sizeof(*obj->attr->numanode.page_types));
	    obj->attr->numanode.page_types[0].size = hwloc_getpagesize();
#if HAVE_DECL__SC_LARGE_PAGESIZE
	    obj->attr->numanode.page_types[1].size = sysconf(_SC_LARGE_PAGESIZE);
#endif
          }

	  if (hwloc_filter_check_keep_object_type(topology, obj->type))
	    hwloc_insert_object_by_cpuset(topology, obj);
	  else
	    hwloc_free_unlinked_object(obj); /* FIXME: don't built at all, just build the cpuset in case l1i needs it */
        }
      }
    }
  }

  if (gotnuma)
    topology->support.discovery->numa = 1;
  if (gotnumamemory)
    topology->support.discovery->numa = 1;

  /* add PU objects */
  hwloc_setup_pu_level(topology, nprocs);

  hwloc_obj_add_info(topology->levels[0][0], "Backend", "Darwin");
  hwloc_add_uname_info(topology, NULL);
  return 0;
}

void
hwloc_set_darwin_hooks(struct hwloc_binding_hooks *hooks __hwloc_attribute_unused,
		       struct hwloc_topology_support *support __hwloc_attribute_unused)
{
}

static struct hwloc_backend *
hwloc_darwin_component_instantiate(struct hwloc_disc_component *component,
				   const void *_data1 __hwloc_attribute_unused,
				   const void *_data2 __hwloc_attribute_unused,
				   const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_look_darwin;
  return backend;
}

static struct hwloc_disc_component hwloc_darwin_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_CPU,
  "darwin",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_darwin_component_instantiate,
  50,
  1,
  NULL
};

const struct hwloc_component hwloc_darwin_component = {
  HWLOC_COMPONENT_ABI,
  NULL, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_darwin_disc_component
};
