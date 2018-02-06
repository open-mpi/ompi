/* Example hwloc API program.
 *
 * See other examples under doc/examples/ in the source tree
 * for more details.
 *
 * Copyright © 2009-2016 Inria.  All rights reserved.
 * Copyright © 2009-2011,2017 Université Bordeaux
 * Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 *
 * hwloc-hello.c
 */

#include <hwloc.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

int main(void)
{
    hwloc_topology_t topology;
    hwloc_obj_t obj;
    unsigned n, i;
    int devid, platformid;
    const char *dev;

    /* Allocate, initialize and load topology object. */
    hwloc_topology_init(&topology);
    hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
    hwloc_topology_load(topology);

    /* Find CUDA devices through the corresponding OS devices */
    n = hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_OS_DEVICE);

    for (i = 0; i < n ; i++) {
      const char *s;
      obj = hwloc_get_obj_by_type(topology, HWLOC_OBJ_OS_DEVICE, i);
      printf("%s:\n", obj->name);

      s = hwloc_obj_get_info_by_name(obj, "Backend");

      if (s && !strcmp(s, "CUDA")) {
        /* This is a CUDA device */
        assert(!strncmp(obj->name, "cuda", 4));
        devid = atoi(obj->name + 4);
        printf("CUDA device %d\n", devid);

        s = hwloc_obj_get_info_by_name(obj, "GPUModel");
        if (s)
          printf("Model: %s\n", s);

        s = hwloc_obj_get_info_by_name(obj, "CUDAGlobalMemorySize");
        if (s)
          printf("Memory: %s\n", s);

        s = hwloc_obj_get_info_by_name(obj, "CUDAMultiProcessors");
        if (s)
        {
          int mp = atoi(s);
          s = hwloc_obj_get_info_by_name(obj, "CUDACoresPerMP");
          if (s) {
            int mp_cores = atoi(s);
            printf("Cores: %d\n", mp * mp_cores);
          }
        }
      }

      if (s && !strcmp(s, "OpenCL")) {
        /* This is an OpenCL device */
        assert(!strncmp(obj->name, "opencl", 6));
        platformid = atoi(obj->name + 6);
        printf("OpenCL platform %d\n", platformid);
        dev = strchr(obj->name + 6, 'd');
        devid = atoi(dev + 1);
        printf("OpenCL device %d\n", devid);

        s = hwloc_obj_get_info_by_name(obj, "GPUModel");
        if (s)
          printf("Model: %s\n", s);

        s = hwloc_obj_get_info_by_name(obj, "OpenCLGlobalMemorySize");
        if (s)
          printf("Memory: %s\n", s);
      }

      /* One can also use helpers from hwloc/cuda.h, hwloc/cudart.h,
       * hwloc/opencl.h */


      /* Find out cpuset this is connected to */
      while (obj && (!obj->cpuset || hwloc_bitmap_iszero(obj->cpuset)))
        obj = obj->parent;

      if (obj) {
        char *cpuset_string;
        char name[16];
        hwloc_obj_type_snprintf(name, sizeof(name), obj, 0);
        hwloc_bitmap_asprintf(&cpuset_string, obj->cpuset);
        printf("Location: %s P#%d\n", name, obj->os_index);
        printf("Cpuset: %s\n", cpuset_string);
      }
      printf("\n");
    }

    /* Destroy topology object. */
    hwloc_topology_destroy(topology);

    return 0;
}
