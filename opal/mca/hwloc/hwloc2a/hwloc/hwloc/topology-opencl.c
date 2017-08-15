/*
 * Copyright © 2012-2017 Inria.  All rights reserved.
 * Copyright © 2013 Université Bordeaux.  All right reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/plugins.h>

/* private headers allowed for convenience because this plugin is built within hwloc */
#include <private/misc.h>
#include <private/debug.h>

#include <CL/cl_ext.h>

static int
hwloc_opencl_discover(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  enum hwloc_type_filter_e filter;
  cl_platform_id *platform_ids = NULL;
  cl_uint nr_platforms;
  cl_int clret;
  unsigned j;

  hwloc_topology_get_type_filter(topology, HWLOC_OBJ_OS_DEVICE, &filter);
  if (filter == HWLOC_TYPE_FILTER_KEEP_NONE)
    return 0;

  clret = clGetPlatformIDs(0, NULL, &nr_platforms);
  if (CL_SUCCESS != clret || !nr_platforms)
    return -1;
  hwloc_debug("%u OpenCL platforms\n", nr_platforms);
  platform_ids = malloc(nr_platforms * sizeof(*platform_ids));
  if (!platform_ids)
    return -1;
  clret = clGetPlatformIDs(nr_platforms, platform_ids, &nr_platforms);
  if (CL_SUCCESS != clret || !nr_platforms) {
    free(platform_ids);
    return -1;
  }

  for(j=0; j<nr_platforms; j++) {
    cl_device_id *device_ids = NULL;
    cl_uint nr_devices;
    unsigned i;

    clret = clGetDeviceIDs(platform_ids[j], CL_DEVICE_TYPE_ALL, 0, NULL, &nr_devices);
    if (CL_SUCCESS != clret)
      continue;
    device_ids = malloc(nr_devices * sizeof(*device_ids));
    clret = clGetDeviceIDs(platform_ids[j], CL_DEVICE_TYPE_ALL, nr_devices, device_ids, &nr_devices);
    if (CL_SUCCESS != clret) {
      free(device_ids);
      continue;
    }

    for(i=0; i<nr_devices; i++) {
      cl_platform_id platform_id = 0;
      cl_device_type type;
#ifdef CL_DEVICE_TOPOLOGY_AMD
      cl_device_topology_amd amdtopo;
#endif
      cl_ulong globalmemsize;
      cl_uint computeunits;
      hwloc_obj_t osdev, parent;
      char buffer[64];

      hwloc_debug("This is opencl%ud%u\n", j, i);

#ifdef CL_DEVICE_TOPOLOGY_AMD
      clret = clGetDeviceInfo(device_ids[i], CL_DEVICE_TOPOLOGY_AMD, sizeof(amdtopo), &amdtopo, NULL);
      if (CL_SUCCESS != clret) {
        hwloc_debug("no AMD-specific device information: %d\n", clret);
        continue;
      } else if (CL_DEVICE_TOPOLOGY_TYPE_PCIE_AMD != amdtopo.raw.type) {
        hwloc_debug("AMD-specific device topology reports non-PCIe device type: %u\n", amdtopo.raw.type);
        continue;
      }
#else
      continue;
#endif

      osdev = hwloc_alloc_setup_object(topology, HWLOC_OBJ_OS_DEVICE, -1);
      snprintf(buffer, sizeof(buffer), "opencl%ud%u", j, i);
      osdev->name = strdup(buffer);
      osdev->depth = (unsigned) HWLOC_TYPE_DEPTH_UNKNOWN;
      osdev->attr->osdev.type = HWLOC_OBJ_OSDEV_COPROC;

      osdev->subtype = strdup("OpenCL");
      hwloc_obj_add_info(osdev, "Backend", "OpenCL");

      clGetDeviceInfo(device_ids[i], CL_DEVICE_TYPE, sizeof(type), &type, NULL);
      if (type == CL_DEVICE_TYPE_GPU)
        hwloc_obj_add_info(osdev, "OpenCLDeviceType", "GPU");
      else if (type == CL_DEVICE_TYPE_ACCELERATOR)
        hwloc_obj_add_info(osdev, "OpenCLDeviceType", "Accelerator");
      else if (type == CL_DEVICE_TYPE_CPU)
        hwloc_obj_add_info(osdev, "OpenCLDeviceType", "CPU");
      else if (type == CL_DEVICE_TYPE_CUSTOM)
        hwloc_obj_add_info(osdev, "OpenCLDeviceType", "Custom");
      else
        hwloc_obj_add_info(osdev, "OpenCLDeviceType", "Unknown");

      buffer[0] = '\0';
      clGetDeviceInfo(device_ids[i], CL_DEVICE_VENDOR, sizeof(buffer), buffer, NULL);
      if (buffer[0] != '\0')
        hwloc_obj_add_info(osdev, "GPUVendor", buffer);

      buffer[0] = '\0';
#ifdef CL_DEVICE_BOARD_NAME_AMD
      clGetDeviceInfo(device_ids[i], CL_DEVICE_BOARD_NAME_AMD, sizeof(buffer), buffer, NULL);
#else
      clGetDeviceInfo(device_ids[i], CL_DEVICE_NAME, sizeof(buffer), buffer, NULL);
#endif
      if (buffer[0] != '\0')
        hwloc_obj_add_info(osdev, "GPUModel", buffer);

      snprintf(buffer, sizeof(buffer), "%u", j);
      hwloc_obj_add_info(osdev, "OpenCLPlatformIndex", buffer);

      buffer[0] = '\0';
      clret = clGetDeviceInfo(device_ids[i], CL_DEVICE_PLATFORM, sizeof(platform_id), &platform_id, NULL);
      if (CL_SUCCESS == clret) {
        clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, sizeof(buffer), buffer, NULL);
        if (buffer[0] != '\0')
          hwloc_obj_add_info(osdev, "OpenCLPlatformName", buffer);
      }

      snprintf(buffer, sizeof(buffer), "%u", i);
      hwloc_obj_add_info(osdev, "OpenCLPlatformDeviceIndex", buffer);

      clGetDeviceInfo(device_ids[i], CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(computeunits), &computeunits, NULL);
      snprintf(buffer, sizeof(buffer), "%u", computeunits);
      hwloc_obj_add_info(osdev, "OpenCLComputeUnits", buffer);

      clGetDeviceInfo(device_ids[i], CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(globalmemsize), &globalmemsize, NULL);
      snprintf(buffer, sizeof(buffer), "%llu", (unsigned long long) globalmemsize / 1024);
      hwloc_obj_add_info(osdev, "OpenCLGlobalMemorySize", buffer);

      parent = NULL;
#ifdef CL_DEVICE_TOPOLOGY_AMD
      parent = hwloc_pcidisc_find_by_busid(topology, 0, amdtopo.pcie.bus, amdtopo.pcie.device, amdtopo.pcie.function);
      if (!parent)
        parent = hwloc_pcidisc_find_busid_parent(topology, 0, amdtopo.pcie.bus, amdtopo.pcie.device, amdtopo.pcie.function);
#endif
      if (!parent)
        parent = hwloc_get_root_obj(topology);

      hwloc_insert_object_by_parent(topology, parent, osdev);
    }
    free(device_ids);
  }
  free(platform_ids);
  return 0;
}

static struct hwloc_backend *
hwloc_opencl_component_instantiate(struct hwloc_disc_component *component,
                                   const void *_data1 __hwloc_attribute_unused,
                                   const void *_data2 __hwloc_attribute_unused,
                                   const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;

  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_opencl_discover;
  return backend;
}

static struct hwloc_disc_component hwloc_opencl_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC,
  "opencl",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_opencl_component_instantiate,
  10, /* after pci */
  NULL
};

static int
hwloc_opencl_component_init(unsigned long flags)
{
  if (flags)
    return -1;
  if (hwloc_plugin_check_namespace("opencl", "hwloc_backend_alloc") < 0)
    return -1;
  return 0;
}

#ifdef HWLOC_INSIDE_PLUGIN
HWLOC_DECLSPEC extern const struct hwloc_component hwloc_opencl_component;
#endif

const struct hwloc_component hwloc_opencl_component = {
  HWLOC_COMPONENT_ABI,
  hwloc_opencl_component_init, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_opencl_disc_component
};
