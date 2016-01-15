/*
 * Copyright © 2012-2014 Inria.  All rights reserved.
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

typedef enum hwloc_opencl_device_type_e {
  HWLOC_OPENCL_DEVICE_AMD
} hwloc_opencl_device_type_t;

struct hwloc_opencl_backend_data_s {
  unsigned nr_devices; /* -1 when unknown yet, first callback will setup */
  struct hwloc_opencl_device_info_s {
    hwloc_opencl_device_type_t type;

    unsigned platformidx;
    char platformname[64];
    unsigned platformdeviceidx;
    char devicename[64];
    char devicevendor[64];
    char devicetype[64];

    unsigned computeunits;
    unsigned long long globalmemsize;

    union hwloc_opencl_device_info_u {
      struct hwloc_opencl_device_info_amd_s {
        unsigned pcidomain, pcibus, pcidev, pcifunc;
      } amd;
    } specific;
  } * devices;
};

static void
hwloc_opencl_query_devices(struct hwloc_opencl_backend_data_s *data)
{
  cl_platform_id *platform_ids = NULL;
  cl_uint nr_platforms;
  cl_device_id *device_ids = NULL;
  cl_uint nr_devices, nr_total_devices, tmp;
  cl_int clret;
  unsigned curpfidx, curpfdvidx, i;

  /* mark the number of devices as 0 in case we fail below,
   * so that we don't try again later.
   */
  data->nr_devices = 0;

  /* count platforms, allocate and get them */
  clret = clGetPlatformIDs(0, NULL, &nr_platforms);
  if (CL_SUCCESS != clret || !nr_platforms)
    goto out;
  hwloc_debug("%u OpenCL platforms\n", nr_platforms);
  platform_ids = malloc(nr_platforms * sizeof(*platform_ids));
  if (!platform_ids)
    goto out;
  clret = clGetPlatformIDs(nr_platforms, platform_ids, &nr_platforms);
  if (CL_SUCCESS != clret || !nr_platforms)
    goto out_with_platform_ids;

  /* how many devices, total? */
  tmp = 0;
  for(i=0; i<nr_platforms; i++) {
    clret = clGetDeviceIDs(platform_ids[i], CL_DEVICE_TYPE_ALL, 0, NULL, &nr_devices);
    if (CL_SUCCESS != clret)
      goto out_with_platform_ids;
    tmp += nr_devices;
  }
  nr_total_devices = tmp;
  hwloc_debug("%u OpenCL devices total\n", nr_total_devices);
  /* allocate structs */
  device_ids = malloc(nr_total_devices * sizeof(*device_ids));
  data->devices = malloc(nr_total_devices * sizeof(*data->devices));
  if (!data->devices || !device_ids)
    goto out_with_device_ids;
  /* actually query device ids */
  tmp = 0;
  for(i=0; i<nr_platforms; i++) {
    clret = clGetDeviceIDs(platform_ids[i], CL_DEVICE_TYPE_ALL, nr_total_devices - tmp, device_ids + tmp, &nr_devices);
    if (CL_SUCCESS != clret)
      goto out_with_device_ids;
    tmp += nr_devices;
  }

  /* query individual devices */
  curpfidx = 0;
  curpfdvidx = 0;
  for(i=0; i<nr_total_devices; i++) {
    struct hwloc_opencl_device_info_s *info = &data->devices[data->nr_devices];
    cl_platform_id platform_id = 0;
    cl_device_type type;
#ifdef CL_DEVICE_TOPOLOGY_AMD
    cl_device_topology_amd amdtopo;
#endif
    cl_ulong globalmemsize;
    cl_uint computeunits;

    hwloc_debug("Looking device %p\n", device_ids[i]);

    info->platformname[0] = '\0';
    clret = clGetDeviceInfo(device_ids[i], CL_DEVICE_PLATFORM, sizeof(platform_id), &platform_id, NULL);
    if (CL_SUCCESS != clret)
      continue;
    clGetPlatformInfo(platform_id, CL_PLATFORM_NAME, sizeof(info->platformname), info->platformname, NULL);

    info->devicename[0] = '\0';
#ifdef CL_DEVICE_BOARD_NAME_AMD
    clGetDeviceInfo(device_ids[i], CL_DEVICE_BOARD_NAME_AMD, sizeof(info->devicename), info->devicename, NULL);
#else
    clGetDeviceInfo(device_ids[i], CL_DEVICE_NAME, sizeof(info->devicename), info->devicename, NULL);
#endif
    info->devicevendor[0] = '\0';
    clGetDeviceInfo(device_ids[i], CL_DEVICE_VENDOR, sizeof(info->devicevendor), info->devicevendor, NULL);

    clGetDeviceInfo(device_ids[i], CL_DEVICE_TYPE, sizeof(type), &type, NULL);
    switch (type) {
    case CL_DEVICE_TYPE_CPU: /* FIXME: cannot happen in PCI devices? */
      strcpy(info->devicetype, "CPU");
      break;
    case CL_DEVICE_TYPE_GPU:
      strcpy(info->devicetype, "GPU");
      break;
    case CL_DEVICE_TYPE_ACCELERATOR:
      strcpy(info->devicetype, "Accelerator");
      break;
    default:
      strcpy(info->devicetype, "Unknown");
      break;
    }

    clGetDeviceInfo(device_ids[i], CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(globalmemsize), &globalmemsize, NULL);
    info->globalmemsize = globalmemsize / 1024;

    clGetDeviceInfo(device_ids[i], CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(computeunits), &computeunits, NULL);
    info->computeunits = computeunits;

    hwloc_debug("platform %s device %s vendor %s type %s\n", info->platformname, info->devicename, info->devicevendor, info->devicetype);

    /* find our indexes */
    while (platform_id != platform_ids[curpfidx]) {
      curpfidx++;
      curpfdvidx = 0;
    }
    info->platformidx = curpfidx;
    info->platformdeviceidx = curpfdvidx;
    curpfdvidx++;

    hwloc_debug("This is opencl%dd%d\n", info->platformidx, info->platformdeviceidx);

#ifdef CL_DEVICE_TOPOLOGY_AMD
    clret = clGetDeviceInfo(device_ids[i], CL_DEVICE_TOPOLOGY_AMD, sizeof(amdtopo), &amdtopo, NULL);
    if (CL_SUCCESS != clret) {
      hwloc_debug("no AMD-specific device information: %d\n", clret);
      continue;
    }
    if (CL_DEVICE_TOPOLOGY_TYPE_PCIE_AMD != amdtopo.raw.type) {
      hwloc_debug("not a PCIe device: %u\n", amdtopo.raw.type);
      continue;
    }

    info->type = HWLOC_OPENCL_DEVICE_AMD;
    info->specific.amd.pcidomain = 0;
    info->specific.amd.pcibus = amdtopo.pcie.bus;
    info->specific.amd.pcidev = amdtopo.pcie.device;
    info->specific.amd.pcifunc = amdtopo.pcie.function;

    hwloc_debug("OpenCL device on PCI 0000:%02x:%02x.%u\n", amdtopo.pcie.bus, amdtopo.pcie.device, amdtopo.pcie.function);

    /* validate this device */
    data->nr_devices++;
#endif /* HAVE_DECL_CL_DEVICE_TOPOLOGY_AMD */
  }
  free(device_ids);
  free(platform_ids);
  return;

out_with_device_ids:
  free(device_ids);
  free(data->devices);
  data->devices = NULL;
out_with_platform_ids:
  free(platform_ids);
out:
  return;
}

static int
hwloc_opencl_backend_notify_new_object(struct hwloc_backend *backend, struct hwloc_backend *caller __hwloc_attribute_unused,
				       struct hwloc_obj *pcidev)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_opencl_backend_data_s *data = backend->private_data;
  unsigned i;

  if (!(hwloc_topology_get_flags(topology) & (HWLOC_TOPOLOGY_FLAG_IO_DEVICES|HWLOC_TOPOLOGY_FLAG_WHOLE_IO)))
    return 0;

  if (!hwloc_topology_is_thissystem(topology)) {
    hwloc_debug("%s", "\nno OpenCL detection (not thissystem)\n");
    return 0;
  }

  if (HWLOC_OBJ_PCI_DEVICE != pcidev->type)
    return 0;

  if (data->nr_devices == (unsigned) -1) {
    /* first call, lookup all devices */
    hwloc_opencl_query_devices(data);
    /* if it fails, data->nr_devices = 0 so we won't do anything below and in next callbacks */
  }

  if (!data->nr_devices)
    /* found no devices */
    return 0;

  /* now the devices array is ready to use */
  for(i=0; i<data->nr_devices; i++) {
    struct hwloc_opencl_device_info_s *info = &data->devices[i];
    hwloc_obj_t osdev;
    char buffer[64];

    assert(info->type == HWLOC_OPENCL_DEVICE_AMD);
    if (info->specific.amd.pcidomain != pcidev->attr->pcidev.domain)
      continue;
    if (info->specific.amd.pcibus != pcidev->attr->pcidev.bus)
      continue;
    if (info->specific.amd.pcidev != pcidev->attr->pcidev.dev)
      continue;
    if (info->specific.amd.pcifunc != pcidev->attr->pcidev.func)
      continue;

    osdev = hwloc_alloc_setup_object(HWLOC_OBJ_OS_DEVICE, -1);
    snprintf(buffer, sizeof(buffer), "opencl%dd%d", info->platformidx, info->platformdeviceidx);
    osdev->name = strdup(buffer);
    osdev->depth = (unsigned) HWLOC_TYPE_DEPTH_UNKNOWN;
    osdev->attr->osdev.type = HWLOC_OBJ_OSDEV_COPROC;

    hwloc_obj_add_info(osdev, "CoProcType", "OpenCL");
    hwloc_obj_add_info(osdev, "Backend", "OpenCL");
    hwloc_obj_add_info(osdev, "OpenCLDeviceType", info->devicetype);

    if (info->devicevendor[0] != '\0')
      hwloc_obj_add_info(osdev, "GPUVendor", info->devicevendor);
    if (info->devicename[0] != '\0')
      hwloc_obj_add_info(osdev, "GPUModel", info->devicename);

    snprintf(buffer, sizeof(buffer), "%u", info->platformidx);
    hwloc_obj_add_info(osdev, "OpenCLPlatformIndex", buffer);
    if (info->platformname[0] != '\0')
      hwloc_obj_add_info(osdev, "OpenCLPlatformName", info->platformname);

    snprintf(buffer, sizeof(buffer), "%u", info->platformdeviceidx);
    hwloc_obj_add_info(osdev, "OpenCLPlatformDeviceIndex", buffer);

    snprintf(buffer, sizeof(buffer), "%u", info->computeunits);
    hwloc_obj_add_info(osdev, "OpenCLComputeUnits", buffer);

    snprintf(buffer, sizeof(buffer), "%llu", info->globalmemsize);
    hwloc_obj_add_info(osdev, "OpenCLGlobalMemorySize", buffer);

    hwloc_insert_object_by_parent(topology, pcidev, osdev);
    return 1;
  }

  return 0;
}

static void
hwloc_opencl_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_opencl_backend_data_s *data = backend->private_data;
  free(data->devices);
  free(data);
}

static struct hwloc_backend *
hwloc_opencl_component_instantiate(struct hwloc_disc_component *component,
				   const void *_data1 __hwloc_attribute_unused,
				   const void *_data2 __hwloc_attribute_unused,
				   const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  struct hwloc_opencl_backend_data_s *data;

  /* thissystem may not be fully initialized yet, we'll check flags in discover() */

  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;

  data = malloc(sizeof(*data));
  if (!data) {
    free(backend);
    return NULL;
  }
  /* the first callback will initialize those */
  data->nr_devices = (unsigned) -1; /* unknown yet */
  data->devices = NULL;

  backend->private_data = data;
  backend->disable = hwloc_opencl_backend_disable;

  backend->notify_new_object = hwloc_opencl_backend_notify_new_object;
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
