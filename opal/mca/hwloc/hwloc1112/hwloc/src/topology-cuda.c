/*
 * Copyright © 2011 Université Bordeaux
 * Copyright © 2012-2014 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/plugins.h>
#include <hwloc/cudart.h>

/* private headers allowed for convenience because this plugin is built within hwloc */
#include <private/misc.h>
#include <private/debug.h>

#include <cuda_runtime_api.h>

struct hwloc_cuda_backend_data_s {
  unsigned nr_devices; /* -1 when unknown yet, first callback will setup */
  struct hwloc_cuda_device_info_s {
    int idx;
    unsigned pcidomain, pcibus, pcidev, pcifunc;
  } * devices;
};

/* query all PCI bus ids for later */
static void
hwloc_cuda_query_devices(struct hwloc_cuda_backend_data_s *data)
{
  cudaError_t cures;
  int nb, i;

  /* mark the number of devices as 0 in case we fail below,
   * so that we don't try again later.
   */
  data->nr_devices = 0;

  cures = cudaGetDeviceCount(&nb);
  if (cures)
    return;

  /* allocate structs */
  data->devices = malloc(nb * sizeof(*data->devices));
  if (!data->devices)
    return;

  for (i = 0; i < nb; i++) {
    struct hwloc_cuda_device_info_s *info = &data->devices[data->nr_devices];
    int domain, bus, dev;

    if (hwloc_cudart_get_device_pci_ids(NULL /* topology unused */, i, &domain, &bus, &dev))
      continue;

    info->idx = i;
    info->pcidomain = (unsigned) domain;
    info->pcibus = (unsigned) bus;
    info->pcidev = (unsigned) dev;
    info->pcifunc = 0;

    /* validate this device */
    data->nr_devices++;
  }

  return;
}

static unsigned hwloc_cuda_cores_per_MP(int major, int minor)
{
  /* based on CUDA C Programming Guide, Annex G */
  switch (major) {
    case 1:
      switch (minor) {
        case 0:
        case 1:
        case 2:
        case 3: return 8;
      }
      break;
    case 2:
      switch (minor) {
        case 0: return 32;
        case 1: return 48;
      }
      break;
    case 3:
      return 192;
    case 5:
      return 128;
  }
  hwloc_debug("unknown compute capability %u.%u, disabling core display.\n", major, minor);
  return 0;
}

static int
hwloc_cuda_backend_notify_new_object(struct hwloc_backend *backend, struct hwloc_backend *caller __hwloc_attribute_unused,
				     struct hwloc_obj *pcidev)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_cuda_backend_data_s *data = backend->private_data;
  unsigned i;

  if (!(hwloc_topology_get_flags(topology) & (HWLOC_TOPOLOGY_FLAG_IO_DEVICES|HWLOC_TOPOLOGY_FLAG_WHOLE_IO)))
    return 0;

  if (!hwloc_topology_is_thissystem(topology)) {
    hwloc_debug("%s", "\nno CUDA detection (not thissystem)\n");
    return 0;
  }

  if (HWLOC_OBJ_PCI_DEVICE != pcidev->type)
    return 0;

  if (data->nr_devices == (unsigned) -1) {
    /* first call, lookup all devices */
    hwloc_cuda_query_devices(data);
    /* if it fails, data->nr_devices = 0 so we won't do anything below and in next callbacks */
  }

  if (!data->nr_devices)
    /* found no devices */
    return 0;

  for(i=0; i<data->nr_devices; i++) {
    struct hwloc_cuda_device_info_s *info = &data->devices[i];
    char cuda_name[32];
    char number[32];
    struct cudaDeviceProp prop;
    hwloc_obj_t cuda_device;
    cudaError_t cures;
    unsigned cores;

    if (info->pcidomain != pcidev->attr->pcidev.domain)
      continue;
    if (info->pcibus != pcidev->attr->pcidev.bus)
      continue;
    if (info->pcidev != pcidev->attr->pcidev.dev)
      continue;
    if (info->pcifunc != pcidev->attr->pcidev.func)
      continue;

    cuda_device = hwloc_alloc_setup_object(HWLOC_OBJ_OS_DEVICE, -1);
    snprintf(cuda_name, sizeof(cuda_name), "cuda%d", info->idx);
    cuda_device->name = strdup(cuda_name);
    cuda_device->depth = (unsigned) HWLOC_TYPE_DEPTH_UNKNOWN;
    cuda_device->attr->osdev.type = HWLOC_OBJ_OSDEV_COPROC;

    hwloc_obj_add_info(cuda_device, "CoProcType", "CUDA");
    hwloc_obj_add_info(cuda_device, "Backend", "CUDA");
    hwloc_obj_add_info(cuda_device, "GPUVendor", "NVIDIA Corporation");

    cures = cudaGetDeviceProperties(&prop, info->idx);
    if (!cures)
      hwloc_obj_add_info(cuda_device, "GPUModel", prop.name);

    snprintf(number, sizeof(number), "%llu", ((unsigned long long) prop.totalGlobalMem) >> 10);
    hwloc_obj_add_info(cuda_device, "CUDAGlobalMemorySize", number);

    snprintf(number, sizeof(number), "%llu", ((unsigned long long) prop.l2CacheSize) >> 10);
    hwloc_obj_add_info(cuda_device, "CUDAL2CacheSize", number);

    snprintf(number, sizeof(number), "%d", prop.multiProcessorCount);
    hwloc_obj_add_info(cuda_device, "CUDAMultiProcessors", number);

    cores = hwloc_cuda_cores_per_MP(prop.major, prop.minor);
    if (cores) {
      snprintf(number, sizeof(number), "%u", cores);
      hwloc_obj_add_info(cuda_device, "CUDACoresPerMP", number);
    }

    snprintf(number, sizeof(number), "%llu", ((unsigned long long) prop.sharedMemPerBlock) >> 10);
    hwloc_obj_add_info(cuda_device, "CUDASharedMemorySizePerMP", number);

    hwloc_insert_object_by_parent(topology, pcidev, cuda_device);
    return 1;
  }

  return 0;
}

static void
hwloc_cuda_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_cuda_backend_data_s *data = backend->private_data;
  free(data->devices);
  free(data);
}

static struct hwloc_backend *
hwloc_cuda_component_instantiate(struct hwloc_disc_component *component,
                                 const void *_data1 __hwloc_attribute_unused,
                                 const void *_data2 __hwloc_attribute_unused,
                                 const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  struct hwloc_cuda_backend_data_s *data;

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
  backend->disable = hwloc_cuda_backend_disable;

  backend->notify_new_object = hwloc_cuda_backend_notify_new_object;
  return backend;
}

static struct hwloc_disc_component hwloc_cuda_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC,
  "cuda",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_cuda_component_instantiate,
  10, /* after pci */
  NULL
};

static int
hwloc_cuda_component_init(unsigned long flags)
{
  if (flags)
    return -1;
  if (hwloc_plugin_check_namespace("cuda", "hwloc_backend_alloc") < 0)
    return -1;
  return 0;
}

#ifdef HWLOC_INSIDE_PLUGIN
HWLOC_DECLSPEC extern const struct hwloc_component hwloc_cuda_component;
#endif

const struct hwloc_component hwloc_cuda_component = {
  HWLOC_COMPONENT_ABI,
  hwloc_cuda_component_init, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_cuda_disc_component
};
