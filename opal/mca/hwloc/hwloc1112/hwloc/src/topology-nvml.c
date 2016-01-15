/*
 * Copyright © 2012-2014 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/plugins.h>

/* private headers allowed for convenience because this plugin is built within hwloc */
#include <private/misc.h>
#include <private/debug.h>

#include <nvml.h>

struct hwloc_nvml_backend_data_s {
  unsigned nr_devices; /* -1 when unknown yet, first callback will setup */
  struct hwloc_nvml_device_info_s {
    char name[64];
    char serial[64];
    char uuid[64];
    unsigned pcidomain, pcibus, pcidev, pcifunc;
    float maxlinkspeed;
  } * devices;
};

static void
hwloc_nvml_query_devices(struct hwloc_nvml_backend_data_s *data)
{
  nvmlReturn_t ret;
  unsigned nb, i;

  /* mark the number of devices as 0 in case we fail below,
   * so that we don't try again later.
   */
  data->nr_devices = 0;

  ret = nvmlInit();
  if (NVML_SUCCESS != ret)
    goto out;
  ret = nvmlDeviceGetCount(&nb);
  if (NVML_SUCCESS != ret)
    goto out_with_init;

  /* allocate structs */
  data->devices = malloc(nb * sizeof(*data->devices));
  if (!data->devices)
    goto out_with_init;

  for(i=0; i<nb; i++) {
    struct hwloc_nvml_device_info_s *info = &data->devices[data->nr_devices];
    nvmlPciInfo_t pci;
    nvmlDevice_t device;

    ret = nvmlDeviceGetHandleByIndex(i, &device);
    assert(ret == NVML_SUCCESS);

    ret = nvmlDeviceGetPciInfo(device, &pci);
    if (NVML_SUCCESS != ret)
      continue;

    info->pcidomain = pci.domain;
    info->pcibus = pci.bus;
    info->pcidev = pci.device;
    info->pcifunc = 0;

    info->name[0] = '\0';
    ret = nvmlDeviceGetName(device, info->name, sizeof(info->name));
    /* these may fail with NVML_ERROR_NOT_SUPPORTED on old devices */
    info->serial[0] = '\0';
    ret = nvmlDeviceGetSerial(device, info->serial, sizeof(info->serial));
    info->uuid[0] = '\0';
    ret = nvmlDeviceGetUUID(device, info->uuid, sizeof(info->uuid));

    info->maxlinkspeed = 0.0f;
#if HAVE_DECL_NVMLDEVICEGETMAXPCIELINKGENERATION
    {
      unsigned maxwidth = 0, maxgen = 0;
      float lanespeed;
      nvmlDeviceGetMaxPcieLinkWidth(device, &maxwidth);
      nvmlDeviceGetMaxPcieLinkGeneration(device, &maxgen);
      /* PCIe Gen1 = 2.5GT/s signal-rate per lane with 8/10 encoding    = 0.25GB/s data-rate per lane
       * PCIe Gen2 = 5  GT/s signal-rate per lane with 8/10 encoding    = 0.5 GB/s data-rate per lane
       * PCIe Gen3 = 8  GT/s signal-rate per lane with 128/130 encoding = 1   GB/s data-rate per lane
       */
      lanespeed = maxgen <= 2 ? 2.5 * maxgen * 0.8 : 8.0 * 128/130; /* Gbit/s per lane */
      info->maxlinkspeed = lanespeed * maxwidth / 8; /* GB/s */
    }
#endif

    /* validate this device */
    data->nr_devices++;
  }

out_with_init:
  nvmlShutdown();
out:
  return;
}

static int
hwloc_nvml_backend_notify_new_object(struct hwloc_backend *backend, struct hwloc_backend *caller __hwloc_attribute_unused,
				     struct hwloc_obj *pcidev)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_nvml_backend_data_s *data = backend->private_data;
  unsigned i;

  if (!(hwloc_topology_get_flags(topology) & (HWLOC_TOPOLOGY_FLAG_IO_DEVICES|HWLOC_TOPOLOGY_FLAG_WHOLE_IO)))
    return 0;

  if (!hwloc_topology_is_thissystem(topology)) {
    hwloc_debug("%s", "\nno NVML detection (not thissystem)\n");
    return 0;
  }

  if (HWLOC_OBJ_PCI_DEVICE != pcidev->type)
    return 0;

  if (data->nr_devices == (unsigned) -1) {
    /* first call, lookup all devices */
    hwloc_nvml_query_devices(data);
    /* if it fails, data->nr_devices = 0 so we won't do anything below and in next callbacks */
  }

  if (!data->nr_devices)
    /* found no devices */
    return 0;

  /* now the devices array is ready to use */
  for(i=0; i<data->nr_devices; i++) {
    struct hwloc_nvml_device_info_s *info = &data->devices[i];
    hwloc_obj_t osdev;
    char buffer[64];

    if (info->pcidomain != pcidev->attr->pcidev.domain)
      continue;
    if (info->pcibus != pcidev->attr->pcidev.bus)
      continue;
    if (info->pcidev != pcidev->attr->pcidev.dev)
      continue;
    if (info->pcifunc != pcidev->attr->pcidev.func)
      continue;

    osdev = hwloc_alloc_setup_object(HWLOC_OBJ_OS_DEVICE, -1);
    snprintf(buffer, sizeof(buffer), "nvml%d", i);
    osdev->name = strdup(buffer);
    osdev->depth = (unsigned) HWLOC_TYPE_DEPTH_UNKNOWN;
    osdev->attr->osdev.type = HWLOC_OBJ_OSDEV_GPU;

    hwloc_obj_add_info(osdev, "Backend", "NVML");
    hwloc_obj_add_info(osdev, "GPUVendor", "NVIDIA Corporation");
    hwloc_obj_add_info(osdev, "GPUModel", info->name);
    if (info->serial[0] != '\0')
      hwloc_obj_add_info(osdev, "NVIDIASerial", info->serial);
    if (info->uuid[0] != '\0')
      hwloc_obj_add_info(osdev, "NVIDIAUUID", info->uuid);

    hwloc_insert_object_by_parent(topology, pcidev, osdev);

    if (info->maxlinkspeed != 0.0f)
      /* we found the max link speed, replace the current link speed found by pci (or none) */
      pcidev->attr->pcidev.linkspeed = info->maxlinkspeed;

    return 1;
  }

  return 0;
}

static void
hwloc_nvml_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_nvml_backend_data_s *data = backend->private_data;
  free(data->devices);
  free(data);
}

static struct hwloc_backend *
hwloc_nvml_component_instantiate(struct hwloc_disc_component *component,
				 const void *_data1 __hwloc_attribute_unused,
				 const void *_data2 __hwloc_attribute_unused,
				 const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  struct hwloc_nvml_backend_data_s *data;

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
  backend->disable = hwloc_nvml_backend_disable;

  backend->notify_new_object = hwloc_nvml_backend_notify_new_object;
  return backend;
}

static struct hwloc_disc_component hwloc_nvml_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC,
  "nvml",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_nvml_component_instantiate,
  5, /* after pci, and after cuda since likely less useful */
  NULL
};

static int
hwloc_nvml_component_init(unsigned long flags)
{
  if (flags)
    return -1;
  if (hwloc_plugin_check_namespace("nvml", "hwloc_backend_alloc") < 0)
    return -1;
  return 0;
}

#ifdef HWLOC_INSIDE_PLUGIN
HWLOC_DECLSPEC extern const struct hwloc_component hwloc_nvml_component;
#endif

const struct hwloc_component hwloc_nvml_component = {
  HWLOC_COMPONENT_ABI,
  hwloc_nvml_component_init, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_nvml_disc_component
};
