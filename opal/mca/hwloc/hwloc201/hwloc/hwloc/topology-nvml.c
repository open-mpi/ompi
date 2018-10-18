/*
 * Copyright © 2012-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/plugins.h>

/* private headers allowed for convenience because this plugin is built within hwloc */
#include <private/misc.h>
#include <private/debug.h>

#include <nvml.h>

static int
hwloc_nvml_discover(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  enum hwloc_type_filter_e filter;
  nvmlReturn_t ret;
  unsigned nb, i;

  hwloc_topology_get_type_filter(topology, HWLOC_OBJ_OS_DEVICE, &filter);
  if (filter == HWLOC_TYPE_FILTER_KEEP_NONE)
    return 0;

  ret = nvmlInit();
  if (NVML_SUCCESS != ret)
    return -1;
  ret = nvmlDeviceGetCount(&nb);
  if (NVML_SUCCESS != ret || !nb) {
    nvmlShutdown();
    return 0;
  }

  for(i=0; i<nb; i++) {
    nvmlPciInfo_t pci;
    nvmlDevice_t device;
    hwloc_obj_t osdev, parent;
    char buffer[64];

    ret = nvmlDeviceGetHandleByIndex(i, &device);
    assert(ret == NVML_SUCCESS);

    osdev = hwloc_alloc_setup_object(topology, HWLOC_OBJ_OS_DEVICE, HWLOC_UNKNOWN_INDEX);
    snprintf(buffer, sizeof(buffer), "nvml%u", i);
    osdev->name = strdup(buffer);
    osdev->depth = HWLOC_TYPE_DEPTH_UNKNOWN;
    osdev->attr->osdev.type = HWLOC_OBJ_OSDEV_GPU;

    hwloc_obj_add_info(osdev, "Backend", "NVML");
    hwloc_obj_add_info(osdev, "GPUVendor", "NVIDIA Corporation");

    buffer[0] = '\0';
    ret = nvmlDeviceGetName(device, buffer, sizeof(buffer));
    hwloc_obj_add_info(osdev, "GPUModel", buffer);

    /* these may fail with NVML_ERROR_NOT_SUPPORTED on old devices */
    buffer[0] = '\0';
    ret = nvmlDeviceGetSerial(device, buffer, sizeof(buffer));
    if (buffer[0] != '\0')
      hwloc_obj_add_info(osdev, "NVIDIASerial", buffer);

    buffer[0] = '\0';
    ret = nvmlDeviceGetUUID(device, buffer, sizeof(buffer));
    if (buffer[0] != '\0')
      hwloc_obj_add_info(osdev, "NVIDIAUUID", buffer);

    parent = NULL;
    if (NVML_SUCCESS == nvmlDeviceGetPciInfo(device, &pci)) {
      parent = hwloc_pcidisc_find_by_busid(topology, pci.domain, pci.bus, pci.device, 0);
      if (!parent)
	parent = hwloc_pcidisc_find_busid_parent(topology, pci.domain, pci.bus, pci.device, 0);
#if HAVE_DECL_NVMLDEVICEGETMAXPCIELINKGENERATION
      if (parent && parent->type == HWLOC_OBJ_PCI_DEVICE) {
	unsigned maxwidth = 0, maxgen = 0;
	float lanespeed;
	nvmlDeviceGetMaxPcieLinkWidth(device, &maxwidth);
	nvmlDeviceGetMaxPcieLinkGeneration(device, &maxgen);
	/* PCIe Gen1 = 2.5GT/s signal-rate per lane with 8/10 encoding    = 0.25GB/s data-rate per lane
	 * PCIe Gen2 = 5  GT/s signal-rate per lane with 8/10 encoding    = 0.5 GB/s data-rate per lane
	 * PCIe Gen3 = 8  GT/s signal-rate per lane with 128/130 encoding = 1   GB/s data-rate per lane
	 */
	lanespeed = maxgen <= 2 ? 2.5 * maxgen * 0.8 : 8.0 * 128/130; /* Gbit/s per lane */
	if (lanespeed * maxwidth != 0.)
	  /* we found the max link speed, replace the current link speed found by pci (or none) */
	  parent->attr->pcidev.linkspeed = lanespeed * maxwidth / 8; /* GB/s */
      }
#endif
    }
    if (!parent)
      parent = hwloc_get_root_obj(topology);

    hwloc_insert_object_by_parent(topology, parent, osdev);
  }

  nvmlShutdown();
  return 0;
}

static struct hwloc_backend *
hwloc_nvml_component_instantiate(struct hwloc_disc_component *component,
				 const void *_data1 __hwloc_attribute_unused,
				 const void *_data2 __hwloc_attribute_unused,
				 const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;

  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_nvml_discover;
  return backend;
}

static struct hwloc_disc_component hwloc_nvml_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC,
  "nvml",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_nvml_component_instantiate,
  5, /* after pci, and after cuda since likely less useful */
  1,
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
