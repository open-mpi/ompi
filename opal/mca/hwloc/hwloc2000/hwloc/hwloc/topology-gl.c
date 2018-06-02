/*
 * Copyright © 2012-2013 Blue Brain Project, BBP/EPFL. All rights reserved.
 * Copyright © 2012-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/plugins.h>

/* private headers allowed for convenience because this plugin is built within hwloc */
#include <private/misc.h>
#include <private/debug.h>

#include <stdarg.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <NVCtrl/NVCtrl.h>
#include <NVCtrl/NVCtrlLib.h>

#define HWLOC_GL_SERVER_MAX 10
#define HWLOC_GL_SCREEN_MAX 10

static int
hwloc_gl_discover(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  enum hwloc_type_filter_e filter;
  unsigned i;
  int err;

  hwloc_topology_get_type_filter(topology, HWLOC_OBJ_OS_DEVICE, &filter);
  if (filter == HWLOC_TYPE_FILTER_KEEP_NONE)
    return 0;

  for (i = 0; i < HWLOC_GL_SERVER_MAX; ++i) {
    Display* display;
    char displayName[10];
    int opcode, event, error;
    unsigned j;

    /* open X server */
    snprintf(displayName, sizeof(displayName), ":%u", i);
    display = XOpenDisplay(displayName);
    if (!display)
      continue;

    /* Check for NV-CONTROL extension (it's per server) */
    if(!XQueryExtension(display, "NV-CONTROL", &opcode, &event, &error)) {
      XCloseDisplay(display);
      continue;
    }

    for (j = 0; j < (unsigned) ScreenCount(display) && j < HWLOC_GL_SCREEN_MAX; j++) {
      hwloc_obj_t osdev, parent;
      const int screen = (int)j;
      unsigned int *ptr_binary_data;
      int data_length;
      int gpu_number;
      int nv_ctrl_pci_bus;
      int nv_ctrl_pci_device;
      int nv_ctrl_pci_domain;
      int nv_ctrl_pci_func;
      char *productname;
      char name[64];

      /* the server supports NV-CONTROL but it may contain non-NVIDIA screen that don't support it */
      if (!XNVCTRLIsNvScreen(display, screen))
        continue;

      /* Gets the GPU number attached to the default screen. */
      /* For further details, see the <NVCtrl/NVCtrlLib.h> */
      err = XNVCTRLQueryTargetBinaryData (display, NV_CTRL_TARGET_TYPE_X_SCREEN, screen, 0,
                                          NV_CTRL_BINARY_DATA_GPUS_USED_BY_XSCREEN,
                                          (unsigned char **) &ptr_binary_data, &data_length);
      if (!err)
        continue;

      gpu_number = (int)ptr_binary_data[1];
      free(ptr_binary_data);

#ifdef NV_CTRL_PCI_DOMAIN
      /* Gets the ID's of the GPU defined by gpu_number
       * For further details, see the <NVCtrl/NVCtrlLib.h> */
      err = XNVCTRLQueryTargetAttribute(display, NV_CTRL_TARGET_TYPE_GPU, gpu_number, 0,
                                        NV_CTRL_PCI_DOMAIN, &nv_ctrl_pci_domain);
      if (!err)
        continue;
#else
      nv_ctrl_pci_domain = 0;
#endif

      err = XNVCTRLQueryTargetAttribute(display, NV_CTRL_TARGET_TYPE_GPU, gpu_number, 0,
                                        NV_CTRL_PCI_BUS, &nv_ctrl_pci_bus);
      if (!err)
        continue;

      err = XNVCTRLQueryTargetAttribute(display, NV_CTRL_TARGET_TYPE_GPU, gpu_number, 0,
                                        NV_CTRL_PCI_DEVICE, &nv_ctrl_pci_device);
      if (!err)
        continue;

      err = XNVCTRLQueryTargetAttribute(display, NV_CTRL_TARGET_TYPE_GPU, gpu_number, 0,
                                        NV_CTRL_PCI_FUNCTION, &nv_ctrl_pci_func);
      if (!err)
        continue;

      productname = NULL;
      err = XNVCTRLQueryTargetStringAttribute(display, NV_CTRL_TARGET_TYPE_GPU, gpu_number, 0,
                                              NV_CTRL_STRING_PRODUCT_NAME, &productname);

      snprintf(name, sizeof(name), ":%u.%u", i, j);

      osdev = hwloc_alloc_setup_object(topology, HWLOC_OBJ_OS_DEVICE, HWLOC_UNKNOWN_INDEX);
      osdev->name = strdup(name);
      osdev->attr->osdev.type = HWLOC_OBJ_OSDEV_GPU;
      hwloc_obj_add_info(osdev, "Backend", "GL");
      hwloc_obj_add_info(osdev, "GPUVendor", "NVIDIA Corporation");
      if (productname)
	hwloc_obj_add_info(osdev, "GPUModel", productname);

      parent = hwloc_pcidisc_find_by_busid(topology, (unsigned)nv_ctrl_pci_domain, (unsigned)nv_ctrl_pci_bus, (unsigned)nv_ctrl_pci_device, (unsigned)nv_ctrl_pci_func);
      if (!parent)
	parent = hwloc_pcidisc_find_busid_parent(topology, (unsigned)nv_ctrl_pci_domain, (unsigned)nv_ctrl_pci_bus, (unsigned)nv_ctrl_pci_device, (unsigned)nv_ctrl_pci_func);
      if (!parent)
	parent = hwloc_get_root_obj(topology);

      hwloc_insert_object_by_parent(topology, parent, osdev);

      hwloc_debug("GL device %s (product %s) on PCI %04x:%02x:%02x.%01x\n",
		  name, productname,
		  (unsigned) nv_ctrl_pci_domain, (unsigned) nv_ctrl_pci_bus, (unsigned) nv_ctrl_pci_device, (unsigned) nv_ctrl_pci_func);
    }
    XCloseDisplay(display);
  }

  return 0;
}

static struct hwloc_backend *
hwloc_gl_component_instantiate(struct hwloc_disc_component *component,
			       const void *_data1 __hwloc_attribute_unused,
			       const void *_data2 __hwloc_attribute_unused,
			       const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;

  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->discover = hwloc_gl_discover;
  return backend;
}

static struct hwloc_disc_component hwloc_gl_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC,
  "gl",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_gl_component_instantiate,
  10, /* after pci */
  1,
  NULL
};

static int
hwloc_gl_component_init(unsigned long flags)
{
  if (flags)
    return -1;
  if (hwloc_plugin_check_namespace("gl", "hwloc_backend_alloc") < 0)
    return -1;
  return 0;
}

#ifdef HWLOC_INSIDE_PLUGIN
HWLOC_DECLSPEC extern const struct hwloc_component hwloc_gl_component;
#endif

const struct hwloc_component hwloc_gl_component = {
  HWLOC_COMPONENT_ABI,
  hwloc_gl_component_init, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_gl_disc_component
};
