/*
 * Copyright © 2012-2013 Blue Brain Project, BBP/EPFL. All rights reserved.
 * Copyright © 2012-2013 Inria.  All rights reserved.
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
struct hwloc_gl_backend_data_s {
  unsigned nr_display;
  struct hwloc_gl_display_info_s {
    char name[10];
    unsigned port, device;
    unsigned pcidomain, pcibus, pcidevice, pcifunc;
    char *productname;
  } display[HWLOC_GL_SERVER_MAX*HWLOC_GL_SCREEN_MAX];
};

static void
hwloc_gl_query_devices(struct hwloc_gl_backend_data_s *data)
{
  int err;
  unsigned i,j;

  /* mark the number of display as 0 in case we fail below,
   * so that we don't try again later.
   */
  data->nr_display = 0;

  for (i = 0; i < HWLOC_GL_SERVER_MAX; ++i) {
    Display* display;
    char displayName[10];
    int opcode, event, error;

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
      struct hwloc_gl_display_info_s *info = &data->display[data->nr_display];
      const int screen = j;
      unsigned int *ptr_binary_data;
      int data_length;
      int gpu_number;
      int nv_ctrl_pci_bus;
      int nv_ctrl_pci_device;
      int nv_ctrl_pci_domain;
      int nv_ctrl_pci_func;
      char *productname;

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

      gpu_number = ptr_binary_data[1];
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

      snprintf(info->name, sizeof(info->name), ":%u.%u", i, j);
      info->port = i;
      info->device = j;
      info->pcidomain = nv_ctrl_pci_domain;
      info->pcibus = nv_ctrl_pci_bus;
      info->pcidevice = nv_ctrl_pci_device;
      info->pcifunc = nv_ctrl_pci_func;
      info->productname = productname;

      hwloc_debug("GL device %s (product %s) on PCI 0000:%02x:%02x.%u\n", info->name, productname,
		  nv_ctrl_pci_domain, nv_ctrl_pci_bus, nv_ctrl_pci_device, nv_ctrl_pci_func);

      /* validate this device */
      data->nr_display++;
    }
    XCloseDisplay(display);
  }
}

static int
hwloc_gl_backend_notify_new_object(struct hwloc_backend *backend, struct hwloc_backend *caller __hwloc_attribute_unused,
				   struct hwloc_obj *pcidev)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_gl_backend_data_s *data = backend->private_data;
  unsigned i, res;

  if (!(hwloc_topology_get_flags(topology) & (HWLOC_TOPOLOGY_FLAG_IO_DEVICES|HWLOC_TOPOLOGY_FLAG_WHOLE_IO)))
    return 0;

  if (!hwloc_topology_is_thissystem(topology)) {
    hwloc_debug("%s", "\nno GL detection (not thissystem)\n");
    return 0;
  }

  if (HWLOC_OBJ_PCI_DEVICE != pcidev->type)
    return 0;

  if (data->nr_display == (unsigned) -1) {
    /* first call, lookup all display */
    hwloc_gl_query_devices(data);
    /* if it fails, data->nr_display = 0 so we won't do anything below and in next callbacks */
  }

  if (!data->nr_display)
    /* found no display */
    return 0;

  /* now the display array is ready to use */
  res = 0;
  for(i=0; i<data->nr_display; i++) {
    struct hwloc_gl_display_info_s *info = &data->display[i];
    hwloc_obj_t osdev;

    if (info->pcidomain != pcidev->attr->pcidev.domain)
      continue;
    if (info->pcibus != pcidev->attr->pcidev.bus)
      continue;
    if (info->pcidevice != pcidev->attr->pcidev.dev)
      continue;
    if (info->pcifunc != pcidev->attr->pcidev.func)
      continue;

    osdev = hwloc_alloc_setup_object(HWLOC_OBJ_OS_DEVICE, -1);
    osdev->name = strdup(info->name);
    osdev->logical_index = -1;
    osdev->attr->osdev.type = HWLOC_OBJ_OSDEV_GPU;
    hwloc_obj_add_info(osdev, "Backend", "GL");
    hwloc_obj_add_info(osdev, "GPUVendor", "NVIDIA Corporation");
    if (info->productname)
      hwloc_obj_add_info(osdev, "GPUModel", info->productname);
    hwloc_insert_object_by_parent(topology, pcidev, osdev);

    res++;
    /* there may be others */
  }

  return res;
}

static void
hwloc_gl_backend_disable(struct hwloc_backend *backend)
{
  struct hwloc_gl_backend_data_s *data = backend->private_data;
  unsigned i;
  if (data->nr_display != (unsigned) -1) { /* could be -1 if --no-io */
    for(i=0; i<data->nr_display; i++) {
      struct hwloc_gl_display_info_s *info = &data->display[i];
      free(info->productname);
    }
  }
  free(backend->private_data);
}

static struct hwloc_backend *
hwloc_gl_component_instantiate(struct hwloc_disc_component *component,
			       const void *_data1 __hwloc_attribute_unused,
			       const void *_data2 __hwloc_attribute_unused,
			       const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;
  struct hwloc_gl_backend_data_s *data;

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
  data->nr_display = (unsigned) -1; /* unknown yet */

  backend->private_data = data;
  backend->disable = hwloc_gl_backend_disable;

  backend->notify_new_object = hwloc_gl_backend_notify_new_object;
  return backend;
}

static struct hwloc_disc_component hwloc_gl_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC,
  "gl",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_gl_component_instantiate,
  10, /* after pci */
  NULL
};

#ifdef HWLOC_INSIDE_PLUGIN
HWLOC_DECLSPEC extern const struct hwloc_component hwloc_gl_component;
#endif

const struct hwloc_component hwloc_gl_component = {
  HWLOC_COMPONENT_ABI,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_gl_disc_component
};
