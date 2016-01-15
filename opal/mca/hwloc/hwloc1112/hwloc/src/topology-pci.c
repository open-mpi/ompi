/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2015 Inria.  All rights reserved.
 * Copyright © 2009-2011, 2013 Université Bordeaux
 * Copyright © 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright © 2015      Research Organization for Information Science
 *                       and Technology (RIST). All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/helper.h>
#include <hwloc/plugins.h>

/* private headers allowed for convenience because this plugin is built within hwloc */
#include <private/debug.h>
#include <private/misc.h>

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <setjmp.h>
#ifdef HWLOC_LINUX_SYS
#include <dirent.h>
#endif

#include <pciaccess.h>

#ifndef PCI_HEADER_TYPE
#define PCI_HEADER_TYPE 0x0e
#endif
#ifndef PCI_HEADER_TYPE_BRIDGE
#define PCI_HEADER_TYPE_BRIDGE 1
#endif

#ifndef PCI_CLASS_DEVICE
#define PCI_CLASS_DEVICE 0x0a
#endif
#ifndef PCI_CLASS_BRIDGE_PCI
#define PCI_CLASS_BRIDGE_PCI 0x0604
#endif

#ifndef PCI_REVISION_ID
#define PCI_REVISION_ID 0x08
#endif

#ifndef PCI_SUBSYSTEM_VENDOR_ID
#define PCI_SUBSYSTEM_VENDOR_ID 0x2c
#endif
#ifndef PCI_SUBSYSTEM_ID
#define PCI_SUBSYSTEM_ID 0x2e
#endif

#ifndef PCI_PRIMARY_BUS
#define PCI_PRIMARY_BUS 0x18
#endif
#ifndef PCI_SECONDARY_BUS
#define PCI_SECONDARY_BUS 0x19
#endif
#ifndef PCI_SUBORDINATE_BUS
#define PCI_SUBORDINATE_BUS 0x1a
#endif

#ifndef PCI_CAP_ID_EXP
#define PCI_CAP_ID_EXP 0x10
#endif

#ifndef PCI_CAP_NORMAL
#define PCI_CAP_NORMAL 1
#endif

#define CONFIG_SPACE_CACHESIZE 256


static int
hwloc_look_pci(struct hwloc_backend *backend)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_obj *first_obj = NULL, *last_obj = NULL;
  int ret;
  struct pci_device_iterator *iter;
  struct pci_device *pcidev;
#ifdef HWLOC_LINUX_SYS
  DIR *dir;
#endif

  if (!(hwloc_topology_get_flags(topology) & (HWLOC_TOPOLOGY_FLAG_IO_DEVICES|HWLOC_TOPOLOGY_FLAG_WHOLE_IO)))
    return 0;

  if (hwloc_get_next_pcidev(topology, NULL)) {
    hwloc_debug("%s", "PCI objects already added, ignoring pci backend.\n");
    return 0;
  }

  if (!hwloc_topology_is_thissystem(topology)) {
    hwloc_debug("%s", "\nno PCI detection (not thissystem)\n");
    return 0;
  }

  hwloc_debug("%s", "\nScanning PCI buses...\n");

  /* initialize PCI scanning */
  ret = pci_system_init();
  if (ret) {
    hwloc_debug("%s", "Can not initialize libpciaccess\n");
    return -1;
  }

  iter = pci_slot_match_iterator_create(NULL);

  /* iterate over devices */
  for (pcidev = pci_device_next(iter);
       pcidev;
       pcidev = pci_device_next(iter))
  {
    const char *vendorname, *devicename, *fullname;
    unsigned char config_space_cache[CONFIG_SPACE_CACHESIZE];
    struct hwloc_obj *obj;
    unsigned os_index;
    unsigned domain;
    unsigned device_class;
    unsigned short tmp16;
    char name[128];
    unsigned offset;

    /* initialize the config space in case we fail to read it (missing permissions, etc). */
    memset(config_space_cache, 0xff, CONFIG_SPACE_CACHESIZE);
    pci_device_probe(pcidev);
    pci_device_cfg_read(pcidev, config_space_cache, 0, CONFIG_SPACE_CACHESIZE, NULL);

    /* try to read the domain */
    domain = pcidev->domain;

    /* try to read the device_class */
    device_class = pcidev->device_class >> 8;

    /* fixup SR-IOV buggy VF device/vendor IDs */
    if (0xffff == pcidev->vendor_id && 0xffff == pcidev->device_id) {
      /* SR-IOV puts ffff:ffff in Virtual Function config space.
       * The actual VF device ID is stored at a special (dynamic) location in the Physical Function config space.
       * VF and PF have the same vendor ID.
       *
       * libpciaccess just returns ffff:ffff, needs to be fixed.
       * linuxpci is OK because sysfs files are already fixed the kernel.
       * (pciutils is OK when it uses those Linux sysfs files.)
       *
       * Reading these files is an easy way to work around the libpciaccess issue on Linux,
       * but we have no way to know if this is caused by SR-IOV or not.
       *
       * TODO:
       *  If PF has CAP_ID_PCIX or CAP_ID_EXP (offset>0),
       *  look for extended capability PCI_EXT_CAP_ID_SRIOV (need extended config space (more than 256 bytes)),
       *  then read the VF device ID after it (PCI_IOV_DID bytes later).
       *  Needs access to extended config space (needs root on Linux).
       * TODO:
       *  Add string info attributes in VF and PF objects?
       */
#ifdef HWLOC_LINUX_SYS
      /* Workaround for Linux (the kernel returns the VF device/vendor IDs). */
      char path[64];
      char value[16];
      FILE *file;
      size_t read;

      snprintf(path, sizeof(path), "/sys/bus/pci/devices/%04x:%02x:%02x.%01x/vendor",
	       domain, pcidev->bus, pcidev->dev, pcidev->func);
      file = fopen(path, "r");
      if (file) {
	read = fread(value, 1, sizeof(value), file);
	fclose(file);
	if (read)
	  /* fixup the pciaccess struct so that pci_device_get_vendor_name() is correct later. */
          pcidev->vendor_id = strtoul(value, NULL, 16);
      }

      snprintf(path, sizeof(path), "/sys/bus/pci/devices/%04x:%02x:%02x.%01x/device",
	       domain, pcidev->bus, pcidev->dev, pcidev->func);
      file = fopen(path, "r");
      if (file) {
	read = fread(value, 1, sizeof(value), file);
	fclose(file);
	if (read)
	  /* fixup the pciaccess struct so that pci_device_get_device_name() is correct later. */
          pcidev->device_id = strtoul(value, NULL, 16);
      }
#endif
    }

    /* might be useful for debugging (note that domain might be truncated) */
    os_index = (domain << 20) + (pcidev->bus << 12) + (pcidev->dev << 4) + pcidev->func;

    obj = hwloc_alloc_setup_object(HWLOC_OBJ_PCI_DEVICE, os_index);
    obj->attr->pcidev.domain = domain;
    obj->attr->pcidev.bus = pcidev->bus;
    obj->attr->pcidev.dev = pcidev->dev;
    obj->attr->pcidev.func = pcidev->func;
    obj->attr->pcidev.vendor_id = pcidev->vendor_id;
    obj->attr->pcidev.device_id = pcidev->device_id;
    obj->attr->pcidev.class_id = device_class;
    obj->attr->pcidev.revision = config_space_cache[PCI_REVISION_ID];

    obj->attr->pcidev.linkspeed = 0; /* unknown */
    offset = hwloc_pci_find_cap(config_space_cache, PCI_CAP_ID_EXP);

    if (offset > 0 && offset + 20 /* size of PCI express block up to link status */ <= CONFIG_SPACE_CACHESIZE)
      hwloc_pci_find_linkspeed(config_space_cache, offset, &obj->attr->pcidev.linkspeed);

    if (hwloc_pci_prepare_bridge(obj, config_space_cache) < 0)
      continue;

    if (obj->type == HWLOC_OBJ_PCI_DEVICE) {
      memcpy(&tmp16, &config_space_cache[PCI_SUBSYSTEM_VENDOR_ID], sizeof(tmp16));
      obj->attr->pcidev.subvendor_id = tmp16;
      memcpy(&tmp16, &config_space_cache[PCI_SUBSYSTEM_ID], sizeof(tmp16));
      obj->attr->pcidev.subdevice_id = tmp16;
    } else {
      /* TODO:
       * bridge must lookup PCI_CAP_ID_SSVID and then look at offset+PCI_SSVID_VENDOR/DEVICE_ID
       * cardbus must look at PCI_CB_SUBSYSTEM_VENDOR_ID and PCI_CB_SUBSYSTEM_ID
       */
    }

    /* get the vendor name */
    vendorname = pci_device_get_vendor_name(pcidev);
    if (vendorname && *vendorname)
      hwloc_obj_add_info(obj, "PCIVendor", vendorname);

    /* get the device name */
    devicename = pci_device_get_device_name(pcidev);
    if (devicename && *devicename)
      hwloc_obj_add_info(obj, "PCIDevice", devicename);

    /* generate or get the fullname */
    snprintf(name, sizeof(name), "%s%s%s",
	     vendorname ? vendorname : "",
	     vendorname && devicename ? " " : "",
	     devicename ? devicename : "");
    fullname = name;
    if (*name)
      obj->name = strdup(name);
    hwloc_debug("  %04x:%02x:%02x.%01x %04x %04x:%04x %s\n",
		domain, pcidev->bus, pcidev->dev, pcidev->func,
		device_class, pcidev->vendor_id, pcidev->device_id,
		fullname && *fullname ? fullname : "??");

    /* queue the object for now */
    if (first_obj)
      last_obj->next_sibling = obj;
    else
      first_obj = obj;
    last_obj = obj;
  }

  /* finalize device scanning */
  pci_iterator_destroy(iter);
  pci_system_cleanup();

#ifdef HWLOC_LINUX_SYS
  dir = opendir("/sys/bus/pci/slots/");
  if (dir) {
    struct dirent *dirent;
    while ((dirent = readdir(dir)) != NULL) {
      char path[64];
      FILE *file;
      if (dirent->d_name[0] == '.')
	continue;
      snprintf(path, sizeof(path), "/sys/bus/pci/slots/%s/address", dirent->d_name);
      file = fopen(path, "r");
      if (file) {
	unsigned domain, bus, dev;
	if (fscanf(file, "%x:%x:%x", &domain, &bus, &dev) == 3) {
	  hwloc_obj_t obj = first_obj;
	  while (obj) {
	    if (obj->attr->pcidev.domain == domain
		&& obj->attr->pcidev.bus == bus
		&& obj->attr->pcidev.dev == dev) {
	      hwloc_obj_add_info(obj, "PCISlot", dirent->d_name);
	    }
	    obj = obj->next_sibling;
	  }
	}
	fclose(file);
      }
    }
    closedir(dir);
  }
#endif

  return hwloc_insert_pci_device_list(backend, first_obj);
}

static struct hwloc_backend *
hwloc_pci_component_instantiate(struct hwloc_disc_component *component,
				   const void *_data1 __hwloc_attribute_unused,
				   const void *_data2 __hwloc_attribute_unused,
				   const void *_data3 __hwloc_attribute_unused)
{
  struct hwloc_backend *backend;

  /* thissystem may not be fully initialized yet, we'll check flags in discover() */

  backend = hwloc_backend_alloc(component);
  if (!backend)
    return NULL;
  backend->flags = HWLOC_BACKEND_FLAG_NEED_LEVELS;
#ifdef HWLOC_SOLARIS_SYS
  if ((uid_t)0 != geteuid())
    backend->discover = NULL;
  else
#endif
    backend->discover = hwloc_look_pci;
  return backend;
}

static struct hwloc_disc_component hwloc_pci_disc_component = {
  HWLOC_DISC_COMPONENT_TYPE_MISC,
  "pci",
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL,
  hwloc_pci_component_instantiate,
  20,
  NULL
};

static int
hwloc_pci_component_init(unsigned long flags)
{
  if (flags)
    return -1;
  if (hwloc_plugin_check_namespace("pci", "hwloc_backend_alloc") < 0)
    return -1;
  return 0;
}

#ifdef HWLOC_INSIDE_PLUGIN
HWLOC_DECLSPEC extern const struct hwloc_component hwloc_pci_component;
#endif

const struct hwloc_component hwloc_pci_component = {
  HWLOC_COMPONENT_ABI,
  hwloc_pci_component_init, NULL,
  HWLOC_COMPONENT_TYPE_DISC,
  0,
  &hwloc_pci_disc_component
};
