/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2015 Inria.  All rights reserved.
 * Copyright © Université Bordeaux
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>

#include <assert.h>

/* check misc i/O device related stuff */

int main(void)
{
  hwloc_topology_t topology;
  hwloc_obj_t obj;

  hwloc_topology_init(&topology);
  hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_ALL);
  hwloc_topology_load(topology);

  printf("Found %d bridges\n", hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_BRIDGE));
  obj = NULL;
  while ((obj = hwloc_get_next_bridge(topology, obj)) != NULL) {
    assert(obj->type == HWLOC_OBJ_BRIDGE);
    /* only host->pci and pci->pci bridge supported so far */
    if (obj->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_HOST) {
      assert(obj->attr->bridge.downstream_type == HWLOC_OBJ_BRIDGE_PCI);
      printf(" Found host->PCI bridge for domain %04x bus %02x-%02x\n",
	     obj->attr->bridge.downstream.pci.domain,
	     obj->attr->bridge.downstream.pci.secondary_bus,
	     obj->attr->bridge.downstream.pci.subordinate_bus);
    } else {
      assert(obj->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI);
      assert(obj->attr->bridge.downstream_type == HWLOC_OBJ_BRIDGE_PCI);
      printf(" Found PCI->PCI bridge [%04x:%04x] for domain %04x bus %02x-%02x\n",
	     obj->attr->bridge.upstream.pci.vendor_id,
	     obj->attr->bridge.upstream.pci.device_id,
	     obj->attr->bridge.downstream.pci.domain,
	     obj->attr->bridge.downstream.pci.secondary_bus,
	     obj->attr->bridge.downstream.pci.subordinate_bus);
    }
  }

  printf("Found %d PCI devices\n", hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_PCI_DEVICE));
  obj = NULL;
  while ((obj = hwloc_get_next_pcidev(topology, obj)) != NULL) {
    assert(obj->type == HWLOC_OBJ_PCI_DEVICE);
    printf(" Found PCI device class %04x vendor %04x model %04x\n",
	   obj->attr->pcidev.class_id, obj->attr->pcidev.vendor_id, obj->attr->pcidev.device_id);
  }

  printf("Found %d OS devices\n", hwloc_get_nbobjs_by_type(topology, HWLOC_OBJ_OS_DEVICE));
  obj = NULL;
  while ((obj = hwloc_get_next_osdev(topology, obj)) != NULL) {
    assert(obj->type == HWLOC_OBJ_OS_DEVICE);
    printf(" Found OS device %s subtype %d\n", obj->name, obj->attr->osdev.type);
  }

  assert(HWLOC_TYPE_DEPTH_BRIDGE == hwloc_get_type_depth(topology, HWLOC_OBJ_BRIDGE));
  assert(HWLOC_TYPE_DEPTH_PCI_DEVICE == hwloc_get_type_depth(topology, HWLOC_OBJ_PCI_DEVICE));
  assert(HWLOC_TYPE_DEPTH_OS_DEVICE == hwloc_get_type_depth(topology, HWLOC_OBJ_OS_DEVICE));
  assert(hwloc_compare_types(HWLOC_OBJ_BRIDGE, HWLOC_OBJ_PCI_DEVICE) < 0);
  assert(hwloc_compare_types(HWLOC_OBJ_BRIDGE, HWLOC_OBJ_OS_DEVICE) < 0);
  assert(hwloc_compare_types(HWLOC_OBJ_PCI_DEVICE, HWLOC_OBJ_OS_DEVICE) < 0);

  hwloc_topology_destroy(topology);

  return 0;
}
