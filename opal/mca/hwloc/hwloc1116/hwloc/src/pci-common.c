/*
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <hwloc/plugins.h>
#include <private/private.h>
#include <private/debug.h>
#include <private/misc.h>

#ifdef HWLOC_DEBUG
static void
hwloc_pci_traverse_print_cb(void * cbdata __hwloc_attribute_unused,
			    struct hwloc_obj *pcidev)
{
  char busid[14];
  hwloc_obj_t parent;

  /* indent */
  parent = pcidev->parent;
  while (parent) {
    hwloc_debug("%s", "  ");
    parent = parent->parent;
  }

  snprintf(busid, sizeof(busid), "%04x:%02x:%02x.%01x",
           pcidev->attr->pcidev.domain, pcidev->attr->pcidev.bus, pcidev->attr->pcidev.dev, pcidev->attr->pcidev.func);

  if (pcidev->type == HWLOC_OBJ_BRIDGE) {
    if (pcidev->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_HOST)
      hwloc_debug("HostBridge");
    else
      hwloc_debug("Bridge [%04x:%04x]", busid,
		  pcidev->attr->pcidev.vendor_id, pcidev->attr->pcidev.device_id);
    hwloc_debug(" to %04x:[%02x:%02x]\n",
		pcidev->attr->bridge.downstream.pci.domain, pcidev->attr->bridge.downstream.pci.secondary_bus, pcidev->attr->bridge.downstream.pci.subordinate_bus);
  } else
    hwloc_debug("%s Device [%04x:%04x (%04x:%04x) rev=%02x class=%04x]\n", busid,
		pcidev->attr->pcidev.vendor_id, pcidev->attr->pcidev.device_id,
		pcidev->attr->pcidev.subvendor_id, pcidev->attr->pcidev.subdevice_id,
		pcidev->attr->pcidev.revision, pcidev->attr->pcidev.class_id);
}
#endif /* HWLOC_DEBUG */

static void
hwloc_pci_traverse_lookuposdevices_cb(void * cbdata,
				      struct hwloc_obj *pcidev)
{
  struct hwloc_backend *backend = cbdata;

  if (pcidev->type == HWLOC_OBJ_BRIDGE)
    return;

  hwloc_backends_notify_new_object(backend, pcidev);
}

static void
hwloc_pci__traverse(void * cbdata, struct hwloc_obj *root,
		    void (*cb)(void * cbdata, struct hwloc_obj *))
{
  struct hwloc_obj *child = root->first_child;
  while (child) {
    cb(cbdata, child);
    if (child->type == HWLOC_OBJ_BRIDGE)
      hwloc_pci__traverse(cbdata, child, cb);
    child = child->next_sibling;
  }
}

static void
hwloc_pci_traverse(void * cbdata, struct hwloc_obj *root,
		   void (*cb)(void * cbdata, struct hwloc_obj *))
{
  hwloc_pci__traverse(cbdata, root, cb);
}

enum hwloc_pci_busid_comparison_e {
  HWLOC_PCI_BUSID_LOWER,
  HWLOC_PCI_BUSID_HIGHER,
  HWLOC_PCI_BUSID_INCLUDED,
  HWLOC_PCI_BUSID_SUPERSET
};

static enum hwloc_pci_busid_comparison_e
hwloc_pci_compare_busids(struct hwloc_obj *a, struct hwloc_obj *b)
{
  if (a->type == HWLOC_OBJ_BRIDGE)
    assert(a->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI);
  if (b->type == HWLOC_OBJ_BRIDGE)
    assert(b->attr->bridge.upstream_type == HWLOC_OBJ_BRIDGE_PCI);

  if (a->attr->pcidev.domain < b->attr->pcidev.domain)
    return HWLOC_PCI_BUSID_LOWER;
  if (a->attr->pcidev.domain > b->attr->pcidev.domain)
    return HWLOC_PCI_BUSID_HIGHER;

  if (a->type == HWLOC_OBJ_BRIDGE
      && b->attr->pcidev.bus >= a->attr->bridge.downstream.pci.secondary_bus
      && b->attr->pcidev.bus <= a->attr->bridge.downstream.pci.subordinate_bus)
    return HWLOC_PCI_BUSID_SUPERSET;
  if (b->type == HWLOC_OBJ_BRIDGE
      && a->attr->pcidev.bus >= b->attr->bridge.downstream.pci.secondary_bus
      && a->attr->pcidev.bus <= b->attr->bridge.downstream.pci.subordinate_bus)
    return HWLOC_PCI_BUSID_INCLUDED;

  if (a->attr->pcidev.bus < b->attr->pcidev.bus)
    return HWLOC_PCI_BUSID_LOWER;
  if (a->attr->pcidev.bus > b->attr->pcidev.bus)
    return HWLOC_PCI_BUSID_HIGHER;

  if (a->attr->pcidev.dev < b->attr->pcidev.dev)
    return HWLOC_PCI_BUSID_LOWER;
  if (a->attr->pcidev.dev > b->attr->pcidev.dev)
    return HWLOC_PCI_BUSID_HIGHER;

  if (a->attr->pcidev.func < b->attr->pcidev.func)
    return HWLOC_PCI_BUSID_LOWER;
  if (a->attr->pcidev.func > b->attr->pcidev.func)
    return HWLOC_PCI_BUSID_HIGHER;

  /* Should never reach here.  Abort on both debug builds and
     non-debug builds */
  assert(0);
  fprintf(stderr, "Bad assertion in hwloc %s:%d (aborting)\n", __FILE__, __LINE__);
  exit(1);
}

static void
hwloc_pci_add_child_before(struct hwloc_obj *root, struct hwloc_obj *child, struct hwloc_obj *new)
{
  if (child) {
    new->prev_sibling = child->prev_sibling;
    child->prev_sibling = new;
  } else {
    new->prev_sibling = root->last_child;
    root->last_child = new;
  }

  if (new->prev_sibling)
    new->prev_sibling->next_sibling = new;
  else
    root->first_child = new;
  new->next_sibling = child;

  new->parent = root; /* so that hwloc_pci_traverse_print_cb() can indent by depth */
}

static void
hwloc_pci_remove_child(struct hwloc_obj *root, struct hwloc_obj *child)
{
  if (child->next_sibling)
    child->next_sibling->prev_sibling = child->prev_sibling;
  else
    root->last_child = child->prev_sibling;
  if (child->prev_sibling)
    child->prev_sibling->next_sibling = child->next_sibling;
  else
    root->first_child = child->next_sibling;
  child->prev_sibling = NULL;
  child->next_sibling = NULL;
}

static void hwloc_pci_add_object(struct hwloc_obj *root, struct hwloc_obj *new);

static void
hwloc_pci_try_insert_siblings_below_new_bridge(struct hwloc_obj *root, struct hwloc_obj *new)
{
  enum hwloc_pci_busid_comparison_e comp;
  struct hwloc_obj *current, *next;

  next = new->next_sibling;
  while (next) {
    current = next;
    next = current->next_sibling;

    comp = hwloc_pci_compare_busids(current, new);
    assert(comp != HWLOC_PCI_BUSID_SUPERSET);
    if (comp == HWLOC_PCI_BUSID_HIGHER)
      continue;
    assert(comp == HWLOC_PCI_BUSID_INCLUDED);

    /* move this object below the new bridge */
    hwloc_pci_remove_child(root, current);
    hwloc_pci_add_object(new, current);
  }
}

static void
hwloc_pci_add_object(struct hwloc_obj *root, struct hwloc_obj *new)
{
  struct hwloc_obj *current;

  current = root->first_child;
  while (current) {
    enum hwloc_pci_busid_comparison_e comp = hwloc_pci_compare_busids(new, current);
    switch (comp) {
    case HWLOC_PCI_BUSID_HIGHER:
      /* go further */
      current = current->next_sibling;
      continue;
    case HWLOC_PCI_BUSID_INCLUDED:
      /* insert below current bridge */
      hwloc_pci_add_object(current, new);
      return;
    case HWLOC_PCI_BUSID_LOWER:
    case HWLOC_PCI_BUSID_SUPERSET:
      /* insert before current object */
      hwloc_pci_add_child_before(root, current, new);
      /* walk next siblings and move them below new bridge if needed */
      hwloc_pci_try_insert_siblings_below_new_bridge(root, new);
      return;
    }
  }
  /* add to the end of the list if higher than everybody */
  hwloc_pci_add_child_before(root, NULL, new);
}

static struct hwloc_obj *
hwloc_pci_fixup_hostbridge_parent(struct hwloc_topology *topology __hwloc_attribute_unused,
				  struct hwloc_obj *hostbridge,
				  struct hwloc_obj *parent)
{
  /* Xeon E5v3 in cluster-on-die mode only have PCI on the first NUMA node of each package.
   * but many dual-processor host report the second PCI hierarchy on 2nd NUMA of first package.
   */
  if (parent->depth >= 2
      && parent->type == HWLOC_OBJ_NUMANODE
      && parent->sibling_rank == 1 && parent->parent->arity == 2
      && parent->parent->type == HWLOC_OBJ_PACKAGE
      && parent->parent->sibling_rank == 0 && parent->parent->parent->arity == 2) {
    const char *cpumodel = hwloc_obj_get_info_by_name(parent->parent, "CPUModel");
    if (cpumodel && strstr(cpumodel, "Xeon")) {
      if (!hwloc_hide_errors()) {
	fprintf(stderr, "****************************************************************************\n");
	fprintf(stderr, "* hwloc %s has encountered an incorrect PCI locality information.\n", HWLOC_VERSION);
	fprintf(stderr, "* PCI bus %04x:%02x is supposedly close to 2nd NUMA node of 1st package,\n",
		hostbridge->first_child->attr->pcidev.domain, hostbridge->first_child->attr->pcidev.bus);
	fprintf(stderr, "* however hwloc believes this is impossible on this architecture.\n");
	fprintf(stderr, "* Therefore the PCI bus will be moved to 1st NUMA node of 2nd package.\n");
	fprintf(stderr, "*\n");
	fprintf(stderr, "* If you feel this fixup is wrong, disable it by setting in your environment\n");
	fprintf(stderr, "* HWLOC_PCI_%04x_%02x_LOCALCPUS= (empty value), and report the problem\n",
		hostbridge->first_child->attr->pcidev.domain, hostbridge->first_child->attr->pcidev.bus);
	fprintf(stderr, "* to the hwloc's user mailing list together with the XML output of lstopo.\n");
	fprintf(stderr, "*\n");
	fprintf(stderr, "* You may silence this message by setting HWLOC_HIDE_ERRORS=1 in your environment.\n");
	fprintf(stderr, "****************************************************************************\n");
      }
      return parent->parent->next_sibling->first_child;
    }
  }

  return parent;
}

static struct hwloc_obj *
hwloc_pci_find_hostbridge_parent(struct hwloc_topology *topology, struct hwloc_backend *backend,
				 struct hwloc_obj *hostbridge)
{
  hwloc_bitmap_t cpuset = hwloc_bitmap_alloc();
  struct hwloc_obj *parent;
  const char *env;
  int err;

  /* override the cpuset with the environment if given */
  int forced = 0;
  char envname[256];
  snprintf(envname, sizeof(envname), "HWLOC_PCI_%04x_%02x_LOCALCPUS",
	   hostbridge->first_child->attr->pcidev.domain, hostbridge->first_child->attr->pcidev.bus);
  env = getenv(envname);
  if (env)
    /* if env exists but is empty, don't let quirks change what the OS reports */
    forced = 1;
  if (env && *env) {
    /* force the hostbridge cpuset */
    hwloc_debug("Overriding localcpus using %s in the environment\n", envname);
    hwloc_bitmap_sscanf(cpuset, env);
  } else {
    /* get the hostbridge cpuset by acking the OS backend.
     * it's not a PCI device, so we use its first child locality info.
     */
    err = hwloc_backends_get_obj_cpuset(backend, hostbridge->first_child, cpuset);
    if (err < 0)
      /* if we got nothing, assume the hostbridge is attached to the top of hierarchy */
      hwloc_bitmap_copy(cpuset, hwloc_topology_get_topology_cpuset(topology));
  }

  hwloc_debug_bitmap("Attaching hostbridge to cpuset %s\n", cpuset);

  /* restrict to the existing topology cpuset to avoid errors later */
  hwloc_bitmap_and(cpuset, cpuset, hwloc_topology_get_topology_cpuset(topology));

  /* if the remaining cpuset is empty, take the root */
  if (hwloc_bitmap_iszero(cpuset))
    hwloc_bitmap_copy(cpuset, hwloc_topology_get_topology_cpuset(topology));

  /* attach the hostbridge now that it contains the right objects */
  parent = hwloc_get_obj_covering_cpuset(topology, cpuset);
  /* in the worst case, we got the root object */

  if (hwloc_bitmap_isequal(cpuset, parent->cpuset)) {
    /* this object has the right cpuset, but it could be a cache or so,
     * go up as long as the cpuset is the same
     */
    while (parent->parent && hwloc_bitmap_isequal(parent->cpuset, parent->parent->cpuset))
      parent = parent->parent;

    if (!forced)
      parent = hwloc_pci_fixup_hostbridge_parent(topology, hostbridge, parent);

  } else {
    /* the object we found is too large, insert an intermediate group */
    hwloc_obj_t group_obj = hwloc_alloc_setup_object(HWLOC_OBJ_GROUP, -1);
    if (group_obj) {
      group_obj->cpuset = hwloc_bitmap_dup(cpuset);
      group_obj->complete_cpuset = hwloc_bitmap_dup(cpuset);
      group_obj->attr->group.depth = (unsigned) -1;
      parent = hwloc__insert_object_by_cpuset(topology, group_obj, hwloc_report_os_error);
      if (parent == group_obj)
	/* if didn't get merged, setup its sets */
	hwloc_fill_object_sets(group_obj);
      if (!parent)
	/* Failed to insert the parent, maybe a conflicting cpuset, attach to the root object instead */
	parent = hwloc_get_root_obj(topology);
    }
  }

  hwloc_bitmap_free(cpuset);

  return parent;
}

int
hwloc_insert_pci_device_list(struct hwloc_backend *backend,
			     struct hwloc_obj *first_obj)
{
  struct hwloc_topology *topology = backend->topology;
  struct hwloc_obj fakeparent;
  struct hwloc_obj *obj;
  unsigned current_hostbridge;

  if (!first_obj)
    /* found nothing, exit */
    return 0;

  /* first, organise object as tree under a fake parent object */
  fakeparent.parent = NULL;
  fakeparent.first_child = NULL;
  fakeparent.last_child = NULL;
  while (first_obj) {
    obj = first_obj;
    first_obj = obj->next_sibling;
    hwloc_pci_add_object(&fakeparent, obj);
  }

#ifdef HWLOC_DEBUG
  hwloc_debug("%s", "\nPCI hierarchy under fake parent:\n");
  hwloc_pci_traverse(NULL, &fakeparent, hwloc_pci_traverse_print_cb);
  hwloc_debug("%s", "\n");
#endif

  /* walk the hierarchy, and lookup OS devices */
  hwloc_pci_traverse(backend, &fakeparent, hwloc_pci_traverse_lookuposdevices_cb);

  /*
   * fakeparent lists all objects connected to any upstream bus in the machine.
   * We now create one real hostbridge object per upstream bus.
   * It's not actually a PCI device so we have to create it.
   */
  current_hostbridge = 0;
  while (fakeparent.first_child) {
    /* start a new host bridge */
    struct hwloc_obj *hostbridge = hwloc_alloc_setup_object(HWLOC_OBJ_BRIDGE, current_hostbridge++);
    struct hwloc_obj *child = fakeparent.first_child;
    struct hwloc_obj *next_child;
    struct hwloc_obj *parent;
    unsigned short current_domain = child->attr->pcidev.domain;
    unsigned char current_bus = child->attr->pcidev.bus;
    unsigned char current_subordinate = current_bus;

    hwloc_debug("Starting new PCI hostbridge %04x:%02x\n", current_domain, current_bus);

    /*
     * attach all objects from the same upstream domain/bus
     */
  next_child:
    next_child = child->next_sibling;
    hwloc_pci_remove_child(&fakeparent, child);
    hwloc_pci_add_child_before(hostbridge, NULL, child);

    /* compute hostbridge secondary/subordinate buses */
    if (child->type == HWLOC_OBJ_BRIDGE
	&& child->attr->bridge.downstream.pci.subordinate_bus > current_subordinate)
      current_subordinate = child->attr->bridge.downstream.pci.subordinate_bus;

    /* use next child if it has the same domains/bus */
    child = next_child;
    if (child
	&& child->attr->pcidev.domain == current_domain
	&& child->attr->pcidev.bus == current_bus)
      goto next_child;

    /* finish setting up this hostbridge */
    hostbridge->attr->bridge.upstream_type = HWLOC_OBJ_BRIDGE_HOST;
    hostbridge->attr->bridge.downstream_type = HWLOC_OBJ_BRIDGE_PCI;
    hostbridge->attr->bridge.downstream.pci.domain = current_domain;
    hostbridge->attr->bridge.downstream.pci.secondary_bus = current_bus;
    hostbridge->attr->bridge.downstream.pci.subordinate_bus = current_subordinate;
    hwloc_debug("New PCI hostbridge %04x:[%02x-%02x]\n",
		current_domain, current_bus, current_subordinate);

    /* attach the hostbridge where it belongs */
    parent = hwloc_pci_find_hostbridge_parent(topology, backend, hostbridge);
    hwloc_insert_object_by_parent(topology, parent, hostbridge);
  }

  return 1;
}

#define HWLOC_PCI_STATUS 0x06
#define HWLOC_PCI_STATUS_CAP_LIST 0x10
#define HWLOC_PCI_CAPABILITY_LIST 0x34
#define HWLOC_PCI_CAP_LIST_ID 0
#define HWLOC_PCI_CAP_LIST_NEXT 1

unsigned
hwloc_pci_find_cap(const unsigned char *config, unsigned cap)
{
  unsigned char seen[256] = { 0 };
  unsigned char ptr; /* unsigned char to make sure we stay within the 256-byte config space */

  if (!(config[HWLOC_PCI_STATUS] & HWLOC_PCI_STATUS_CAP_LIST))
    return 0;

  for (ptr = config[HWLOC_PCI_CAPABILITY_LIST] & ~3;
       ptr; /* exit if next is 0 */
       ptr = config[ptr + HWLOC_PCI_CAP_LIST_NEXT] & ~3) {
    unsigned char id;

    /* Looped around! */
    if (seen[ptr])
      break;
    seen[ptr] = 1;

    id = config[ptr + HWLOC_PCI_CAP_LIST_ID];
    if (id == cap)
      return ptr;
    if (id == 0xff) /* exit if id is 0 or 0xff */
      break;
  }
  return 0;
}

#define HWLOC_PCI_EXP_LNKSTA 0x12
#define HWLOC_PCI_EXP_LNKSTA_SPEED 0x000f
#define HWLOC_PCI_EXP_LNKSTA_WIDTH 0x03f0

int
hwloc_pci_find_linkspeed(const unsigned char *config,
			 unsigned offset, float *linkspeed)
{
  unsigned linksta, speed, width;
  float lanespeed;

  memcpy(&linksta, &config[offset + HWLOC_PCI_EXP_LNKSTA], 4);
  speed = linksta & HWLOC_PCI_EXP_LNKSTA_SPEED; /* PCIe generation */
  width = (linksta & HWLOC_PCI_EXP_LNKSTA_WIDTH) >> 4; /* how many lanes */
  /* PCIe Gen1 = 2.5GT/s signal-rate per lane with 8/10 encoding    = 0.25GB/s data-rate per lane
   * PCIe Gen2 = 5  GT/s signal-rate per lane with 8/10 encoding    = 0.5 GB/s data-rate per lane
   * PCIe Gen3 = 8  GT/s signal-rate per lane with 128/130 encoding = 1   GB/s data-rate per lane
   * PCIe Gen4 = 16 GT/s signal-rate per lane with 128/130 encoding = 2   GB/s data-rate per lane
   */

  /* lanespeed in Gbit/s */
  if (speed <= 2)
    lanespeed = 2.5f * speed * 0.8f;
  else
    lanespeed = 8.0f * (1<<(speed-3)) * 128/130; /* assume Gen5 will be 32 GT/s and so on */

  /* linkspeed in GB/s */
  *linkspeed = lanespeed * width / 8;
  return 0;
}

#define HWLOC_PCI_HEADER_TYPE 0x0e
#define HWLOC_PCI_HEADER_TYPE_BRIDGE 1
#define HWLOC_PCI_CLASS_BRIDGE_PCI 0x0604
#define HWLOC_PCI_PRIMARY_BUS 0x18
#define HWLOC_PCI_SECONDARY_BUS 0x19
#define HWLOC_PCI_SUBORDINATE_BUS 0x1a

int
hwloc_pci_prepare_bridge(hwloc_obj_t obj,
			 const unsigned char *config)
{
  unsigned char headertype;
  unsigned isbridge;
  struct hwloc_pcidev_attr_s *pattr = &obj->attr->pcidev;
  struct hwloc_bridge_attr_s *battr;

  headertype = config[HWLOC_PCI_HEADER_TYPE] & 0x7f;
  isbridge = (pattr->class_id == HWLOC_PCI_CLASS_BRIDGE_PCI
	      && headertype == HWLOC_PCI_HEADER_TYPE_BRIDGE);

  if (!isbridge)
    return 0;

  battr = &obj->attr->bridge;

  if (config[HWLOC_PCI_PRIMARY_BUS] != pattr->bus) {
    /* Sometimes the config space contains 00 instead of the actual primary bus number.
     * Always trust the bus ID because it was built by the system which has more information
     * to workaround such problems (e.g. ACPI information about PCI parent/children).
     */
    hwloc_debug("  %04x:%02x:%02x.%01x bridge with (ignored) invalid PCI_PRIMARY_BUS %02x\n",
		pattr->domain, pattr->bus, pattr->dev, pattr->func, config[HWLOC_PCI_PRIMARY_BUS]);
  }

  obj->type = HWLOC_OBJ_BRIDGE;
  battr->upstream_type = HWLOC_OBJ_BRIDGE_PCI;
  battr->downstream_type = HWLOC_OBJ_BRIDGE_PCI;
  battr->downstream.pci.domain = pattr->domain;
  battr->downstream.pci.secondary_bus = config[HWLOC_PCI_SECONDARY_BUS];
  battr->downstream.pci.subordinate_bus = config[HWLOC_PCI_SUBORDINATE_BUS];

  if (battr->downstream.pci.secondary_bus <= pattr->bus
      || battr->downstream.pci.subordinate_bus <= pattr->bus
      || battr->downstream.pci.secondary_bus > battr->downstream.pci.subordinate_bus) {
    /* This should catch most cases of invalid bridge information
     * (e.g. 00 for secondary and subordinate).
     * Ideally we would also check that [secondary-subordinate] is included
     * in the parent bridge [secondary+1:subordinate]. But that's hard to do
     * because objects may be discovered out of order (especially in the fsroot case).
     */
    hwloc_debug("  %04x:%02x:%02x.%01x bridge has invalid secondary-subordinate buses [%02x-%02x]\n",
		pattr->domain, pattr->bus, pattr->dev, pattr->func,
		battr->downstream.pci.secondary_bus, battr->downstream.pci.subordinate_bus);
    hwloc_free_unlinked_object(obj);
    return -1;
  }

  return 0;
}
