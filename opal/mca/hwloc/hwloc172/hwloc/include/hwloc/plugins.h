/*
 * Copyright © 2013 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PLUGINS_H
#define HWLOC_PLUGINS_H

/** \file
 * \brief Public interface for building hwloc plugins.
 */

struct hwloc_backend;

#include <hwloc.h>




/** \defgroup hwlocality_disc_components Discovery components
 * @{
 */

/** \brief Discovery component type */
typedef enum hwloc_disc_component_type_e {
  /** \brief CPU-only discovery through the OS, or generic no-OS support.
   * \hideinitializer */
  HWLOC_DISC_COMPONENT_TYPE_CPU = (1<<0),

  /** \brief xml, synthetic or custom,
   * platform-specific components such as bgq.
   * Anything the discovers CPU and everything else.
   * No misc backend is expected to complement a global component.
   * \hideinitializer */
  HWLOC_DISC_COMPONENT_TYPE_GLOBAL = (1<<1),

  /** \brief PCI, etc.
   * \hideinitializer */
  HWLOC_DISC_COMPONENT_TYPE_MISC = (1<<2)
} hwloc_disc_component_type_t;

/** \brief Discovery component structure
 *
 * This is the major kind of components, taking care of the discovery.
 * They are registered by generic components, either statically-built or as plugins.
 */
struct hwloc_disc_component {
  /** \brief Discovery component type */
  hwloc_disc_component_type_t type;

  /** \brief Name.
   * If this component is built as a plugin, this name does not have to match the plugin filename.
   */
  const char *name;

  /** \brief Component types to exclude, as an OR'ed set of HWLOC_DISC_COMPONENT_TYPE_*.
   *
   * For a GLOBAL component, this usually includes all other types (~0).
   *
   * Other components only exclude types that may bring conflicting
   * topology information. MISC components should likely not be excluded
   * since they usually bring non-primary additional information.
   */
  unsigned excludes;

  /** \brief Instantiate callback to create a backend from the component.
   * Parameters data1, data2, data3 are NULL except for components
   * that have special enabling routines such as hwloc_topology_set_xml(). */
  struct hwloc_backend * (*instantiate)(struct hwloc_disc_component *component, const void *data1, const void *data2, const void *data3);

  /** \brief Component priority.
   * Used to sort topology->components, higher priority first.
   * Also used to decide between two components with the same name.
   *
   * Usual values are
   * 50 for native OS (or platform) components,
   * 45 for x86,
   * 40 for no-OS fallback,
   * 30 for global components (xml/synthetic/custom),
   * 20 for pci,
   * 10 for other misc components (opencl etc.).
   */
  unsigned priority;

  /** \private Used internally to list components by priority on topology->components
   * (the component structure is usually read-only,
   *  the core copies it before using this field for queueing)
   */
  struct hwloc_disc_component * next;
};

/** @} */




/** \defgroup hwlocality_disc_backends Discovery backends
 * @{
 */

/** \brief Discovery backend structure
 *
 * A backend is the instantiation of a discovery component.
 * When a component gets enabled for a topology,
 * its instantiate() callback creates a backend.
 *
 * hwloc_backend_alloc() initializes all fields to default values
 * that the component may change (except "component" and "next")
 * before enabling the backend with hwloc_backend_enable().
 */
struct hwloc_backend {
  /** \private Reserved for the core, set by hwloc_backend_alloc() */
  struct hwloc_disc_component * component;
  /** \private Reserved for the core, set by hwloc_backend_enable() */
  struct hwloc_topology * topology;
  /** \private Reserved for the core. Set to 1 if forced through envvar, 0 otherwise. */
  int envvar_forced;
  /** \private Reserved for the core. Used internally to list backends topology->backends. */
  struct hwloc_backend * next;

  /** \brief Backend flags, as an OR'ed set of HWLOC_BACKEND_FLAG_* */
  unsigned long flags;

  /** \brief Backend-specific 'is_custom' property.
   * Shortcut on !strcmp(..->component->name, "custom").
   * Only the custom component should touch this. */
  int is_custom;

  /** \brief Backend-specific 'is_thissystem' property.
   * Set to 0 or 1 if the backend should enforce the thissystem flag when it gets enabled.
   * Set to -1 if the backend doesn't care (default). */
  int is_thissystem;

  /** \brief Backend private data, or NULL if none. */
  void * private_data;
  /** \brief Callback for freeing the private_data.
   * May be NULL.
   */
  void (*disable)(struct hwloc_backend *backend);

  /** \brief Main discovery callback.
   * returns > 0 if it modified the topology tree, -1 on error, 0 otherwise.
   * May be NULL if type is HWLOC_DISC_COMPONENT_TYPE_MISC. */
  int (*discover)(struct hwloc_backend *backend);

  /** \brief Callback used by the PCI backend to retrieve the locality of a PCI object from the OS/cpu backend.
   * May be NULL. */
  int (*get_obj_cpuset)(struct hwloc_backend *backend, struct hwloc_backend *caller, struct hwloc_obj *obj, hwloc_bitmap_t cpuset);

  /** \brief Callback called by backends to notify this backend that a new object was added.
   * returns > 0 if it modified the topology tree, 0 otherwise.
   * May be NULL. */
  int (*notify_new_object)(struct hwloc_backend *backend, struct hwloc_backend *caller, struct hwloc_obj *obj);
};

/** \brief Backend flags */
enum hwloc_backend_flag_e {
  /** \brief Levels should be reconnected before this backend discover() is used.
   * \hideinitializer */
  HWLOC_BACKEND_FLAG_NEED_LEVELS = (1UL<<0)
};

/** \brief Allocate a backend structure, set good default values, initialize backend->component and topology, etc.
 * The caller will then modify whatever needed, and call hwloc_backend_enable().
 */
HWLOC_DECLSPEC struct hwloc_backend * hwloc_backend_alloc(struct hwloc_disc_component *component);

/** \brief Enable a previously allocated and setup backend. */
HWLOC_DECLSPEC int hwloc_backend_enable(struct hwloc_topology *topology, struct hwloc_backend *backend);

/** \brief Used by backends discovery callbacks to request locality information from others.
 *
 * Traverse the list of enabled backends until one has a
 * get_obj_cpuset() method, and call it.
 */
HWLOC_DECLSPEC int hwloc_backends_get_obj_cpuset(struct hwloc_backend *caller, struct hwloc_obj *obj, hwloc_bitmap_t cpuset);

/** \brief Used by backends discovery callbacks to notify other
 * backends of new objects.
 *
 * Traverse the list of enabled backends (all but caller) and invoke
 * their notify_new_object() method to notify them that a new object
 * just got added to the topology.
 *
 * Currently only used for notifying of new PCI device objects.
 */
HWLOC_DECLSPEC int hwloc_backends_notify_new_object(struct hwloc_backend *caller, struct hwloc_obj *obj);

/** @} */




/** \defgroup hwlocality_generic_components Generic components
 * @{
 */

/** \brief Generic component type */
typedef enum hwloc_component_type_e {
  /** \brief The data field must point to a struct hwloc_disc_component. */
  HWLOC_COMPONENT_TYPE_DISC,

  /** \brief The data field must point to a struct hwloc_xml_component. */
  HWLOC_COMPONENT_TYPE_XML
} hwloc_component_type_t;

/** \brief Generic component structure
 *
 * Generic components structure, either statically listed by configure in static-components.h
 * or dynamically loaded as a plugin.
 */
struct hwloc_component {
  /** \brief Component ABI version, set to HWLOC_COMPONENT_ABI */
  unsigned abi;

  /** \brief Component type */
  hwloc_component_type_t type;

  /** \brief Component flags, unused for now */
  unsigned long flags;

  /** \brief Component data, pointing to a struct hwloc_disc_component or struct hwloc_xml_component. */
  void * data;
};

/** @} */




/** \defgroup hwlocality_components_core_funcs Core functions to be used by components
 * @{
 */

/** \brief Add an object to the topology.
 *
 * It is sorted along the tree of other objects according to the inclusion of
 * cpusets, to eventually be added as a child of the smallest object including
 * this object.
 *
 * If the cpuset is empty, the type of the object (and maybe some attributes)
 * must be enough to find where to insert the object. This is especially true
 * for NUMA nodes with memory and no CPUs.
 *
 * The given object should not have children.
 *
 * This shall only be called before levels are built.
 *
 * In case of error, hwloc_report_os_error() is called.
 *
 * Returns the object on success.
 * Returns NULL and frees obj on error.
 * Returns another object and frees obj if it was merged with an identical pre-existing object.
 */
HWLOC_DECLSPEC struct hwloc_obj *hwloc_insert_object_by_cpuset(struct hwloc_topology *topology, hwloc_obj_t obj);

/** \brief Type of error callbacks during object insertion */
typedef void (*hwloc_report_error_t)(const char * msg, int line);
/** \brief Report an insertion error from a backend */
HWLOC_DECLSPEC void hwloc_report_os_error(const char * msg, int line);
/** \brief Check whether insertion errors are hidden */
HWLOC_DECLSPEC int hwloc_hide_errors(void);

/** \brief Add an object to the topology and specify which error callback to use.
 *
 * Aside from the error callback selection, this function is identical to hwloc_insert_object_by_cpuset()
 */
HWLOC_DECLSPEC struct hwloc_obj *hwloc__insert_object_by_cpuset(struct hwloc_topology *topology, hwloc_obj_t obj, hwloc_report_error_t report_error);

/** \brief Insert an object somewhere in the topology.
 *
 * It is added as the last child of the given parent.
 * The cpuset is completely ignored, so strange objects such as I/O devices should
 * preferably be inserted with this.
 *
 * The given object may have children.
 *
 * Remember to call topology_connect() afterwards to fix handy pointers.
 */
HWLOC_DECLSPEC void hwloc_insert_object_by_parent(struct hwloc_topology *topology, hwloc_obj_t parent, hwloc_obj_t obj);

/** \brief Allocate and initialize an object of the given type and physical index */
static __hwloc_inline struct hwloc_obj *
hwloc_alloc_setup_object(hwloc_obj_type_t type, signed os_index)
{
  struct hwloc_obj *obj = malloc(sizeof(*obj));
  memset(obj, 0, sizeof(*obj));
  obj->type = type;
  obj->os_index = os_index;
  obj->os_level = -1;
  obj->attr = malloc(sizeof(*obj->attr));
  memset(obj->attr, 0, sizeof(*obj->attr));
  /* do not allocate the cpuset here, let the caller do it */
  return obj;
}

/** @} */




#endif /* HWLOC_PLUGINS_H */
