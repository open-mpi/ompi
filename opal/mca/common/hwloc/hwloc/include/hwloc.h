/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2010 INRIA
 * Copyright © 2009-2010 Université Bordeaux 1
 * Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/** \file
 * \brief The hwloc API.
 *
 * See hwloc/bitmap.h for bitmap specific macros.
 * See hwloc/helper.h for high-level topology traversal helpers.
 */

#ifndef HWLOC_H
#define HWLOC_H

#include <hwloc/config.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#ifdef HWLOC_HAVE_STDINT_H
#include <stdint.h>
#endif

/*
 * Symbol transforms
 */
#include <hwloc/rename.h>

/*
 * Bitmap definitions
 */

#include <hwloc/bitmap.h>
#include <hwloc/cpuset.h>


#ifdef __cplusplus
extern "C" {
#endif


/** \defgroup hwlocality_api_version API version
 * @{
 */

/** \brief Indicate at build time which hwloc API version is being used. */
#define HWLOC_API_VERSION 0x00010100

/** @} */



/** \defgroup hwlocality_topology Topology context
 * @{
 */

struct hwloc_topology;
/** \brief Topology context
 *
 * To be initialized with hwloc_topology_init() and built with hwloc_topology_load().
 */
typedef struct hwloc_topology * hwloc_topology_t;

/** @} */



/** \defgroup hwlocality_sets Object sets
 * @{
 */

/** \brief A CPU set is a bitmap whose bits are set according to CPU physical OS indexes.
 *
 * It may be consulted and modified with the bitmap API as any ::hwloc_bitmap_t (see hwloc/bitmap.h).
 */
typedef hwloc_bitmap_t hwloc_cpuset_t;
/** \brief A non-modifiable ::hwloc_cpuset_t. */
typedef hwloc_const_bitmap_t hwloc_const_cpuset_t;

/** \brief A node set is a bitmap whose bits are set according to NUMA memory node physical OS indexes.
 *
 * It may be consulted and modified with the bitmap API as any ::hwloc_bitmap_t (see hwloc/bitmap.h).
 *
 * When binding memory on a system without any NUMA node
 * (when the whole memory is considered as a single memory bank),
 * the nodeset may be either empty (no memory selected)
 * or full (whole system memory selected).
 *
 * See also \ref hwlocality_helper_nodeset_convert.
 */
typedef hwloc_bitmap_t hwloc_nodeset_t;
/** \brief A non-modifiable ::hwloc_nodeset_t.
 */
typedef hwloc_const_bitmap_t hwloc_const_nodeset_t;

/** @} */



/** \defgroup hwlocality_types Topology Object Types
 * @{
 */

/** \brief Type of topology object.
 *
 * \note Do not rely on the ordering or completeness of the values as new ones
 * may be defined in the future!  If you need to compare types, use
 * hwloc_compare_types() instead.
 */
typedef enum {
  HWLOC_OBJ_SYSTEM,	/**< \brief Whole system (may be a cluster of machines).
  			  * The whole system that is accessible to hwloc.
			  * That may comprise several machines in SSI systems
			  * like Kerrighed.
			  */
  HWLOC_OBJ_MACHINE,	/**< \brief Machine.
			  * The typical root object type.
			  * A set of processors and memory with cache
			  * coherency.
			  */
  HWLOC_OBJ_NODE,	/**< \brief NUMA node.
			  * A set of processors around memory which the
			  * processors can directly access.
			  */
  HWLOC_OBJ_SOCKET,	/**< \brief Socket, physical package, or chip.
			  * In the physical meaning, i.e. that you can add
			  * or remove physically.
			  */
  HWLOC_OBJ_CACHE,	/**< \brief Data cache.
			  * Can be L1, L2, L3, ...
			  */
  HWLOC_OBJ_CORE,	/**< \brief Core.
			  * A computation unit (may be shared by several
			  * logical processors).
			  */
  HWLOC_OBJ_PU,		/**< \brief Processing Unit, or (Logical) Processor.
			  * An execution unit (may share a core with some
			  * other logical processors, e.g. in the case of
			  * an SMT core).
			  *
			  * Objects of this kind are always reported and can
			  * thus be used as fallback when others are not.
			  */

  HWLOC_OBJ_GROUP,	/**< \brief Group objects.
			  * Objects which do not fit in the above but are
			  * detected by hwloc and are useful to take into
			  * account for affinity. For instance, some OSes
			  * expose their arbitrary processors aggregation this
			  * way.  And hwloc may insert such objects to group
			  * NUMA nodes according to their distances.
			  *
			  * These objects are ignored when they do not bring
			  * any structure.
			  */

  HWLOC_OBJ_MISC 	/**< \brief Miscellaneous objects.
			  * Objects without particular meaning, that can e.g. be
			  * added by the application for its own use.
			  */
} hwloc_obj_type_t;

/** \brief Compare the depth of two object types
 *
 * Types shouldn't be compared as they are, since newer ones may be added in
 * the future.  This function returns less than, equal to, or greater than zero
 * respectively if \p type1 objects usually include \p type2 objects, are the
 * same as \p type2 objects, or are included in \p type2 objects. If the types
 * can not be compared (because neither is usually contained in the other),
 * HWLOC_TYPE_UNORDERED is returned.  Object types containing CPUs can always
 * be compared (usually, a system contains machines which contain nodes which
 * contain sockets which contain caches, which contain cores, which contain
 * processors).
 *
 * \note HWLOC_OBJ_PU will always be the deepest.
 * \note This does not mean that the actual topology will respect that order:
 * e.g. as of today cores may also contain caches, and sockets may also contain
 * nodes. This is thus just to be seen as a fallback comparison method.
 */
HWLOC_DECLSPEC int hwloc_compare_types (hwloc_obj_type_t type1, hwloc_obj_type_t type2) __hwloc_attribute_const;

enum hwloc_compare_types_e {
    HWLOC_TYPE_UNORDERED = INT_MAX	/**< \brief Value returned by hwloc_compare_types when types can not be compared. \hideinitializer */
};

/** @} */



/** \defgroup hwlocality_objects Topology Objects
 * @{
 */

union hwloc_obj_attr_u;

/** \brief Object memory */
struct hwloc_obj_memory_s {
  uint64_t total_memory; /**< \brief Total memory (in bytes) in this object and its children */
  uint64_t local_memory; /**< \brief Local memory (in bytes) */

  unsigned page_types_len; /**< \brief Size of array \p page_types */
  /** \brief Array of local memory page types, \c NULL if no local memory and \p page_types is 0.
   *
   * The array is sorted by increasing \p size fields.
   * It contains \p page_types_len slots.
   */
  struct hwloc_obj_memory_page_type_s {
    uint64_t size;	/**< \brief Size of pages */
    uint64_t count;	/**< \brief Number of pages of this size */
  } * page_types;
};

/** \brief Structure of a topology object
 *
 * Applications mustn't modify any field except ::userdata .
 */
struct hwloc_obj {
  /* physical information */
  hwloc_obj_type_t type;		/**< \brief Type of object */
  unsigned os_index;			/**< \brief OS-provided physical index number */
  char *name;				/**< \brief Object description if any */

  struct hwloc_obj_memory_s memory;	/**< \brief Memory attributes */

  union hwloc_obj_attr_u *attr;		/**< \brief Object type-specific Attributes,
					 * may be \c NULL if no attribute value was found */

  /* global position */
  unsigned depth;			/**< \brief Vertical index in the hierarchy */
  unsigned logical_index;		/**< \brief Horizontal index in the whole list of similar objects,
					 * could be a "cousin_rank" since it's the rank within the "cousin" list below */
  signed os_level;			/**< \brief OS-provided physical level, -1 if unknown or meaningless */

  struct hwloc_obj *next_cousin;	/**< \brief Next object of same type */
  struct hwloc_obj *prev_cousin;	/**< \brief Previous object of same type */

  /* parent */
  struct hwloc_obj *parent;		/**< \brief Parent, \c NULL if root (system object) */
  unsigned sibling_rank;		/**< \brief Index in parent's \c children[] array */
  struct hwloc_obj *next_sibling;	/**< \brief Next object below the same parent */
  struct hwloc_obj *prev_sibling;	/**< \brief Previous object below the same parent */

  /* children */
  unsigned arity;			/**< \brief Number of children */
  struct hwloc_obj **children;		/**< \brief Children, \c children[0 .. arity -1] */
  struct hwloc_obj *first_child;	/**< \brief First child */
  struct hwloc_obj *last_child;		/**< \brief Last child */

  /* misc */
  void *userdata;			/**< \brief Application-given private data pointer, initialized to \c NULL, use it as you wish */

  /* cpusets and nodesets */
  hwloc_cpuset_t cpuset;		/**< \brief CPUs covered by this object
                                          *
                                          * This is the set of CPUs for which there are PU objects in the topology
                                          * under this object, i.e. which are known to be physically contained in this
                                          * object and known how (the children path between this object and the PU
                                          * objects).
                                          *
                                          * If the HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM configuration flag is set, some of
                                          * these CPUs may be offline, or not allowed for binding, see online_cpuset
                                          * and allowed_cpuset.
                                          *
                                          * \note Its value must not be changed, hwloc_bitmap_dup must be used instead.
                                          */
  hwloc_cpuset_t complete_cpuset;       /**< \brief The complete CPU set of logical processors of this object,
                                          *
                                          * This includes not only the same as the cpuset field, but also the CPUs for
                                          * which topology information is unknown or incomplete, and the CPUs that are
                                          * ignored when the HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM flag is not set.
                                          * Thus no corresponding PU object may be found in the topology, because the
                                          * precise position is undefined. It is however known that it would be somewhere
                                          * under this object.
                                          *
                                          * \note Its value must not be changed, hwloc_bitmap_dup must be used instead.
                                          */
  hwloc_cpuset_t online_cpuset;         /**< \brief The CPU set of online logical processors
                                          *
                                          * This includes the CPUs contained in this object that are online, i.e. draw
                                          * power and can execute threads.  It may however not be allowed to bind to
                                          * them due to administration rules, see allowed_cpuset.
                                          *
                                          * \note Its value must not be changed, hwloc_bitmap_dup must be used instead.
                                          */
  hwloc_cpuset_t allowed_cpuset;        /**< \brief The CPU set of allowed logical processors
                                          *
                                          * This includes the CPUs contained in this object which are allowed for
                                          * binding, i.e. passing them to the hwloc binding functions should not return
                                          * permission errors.  This is usually restricted by administration rules.
                                          * Some of them may however be offline so binding to them may still not be
                                          * possible, see online_cpuset.
                                          *
                                          * \note Its value must not be changed, hwloc_bitmap_dup must be used instead.
                                          */

  hwloc_nodeset_t nodeset;              /**< \brief NUMA nodes covered by this object or containing this object
                                          *
                                          * This is the set of NUMA nodes for which there are NODE objects in the
                                          * topology under or above this object, i.e. which are known to be physically
                                          * contained in this object or containing it and known how (the children path
                                          * between this object and the NODE objects).
                                          *
                                          * In the end, these nodes are those that are close to the current object.
                                          *
                                          * If the HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM configuration flag is set, some of
                                          * these nodes may not be allowed for allocation, see allowed_nodeset.
                                          *
                                          * If there are no NUMA nodes in the machine, all the memory is close to this
                                          * object, so \p nodeset is full.
                                          *
                                          * \note Its value must not be changed, hwloc_bitmap_dup must be used instead.
                                          */
  hwloc_nodeset_t complete_nodeset;     /**< \brief The complete NUMA node set of this object,
                                          *
                                          * This includes not only the same as the nodeset field, but also the NUMA
                                          * nodes for which topology information is unknown or incomplete, and the nodes
                                          * that are ignored when the HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM flag is not set.
                                          * Thus no corresponding NODE object may be found in the topology, because the
                                          * precise position is undefined. It is however known that it would be
                                          * somewhere under this object.
                                          *
                                          * If there are no NUMA nodes in the machine, all the memory is close to this
                                          * object, so \p complete_nodeset is full.
                                          *
                                          * \note Its value must not be changed, hwloc_bitmap_dup must be used instead.
                                          */
  hwloc_nodeset_t allowed_nodeset;      /**< \brief The set of allowed NUMA memory nodes
                                          *
                                          * This includes the NUMA memory nodes contained in this object which are
                                          * allowed for memory allocation, i.e. passing them to NUMA node-directed
                                          * memory allocation should not return permission errors. This is usually
                                          * restricted by administration rules.
                                          *
                                          * If there are no NUMA nodes in the machine, all the memory is close to this
                                          * object, so \p allowed_nodeset is full.
                                          *
                                          * \note Its value must not be changed, hwloc_bitmap_dup must be used instead.
                                          */

  struct hwloc_obj_info_s *infos;	/**< \brief Array of stringified info type=name. */
  unsigned infos_count;			/**< \brief Size of infos array. */
};
/**
 * \brief Convenience typedef; a pointer to a struct hwloc_obj.
 */
typedef struct hwloc_obj * hwloc_obj_t;

/** \brief Object type-specific Attributes */
union hwloc_obj_attr_u {
  /** \brief Cache-specific Object Attributes */
  struct hwloc_cache_attr_s {
    uint64_t size;			  /**< \brief Size of cache in bytes */
    unsigned depth;			  /**< \brief Depth of cache */
    unsigned linesize;			  /**< \brief Cache-line size in bytes */
  } cache;
  /** \brief Group-specific Object Attributes */
  struct hwloc_group_attr_s {
    unsigned depth;			  /**< \brief Depth of group object */
  } group;
};

/** \brief Object info */
struct hwloc_obj_info_s {
  char *name;	/**< \brief Info name */
  char *value;	/**< \brief Info value */
};

/** @} */



/** \defgroup hwlocality_creation Create and Destroy Topologies
 * @{
 */

/** \brief Allocate a topology context.
 *
 * \param[out] topologyp is assigned a pointer to the new allocated context.
 *
 * \return 0 on success, -1 on error.
 */
HWLOC_DECLSPEC int hwloc_topology_init (hwloc_topology_t *topologyp);

/** \brief Build the actual topology
 *
 * Build the actual topology once initialized with hwloc_topology_init() and
 * tuned with \ref hwlocality_configuration routines.
 * No other routine may be called earlier using this topology context.
 *
 * \param topology is the topology to be loaded with objects.
 *
 * \return 0 on success, -1 on error.
 *
 * \sa hwlocality_configuration
 */
HWLOC_DECLSPEC int hwloc_topology_load(hwloc_topology_t topology);

/** \brief Terminate and free a topology context
 *
 * \param topology is the topology to be freed
 */
HWLOC_DECLSPEC void hwloc_topology_destroy (hwloc_topology_t topology);

/** \brief Run internal checks on a topology structure
 *
 * \param topology is the topology to be checked
 */
HWLOC_DECLSPEC void hwloc_topology_check(hwloc_topology_t topology);

/** @} */



/** \defgroup hwlocality_configuration Configure Topology Detection
 *
 * These functions can optionally be called between hwloc_topology_init() and
 * hwloc_topology_load() to configure how the detection should be performed,
 * e.g. to ignore some objects types, define a synthetic topology, etc.
 *
 * If none of them is called, the default is to detect all the objects of the
 * machine that the caller is allowed to access.
 *
 * This default behavior may also be modified through environment variables
 * if the application did not modify it already.
 * Setting HWLOC_XMLFILE in the environment enforces the discovery from a XML
 * file as if hwloc_topology_set_xml() had been called.
 * HWLOC_FSROOT switches to reading the topology from the specified Linux
 * filesystem root as if hwloc_topology_set_fsroot() had been called.
 * Finally, HWLOC_THISSYSTEM enforces the return value of
 * hwloc_topology_is_thissystem().
 *
 * @{
 */

/** \brief Ignore an object type.
 *
 * Ignore all objects from the given type.
 * The bottom-level type HWLOC_OBJ_PU may not be ignored.
 * The top-level object of the hierarchy will never be ignored, even if this function
 * succeeds.
 */
HWLOC_DECLSPEC int hwloc_topology_ignore_type(hwloc_topology_t topology, hwloc_obj_type_t type);

/** \brief Ignore an object type if it does not bring any structure.
 *
 * Ignore all objects from the given type as long as they do not bring any structure:
 * Each ignored object should have a single children or be the only child of its parent.
 * The bottom-level type HWLOC_OBJ_PU may not be ignored.
 */
HWLOC_DECLSPEC int hwloc_topology_ignore_type_keep_structure(hwloc_topology_t topology, hwloc_obj_type_t type);

/** \brief Ignore all objects that do not bring any structure.
 *
 * Ignore all objects that do not bring any structure:
 * Each ignored object should have a single children or be the only child of its parent.
 */
HWLOC_DECLSPEC int hwloc_topology_ignore_all_keep_structure(hwloc_topology_t topology);

/** \brief Flags to be set onto a topology context before load.
 *
 * Flags should be given to hwloc_topology_set_flags().
 */
enum hwloc_topology_flags_e {
  HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM = (1<<0),
 /**< \brief Detect the whole system, ignore reservations and offline settings.
   * \hideinitializer
   *
   * Gather all resources, even if some were disabled by the administrator.
   * For instance, ignore Linux Cpusets and gather all processors and memory nodes,
   * and ignore the fact that some resources may be offline.
   */

  HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM = (1<<1)
 /**< \brief Assume that the selected backend provides the topology for the
   * system on which we are running.
   * \hideinitializer
   *
   * This forces hwloc_topology_is_thissystem to return 1, i.e. makes hwloc assume that
   * the selected backend provides the topology for the system on which we are running,
   * even if it is not the OS-specific backend but the XML backend for instance.
   * This means making the binding functions actually call the OS-specific
   * system calls and really do binding, while the XML backend would otherwise
   * provide empty hooks just returning success.
   *
   * Setting the environment variable HWLOC_THISSYSTEM may also result in the
   * same behavior.
   *
   * This can be used for efficiency reasons to first detect the topology once,
   * save it to an XML file, and quickly reload it later through the XML
   * backend, but still having binding functions actually do bind.
   */
};

/** \brief Set OR'ed flags to non-yet-loaded topology.
 *
 * Set a OR'ed set of hwloc_topology_flags_e onto a topology that was not yet loaded.
 */
HWLOC_DECLSPEC int hwloc_topology_set_flags (hwloc_topology_t topology, unsigned long flags);

/** \brief Change the file-system root path when building the topology from sysfs/procfs.
 *
 * On Linux system, use sysfs and procfs files as if they were mounted on the given
 * \p fsroot_path instead of the main file-system root. Setting the environment
 * variable HWLOC_FSROOT may also result in this behavior.
 * Not using the main file-system root causes hwloc_topology_is_thissystem()
 * to return 0.
 *
 * \note For conveniency, this backend provides empty binding hooks which just
 * return success.  To have hwloc still actually call OS-specific hooks, the
 * HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM has to be set to assert that the loaded
 * file is really the underlying system.
 */
HWLOC_DECLSPEC int hwloc_topology_set_fsroot(hwloc_topology_t __hwloc_restrict topology, const char * __hwloc_restrict fsroot_path);

/** \brief Change which pid the topology is viewed from
 *
 * On some systems, processes may have different views of the machine, for
 * instance the set of allowed CPUs. By default, hwloc exposes the view from
 * the current process. Calling hwloc_topology_set_pid() permits to make it
 * expose the topology of the machine from the point of view of another
 * process.
 *
 * \note hwloc_pid_t is pid_t on unix platforms, and HANDLE on native Windows
 * platforms
 * \note -1 is returned and errno is set to ENOSYS on platforms that do not
 * support this feature.
 */
HWLOC_DECLSPEC int hwloc_topology_set_pid(hwloc_topology_t __hwloc_restrict topology, hwloc_pid_t pid);

/** \brief Enable synthetic topology.
 *
 * Gather topology information from the given \p description
 * which should be a comma separated string of numbers describing
 * the arity of each level.
 * Each number may be prefixed with a type and a colon to enforce the type
 * of a level.  If only some level types are enforced, hwloc will try to
 * choose the other types according to usual topologies, but it may fail
 * and you may have to specify more level types manually.
 *
 * \note For conveniency, this backend provides empty binding hooks which just
 * return success.
 */
HWLOC_DECLSPEC int hwloc_topology_set_synthetic(hwloc_topology_t __hwloc_restrict topology, const char * __hwloc_restrict description);

/** \brief Enable XML-file based topology.
 *
 * Gather topology information from the XML file given at \p xmlpath.
 * Setting the environment variable HWLOC_XMLFILE may also result in this behavior.
 * This file may have been generated earlier with lstopo file.xml.
 *
 * \note For conveniency, this backend provides empty binding hooks which just
 * return success.  To have hwloc still actually call OS-specific hooks, the
 * HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM has to be set to assert that the loaded
 * file is really the underlying system.
 */
HWLOC_DECLSPEC int hwloc_topology_set_xml(hwloc_topology_t __hwloc_restrict topology, const char * __hwloc_restrict xmlpath);

/** \brief Enable XML based topology using a memory buffer instead of a file.
 *
 * Gather topology information from the XML memory buffer given at \p buffer
 * and of length \p length.
 */
HWLOC_DECLSPEC int hwloc_topology_set_xmlbuffer(hwloc_topology_t __hwloc_restrict topology, const char * __hwloc_restrict buffer, int size);

/** \brief Flags describing actual discovery support for this topology. */
struct hwloc_topology_discovery_support {
  /** \brief Detecting the number of PU objects is supported. */
  unsigned char pu;
};

/** \brief Flags describing actual PU binding support for this topology. */
struct hwloc_topology_cpubind_support {
  /** Binding the whole current process is supported.  */
  unsigned char set_thisproc_cpubind;
  /** Getting the binding of the whole current process is supported.  */
  unsigned char get_thisproc_cpubind;
  /** Binding a whole given process is supported.  */
  unsigned char set_proc_cpubind;
  /** Getting the binding of a whole given process is supported.  */
  unsigned char get_proc_cpubind;
  /** Binding the current thread only is supported.  */
  unsigned char set_thisthread_cpubind;
  /** Getting the binding of the current thread only is supported.  */
  unsigned char get_thisthread_cpubind;
  /** Binding a given thread only is supported.  */
  unsigned char set_thread_cpubind;
  /** Getting the binding of a given thread only is supported.  */
  unsigned char get_thread_cpubind;
};

/** \brief Flags describing actual memory binding support for this topology. */
struct hwloc_topology_membind_support {
  /** Binding the whole current process is supported.  */
  unsigned char set_thisproc_membind;
  /** Getting the binding of the whole current process is supported.  */
  unsigned char get_thisproc_membind;
  /** Binding a whole given process is supported.  */
  unsigned char set_proc_membind;
  /** Getting the binding of a whole given process is supported.  */
  unsigned char get_proc_membind;
  /** Binding the current thread only is supported.  */
  unsigned char set_thisthread_membind;
  /** Getting the binding of the current thread only is supported.  */
  unsigned char get_thisthread_membind;
  /** Binding a given memory area is supported. */
  unsigned char set_area_membind;
  /** Getting the binding of a given memory area is supported.  */
  unsigned char get_area_membind;
  /** Allocating a bound memory area is supported. */
  unsigned char alloc_membind;
  /** First-touch policy is supported. */
  unsigned char firsttouch_membind;
  /** Bind policy is supported. */
  unsigned char bind_membind;
  /** Interleave policy is supported. */
  unsigned char interleave_membind;
  /** Replication policy is supported. */
  unsigned char replicate_membind;
  /** Next-touch migration policy is supported. */
  unsigned char nexttouch_membind;

  /** Migration flags is supported. */
  unsigned char migrate_membind;
};

/** \brief Set of flags describing actual support for this topology.
 *
 * This is retrieved with hwloc_topology_get_support() and will be valid until
 * the topology object is destroyed.  Note: the values are correct only after
 * discovery.
 */
struct hwloc_topology_support {
  struct hwloc_topology_discovery_support *discovery;
  struct hwloc_topology_cpubind_support *cpubind;
  struct hwloc_topology_membind_support *membind;
};

/** \brief Retrieve the topology support. */
HWLOC_DECLSPEC const struct hwloc_topology_support *hwloc_topology_get_support(hwloc_topology_t __hwloc_restrict topology);

/** @} */



/** \defgroup hwlocality_tinker Tinker with topologies.
 * @{
 */

/** \brief Export the topology into an XML file.
 *
 * This file may be loaded later through hwloc_topology_set_xml().
 */
HWLOC_DECLSPEC void hwloc_topology_export_xml(hwloc_topology_t topology, const char *xmlpath);

/** \brief Export the topology into a newly-allocated XML memory buffer.
 *
 * \p xmlbuffer is allocated by the callee and should be freed with xmlFree later in the caller.
 *
 * This memory buffer may be loaded later through hwloc_topology_set_xmlbuffer().
 */
HWLOC_DECLSPEC void hwloc_topology_export_xmlbuffer(hwloc_topology_t topology, char **xmlbuffer, int *buflen);

/** \brief Add a MISC object to the topology
 *
 * A new MISC object will be created and inserted into the topology at the
 * position given by bitmap \p cpuset.
 *
 * cpuset and name will be copied.
 *
 * \return the newly-created object
 */
HWLOC_DECLSPEC hwloc_obj_t hwloc_topology_insert_misc_object_by_cpuset(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset, const char *name);

/** \brief Add a MISC object to the topology
 *
 * A new MISC object will be created and inserted into the topology at the
 * position given by parent.
 *
 * name will be copied.
 *
 * \return the newly-created object
 */
HWLOC_DECLSPEC hwloc_obj_t hwloc_topology_insert_misc_object_by_parent(hwloc_topology_t topology, hwloc_obj_t parent, const char *name);

/** @} */



/** \defgroup hwlocality_information Get some Topology Information
 * @{
 */

/** \brief Get the depth of the hierachical tree of objects.
 *
 * This is the depth of HWLOC_OBJ_PU objects plus one.
 */
HWLOC_DECLSPEC unsigned hwloc_topology_get_depth(hwloc_topology_t __hwloc_restrict topology) __hwloc_attribute_pure;

/** \brief Returns the depth of objects of type \p type.
 *
 * If no object of this type is present on the underlying architecture, or if
 * the OS doesn't provide this kind of information, the function returns
 * HWLOC_TYPE_DEPTH_UNKNOWN.
 *
 * If type is absent but a similar type is acceptable, see also
 * hwloc_get_type_or_below_depth() and hwloc_get_type_or_above_depth().
 */
HWLOC_DECLSPEC int hwloc_get_type_depth (hwloc_topology_t topology, hwloc_obj_type_t type);

enum hwloc_get_type_depth_e {
    HWLOC_TYPE_DEPTH_UNKNOWN = -1, /**< \brief No object of given type exists in the topology. \hideinitializer */
    HWLOC_TYPE_DEPTH_MULTIPLE = -2 /**< \brief Objects of given type exist at different depth in the topology. \hideinitializer */
};

/** \brief Returns the type of objects at depth \p depth.
 *
 * \return -1 if depth \p depth does not exist.
 */
HWLOC_DECLSPEC hwloc_obj_type_t hwloc_get_depth_type (hwloc_topology_t topology, unsigned depth) __hwloc_attribute_pure;

/** \brief Returns the width of level at depth \p depth */
HWLOC_DECLSPEC unsigned hwloc_get_nbobjs_by_depth (hwloc_topology_t topology, unsigned depth) __hwloc_attribute_pure;

/** \brief Returns the width of level type \p type
 *
 * If no object for that type exists, 0 is returned.
 * If there are several levels with objects of that type, -1 is returned.
 */
static __hwloc_inline int __hwloc_attribute_pure
hwloc_get_nbobjs_by_type (hwloc_topology_t topology, hwloc_obj_type_t type)
{
	int depth = hwloc_get_type_depth(topology, type);
	if (depth == HWLOC_TYPE_DEPTH_UNKNOWN)
		return 0;
	if (depth == HWLOC_TYPE_DEPTH_MULTIPLE)
		return -1; /* FIXME: agregate nbobjs from different levels? */
	return hwloc_get_nbobjs_by_depth(topology, depth);
}

/** \brief Does the topology context come from this system?
 *
 * \return 1 if this topology context was built using the system
 * running this program.
 * \return 0 instead (for instance if using another file-system root,
 * a XML topology file, or a synthetic topology).
 */
HWLOC_DECLSPEC int hwloc_topology_is_thissystem(hwloc_topology_t  __hwloc_restrict topology) __hwloc_attribute_pure;

/** @} */



/** \defgroup hwlocality_traversal Retrieve Objects
 * @{
 */

/** \brief Returns the topology object at index \p index from depth \p depth */
HWLOC_DECLSPEC hwloc_obj_t hwloc_get_obj_by_depth (hwloc_topology_t topology, unsigned depth, unsigned idx) __hwloc_attribute_pure;

/** \brief Returns the topology object at index \p index with type \p type
 *
 * If no object for that type exists, \c NULL is returned.
 * If there are several levels with objects of that type, \c NULL is returned
 * and ther caller may fallback to hwloc_get_obj_by_depth().
 */
static __hwloc_inline hwloc_obj_t __hwloc_attribute_pure
hwloc_get_obj_by_type (hwloc_topology_t topology, hwloc_obj_type_t type, unsigned idx)
{
  int depth = hwloc_get_type_depth(topology, type);
  if (depth == HWLOC_TYPE_DEPTH_UNKNOWN)
    return NULL;
  if (depth == HWLOC_TYPE_DEPTH_MULTIPLE)
    return NULL;
  return hwloc_get_obj_by_depth(topology, depth, idx);
}

/** @} */



/** \defgroup hwlocality_conversion Object/String Conversion
 * @{
 */

/** \brief Return a stringified topology object type */
HWLOC_DECLSPEC const char * hwloc_obj_type_string (hwloc_obj_type_t type) __hwloc_attribute_const;

/** \brief Return an object type from the string
 *
 * \return -1 if unrecognized.
 */
HWLOC_DECLSPEC hwloc_obj_type_t hwloc_obj_type_of_string (const char * string) __hwloc_attribute_pure;

/** \brief Stringify the type of a given topology object into a human-readable form.
 *
 * It differs from hwloc_obj_type_string() because it prints type attributes such
 * as cache depth.
 *
 * \return how many characters were actually written (not including the ending
 * \\0), or -1 on error.
 */
HWLOC_DECLSPEC int hwloc_obj_type_snprintf(char * __hwloc_restrict string, size_t size, hwloc_obj_t obj,
				   int verbose);

/** \brief Stringify the attributes of a given topology object into a human-readable form.
 *
 * Attribute values are separated by \p separator.
 *
 * Only the major attributes are printed in non-verbose mode.
 *
 * \return how many characters were actually written (not including the ending
 * \\0), or -1 on error.
 */
HWLOC_DECLSPEC int hwloc_obj_attr_snprintf(char * __hwloc_restrict string, size_t size, hwloc_obj_t obj, const char * __hwloc_restrict separator,
				   int verbose);

/** \brief Stringify a given topology object into a human-readable form.
 *
 * \note This function is deprecated in favor of hwloc_obj_type_snprintf()
 * and hwloc_obj_attr_snprintf() since it is not very flexible and
 * only prints physical/OS indexes.
 *
 * Fill string \p string up to \p size characters with the description
 * of topology object \p obj in topology \p topology.
 *
 * If \p verbose is set, a longer description is used. Otherwise a
 * short description is used.
 *
 * \p indexprefix is used to prefix the \p os_index attribute number of
 * the object in the description. If \c NULL, the \c # character is used.
 *
 * \return how many characters were actually written (not including the ending
 * \\0), or -1 on error.
 */
HWLOC_DECLSPEC int hwloc_obj_snprintf(char * __hwloc_restrict string, size_t size,
			     hwloc_topology_t topology, hwloc_obj_t obj,
			     const char * __hwloc_restrict indexprefix, int verbose);

/** \brief Stringify the cpuset containing a set of objects.
 *
 * \return how many characters were actually written (not including the ending \\0). */
HWLOC_DECLSPEC int hwloc_obj_cpuset_snprintf(char * __hwloc_restrict str, size_t size, size_t nobj, const hwloc_obj_t * __hwloc_restrict objs);

/** \brief Search the given key name in object infos and return the corresponding value.
 *
 * \return \c NULL if no such key exists.
 */
static __hwloc_inline char * __hwloc_attribute_pure
hwloc_obj_get_info_by_name(hwloc_obj_t obj, const char *name)
{
  unsigned i;
  for(i=0; i<obj->infos_count; i++)
    if (!strcmp(obj->infos[i].name, name))
      return obj->infos[i].value;
  return NULL;
}

/** @} */



/** \defgroup hwlocality_cpubinding CPU binding
 *
 * It is often useful to call hwloc_bitmap_singlify() first so that a single CPU
 * remains in the set. This way, the process will not even migrate between
 * different CPUs. Some OSes also only support that kind of binding.
 *
 * \note Some OSes do not provide all ways to bind processes, threads, etc and
 * the corresponding binding functions may fail. -1 is returned and errno is set
 * to ENOSYS when it is not possible to bind the requested kind of object
 * processes/threads. errno is set to EXDEV when the requested cpuset can not be
 * enforced (e.g. some systems only allow one CPU, and some other systems only
 * allow one NUMA node).
 *
 * The most portable version that should be preferred over the others, whenever
 * possible, is
 *
 * \code
 * hwloc_set_cpubind(topology, set, 0),
 * \endcode
 *
 * as it just binds the current program, assuming it is monothread, or
 *
 * \code
 * hwloc_set_cpubind(topology, set, HWLOC_CPUBIND_THREAD),
 * \endcode
 *
 * which binds the current thread of the current program (which may be
 * multithreaded).
 *
 * \note To unbind, just call the binding function with either a full cpuset or
 * a cpuset equal to the system cpuset.
 *
 * \note On some OSes, CPU binding may have effects on memory binding, see
 * ::HWLOC_CPUBIND_NOMEMBIND
 * @{
 */

/** \brief Process/Thread binding flags.
 *
 * These flags can be used to refine the binding policy.
 *
 * The default (0) is to bind the current process, assumed to be mono-thread,
 * in a non-strict way.  This is the most portable way to bind as all OSes
 * usually provide it.
 *
 * \note Not all systems support all kinds of binding.
 */
typedef enum {
  HWLOC_CPUBIND_PROCESS = (1<<0), /**< \brief Bind all threads of the current
                                   * (possibly) multithreaded process.
                                   * \hideinitializer */
  HWLOC_CPUBIND_THREAD = (1<<1),  /**< \brief Bind current thread of current process.
                                   * \hideinitializer */
  HWLOC_CPUBIND_STRICT = (1<<2),  /**< \brief Request for strict binding from the OS.
                                   * \hideinitializer
                                   *
                                   * By default, when the designated CPUs are
                                   * all busy while other CPUs are idle, OSes
                                   * may execute the thread/process on those
                                   * other CPUs instead of the designated CPUs,
                                   * to let them progress anyway.  Strict
                                   * binding means that the thread/process will
                                   * _never_ execute on other cpus than the
                                   * designated CPUs, even when those are busy
                                   * with other tasks and other CPUs are idle.
                                   *
                                   * \note Depending on OSes and
                                   * implementations, strict binding may not be
                                   * possible (implementation reason) or not
                                   * allowed (administrative reasons), and the
                                   * function will fail in that case.
				   *
				   * When retrieving the binding of a process,
				   * this flag checks whether all its threads
				   * actually have the same binding.
				   * If the flag is not given, the binding of
				   * each thread will be accumulated.
				   *
				   * \note This flag is meaningless when retrieving
				   * the binding of a thread.
                                   */
  HWLOC_CPUBIND_NOMEMBIND = (1<<3)/**< \brief Avoid any effect on memory binding
                                   * \hideinitializer
                                   *
                                   * On some OSes, some CPU binding function
                                   * would also bind the memory on the
                                   * corresponding NUMA node.  It is often not
                                   * a problem for the application, but if it
                                   * is, setting this flag will make hwloc
                                   * avoid using OS functions that would also
                                   * bind memory.  This will however reduce the
                                   * support of CPU bindings, i.e. potentially
                                   * return -1 with errno set to ENOSYS in some
                                   * cases.
                                   */
} hwloc_cpubind_flags_t;

/** \brief Bind current process or thread on cpus given in bitmap \p set
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t set, int flags);

/** \brief Get current process or thread binding
 */
HWLOC_DECLSPEC int hwloc_get_cpubind(hwloc_topology_t topology, hwloc_cpuset_t set, int flags);

/** \brief Bind a process \p pid on cpus given in bitmap \p set
 *
 * \note hwloc_pid_t is pid_t on unix platforms, and HANDLE on native Windows
 * platforms
 *
 * \note HWLOC_CPUBIND_THREAD can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t set, int flags);

/** \brief Get the current binding of process \p pid
 *
 * \note hwloc_pid_t is pid_t on unix platforms, and HANDLE on native Windows
 * platforms
 *
 * \note HWLOC_CPUBIND_THREAD can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_get_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t set, int flags);

#ifdef hwloc_thread_t
/** \brief Bind a thread \p tid on cpus given in bitmap \p set
 *
 * \note hwloc_thread_t is pthread_t on unix platforms, and HANDLE on native
 * Windows platforms
 *
 * \note HWLOC_CPUBIND_PROCESS can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_set_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t tid, hwloc_const_cpuset_t set, int flags);
#endif

#ifdef hwloc_thread_t
/** \brief Get the current binding of thread \p tid
 *
 * \note hwloc_thread_t is pthread_t on unix platforms, and HANDLE on native
 * Windows platforms
 *
 * \note HWLOC_CPUBIND_PROCESS can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_get_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t tid, hwloc_cpuset_t set, int flags);
#endif

/** @} */


/** \defgroup hwlocality_membinding Memory binding
 *
 * \note Not all OSes support all ways to bind existing allocated memory
 * (migration), future memory allocation, explicit memory allocation, etc. and
 * the corresponding binding functions may fail. -1 is returned and errno is
 * set to ENOSYS when it is not possible to bind the requested kind of object
 * processes/threads). errno is set to EXDEV when the requested cpuset can not
 * be enforced (e.g. some systems only allow one NUMA node).
 *
 * The most portable version that should be preferred over the others, whenever
 * possible, is
 *
 * \code
 * hwloc_alloc_membind_policy(topology, size, set, HWLOC_MEMBIND_DEFAULT, 0),
 * \endcode
 *
 * which will try to allocate new data bound to the given set, possibly by
 * changing the current memory binding policy, or at worse allocate memory
 * without binding it at all.  Since HWLOC_MEMBIND_STRICT is not given, this
 * will even not fail unless a mere malloc() itself would fail, i.e. ENOMEM.
 *
 * Each binding is available with a CPU set argument or a NUMA memory node set
 * argument. The name of the latter ends with _nodeset. It is also possible to
 * convert between CPU set and node set using ::hwloc_cpuset_to_nodeset or
 * ::hwloc_cpuset_from_nodeset.
 *
 * \note On some OSes, memory binding may have effects on CPU binding, see
 * ::HWLOC_MEMBIND_NOCPUBIND
 * @{
 */

/** \brief Memory binding policy.
 *
 * These can be used to choose the binding policy.
 *
 * Note that not all systems support all kinds of binding.
 */
typedef enum {
  HWLOC_MEMBIND_DEFAULT =	0,	/**< \brief Reset the memory allocation policy to the system default.
					 * \hideinitializer */
  HWLOC_MEMBIND_FIRSTTOUCH =	1,	/**< \brief Allocate memory on the given nodes, but preferably on the
					  node where the first accessor is running.
					 * \hideinitializer */
  HWLOC_MEMBIND_BIND =		2,	/**< \brief Allocate memory on the given nodes.
					 * \hideinitializer */
  HWLOC_MEMBIND_INTERLEAVE =	3,	/**< \brief Allocate memory on the given nodes in a round-robin manner.
					 * \hideinitializer */
  HWLOC_MEMBIND_REPLICATE =	4,	/**< \brief Replicate memory on the given nodes.
					 * \hideinitializer */
  HWLOC_MEMBIND_NEXTTOUCH =	5	/**< \brief On next touch of existing allocated memory, migrate it to the node
					 * where the memory reference happened.
					 * \hideinitializer */
} hwloc_membind_policy_t;

/** \brief Memory binding flags.
 *
 * These flags can be used to refine the binding policy.
 *
 * \note Not all systems support all kinds of binding.
 */
typedef enum {
  HWLOC_MEMBIND_PROCESS =       (1<<0), /**< \brief Set policy for all threads of the
                                         * current (possibly multithreaded) process.
                                         * \hideinitializer */
  HWLOC_MEMBIND_THREAD =        (1<<1), /**< \brief Set policy for the current thread of
                                         * the current process.
                                         * \hideinitializer */
  HWLOC_MEMBIND_STRICT =        (1<<2), /**< Request strict binding from the OS.
                                         * The function will fail if the
                                         * binding can not be completely
                                         * enforced.
                                         * \hideinitializer  */
  HWLOC_MEMBIND_MIGRATE =       (1<<3), /**< \brief Migrate existing allocated memory.
                                         * If memory can not be migrated and the STRICT
                                         * flag is passed, an error will be returned.
                                         * \hideinitializer  */
  HWLOC_MEMBIND_NOCPUBIND =     (1<<4)  /**< \brief Avoid any effect on CPU binding
                                         * \hideinitializer
                                         *
                                         * On some OSes, some memory binding function
                                         * would also bind the application on
                                         * the corresponding CPUs. It is often
                                         * not a problem for the application, but if it
                                         * is, setting this flag will make hwloc
                                         * avoid using OS functions that would also
                                         * bind on CPUs.  This will however reduce the
                                         * support of memory bindings, i.e. potentially
                                         * return ENOSYS in some cases.
                                         */
} hwloc_membind_flags_t;

/** \brief Bind current process memory on the given nodeset \p nodeset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_membind_nodeset(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);

/** \brief Bind current process memory on memory nodes near the given cpuset \p cpuset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_membind(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset, hwloc_membind_policy_t policy, int flags);

/** \brief Get current process memory binding in nodeset \p nodeset
 */
HWLOC_DECLSPEC int hwloc_get_membind_nodeset(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);

/** \brief Get current process memory binding in cpuset \p cpuset
 */
HWLOC_DECLSPEC int hwloc_get_membind(hwloc_topology_t topology, hwloc_cpuset_t cpuset, hwloc_membind_policy_t * policy, int flags);

/** \brief Bind given process memory on the given nodeset \p nodeset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_proc_membind_nodeset(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);

/** \brief Bind given process memory on memory nodes near the given cpuset \p cpuset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t cpuset, hwloc_membind_policy_t policy, int flags);

/** \brief Get current process memory binding in nodeset \p nodeset
 */
HWLOC_DECLSPEC int hwloc_get_proc_membind_nodeset(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);

/** \brief Get current process memory binding in cpuset \p cpuset
 */
HWLOC_DECLSPEC int hwloc_get_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t cpuset, hwloc_membind_policy_t * policy, int flags);

/** \brief Bind some memory range on the given nodeset \p nodeset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_area_membind_nodeset(hwloc_topology_t topology, const void *addr, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);

/** \brief Bind some memory range on memory nodes near the given cpuset \p cpuset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_area_membind(hwloc_topology_t topology, const void *addr, size_t len, hwloc_const_cpuset_t cpuset, hwloc_membind_policy_t policy, int flags);

/** \brief Get some memory range memory binding in nodeset \p nodeset
 */
HWLOC_DECLSPEC int hwloc_get_area_membind_nodeset(hwloc_topology_t topology, const void *addr, size_t len, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);

/** \brief Get some memory range memory binding in cpuset \p cpuset
 */
HWLOC_DECLSPEC int hwloc_get_area_membind(hwloc_topology_t topology, const void *addr, size_t len, hwloc_cpuset_t cpuset, hwloc_membind_policy_t * policy, int flags);

/** \brief Allocate some memory
 *
 * This is equivalent to malloc(), except it tries to allocated page-aligned
 * memory from the OS.
 *
 * \note The allocated memory should be freed with hwloc_free().
 */
HWLOC_DECLSPEC void *hwloc_alloc(hwloc_topology_t topology, size_t len);

/** \brief Allocate some memory on the given nodeset \p nodeset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * and HWLOC_MEMBIND_STRICT is given
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 * and HWLOC_MEMBIND_STRICT is given
 *
 * \note The allocated memory should be freed with hwloc_free().
 */
HWLOC_DECLSPEC void *hwloc_alloc_membind_nodeset(hwloc_topology_t topology, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags) __hwloc_attribute_malloc;

/** \brief Allocate some memory on memory nodes near the given cpuset \p cpuset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * and HWLOC_MEMBIND_STRICT is given
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 * and HWLOC_MEMBIND_STRICT is given
 *
 * \note The allocated memory should be freed with hwloc_free().
 */
HWLOC_DECLSPEC void *hwloc_alloc_membind(hwloc_topology_t topology, size_t len, hwloc_const_cpuset_t cpuset, hwloc_membind_policy_t policy, int flags) __hwloc_attribute_malloc;

/** \brief Free some memory allocated by hwloc_alloc() or hwloc_alloc_membind().
 */
HWLOC_DECLSPEC int hwloc_free(hwloc_topology_t topology, void *addr, size_t len);

/** @} */


#ifdef __cplusplus
} /* extern "C" */
#endif


/* high-level helpers */
#include <hwloc/helper.h>


#endif /* HWLOC_H */
