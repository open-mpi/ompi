/*
 * Copyright © 2009 CNRS
 * Copyright © 2009-2014 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux 1
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

/*=====================================================================
 *                 PLEASE GO READ THE DOCUMENTATION!
 *         ------------------------------------------------
 *               $tarball_directory/doc/doxygen-doc/
 *                                or                            
 *           http://www.open-mpi.org/projects/hwloc/doc/
 *=====================================================================
 *
 * FAIR WARNING: Do NOT expect to be able to figure out all the
 * subtleties of hwloc by simply reading function prototypes and
 * constant descrptions here in this file.
 *
 * Hwloc has wonderful documentation in both PDF and HTML formats for
 * your reading pleasure.  The formal documentation explains a LOT of
 * hwloc-specific concepts, provides definitions, and discusses the
 * "big picture" for many of the things that you'll find here in this
 * header file.
 *
 * The PDF/HTML documentation was generated via Doxygen; much of what
 * you'll see in there is also here in this file.  BUT THERE IS A LOT
 * THAT IS IN THE PDF/HTML THAT IS ***NOT*** IN hwloc.h!
 *
 * There are entire paragraph-length descriptions, discussions, and
 * pretty prictures to explain subtle corner cases, provide concrete
 * examples, etc.
 *
 * Please, go read the documentation.  :-)
 *
 *=====================================================================*/

/** \file
 * \brief The hwloc API.
 *
 * See hwloc/bitmap.h for bitmap specific macros.
 * See hwloc/helper.h for high-level topology traversal helpers.
 * See hwloc/inlines.h for the actual inline code of some functions below.
 */

#ifndef HWLOC_H
#define HWLOC_H

#include <hwloc/autogen/config.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

/*
 * Symbol transforms
 */
#include <hwloc/rename.h>

/*
 * Bitmap definitions
 */

#include <hwloc/bitmap.h>


#ifdef __cplusplus
extern "C" {
#endif


/** \defgroup hwlocality_api_version API version
 * @{
 */

/** \brief Indicate at build time which hwloc API version is being used. */
#define HWLOC_API_VERSION 0x00010900

/** \brief Indicate at runtime which hwloc API version was used at build time. */
HWLOC_DECLSPEC unsigned hwloc_get_api_version(void);

/** \brief Current component and plugin ABI version (see hwloc/plugins.h) */
#define HWLOC_COMPONENT_ABI 3

/** @} */



/** \defgroup hwlocality_object_sets Object Sets (hwloc_cpuset_t and hwloc_nodeset_t)
 *
 * Hwloc uses bitmaps to represent two distinct kinds of object sets:
 * CPU sets (::hwloc_cpuset_t) and NUMA node sets (::hwloc_nodeset_t).
 * These types are both typedefs to a common back end type
 * (::hwloc_bitmap_t), and therefore all the hwloc bitmap functions
 * are applicable to both ::hwloc_cpuset_t and ::hwloc_nodeset_t (see
 * \ref hwlocality_bitmap).
 *
 * The rationale for having two different types is that even though
 * the actions one wants to perform on these types are the same (e.g.,
 * enable and disable individual items in the set/mask), they're used
 * in very different contexts: one for specifying which processors to
 * use and one for specifying which NUMA nodes to use.  Hence, the
 * name difference is really just to reflect the intent of where the
 * type is used.
 *
 * @{
 */

/** \brief A CPU set is a bitmap whose bits are set according to CPU
 * physical OS indexes.
 *
 * It may be consulted and modified with the bitmap API as any
 * ::hwloc_bitmap_t (see hwloc/bitmap.h).
 */
typedef hwloc_bitmap_t hwloc_cpuset_t;
/** \brief A non-modifiable ::hwloc_cpuset_t. */
typedef hwloc_const_bitmap_t hwloc_const_cpuset_t;

/** \brief A node set is a bitmap whose bits are set according to NUMA
 * memory node physical OS indexes.
 *
 * It may be consulted and modified with the bitmap API as any
 * ::hwloc_bitmap_t (see hwloc/bitmap.h).
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



/** \defgroup hwlocality_object_types Object Types
 * @{
 */

/** \brief Type of topology object.
 *
 * \note Do not rely on the ordering or completeness of the values as new ones
 * may be defined in the future!  If you need to compare types, use
 * hwloc_compare_types() instead.
 */
typedef enum {
    /* ***************************************************************
       WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING

       If new enum values are added here, you MUST also go update the
       obj_type_order[] and obj_order_type[] arrays in src/topology.c.

       WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
       *************************************************************** */

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
  HWLOC_OBJ_CACHE,	/**< \brief Cache.
			  * Can be L1i, L1d, L2, L3, ...
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
			  * account for affinity. For instance, some operating systems
			  * expose their arbitrary processors aggregation this
			  * way.  And hwloc may insert such objects to group
			  * NUMA nodes according to their distances.
			  *
			  * These objects are ignored when they do not bring
			  * any structure.
			  */

  HWLOC_OBJ_MISC,	/**< \brief Miscellaneous objects.
			  * Objects without particular meaning, that can e.g. be
			  * added by the application for its own use.
			  */

  HWLOC_OBJ_BRIDGE,	/**< \brief Bridge.
			  * Any bridge that connects the host or an I/O bus,
			  * to another I/O bus.
			  * Bridge objects have neither CPU sets nor node sets.
			  * They are not added to the topology unless I/O discovery
			  * is enabled with hwloc_topology_set_flags().
			  */
  HWLOC_OBJ_PCI_DEVICE,	/**< \brief PCI device.
			  * These objects have neither CPU sets nor node sets.
			  * They are not added to the topology unless I/O discovery
			  * is enabled with hwloc_topology_set_flags().
			  */
  HWLOC_OBJ_OS_DEVICE,	/**< \brief Operating system device.
			  * These objects have neither CPU sets nor node sets.
			  * They are not added to the topology unless I/O discovery
			  * is enabled with hwloc_topology_set_flags().
			  */

  HWLOC_OBJ_TYPE_MAX    /**< \private Sentinel value */

    /* ***************************************************************
       WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING

       If new enum values are added here, you MUST also go update the
       obj_type_order[] and obj_order_type[] arrays in src/topology.c.

       WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
       *************************************************************** */
} hwloc_obj_type_t;

/** \brief Cache type. */
typedef enum hwloc_obj_cache_type_e {
  HWLOC_OBJ_CACHE_UNIFIED,      /**< \brief Unified cache. */
  HWLOC_OBJ_CACHE_DATA,         /**< \brief Data cache. */
  HWLOC_OBJ_CACHE_INSTRUCTION   /**< \brief Instruction cache.
				  * Only used when the HWLOC_TOPOLOGY_FLAG_ICACHES topology flag is set. */
} hwloc_obj_cache_type_t;

/** \brief Type of one side (upstream or downstream) of an I/O bridge. */
typedef enum hwloc_obj_bridge_type_e {
  HWLOC_OBJ_BRIDGE_HOST,	/**< \brief Host-side of a bridge, only possible upstream. */
  HWLOC_OBJ_BRIDGE_PCI		/**< \brief PCI-side of a bridge. */
} hwloc_obj_bridge_type_t;

/** \brief Type of a OS device. */
typedef enum hwloc_obj_osdev_type_e {
  HWLOC_OBJ_OSDEV_BLOCK,	/**< \brief Operating system block device.
				  * For instance "sda" on Linux. */
  HWLOC_OBJ_OSDEV_GPU,		/**< \brief Operating system GPU device.
				  * For instance ":0.0" for a GL display,
				  * "card0" for a Linux DRM device. */
  HWLOC_OBJ_OSDEV_NETWORK,	/**< \brief Operating system network device.
				  * For instance the "eth0" interface on Linux. */
  HWLOC_OBJ_OSDEV_OPENFABRICS,	/**< \brief Operating system openfabrics device.
				  * For instance the "mlx4_0" InfiniBand HCA device on Linux. */
  HWLOC_OBJ_OSDEV_DMA,		/**< \brief Operating system dma engine device.
				  * For instance the "dma0chan0" DMA channel on Linux. */
  HWLOC_OBJ_OSDEV_COPROC	/**< \brief Operating system co-processor device.
				  * For instance "mic0" for a Xeon Phi (MIC) on Linux,
				  * "opencl0d0" for a OpenCL device,
				  * "cuda0" for a CUDA device. */
} hwloc_obj_osdev_type_t;

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



/** \defgroup hwlocality_objects Object Structure and Attributes
 * @{
 */

union hwloc_obj_attr_u;

/** \brief Object memory */
struct hwloc_obj_memory_s {
  hwloc_uint64_t total_memory; /**< \brief Total memory (in bytes) in this object and its children */
  hwloc_uint64_t local_memory; /**< \brief Local memory (in bytes) */

  /** \brief Size of array \p page_types */
  unsigned page_types_len;
  /** \brief Array of local memory page types, \c NULL if no local memory and \p page_types is 0.
   *
   * The array is sorted by increasing \p size fields.
   * It contains \p page_types_len slots.
   */
  struct hwloc_obj_memory_page_type_s {
    hwloc_uint64_t size;	/**< \brief Size of pages */
    hwloc_uint64_t count;	/**< \brief Number of pages of this size */
  } * page_types;
};

/** \brief Structure of a topology object
 *
 * Applications must not modify any field except hwloc_obj.userdata.
 */
struct hwloc_obj {
  /* physical information */
  hwloc_obj_type_t type;		/**< \brief Type of object */
  unsigned os_index;			/**< \brief OS-provided physical index number.
					 * It is not guaranteed unique across the entire machine,
					 * except for PUs and NUMA nodes.
					 */
  char *name;				/**< \brief Object description if any */

  struct hwloc_obj_memory_s memory;	/**< \brief Memory attributes */

  union hwloc_obj_attr_u *attr;		/**< \brief Object type-specific Attributes,
					 * may be \c NULL if no attribute value was found */

  /* global position */
  unsigned depth;			/**< \brief Vertical index in the hierarchy.
					 * If the topology is symmetric, this is equal to the
					 * parent depth plus one, and also equal to the number
					 * of parent/child links from the root object to here.
					 */
  unsigned logical_index;		/**< \brief Horizontal index in the whole list of similar objects,
					 * hence guaranteed unique across the entire machine.
					 * Could be a "cousin_rank" since it's the rank within the "cousin" list below
					 */
  signed os_level;			/**< \brief OS-provided physical level, -1 if unknown or meaningless */

  /* cousins are all objects of the same type (and depth) across the entire topology */
  struct hwloc_obj *next_cousin;	/**< \brief Next object of same type and depth */
  struct hwloc_obj *prev_cousin;	/**< \brief Previous object of same type and depth */

  /* children of the same parent are siblings, even if they may have different type and depth */
  struct hwloc_obj *parent;		/**< \brief Parent, \c NULL if root (system object) */
  unsigned sibling_rank;		/**< \brief Index in parent's \c children[] array */
  struct hwloc_obj *next_sibling;	/**< \brief Next object below the same parent */
  struct hwloc_obj *prev_sibling;	/**< \brief Previous object below the same parent */

  /* children array below this object */
  unsigned arity;			/**< \brief Number of children */
  struct hwloc_obj **children;		/**< \brief Children, \c children[0 .. arity -1] */
  struct hwloc_obj *first_child;	/**< \brief First child */
  struct hwloc_obj *last_child;		/**< \brief Last child */

  /* misc */
  void *userdata;			/**< \brief Application-given private data pointer,
					 * initialized to \c NULL, use it as you wish.
					 * See hwloc_topology_set_userdata_export_callback()
					 * if you wish to export this field to XML. */

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

  struct hwloc_distances_s **distances;	/**< \brief Distances between all objects at same depth below this object */
  unsigned distances_count;

  struct hwloc_obj_info_s *infos;	/**< \brief Array of stringified info type=name. */
  unsigned infos_count;			/**< \brief Size of infos array. */

  int symmetric_subtree;		/**< \brief Set if the subtree of objects below this object is symmetric,
					  * which means all children and their children have identical subtrees.
					  * If set in the topology root object, lstopo may export the topology
					  * as a synthetic string.
					  */
};
/**
 * \brief Convenience typedef; a pointer to a struct hwloc_obj.
 */
typedef struct hwloc_obj * hwloc_obj_t;

/** \brief Object type-specific Attributes */
union hwloc_obj_attr_u {
  /** \brief Cache-specific Object Attributes */
  struct hwloc_cache_attr_s {
    hwloc_uint64_t size;		  /**< \brief Size of cache in bytes */
    unsigned depth;			  /**< \brief Depth of cache (e.g., L1, L2, ...etc.) */
    unsigned linesize;			  /**< \brief Cache-line size in bytes. 0 if unknown */
    int associativity;			  /**< \brief Ways of associativity,
    					    *  -1 if fully associative, 0 if unknown */
    hwloc_obj_cache_type_t type;          /**< \brief Cache type */
  } cache;
  /** \brief Group-specific Object Attributes */
  struct hwloc_group_attr_s {
    unsigned depth;			  /**< \brief Depth of group object */
  } group;
  /** \brief PCI Device specific Object Attributes */
  struct hwloc_pcidev_attr_s {
    unsigned short domain;
    unsigned char bus, dev, func;
    unsigned short class_id;
    unsigned short vendor_id, device_id, subvendor_id, subdevice_id;
    unsigned char revision;
    float linkspeed; /* in GB/s */
  } pcidev;
  /** \brief Bridge specific Object Attribues */
  struct hwloc_bridge_attr_s {
    union {
      struct hwloc_pcidev_attr_s pci;
    } upstream;
    hwloc_obj_bridge_type_t upstream_type;
    union {
      struct {
	unsigned short domain;
	unsigned char secondary_bus, subordinate_bus;
      } pci;
    } downstream;
    hwloc_obj_bridge_type_t downstream_type;
    unsigned depth;
  } bridge;
  /** \brief OS Device specific Object Attributes */
  struct hwloc_osdev_attr_s {
    hwloc_obj_osdev_type_t type;
  } osdev;
};

/** \brief Distances between objects
 *
 * One object may contain a distance structure describing distances
 * between all its descendants at a given relative depth. If the
 * containing object is the root object of the topology, then the
 * distances are available for all objects in the machine.
 *
 * If the \p latency pointer is not \c NULL, the pointed array contains
 * memory latencies (non-zero values), as defined by the ACPI SLIT
 * specification.
 *
 * In the future, some other types of distances may be considered.
 * In these cases, \p latency may be \c NULL.
 */
struct hwloc_distances_s {
  unsigned relative_depth;	/**< \brief Relative depth of the considered objects
				 * below the object containing this distance information. */
  unsigned nbobjs;		/**< \brief Number of objects considered in the matrix.
				 * It is the number of descendant objects at \p relative_depth
				 * below the containing object.
				 * It corresponds to the result of hwloc_get_nbobjs_inside_cpuset_by_depth. */

  float *latency;		/**< \brief Matrix of latencies between objects, stored as a one-dimension array.
				 * May be \c NULL if the distances considered here are not latencies.
				 * Values are normalized to get 1.0 as the minimal value in the matrix.
				 * Latency from i-th to j-th object is stored in slot i*nbobjs+j.
				 */
  float latency_max;		/**< \brief The maximal value in the latency matrix. */
  float latency_base;		/**< \brief The multiplier that should be applied to latency matrix
				 * to retrieve the original OS-provided latencies.
				 * Usually 10 on Linux since ACPI SLIT uses 10 for local latency.
				 */
};

/** \brief Object info */
struct hwloc_obj_info_s {
  char *name;	/**< \brief Info name */
  char *value;	/**< \brief Info value */
};

/** @} */



/** \defgroup hwlocality_creation Topology Creation and Destruction
 * @{
 */

struct hwloc_topology;
/** \brief Topology context
 *
 * To be initialized with hwloc_topology_init() and built with hwloc_topology_load().
 */
typedef struct hwloc_topology * hwloc_topology_t;

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
 * \note On failure, the topology is reinitialized. It should be either
 * destroyed with hwloc_topology_destroy() or configured and loaded again.
 *
 * \note This function may be called only once per topology.
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
 * The program aborts if an inconsistency is detected in the given topology.
 *
 * \param topology is the topology to be checked
 *
 * \note This routine is only useful to developers.
 *
 * \note The input topology should have been previously loaded with
 * hwloc_topology_load().
 */
HWLOC_DECLSPEC void hwloc_topology_check(hwloc_topology_t topology);

/** @} */



/** \defgroup hwlocality_configuration Topology Detection Configuration and Query
 *
 * Several functions can optionally be called between hwloc_topology_init() and
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
 * I/O objects may not be ignored, topology flags should be used to configure
 * their discovery instead.
 */
HWLOC_DECLSPEC int hwloc_topology_ignore_type(hwloc_topology_t topology, hwloc_obj_type_t type);

/** \brief Ignore an object type if it does not bring any structure.
 *
 * Ignore all objects from the given type as long as they do not bring any structure:
 * Each ignored object should have a single children or be the only child of its parent.
 * The bottom-level type HWLOC_OBJ_PU may not be ignored.
 * I/O objects may not be ignored, topology flags should be used to configure
 * their discovery instead.
 */
HWLOC_DECLSPEC int hwloc_topology_ignore_type_keep_structure(hwloc_topology_t topology, hwloc_obj_type_t type);

/** \brief Ignore all objects that do not bring any structure.
 *
 * Ignore all objects that do not bring any structure:
 * Each ignored object should have a single children or be the only child of its parent.
 * I/O objects may not be ignored, topology flags should be used to configure
 * their discovery instead.
 */
HWLOC_DECLSPEC int hwloc_topology_ignore_all_keep_structure(hwloc_topology_t topology);

/** \brief Flags to be set onto a topology context before load.
 *
 * Flags should be given to hwloc_topology_set_flags().
 * They may also be returned by hwloc_topology_get_flags().
 */
enum hwloc_topology_flags_e {
 /** \brief Detect the whole system, ignore reservations and offline settings.
   *
   * Gather all resources, even if some were disabled by the administrator.
   * For instance, ignore Linux Cpusets and gather all processors and memory nodes,
   * and ignore the fact that some resources may be offline.
   * \hideinitializer
   */
  HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM = (1UL<<0),

 /** \brief Assume that the selected backend provides the topology for the
   * system on which we are running.
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
   * \hideinitializer
   */
  HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM = (1UL<<1),

  /** \brief Detect PCI devices.
   *
   * By default, I/O devices are ignored. This flag enables I/O device
   * detection using the pci backend. Only the common PCI devices (GPUs,
   * NICs, block devices, ...) and host bridges (objects that connect the host
   * objects to an I/O subsystem) will be added to the topology.
   * Uncommon devices and other bridges (such as PCI-to-PCI bridges) will be
   * ignored.
   * \hideinitializer
   */
  HWLOC_TOPOLOGY_FLAG_IO_DEVICES = (1UL<<2),

  /** \brief Detect PCI bridges.
   *
   * This flag should be combined with HWLOC_TOPOLOGY_FLAG_IO_DEVICES to enable
   * the detection of both common devices and of all useful bridges (bridges that
   * have at least one device behind them).
   * \hideinitializer
   */
  HWLOC_TOPOLOGY_FLAG_IO_BRIDGES = (1UL<<3),

  /** \brief Detect the whole PCI hierarchy.
   *
   * This flag enables detection of all I/O devices (even the uncommon ones)
   * and bridges (even those that have no device behind them) using the pci
   * backend.
   * \hideinitializer
   */
  HWLOC_TOPOLOGY_FLAG_WHOLE_IO = (1UL<<4),

  /** \brief Detect instruction caches.
   *
   * This flag enables detection of Instruction caches,
   * instead of only Data and Unified caches.
   * \hideinitializer
   */
  HWLOC_TOPOLOGY_FLAG_ICACHES = (1UL<<5)
};

/** \brief Set OR'ed flags to non-yet-loaded topology.
 *
 * Set a OR'ed set of ::hwloc_topology_flags_e onto a topology that was not yet loaded.
 *
 * If this function is called multiple times, the last invokation will erase
 * and replace the set of flags that was previously set.
 *
 * The flags set in a topology may be retrieved with hwloc_topology_get_flags()
 */
HWLOC_DECLSPEC int hwloc_topology_set_flags (hwloc_topology_t topology, unsigned long flags);

/** \brief Get OR'ed flags of a topology.
 *
 * Get the OR'ed set of ::hwloc_topology_flags_e of a topology.
 *
 * \return the flags previously set with hwloc_topology_set_flags().
 */
HWLOC_DECLSPEC unsigned long hwloc_topology_get_flags (hwloc_topology_t topology);

/** \brief Change which pid the topology is viewed from
 *
 * On some systems, processes may have different views of the machine, for
 * instance the set of allowed CPUs. By default, hwloc exposes the view from
 * the current process. Calling hwloc_topology_set_pid() permits to make it
 * expose the topology of the machine from the point of view of another
 * process.
 *
 * \note \p hwloc_pid_t is \p pid_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 *
 * \note -1 is returned and errno is set to ENOSYS on platforms that do not
 * support this feature.
 */
HWLOC_DECLSPEC int hwloc_topology_set_pid(hwloc_topology_t __hwloc_restrict topology, hwloc_pid_t pid);

/** \brief Change the file-system root path when building the topology from sysfs/procfs.
 *
 * On Linux system, use sysfs and procfs files as if they were mounted on the given
 * \p fsroot_path instead of the main file-system root. Setting the environment
 * variable HWLOC_FSROOT may also result in this behavior.
 * Not using the main file-system root causes hwloc_topology_is_thissystem()
 * to return 0.
 *
 * Note that this function does not actually load topology
 * information; it just tells hwloc where to load it from.  You'll
 * still need to invoke hwloc_topology_load() to actually load the
 * topology information.
 *
 * \return -1 with errno set to ENOSYS on non-Linux and on Linux systems that
 * do not support it.
 * \return -1 with the appropriate errno if \p fsroot_path cannot be used.
 *
 * \note For convenience, this backend provides empty binding hooks which just
 * return success.  To have hwloc still actually call OS-specific hooks, the
 * HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM has to be set to assert that the loaded
 * file is really the underlying system.
 *
 * \note On success, the Linux component replaces the previously enabled
 * component (if any), but the topology is not actually modified until
 * hwloc_topology_load().
 */
HWLOC_DECLSPEC int hwloc_topology_set_fsroot(hwloc_topology_t __hwloc_restrict topology, const char * __hwloc_restrict fsroot_path);

/** \brief Enable synthetic topology.
 *
 * Gather topology information from the given \p description,
 * a space-separated string of numbers describing
 * the arity of each level.
 * Each number may be prefixed with a type and a colon to enforce the type
 * of a level.  If only some level types are enforced, hwloc will try to
 * choose the other types according to usual topologies, but it may fail
 * and you may have to specify more level types manually.
 * See also the \ref synthetic.
 *
 * If \p description was properly parsed and describes a valid topology
 * configuration, this function returns 0.
 * Otherwise -1 is returned and errno is set to EINVAL.
 *
 * Note that this function does not actually load topology
 * information; it just tells hwloc where to load it from.  You'll
 * still need to invoke hwloc_topology_load() to actually load the
 * topology information.
 *
 * \note For convenience, this backend provides empty binding hooks which just
 * return success.
 *
 * \note On success, the synthetic component replaces the previously enabled
 * component (if any), but the topology is not actually modified until
 * hwloc_topology_load().
 */
HWLOC_DECLSPEC int hwloc_topology_set_synthetic(hwloc_topology_t __hwloc_restrict topology, const char * __hwloc_restrict description);

/** \brief Enable XML-file based topology.
 *
 * Gather topology information from the XML file given at \p xmlpath.
 * Setting the environment variable HWLOC_XMLFILE may also result in this behavior.
 * This file may have been generated earlier with hwloc_topology_export_xml()
 * or lstopo file.xml.
 *
 * Note that this function does not actually load topology
 * information; it just tells hwloc where to load it from.  You'll
 * still need to invoke hwloc_topology_load() to actually load the
 * topology information.
 *
 * \return -1 with errno set to EINVAL on failure to read the XML file.
 *
 * \note See also hwloc_topology_set_userdata_import_callback()
 * for importing application-specific object userdata.
 *
 * \note For convenience, this backend provides empty binding hooks which just
 * return success.  To have hwloc still actually call OS-specific hooks, the
 * HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM has to be set to assert that the loaded
 * file is really the underlying system.
 *
 * \note On success, the XML component replaces the previously enabled
 * component (if any), but the topology is not actually modified until
 * hwloc_topology_load().
 */
HWLOC_DECLSPEC int hwloc_topology_set_xml(hwloc_topology_t __hwloc_restrict topology, const char * __hwloc_restrict xmlpath);

/** \brief Enable XML based topology using a memory buffer (instead of
 * a file, as with hwloc_topology_set_xml()).
 *
 * Gather topology information from the XML memory buffer given at \p
 * buffer and of length \p size.  This buffer may have been filled
 * earlier with hwloc_topology_export_xmlbuffer().
 *
 * Note that this function does not actually load topology
 * information; it just tells hwloc where to load it from.  You'll
 * still need to invoke hwloc_topology_load() to actually load the
 * topology information.
 *
 * \return -1 with errno set to EINVAL on failure to read the XML buffer.
 *
 * \note See also hwloc_topology_set_userdata_import_callback()
 * for importing application-specific object userdata.
 *
 * \note For convenience, this backend provides empty binding hooks which just
 * return success.  To have hwloc still actually call OS-specific hooks, the
 * HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM has to be set to assert that the loaded
 * file is really the underlying system.
 *
 * \note On success, the XML component replaces the previously enabled
 * component (if any), but the topology is not actually modified until
 * hwloc_topology_load().
 */
HWLOC_DECLSPEC int hwloc_topology_set_xmlbuffer(hwloc_topology_t __hwloc_restrict topology, const char * __hwloc_restrict buffer, int size);

/** \brief Prepare the topology for custom assembly.
 *
 * The topology then contains a single root object.
 * It must then be built by inserting other topologies with
 * hwloc_custom_insert_topology() or single objects with
 * hwloc_custom_insert_group_object_by_parent().
 * hwloc_topology_load() must be called to finalize the new
 * topology as usual.
 *
 * \note If nothing is inserted in the topology,
 * hwloc_topology_load() will fail with errno set to EINVAL.
 *
 * \note The cpuset and nodeset of the root object are NULL because
 * these sets are meaningless when assembling multiple topologies.
 *
 * \note On success, the custom component replaces the previously enabled
 * component (if any), but the topology is not actually modified until
 * hwloc_topology_load().
 */
HWLOC_DECLSPEC int hwloc_topology_set_custom(hwloc_topology_t topology);

/** \brief Provide a distance matrix.
 *
 * Provide the matrix of distances between a set of objects of the given type.
 * The set may or may not contain all the existing objects of this type.
 * The objects are specified by their OS/physical index in the \p os_index
 * array. The \p distances matrix follows the same order.
 * The distance from object i to object j in the i*nbobjs+j.
 *
 * A single latency matrix may be defined for each type.
 * If another distance matrix already exists for the given type,
 * either because the user specified it or because the OS offers it,
 * it will be replaced by the given one.
 * If \p nbobjs is \c 0, \p os_index is \c NULL and \p distances is \c NULL,
 * the existing distance matrix for the given type is removed.
 *
 * \note Distance matrices are ignored in multi-node topologies.
 */
HWLOC_DECLSPEC int hwloc_topology_set_distance_matrix(hwloc_topology_t __hwloc_restrict topology,
						      hwloc_obj_type_t type, unsigned nbobjs,
						      unsigned *os_index, float *distances);

/** \brief Does the topology context come from this system?
 *
 * \return 1 if this topology context was built using the system
 * running this program.
 * \return 0 instead (for instance if using another file-system root,
 * a XML topology file, or a synthetic topology).
 */
HWLOC_DECLSPEC int hwloc_topology_is_thissystem(hwloc_topology_t  __hwloc_restrict topology) __hwloc_attribute_pure;

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
  /** Getting the last processors where the whole current process ran is supported */
  unsigned char get_thisproc_last_cpu_location;
  /** Getting the last processors where a whole process ran is supported */
  unsigned char get_proc_last_cpu_location;
  /** Getting the last processors where the current thread ran is supported */
  unsigned char get_thisthread_last_cpu_location;
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



/** \defgroup hwlocality_levels Object levels, depths and types
 * @{
 *
 * Be sure to see the figure in \ref termsanddefs that shows a
 * complete topology tree, including depths, child/sibling/cousin
 * relationships, and an example of an asymmetric topology where one
 * socket has fewer caches than its peers.
 */

/** \brief Get the depth of the hierarchical tree of objects.
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
 *
 * If some objects of the given type exist in different levels,
 * for instance L1 and L2 caches, or L1i and L1d caches,
 * the function returns HWLOC_TYPE_DEPTH_MULTIPLE.
 * See hwloc_get_cache_type_depth() in hwloc/helper.h to better handle this
 * case.
 *
 * If an I/O object type is given, the function returns a virtual value
 * because I/O objects are stored in special levels that are not CPU-related.
 * This virtual depth may be passed to other hwloc functions such as
 * hwloc_get_obj_by_depth() but it should not be considered as an actual
 * depth by the application. In particular, it should not be compared with
 * any other object depth or with the entire topology depth.
 */
HWLOC_DECLSPEC int hwloc_get_type_depth (hwloc_topology_t topology, hwloc_obj_type_t type);

enum hwloc_get_type_depth_e {
    HWLOC_TYPE_DEPTH_UNKNOWN = -1,    /**< \brief No object of given type exists in the topology. \hideinitializer */
    HWLOC_TYPE_DEPTH_MULTIPLE = -2,   /**< \brief Objects of given type exist at different depth in the topology. \hideinitializer */
    HWLOC_TYPE_DEPTH_BRIDGE = -3,     /**< \brief Virtual depth for bridge object level. \hideinitializer */
    HWLOC_TYPE_DEPTH_PCI_DEVICE = -4, /**< \brief Virtual depth for PCI device object level. \hideinitializer */
    HWLOC_TYPE_DEPTH_OS_DEVICE = -5   /**< \brief Virtual depth for software device object level. \hideinitializer */
};

/** \brief Returns the depth of objects of type \p type or below
 *
 * If no object of this type is present on the underlying architecture, the
 * function returns the depth of the first "present" object typically found
 * inside \p type.
 *
 * If some objects of the given type exist in different levels, for instance
 * L1 and L2 caches, the function returns HWLOC_TYPE_DEPTH_MULTIPLE.
 */
static __hwloc_inline int
hwloc_get_type_or_below_depth (hwloc_topology_t topology, hwloc_obj_type_t type) __hwloc_attribute_pure;

/** \brief Returns the depth of objects of type \p type or above
 *
 * If no object of this type is present on the underlying architecture, the
 * function returns the depth of the first "present" object typically
 * containing \p type.
 *
 * If some objects of the given type exist in different levels, for instance
 * L1 and L2 caches, the function returns HWLOC_TYPE_DEPTH_MULTIPLE.
 */
static __hwloc_inline int
hwloc_get_type_or_above_depth (hwloc_topology_t topology, hwloc_obj_type_t type) __hwloc_attribute_pure;

/** \brief Returns the type of objects at depth \p depth.
 *
 * \return -1 if depth \p depth does not exist.
 */
HWLOC_DECLSPEC hwloc_obj_type_t hwloc_get_depth_type (hwloc_topology_t topology, unsigned depth) __hwloc_attribute_pure;

/** \brief Returns the width of level at depth \p depth.
 */
HWLOC_DECLSPEC unsigned hwloc_get_nbobjs_by_depth (hwloc_topology_t topology, unsigned depth) __hwloc_attribute_pure;

/** \brief Returns the width of level type \p type
 *
 * If no object for that type exists, 0 is returned.
 * If there are several levels with objects of that type, -1 is returned.
 */
static __hwloc_inline int
hwloc_get_nbobjs_by_type (hwloc_topology_t topology, hwloc_obj_type_t type) __hwloc_attribute_pure;

/** \brief Returns the top-object of the topology-tree.
 *
 * Its type is typically ::HWLOC_OBJ_MACHINE but it could be different
 * for complex topologies.
 */
static __hwloc_inline hwloc_obj_t
hwloc_get_root_obj (hwloc_topology_t topology) __hwloc_attribute_pure;

/** \brief Returns the topology object at logical index \p idx from depth \p depth */
HWLOC_DECLSPEC hwloc_obj_t hwloc_get_obj_by_depth (hwloc_topology_t topology, unsigned depth, unsigned idx) __hwloc_attribute_pure;

/** \brief Returns the topology object at logical index \p idx with type \p type
 *
 * If no object for that type exists, \c NULL is returned.
 * If there are several levels with objects of that type, \c NULL is returned
 * and ther caller may fallback to hwloc_get_obj_by_depth().
 */
static __hwloc_inline hwloc_obj_t
hwloc_get_obj_by_type (hwloc_topology_t topology, hwloc_obj_type_t type, unsigned idx) __hwloc_attribute_pure;

/** \brief Returns the next object at depth \p depth.
 *
 * If \p prev is \c NULL, return the first object at depth \p depth.
 */
static __hwloc_inline hwloc_obj_t
hwloc_get_next_obj_by_depth (hwloc_topology_t topology, unsigned depth, hwloc_obj_t prev);

/** \brief Returns the next object of type \p type.
 *
 * If \p prev is \c NULL, return the first object at type \p type.  If
 * there are multiple or no depth for given type, return \c NULL and
 * let the caller fallback to hwloc_get_next_obj_by_depth().
 */
static __hwloc_inline hwloc_obj_t
hwloc_get_next_obj_by_type (hwloc_topology_t topology, hwloc_obj_type_t type,
			    hwloc_obj_t prev);

/** @} */



/** \defgroup hwlocality_object_strings Manipulating Object Type, Sets and Attributes as Strings
 * @{
 */

/** \brief Return a stringified topology object type */
HWLOC_DECLSPEC const char * hwloc_obj_type_string (hwloc_obj_type_t type) __hwloc_attribute_const;

/** \brief Return an object type and attributes from a type string.
 *
 * Convert strings such as "socket" or "cache" into the corresponding types.
 * Matching is case-insensitive, and only the first letters are actually
 * required to match.
 *
 * Types that have specific attributes, for instance caches and groups,
 * may be returned in \p depthattrp and \p typeattrp. They are ignored
 * when these pointers are \c NULL.
 *
 * For instance "L2i" or "L2iCache" would return
 * type HWLOC_OBJ_CACHE in \p typep, 2 in \p depthattrp,
 * and HWLOC_OBJ_CACHE_TYPE_INSTRUCTION in \p typeattrp
 * (this last pointer should point to a hwloc_obj_cache_type_t).
 * "Group3" would return type HWLOC_OBJ_GROUP type and 3 in \p depthattrp.
 * Attributes that are not specified in the string (for instance "Group"
 * without a depth, or "L2Cache" without a cache type) are set to -1.
 *
 * \p typeattrd is only filled if the size specified in \p typeattrsize
 * is large enough. It is currently only used for caches, and the required
 * size is at least the size of hwloc_obj_cache_type_t.
 *
 * \return 0 if a type was correctly identified, otherwise -1.
 *
 * \note This is an extended version of the now deprecated hwloc_obj_type_of_string()
 */
HWLOC_DECLSPEC int hwloc_obj_type_sscanf(const char *string,
					 hwloc_obj_type_t *typep,
					 int *depthattrp,
					 void *typeattrp, size_t typeattrsize);

/** \brief Stringify the type of a given topology object into a human-readable form.
 *
 * It differs from hwloc_obj_type_string() because it prints type attributes such
 * as cache depth and type.
 *
 * If \p size is 0, \p string may safely be \c NULL.
 *
 * \return the number of character that were actually written if not truncating,
 * or that would have been written (not including the ending \\0).
 */
HWLOC_DECLSPEC int hwloc_obj_type_snprintf(char * __hwloc_restrict string, size_t size, hwloc_obj_t obj,
				   int verbose);

/** \brief Stringify the attributes of a given topology object into a human-readable form.
 *
 * Attribute values are separated by \p separator.
 *
 * Only the major attributes are printed in non-verbose mode.
 *
 * If \p size is 0, \p string may safely be \c NULL.
 *
 * \return the number of character that were actually written if not truncating,
 * or that would have been written (not including the ending \\0).
 */
HWLOC_DECLSPEC int hwloc_obj_attr_snprintf(char * __hwloc_restrict string, size_t size, hwloc_obj_t obj, const char * __hwloc_restrict separator,
				   int verbose);

/** \brief Stringify the cpuset containing a set of objects.
 *
 * If \p size is 0, \p string may safely be \c NULL.
 *
 * \return the number of character that were actually written if not truncating,
 * or that would have been written (not including the ending \\0).
 */
HWLOC_DECLSPEC int hwloc_obj_cpuset_snprintf(char * __hwloc_restrict str, size_t size, size_t nobj, const hwloc_obj_t * __hwloc_restrict objs);

/** \brief Search the given key name in object infos and return the corresponding value.
 *
 * If multiple keys match the given name, only the first one is returned.
 *
 * \return \c NULL if no such key exists.
 */
static __hwloc_inline const char *
hwloc_obj_get_info_by_name(hwloc_obj_t obj, const char *name) __hwloc_attribute_pure;

/** \brief Add the given info name and value pair to the given object.
 *
 * The info is appended to the existing info array even if another key
 * with the same name already exists.
 *
 * The input strings are copied before being added in the object infos.
 *
 * \note This function may be used to enforce object colors in the lstopo
 * graphical output by using "lstopoStyle" as a name and "Background=#rrggbb"
 * as a value. See CUSTOM COLORS in the lstopo(1) manpage for details.
 *
 * \note If \p value contains some non-printable characters, they will
 * be dropped when exporting to XML, see hwloc_topology_export_xml().
 */
HWLOC_DECLSPEC void hwloc_obj_add_info(hwloc_obj_t obj, const char *name, const char *value);

/** @} */



/** \defgroup hwlocality_cpubinding CPU binding
 *
 * It is often useful to call hwloc_bitmap_singlify() first so that a single CPU
 * remains in the set. This way, the process will not even migrate between
 * different CPUs. Some operating systems also only support that kind of binding.
 *
 * \note Some operating systems do not provide all hwloc-supported
 * mechanisms to bind processes, threads, etc. and the corresponding
 * binding functions may fail. -1 is returned and errno is set to
 * ENOSYS when it is not possible to bind the requested kind of object
 * processes/threads. errno is set to EXDEV when the requested cpuset
 * can not be enforced (e.g. some systems only allow one CPU, and some
 * other systems only allow one NUMA node).
 *
 * The most portable version that should be preferred over the others, whenever
 * possible, is
 *
 * \code
 * hwloc_set_cpubind(topology, set, 0),
 * \endcode
 *
 * as it just binds the current program, assuming it is single-threaded, or
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
 * \note On some operating systems, CPU binding may have effects on memory binding, see
 * ::HWLOC_CPUBIND_NOMEMBIND
 *
 * Running lstopo --top can be a very convenient tool to check how binding
 * actually happened.
 * @{
 */

/** \brief Process/Thread binding flags.
 *
 * These bit flags can be used to refine the binding policy.
 *
 * The default (0) is to bind the current process, assumed to be
 * single-threaded, in a non-strict way.  This is the most portable
 * way to bind as all operating systems usually provide it.
 *
 * \note Not all systems support all kinds of binding.  See the
 * "Detailed Description" section of \ref hwlocality_cpubinding for a
 * description of errors that can occur.
 */
typedef enum {
  /** \brief Bind all threads of the current (possibly) multithreaded process.
   * \hideinitializer */
  HWLOC_CPUBIND_PROCESS = (1<<0),

  /** \brief Bind current thread of current process.
   * \hideinitializer */
  HWLOC_CPUBIND_THREAD = (1<<1),

  /** \brief Request for strict binding from the OS.
   *
   * By default, when the designated CPUs are all busy while other
   * CPUs are idle, operating systems may execute the thread/process
   * on those other CPUs instead of the designated CPUs, to let them
   * progress anyway.  Strict binding means that the thread/process
   * will _never_ execute on other cpus than the designated CPUs, even
   * when those are busy with other tasks and other CPUs are idle.
   *
   * \note Depending on the operating system, strict binding may not
   * be possible (e.g., the OS does not implement it) or not allowed
   * (e.g., for an administrative reasons), and the function will fail
   * in that case.
   *
   * When retrieving the binding of a process, this flag checks
   * whether all its threads  actually have the same binding. If the
   * flag is not given, the binding of each thread will be
   * accumulated.
   *
   * \note This flag is meaningless when retrieving the binding of a
   * thread.
   * \hideinitializer
   */
  HWLOC_CPUBIND_STRICT = (1<<2),

  /** \brief Avoid any effect on memory binding
   *
   * On some operating systems, some CPU binding function would also
   * bind the memory on the corresponding NUMA node.  It is often not
   * a problem for the application, but if it is, setting this flag
   * will make hwloc avoid using OS functions that would also bind
   * memory.  This will however reduce the support of CPU bindings,
   * i.e. potentially return -1 with errno set to ENOSYS in some
   * cases.
   *
   * This flag is only meaningful when used with functions that set
   * the CPU binding.  It is ignored when used with functions that get
   * CPU binding information.
   * \hideinitializer
   */
  HWLOC_CPUBIND_NOMEMBIND = (1<<3)
} hwloc_cpubind_flags_t;

/** \brief Bind current process or thread on cpus given in physical bitmap \p set.
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_cpubind(hwloc_topology_t topology, hwloc_const_cpuset_t set, int flags);

/** \brief Get current process or thread binding.
 *
 * Writes into \p set the physical cpuset which the process or thread (according to \e
 * flags) was last bound to.
 */
HWLOC_DECLSPEC int hwloc_get_cpubind(hwloc_topology_t topology, hwloc_cpuset_t set, int flags);

/** \brief Bind a process \p pid on cpus given in physical bitmap \p set.
 *
 * \note \p hwloc_pid_t is \p pid_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 *
 * \note As a special case on Linux, if a tid (thread ID) is supplied
 * instead of a pid (process ID) and HWLOC_CPUBIND_THREAD is passed in flags,
 * the binding is applied to that specific thread.
 *
 * \note On non-Linux systems, HWLOC_CPUBIND_THREAD can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_set_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t set, int flags);

/** \brief Get the current physical binding of process \p pid.
 *
 * \note \p hwloc_pid_t is \p pid_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 *
 * \note As a special case on Linux, if a tid (thread ID) is supplied
 * instead of a pid (process ID) and HWLOC_CPUBIND_THREAD is passed in flags,
 * the binding for that specific thread is returned.
 *
 * \note On non-Linux systems, HWLOC_CPUBIND_THREAD can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_get_proc_cpubind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t set, int flags);

#ifdef hwloc_thread_t
/** \brief Bind a thread \p thread on cpus given in physical bitmap \p set.
 *
 * \note \p hwloc_thread_t is \p pthread_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 *
 * \note HWLOC_CPUBIND_PROCESS can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_set_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t thread, hwloc_const_cpuset_t set, int flags);
#endif

#ifdef hwloc_thread_t
/** \brief Get the current physical binding of thread \p tid.
 *
 * \note \p hwloc_thread_t is \p pthread_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 *
 * \note HWLOC_CPUBIND_PROCESS can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_get_thread_cpubind(hwloc_topology_t topology, hwloc_thread_t thread, hwloc_cpuset_t set, int flags);
#endif

/** \brief Get the last physical CPU where the current process or thread ran.
 *
 * The operating system may move some tasks from one processor
 * to another at any time according to their binding,
 * so this function may return something that is already
 * outdated.
 *
 * \p flags can include either HWLOC_CPUBIND_PROCESS or HWLOC_CPUBIND_THREAD to
 * specify whether the query should be for the whole process (union of all CPUs
 * on which all threads are running), or only the current thread. If the
 * process is single-threaded, flags can be set to zero to let hwloc use
 * whichever method is available on the underlying OS.
 */
HWLOC_DECLSPEC int hwloc_get_last_cpu_location(hwloc_topology_t topology, hwloc_cpuset_t set, int flags);

/** \brief Get the last physical CPU where a process ran.
 *
 * The operating system may move some tasks from one processor
 * to another at any time according to their binding,
 * so this function may return something that is already
 * outdated.
 *
 * \note \p hwloc_pid_t is \p pid_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 *
 * \note As a special case on Linux, if a tid (thread ID) is supplied
 * instead of a pid (process ID) and HWLOC_CPUBIND_THREAD is passed in flags,
 * the last CPU location of that specific thread is returned.
 *
 * \note On non-Linux systems, HWLOC_CPUBIND_THREAD can not be used in \p flags.
 */
HWLOC_DECLSPEC int hwloc_get_proc_last_cpu_location(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t set, int flags);

/** @} */



/** \defgroup hwlocality_membinding Memory binding
 *
 * Memory binding can be done three ways:
 *
 * - explicit memory allocation thanks to hwloc_alloc_membind and friends: the
 *   binding will have effect on the memory allocated by these functions.
 * - implicit memory binding through binding policy: hwloc_set_membind and
 *   friends only define the current policy of the process, which will be
 *   applied to the subsequent calls to malloc() and friends.
 * - migration of existing memory ranges, thanks to hwloc_set_area_membind()
 *   and friends, which move already-allocated data.
 *
 * \note Not all operating systems support all three ways Using a binding flag
 * or policy that is not supported by the underlying OS will cause hwloc's
 * binding functions to fail and return -1.  errno will be set to
 * ENOSYS when the system does support the specified action or policy
 * (e.g., some systems only allow binding memory on a per-thread
 * basis, whereas other systems only allow binding memory for all
 * threads in a process).  errno will be set to EXDEV when the
 * requested cpuset can not be enforced (e.g., some systems only allow
 * binding memory to a single NUMA node).
 *
 * The most portable form that should be preferred over the others
 * whenever possible is as follows:
 *
 * \code
 * hwloc_alloc_membind_policy(topology, size, set, 
 *                            HWLOC_MEMBIND_DEFAULT, 0);
 * \endcode
 *
 * This will allocate some memory hopefully bound to the specified set.
 * To do so, hwloc will possibly have to change the current memory
 * binding policy in order to actually get the memory bound, if the OS
 * does not provide any other way to simply allocate bound memory
 * without changing the policy for all allocations. That is the
 * difference with hwloc_alloc_membind(), which will never change the
 * current memory binding policy. Note that since HWLOC_MEMBIND_STRICT
 * was not specified, failures to bind will not be reported --
 * generally, only memory allocation failures will be reported (e.g.,
 * even a plain malloc() would have failed with ENOMEM).
 *
 * Each hwloc memory binding function is available in two forms: one
 * that takes a CPU set argument and another that takes a NUMA memory
 * node set argument (see \ref hwlocality_object_sets and \ref
 * hwlocality_bitmap for a discussion of CPU sets and NUMA memory node
 * sets).  The names of the latter form end with _nodeset.  It is also
 * possible to convert between CPU set and node set using
 * hwloc_cpuset_to_nodeset() or hwloc_cpuset_from_nodeset().
 *
 * \note On some operating systems, memory binding affects the CPU
 * binding; see ::HWLOC_MEMBIND_NOCPUBIND 
 * @{
 */

/** \brief Memory binding policy.
 *
 * These constants can be used to choose the binding policy.  Only one policy can
 * be used at a time (i.e., the values cannot be OR'ed together).
 *
 * \note Not all systems support all kinds of binding.  See the
 * "Detailed Description" section of \ref hwlocality_membinding for a
 * description of errors that can occur.
 */
typedef enum {
  /** \brief Reset the memory allocation policy to the system default.
   * \hideinitializer */
  HWLOC_MEMBIND_DEFAULT =	0,

  /** \brief Allocate memory
   * but do not immediately bind it to a specific locality. Instead,
   * each page in the allocation is bound only when it is first
   * touched. Pages are individually bound to the local NUMA node of
   * the first thread that touches it. If there is not enough memory
   * on the node, allocation may be done in the specified cpuset
   * before allocating on other nodes.
   * \hideinitializer */
  HWLOC_MEMBIND_FIRSTTOUCH =	1,

  /** \brief Allocate memory on the specified nodes.
   * \hideinitializer */
  HWLOC_MEMBIND_BIND =		2,

  /** \brief Allocate memory on the given nodes in an interleaved
   * / round-robin manner.  The precise layout of the memory across
   * multiple NUMA nodes is OS/system specific. Interleaving can be
   * useful when threads distributed across the specified NUMA nodes
   * will all be accessing the whole memory range concurrently, since
   * the interleave will then balance the memory references.
   * \hideinitializer */
  HWLOC_MEMBIND_INTERLEAVE =	3,

  /** \brief Replicate memory on the given nodes; reads from this
   * memory will attempt to be serviced from the NUMA node local to
   * the reading thread. Replicating can be useful when multiple
   * threads from the specified NUMA nodes will be sharing the same
   * read-only data.
   *
   * This policy can only be used with existing memory allocations
   * (i.e., the hwloc_set_*membind*() functions); it cannot be used
   * with functions that allocate new memory (i.e., the hwloc_alloc*()
   * functions).
   * \hideinitializer */
  HWLOC_MEMBIND_REPLICATE =	4,

  /** \brief For each page bound with this policy, by next time
   * it is touched (and next time only), it is moved from its current
   * location to the local NUMA node of the thread where the memory
   * reference occurred (if it needs to be moved at all).
   * \hideinitializer */
  HWLOC_MEMBIND_NEXTTOUCH =	5,

  /** \brief Returned by hwloc_get_membind*() functions when multiple
   * threads or parts of a memory area have differing memory binding
   * policies.
   * \hideinitializer */
  HWLOC_MEMBIND_MIXED = -1             
} hwloc_membind_policy_t;

/** \brief Memory binding flags.
 *
 * These flags can be used to refine the binding policy.  All flags
 * can be logically OR'ed together with the exception of
 * HWLOC_MEMBIND_PROCESS and HWLOC_MEMBIND_THREAD; these two flags are
 * mutually exclusive.
 *
 * \note Not all systems support all kinds of binding.  See the
 * "Detailed Description" section of \ref hwlocality_membinding for a
 * description of errors that can occur.
 */
typedef enum {
  /** \brief Set policy for all threads of the specified (possibly
   * multithreaded) process.  This flag is mutually exclusive with
   * HWLOC_MEMBIND_THREAD.
   * \hideinitializer */
  HWLOC_MEMBIND_PROCESS =       (1<<0),

 /** \brief Set policy for a specific thread of the current process.
  * This flag is mutually exclusive with HWLOC_MEMBIND_PROCESS.
  * \hideinitializer */
  HWLOC_MEMBIND_THREAD =        (1<<1),

 /** Request strict binding from the OS.  The function will fail if
  * the binding can not be guaranteed / completely enforced.
  *
  * This flag has slightly different meanings depending on which
  * function it is used with.  
  * \hideinitializer  */
  HWLOC_MEMBIND_STRICT =        (1<<2),

 /** \brief Migrate existing allocated memory.  If the memory cannot
  * be migrated and the HWLOC_MEMBIND_STRICT flag is passed, an error
  * will be returned.
  * \hideinitializer  */
  HWLOC_MEMBIND_MIGRATE =       (1<<3),

  /** \brief Avoid any effect on CPU binding.
   *
   * On some operating systems, some underlying memory binding
   * functions also bind the application to the corresponding CPU(s).
   * Using this flag will cause hwloc to avoid using OS functions that
   * could potentially affect CPU bindings.  Note, however, that using
   * NOCPUBIND may reduce hwloc's overall memory binding
   * support. Specifically: some of hwloc's memory binding functions
   * may fail with errno set to ENOSYS when used with NOCPUBIND.
   * \hideinitializer
   */
  HWLOC_MEMBIND_NOCPUBIND =     (1<<4)
} hwloc_membind_flags_t;

/** \brief Set the default memory binding policy of the current
 * process or thread to prefer the NUMA node(s) specified by physical \p nodeset
 *
 * If neither HWLOC_MEMBIND_PROCESS nor HWLOC_MEMBIND_THREAD is
 * specified, the current process is assumed to be single-threaded.
 * This is the most portable form as it permits hwloc to use either
 * process-based OS functions or thread-based OS functions, depending
 * on which are available.
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_membind_nodeset(hwloc_topology_t topology, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);

/** \brief Set the default memory binding policy of the current
 * process or thread to prefer the NUMA node(s) near the specified physical \p
 * cpuset
 *
 * If neither HWLOC_MEMBIND_PROCESS nor HWLOC_MEMBIND_THREAD is
 * specified, the current process is assumed to be single-threaded.
 * This is the most portable form as it permits hwloc to use either
 * process-based OS functions or thread-based OS functions, depending
 * on which are available.
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_membind(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset, hwloc_membind_policy_t policy, int flags);

/** \brief Query the default memory binding policy and physical locality of the
 * current process or thread.
 *
 * This function has two output parameters: \p nodeset and \p policy.
 * The values returned in these parameters depend on both the \p flags
 * passed in and the current memory binding policies and nodesets in
 * the queried target.
 *
 * Passing the HWLOC_MEMBIND_PROCESS flag specifies that the query
 * target is the current policies and nodesets for all the threads in
 * the current process.  Passing HWLOC_MEMBIND_THREAD specifies that
 * the query target is the current policy and nodeset for only the
 * thread invoking this function.
 *
 * If neither of these flags are passed (which is the most portable
 * method), the process is assumed to be single threaded.  This allows
 * hwloc to use either process-based OS functions or thread-based OS
 * functions, depending on which are available.
 *
 * HWLOC_MEMBIND_STRICT is only meaningful when HWLOC_MEMBIND_PROCESS
 * is also specified.  In this case, hwloc will check the default
 * memory policies and nodesets for all threads in the process.  If
 * they are not identical, -1 is returned and errno is set to EXDEV.
 * If they are identical, the values are returned in \p nodeset and \p
 * policy.
 *
 * Otherwise, if HWLOC_MEMBIND_PROCESS is specified (and
 * HWLOC_MEMBIND_STRICT is \em not specified), \p nodeset is set to
 * the logical OR of all threads' default nodeset.  If all threads'
 * default policies are the same, \p policy is set to that policy.  If
 * they are different, \p policy is set to HWLOC_MEMBIND_MIXED.
 *
 * In the HWLOC_MEMBIND_THREAD case (or when neither
 * HWLOC_MEMBIND_PROCESS or HWLOC_MEMBIND_THREAD is specified), there
 * is only one nodeset and policy; they are returned in \p nodeset and
 * \p policy, respectively.
 *
 * If any other flags are specified, -1 is returned and errno is set
 * to EINVAL.
 */
HWLOC_DECLSPEC int hwloc_get_membind_nodeset(hwloc_topology_t topology, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);

/** \brief Query the default memory binding policy and physical locality of the
 * current process or thread (the locality is returned in \p cpuset as
 * CPUs near the locality's actual NUMA node(s)).
 *
 * This function has two output parameters: \p cpuset and \p policy.
 * The values returned in these parameters depend on both the \p flags
 * passed in and the current memory binding policies and nodesets in
 * the queried target.
 *
 * Passing the HWLOC_MEMBIND_PROCESS flag specifies that the query
 * target is the current policies and nodesets for all the threads in
 * the current process.  Passing HWLOC_MEMBIND_THREAD specifies that
 * the query target is the current policy and nodeset for only the
 * thread invoking this function.
 *
 * If neither of these flags are passed (which is the most portable
 * method), the process is assumed to be single threaded.  This allows
 * hwloc to use either process-based OS functions or thread-based OS
 * functions, depending on which are available.
 *
 * HWLOC_MEMBIND_STRICT is only meaningful when HWLOC_MEMBIND_PROCESS
 * is also specified.  In this case, hwloc will check the default
 * memory policies and nodesets for all threads in the process.  If
 * they are not identical, -1 is returned and errno is set to EXDEV.
 * If they are identical, the policy is returned in \p policy.  \p
 * cpuset is set to the union of CPUs near the NUMA node(s) in the
 * nodeset.
 *
 * Otherwise, if HWLOC_MEMBIND_PROCESS is specified (and
 * HWLOC_MEMBIND_STRICT is \em not specified), the default nodeset
 * from each thread is logically OR'ed together.  \p cpuset is set to
 * the union of CPUs near the NUMA node(s) in the resulting nodeset.
 * If all threads' default policies are the same, \p policy is set to
 * that policy.  If they are different, \p policy is set to
 * HWLOC_MEMBIND_MIXED.
 *
 * In the HWLOC_MEMBIND_THREAD case (or when neither
 * HWLOC_MEMBIND_PROCESS or HWLOC_MEMBIND_THREAD is specified), there
 * is only one nodeset and policy.  The policy is returned in \p
 * policy; \p cpuset is set to the union of CPUs near the NUMA node(s)
 * in the \p nodeset.
 *
 * If any other flags are specified, -1 is returned and errno is set
 * to EINVAL.
 */
HWLOC_DECLSPEC int hwloc_get_membind(hwloc_topology_t topology, hwloc_cpuset_t cpuset, hwloc_membind_policy_t * policy, int flags);

/** \brief Set the default memory binding policy of the specified
 * process to prefer the NUMA node(s) specified by physical \p nodeset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 *
 * \note \p hwloc_pid_t is \p pid_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 */
HWLOC_DECLSPEC int hwloc_set_proc_membind_nodeset(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);

/** \brief Set the default memory binding policy of the specified
 * process to prefer the NUMA node(s) near the specified physical \p cpuset
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 *
 * \note \p hwloc_pid_t is \p pid_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 */
HWLOC_DECLSPEC int hwloc_set_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_const_cpuset_t cpuset, hwloc_membind_policy_t policy, int flags);

/** \brief Query the default memory binding policy and physical locality of the
 * specified process.
 *
 * This function has two output parameters: \p nodeset and \p policy.
 * The values returned in these parameters depend on both the \p flags
 * passed in and the current memory binding policies and nodesets in
 * the queried target.
 *
 * Passing the HWLOC_MEMBIND_PROCESS flag specifies that the query
 * target is the current policies and nodesets for all the threads in
 * the specified process.  If HWLOC_MEMBIND_PROCESS is not specified
 * (which is the most portable method), the process is assumed to be
 * single threaded.  This allows hwloc to use either process-based OS
 * functions or thread-based OS functions, depending on which are
 * available.
 *
 * Note that it does not make sense to pass HWLOC_MEMBIND_THREAD to
 * this function.
 *
 * If HWLOC_MEMBIND_STRICT is specified, hwloc will check the default
 * memory policies and nodesets for all threads in the specified
 * process.  If they are not identical, -1 is returned and errno is
 * set to EXDEV.  If they are identical, the values are returned in \p
 * nodeset and \p policy.
 *
 * Otherwise, \p nodeset is set to the logical OR of all threads'
 * default nodeset.  If all threads' default policies are the same, \p
 * policy is set to that policy.  If they are different, \p policy is
 * set to HWLOC_MEMBIND_MIXED.
 *
 * If any other flags are specified, -1 is returned and errno is set
 * to EINVAL.
 *
 * \note \p hwloc_pid_t is \p pid_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 */
HWLOC_DECLSPEC int hwloc_get_proc_membind_nodeset(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);

/** \brief Query the default memory binding policy and physical locality of the
 * specified process (the locality is returned in \p cpuset as CPUs
 * near the locality's actual NUMA node(s)).
 *
 * This function has two output parameters: \p cpuset and \p policy.
 * The values returned in these parameters depend on both the \p flags
 * passed in and the current memory binding policies and nodesets in
 * the queried target.
 *
 * Passing the HWLOC_MEMBIND_PROCESS flag specifies that the query
 * target is the current policies and nodesets for all the threads in
 * the specified process.  If HWLOC_MEMBIND_PROCESS is not specified
 * (which is the most portable method), the process is assumed to be
 * single threaded.  This allows hwloc to use either process-based OS
 * functions or thread-based OS functions, depending on which are
 * available.
 *
 * Note that it does not make sense to pass HWLOC_MEMBIND_THREAD to
 * this function.
 *
 * If HWLOC_MEMBIND_STRICT is specified, hwloc will check the default
 * memory policies and nodesets for all threads in the specified
 * process.  If they are not identical, -1 is returned and errno is
 * set to EXDEV.  If they are identical, the policy is returned in \p
 * policy.  \p cpuset is set to the union of CPUs near the NUMA
 * node(s) in the nodeset.
 *
 * Otherwise, the default nodeset from each thread is logically OR'ed
 * together.  \p cpuset is set to the union of CPUs near the NUMA
 * node(s) in the resulting nodeset.  If all threads' default policies
 * are the same, \p policy is set to that policy.  If they are
 * different, \p policy is set to HWLOC_MEMBIND_MIXED.
 *
 * If any other flags are specified, -1 is returned and errno is set
 * to EINVAL.
 *
 * \note \p hwloc_pid_t is \p pid_t on Unix platforms,
 * and \p HANDLE on native Windows platforms.
 */
HWLOC_DECLSPEC int hwloc_get_proc_membind(hwloc_topology_t topology, hwloc_pid_t pid, hwloc_cpuset_t cpuset, hwloc_membind_policy_t * policy, int flags);

/** \brief Bind the already-allocated memory identified by (addr, len)
 * to the NUMA node(s) in physical \p nodeset.
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_area_membind_nodeset(hwloc_topology_t topology, const void *addr, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags);

/** \brief Bind the already-allocated memory identified by (addr, len)
 * to the NUMA node(s) near physical \p cpuset.
 *
 * \return -1 with errno set to ENOSYS if the action is not supported
 * \return -1 with errno set to EXDEV if the binding cannot be enforced
 */
HWLOC_DECLSPEC int hwloc_set_area_membind(hwloc_topology_t topology, const void *addr, size_t len, hwloc_const_cpuset_t cpuset, hwloc_membind_policy_t policy, int flags);

/** \brief Query the physical NUMA node(s) and binding policy of the memory
 * identified by (\p addr, \p len ).
 *
 * This function has two output parameters: \p nodeset and \p policy.
 * The values returned in these parameters depend on both the \p flags
 * passed in and the memory binding policies and nodesets of the pages
 * in the address range.
 *
 * If HWLOC_MEMBIND_STRICT is specified, the target pages are first
 * checked to see if they all have the same memory binding policy and
 * nodeset.  If they do not, -1 is returned and errno is set to EXDEV.
 * If they are identical across all pages, the nodeset and policy are
 * returned in \p nodeset and \p policy, respectively.
 *
 * If HWLOC_MEMBIND_STRICT is not specified, \p nodeset is set to the
 * union of all NUMA node(s) containing pages in the address range.
 * If all pages in the target have the same policy, it is returned in
 * \p policy.  Otherwise, \p policy is set to HWLOC_MEMBIND_MIXED.
 *
 * If any other flags are specified, -1 is returned and errno is set
 * to EINVAL.
 */
HWLOC_DECLSPEC int hwloc_get_area_membind_nodeset(hwloc_topology_t topology, const void *addr, size_t len, hwloc_nodeset_t nodeset, hwloc_membind_policy_t * policy, int flags);

/** \brief Query the CPUs near the physical NUMA node(s) and binding policy of
 * the memory identified by (\p addr, \p len ).
 *
 * This function has two output parameters: \p cpuset and \p policy.
 * The values returned in these parameters depend on both the \p flags
 * passed in and the memory binding policies and nodesets of the pages
 * in the address range.
 *
 * If HWLOC_MEMBIND_STRICT is specified, the target pages are first
 * checked to see if they all have the same memory binding policy and
 * nodeset.  If they do not, -1 is returned and errno is set to EXDEV.
 * If they are identical across all pages, the policy is returned in
 * \p policy.  \p cpuset is set to the union of CPUs near the NUMA
 * node(s) in the nodeset.
 *
 * If HWLOC_MEMBIND_STRICT is not specified, the union of all NUMA
 * node(s) containing pages in the address range is calculated.  \p
 * cpuset is then set to the CPUs near the NUMA node(s) in this union.
 * If all pages in the target have the same policy, it is returned in
 * \p policy.  Otherwise, \p policy is set to HWLOC_MEMBIND_MIXED.
 *
 * If any other flags are specified, -1 is returned and errno is set
 * to EINVAL.
 */
HWLOC_DECLSPEC int hwloc_get_area_membind(hwloc_topology_t topology, const void *addr, size_t len, hwloc_cpuset_t cpuset, hwloc_membind_policy_t * policy, int flags);

/** \brief Allocate some memory
 *
 * This is equivalent to malloc(), except that it tries to allocate
 * page-aligned memory from the OS.
 *
 * \note The allocated memory should be freed with hwloc_free().
 */
HWLOC_DECLSPEC void *hwloc_alloc(hwloc_topology_t topology, size_t len);

/** \brief Allocate some memory on the given physical nodeset \p nodeset
 *
 * \return NULL with errno set to ENOSYS if the action is not supported
 * and HWLOC_MEMBIND_STRICT is given
 * \return NULL with errno set to EXDEV if the binding cannot be enforced
 * and HWLOC_MEMBIND_STRICT is given
 *
 * \note The allocated memory should be freed with hwloc_free().
 */
HWLOC_DECLSPEC void *hwloc_alloc_membind_nodeset(hwloc_topology_t topology, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags) __hwloc_attribute_malloc;

/** \brief Allocate some memory on memory nodes near the given physical cpuset \p cpuset
 *
 * \return NULL with errno set to ENOSYS if the action is not supported
 * and HWLOC_MEMBIND_STRICT is given
 * \return NULL with errno set to EXDEV if the binding cannot be enforced
 * and HWLOC_MEMBIND_STRICT is given
 *
 * \note The allocated memory should be freed with hwloc_free().
 */
HWLOC_DECLSPEC void *hwloc_alloc_membind(hwloc_topology_t topology, size_t len, hwloc_const_cpuset_t cpuset, hwloc_membind_policy_t policy, int flags) __hwloc_attribute_malloc;

/** \brief Allocate some memory on the given nodeset \p nodeset
 *
 * This is similar to hwloc_alloc_membind except that it is allowed to change
 * the current memory binding policy, thus providing more binding support, at
 * the expense of changing the current state.
 */
static __hwloc_inline void *
hwloc_alloc_membind_policy_nodeset(hwloc_topology_t topology, size_t len, hwloc_const_nodeset_t nodeset, hwloc_membind_policy_t policy, int flags) __hwloc_attribute_malloc;

/** \brief Allocate some memory on the memory nodes near given cpuset \p cpuset
 *
 * This is similar to hwloc_alloc_membind_policy_nodeset, but for a given cpuset.
 */
static __hwloc_inline void *
hwloc_alloc_membind_policy(hwloc_topology_t topology, size_t len, hwloc_const_cpuset_t set, hwloc_membind_policy_t policy, int flags) __hwloc_attribute_malloc;

/** \brief Free memory that was previously allocated by hwloc_alloc()
 * or hwloc_alloc_membind().
 */
HWLOC_DECLSPEC int hwloc_free(hwloc_topology_t topology, void *addr, size_t len);

/** @} */



/** \defgroup hwlocality_tinker Modifying a loaded Topology
 * @{
 */

/** \brief Add a MISC object to the topology
 *
 * A new MISC object will be created and inserted into the topology at the
 * position given by bitmap \p cpuset. This offers a way to add new
 * intermediate levels to the topology hierarchy.
 *
 * \p cpuset and \p name will be copied to setup the new object attributes.
 *
 * \return the newly-created object.
 * \return \c NULL if the insertion conflicts with the existing topology tree.
 *
 * \note If \p name contains some non-printable characters, they will
 * be dropped when exporting to XML, see hwloc_topology_export_xml().
 */
HWLOC_DECLSPEC hwloc_obj_t hwloc_topology_insert_misc_object_by_cpuset(hwloc_topology_t topology, hwloc_const_cpuset_t cpuset, const char *name);

/** \brief Add a MISC object as a leaf of the topology
 *
 * A new MISC object will be created and inserted into the topology at the
 * position given by parent. It is appended to the list of existing children,
 * without ever adding any intermediate hierarchy level. This is useful for
 * annotating the topology without actually changing the hierarchy.
 *
 * \p name will be copied to the setup the new object attributes.
 * However, the new leaf object will not have any \p cpuset.
 *
 * \return the newly-created object
 *
 * \note If \p name contains some non-printable characters, they will
 * be dropped when exporting to XML, see hwloc_topology_export_xml().
 */
HWLOC_DECLSPEC hwloc_obj_t hwloc_topology_insert_misc_object_by_parent(hwloc_topology_t topology, hwloc_obj_t parent, const char *name);

/** \brief Flags to be given to hwloc_topology_restrict(). */
enum hwloc_restrict_flags_e {
  /** \brief Adapt distance matrices according to objects being removed during restriction.
   * If this flag is not set, distance matrices are removed.
   * \hideinitializer
   */
  HWLOC_RESTRICT_FLAG_ADAPT_DISTANCES = (1<<0),

  /** \brief Move Misc objects to ancestors if their parents are removed during restriction.
   * If this flag is not set, Misc objects are removed when their parents are removed.
   * \hideinitializer
   */
  HWLOC_RESTRICT_FLAG_ADAPT_MISC = (1<<1),

  /** \brief Move I/O objects to ancestors if their parents are removed during restriction.
   * If this flag is not set, I/O devices and bridges are removed when their parents are removed.
   * \hideinitializer
   */
  HWLOC_RESTRICT_FLAG_ADAPT_IO = (1<<2)
};

/** \brief Restrict the topology to the given CPU set.
 *
 * Topology \p topology is modified so as to remove all objects that
 * are not included (or partially included) in the CPU set \p cpuset.
 * All objects CPU and node sets are restricted accordingly.
 *
 * \p flags is a OR'ed set of ::hwloc_restrict_flags_e.
 *
 * \note This call may not be reverted by restricting back to a larger
 * cpuset. Once dropped during restriction, objects may not be brought
 * back, except by loading another topology with hwloc_topology_load().
 *
 * \return 0 on success.
 *
 * \return -1 with errno set to EINVAL if the input cpuset is invalid.
 * The topology is not modified in this case.
 *
 * \return -1 with errno set to ENOMEM on failure to allocate internal data.
 * The topology is reinitialized in this case. It should be either
 * destroyed with hwloc_topology_destroy() or configured and loaded again.
 */
HWLOC_DECLSPEC int hwloc_topology_restrict(hwloc_topology_t __hwloc_restrict topology, hwloc_const_cpuset_t cpuset, unsigned long flags);

/** \brief Duplicate a topology.
 *
 * The entire topology structure as well as its objects
 * are duplicated into a new one.
 *
 * This is useful for keeping a backup while modifying a topology.
 */
HWLOC_DECLSPEC int hwloc_topology_dup(hwloc_topology_t *newtopology, hwloc_topology_t oldtopology);

/** @} */



/** \defgroup hwlocality_custom Building Custom Topologies
 *
 * A custom topology may be initialized by calling hwloc_topology_set_custom()
 * after hwloc_topology_init(). It may then be modified by inserting objects
 * or entire topologies. Once done assembling, hwloc_topology_load() should
 * be invoked as usual to finalize the topology.
 * @{
 */

/** \brief Insert an existing topology inside a custom topology
 *
 * Duplicate the existing topology \p oldtopology inside a new
 * custom topology \p newtopology as a leaf of object \p newparent.
 *
 * If \p oldroot is not \c NULL, duplicate \p oldroot and all its
 * children instead of the entire \p oldtopology. Passing the root
 * object of \p oldtopology in \p oldroot is equivalent to passing
 * \c NULL.
 *
 * The custom topology \p newtopology must have been prepared with
 * hwloc_topology_set_custom() and not loaded with hwloc_topology_load()
 * yet.
 *
 * \p newparent may be either the root of \p newtopology or an object
 * that was added through hwloc_custom_insert_group_object_by_parent().
 *
 * \note The cpuset and nodeset of the \p newparent object are not
 * modified based on the contents of \p oldtopology.
 */
HWLOC_DECLSPEC int hwloc_custom_insert_topology(hwloc_topology_t newtopology, hwloc_obj_t newparent, hwloc_topology_t oldtopology, hwloc_obj_t oldroot);

/** \brief Insert a new group object inside a custom topology
 *
 * An object with type ::HWLOC_OBJ_GROUP is inserted as a new child
 * of object \p parent.
 *
 * \p groupdepth is the depth attribute to be given to the new object.
 * It may for instance be 0 for top-level groups, 1 for their children,
 * and so on.
 *
 * The custom topology \p newtopology must have been prepared with
 * hwloc_topology_set_custom() and not loaded with hwloc_topology_load()
 * yet.
 *
 * \p parent may be either the root of \p topology or an object that
 * was added earlier through hwloc_custom_insert_group_object_by_parent().
 *
 * \note The cpuset and nodeset of the new group object are NULL because
 * these sets are meaningless when assembling multiple topologies.
 *
 * \note The cpuset and nodeset of the \p parent object are not modified.
 */
HWLOC_DECLSPEC hwloc_obj_t hwloc_custom_insert_group_object_by_parent(hwloc_topology_t topology, hwloc_obj_t parent, int groupdepth);

/** @} */



/** \defgroup hwlocality_xmlexport Exporting Topologies to XML
 * @{
 */

/** \brief Export the topology into an XML file.
 *
 * This file may be loaded later through hwloc_topology_set_xml().
 *
 * \return -1 if a failure occured.
 *
 * \note See also hwloc_topology_set_userdata_export_callback()
 * for exporting application-specific object userdata.
 *
 * \note Only printable characters may be exported to XML string attributes.
 * Any other character, especially any non-ASCII character, will be silently
 * dropped.
 *
 * \note If \p name is "-", the XML output is sent to the standard output.
 */
HWLOC_DECLSPEC int hwloc_topology_export_xml(hwloc_topology_t topology, const char *xmlpath);

/** \brief Export the topology into a newly-allocated XML memory buffer.
 *
 * \p xmlbuffer is allocated by the callee and should be freed with
 * hwloc_free_xmlbuffer() later in the caller.
 *
 * This memory buffer may be loaded later through hwloc_topology_set_xmlbuffer().
 *
 * \return -1 if a failure occured.
 *
 * \note See also hwloc_topology_set_userdata_export_callback()
 * for exporting application-specific object userdata.
 *
 * \note Only printable characters may be exported to XML string attributes.
 * Any other character, especially any non-ASCII character, will be silently
 * dropped.
 */
HWLOC_DECLSPEC int hwloc_topology_export_xmlbuffer(hwloc_topology_t topology, char **xmlbuffer, int *buflen);

/** \brief Free a buffer allocated by hwloc_topology_export_xmlbuffer() */
HWLOC_DECLSPEC void hwloc_free_xmlbuffer(hwloc_topology_t topology, char *xmlbuffer);

/** \brief Set the application-specific callback for exporting object userdata
 *
 * The object userdata pointer is not exported to XML by default because hwloc
 * does not know what it contains.
 *
 * This function lets applications set \p export_cb to a callback function
 * that converts this opaque userdata into an exportable string.
 *
 * \p export_cb is invoked during XML export for each object whose
 * \p userdata pointer is not \c NULL.
 * The callback should use hwloc_export_obj_userdata() or
 * hwloc_export_obj_userdata_base64() to actually export
 * something to XML (possibly multiple times per object).
 *
 * \p export_cb may be set to \c NULL if userdata should not be exported to XML.
 */
HWLOC_DECLSPEC void hwloc_topology_set_userdata_export_callback(hwloc_topology_t topology,
								void (*export_cb)(void *reserved, hwloc_topology_t topology, hwloc_obj_t obj));

/** \brief Export some object userdata to XML
 *
 * This function may only be called from within the export() callback passed
 * to hwloc_topology_set_userdata_export_callback().
 * It may be invoked one of multiple times to export some userdata to XML.
 * The \p buffer content of length \p length is stored with optional name
 * \p name.
 *
 * When importing this XML file, the import() callback (if set) will be
 * called exactly as many times as hwloc_export_obj_userdata() was called
 * during export(). It will receive the corresponding \p name, \p buffer
 * and \p length arguments.
 *
 * \p reserved, \p topology and \p obj must be the first three parameters
 * that were given to the export callback.
 *
 * Only printable characters may be exported to XML string attributes.
 * If a non-printable character is passed in \p name or \p buffer,
 * the function returns -1 with errno set to EINVAL.
 *
 * If exporting binary data, the application should first encode into
 * printable characters only (or use hwloc_export_obj_userdata_base64()).
 * It should also take care of portability issues if the export may
 * be reimported on a different architecture.
 */
HWLOC_DECLSPEC int hwloc_export_obj_userdata(void *reserved, hwloc_topology_t topology, hwloc_obj_t obj, const char *name, const void *buffer, size_t length);

/** \brief Encode and export some object userdata to XML
 *
 * This function is similar to hwloc_export_obj_userdata() but it encodes
 * the input buffer into printable characters before exporting.
 * On import, decoding is automatically performed before the data is given
 * to the import() callback if any.
 *
 * This function may only be called from within the export() callback passed
 * to hwloc_topology_set_userdata_export_callback().
 *
 * The function does not take care of portability issues if the export
 * may be reimported on a different architecture.
 */
HWLOC_DECLSPEC int hwloc_export_obj_userdata_base64(void *reserved, hwloc_topology_t topology, hwloc_obj_t obj, const char *name, const void *buffer, size_t length);

/** \brief Set the application-specific callback for importing userdata
 *
 * On XML import, userdata is ignored by default because hwloc does not know
 * how to store it in memory.
 *
 * This function lets applications set \p import_cb to a callback function
 * that will get the XML-stored userdata and store it in the object as expected
 * by the application.
 *
 * \p import_cb is called during hwloc_topology_load() as many times as
 * hwloc_export_obj_userdata() was called during export. The topology
 * is not entirely setup yet. Object attributes are ready to consult,
 * but links between objects are not.
 *
 * \p import_cb may be \c NULL if userdata should be ignored during import.
 *
 * \note \p buffer contains \p length characters followed by a null byte ('\0').
 *
 * \note This function should be called before hwloc_topology_load().
 */
HWLOC_DECLSPEC void hwloc_topology_set_userdata_import_callback(hwloc_topology_t topology,
								void (*import_cb)(hwloc_topology_t topology, hwloc_obj_t obj, const char *name, const void *buffer, size_t length));

/** @} */



#ifdef __cplusplus
} /* extern "C" */
#endif


/* high-level helpers */
#include <hwloc/helper.h>

/* inline code of some functions above */
#include <hwloc/inlines.h>

/* topology diffs */
#include <hwloc/diff.h>

/* deprecated headers */
#include <hwloc/deprecated.h>

#endif /* HWLOC_H */
