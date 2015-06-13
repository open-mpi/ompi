/*
 * Copyright © 2009-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright © 2010-2014 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_RENAME_H
#define HWLOC_RENAME_H

#include <hwloc/autogen/config.h>


#ifdef __cplusplus
extern "C" {
#endif


/* Only enact these defines if we're actually renaming the symbols
   (i.e., avoid trying to have no-op defines if we're *not*
   renaming). */

#if HWLOC_SYM_TRANSFORM

/* Use a preprocessor two-step in order to get the prefixing right.
   Make 2 macros: HWLOC_NAME and HWLOC_NAME_CAPS for renaming
   things. */

#define HWLOC_MUNGE_NAME(a, b) HWLOC_MUNGE_NAME2(a, b)
#define HWLOC_MUNGE_NAME2(a, b) a ## b
#define HWLOC_NAME(name) HWLOC_MUNGE_NAME(HWLOC_SYM_PREFIX, hwloc_ ## name)
#define HWLOC_NAME_CAPS(name) HWLOC_MUNGE_NAME(HWLOC_SYM_PREFIX_CAPS, hwloc_ ## name)

/* Now define all the "real" names to be the prefixed names.  This
   allows us to use the real names throughout the code base (i.e.,
   "hwloc_<foo>"); the preprocessor will adjust to have the prefixed
   name under the covers. */

/* Names from hwloc.h */

#define hwloc_get_api_version HWLOC_NAME(get_api_version)

#define hwloc_topology HWLOC_NAME(topology)
#define hwloc_topology_t HWLOC_NAME(topology_t)

#define hwloc_cpuset_t HWLOC_NAME(cpuset_t)
#define hwloc_const_cpuset_t HWLOC_NAME(const_cpuset_t)
#define hwloc_nodeset_t HWLOC_NAME(nodeset_t)
#define hwloc_const_nodeset_t HWLOC_NAME(const_nodeset_t)

#define HWLOC_OBJ_SYSTEM HWLOC_NAME_CAPS(OBJ_SYSTEM)
#define HWLOC_OBJ_MACHINE HWLOC_NAME_CAPS(OBJ_MACHINE)
#define HWLOC_OBJ_NUMANODE HWLOC_NAME_CAPS(OBJ_NUMANODE)
#define HWLOC_OBJ_PACKAGE HWLOC_NAME_CAPS(OBJ_PACKAGE)
#define HWLOC_OBJ_CACHE HWLOC_NAME_CAPS(OBJ_CACHE)
#define HWLOC_OBJ_CORE HWLOC_NAME_CAPS(OBJ_CORE)
#define HWLOC_OBJ_PU HWLOC_NAME_CAPS(OBJ_PU)
#define HWLOC_OBJ_MISC HWLOC_NAME_CAPS(OBJ_MISC)
#define HWLOC_OBJ_GROUP HWLOC_NAME_CAPS(OBJ_GROUP)
#define HWLOC_OBJ_BRIDGE HWLOC_NAME_CAPS(OBJ_BRIDGE)
#define HWLOC_OBJ_PCI_DEVICE HWLOC_NAME_CAPS(OBJ_PCI_DEVICE)
#define HWLOC_OBJ_OS_DEVICE HWLOC_NAME_CAPS(OBJ_OS_DEVICE)
#define HWLOC_OBJ_TYPE_MAX HWLOC_NAME_CAPS(OBJ_TYPE_MAX)
#define hwloc_obj_type_t HWLOC_NAME(obj_type_t)

#define hwloc_obj_cache_type_e HWLOC_NAME(obj_cache_type_e)
#define hwloc_obj_cache_type_t HWLOC_NAME(obj_cache_type_t)
#define HWLOC_OBJ_CACHE_UNIFIED HWLOC_NAME_CAPS(OBJ_CACHE_UNIFIED)
#define HWLOC_OBJ_CACHE_DATA HWLOC_NAME_CAPS(OBJ_CACHE_DATA)
#define HWLOC_OBJ_CACHE_INSTRUCTION HWLOC_NAME_CAPS(OBJ_CACHE_INSTRUCTION)

#define hwloc_obj_bridge_type_e HWLOC_NAME(obj_bridge_type_e)
#define hwloc_obj_bridge_type_t HWLOC_NAME(obj_bridge_type_t)
#define HWLOC_OBJ_BRIDGE_HOST HWLOC_NAME_CAPS(OBJ_BRIDGE_HOST)
#define HWLOC_OBJ_BRIDGE_PCI HWLOC_NAME_CAPS(OBJ_BRIDGE_PCI)

#define hwloc_obj_osdev_type_e HWLOC_NAME(obj_osdev_type_e)
#define hwloc_obj_osdev_type_t HWLOC_NAME(obj_osdev_type_t)
#define HWLOC_OBJ_OSDEV_BLOCK HWLOC_NAME_CAPS(OBJ_OSDEV_BLOCK)
#define HWLOC_OBJ_OSDEV_GPU HWLOC_NAME_CAPS(OBJ_OSDEV_GPU)
#define HWLOC_OBJ_OSDEV_NETWORK HWLOC_NAME_CAPS(OBJ_OSDEV_NETWORK)
#define HWLOC_OBJ_OSDEV_OPENFABRICS HWLOC_NAME_CAPS(OBJ_OSDEV_OPENFABRICS)
#define HWLOC_OBJ_OSDEV_DMA HWLOC_NAME_CAPS(OBJ_OSDEV_DMA)
#define HWLOC_OBJ_OSDEV_COPROC HWLOC_NAME_CAPS(OBJ_OSDEV_COPROC)

#define hwloc_compare_types HWLOC_NAME(compare_types)

#define hwloc_compare_types_e HWLOC_NAME(compare_types_e)
#define HWLOC_TYPE_UNORDERED HWLOC_NAME_CAPS(TYPE_UNORDERED)

#define hwloc_obj_memory_s HWLOC_NAME(obj_memory_s)
#define hwloc_obj_memory_page_type_s HWLOC_NAME(obj_memory_page_type_s)

#define hwloc_obj HWLOC_NAME(obj)
#define hwloc_obj_t HWLOC_NAME(obj_t)

#define hwloc_distances_s HWLOC_NAME(distances_s)
#define hwloc_obj_info_s HWLOC_NAME(obj_info_s)

#define hwloc_obj_attr_u HWLOC_NAME(obj_attr_u)
#define hwloc_cache_attr_s HWLOC_NAME(cache_attr_s)
#define hwloc_group_attr_s HWLOC_NAME(group_attr_s)
#define hwloc_pcidev_attr_s HWLOC_NAME(pcidev_attr_s)
#define hwloc_bridge_attr_s HWLOC_NAME(bridge_attr_s)
#define hwloc_osdev_attr_s HWLOC_NAME(osdev_attr_s)

#define hwloc_topology_init HWLOC_NAME(topology_init)
#define hwloc_topology_load HWLOC_NAME(topology_load)
#define hwloc_topology_destroy HWLOC_NAME(topology_destroy)
#define hwloc_topology_dup HWLOC_NAME(topology_dup)
#define hwloc_topology_check HWLOC_NAME(topology_check)
#define hwloc_topology_ignore_type HWLOC_NAME(topology_ignore_type)
#define hwloc_topology_ignore_type_keep_structure HWLOC_NAME(topology_ignore_type_keep_structure)
#define hwloc_topology_ignore_all_keep_structure HWLOC_NAME(topology_ignore_all_keep_structure)

#define hwloc_topology_flags_e HWLOC_NAME(topology_flags_e)

#define HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM HWLOC_NAME_CAPS(TOPOLOGY_FLAG_WHOLE_SYSTEM)
#define HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM HWLOC_NAME_CAPS(TOPOLOGY_FLAG_IS_THISSYSTEM)
#define HWLOC_TOPOLOGY_FLAG_IO_DEVICES HWLOC_NAME_CAPS(TOPOLOGY_FLAG_IO_DEVICES)
#define HWLOC_TOPOLOGY_FLAG_IO_BRIDGES HWLOC_NAME_CAPS(TOPOLOGY_FLAG_IO_BRIDGES)
#define HWLOC_TOPOLOGY_FLAG_WHOLE_IO HWLOC_NAME_CAPS(TOPOLOGY_FLAG_WHOLE_IO)
#define HWLOC_TOPOLOGY_FLAG_ICACHES HWLOC_NAME_CAPS(TOPOLOGY_FLAG_ICACHES)

#define hwloc_topology_set_flags HWLOC_NAME(topology_set_flags)
#define hwloc_topology_set_fsroot HWLOC_NAME(topology_set_fsroot)
#define hwloc_topology_set_pid HWLOC_NAME(topology_set_pid)
#define hwloc_topology_set_synthetic HWLOC_NAME(topology_set_synthetic)
#define hwloc_topology_set_xml HWLOC_NAME(topology_set_xml)
#define hwloc_topology_set_xmlbuffer HWLOC_NAME(topology_set_xmlbuffer)
#define hwloc_topology_set_custom HWLOC_NAME(topology_set_custom)
#define hwloc_topology_set_distance_matrix HWLOC_NAME(topology_set_distance_matrix)

#define hwloc_topology_discovery_support HWLOC_NAME(topology_discovery_support)
#define hwloc_topology_cpubind_support HWLOC_NAME(topology_cpubind_support)
#define hwloc_topology_membind_support HWLOC_NAME(topology_membind_support)
#define hwloc_topology_support HWLOC_NAME(topology_support)
#define hwloc_topology_get_support HWLOC_NAME(topology_get_support)
#define hwloc_topology_set_userdata HWLOC_NAME(topology_set_userdata)
#define hwloc_topology_get_userdata HWLOC_NAME(topology_get_userdata)

#define hwloc_topology_export_xml HWLOC_NAME(topology_export_xml)
#define hwloc_topology_export_xmlbuffer HWLOC_NAME(topology_export_xmlbuffer)
#define hwloc_free_xmlbuffer HWLOC_NAME(free_xmlbuffer)
#define hwloc_topology_set_userdata_export_callback HWLOC_NAME(topology_set_userdata_export_callback)
#define hwloc_export_obj_userdata HWLOC_NAME(export_obj_userdata)
#define hwloc_export_obj_userdata_base64 HWLOC_NAME(export_obj_userdata_base64)
#define hwloc_topology_set_userdata_import_callback HWLOC_NAME(topology_set_userdata_import_callback)

#define hwloc_topology_export_synthetic_flags_e HWLOC_NAME(topology_export_synthetic_flags_e)
#define HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_EXTENDED_TYPES HWLOC_NAME_CAPS(TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_EXTENDED_TYPES)
#define HWLOC_TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_ATTRS HWLOC_NAME_CAPS(TOPOLOGY_EXPORT_SYNTHETIC_FLAG_NO_ATTRS)
#define hwloc_topology_export_synthetic HWLOC_NAME(topology_export_synthetic)

#define hwloc_topology_insert_misc_object_by_cpuset HWLOC_NAME(topology_insert_misc_object_by_cpuset)
#define hwloc_topology_insert_misc_object_by_parent HWLOC_NAME(topology_insert_misc_object_by_parent)

#define hwloc_custom_insert_topology HWLOC_NAME(custom_insert_topology)
#define hwloc_custom_insert_group_object_by_parent HWLOC_NAME(custom_insert_group_object_by_parent)

#define hwloc_restrict_flags_e HWLOC_NAME(restrict_flags_e)
#define HWLOC_RESTRICT_FLAG_ADAPT_DISTANCES HWLOC_NAME_CAPS(RESTRICT_FLAG_ADAPT_DISTANCES)
#define HWLOC_RESTRICT_FLAG_ADAPT_MISC HWLOC_NAME_CAPS(RESTRICT_FLAG_ADAPT_MISC)
#define HWLOC_RESTRICT_FLAG_ADAPT_IO HWLOC_NAME_CAPS(RESTRICT_FLAG_ADAPT_IO)
#define hwloc_topology_restrict HWLOC_NAME(topology_restrict)

#define hwloc_topology_get_depth HWLOC_NAME(topology_get_depth)
#define hwloc_get_type_depth HWLOC_NAME(get_type_depth)

#define hwloc_get_type_depth_e HWLOC_NAME(get_type_depth_e)
#define HWLOC_TYPE_DEPTH_UNKNOWN HWLOC_NAME_CAPS(TYPE_DEPTH_UNKNOWN)
#define HWLOC_TYPE_DEPTH_MULTIPLE HWLOC_NAME_CAPS(TYPE_DEPTH_MULTIPLE)
#define HWLOC_TYPE_DEPTH_BRIDGE HWLOC_NAME_CAPS(TYPE_DEPTH_BRIDGE)
#define HWLOC_TYPE_DEPTH_PCI_DEVICE HWLOC_NAME_CAPS(TYPE_DEPTH_PCI_DEVICE)
#define HWLOC_TYPE_DEPTH_OS_DEVICE HWLOC_NAME_CAPS(TYPE_DEPTH_OS_DEVICE)

#define hwloc_get_depth_type HWLOC_NAME(get_depth_type)
#define hwloc_get_nbobjs_by_depth HWLOC_NAME(get_nbobjs_by_depth)
#define hwloc_get_nbobjs_by_type HWLOC_NAME(get_nbobjs_by_type)

#define hwloc_topology_is_thissystem HWLOC_NAME(topology_is_thissystem)
#define hwloc_topology_get_flags HWLOC_NAME(topology_get_flags)

#define hwloc_get_obj_by_depth HWLOC_NAME(get_obj_by_depth )
#define hwloc_get_obj_by_type HWLOC_NAME(get_obj_by_type )

#define hwloc_obj_type_string HWLOC_NAME(obj_type_string )
#define hwloc_obj_type_sscanf HWLOC_NAME(obj_type_sscanf)
#define hwloc_obj_type_snprintf HWLOC_NAME(obj_type_snprintf )
#define hwloc_obj_attr_snprintf HWLOC_NAME(obj_attr_snprintf )
#define hwloc_obj_cpuset_snprintf HWLOC_NAME(obj_cpuset_snprintf)
#define hwloc_obj_get_info_by_name HWLOC_NAME(obj_get_info_by_name)
#define hwloc_obj_add_info HWLOC_NAME(obj_add_info)

#define HWLOC_CPUBIND_PROCESS HWLOC_NAME_CAPS(CPUBIND_PROCESS)
#define HWLOC_CPUBIND_THREAD HWLOC_NAME_CAPS(CPUBIND_THREAD)
#define HWLOC_CPUBIND_STRICT HWLOC_NAME_CAPS(CPUBIND_STRICT)
#define HWLOC_CPUBIND_NOMEMBIND HWLOC_NAME_CAPS(CPUBIND_NOMEMBIND)

#define hwloc_cpubind_flags_t HWLOC_NAME(cpubind_flags_t)

#define hwloc_set_cpubind HWLOC_NAME(set_cpubind)
#define hwloc_get_cpubind HWLOC_NAME(get_cpubind)
#define hwloc_set_proc_cpubind HWLOC_NAME(set_proc_cpubind)
#define hwloc_get_proc_cpubind HWLOC_NAME(get_proc_cpubind)
#define hwloc_set_thread_cpubind HWLOC_NAME(set_thread_cpubind)
#define hwloc_get_thread_cpubind HWLOC_NAME(get_thread_cpubind)

#define hwloc_get_last_cpu_location HWLOC_NAME(get_last_cpu_location)
#define hwloc_get_proc_last_cpu_location HWLOC_NAME(get_proc_last_cpu_location)

#define HWLOC_MEMBIND_DEFAULT HWLOC_NAME_CAPS(MEMBIND_DEFAULT)
#define HWLOC_MEMBIND_FIRSTTOUCH HWLOC_NAME_CAPS(MEMBIND_FIRSTTOUCH)
#define HWLOC_MEMBIND_BIND HWLOC_NAME_CAPS(MEMBIND_BIND)
#define HWLOC_MEMBIND_INTERLEAVE HWLOC_NAME_CAPS(MEMBIND_INTERLEAVE)
#define HWLOC_MEMBIND_REPLICATE HWLOC_NAME_CAPS(MEMBIND_REPLICATE)
#define HWLOC_MEMBIND_NEXTTOUCH HWLOC_NAME_CAPS(MEMBIND_NEXTTOUCH)
#define HWLOC_MEMBIND_MIXED HWLOC_NAME_CAPS(MEMBIND_MIXED)

#define hwloc_membind_policy_t HWLOC_NAME(membind_policy_t)

#define HWLOC_MEMBIND_PROCESS HWLOC_NAME_CAPS(MEMBIND_PROCESS)
#define HWLOC_MEMBIND_THREAD HWLOC_NAME_CAPS(MEMBIND_THREAD)
#define HWLOC_MEMBIND_STRICT HWLOC_NAME_CAPS(MEMBIND_STRICT)
#define HWLOC_MEMBIND_MIGRATE HWLOC_NAME_CAPS(MEMBIND_MIGRATE)
#define HWLOC_MEMBIND_NOCPUBIND HWLOC_NAME_CAPS(MEMBIND_NOCPUBIND)

#define hwloc_membind_flags_t HWLOC_NAME(membind_flags_t)

#define hwloc_set_membind_nodeset HWLOC_NAME(set_membind_nodeset)
#define hwloc_set_membind HWLOC_NAME(set_membind)
#define hwloc_get_membind_nodeset HWLOC_NAME(get_membind_nodeset)
#define hwloc_get_membind HWLOC_NAME(get_membind)
#define hwloc_set_proc_membind_nodeset HWLOC_NAME(set_proc_membind_nodeset)
#define hwloc_set_proc_membind HWLOC_NAME(set_proc_membind)
#define hwloc_get_proc_membind_nodeset HWLOC_NAME(get_proc_membind_nodeset)
#define hwloc_get_proc_membind HWLOC_NAME(get_proc_membind)
#define hwloc_set_area_membind_nodeset HWLOC_NAME(set_area_membind_nodeset)
#define hwloc_set_area_membind HWLOC_NAME(set_area_membind)
#define hwloc_get_area_membind_nodeset HWLOC_NAME(get_area_membind_nodeset)
#define hwloc_get_area_membind HWLOC_NAME(get_area_membind)
#define hwloc_alloc_membind_nodeset HWLOC_NAME(alloc_membind_nodeset)
#define hwloc_alloc_membind HWLOC_NAME(alloc_membind)
#define hwloc_alloc HWLOC_NAME(alloc)
#define hwloc_free HWLOC_NAME(free)

#define hwloc_get_non_io_ancestor_obj HWLOC_NAME(get_non_io_ancestor_obj)
#define hwloc_get_next_pcidev HWLOC_NAME(get_next_pcidev)
#define hwloc_get_pcidev_by_busid HWLOC_NAME(get_pcidev_by_busid)
#define hwloc_get_pcidev_by_busidstring HWLOC_NAME(get_pcidev_by_busidstring)
#define hwloc_get_next_osdev HWLOC_NAME(get_next_osdev)
#define hwloc_get_next_bridge HWLOC_NAME(get_next_bridge)
#define hwloc_bridge_covers_pcibus HWLOC_NAME(bridge_covers_pcibus)
#define hwloc_get_hostbridge_by_pcibus HWLOC_NAME(get_hostbridge_by_pcibus)

/* hwloc/bitmap.h */

#define hwloc_bitmap_s HWLOC_NAME(bitmap_s)
#define hwloc_bitmap_t HWLOC_NAME(bitmap_t)
#define hwloc_const_bitmap_t HWLOC_NAME(const_bitmap_t)

#define hwloc_bitmap_alloc HWLOC_NAME(bitmap_alloc)
#define hwloc_bitmap_alloc_full HWLOC_NAME(bitmap_alloc_full)
#define hwloc_bitmap_free HWLOC_NAME(bitmap_free)
#define hwloc_bitmap_dup HWLOC_NAME(bitmap_dup)
#define hwloc_bitmap_copy HWLOC_NAME(bitmap_copy)
#define hwloc_bitmap_snprintf HWLOC_NAME(bitmap_snprintf)
#define hwloc_bitmap_asprintf HWLOC_NAME(bitmap_asprintf)
#define hwloc_bitmap_sscanf HWLOC_NAME(bitmap_sscanf)
#define hwloc_bitmap_list_snprintf HWLOC_NAME(bitmap_list_snprintf)
#define hwloc_bitmap_list_asprintf HWLOC_NAME(bitmap_list_asprintf)
#define hwloc_bitmap_list_sscanf HWLOC_NAME(bitmap_list_sscanf)
#define hwloc_bitmap_taskset_snprintf HWLOC_NAME(bitmap_taskset_snprintf)
#define hwloc_bitmap_taskset_asprintf HWLOC_NAME(bitmap_taskset_asprintf)
#define hwloc_bitmap_taskset_sscanf HWLOC_NAME(bitmap_taskset_sscanf)
#define hwloc_bitmap_zero HWLOC_NAME(bitmap_zero)
#define hwloc_bitmap_fill HWLOC_NAME(bitmap_fill)
#define hwloc_bitmap_from_ulong HWLOC_NAME(bitmap_from_ulong)

#define hwloc_bitmap_from_ith_ulong HWLOC_NAME(bitmap_from_ith_ulong)
#define hwloc_bitmap_to_ulong HWLOC_NAME(bitmap_to_ulong)
#define hwloc_bitmap_to_ith_ulong HWLOC_NAME(bitmap_to_ith_ulong)
#define hwloc_bitmap_only HWLOC_NAME(bitmap_only)
#define hwloc_bitmap_allbut HWLOC_NAME(bitmap_allbut)
#define hwloc_bitmap_set HWLOC_NAME(bitmap_set)
#define hwloc_bitmap_set_range HWLOC_NAME(bitmap_set_range)
#define hwloc_bitmap_set_ith_ulong HWLOC_NAME(bitmap_set_ith_ulong)
#define hwloc_bitmap_clr HWLOC_NAME(bitmap_clr)
#define hwloc_bitmap_clr_range HWLOC_NAME(bitmap_clr_range)
#define hwloc_bitmap_isset HWLOC_NAME(bitmap_isset)
#define hwloc_bitmap_iszero HWLOC_NAME(bitmap_iszero)
#define hwloc_bitmap_isfull HWLOC_NAME(bitmap_isfull)
#define hwloc_bitmap_isequal HWLOC_NAME(bitmap_isequal)
#define hwloc_bitmap_intersects HWLOC_NAME(bitmap_intersects)
#define hwloc_bitmap_isincluded HWLOC_NAME(bitmap_isincluded)
#define hwloc_bitmap_or HWLOC_NAME(bitmap_or)
#define hwloc_bitmap_and HWLOC_NAME(bitmap_and)
#define hwloc_bitmap_andnot HWLOC_NAME(bitmap_andnot)
#define hwloc_bitmap_xor HWLOC_NAME(bitmap_xor)
#define hwloc_bitmap_not HWLOC_NAME(bitmap_not)
#define hwloc_bitmap_first HWLOC_NAME(bitmap_first)
#define hwloc_bitmap_last HWLOC_NAME(bitmap_last)
#define hwloc_bitmap_next HWLOC_NAME(bitmap_next)
#define hwloc_bitmap_singlify HWLOC_NAME(bitmap_singlify)
#define hwloc_bitmap_compare_first HWLOC_NAME(bitmap_compare_first)
#define hwloc_bitmap_compare HWLOC_NAME(bitmap_compare)
#define hwloc_bitmap_weight HWLOC_NAME(bitmap_weight)

/* hwloc/helper.h */

#define hwloc_get_type_or_below_depth HWLOC_NAME(get_type_or_below_depth)
#define hwloc_get_type_or_above_depth HWLOC_NAME(get_type_or_above_depth)
#define hwloc_get_root_obj HWLOC_NAME(get_root_obj)
#define hwloc_get_ancestor_obj_by_depth HWLOC_NAME(get_ancestor_obj_by_depth)
#define hwloc_get_ancestor_obj_by_type HWLOC_NAME(get_ancestor_obj_by_type)
#define hwloc_get_next_obj_by_depth HWLOC_NAME(get_next_obj_by_depth)
#define hwloc_get_next_obj_by_type HWLOC_NAME(get_next_obj_by_type)
#define hwloc_get_pu_obj_by_os_index HWLOC_NAME(get_pu_obj_by_os_index)
#define hwloc_get_numanode_obj_by_os_index HWLOC_NAME(get_numanode_obj_by_os_index)
#define hwloc_get_next_child HWLOC_NAME(get_next_child)
#define hwloc_get_common_ancestor_obj HWLOC_NAME(get_common_ancestor_obj)
#define hwloc_obj_is_in_subtree HWLOC_NAME(obj_is_in_subtree)
#define hwloc_get_first_largest_obj_inside_cpuset HWLOC_NAME(get_first_largest_obj_inside_cpuset)
#define hwloc_get_largest_objs_inside_cpuset HWLOC_NAME(get_largest_objs_inside_cpuset)
#define hwloc_get_next_obj_inside_cpuset_by_depth HWLOC_NAME(get_next_obj_inside_cpuset_by_depth)
#define hwloc_get_next_obj_inside_cpuset_by_type HWLOC_NAME(get_next_obj_inside_cpuset_by_type)
#define hwloc_get_obj_inside_cpuset_by_depth HWLOC_NAME(get_obj_inside_cpuset_by_depth)
#define hwloc_get_obj_inside_cpuset_by_type HWLOC_NAME(get_obj_inside_cpuset_by_type)
#define hwloc_get_nbobjs_inside_cpuset_by_depth HWLOC_NAME(get_nbobjs_inside_cpuset_by_depth)
#define hwloc_get_nbobjs_inside_cpuset_by_type HWLOC_NAME(get_nbobjs_inside_cpuset_by_type)
#define hwloc_get_obj_index_inside_cpuset HWLOC_NAME(get_obj_index_inside_cpuset)
#define hwloc_get_child_covering_cpuset HWLOC_NAME(get_child_covering_cpuset)
#define hwloc_get_obj_covering_cpuset HWLOC_NAME(get_obj_covering_cpuset)
#define hwloc_get_next_obj_covering_cpuset_by_depth HWLOC_NAME(get_next_obj_covering_cpuset_by_depth)
#define hwloc_get_next_obj_covering_cpuset_by_type HWLOC_NAME(get_next_obj_covering_cpuset_by_type)
#define hwloc_get_cache_type_depth HWLOC_NAME(get_cache_type_depth)
#define hwloc_get_cache_covering_cpuset HWLOC_NAME(get_cache_covering_cpuset)
#define hwloc_get_shared_cache_covering_obj HWLOC_NAME(get_shared_cache_covering_obj)
#define hwloc_get_closest_objs HWLOC_NAME(get_closest_objs)
#define hwloc_get_obj_below_by_type HWLOC_NAME(get_obj_below_by_type)
#define hwloc_get_obj_below_array_by_type HWLOC_NAME(get_obj_below_array_by_type)
#define hwloc_distrib_flags_e HWLOC_NAME(distrib_flags_e)
#define HWLOC_DISTRIB_FLAG_REVERSE HWLOC_NAME_CAPS(DISTRIB_FLAG_REVERSE)
#define hwloc_distrib HWLOC_NAME(distrib)
#define hwloc_alloc_membind_policy HWLOC_NAME(alloc_membind_policy)
#define hwloc_alloc_membind_policy_nodeset HWLOC_NAME(alloc_membind_policy_nodeset)
#define hwloc_topology_get_complete_cpuset HWLOC_NAME(topology_get_complete_cpuset)
#define hwloc_topology_get_topology_cpuset HWLOC_NAME(topology_get_topology_cpuset)
#define hwloc_topology_get_online_cpuset HWLOC_NAME(topology_get_online_cpuset)
#define hwloc_topology_get_allowed_cpuset HWLOC_NAME(topology_get_allowed_cpuset)
#define hwloc_topology_get_complete_nodeset HWLOC_NAME(topology_get_complete_nodeset)
#define hwloc_topology_get_topology_nodeset HWLOC_NAME(topology_get_topology_nodeset)
#define hwloc_topology_get_allowed_nodeset HWLOC_NAME(topology_get_allowed_nodeset)
#define hwloc_cpuset_to_nodeset HWLOC_NAME(cpuset_to_nodeset)
#define hwloc_cpuset_to_nodeset_strict HWLOC_NAME(cpuset_to_nodeset_strict)
#define hwloc_cpuset_from_nodeset HWLOC_NAME(cpuset_from_nodeset)
#define hwloc_cpuset_from_nodeset_strict HWLOC_NAME(cpuset_from_nodeset_strict)
#define hwloc_get_whole_distance_matrix_by_depth HWLOC_NAME(get_whole_distance_matrix_by_depth)
#define hwloc_get_whole_distance_matrix_by_type HWLOC_NAME(get_whole_distance_matrix_by_type)
#define hwloc_get_distance_matrix_covering_obj_by_depth HWLOC_NAME(get_distance_matrix_covering_obj_by_depth)
#define hwloc_get_latency HWLOC_NAME(get_latency)

/* diff.h */

#define hwloc_topology_diff_obj_attr_type_e HWLOC_NAME(topology_diff_obj_attr_type_e)
#define hwloc_topology_diff_obj_attr_type_t HWLOC_NAME(topology_diff_obj_attr_type_t)
#define HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_SIZE HWLOC_NAME_CAPS(TOPOLOGY_DIFF_OBJ_ATTR_SIZE)
#define HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_NAME HWLOC_NAME_CAPS(TOPOLOGY_DIFF_OBJ_ATTR_NAME)
#define HWLOC_TOPOLOGY_DIFF_OBJ_ATTR_INFO HWLOC_NAME_CAPS(TOPOLOGY_DIFF_OBJ_ATTR_INFO)
#define hwloc_topology_diff_obj_attr_u HWLOC_NAME(topology_diff_obj_attr_u)
#define hwloc_topology_diff_obj_attr_generic_s HWLOC_NAME(topology_diff_obj_attr_generic_s)
#define hwloc_topology_diff_obj_attr_uint64_s HWLOC_NAME(topology_diff_obj_attr_uint64_s)
#define hwloc_topology_diff_obj_attr_string_s HWLOC_NAME(topology_diff_obj_attr_string_s)
#define hwloc_topology_diff_type_e HWLOC_NAME(topology_diff_type_e)
#define hwloc_topology_diff_type_t HWLOC_NAME(topology_diff_type_t)
#define HWLOC_TOPOLOGY_DIFF_OBJ_ATTR HWLOC_NAME_CAPS(TOPOLOGY_DIFF_OBJ_ATTR)
#define HWLOC_TOPOLOGY_DIFF_TOO_COMPLEX HWLOC_NAME_CAPS(TOPOLOGY_DIFF_TOO_COMPLEX)
#define hwloc_topology_diff_u HWLOC_NAME(topology_diff_u)
#define hwloc_topology_diff_t HWLOC_NAME(topology_diff_t)
#define hwloc_topology_diff_generic_s HWLOC_NAME(topology_diff_generic_s)
#define hwloc_topology_diff_obj_attr_s HWLOC_NAME(topology_diff_obj_attr_s)
#define hwloc_topology_diff_too_complex_s HWLOC_NAME(topology_diff_too_complex_s)
#define hwloc_topology_diff_build HWLOC_NAME(topology_diff_build)
#define hwloc_topology_diff_apply_flags_e HWLOC_NAME(topology_diff_apply_flags_e)
#define HWLOC_TOPOLOGY_DIFF_APPLY_REVERSE HWLOC_NAME_CAPS(TOPOLOGY_DIFF_APPLY_REVERSE)
#define hwloc_topology_diff_apply HWLOC_NAME(topology_diff_apply)
#define hwloc_topology_diff_destroy HWLOC_NAME(topology_diff_destroy)
#define hwloc_topology_diff_load_xml HWLOC_NAME(topology_diff_load_xml)
#define hwloc_topology_diff_export_xml HWLOC_NAME(topology_diff_export_xml)
#define hwloc_topology_diff_load_xmlbuffer HWLOC_NAME(topology_diff_load_xmlbuffer)
#define hwloc_topology_diff_export_xmlbuffer HWLOC_NAME(topology_diff_export_xmlbuffer)

/* glibc-sched.h */

#define hwloc_cpuset_to_glibc_sched_affinity HWLOC_NAME(cpuset_to_glibc_sched_affinity)
#define hwloc_cpuset_from_glibc_sched_affinity HWLOC_NAME(cpuset_from_glibc_sched_affinity)

/* linux-libnuma.h */

#define hwloc_cpuset_to_linux_libnuma_ulongs HWLOC_NAME(cpuset_to_linux_libnuma_ulongs)
#define hwloc_nodeset_to_linux_libnuma_ulongs HWLOC_NAME(nodeset_to_linux_libnuma_ulongs)
#define hwloc_cpuset_from_linux_libnuma_ulongs HWLOC_NAME(cpuset_from_linux_libnuma_ulongs)
#define hwloc_nodeset_from_linux_libnuma_ulongs HWLOC_NAME(nodeset_from_linux_libnuma_ulongs)
#define hwloc_cpuset_to_linux_libnuma_bitmask HWLOC_NAME(cpuset_to_linux_libnuma_bitmask)
#define hwloc_nodeset_to_linux_libnuma_bitmask HWLOC_NAME(nodeset_to_linux_libnuma_bitmask)
#define hwloc_cpuset_from_linux_libnuma_bitmask HWLOC_NAME(cpuset_from_linux_libnuma_bitmask)
#define hwloc_nodeset_from_linux_libnuma_bitmask HWLOC_NAME(nodeset_from_linux_libnuma_bitmask)

/* linux.h */

#define hwloc_linux_parse_cpumap_file HWLOC_NAME(linux_parse_cpumap_file)
#define hwloc_linux_set_tid_cpubind HWLOC_NAME(linux_set_tid_cpubind)
#define hwloc_linux_get_tid_cpubind HWLOC_NAME(linux_get_tid_cpubind)
#define hwloc_linux_get_tid_last_cpu_location HWLOC_NAME(linux_get_tid_last_cpu_location)

/* openfabrics-verbs.h */

#define hwloc_ibv_get_device_cpuset HWLOC_NAME(ibv_get_device_cpuset)
#define hwloc_ibv_get_device_osdev HWLOC_NAME(ibv_get_device_osdev)
#define hwloc_ibv_get_device_osdev_by_name HWLOC_NAME(ibv_get_device_osdev_by_name)

/* myriexpress.h */

#define hwloc_mx_board_get_device_cpuset HWLOC_NAME(mx_board_get_device_cpuset)
#define hwloc_mx_endpoint_get_device_cpuset HWLOC_NAME(mx_endpoint_get_device_cpuset)

/* intel-mic.h */

#define hwloc_intel_mic_get_device_cpuset HWLOC_NAME(intel_mic_get_device_cpuset)
#define hwloc_intel_mic_get_device_osdev_by_index HWLOC_NAME(intel_mic_get_device_osdev_by_index)

/* opencl.h */

#define hwloc_opencl_get_device_cpuset HWLOC_NAME(opencl_get_device_cpuset)
#define hwloc_opencl_get_device_osdev HWLOC_NAME(opencl_get_device_osdev)
#define hwloc_opencl_get_device_osdev_by_index HWLOC_NAME(opencl_get_device_osdev_by_index)

/* cuda.h */

#define hwloc_cuda_get_device_pci_ids HWLOC_NAME(cuda_get_device_pci_ids)
#define hwloc_cuda_get_device_cpuset HWLOC_NAME(cuda_get_device_cpuset)
#define hwloc_cuda_get_device_pcidev HWLOC_NAME(cuda_get_device_pcidev)
#define hwloc_cuda_get_device_osdev HWLOC_NAME(cuda_get_device_osdev)
#define hwloc_cuda_get_device_osdev_by_index HWLOC_NAME(cuda_get_device_osdev_by_index)

/* cudart.h */

#define hwloc_cudart_get_device_pci_ids HWLOC_NAME(cudart_get_device_pci_ids)
#define hwloc_cudart_get_device_cpuset HWLOC_NAME(cudart_get_device_cpuset)
#define hwloc_cudart_get_device_pcidev HWLOC_NAME(cudart_get_device_pcidev)
#define hwloc_cudart_get_device_osdev_by_index HWLOC_NAME(cudart_get_device_osdev_by_index)

/* nvml.h */

#define hwloc_nvml_get_device_cpuset HWLOC_NAME(nvml_get_device_cpuset)
#define hwloc_nvml_get_device_osdev HWLOC_NAME(nvml_get_device_osdev)
#define hwloc_nvml_get_device_osdev_by_index HWLOC_NAME(nvml_get_device_osdev_by_index)

/* gl.h */

#define hwloc_gl_get_display_osdev_by_port_device HWLOC_NAME(gl_get_display_osdev_by_port_device)
#define hwloc_gl_get_display_osdev_by_name HWLOC_NAME(gl_get_display_osdev_by_name)
#define hwloc_gl_get_display_by_osdev HWLOC_NAME(gl_get_display_by_osdev)

/* hwloc/plugins.h */

#define hwloc_disc_component_type_e HWLOC_NAME(disc_component_type_e)
#define HWLOC_DISC_COMPONENT_TYPE_CPU HWLOC_NAME_CAPS(DISC_COMPONENT_TYPE_CPU)
#define HWLOC_DISC_COMPONENT_TYPE_GLOBAL HWLOC_NAME_CAPS(DISC_COMPONENT_TYPE_GLOBAL)
#define HWLOC_DISC_COMPONENT_TYPE_MISC HWLOC_NAME_CAPS(DISC_COMPONENT_TYPE_MISC)
#define hwloc_disc_component_type_t HWLOC_NAME(disc_component_type_t)
#define hwloc_disc_component HWLOC_NAME(disc_component)

#define hwloc_backend HWLOC_NAME(backend)
#define hwloc_backend_flag_e HWLOC_NAME(backend_flag_e)
#define HWLOC_BACKEND_FLAG_NEED_LEVELS HWLOC_NAME_CAPS(BACKEND_FLAG_NEED_LEVELS)

#define hwloc_backend_alloc HWLOC_NAME(backend_alloc)
#define hwloc_backend_enable HWLOC_NAME(backend_enable)
#define hwloc_backends_get_obj_cpuset HWLOC_NAME(backends_get_obj_cpuset)
#define hwloc_backends_notify_new_object HWLOC_NAME(backends_notify_new_object)

#define hwloc_component_type_e HWLOC_NAME(component_type_e)
#define HWLOC_COMPONENT_TYPE_DISC HWLOC_NAME_CAPS(COMPONENT_TYPE_DISC)
#define HWLOC_COMPONENT_TYPE_XML HWLOC_NAME_CAPS(COMPONENT_TYPE_XML)
#define hwloc_component_type_t HWLOC_NAME(component_type_t)
#define hwloc_component HWLOC_NAME(component)

#define hwloc_plugin_check_namespace HWLOC_NAME(plugin_check_namespace)

#define hwloc_insert_object_by_cpuset HWLOC_NAME(insert_object_by_cpuset)
#define hwloc_report_error_t HWLOC_NAME(report_error_t)
#define hwloc_report_os_error HWLOC_NAME(report_os_error)
#define hwloc_hide_errors HWLOC_NAME(hide_errors)
#define hwloc__insert_object_by_cpuset HWLOC_NAME(_insert_object_by_cpuset)
#define hwloc_insert_object_by_parent HWLOC_NAME(insert_object_by_parent)
#define hwloc_alloc_setup_object HWLOC_NAME(alloc_setup_object)
#define hwloc_fill_object_sets HWLOC_NAME(fill_object_sets)

#define hwloc_insert_pci_device_list HWLOC_NAME(insert_pci_device_list)
#define hwloc_pci_find_cap HWLOC_NAME(pci_find_cap)
#define hwloc_pci_find_linkspeed HWLOC_NAME(pci_find_linkspeed)
#define hwloc_pci_prepare_bridge HWLOC_NAME(pci_prepare_bridge)

/* hwloc/deprecated.h */

#define hwloc_obj_type_of_string HWLOC_NAME(obj_type_of_string )
#define hwloc_obj_snprintf HWLOC_NAME(obj_snprintf)
#define hwloc_distributev HWLOC_NAME(distributev)
#define hwloc_distribute HWLOC_NAME(distribute)

/* private/debug.h */

#define hwloc_debug HWLOC_NAME(debug)

/* private/misc.h */

#define hwloc_snprintf HWLOC_NAME(snprintf)
#define hwloc_namecoloncmp HWLOC_NAME(namecoloncmp)
#define hwloc_ffsl_manual HWLOC_NAME(ffsl_manual)
#define hwloc_ffs32 HWLOC_NAME(ffs32)
#define hwloc_ffsl_from_ffs32 HWLOC_NAME(ffsl_from_ffs32)
#define hwloc_flsl_manual HWLOC_NAME(flsl_manual)
#define hwloc_fls32 HWLOC_NAME(fls32)
#define hwloc_flsl_from_fls32 HWLOC_NAME(flsl_from_fls32)
#define hwloc_weight_long HWLOC_NAME(weight_long)
#define hwloc_strncasecmp HWLOC_NAME(strncasecmp)

/* private/cpuid-x86.h */

#define hwloc_have_x86_cpuid HWLOC_NAME(have_x86_cpuid)
#define hwloc_x86_cpuid HWLOC_NAME(x86_cpuid)

/* private/xml.h */

#define hwloc__xml_verbose HWLOC_NAME(_xml_verbose)

#define hwloc__xml_import_state_s HWLOC_NAME(_xml_import_state_s)
#define hwloc__xml_import_state_t HWLOC_NAME(_xml_import_state_t)
#define hwloc__xml_import_diff HWLOC_NAME(_xml_import_diff)
#define hwloc_xml_backend_data_s HWLOC_NAME(xml_backend_data_s)
#define hwloc__xml_export_state_s HWLOC_NAME(_xml_export_state_s)
#define hwloc__xml_export_state_t HWLOC_NAME(_xml_export_state_t)
#define hwloc__xml_export_object HWLOC_NAME(_xml_export_object)
#define hwloc__xml_export_diff HWLOC_NAME(_xml_export_diff)

#define hwloc_xml_callbacks HWLOC_NAME(xml_callbacks)
#define hwloc_xml_component HWLOC_NAME(xml_component)
#define hwloc_xml_callbacks_register HWLOC_NAME(xml_callbacks_register)
#define hwloc_xml_callbacks_reset HWLOC_NAME(xml_callbacks_reset)

/* private/components.h */

#define hwloc_disc_component_force_enable HWLOC_NAME(disc_component_force_enable)
#define hwloc_disc_components_enable_others HWLOC_NAME(disc_components_instantiate_others)

#define hwloc_backends_disable_all HWLOC_NAME(backends_disable_all)
#define hwloc_backends_is_thissystem HWLOC_NAME(backends_is_thissystem)

#define hwloc_components_init HWLOC_NAME(components_init)
#define hwloc_components_destroy_all HWLOC_NAME(components_destroy_all)

/* private/private.h */

#define hwloc_ignore_type_e HWLOC_NAME(ignore_type_e)

#define HWLOC_IGNORE_TYPE_NEVER HWLOC_NAME_CAPS(IGNORE_TYPE_NEVER)
#define HWLOC_IGNORE_TYPE_KEEP_STRUCTURE HWLOC_NAME_CAPS(IGNORE_TYPE_KEEP_STRUCTURE)
#define HWLOC_IGNORE_TYPE_ALWAYS HWLOC_NAME_CAPS(IGNORE_TYPE_ALWAYS)

#define hwloc_os_distances_s HWLOC_NAME(os_distances_s)

#define hwloc_xml_imported_distances_s HWLOC_NAME(xml_imported_distances_s)

#define hwloc_alloc_obj_cpusets HWLOC_NAME(alloc_obj_cpusets)
#define hwloc_setup_pu_level HWLOC_NAME(setup_pu_level)
#define hwloc_get_sysctlbyname HWLOC_NAME(get_sysctlbyname)
#define hwloc_get_sysctl HWLOC_NAME(get_sysctl)
#define hwloc_fallback_nbprocessors HWLOC_NAME(fallback_nbprocessors)
#define hwloc_connect_children HWLOC_NAME(connect_children)
#define hwloc_connect_levels HWLOC_NAME(connect_levels)

#define hwloc__object_cpusets_compare_first HWLOC_NAME(_object_cpusets_compare_first)

#define hwloc_topology_setup_defaults HWLOC_NAME(topology_setup_defaults)
#define hwloc_topology_clear HWLOC_NAME(topology_clear)

#define hwloc__add_info HWLOC_NAME(_add_info)
#define hwloc__find_info_slot HWLOC_NAME(_find_info_slot)
#define hwloc__move_infos HWLOC_NAME(_move_infos)
#define hwloc__free_infos HWLOC_NAME(_free_infos)

#define hwloc_binding_hooks HWLOC_NAME(binding_hooks)
#define hwloc_set_native_binding_hooks HWLOC_NAME(set_native_binding_hooks)
#define hwloc_set_binding_hooks HWLOC_NAME(set_binding_hooks)

#define hwloc_set_linuxfs_hooks HWLOC_NAME(set_linuxfs_hooks)
#define hwloc_set_bgq_hooks HWLOC_NAME(set_bgq_hooks)
#define hwloc_set_solaris_hooks HWLOC_NAME(set_solaris_hooks)
#define hwloc_set_aix_hooks HWLOC_NAME(set_aix_hooks)
#define hwloc_set_osf_hooks HWLOC_NAME(set_osf_hooks)
#define hwloc_set_windows_hooks HWLOC_NAME(set_windows_hooks)
#define hwloc_set_darwin_hooks HWLOC_NAME(set_darwin_hooks)
#define hwloc_set_freebsd_hooks HWLOC_NAME(set_freebsd_hooks)
#define hwloc_set_netbsd_hooks HWLOC_NAME(set_netbsd_hooks)
#define hwloc_set_hpux_hooks HWLOC_NAME(set_hpux_hooks)

#define hwloc_add_uname_info HWLOC_NAME(add_uname_info)
#define hwloc_free_unlinked_object HWLOC_NAME(free_unlinked_object)
#define hwloc__duplicate_objects HWLOC_NAME(_duplicate_objects)

#define hwloc_alloc_heap HWLOC_NAME(alloc_heap)
#define hwloc_alloc_mmap HWLOC_NAME(alloc_mmap)
#define hwloc_free_heap HWLOC_NAME(free_heap)
#define hwloc_free_mmap HWLOC_NAME(free_mmap)
#define hwloc_alloc_or_fail HWLOC_NAME(alloc_or_fail)

#define hwloc_distances_init HWLOC_NAME(distances_init)
#define hwloc_distances_destroy HWLOC_NAME(distances_destroy)
#define hwloc_distances_set HWLOC_NAME(distances_set)
#define hwloc_distances_set_from_env HWLOC_NAME(distances_set_from_env)
#define hwloc_distances_restrict_os HWLOC_NAME(distances_restrict_os)
#define hwloc_distances_restrict HWLOC_NAME(distances_restrict)
#define hwloc_distances_finalize_os HWLOC_NAME(distances_finalize_os)
#define hwloc_distances_finalize_logical HWLOC_NAME(distances_finalize_logical)
#define hwloc_clear_object_distances HWLOC_NAME(clear_object_distances)
#define hwloc_clear_object_distances_one HWLOC_NAME(clear_object_distances_one)
#define hwloc_group_by_distances HWLOC_NAME(group_by_distances)

#define hwloc_encode_to_base64 HWLOC_NAME(encode_to_base64)
#define hwloc_decode_from_base64 HWLOC_NAME(decode_from_base64)

#define hwloc_obj_add_info_nodup HWLOC_NAME(obj_add_info_nodup)

#define hwloc_progname HWLOC_NAME(progname)

#define hwloc_bitmap_compare_inclusion HWLOC_NAME(bitmap_compare_inclusion)

/* private/solaris-chiptype.h */

#define hwloc_solaris_get_chip_type HWLOC_NAME(solaris_get_chip_type)
#define hwloc_solaris_get_chip_model HWLOC_NAME(solaris_get_chip_model)

#endif /* HWLOC_SYM_TRANSFORM */


#ifdef __cplusplus
} /* extern "C" */
#endif


#endif /* HWLOC_RENAME_H */
