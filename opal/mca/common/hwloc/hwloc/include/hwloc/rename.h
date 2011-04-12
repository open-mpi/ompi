/*
 * Copyright © 2009-2010 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_RENAME_H
#define HWLOC_RENAME_H

#include <hwloc/config.h>


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
#define HWLOC_OBJ_NODE HWLOC_NAME_CAPS(OBJ_NODE)
#define HWLOC_OBJ_SOCKET HWLOC_NAME_CAPS(OBJ_SOCKET)
#define HWLOC_OBJ_CACHE HWLOC_NAME_CAPS(OBJ_CACHE)
#define HWLOC_OBJ_CORE HWLOC_NAME_CAPS(OBJ_CORE)
#define HWLOC_OBJ_PU HWLOC_NAME_CAPS(OBJ_PU)
#define HWLOC_OBJ_MISC HWLOC_NAME_CAPS(OBJ_MISC)
#define HWLOC_OBJ_GROUP HWLOC_NAME_CAPS(OBJ_GROUP)

#define hwloc_obj_type_t HWLOC_NAME(obj_type_t)

#define hwloc_compare_types HWLOC_NAME(compare_types)

#define HWLOC_TYPE_UNORDERED HWLOC_NAME_CAPS(TYPE_UNORDERED)

#define hwloc_obj_memory_s HWLOC_NAME(obj_memory_s)

#define hwloc_obj HWLOC_NAME(obj)
#define hwloc_obj_t HWLOC_NAME(obj_t)

#define hwloc_obj_attr_u HWLOC_NAME(obj_attr_u)
#define hwloc_cache_attr_s HWLOC_NAME(cache_attr_s)
#define hwloc_memory_attr_s HWLOC_NAME(cache_memory_s)
#define hwloc_machine_attr_s HWLOC_NAME(cache_machine_s)
#define hwloc_misc_attr_s HWLOC_NAME(cache_misc_s)

#define hwloc_topology_init HWLOC_NAME(topology_init)
#define hwloc_topology_load HWLOC_NAME(topology_load)
#define hwloc_topology_destroy HWLOC_NAME(topology_destroy)
#define hwloc_topology_check HWLOC_NAME(topology_check)
#define hwloc_topology_ignore_type HWLOC_NAME(topology_ignore_type)
#define hwloc_topology_ignore_type_keep_structure HWLOC_NAME(topology_ignore_type_keep_structure)
#define hwloc_topology_ignore_all_keep_structure HWLOC_NAME(topology_ignore_all_keep_structure)

#define hwloc_topology_flags_e HWLOC_NAME(topology_flags_e)

#define HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM HWLOC_NAME_CAPS(TOPOLOGY_FLAG_WHOLE_SYSTEM)
#define HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM HWLOC_NAME_CAPS(TOPOLOGY_FLAG_IS_THISSYSTEM)

#define hwloc_topology_set_flags HWLOC_NAME(topology_set_flags)
#define hwloc_topology_set_fsroot HWLOC_NAME(topology_set_fsroot)
#define hwloc_topology_set_pid HWLOC_NAME(topology_set_pid)
#define hwloc_topology_set_synthetic HWLOC_NAME(topology_set_synthetic)
#define hwloc_topology_set_xml HWLOC_NAME(topology_set_xml)

#define hwloc_topology_support HWLOC_NAME(topology_support)
#define hwloc_topology_get_support HWLOC_NAME(topology_get_support)
#define hwloc_topology_export_xml HWLOC_NAME(topology_export_xml)

#define hwloc_topology_insert_misc_object_by_cpuset HWLOC_NAME(topology_insert_misc_object_by_cpuset)
#define hwloc_topology_insert_misc_object_by_parent HWLOC_NAME(topology_insert_misc_object_by_parent)

#define hwloc_topology_get_depth HWLOC_NAME(topology_get_depth)
#define hwloc_get_type_depth HWLOC_NAME(get_type_depth)

#define HWLOC_TYPE_DEPTH_UNKNOWN HWLOC_NAME_CAPS(TYPE_DEPTH_UNKNOWN)
#define HWLOC_TYPE_DEPTH_MULTIPLE HWLOC_NAME_CAPS(TYPE_DEPTH_MULTIPLE)

#define hwloc_get_depth_type HWLOC_NAME(get_depth_type)
#define hwloc_get_nbobjs_by_depth HWLOC_NAME(get_nbobjs_by_depth)
#define hwloc_get_nbobjs_by_type HWLOC_NAME(get_nbobjs_by_type)

#define hwloc_topology_is_thissystem HWLOC_NAME(topology_is_thissystem)

#define hwloc_get_obj_by_depth HWLOC_NAME(get_obj_by_depth )
#define hwloc_get_obj_by_type HWLOC_NAME(get_obj_by_type )

#define hwloc_obj_type_string HWLOC_NAME(obj_type_string )
#define hwloc_obj_type_of_string HWLOC_NAME(obj_type_of_string )
#define hwloc_obj_type_snprintf HWLOC_NAME(obj_type_snprintf )
#define hwloc_obj_attr_snprintf HWLOC_NAME(obj_attr_snprintf )
#define hwloc_obj_snprintf HWLOC_NAME(obj_snprintf)
#define hwloc_obj_cpuset_snprintf HWLOC_NAME(obj_cpuset_snprintf)
#define hwloc_obj_get_info_by_name HWLOC_NAME(obj_get_info_by_name)

#define HWLOC_CPUBIND_PROCESS HWLOC_NAME_CAPS(CPUBIND_PROCESS)
#define HWLOC_CPUBIND_THREAD HWLOC_NAME_CAPS(CPUBIND_THREAD)
#define HWLOC_CPUBIND_STRICT HWLOC_NAME_CAPS(CPUBIND_STRICT)

#define hwloc_cpubind_flags_t HWLOC_NAME(cpubind_flags_t)

#define hwloc_set_cpubind HWLOC_NAME(set_cpubind)
#define hwloc_get_cpubind HWLOC_NAME(get_cpubind)
#define hwloc_set_proc_cpubind HWLOC_NAME(set_proc_cpubind)
#define hwloc_get_proc_cpubind HWLOC_NAME(get_proc_cpubind)
#define hwloc_set_thread_cpubind HWLOC_NAME(set_thread_cpubind)
#define hwloc_get_thread_cpubind HWLOC_NAME(get_thread_cpubind)

#define HWLOC_MEMBIND_DEFAULT HWLOC_NAME_CAPS(MEMBIND_DEFAULT)
#define HWLOC_MEMBIND_FIRSTTOUCH HWLOC_NAME_CAPS(MEMBIND_FIRSTTOUCH)
#define HWLOC_MEMBIND_BIND HWLOC_NAME_CAPS(MEMBIND_BIND)
#define HWLOC_MEMBIND_INTERLEAVE HWLOC_NAME_CAPS(MEMBIND_INTERLEAVE)
#define HWLOC_MEMBIND_REPLICATE HWLOC_NAME_CAPS(MEMBIND_REPLICATE)
#define HWLOC_MEMBIND_NEXTTOUCH HWLOC_NAME_CAPS(MEMBIND_NEXTTOUCH)

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

/* hwloc/bitmap.h */

#define hwloc_bitmap HWLOC_NAME(bitmap)
#define hwloc_bitmap_s HWLOC_NAME(bitmap_s)
#define hwloc_bitmap_t HWLOC_NAME(bitmap_t)
#define hwloc_const_bitmap_t HWLOC_NAME(const_bitmap_t)

#define hwloc_bitmap_alloc HWLOC_NAME(bitmap_alloc)
#define hwloc_bitmap_free HWLOC_NAME(bitmap_free)
#define hwloc_bitmap_dup HWLOC_NAME(bitmap_dup)
#define hwloc_bitmap_copy HWLOC_NAME(bitmap_copy)
#define hwloc_bitmap_snprintf HWLOC_NAME(bitmap_snprintf)
#define hwloc_bitmap_asprintf HWLOC_NAME(bitmap_asprintf)
#define hwloc_bitmap_sscanf HWLOC_NAME(bitmap_sscanf)
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

/* hwloc/cpuset.h -- deprecated but still available */

#define hwloc_cpuset HWLOC_NAME(cpuset)
#define hwloc_cpuset_s HWLOC_NAME(cpuset_s)
#define hwloc_cpuset_t HWLOC_NAME(cpuset_t)
#define hwloc_const_cpuset_t HWLOC_NAME(const_cpuset_t)

#define hwloc_cpuset_alloc HWLOC_NAME(cpuset_alloc)
#define hwloc_cpuset_free HWLOC_NAME(cpuset_free)
#define hwloc_cpuset_dup HWLOC_NAME(cpuset_dup)
#define hwloc_cpuset_copy HWLOC_NAME(cpuset_copy)
#define hwloc_cpuset_snprintf HWLOC_NAME(cpuset_snprintf)
#define hwloc_cpuset_asprintf HWLOC_NAME(cpuset_asprintf)
#define hwloc_cpuset_from_string HWLOC_NAME(cpuset_from_string)
#define hwloc_cpuset_zero HWLOC_NAME(cpuset_zero)
#define hwloc_cpuset_fill HWLOC_NAME(cpuset_fill)
#define hwloc_cpuset_from_ulong HWLOC_NAME(cpuset_from_ulong)

#define hwloc_cpuset_from_ith_ulong HWLOC_NAME(cpuset_from_ith_ulong)
#define hwloc_cpuset_to_ulong HWLOC_NAME(cpuset_to_ulong)
#define hwloc_cpuset_to_ith_ulong HWLOC_NAME(cpuset_to_ith_ulong)
#define hwloc_cpuset_cpu HWLOC_NAME(cpuset_cpu)
#define hwloc_cpuset_all_but_cpu HWLOC_NAME(cpuset_all_but_cpu)
#define hwloc_cpuset_set HWLOC_NAME(cpuset_set)
#define hwloc_cpuset_set_range HWLOC_NAME(cpuset_set_range)
#define hwloc_cpuset_clr HWLOC_NAME(cpuset_clr)
#define hwloc_cpuset_clr_range HWLOC_NAME(cpuset_clr_range)
#define hwloc_cpuset_isset HWLOC_NAME(cpuset_isset)
#define hwloc_cpuset_iszero HWLOC_NAME(cpuset_iszero)
#define hwloc_cpuset_isfull HWLOC_NAME(cpuset_isfull)
#define hwloc_cpuset_isequal HWLOC_NAME(cpuset_isequal)
#define hwloc_cpuset_intersects HWLOC_NAME(cpuset_intersects)
#define hwloc_cpuset_isincluded HWLOC_NAME(cpuset_isincluded)
#define hwloc_cpuset_or HWLOC_NAME(cpuset_or)
#define hwloc_cpuset_and HWLOC_NAME(cpuset_and)
#define hwloc_cpuset_andnot HWLOC_NAME(cpuset_andnot)
#define hwloc_cpuset_xor HWLOC_NAME(cpuset_xor)
#define hwloc_cpuset_not HWLOC_NAME(cpuset_not)
#define hwloc_cpuset_first HWLOC_NAME(cpuset_first)
#define hwloc_cpuset_last HWLOC_NAME(cpuset_last)
#define hwloc_cpuset_next HWLOC_NAME(cpuset_next)
#define hwloc_cpuset_singlify HWLOC_NAME(cpuset_singlify)
#define hwloc_cpuset_compare_first HWLOC_NAME(cpuset_compare_first)
#define hwloc_cpuset_compare HWLOC_NAME(cpuset_compare)
#define hwloc_cpuset_weight HWLOC_NAME(cpuset_weight)

/* hwloc/helper.h */

#define hwloc_get_type_or_below_depth HWLOC_NAME(get_type_or_below_depth)
#define hwloc_get_type_or_above_depth HWLOC_NAME(get_type_or_above_depth)
#define hwloc_get_root_obj HWLOC_NAME(get_root_obj)
#define hwloc_get_system_obj HWLOC_NAME(get_system_obj)
#define hwloc_get_ancestor_obj_by_depth HWLOC_NAME(get_ancestor_obj_by_depth)
#define hwloc_get_ancestor_obj_by_type HWLOC_NAME(get_ancestor_obj_by_type)
#define hwloc_get_next_obj_by_depth HWLOC_NAME(get_next_obj_by_depth)
#define hwloc_get_next_obj_by_type HWLOC_NAME(get_next_obj_by_type)
#define hwloc_get_pu_obj_by_os_index HWLOC_NAME(get_pu_obj_by_os_index)
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
#define hwloc_get_child_covering_cpuset HWLOC_NAME(get_child_covering_cpuset)
#define hwloc_get_obj_covering_cpuset HWLOC_NAME(get_obj_covering_cpuset)
#define hwloc_get_next_obj_covering_cpuset_by_depth HWLOC_NAME(get_next_obj_covering_cpuset_by_depth)
#define hwloc_get_next_obj_covering_cpuset_by_type HWLOC_NAME(get_next_obj_covering_cpuset_by_type)
#define hwloc_get_cache_covering_cpuset HWLOC_NAME(get_cache_covering_cpuset)
#define hwloc_get_shared_cache_covering_obj HWLOC_NAME(get_shared_cache_covering_obj)
#define hwloc_get_closest_objs HWLOC_NAME(get_closest_objs)
#define hwloc_get_obj_below_by_type HWLOC_NAME(get_obj_below_by_type)
#define hwloc_get_obj_below_array_by_type HWLOC_NAME(get_obj_below_array_by_type)
#define hwloc_distribute HWLOC_NAME(distribute)
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

/* glibc-sched.h */

#define hwloc_cpuset_to_glibc_sched_affinity HWLOC_NAME(cpuset_to_glibc_sched_affinity)
#define hwloc_cpuset_from_glibc_sched_affinity HWLOC_NAME(cpuset_from_glibc_sched_affinity)

/* linux-libnuma.h */

#define hwloc_cpuset_to_linux_libnuma_ulongs HWLOC_NAME(cpuset_to_linux_libnuma_ulongs)
#define hwloc_cpuset_from_linux_libnuma_ulongs HWLOC_NAME(cpuset_from_linux_libnuma_ulongs)
#define hwloc_cpuset_to_linux_libnuma_bitmask HWLOC_NAME(cpuset_to_linux_libnuma_bitmask)
#define hwloc_cpuset_from_linux_libnuma_bitmask HWLOC_NAME(cpuset_from_linux_libnuma_bitmask)
#define hwloc_cpuset_to_linux_libnuma_nodemask HWLOC_NAME(cpuset_to_linux_libnuma_nodemask)
#define hwloc_cpuset_from_linux_libnuma_nodemask HWLOC_NAME(cpuset_from_linux_libnuma_nodemask)

/* linux.h */

#define hwloc_linux_parse_cpumap_file HWLOC_NAME(linux_parse_cpumap_file)
#define hwloc_linux_set_tid_cpubind HWLOC_NAME(linux_set_tid_cpubind)
#define hwloc_linux_get_tid_cpubind HWLOC_NAME(linux_get_tid_cpubind)

/* openfabrics-verbs.h */

#define hwloc_ibv_get_device_cpuset HWLOC_NAME(ibv_get_device_cpuset)

/* myriexpress.h */

#define hwloc_mx_board_get_device_cpuset HWLOC_NAME(mx_board_get_device_cpuset)
#define hwloc_mx_endpoint_get_device_cpuset HWLOC_NAME(mx_endpoint_get_device_cpuset)

/* cuda.h */

#define hwloc_cuda_get_device_cpuset HWLOC_NAME(cuda_get_device_cpuset)

/* cudart.h */

#define hwloc_cudart_get_device_cpuset HWLOC_NAME(cudart_get_device_cpuset)

/* private/misc.h */

#define hwloc_snprintf HWLOC_NAME(snprintf)
#define hwloc_namecoloncmp HWLOC_NAME(namecoloncmp)
#define hwloc_weight_long HWLOC_NAME(weight_long)

/* private/cpuid.h */

#define hwloc_have_cpuid HWLOC_NAME(have_cpuid)
#define hwloc_cpuid HWLOC_NAME(cpuid)

/* private/private.h */

#define hwloc_ignore_type_e HWLOC_NAME(ignore_type_e)

#define HWLOC_IGNORE_TYPE_NEVER HWLOC_NAME_CAPS(IGNORE_TYPE_NEVER)
#define HWLOC_IGNORE_TYPE_KEEP_STRUCTURE HWLOC_NAME_CAPS(IGNORE_TYPE_KEEP_STRUCTURE)
#define HWLOC_IGNORE_TYPE_ALWAYS HWLOC_NAME_CAPS(IGNORE_TYPE_ALWAYS)

#define hwloc_backend_e HWLOC_NAME(backend_e)
#define hwloc_backend_t HWLOC_NAME(backend_t)

#define HWLOC_BACKEND_NONE HWLOC_NAME_CAPS(BACKEND_NONE)
#define HWLOC_BACKEND_SYNTHETIC HWLOC_NAME_CAPS(BACKEND_SYNTHETIC)
#define HWLOC_BACKEND_SYSFS HWLOC_NAME_CAPS(BACKEND_SYSFS)
#define HWLOC_BACKEND_XML HWLOC_NAME_CAPS(BACKEND_XML)

#define hwloc_backend_params_u HWLOC_NAME(backend_params_u)
#define hwloc_backend_params_sysfs_s HWLOC_NAME(backend_params_sysfs_s)
#define hwloc_backend_params_osf HWLOC_NAME(backend_params_osf)
#define hwloc_backend_params_xml_s HWLOC_NAME(backend_params_xml_s)
#define hwloc_backend_params_synthetic_s HWLOC_NAME(backend_params_synthetic_s)

#define hwloc_setup_pu_level HWLOC_NAME(setup_pu_level)
#define hwloc_setup_misc_level_from_distances HWLOC_NAME(setup_misc_level_from_distances)
#define hwloc_get_sysctlbyname HWLOC_NAME(get_sysctlbyname)
#define hwloc_get_sysctl HWLOC_NAME(get_sysctl)
#define hwloc_fallback_nbprocessors HWLOC_NAME(fallback_nbprocessors)

#define hwloc_look_linux HWLOC_NAME(look_linux)
#define hwloc_set_linux_hooks HWLOC_NAME(set_linux_hooks)
#define hwloc_backend_sysfs_init HWLOC_NAME(backend_sysfs_init)
#define hwloc_backend_sysfs_exit HWLOC_NAME(backend_sysfs_exit)

#define hwloc_backend_xml_init HWLOC_NAME(backend_xml_init)
#define hwloc_look_xml HWLOC_NAME(look_xml)
#define hwloc_backend_xml_exit HWLOC_NAME(backend_xml_exit)

#define hwloc_look_solaris HWLOC_NAME(look_solaris)
#define hwloc_set_solaris_hooks HWLOC_NAME(set_solaris_hooks)

#define hwloc_look_aix HWLOC_NAME(look_aix)
#define hwloc_set_aix_hooks HWLOC_NAME(set_aix_hooks)

#define hwloc_look_osf HWLOC_NAME(look_osf)
#define hwloc_set_osf_hooks HWLOC_NAME(set_osf_hooks)

#define hwloc_look_windows HWLOC_NAME(look_windows)
#define hwloc_set_windows_hooks HWLOC_NAME(set_windows_hooks)

#define hwloc_look_darwin HWLOC_NAME(look_darwin)
#define hwloc_set_darwin_hooks HWLOC_NAME(set_darwin_hooks)

#define hwloc_look_freebsd HWLOC_NAME(look_freebsd)
#define hwloc_set_freebsd_hooks HWLOC_NAME(set_freebsd_hooks)

#define hwloc_look_hpux HWLOC_NAME(look_hpux)
#define hwloc_set_hpux_hooks HWLOC_NAME(set_hpux_hooks)

#define hwloc_look_x86 HWLOC_NAME(look_x86)

#define hwloc_backend_synthetic_init HWLOC_NAME(backend_synthetic_init)
#define hwloc_backend_synthetic_exit HWLOC_NAME(backend_synthetic_exit)
#define hwloc_look_synthetic  HWLOC_NAME(look_synthetic )

#define hwloc_insert_object_by_cpuset HWLOC_NAME(insert_object_by_cpuset)
#define hwloc_insert_object_by_parent HWLOC_NAME(insert_object_by_parent)
#define hwloc_add_object_info HWLOC_NAME(add_object_info)
#define hwloc_add_uname_info HWLOC_NAME(add_uname_info)
#define hwloc_free_object HWLOC_NAME(free_object)
#define hwloc_cpuset_printf_value HWLOC_NAME(cpuset_printf_value)
#define hwloc_alloc_setup_object HWLOC_NAME(alloc_setup_object)
#define hwloc_setup_level HWLOC_NAME(setup_level)

#endif /* HWLOC_SYM_TRANSFORM */


#ifdef __cplusplus
} /* extern "C" */
#endif


#endif /* HWLOC_RENAME_H */
