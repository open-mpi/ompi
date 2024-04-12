/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Inria.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef PRTE_RTC_HWLOC_H
#define PRTE_RTC_HWLOC_H

#include "prte_config.h"

#include "src/mca/rtc/rtc.h"

BEGIN_C_DECLS

typedef enum {
    VM_HOLE_NONE = -1,
    VM_HOLE_BEGIN = 0,        /* use hole at the very beginning */
    VM_HOLE_AFTER_HEAP = 1,   /* use hole right after heap */
    VM_HOLE_BEFORE_STACK = 2, /* use hole right before stack */
    VM_HOLE_BIGGEST = 3,      /* use biggest hole */
    VM_HOLE_IN_LIBS = 4,      /* use biggest hole between heap and stack */
    VM_HOLE_CUSTOM = 5,       /* use given address if available */
} prte_rtc_hwloc_vm_hole_kind_t;

typedef enum {
    VM_MAP_FILE = 0,
    VM_MAP_ANONYMOUS = 1,
    VM_MAP_HEAP = 2,
    VM_MAP_STACK = 3,
    VM_MAP_OTHER = 4 /* vsyscall/vdso/vvar shouldn't occur since we stop after stack */
} prte_rtc_hwloc_vm_map_kind_t;

typedef struct {
    prte_rtc_base_component_t super;
    prte_rtc_hwloc_vm_hole_kind_t kind;
} prte_mca_rtc_hwloc_component_t;

PRTE_MODULE_EXPORT extern prte_mca_rtc_hwloc_component_t prte_mca_rtc_hwloc_component;

extern prte_rtc_base_module_t prte_rtc_hwloc_module;

END_C_DECLS

#endif /* PRTE_RTC_HWLOC_H */
