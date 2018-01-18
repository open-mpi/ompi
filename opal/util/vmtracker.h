/*
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * @file
 */

#ifndef OPAL_VMTRACKER_H
#define OPAL_VMTRACKER_H

#include "opal_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

BEGIN_C_DECLS

typedef enum {
    OPAL_VM_HOLE_NONE = -1,
    OPAL_VM_HOLE_BEGIN = 0,        /* use hole at the very beginning */
    OPAL_VM_HOLE_AFTER_HEAP = 1,   /* use hole right after heap */
    OPAL_VM_HOLE_BEFORE_STACK = 2, /* use hole right before stack */
    OPAL_VM_HOLE_BIGGEST = 3,      /* use biggest hole */
    OPAL_VM_HOLE_IN_LIBS = 4,      /* use biggest hole between heap and stack */
    OPAL_VM_HOLE_CUSTOM = 5,       /* use given address if available */
} opal_vm_hole_kind_t;

OPAL_DECLSPEC void opal_vmtracker_init(void);
OPAL_DECLSPEC void opal_vmtracker_finalize(void);

/**
 * Search the virtual memory space to find a "hole" - i.e., an empty spot
 * spanning at least memsize bytes. The hole "kind" indicates where to
 * search for the desired hole. The vmtracker caches locations as they
 * are assigned to avoid conflicts.
 */
OPAL_DECLSPEC int opal_vmtracker_assign_address(opal_vm_hole_kind_t kind,
                                                size_t *address, size_t memsize);

END_C_DECLS
#endif /* OPAL_VMTRACKER_H */
