/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_PATCHER_LINUX_H)
#define OPAL_PATCHER_LINUX_H

#include "opal_config.h"

#include "opal/mca/patcher/base/base.h"
#include "opal/mca/patcher/patcher.h"

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"

struct mca_patcher_linux_patch_got_t {
    opal_list_item_t super;
    void **got_entry;
    void *got_orig;
};

typedef struct mca_patcher_linux_patch_got_t mca_patcher_linux_patch_got_t;

OBJ_CLASS_DECLARATION(mca_patcher_linux_patch_got_t);

struct mca_patcher_linux_patch_t {
    mca_patcher_base_patch_t super;
    opal_list_t patch_got_list;
};

typedef struct mca_patcher_linux_patch_t mca_patcher_linux_patch_t;

OBJ_CLASS_DECLARATION(mca_patcher_linux_patch_t);

extern mca_patcher_base_module_t mca_patcher_linux_module;
extern mca_patcher_base_component_t mca_patcher_linux_component;

#endif /* !defined(OPAL_PATCHER_LINUX_H) */
