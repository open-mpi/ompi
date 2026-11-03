/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef OPAL_MCA_BASE_VAR_GROUP_H
#define OPAL_MCA_BASE_VAR_GROUP_H

#include "opal/mca/mca.h"

struct mca_base_var_group_t {
    opal_list_item_t super;

    /** Index of group */
    int group_index;

    /** Group is valid (registered) */
    bool group_isvalid;

    /** Group name */
    char *group_full_name;

    char *group_project;
    char *group_framework;
    char *group_component;

    /** Group help message (description) */
    char *group_description;

    /** Integer value array of subgroup indices */
    opal_value_array_t group_subgroups;

    /** Integer array of group variables */
    opal_value_array_t group_vars;

    /** Integer array of group performance variables */
    opal_value_array_t group_pvars;

    /** Integer array of group MPI_T events */
    opal_value_array_t group_events;

    /** Pointer array of group enums */
    opal_value_array_t group_enums;
};

typedef struct mca_base_var_group_t mca_base_var_group_t;

/**
 * Object declaration for mca_base_var_group_t
 */
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_base_var_group_t);

/**
 * Register an MCA variable group
 *
 * @param[in] project_name Project name for this group.
 * @param[in] framework_name Framework name for this group.
 * @param[in] component_name Component name for this group.
 * @param[in] description Description of this group.
 *
 * @retval index Unique group index
 * @return opal error code on Error
 *
 * Create an MCA variable group. If the group already exists
 * this call is equivalent to mca_base_ver_find_group().
 */
OPAL_DECLSPEC int mca_base_var_group_register(const char *project_name, const char *framework_name,
                                              const char *component_name, const char *description);

/**
 * Register an MCA variable group for a component
 *
 * @param[in] component [in] Pointer to the component for which the
 * group is being registered.
 * @param[in] description Description of this group.
 *
 * @retval index Unique group index
 * @return opal error code on Error
 */
OPAL_DECLSPEC int mca_base_var_group_component_register(const mca_base_component_t *component,
                                                        const char *description);

/**
 * Deregister an MCA param group
 *
 * @param group_index [in] Group index from mca_base_var_group_register (),
 * mca_base_var_group_find().
 *
 * This call deregisters all associated variables and subgroups, and
 * invalidates all associated performance variables and events.  It does
 * not return while an MPI_T call is using any of the group's variables or
 * performance variables (see mca_base_var_deregister() and
 * mca_base_pvar_mark_invalid()).  That does not cover events: the MPI_T
 * event calls do not take the MPI_T lock (event callbacks must not run
 * while it is held), so an MPI_T event call may still be using one of
 * the group's events when this returns.
 */
OPAL_DECLSPEC int mca_base_var_group_deregister(int group_index);

/**
 * Invalidate the performance variables and events of an MCA param group
 *
 * @param group_index [in] Group index from mca_base_var_group_register (),
 * mca_base_var_group_find().
 *
 * This call invalidates the performance variables and events of the group
 * and its subgroups, as mca_base_var_group_deregister() does, but leaves
 * the group and its (control) variables registered.  It does not return
 * while an MPI_T call is using any of those performance variables, so the
 * caller may then free what their callbacks use.  As with
 * mca_base_var_group_deregister(), that does not cover events.
 */
OPAL_DECLSPEC int mca_base_var_group_invalidate(int group_index);

/**
 * Find an MCA group
 *
 * @param[in] project_name   Project name
 * @param[in] framework_name Framework name
 * @param[in] component_name Component name
 *
 * @returns OPAL_SUCCESS if found
 * @returns OPAL_ERR_NOT_FOUND if not found
 */
OPAL_DECLSPEC int mca_base_var_group_find(const char *project_name, const char *framework_name,
                                          const char *component_name);

/**
 * Find an MCA group by its full name
 *
 * @param[in]  full_name Full name of MCA variable group. Ex: shmem_mmap
 * @param[out] index     Index of group if found
 *
 * @returns OPAL_SUCCESS if found
 * @returns OPAL_ERR_NOT_FOUND if not found
 */
OPAL_DECLSPEC int mca_base_var_group_find_by_name(const char *full_name, int *index);

/**
 * Get the group at a specified index
 *
 * @param[in] group_index Group index
 * @param[out] group Storage for the group object pointer.
 *
 * @retval OPAL_ERR_NOT_FOUND If the group specified by group_index does not exist.
 * @retval OPAL_SUCCESS If the group is found
 *
 * The returned pointer belongs to the MCA variable system. Do not modify/release/retain
 * the pointer.
 */
OPAL_DECLSPEC int mca_base_var_group_get(const int group_index, const mca_base_var_group_t **group);

/**
 * Set/unset a flags for all variables in a group.
 *
 * @param[in] group_index Index of group
 * @param[in] flag Flag(s) to set or unset.
 * @param[in] set Boolean indicating whether to set flag(s).
 *
 * Set a flag for every variable in a group. See mca_base_var_set_flag() for more info.
 */
OPAL_DECLSPEC int mca_base_var_group_set_var_flag(const int group_index, int flags, bool set);

/**
 * Get the number of registered MCA groups
 *
 * @retval count Number of registered MCA groups
 */
OPAL_DECLSPEC int mca_base_var_group_get_count(void);

/**
 * Get a relative timestamp for the MCA group system
 *
 * @retval stamp
 *
 * This value will change if groups or variables are either added or removed.
 */
OPAL_DECLSPEC int mca_base_var_group_get_stamp(void);

#endif /* OPAL_MCA_BASE_VAR_GROUP_H */
