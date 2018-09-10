/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H*/
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#include <errno.h>
#include <memkind.h>
#include "opal/mca/base/base.h"
#include "opal/mca/allocator/base/base.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/util/argv.h"
#include "mpool_memkind.h"


/*
 * Local functions
 */

static int
mca_mpool_memkind_register(void);

static int
mca_mpool_memkind_open(void);

static int
mca_mpool_memkind_close(void);

static int mca_mpool_memkind_query (const char *hints, int *priority,
                                    mca_mpool_base_module_t **module);

mca_mpool_memkind_component_t mca_mpool_memkind_component = {
    {
        /* First, the mca_base_component_t struct containing meta
         information about the component itself */
        .mpool_version = {
            MCA_MPOOL_BASE_VERSION_3_0_0,
            "memkind", /* MCA component name */
             MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                   OPAL_RELEASE_VERSION),
            .mca_open_component = mca_mpool_memkind_open,
            .mca_close_component = mca_mpool_memkind_close,
            .mca_register_component_params = mca_mpool_memkind_register
        },
        .mpool_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        .mpool_query = mca_mpool_memkind_query,
    }
};

static void mca_mpool_memkind_module_le_destroy(mca_mpool_memkind_module_le_t *elem)
{
    if (NULL != elem->module.kind) {
        memkind_destroy_kind(elem->module.kind);
    }
}

OBJ_CLASS_INSTANCE(mca_mpool_memkind_module_le_t,
                   opal_list_item_t,
                   NULL,
                   mca_mpool_memkind_module_le_destroy);

static mca_base_var_enum_value_t memory_types[] = {
  {.value = MEMKIND_MEMTYPE_DEFAULT, .string = "memkind_default"},
  {.value = MEMKIND_MEMTYPE_HIGH_BANDWIDTH, .string = "memkind_hbw"},
  {.string = NULL},
};

static mca_base_var_enum_value_t memory_policy[] = {
  {.value = MEMKIND_POLICY_BIND_LOCAL, .string = "mempolicy_bind_local"},
  {.value = MEMKIND_POLICY_BIND_ALL, .string = "mempolicy_bind_all"},
  {.value = MEMKIND_POLICY_PREFERRED_LOCAL, .string = "mempolicy_perferred_local"},
  {.value = MEMKIND_POLICY_INTERLEAVE_LOCAL, .string = "mempolicy_interleave_local"},
  {.value = MEMKIND_POLICY_INTERLEAVE_ALL, .string = "mempolicy_interleave_all"},
  {.string = NULL},
};

static mca_base_var_enum_value_t memory_kind_bits[] = {
  {.value = 0, .string = "memkind_mask_page_size_4KB"},
  {.value = MEMKIND_MASK_PAGE_SIZE_2MB, .string = "memkind_mask_page_size_2MB"},
  {.string = NULL},
};

static mca_base_var_enum_t *mca_mpool_memkind_policy_enum = NULL;
static mca_base_var_enum_t *mca_mpool_memkind_type_enum = NULL;
static mca_base_var_enum_t *mca_mpool_memkind_kind_bits_enum = NULL;

static int opal_mpool_memkind_verbose;
static int mca_mpool_memkind_register(void)
{
    int rc;

    /* register MEMKIND component parameters */

    mca_mpool_memkind_component.default_type = memory_types[0].value;

    rc = mca_base_var_enum_create ("memkind memory types", memory_types,
                                   &mca_mpool_memkind_type_enum);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "default_type", "Default memkind type to use",
                                           MCA_BASE_VAR_TYPE_INT, mca_mpool_memkind_type_enum, 0, 0,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_mpool_memkind_component.default_type);

    /*
     * see memkind source to understand the 2 
     */
    mca_mpool_memkind_component.default_policy = memory_policy[2].value;

    rc = mca_base_var_enum_create ("memkind memory policy", memory_policy, 
                                   &mca_mpool_memkind_policy_enum);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "default_policy", "Default memkind policy to use",
                                           MCA_BASE_VAR_TYPE_INT, mca_mpool_memkind_policy_enum, 0, 0,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_mpool_memkind_component.default_policy);

    mca_mpool_memkind_component.default_memkind_bits = memory_kind_bits[0].value;

    rc = mca_base_var_enum_create ("memkind memory bits", memory_kind_bits, 
                                   &mca_mpool_memkind_kind_bits_enum);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "default_bits", "Default memkind bits to use",
                                           MCA_BASE_VAR_TYPE_INT, mca_mpool_memkind_kind_bits_enum, 0, 0,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_mpool_memkind_component.default_memkind_bits);

    mca_mpool_memkind_component.priority = 10;
    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "priority", "Default priority of the memkind component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_mpool_memkind_component.priority);

    opal_mpool_memkind_verbose = 0;
    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "verbose", "Verbosity of the memkind mpool component",
                                           MCA_BASE_VAR_TYPE_INT, &mca_base_var_enum_verbose, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &opal_mpool_memkind_verbose);

    return OPAL_SUCCESS;
}

/**
  * component open/close/init function
  */
static int mca_mpool_memkind_open (void)
{
    int rc;
    mca_mpool_memkind_module_le_t *item = NULL;

    if (opal_mpool_memkind_verbose != 0) {
        mca_mpool_memkind_component.output = opal_output_open(NULL);
    } else {
        mca_mpool_memkind_component.output = -1;
    }

    OBJ_CONSTRUCT(&mca_mpool_memkind_component.module_list, opal_list_t);

    rc = memkind_create_kind(mca_mpool_memkind_component.default_type,
                             mca_mpool_memkind_component.default_policy,
                             mca_mpool_memkind_component.default_memkind_bits,
                             &mca_mpool_memkind_component.default_kind);
    if (MEMKIND_SUCCESS != rc) {
        opal_output_verbose (MCA_BASE_VERBOSE_WARN, opal_mpool_base_framework.framework_output,
                                     "memkind_create_kind default returned %d", rc);
        return OPAL_ERR_NOT_AVAILABLE;
    }

    item = OBJ_NEW(mca_mpool_memkind_module_le_t);

    item->module.type =  mca_mpool_memkind_component.default_type;
    item->module.policy =  mca_mpool_memkind_component.default_policy;
    item->module.memkind_bits = mca_mpool_memkind_component.default_memkind_bits;
    item->module.kind = mca_mpool_memkind_component.default_kind;
    /*
     * ufff, magic mask - see memkind.h in the memkind package
     */
    if (MEMKIND_MASK_PAGE_SIZE_2MB == (item->module.memkind_bits & 0x7F)) {
        item->module.page_size = 2097152;
    } else {
        item->module.page_size = 4096;
    }

    opal_list_append(&mca_mpool_memkind_component.module_list,
                    (opal_list_item_t *)item);
 

    return OPAL_SUCCESS;
}

static int mca_mpool_memkind_close(void)
{
    opal_output_close (mca_mpool_memkind_component.output);
    mca_mpool_memkind_component.output = -1;

    OPAL_LIST_DESTRUCT(&mca_mpool_memkind_component.module_list);

    if (mca_mpool_memkind_policy_enum) {
        OBJ_RELEASE(mca_mpool_memkind_policy_enum);
        mca_mpool_memkind_policy_enum = NULL;
    }

    if (mca_mpool_memkind_type_enum) {
        OBJ_RELEASE(mca_mpool_memkind_type_enum);
        mca_mpool_memkind_type_enum = NULL;
    }

    if (mca_mpool_memkind_kind_bits_enum) {
        OBJ_RELEASE(mca_mpool_memkind_kind_bits_enum);
        mca_mpool_memkind_kind_bits_enum = NULL;
    }

    return OPAL_SUCCESS;
}

static int mca_mpool_memkind_query (const char *hints, int *priority_out,
                                    mca_mpool_base_module_t **module)
{
    int my_priority = mca_mpool_memkind_component.priority;
    char **hint_array;
    char *tmp, *key, *value = NULL;
    int rc;
    memkind_memtype_t type = mca_mpool_memkind_component.default_type;
    memkind_policy_t  policy = mca_mpool_memkind_component.default_policy;
    memkind_bits_t    memkind_bits =  mca_mpool_memkind_component.default_memkind_bits;
    mca_mpool_memkind_module_le_t *item = NULL;
    mca_mpool_base_module_t *found_module = NULL;
    memkind_t kind;

    if (NULL == hints) {
        if (priority_out) {
            *priority_out = my_priority;
        }
        return OPAL_SUCCESS;
    }

    hint_array = opal_argv_split (hints, ',');
    if (NULL == hint_array) {
        if (priority_out) {
            *priority_out = my_priority;
        }
        return OPAL_SUCCESS;
    }

    for (int i = 0 ; hint_array[i] ; ++i) {

        key = hint_array[i];
        tmp = strchr (key, '=');
        if (tmp) {
            *tmp = '\0';
            value = tmp + 1;
        }

        /*
         * TODO: may want to emit a warning
         */
        if (value == NULL) break;

        if (0 == strcasecmp (key, "mpool")) {
            if (0 == strcasecmp (value, "memkind")) {
                /* specifically selected */

                my_priority = 100;
            } else {
                if (priority_out) {
                    *priority_out = 0;
                }
                return OPAL_SUCCESS;
            }
        } else if (0 == strcasecmp (key, "policy")) {

            rc = mca_mpool_memkind_policy_enum->value_from_string (mca_mpool_memkind_policy_enum,
                                                                   value, (int *)&policy);
            if (OPAL_SUCCESS != rc) {
                opal_output_verbose (MCA_BASE_VERBOSE_WARN, opal_mpool_base_framework.framework_output,
                                     "invalid memkind policy %s specified", value);
            }

        } else if (0 == strcasecmp (key, "type")) {

            rc = mca_mpool_memkind_type_enum->value_from_string (mca_mpool_memkind_type_enum,
                                                                 value, (int *)&type);
            if (OPAL_SUCCESS != rc) {
                opal_output_verbose (MCA_BASE_VERBOSE_WARN, opal_mpool_base_framework.framework_output,
                                     "invalid memkind type %s specified", value);
            }

        } else if (0 == strcasecmp (key, "kind_bits")) {

            rc = mca_mpool_memkind_kind_bits_enum->value_from_string (mca_mpool_memkind_kind_bits_enum,
                                                                      value, (int *)&memkind_bits);
            if (OPAL_SUCCESS != rc) {
                opal_output_verbose (MCA_BASE_VERBOSE_WARN, opal_mpool_base_framework.framework_output,
                                     "invalid memkind kind_bits %s specified", value);
            }
        }
    }

    /*
     * now look for an existing module with matching policy, type, memkind bits
     */

    OPAL_LIST_FOREACH(item, &mca_mpool_memkind_component.module_list, 
                      mca_mpool_memkind_module_le_t) {
        if ((item->module.type == type) &&
            (item->module.policy == policy) &&
            (item->module.memkind_bits = memkind_bits)) {
               found_module = &item->module.super;
               break;
        }
    } 

    /*
     * didn't find a matching module, try to create one 
     */

    if (NULL == found_module) {
        rc = memkind_create_kind(type, policy, memkind_bits, &kind);
        if (MEMKIND_SUCCESS == rc) {

            item = OBJ_NEW(mca_mpool_memkind_module_le_t);

            item->module.type =  type;
            item->module.policy =  policy;
            item->module.memkind_bits = memkind_bits;
            item->module.kind = kind;

            if (MEMKIND_MASK_PAGE_SIZE_2MB == item->module.memkind_bits) {
                item->module.page_size = 2097152;
            } else {
                item->module.page_size = 4096;
            }

            mca_mpool_memkind_module_init(&item->module);

            opal_list_append(&mca_mpool_memkind_component.module_list,
                            (opal_list_item_t *)item);
            found_module = &item->module.super;

        } else {
            opal_output_verbose (MCA_BASE_VERBOSE_WARN, opal_mpool_base_framework.framework_output,
                                     "memkind_create_kind returned %d", rc);
            if (priority_out) {
                *priority_out = 0;
            }
            return OPAL_SUCCESS;
        }
    }

    if ((found_module) && (NULL != module)) {
        *module = found_module;
    }

    opal_argv_free (hint_array);

    if (priority_out) {
        *priority_out = my_priority;
    }

    return OPAL_SUCCESS;
}
