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
 * Copyright (c) 2010-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
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

static mca_base_var_enum_value_t memory_kinds[] = {
  {.value = MEMKIND_PARTITION_DEFAULT, .string = "memkind_default"},
  {.value = MEMKIND_PARTITION_HBW, .string = "memkind_hbw"},
  {.value = MEMKIND_PARTITION_HBW_HUGETLB, .string = "memkind_hwb_hugetlb"},
  {.value = MEMKIND_PARTITION_HBW_PREFERRED, .string = "memkind_hbw_preferred"},
  {.value = MEMKIND_PARTITION_HBW_PREFERRED_HUGETLB, .string = "memkind_hbw_preferred_hugetlb"},
  {.value = MEMKIND_PARTITION_HUGETLB, .string = "memkind_hugetlb"},
  {.value = MEMKIND_PARTITION_HBW_GBTLB, .string = "memkind_hbw_gbtlb"},
  {.value = MEMKIND_PARTITION_HBW_PREFERRED_GBTLB, .string = "memkind_hbw_preferred_gbtlb"},
  {.value = MEMKIND_PARTITION_GBTLB, .string = "memkind_gbtlb"},
  {.value = MEMKIND_PARTITION_HBW_INTERLEAVE, .string = "memkind_hbw_interleave"},
  {.value = MEMKIND_PARTITION_INTERLEAVE, .string = "memkind_interleave"},
  {.string = NULL},
};

static mca_base_var_enum_t *mca_mpool_memkind_enum = NULL;

static int opal_mpool_memkind_verbose;
static int mca_mpool_memkind_register(void)
{
    int rc;

    /* register MEMKIND component parameters */
    mca_mpool_memkind_component.default_partition = memory_kinds[0].value;

    rc = mca_base_var_enum_create ("memkind partition types", memory_kinds, &mca_mpool_memkind_enum);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    (void) mca_base_component_var_register(&mca_mpool_memkind_component.super.mpool_version,
                                           "default_partition", "Default memkind partition to use",
                                           MCA_BASE_VAR_TYPE_INT, mca_mpool_memkind_enum, 0, 0,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_LOCAL,
                                           &mca_mpool_memkind_component.default_partition);

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
    memkind_t default_kind;
    int rc;

    if (opal_mpool_memkind_verbose != 0) {
        mca_mpool_memkind_component.output = opal_output_open(NULL);
    } else {
        mca_mpool_memkind_component.output = -1;
    }

    rc = memkind_get_kind_by_partition (mca_mpool_memkind_component.default_partition,
                                        &default_kind);
    if (0 != rc) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    if (memkind_check_available (default_kind)) {
        char *kind_string;

        mca_mpool_memkind_enum->string_from_value (mca_mpool_memkind_enum,
                                                   mca_mpool_memkind_component.default_partition,
                                                   &kind_string);
        opal_output_verbose (MCA_BASE_VERBOSE_WARN, mca_mpool_memkind_component.output,
                             "default kind %s not available", kind_string);
        free (kind_string);
        return OPAL_ERR_NOT_AVAILABLE;
    }

    for (int i = 0 ; i < MEMKIND_NUM_BASE_KIND ; ++i) {
        mca_mpool_memkind_module_init (mca_mpool_memkind_component.modules + i, i);
    }

    return OPAL_SUCCESS;
}

static int mca_mpool_memkind_close(void)
{
    opal_output_close (mca_mpool_memkind_component.output);
    mca_mpool_memkind_component.output = -1;

    if (mca_mpool_memkind_enum) {
        OBJ_RELEASE(mca_mpool_memkind_enum);
        mca_mpool_memkind_enum = NULL;
    }

    return OPAL_SUCCESS;
}

static int mca_mpool_memkind_query (const char *hints, int *priority_out,
                                    mca_mpool_base_module_t **module)
{
    int my_priority = mca_mpool_memkind_component.priority;
    char **hint_array, *partition_name;
    int partition = -1, rc;

    if (module) {
        *module = &mca_mpool_memkind_component.modules[mca_mpool_memkind_component.default_partition].super;
    }

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
        char *tmp, *key, *value;

        key = hint_array[i];
        tmp = strchr (key, '=');
        if (tmp) {
            *tmp = '\0';
            value = tmp + 1;
        }

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
        } else if (0 == strcasecmp (key, "partition")) {
            rc = mca_mpool_memkind_enum->value_from_string (mca_mpool_memkind_enum,
                                                            value, &partition);
            if (OPAL_SUCCESS != rc) {
                opal_output_verbose (MCA_BASE_VERBOSE_WARN, mca_mpool_memkind_component.output,
                                     "invalid partition %s specified", value);
            }

            partition_name = value;
        }
    }

    if (-1 != partition) {
        memkind_t kind;

        my_priority = 0;

        if (!memkind_get_kind_by_partition (partition, &kind)) {
            if (memkind_check_available (kind)) {
                opal_output_verbose (MCA_BASE_VERBOSE_WARN, mca_mpool_memkind_component.output,
                                     "kind %s not available", partition_name);
            } else {
                my_priority = 100;
            }
        }

        if (module) {
            *module = &mca_mpool_memkind_component.modules[partition].super;
        }
    }

    opal_argv_free (hint_array);

    if (priority_out) {
        *priority_out = my_priority;
    }

    return OPAL_SUCCESS;
}
