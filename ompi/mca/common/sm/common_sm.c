/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "ompi/constants.h"
#include "orte/util/show_help.h"
#include "opal/util/argv.h"

#include "common_sm_mmap.h"
#if MCA_COMMON_SM_SYSV
#include "common_sm_sysv.h"
#endif /* MCA_COMMON_SM_SYSV */
#if MCA_COMMON_SM_WINDOWS
#include "common_sm_windows.h"
#endif /* MCA_COMMON_SM_WINDOWS */

static int initialized                             = 0;
static int sysv_index                              = -1;
static char **sm_argv                              = NULL;
/* let mmap be the default selection */
static char *sm_params                             = "mmap";
static mca_common_sm_init_fn_t sm_init             = NULL;
static mca_common_sm_init_group_fn_t sm_init_group = NULL;
static mca_common_sm_seg_alloc_fn_t sm_seg_alloc   = NULL;
static mca_common_sm_fini_fn_t sm_fini             = NULL;
static char sm_all_buff[OPAL_PATH_MAX];

mca_common_sm_module_t *mca_common_sm_module       = NULL;

/******************************************************************************/
int
mca_common_sm_param_register(mca_base_component_t *c)
{
    char sm_avail_help_str[OPAL_PATH_MAX];

    if (-1 == sysv_index)
    {
        if (MCA_COMMON_SM_SYSV)
        {
            snprintf(
                sm_avail_help_str, 
                sizeof(sm_avail_help_str) - 1,
                "Which shared memory support will be used. "
                "Valid values: sysv,mmap - or a comma delimited "
                "combination of them (order dependent).  The first component "
                "that is successfully selected is used."
            );
            /**
             * construct a comma-separated list of valid options for "all".
             * notice that we are going to try sysv first.
             */
            snprintf(sm_all_buff, sizeof(sm_all_buff) - 1, "sysv,mmap");
        }
        else /* only mmap is available */
        {
            snprintf(
                sm_avail_help_str, 
                sizeof(sm_avail_help_str) - 1,
                "Which shared memory support will be used. "
                "Valid values: mmap."
            );
            snprintf(sm_all_buff, sizeof(sm_all_buff) - 1, "mmap");
        }

        mca_base_param_reg_string_name("mpi",
                                       "common_sm",
                                       sm_avail_help_str,
                                       false,
                                       false,
                                       sm_params,
                                       &sm_params);

        /* empty == try all available */
        if (0 == strcmp(sm_params, ""))
        {
            if (NULL == (sm_argv = opal_argv_split(sm_all_buff, ',')))
            {
                opal_output(0,
                            "WARNING: could not parse mpi_common_sm request.");
            }
        }
        else
        {
            if (NULL == (sm_argv = opal_argv_split(sm_params, ',')))
            {
                opal_output(0,
                            "WARNING: could not parse mpi_common_sm request.");
            }
        }

        sysv_index = mca_base_param_reg_int_name(
                         "mpi",
                         "common_sm_have_sysv_support",
                         "Whether shared memory has System V support or not",
                         false,
                         true,
                         MCA_COMMON_SM_SYSV,
                         NULL
                     );
    }

    /* Also register MCA param synonyms for the component */
    mca_base_param_reg_syn(sysv_index, c, "have_sysv_support", false);

    return OMPI_SUCCESS;
}

/******************************************************************************/
mca_common_sm_module_t * 
mca_common_sm_init(ompi_proc_t **procs,
                   size_t num_procs,
                   size_t size, 
                   char *file_name,
                   size_t size_ctl_structure, 
                   size_t data_seg_alignment)
{
    if (!initialized)
    {
        int help_msg_displayed = 0;
        int i;

        if (NULL != sm_argv)
        {
            /**
             * iterate through the entire list
             * stop when a valid component has been selected.
             *
             * warn the user when an invalid option was specified,
             * but continue searching for a valid alternative.
             */
            for (i = 0; NULL != sm_argv[i] && NULL == sm_init; ++i)
            {
                if (0 == strcasecmp(sm_argv[i], "mmap"))
                {
#if !MCA_COMMON_SM_WINDOWS
                    sm_init = mca_common_sm_mmap_init;
                    sm_init_group = mca_common_sm_mmap_init_group;
                    sm_seg_alloc = mca_common_sm_mmap_seg_alloc;
                    sm_fini = mca_common_sm_mmap_fini;
#else /* MCA_COMMON_SM_WINDOWS */
                    sm_init = mca_common_sm_windows_init;
                    sm_init_group = mca_common_sm_windows_init_group;
                    sm_seg_alloc = mca_common_sm_windows_seg_alloc;
                    sm_fini = mca_common_sm_windows_fini;
#endif
                }
                else if (0 == strcasecmp(sm_argv[i], "sysv"))
                {
#if !MCA_COMMON_SM_SYSV
                    if (!help_msg_displayed)
                    {
                        orte_show_help("help-mpi-common-sm.txt",
                                       "sm support",
                                       1,
                                       sm_argv[i]);
                        help_msg_displayed = 1;
                    }
#else /* MCA_COMMON_SM_SYSV */
                    /* make sure that we can safely use sysv on this system */
                    if (OMPI_SUCCESS == mca_common_sm_sysv_component_query())
                    {
                        sm_init = mca_common_sm_sysv_init;
                        sm_init_group = mca_common_sm_sysv_init_group;
                        sm_seg_alloc = mca_common_sm_sysv_seg_alloc;
                        sm_fini = mca_common_sm_sysv_fini;
                    }
                    else /* let the user know that we tried sysv and failed */
                    {
                        orte_show_help("help-mpi-common-sm.txt",
                                       "sysv rt test fail",
                                       1);
                    }
#endif
                }
                else /* unknown value */
                {
                    if (!help_msg_displayed)
                    {
                        orte_show_help("help-mpi-common-sm.txt",
                                       "sm support",
                                       1,
                                       sm_argv[i]);
                        help_msg_displayed = 1;
                    }
                }
            }
            if (NULL != sm_argv)
            {
                opal_argv_free(sm_argv);
            }
        }
        initialized = 1;
    }

    /* call the selected init function */
    if (NULL != sm_init)
    {
        return sm_init(procs, num_procs, size,
                       file_name, size_ctl_structure,
                       data_seg_alignment);
    }
    return NULL;
}

/******************************************************************************/
mca_common_sm_module_t *
mca_common_sm_init_group(ompi_group_t *group,
                         size_t size, 
                         char *file_name,
                         size_t size_ctl_structure, 
                         size_t data_seg_alignment)
{
    if (NULL != sm_init_group) 
    {
        return sm_init_group(group, size,
                             file_name, size_ctl_structure,
                             data_seg_alignment);
    }
    return NULL;
}

/******************************************************************************/
void *
mca_common_sm_seg_alloc(struct mca_mpool_base_module_t* mpool, 
                        size_t* size, 
                        mca_mpool_base_registration_t** registration)
{
    if (NULL != sm_seg_alloc) 
    {
        return sm_seg_alloc(mpool, size, registration);
    }
    return NULL;
}

/******************************************************************************/
int 
mca_common_sm_fini(mca_common_sm_module_t *mca_common_sm_module)
{
    if (NULL != sm_fini) 
    {
        return sm_fini(mca_common_sm_module);
    }
    return OMPI_ERR_NOT_FOUND;
}

