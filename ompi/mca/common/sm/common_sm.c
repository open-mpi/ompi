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

#include "opal/util/argv.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "ompi/constants.h"
#include "ompi/mca/dpm/dpm.h"

#include "common_sm_rml.h"
#include "common_sm_mmap.h"
#if MCA_COMMON_SM_SYSV
#include "common_sm_sysv.h"
#endif /* MCA_COMMON_SM_SYSV */
#if MCA_COMMON_SM_WINDOWS
#include "common_sm_windows.h"
#endif /* MCA_COMMON_SM_WINDOWS */
#if MCA_COMMON_SM_POSIX
#include "common_sm_posix.h"
#endif /* MCA_COMMON_SM_POSIX */

/**
 * ASSUMING local proc homogeneity with respect to all utilized shared memory
 * facilities. that is, if one local proc deems a particular shared memory
 * facility acceptable, then ALL local procs should be able to utilize that
 * facility. as it stands, this is an important point because one process
 * dictates to all other local procs which common sm component will be selected
 * based on its own, local run-time test.
 */

static bool initialized                          = false;
static int sysv_index                            = -1;
static int posix_index                           = -1;
static int common_sm_index                       = -1;
static char **sm_argv                            = NULL;
static char *sm_params                           = NULL;
static mca_common_sm_init_fn_t sm_init           = NULL;
static mca_common_sm_seg_alloc_fn_t sm_seg_alloc = NULL;
static mca_common_sm_fini_fn_t sm_fini           = NULL;
/* should be more than enough to store all common sm component names */
static char sm_default[32];
/* holds common sm help string */
char sm_avail_help_str[OPAL_PATH_MAX];

/**
 * lock to protect multiple instances of query_sm_components()
 * from being invoked simultaneously (because of rml usage).
 */
static opal_mutex_t mutex;

/* common shared memory component information */
typedef struct
{
    /* flag indicating whether or not the component is available */
    bool avail;
    /* component name */
    char *sm_name;
} mca_common_sm_info_t;
/**
 * NOTE:
 * o array position dictates the default order in which
 *   the common shared memory components will be queried.
 * o first component successfully queried gets selected.
 * o sm_name format: {component availability, "component name,"}
 *
 * if you change the order of sm_avail_table below,
 * don't forget to update mca_common_sm_comp_index_map_t.
 *
 * placing mmap before sysv in the list prevents sysv from ever being selected
 * (in the default case). this is because, at least for now, mmap's selection
 * query always succeeds. that is, sysv must be explicitly requested.
 * NOTE: mmap is the default for now.
 *
 * {component availability, component name}
 */
static const mca_common_sm_info_t sm_avail_table[] =
{
    {true                     , "mmap," }, /* assume mmap is always available */
    {(bool)MCA_COMMON_SM_POSIX, "posix,"},
    {(bool)MCA_COMMON_SM_SYSV , "sysv," },
    {false                    , NULL    }  /* MUST BE LAST ITEM */
};
/* component index enum */
typedef enum
{
    MCA_COMMON_SM_COMP_INDEX_MMAP = 0,
    MCA_COMMON_SM_COMP_INDEX_POSIX,
    MCA_COMMON_SM_COMP_INDEX_SYSV,
    MCA_COMMON_SM_COMP_INDEX_NONE /* MUST BE LAST ITEM */
} mca_common_sm_comp_index_map_t;

/**
 * list of RML messages that have arrived that have not yet been
 * consumed by the thread who is looking to complete its component
 * initialization based on the contents of the RML message.
 */
static opal_list_t pending_rml_msgs;

/******************************************************************************/
                         /* STATIC UTILITY FUNCTIONS */
/******************************************************************************/

/******************************************************************************/
/**
 * this routine selects the common sm component that corresponds to
 * sm_component_index's value.
 *
 * @param sm_component_index index corresponding to the common sm component that
 *                           is to be selected. (IN)
 */
static void
select_common_sm_component(int sm_component_index)
{
    switch (sm_component_index)
    {
#if MCA_COMMON_SM_POSIX
        case MCA_COMMON_SM_COMP_INDEX_POSIX:
            sm_init = mca_common_sm_posix_init;
            sm_seg_alloc = mca_common_sm_posix_seg_alloc;
            sm_fini = mca_common_sm_posix_fini;
            break;
#endif
        case MCA_COMMON_SM_COMP_INDEX_MMAP:
#if !MCA_COMMON_SM_WINDOWS
            sm_init = mca_common_sm_mmap_init;
            sm_seg_alloc = mca_common_sm_mmap_seg_alloc;
            sm_fini = mca_common_sm_mmap_fini;
#else /* MCA_COMMON_SM_WINDOWS */
            sm_init = mca_common_sm_windows_init;
            sm_seg_alloc = mca_common_sm_windows_seg_alloc;
            sm_fini = mca_common_sm_windows_fini;
#endif
            break;
#if MCA_COMMON_SM_SYSV
        case MCA_COMMON_SM_COMP_INDEX_SYSV:
            sm_init = mca_common_sm_sysv_init;
            sm_seg_alloc = mca_common_sm_sysv_seg_alloc;
            sm_fini = mca_common_sm_sysv_fini;
            break;
#endif
        case MCA_COMMON_SM_COMP_INDEX_NONE:
            sm_init = NULL;
            sm_seg_alloc = NULL;
            sm_fini = NULL;
            break;
        default:
            sm_init = NULL;
            sm_seg_alloc = NULL;
            sm_fini = NULL;
            opal_output(0, "WARNING: invalid common sm component index.");
            break;
    }
}

/******************************************************************************/
/**
 * this routine performs a series of run-time tests that determines whether or
 * not a particular common sm component can be selected safely. once a component
 * is successfully selected, its component index is returned.
 *
 * @return index corresponding to the selected common sm component.  see
 * mca_common_sm_comp_index_map_t for valid values.
 */
static int
query_sm_components(void)
{
    int help_msg_displayed = 0;
    int sm_component_index = MCA_COMMON_SM_COMP_INDEX_NONE;
    int i;

    if (NULL != sm_argv)
    {
        MCA_COMMON_SM_OUTPUT_VERBOSE("looking for available components");
        for (i = 0; NULL != sm_argv[i]; ++i)
        {
            if (0 == strcasecmp(sm_argv[i], "posix"))
            {
#if !MCA_COMMON_SM_POSIX
                if (!help_msg_displayed)
                {
                    orte_show_help("help-mpi-common-sm.txt",
                                   "sm support",
                                   1,
                                   sm_argv[i]);
                    help_msg_displayed = 1;
                }
#else /* MCA_COMMON_SM_POSIX */
                MCA_COMMON_SM_OUTPUT_VERBOSE("querying posix");
                /**
                 * make sure that we can safely use posix sm on this system
                 */
                if (OMPI_SUCCESS ==
                    mca_common_sm_posix_component_query())
                {
                    MCA_COMMON_SM_OUTPUT_VERBOSE("selecting posix");
                    sm_component_index = MCA_COMMON_SM_COMP_INDEX_POSIX;
                    break;
                }
                else /* let the user know that we tried posix and failed */
                {
                    MCA_COMMON_SM_OUTPUT_VERBOSE("cannot select posix");
                    orte_show_help("help-mpi-common-sm.txt",
                                   "sm rt test fail",
                                   1,
                                   "Posix");
                }
#endif
            }
            else if (0 == strcasecmp(sm_argv[i], "mmap"))
            {
                MCA_COMMON_SM_OUTPUT_VERBOSE("selecting mmap");
                /* there is no run-time test for mmap, so just select it */
                sm_component_index = MCA_COMMON_SM_COMP_INDEX_MMAP;
                break;
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
                MCA_COMMON_SM_OUTPUT_VERBOSE("querying sysv");
                /* make sure that we can safely use sysv on this system */
                if (OMPI_SUCCESS == mca_common_sm_sysv_component_query())
                {
                    MCA_COMMON_SM_OUTPUT_VERBOSE("selecting sysv");
                    sm_component_index = MCA_COMMON_SM_COMP_INDEX_SYSV;
                    break;
                }
                else /* let the user know that we tried sysv and failed */
                {
                    MCA_COMMON_SM_OUTPUT_VERBOSE("cannot select sysv");
                    orte_show_help("help-mpi-common-sm.txt",
                                   "sm rt test fail",
                                   1,
                                   "System V");
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
    }

    if (MCA_COMMON_SM_COMP_INDEX_NONE == sm_component_index)
    {
        MCA_COMMON_SM_OUTPUT_VERBOSE("no component selected");
    }

    return sm_component_index;
}

/******************************************************************************/
int
mca_common_sm_param_register(mca_base_component_t *c)
{
    /* also using sysv_index's value as an initialization flag */
    if (-1 == sysv_index)
    {
        int i;
        char *last_char;

        memset(sm_default, '\0', sizeof(sm_default));

        /* populate sm_default with all available common sm component names */
        for (i = 0; NULL != sm_avail_table[i].sm_name; ++i)
        {
            if (sm_avail_table[i].avail)
            {
                strncat(sm_default,
                        sm_avail_table[i].sm_name,
                        sizeof(sm_default) - 1);
            }
        }
        /* remove the last comma from the char buff */
        if (NULL != (last_char = strrchr(sm_default, ',')))
        {
            *last_char = '\0';
        }
        /* set up help string */
        snprintf(
            sm_avail_help_str,
            sizeof(sm_avail_help_str) - 1,
            "Which shared memory support will be used. Valid values: (%s)%s",
            sm_default,
            (i > 1) ? " - or a comma delimited combination of them "
            "(order dependent).  The first component that is successfully "
            "selected is used." : "."
        );
        sysv_index = mca_base_param_reg_int_name(
                         "mpi",
                         "common_sm_have_sysv_support",
                         "Whether shared memory has System V support or not",
                         false,
                         true,
                         MCA_COMMON_SM_SYSV,
                         NULL
                     );
        posix_index = mca_base_param_reg_int_name(
                          "mpi",
                          "common_sm_have_posix_support",
                          "Whether shared memory has POSIX support or not",
                          false,
                          true,
                          MCA_COMMON_SM_POSIX,
                          NULL
                     );
    }

    /* register mpi_common_sm */
    common_sm_index = mca_base_param_reg_string_name("mpi",
                                                     "common_sm",
                                                     sm_avail_help_str,
                                                     false,
                                                     false,
                                                     /* default value */
                                                     sm_default,
                                                     &sm_params);
    /* also register MCA param synonyms for the component */
    mca_base_param_reg_syn(sysv_index, c, "have_sysv_support", false);
    mca_base_param_reg_syn(posix_index, c, "have_posix_support", false);
    mca_base_param_reg_syn(common_sm_index, c, "store", false);

    if (OPAL_SUCCESS != mca_base_param_lookup_string(common_sm_index,
                                                     &sm_params))
    {
        return OMPI_ERROR;
    }

    /* empty string == try all available */
    if (0 == strcmp(sm_params, ""))
    {
        if (NULL == (sm_argv = opal_argv_split(sm_default, ',')))
        {
            opal_output(0,
                        "WARNING: could not parse mpi_common_sm request.");
        }
    }
    /* try what the user specified */
    else
    {
        if (NULL == (sm_argv = opal_argv_split(sm_params, ',')))
        {
            opal_output(0,
                        "WARNING: could not parse mpi_common_sm request.");
        }
    }

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
    size_t num_local_procs = 0;
    bool found_lowest      = false;
    bool lowest;
    size_t p;
    ompi_proc_t *temp_proc;

    /**
     * NOTE: the selected component's init routine, unlike mca_common_sm_init,
     * must be provided with:
     *     o a SORTED procs array
     *     o the number of LOCAL processes within procs array
     *
     * so always do the following before calling sm_init:
     *     o reorder procs array to have all the local procs at the beginning.
     *     o look for the local proc with the lowest name.
     *     o determine the number of local procs.
     *     o ensure that procs[0] is the lowest named process.
     */
    for (p = 0; p < num_procs; ++p)
    {
        if (OPAL_PROC_ON_LOCAL_NODE(procs[p]->proc_flags))
        {
            /* if we don't have a lowest, save the first one */
            if (!found_lowest)
            {
                procs[0] = procs[p];
                found_lowest = true;
            }
            else
            {
                /* save this proc */
                procs[num_local_procs] = procs[p];
                /**
                 * if we have a new lowest, swap it with position 0
                 * so that procs[0] is always the lowest named proc
                 */
                if (orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                  &(procs[p]->proc_name),
                                                  &(procs[0]->proc_name)) < 0)
                {
                    temp_proc = procs[0];
                    procs[0] = procs[p];
                    procs[num_local_procs] = temp_proc;
                }
            }
            /**
             * regardless of the comparisons above, we found
             * another proc on the local node, so increment
             */
            ++num_local_procs;
        }
    }

    /* if there is less than 2 local processes, there's nothing to do. */
    if (num_local_procs < 2)
    {
        return NULL;
    }

    if (!initialized)
    {
        mca_common_sm_rml_sm_info_t sm_info;
        sm_info.id = MCA_COMMON_SM_COMP_INDEX_NONE;
        memset(sm_info.posix_fname_buff,
               '\0',
               OMPI_COMMON_SM_POSIX_FILE_LEN_MAX);

        lowest = (0 == orte_util_compare_name_fields(
                           ORTE_NS_CMP_ALL,
                           ORTE_PROC_MY_NAME,
                           &(procs[0]->proc_name)));

        /**
         * lock here to prevent multiple threads from invoking this function
         * simultaneously.  the critical section we're protecting is usage of
         * the RML in this block.
         */
        opal_mutex_lock(&mutex);

        OBJ_CONSTRUCT(&(pending_rml_msgs), opal_list_t);

        /**
         * figure out if i am the lowest proc in the group.
         * if i am, select a common sm component and send its index to the rest
         * of the local procs so they can select the same common sm component.
         */
        if (lowest)
        {
            /* get the component index */
            sm_info.id = query_sm_components();
        }
        /* no return code check here because the error
         * path is the same as the expected path */
        mca_common_sm_rml_info_bcast(&sm_info,
                                     procs,
                                     num_local_procs,
                                     OMPI_RML_TAG_COMMON_SM_COMP_INDEX,
                                     lowest,
                                     file_name,
                                     &(pending_rml_msgs));

        opal_mutex_unlock(&mutex);
        select_common_sm_component(sm_info.id);
        initialized = true;
    }

    if (NULL != sm_init)
    {
        /* notice that we are passing a SORTED procs array to the selected
         * component along with the number of LOCAL processes found within
         * procs.
         */
        return sm_init(procs,
                       num_local_procs,
                       size,
                       file_name,
                       size_ctl_structure,
                       data_seg_alignment);
    }
    return NULL;
}

/******************************************************************************/
/**
 * This routine is the same as mca_common_sm_mmap_init() except that
 * it takes an (ompi_group_t *) parameter to specify the peers rather
 * than an array of procs.  Unlike mca_common_sm_mmap_init(), the
 * group must contain *only* local peers, or this function will return
 * NULL and not create any shared memory segment.
 */
mca_common_sm_module_t *
mca_common_sm_init_group(ompi_group_t *group,
                         size_t size,
                         char *file_name,
                         size_t size_ctl_structure,
                         size_t data_seg_alignment)
{
    mca_common_sm_module_t *ret = NULL;
    ompi_proc_t **procs         = NULL;

    /* make sure sm_init has been properly initialized. do this because
     * sm_init_group only does prep work before passing along the real work to
     * sm_init.
     */
    if (NULL != sm_init)
    {
        size_t i;
        size_t group_size;
        ompi_proc_t *proc;

        /* if there is less than 2 procs, there's nothing to do */
        if ((group_size = ompi_group_size(group)) < 2)
        {
            goto out;
        }
        if (NULL == (procs = (ompi_proc_t **)
                             malloc(sizeof(ompi_proc_t *) * group_size)))

        {
            ORTE_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
            goto out;
        }
        /* make sure that all the procs in the group are local */
        for (i = 0; i < group_size; ++i)
        {
            proc = ompi_group_peer_lookup(group, i);
            if (!OPAL_PROC_ON_LOCAL_NODE(proc->proc_flags))
            {
                goto out;
            }
            procs[i] = proc;
        }
        /* let sm_init take care of the rest ... */
        ret = sm_init(procs,
                      group_size,
                      size,
                      file_name,
                      size_ctl_structure,
                      data_seg_alignment);
    }

out:
    if (NULL != procs)
    {
        free(procs);
    }
    return ret;
}

/******************************************************************************/
void *
mca_common_sm_seg_alloc(struct mca_mpool_base_module_t *mpool,
                        size_t *size,
                        mca_mpool_base_registration_t **registration)
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
    if (NULL != sm_argv)
    {
        opal_argv_free(sm_argv);
        sm_argv = NULL;
    }
    if (NULL != sm_fini)
    {
        return sm_fini(mca_common_sm_module);
    }
    return OMPI_ERR_NOT_FOUND;
}

