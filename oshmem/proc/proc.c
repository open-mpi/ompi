/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/proc/proc.h"
#include "oshmem/constants.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/scoll/base/base.h"

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/threads/mutex.h"
#include "opal/dss/dss.h"
#include "opal/util/arch.h"
#include "opal/class/opal_list.h"


static opal_mutex_t oshmem_proc_lock;

int oshmem_proc_init(void)
{
    OBJ_CONSTRUCT(&oshmem_proc_lock, opal_mutex_t);

    assert(sizeof(ompi_proc_t) >= sizeof(oshmem_proc_t));

    return OSHMEM_SUCCESS;
}

int oshmem_proc_finalize(void)
{
    OBJ_DESTRUCT(&oshmem_proc_lock);

    return OSHMEM_SUCCESS;
}

opal_pointer_array_t oshmem_group_array = {{0}};

oshmem_group_t* oshmem_group_all = NULL;
oshmem_group_t* oshmem_group_self = NULL;
oshmem_group_t* oshmem_group_null = NULL;

OBJ_CLASS_INSTANCE(oshmem_group_t, opal_object_t, NULL, NULL);

int oshmem_proc_group_init(void)
{
    if (orte_process_info.num_procs != opal_list_get_size(&ompi_proc_list)) {
        opal_output(0,
                "Error: oshmem_group_all is not created: orte_process_info.num_procs = %d ompi_proc_list = %" PRIsize_t,
		orte_process_info.num_procs,
		opal_list_get_size(&ompi_proc_list));
        return OSHMEM_ERROR;
    }

    /* Setup communicator array */
    OBJ_CONSTRUCT(&oshmem_group_array, opal_pointer_array_t);
    if (OPAL_SUCCESS
            != opal_pointer_array_init(&oshmem_group_array,
                                       0,
                                       ORTE_GLOBAL_ARRAY_MAX_SIZE,
                                       1)) {
        return OSHMEM_ERROR;
    }

    /* Setup SHMEM_GROUP_ALL */
    if (NULL
            == (oshmem_group_all =
                    oshmem_proc_group_create(0,
                                             1,
					     oshmem_num_procs()))) {
        oshmem_proc_group_destroy(oshmem_group_all);
        return OSHMEM_ERROR;
    }

    /* Setup SHMEM_GROUP_SELF */
    if (NULL
            == (oshmem_group_self = oshmem_proc_group_create(oshmem_proc_pe(oshmem_proc_local()),
                                                             0,
                                                             1))) {
        oshmem_proc_group_destroy(oshmem_group_self);
        return OSHMEM_ERROR;
    }

    /* Setup SHMEM_GROUP_NULL */
    oshmem_group_null = NULL;

    return OSHMEM_SUCCESS;
}

int oshmem_proc_group_finalize(void)
{
    int max, i;
    oshmem_group_t *group;

    /* Check whether we have some left */
    max = opal_pointer_array_get_size(&oshmem_group_array);
    for (i = 0; i < max; i++) {
        group =
                (oshmem_group_t *) opal_pointer_array_get_item(&oshmem_group_array,
                                                               i);
        if (NULL != group) {
            /* Group has not been freed before finalize */
            oshmem_proc_group_destroy(group);
        }
    }

    OBJ_DESTRUCT(&oshmem_group_array);

    return OSHMEM_SUCCESS;
}

oshmem_group_t* oshmem_proc_group_create(int pe_start,
                                         int pe_stride,
                                         size_t pe_size)
{
    int cur_pe, count_pe;
    int i;
    oshmem_group_t* group = NULL;
    oshmem_proc_t** proc_array = NULL;
    oshmem_proc_t* proc = NULL;

    assert(oshmem_proc_local());

    group = OBJ_NEW(oshmem_group_t);

    if (group) {
        cur_pe = 0;
        count_pe = 0;

        OPAL_THREAD_LOCK(&oshmem_proc_lock);

        /* allocate an array */
        proc_array = (oshmem_proc_t**) malloc(pe_size * sizeof(oshmem_proc_t*));
        if (NULL == proc_array) {
            OBJ_RELEASE(group);
            OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
            return NULL ;
        }

        group->my_pe = oshmem_proc_pe(oshmem_proc_local());
        group->is_member = 0;
        for (i = 0 ; i < oshmem_num_procs() ; i++) {
            proc = oshmem_proc_find(i);
            if (NULL == proc) {
                opal_output(0,
                             "Error: Can not find proc object for pe = %d", i);
                free(proc_array);
                OBJ_RELEASE(group);
                OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
                return NULL;
            }
            if (count_pe >= (int) pe_size) {
                break;
            } else if ((cur_pe >= pe_start)
                    && ((pe_stride == 0)
                            || (((cur_pe - pe_start) % pe_stride) == 0))) {
                proc_array[count_pe++] = proc;
                if (oshmem_proc_pe(proc) == group->my_pe)
                    group->is_member = 1;
            }
            cur_pe++;
        }
        group->proc_array = proc_array;
        group->proc_count = (int) count_pe;
        group->ompi_comm = NULL;

        /* Prepare peers list */
        OBJ_CONSTRUCT(&(group->peer_list), opal_list_t);
        {
            orte_namelist_t *peer = NULL;

            for (i = 0; i < group->proc_count; i++) {
                peer = OBJ_NEW(orte_namelist_t);
                peer->name.jobid = OSHMEM_PROC_JOBID(group->proc_array[i]);
                peer->name.vpid = OSHMEM_PROC_VPID(group->proc_array[i]);
                opal_list_append(&(group->peer_list), &peer->super);
            }
        }
        group->id = opal_pointer_array_add(&oshmem_group_array, group);

        memset(&group->g_scoll, 0, sizeof(mca_scoll_base_group_scoll_t));

        if (OSHMEM_SUCCESS != mca_scoll_base_select(group)) {
            opal_output(0,
                        "Error: No collective modules are available: group is not created, returning NULL");
            oshmem_proc_group_destroy(group);
            OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
            return NULL;
        }
        OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
    }

    return group;
}

void oshmem_proc_group_destroy(oshmem_group_t* group)
{
    if (group) {
        mca_scoll_base_group_unselect(group);

        /* Destroy proc array */
        if (group->proc_array) {
            free(group->proc_array);
        }

        /* Destroy peer list */
        {
            opal_list_item_t *item;

            while (NULL != (item = opal_list_remove_first(&(group->peer_list)))) {
                /* destruct the item (we constructed it), then free the memory chunk */
                OBJ_RELEASE(item);
            }
            OBJ_DESTRUCT(&(group->peer_list));
        }

        /* reset the oshmem_group_array entry - make sure that the
         * entry is in the table */
        if (NULL
                != opal_pointer_array_get_item(&oshmem_group_array,
                                               group->id)) {
            opal_pointer_array_set_item(&oshmem_group_array, group->id, NULL );
        }

        OBJ_RELEASE(group);
    }
}
