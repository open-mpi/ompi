/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
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

#include "ompi/proc/proc.h"

opal_convertor_t* oshmem_shmem_local_convertor = NULL;

opal_list_t oshmem_proc_list;
static opal_mutex_t oshmem_proc_lock;
oshmem_proc_t* oshmem_proc_local_proc = NULL;

static void oshmem_proc_construct(oshmem_proc_t* proc);
static void oshmem_proc_destruct(oshmem_proc_t* proc);

OBJ_CLASS_INSTANCE( oshmem_proc_t,
                   opal_list_item_t,
                   oshmem_proc_construct,
                   oshmem_proc_destruct);

void oshmem_proc_construct(oshmem_proc_t* proc)
{
    memset(proc->proc_endpoints, 0, sizeof(proc->proc_endpoints));

    /* By default all processors are supposedly having the same architecture as me. Thus,
     * by default we run in a homogeneous environment. Later, when the RTE can tell us
     * the arch of the remote nodes, we will have to set the convertors to the correct
     * architecture.
     */
    proc->proc_arch = opal_local_arch;
    proc->proc_convertor = oshmem_shmem_local_convertor;
    OBJ_RETAIN( oshmem_shmem_local_convertor);

    proc->proc_flags = 0;
    proc->num_transports = 0;

    /* initialize this pointer to NULL */
    proc->proc_hostname = NULL;
}

void oshmem_proc_destruct(oshmem_proc_t* proc)
{
    /* As all the convertors are created with OBJ_NEW we can just call OBJ_RELEASE. All, except
     * the local convertor, will get destroyed at some point here. If the reference count is correct
     * the local convertor (who has the reference count increased in the datatype) will not get
     * destroyed here. It will be destroyed later when the ompi_datatype_finalize is called.
     */
    OBJ_RELEASE( proc->proc_convertor);

    /* DO NOT FREE THE HOSTNAME FIELD AS THIS POINTS
     * TO AN AREA ALLOCATED/FREE'D ELSEWHERE
     */
    OPAL_THREAD_LOCK(&oshmem_proc_lock);
    opal_list_remove_item(&oshmem_proc_list, (opal_list_item_t*) proc);
    OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
}

int oshmem_proc_init(void)
{
    orte_vpid_t i;

    OBJ_CONSTRUCT(&oshmem_proc_list, opal_list_t);
    OBJ_CONSTRUCT(&oshmem_proc_lock, opal_mutex_t);
    oshmem_shmem_local_convertor = opal_convertor_create(opal_local_arch, 0);

    size_t ompi_num_procs;
    ompi_proc_t **ompi_procs = ompi_proc_world(&ompi_num_procs);
    /* create proc structures and find self */
    for (i = 0; i < orte_process_info.num_procs; i++) {
        oshmem_proc_t *proc = OBJ_NEW(oshmem_proc_t);
        opal_list_append(&oshmem_proc_list, (opal_list_item_t*)proc);

        proc->proc_name.jobid = ompi_procs[i]->proc_name.jobid;
        proc->proc_name.vpid = ompi_procs[i]->proc_name.vpid;
        proc->proc_arch = ompi_procs[i]->proc_arch;
        proc->proc_flags = ompi_procs[i]->proc_flags;
        proc->proc_hostname = ompi_procs[i]->proc_hostname;

        if (i == ORTE_PROC_MY_NAME->vpid) {
            oshmem_proc_local_proc = proc;
        }
    }

    if (ompi_procs)
        free(ompi_procs);

    return OSHMEM_SUCCESS;
}

/* in some cases, all PE procs are required to do a modex so they
 * can (at the least) exchange their architecture. Since we cannot
 * know in advance if this was required, we provide a separate function
 * to set the arch (instead of doing it inside of oshmem_proc_init) that
 * can be called after the modex completes in oshmem_shmem_init. Thus, we
 * know that - regardless of how the arch is known, whether via modex
 * or dropped in from a local daemon - the arch can be set correctly
 * at this time
 */
int oshmem_proc_set_arch(void)
{
    oshmem_proc_t *proc = NULL;
    opal_list_item_t *item = NULL;
    int ret = OSHMEM_SUCCESS;

    OPAL_THREAD_LOCK(&oshmem_proc_lock);

    for (item = opal_list_get_first(&oshmem_proc_list);
            item != opal_list_get_end(&oshmem_proc_list);
            item = opal_list_get_next(item)) {
        proc = (oshmem_proc_t*) item;

        if (proc->proc_name.vpid != ORTE_PROC_MY_NAME->vpid) {
            /* if arch is different than mine, create a new convertor for this proc */
            if (proc->proc_arch != opal_local_arch) {
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                OBJ_RELEASE(proc->proc_convertor);
                proc->proc_convertor = opal_convertor_create(proc->proc_arch, 0);
#else
                orte_show_help("help-shmem-runtime.txt",
                               "heterogeneous-support-unavailable",
                               true,
                               orte_process_info.nodename,
                               proc->proc_hostname == NULL ?
                                       "<hostname unavailable>" :
                                       proc->proc_hostname);
                OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
                return OSHMEM_ERR_NOT_SUPPORTED;
#endif
            }
        }
    }

    /* Set predefined groups */
    ret = oshmem_proc_group_init();

    OPAL_THREAD_UNLOCK(&oshmem_proc_lock);

    return ret;
}

int oshmem_proc_finalize(void)
{
    opal_list_item_t *item;

    /* Destroy all groups */
    oshmem_proc_group_finalize();

    /* remove all items from list and destroy them. Since we cannot know
     * the reference count of the procs for certain, it is possible that
     * a single OBJ_RELEASE won't drive the count to zero, and hence will
     * not release the memory. Accordingly, we cycle through the list here,
     * calling release on each item.
     *
     * This will cycle until it forces the reference count of each item
     * to zero, thus causing the destructor to run - which will remove
     * the item from the list!
     *
     * We cannot do this under the thread lock as the destructor will
     * call it when removing the item from the list. However, this function
     * is ONLY called from MPI_Finalize, and all threads are prohibited from
     * calling an MPI function once ANY thread has called MPI_Finalize. Of
     * course, multiple threads are allowed to call MPI_Finalize, so this
     * function may get called multiple times by various threads. We believe
     * it is thread safe to do so...though it may not -appear- to be so
     * without walking through the entire list/destructor sequence.
     */
    while (opal_list_get_end(&oshmem_proc_list)
            != (item = opal_list_get_first(&oshmem_proc_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_RELEASE( oshmem_shmem_local_convertor);
    /* now destruct the list and thread lock */
    OBJ_DESTRUCT(&oshmem_proc_list);
    OBJ_DESTRUCT(&oshmem_proc_lock);

    return OSHMEM_SUCCESS;
}

oshmem_proc_t** oshmem_proc_world(size_t *size)
{
    oshmem_proc_t **procs;
    oshmem_proc_t *proc;
    size_t count = 0;
    orte_ns_cmp_bitmask_t mask;
    orte_process_name_t my_name;

    /* check bozo case */
    if (NULL == oshmem_proc_local_proc) {
        return NULL ;
    }
    mask = ORTE_NS_CMP_JOBID;
    my_name = oshmem_proc_local_proc->proc_name;

    /* First count how many match this jobid */
    OPAL_THREAD_LOCK(&oshmem_proc_lock);
    for (proc = (oshmem_proc_t*) opal_list_get_first(&oshmem_proc_list);
            proc != (oshmem_proc_t*) opal_list_get_end(&oshmem_proc_list);
            proc = (oshmem_proc_t*) opal_list_get_next(proc)) {
        if (OPAL_EQUAL
                == orte_util_compare_name_fields(mask,
                                                 &proc->proc_name,
                                                 &my_name)) {
            ++count;
        }
    }

    /* allocate an array */
    procs = (oshmem_proc_t**) malloc(count * sizeof(oshmem_proc_t*));
    if (NULL == procs) {
        OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
        return NULL ;
    }

    /* now save only the procs that match this jobid */
    count = 0;
    for (proc = (oshmem_proc_t*) opal_list_get_first(&oshmem_proc_list);
            proc != (oshmem_proc_t*) opal_list_get_end(&oshmem_proc_list);
            proc = (oshmem_proc_t*) opal_list_get_next(proc)) {
        if (OPAL_EQUAL
                == orte_util_compare_name_fields(mask,
                                                 &proc->proc_name,
                                                 &my_name)) {
            /* DO NOT RETAIN THIS OBJECT - the reference count on this
             * object will be adjusted by external callers. The intent
             * here is to allow the reference count to drop to zero if
             * the app no longer desires to communicate with this proc.
             * For example, the proc may call comm_disconnect on all
             * communicators involving this proc. In such cases, we want
             * the proc object to be removed from the list. By not incrementing
             * the reference count here, we allow this to occur.
             *
             * We don't implement that yet, but we are still safe for now as
             * the OBJ_NEW in oshmem_proc_init owns the initial reference
             * count which cannot be released until oshmem_proc_finalize is
             * called.
             */
            procs[count++] = proc;
        }
    } OPAL_THREAD_UNLOCK(&oshmem_proc_lock);

    *size = count;
    return procs;
}

oshmem_proc_t** oshmem_proc_all(size_t* size)
{
    oshmem_proc_t **procs =
            (oshmem_proc_t**) malloc(opal_list_get_size(&oshmem_proc_list)
                    * sizeof(oshmem_proc_t*));
    oshmem_proc_t *proc;
    size_t count = 0;

    if (NULL == procs) {
        return NULL ;
    }

    OPAL_THREAD_LOCK(&oshmem_proc_lock);
    for (proc = (oshmem_proc_t*) opal_list_get_first(&oshmem_proc_list);
            ((proc != (oshmem_proc_t*) opal_list_get_end(&oshmem_proc_list))
                    && (proc != NULL ));
            proc = (oshmem_proc_t*)opal_list_get_next(proc)) {
            /* We know this isn't consistent with the behavior in oshmem_proc_world,
             * but we are leaving the RETAIN for now because the code using this function
             * assumes that the results need to be released when done. It will
             * be cleaned up later as the "fix" will impact other places in
             * the code
             */
            OBJ_RETAIN(proc);
            procs[count++] = proc;
        }
    OPAL_THREAD_UNLOCK(&oshmem_proc_lock);

    *size = count;

    return procs;
}

oshmem_proc_t** oshmem_proc_self(size_t* size)
{
    oshmem_proc_t **procs = (oshmem_proc_t**) malloc(sizeof(oshmem_proc_t*));
    if (NULL == procs) {
        return NULL ;
    }
    /* We know this isn't consistent with the behavior in oshmem_proc_world,
     * but we are leaving the RETAIN for now because the code using this function
     * assumes that the results need to be released when done. It will
     * be cleaned up later as the "fix" will impact other places in
     * the code
     */
    OBJ_RETAIN(oshmem_proc_local_proc);

    *procs = oshmem_proc_local_proc;
    *size = 1;
    return procs;
}

oshmem_proc_t * oshmem_proc_find(const orte_process_name_t * name)
{
    oshmem_proc_t *proc, *rproc = NULL;
    orte_ns_cmp_bitmask_t mask;

    /* return the proc-struct which matches this jobid+process id */
    mask = ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    OPAL_THREAD_LOCK(&oshmem_proc_lock);
    for (proc = (oshmem_proc_t*) opal_list_get_first(&oshmem_proc_list);
            proc != (oshmem_proc_t*) opal_list_get_end(&oshmem_proc_list);
            proc = (oshmem_proc_t*) opal_list_get_next(proc)) {
        if (OPAL_EQUAL
                == orte_util_compare_name_fields(mask,
                                                 &proc->proc_name,
                                                 name)) {
            rproc = proc;
            break;
        }
    } OPAL_THREAD_UNLOCK(&oshmem_proc_lock);

    return rproc;
}


int oshmem_proc_pack(oshmem_proc_t **proclist,
                     int proclistsize,
                     opal_buffer_t* buf)
{
    int i, rc;

    OPAL_THREAD_LOCK(&oshmem_proc_lock);

    /* cycle through the provided array, packing the OSHMEM level
     * data for each proc. This data may or may not be included
     * in any subsequent modex operation, so we include it here
     * to ensure completion of a connect/accept handshake. See
     * the ompi/mca/dpm framework for an example of where and how
     * this info is used.
     *
     * Eventually, we will review the procedures that call this
     * function to see if duplication of communication can be
     * reduced. For now, just go ahead and pack the info so it
     * can be sent.
     */
    for (i = 0; i < proclistsize; i++) {
        rc = opal_dss.pack(buf, &(proclist[i]->proc_name), 1, ORTE_NAME);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
            return rc;
        }
        rc = opal_dss.pack(buf, &(proclist[i]->proc_arch), 1, OPAL_UINT32);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
            return rc;
        }
        rc = opal_dss.pack(buf, &(proclist[i]->proc_hostname), 1, OPAL_STRING);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
            return rc;
        }
    } OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
    return OSHMEM_SUCCESS;
}

static oshmem_proc_t *
oshmem_proc_find_and_add(const orte_process_name_t * name, bool* isnew)
{
    oshmem_proc_t *proc, *rproc = NULL;
    orte_ns_cmp_bitmask_t mask;

    /* return the proc-struct which matches this jobid+process id */
    mask = ORTE_NS_CMP_JOBID | ORTE_NS_CMP_VPID;
    OPAL_THREAD_LOCK(&oshmem_proc_lock);
    for (proc = (oshmem_proc_t*) opal_list_get_first(&oshmem_proc_list);
            proc != (oshmem_proc_t*) opal_list_get_end(&oshmem_proc_list);
            proc = (oshmem_proc_t*) opal_list_get_next(proc)) {
        if (OPAL_EQUAL
                == orte_util_compare_name_fields(mask,
                                                 &proc->proc_name,
                                                 name)) {
            rproc = proc;
            *isnew = false;
            break;
        }
    }

    /* if we didn't find this proc in the list, create a new
     * proc_t and append it to the list
     */
    if (NULL == rproc) {
        *isnew = true;
        rproc = OBJ_NEW(oshmem_proc_t);
        if (NULL != rproc) {
            opal_list_append(&oshmem_proc_list, (opal_list_item_t*)rproc);
            rproc->proc_name = *name;
        }
        /* caller had better fill in the rest of the proc, or there's
         going to be pain later... */
    }

    OPAL_THREAD_UNLOCK(&oshmem_proc_lock);

    return rproc;
}

int oshmem_proc_unpack(opal_buffer_t* buf,
                       int proclistsize,
                       oshmem_proc_t ***proclist,
                       int *newproclistsize,
                       oshmem_proc_t ***newproclist)
{
    int i;
    size_t newprocs_len = 0;
    oshmem_proc_t **plist = NULL, **newprocs = NULL;

    /* do not free plist *ever*, since it is used in the remote group
     structure of a communicator */
    plist = (oshmem_proc_t **) calloc(proclistsize, sizeof(oshmem_proc_t *));
    if (NULL == plist) {
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }
    /* free this on the way out */
    newprocs = (oshmem_proc_t **) calloc(proclistsize, sizeof(oshmem_proc_t *));
    if (NULL == newprocs) {
        free(plist);
        return OSHMEM_ERR_OUT_OF_RESOURCE;
    }

    /* cycle through the array of provided procs and unpack
     * their info - as packed by oshmem_proc_pack
     */
    for (i = 0; i < proclistsize; i++) {
        orte_std_cntr_t count = 1;
        orte_process_name_t new_name;
        uint32_t new_arch;
        char *new_hostname;
        bool isnew = false;
        int rc;

        rc = opal_dss.unpack(buf, &new_name, &count, ORTE_NAME);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return rc;
        }
        rc = opal_dss.unpack(buf, &new_arch, &count, OPAL_UINT32);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return rc;
        }
        rc = opal_dss.unpack(buf, &new_hostname, &count, OPAL_STRING);
        if (rc != ORTE_SUCCESS) {
            ORTE_ERROR_LOG(rc);
            free(plist);
            free(newprocs);
            return rc;
        }

        /* see if this proc is already on our oshmem_proc_list */
        plist[i] = oshmem_proc_find_and_add(&new_name, &isnew);
        if (isnew) {
            /* if not, then it was added, so update the values
             * in the proc_t struct with the info that was passed
             * to us
             */
            newprocs[newprocs_len++] = plist[i];

            /* update all the values */
            plist[i]->proc_arch = new_arch;
            /* if arch is different than mine, create a new convertor for this proc */
            if (plist[i]->proc_arch != opal_local_arch) {
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
                OBJ_RELEASE(plist[i]->proc_convertor);
                plist[i]->proc_convertor = opal_convertor_create(plist[i]->proc_arch, 0);
#else
                orte_show_help("help-shmem-runtime.txt",
                               "heterogeneous-support-unavailable",
                               true,
                               orte_process_info.nodename,
                               new_hostname == NULL ? "<hostname unavailable>" :
                                                      new_hostname);
                free(plist);
                free(newprocs);
                return OSHMEM_ERR_NOT_SUPPORTED;
#endif
            }
            if (0
                    == strcmp(oshmem_proc_local_proc->proc_hostname,
                              new_hostname)) {
                plist[i]->proc_flags |= (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU
                        | OPAL_PROC_ON_CLUSTER);
            }

            /* Save the hostname */
            plist[i]->proc_hostname = new_hostname;

            /* eventually, we will update the orte/mca/ess framework's data
             * to contain the info for the new proc. For now, we ignore
             * this step since the MPI layer already has all the info
             * it requires
             */
        }
    }

    if (NULL != newproclistsize)
        *newproclistsize = newprocs_len;
    if (NULL != newproclist) {
        *newproclist = newprocs;
    } else if (newprocs != NULL ) {
        free(newprocs);
    }

    *proclist = plist;
    return OSHMEM_SUCCESS;
}

opal_pointer_array_t oshmem_group_array;

oshmem_group_t* oshmem_group_all = NULL;
oshmem_group_t* oshmem_group_self = NULL;
oshmem_group_t* oshmem_group_null = NULL;

OBJ_CLASS_INSTANCE(oshmem_group_t, opal_object_t, NULL, NULL);

OSHMEM_DECLSPEC int oshmem_proc_group_init(void)
{

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
                                             opal_list_get_size(&oshmem_proc_list)))) {
        oshmem_proc_group_destroy(oshmem_group_all);
        return OSHMEM_ERROR;
    }

    /* Setup SHMEM_GROUP_SELF */
    if (NULL
            == (oshmem_group_self = oshmem_proc_group_create(oshmem_proc_local()
                                                                     ->proc_name
                                                                     .vpid,
                                                             0,
                                                             1))) {
        oshmem_proc_group_destroy(oshmem_group_self);
        return OSHMEM_ERROR;
    }

    /* Setup SHMEM_GROUP_NULL */
    oshmem_group_null = NULL;

    return OSHMEM_SUCCESS;
}

OSHMEM_DECLSPEC int oshmem_proc_group_finalize(void)
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

OSHMEM_DECLSPEC oshmem_group_t* oshmem_proc_group_create(int pe_start,
                                                         int pe_stride,
                                                         size_t pe_size)
{
    int cur_pe, count_pe;
    int i;
    oshmem_group_t* group = NULL;
    oshmem_proc_t** proc_array = NULL;
    oshmem_proc_t* proc = NULL;

    group = OBJ_NEW(oshmem_group_t);

    if (group) {
        cur_pe = 0;
        count_pe = 0;

        OPAL_THREAD_LOCK(&oshmem_proc_lock);

        /* allocate an array */
        proc_array = (oshmem_proc_t**) malloc(pe_size * sizeof(oshmem_proc_t*));
        if (NULL == proc_array) {
            OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
            return NULL ;
        }

        group->my_pe = oshmem_proc_local()->proc_name.vpid;
        group->is_member = 0;
        /* now save only the procs that match this jobid */
        for (proc = (oshmem_proc_t*) opal_list_get_first(&oshmem_proc_list);
                proc != (oshmem_proc_t*) opal_list_get_end(&oshmem_proc_list);
                proc = (oshmem_proc_t*) opal_list_get_next(proc)) {
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
                peer->name.jobid = group->proc_array[i]->proc_name.jobid;
                peer->name.vpid = group->proc_array[i]->proc_name.vpid;
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
            return NULL ;
        } OPAL_THREAD_UNLOCK(&oshmem_proc_lock);
    }

    return group;
}

OSHMEM_DECLSPEC void oshmem_proc_group_destroy(oshmem_group_t* group)
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
