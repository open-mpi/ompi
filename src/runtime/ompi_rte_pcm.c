/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <errno.h>

#include "include/constants.h"
#include "class/ompi_pointer_array.h"
#include "runtime/runtime.h"
#include "runtime/runtime_types.h"
#include "runtime/runtime_internal.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/base/base.h"

static void ompi_rte_spawn_handle_construct(ompi_object_t *);
static void ompi_rte_spawn_handle_destruct(ompi_object_t *);

static ompi_pointer_array_t avail_handles;
static ompi_mutex_t avail_handles_mutex;

OBJ_CLASS_INSTANCE(ompi_rte_spawn_handle_t, ompi_object_t, 
                   ompi_rte_spawn_handle_construct, ompi_rte_spawn_handle_destruct);


int
ompi_rte_internal_init_spawn(void)
{
    OBJ_CONSTRUCT(&avail_handles, ompi_pointer_array_t);
    OBJ_CONSTRUCT(&avail_handles_mutex, ompi_mutex_t);

    return OMPI_SUCCESS;
}

int
ompi_rte_internal_fini_spawn(void)
{
    int i;
    ompi_rte_spawn_handle_t *ptr;

    OMPI_THREAD_LOCK(&avail_handles_mutex);
    for (i = 0 ; i < ompi_pointer_array_get_size(&avail_handles) ; ++i) {
        ptr = (ompi_rte_spawn_handle_t*) ompi_pointer_array_get_item(&avail_handles, i);
        if (NULL == ptr) continue;

        OBJ_RELEASE(ptr);

        ompi_pointer_array_set_item(&avail_handles, i, NULL);
    }
    OMPI_THREAD_UNLOCK(&avail_handles_mutex);

    OBJ_DESTRUCT(&avail_handles);
    OBJ_DESTRUCT(&avail_handles_mutex);

    return OMPI_SUCCESS;
}


ompi_rte_spawn_handle_t *
ompi_rte_get_spawn_handle(int criteria, bool have_threads)
{
    int i;
    ompi_rte_spawn_handle_t *ptr;
    int ret;

    /* BWB - long term, this has to go.  But for now, here we are */
    if (0 != (OMPI_RTE_SPAWN_MULTI_CELL & criteria)) {
        printf("ompi_rte_get_spawn_handle: warning: multi-cell support "
               "not implemented.  Removing criteria.\n");
        criteria ^= OMPI_RTE_SPAWN_MULTI_CELL;
    }

    OMPI_THREAD_LOCK(&avail_handles_mutex);
    
    /* make sure we don't already have a matching criteria */
    for (i = 0 ; i < ompi_pointer_array_get_size(&avail_handles) ; ++i) {
        ptr = (ompi_rte_spawn_handle_t*) ompi_pointer_array_get_item(&avail_handles, i);
        if (NULL == ptr) continue;

        if (ptr->criteria == criteria) {
            OBJ_RETAIN(ptr);
            goto cleanup;
        }
    }

    /* no matching criteria.  create a new set of pcms and we're good
       to go */
    ptr = OBJ_NEW(ompi_rte_spawn_handle_t);
    if (NULL == ptr) return NULL;

    ret = mca_pcm_base_select(have_threads, criteria,
                              &(ptr->modules), &(ptr->modules_len));
    if (ret != OMPI_SUCCESS) {
        errno = ret;
        return NULL;
    }

    /* remove for multi-cell */
    if (ptr->modules_len != 1) {
        OBJ_RELEASE(ptr);
        return NULL;
    }

    ompi_pointer_array_add(&avail_handles, ptr);

 cleanup:
    OMPI_THREAD_UNLOCK(&avail_handles_mutex);

    return ptr;
}


int
ompi_rte_spawn_procs(ompi_rte_spawn_handle_t *handle,
                     mca_ns_base_jobid_t jobid, 
                     ompi_list_t *schedule_list)
{
    mca_pcm_base_module_t *active;

    if (NULL == handle) return OMPI_ERR_BAD_PARAM;
    if (MCA_NS_BASE_JOBID_MAX == jobid) return OMPI_ERR_BAD_PARAM;
    if (NULL == schedule_list) return OMPI_ERR_BAD_PARAM;

    /* remove for multi-cell */
    assert(1 == handle->modules_len);

    active = handle->modules[0];

    if (NULL == active->pcm_spawn_procs) {
        return OMPI_ERR_NOT_IMPLEMENTED;
    }

    return active->pcm_spawn_procs(active, jobid, schedule_list);
}


int
ompi_rte_kill_proc(ompi_process_name_t *name, int flags)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_rte_kill_job(mca_ns_base_jobid_t jobid, int flags)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


ompi_process_name_t*
ompi_rte_get_self(void)
{
    if (NULL == mca_pcmclient.pcmclient_get_self) {
        errno = OMPI_ERR_NOT_IMPLEMENTED;
        return NULL;
    }

    return mca_pcmclient.pcmclient_get_self();
}


int
ompi_rte_get_peers(ompi_process_name_t **peers, size_t *npeers)
{
    ompi_process_name_t *useless;
    ompi_process_name_t **peers_p;

    if (NULL == mca_pcmclient.pcmclient_get_peers) {
        return OMPI_ERR_NOT_IMPLEMENTED;
    }

    if (NULL == peers) {
        /* the returned value is a pointer to a static buffer, so no
           free is neeeded.  This is therefore completely safe.  Yay */
        peers_p = &useless;
    } else {
        peers_p = peers;
    }

    return mca_pcmclient.pcmclient_get_peers(peers_p, npeers);
}


static void
ompi_rte_spawn_handle_construct(ompi_object_t *obj)
{
    ompi_rte_spawn_handle_t *handle = (ompi_rte_spawn_handle_t*) obj;
    handle->criteria = 0;
    handle->modules = NULL;
    handle->modules_len = 0;
}


static void
ompi_rte_spawn_handle_destruct(ompi_object_t *obj)
{
    ompi_rte_spawn_handle_t *handle = (ompi_rte_spawn_handle_t*) obj;
    size_t i;

    handle->criteria = 0;
    for (i = 0 ; i < handle->modules_len ; ++i) {
        mca_pcm_base_module_t *pcm = handle->modules[i];
        if (NULL == pcm) continue;
        if (NULL == pcm->pcm_finalize) continue;
        pcm->pcm_finalize(pcm);
    }

    if (NULL != handle->modules) free(handle->modules);
    
    handle->modules_len = 0;
}
