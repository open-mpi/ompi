#include <string.h>
#include "threads/mutex.h"
#include "util/output.h"
#include "proc/proc.h"
#include "mca/pcm/pcm.h"


static lam_list_t  lam_proc_list;
static lam_mutex_t lam_proc_lock;
lam_proc_t* lam_proc_local_proc = NULL;

static void lam_proc_construct(lam_proc_t* proc);
static void lam_proc_destruct(lam_proc_t* proc);


lam_class_t lam_proc_t_class = {
    "lam_proc_t", 
    OBJ_CLASS(lam_list_item_t),
    (lam_construct_t)lam_proc_construct, 
    (lam_destruct_t)lam_proc_destruct
};

void lam_proc_construct(lam_proc_t* proc)
{
    static int init = 0;
    if(fetchNset(&init,1) == 0) {
        OBJ_CONSTRUCT(&lam_proc_list, lam_list_t);
        OBJ_CONSTRUCT(&lam_proc_lock, lam_mutex_t);
    }

    proc->proc_job = NULL;
    proc->proc_vpid = 0;
    proc->proc_pml = NULL;
    proc->proc_modex = NULL;

    THREAD_LOCK(&lam_proc_lock);
    lam_list_append(&lam_proc_list, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&lam_proc_lock);
}


void lam_proc_destruct(lam_proc_t* proc)
{
    THREAD_LOCK(&lam_proc_lock);
    lam_list_remove_item(&lam_proc_list, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&lam_proc_lock);
}


int lam_proc_init(void)
{
    mca_pcm_proc_t *procs;
    mca_pcm_proc_t *local;
    size_t i, nprocs;
    int rc;

    if(LAM_SUCCESS != (rc = mca_pcm.pcm_proc_startup())) {
        lam_output(0, "lam_proc_init: pcm_proc_startup failed with errno=%d", rc);
        return rc;
    }

    if(NULL == (local = mca_pcm.pcm_proc_get_me())) {
        lam_output(0, "lam_proc_init: unable to determine local proc id");
        return LAM_ERROR;
    }

    if(LAM_SUCCESS != (rc = mca_pcm.pcm_proc_get_peers(&procs, &nprocs))) {
        lam_output(0, "lam_proc_init: pcm_proc_get_peers failed with errno=%d", rc);
        return rc;
    }

    for(i=0; i<nprocs; i++) {
        lam_job_handle_t job = procs[i].job_handle;
        uint32_t vpid = procs[i].vpid;
        lam_proc_t *proc = OBJ_NEW(lam_proc_t);
        proc->proc_job = strdup(job);
        proc->proc_vpid = vpid;
        if(proc->proc_vpid == local->vpid && strcmp(proc->proc_job, local->job_handle) == 0) {
            lam_proc_local_proc = proc;
        }
    }
    free(procs);
    return LAM_SUCCESS;
}


lam_proc_t** lam_proc_world(size_t *size)
{
    lam_proc_t **procs = malloc(lam_list_get_size(&lam_proc_list) * sizeof(lam_proc_t*));
    lam_proc_t *proc;
    lam_job_handle_t job;
    size_t count = 0;

    if(NULL == procs)
        return NULL;

    if(NULL == (job = mca_pcm.pcm_handle_get()))
        return NULL;

    /* return only the procs that match this jobid */
    THREAD_LOCK(&lam_proc_lock);
    for(proc =  (lam_proc_t*)lam_list_get_first(&lam_proc_list); 
        proc != (lam_proc_t*)lam_list_get_end(&lam_proc_list);
        proc =  (lam_proc_t*)lam_list_get_next(proc)) {
        if(strcmp(proc->proc_job,job) == 0) { 
            OBJ_RETAIN(proc);
            procs[count++] = proc;
        }
    }
    THREAD_UNLOCK(&lam_proc_lock);
    *size = count;
    return procs;
}


lam_proc_t** lam_proc_all(size_t* size)
{
    lam_proc_t **procs = malloc(lam_list_get_size(&lam_proc_list) * sizeof(lam_proc_t*));
    lam_proc_t *proc;
    size_t count = 0;

    if(NULL == procs)
        return NULL;

    THREAD_LOCK(&lam_proc_lock);
    for(proc =  (lam_proc_t*)lam_list_get_first(&lam_proc_list); 
        proc != (lam_proc_t*)lam_list_get_end(&lam_proc_list);
        proc =  (lam_proc_t*)lam_list_get_next(proc)) {
        OBJ_RETAIN(proc);
        procs[count++] = proc;
    }
    THREAD_UNLOCK(&lam_proc_lock);
    *size = count;
    return procs;
}


lam_proc_t** lam_proc_self(size_t* size)
{
    lam_proc_t **procs = malloc(sizeof(lam_proc_t*));
    if(NULL == procs)
        return NULL;
    OBJ_RETAIN(lam_proc_local_proc);
    *procs = lam_proc_local_proc;
    *size = 1;
    return procs;
}

