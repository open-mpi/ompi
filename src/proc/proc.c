#include <string.h>
#include "threads/mutex.h"
#include "util/output.h"
#include "proc/proc.h"
#include "mca/pcm/pcm.h"


static ompi_list_t  ompi_proc_list;
static ompi_mutex_t ompi_proc_lock;
ompi_proc_t* ompi_proc_local_proc = NULL;

static void ompi_proc_construct(ompi_proc_t* proc);
static void ompi_proc_destruct(ompi_proc_t* proc);


ompi_class_t ompi_proc_t_class = {
    "ompi_proc_t", 
    OBJ_CLASS(ompi_list_item_t),
    (ompi_construct_t)ompi_proc_construct, 
    (ompi_destruct_t)ompi_proc_destruct
};

void ompi_proc_construct(ompi_proc_t* proc)
{
    static int init = 0;
    if(fetchNset(&init,1) == 0) {
        OBJ_CONSTRUCT(&ompi_proc_list, ompi_list_t);
        OBJ_CONSTRUCT(&ompi_proc_lock, ompi_mutex_t);
    }

    proc->proc_job = NULL;
    proc->proc_vpid = 0;
    proc->proc_pml = NULL;
    proc->proc_modex = NULL;
    proc->proc_arch = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);

    /* FIX - need to determine remote process architecture */
    proc->proc_convertor = ompi_convertor_create(0, 0);

    THREAD_LOCK(&ompi_proc_lock);
    ompi_list_append(&ompi_proc_list, (ompi_list_item_t*)proc);
    THREAD_UNLOCK(&ompi_proc_lock);
}


void ompi_proc_destruct(ompi_proc_t* proc)
{
    THREAD_LOCK(&ompi_proc_lock);
    ompi_list_remove_item(&ompi_proc_list, (ompi_list_item_t*)proc);
    THREAD_UNLOCK(&ompi_proc_lock);
    OBJ_DESTRUCT(&proc->proc_lock);
}


int ompi_proc_init(void)
{
    mca_pcm_proc_t *procs;
    mca_pcm_proc_t *local;
    size_t i, nprocs;
    int rc;

    if(OMPI_SUCCESS != (rc = mca_pcm.pcm_proc_startup())) {
        ompi_output(0, "ompi_proc_init: pcm_proc_startup failed with errno=%d", rc);
        return rc;
    }

    if(NULL == (local = mca_pcm.pcm_proc_get_me())) {
        ompi_output(0, "ompi_proc_init: unable to determine local proc id");
        return OMPI_ERROR;
    }

    if(OMPI_SUCCESS != (rc = mca_pcm.pcm_proc_get_peers(&procs, &nprocs))) {
        ompi_output(0, "ompi_proc_init: pcm_proc_get_peers failed with errno=%d", rc);
        return rc;
    }

    for(i=0; i<nprocs; i++) {
        ompi_job_handle_t job = procs[i].job_handle;
        uint32_t vpid = procs[i].vpid;
        ompi_proc_t *proc = OBJ_NEW(ompi_proc_t);
        proc->proc_job = strdup(job);
        proc->proc_vpid = vpid;
        if(proc->proc_vpid == local->vpid && strcmp(proc->proc_job, local->job_handle) == 0) {
            ompi_proc_local_proc = proc;
        }
    }
    free(procs);
    return OMPI_SUCCESS;
}


ompi_proc_t** ompi_proc_world(size_t *size)
{
    ompi_proc_t **procs = malloc(ompi_list_get_size(&ompi_proc_list) * sizeof(ompi_proc_t*));
    ompi_proc_t *proc;
    ompi_job_handle_t job;
    size_t count = 0;

    if(NULL == procs)
        return NULL;

    if(NULL == (job = mca_pcm.pcm_handle_get()))
        return NULL;

    /* return only the procs that match this jobid */
    THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)ompi_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)ompi_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)ompi_list_get_next(proc)) {
        if(strcmp(proc->proc_job,job) == 0) { 
            OBJ_RETAIN(proc);
            procs[count++] = proc;
        }
    }
    THREAD_UNLOCK(&ompi_proc_lock);
    *size = count;
    return procs;
}


ompi_proc_t** ompi_proc_all(size_t* size)
{
    ompi_proc_t **procs = malloc(ompi_list_get_size(&ompi_proc_list) * sizeof(ompi_proc_t*));
    ompi_proc_t *proc;
    size_t count = 0;

    if(NULL == procs)
        return NULL;

    THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)ompi_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)ompi_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)ompi_list_get_next(proc)) {
        OBJ_RETAIN(proc);
        procs[count++] = proc;
    }
    THREAD_UNLOCK(&ompi_proc_lock);
    *size = count;
    return procs;
}


ompi_proc_t** ompi_proc_self(size_t* size)
{
    ompi_proc_t **procs = malloc(sizeof(ompi_proc_t*));
    if(NULL == procs)
        return NULL;
    OBJ_RETAIN(ompi_proc_local_proc);
    *procs = ompi_proc_local_proc;
    *size = 1;
    return procs;
}

ompi_proc_t * ompi_proc_find ( ompi_job_handle_t jobid, uint32_t vpid )
{
    ompi_proc_t *proc;

    /* return the proc-struct which matches this jobid+process id */
    THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)ompi_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)ompi_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)ompi_list_get_next(proc)) {
        if( (strcmp(proc->proc_job,jobid) == 0) &&
            (proc->proc_vpid == vpid ) )
            { 
                break;
            }
    }
    THREAD_UNLOCK(&ompi_proc_lock);
    return proc;
}
