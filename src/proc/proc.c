#include <string.h>
#include "threads/mutex.h"
#include "util/output.h"
#include "proc/proc.h"
#include "mca/pcm/pcm.h"
#include "mca/oob/oob.h"


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

    proc->proc_pml = NULL;
    proc->proc_modex = NULL;
    proc->proc_arch = 0;
    OBJ_CONSTRUCT(&proc->proc_lock, ompi_mutex_t);

    /* FIX - need to determine remote process architecture */
    proc->proc_convertor = ompi_convertor_create(0, 0);

    OMPI_THREAD_LOCK(&ompi_proc_lock);
    ompi_list_append(&ompi_proc_list, (ompi_list_item_t*)proc);
    OMPI_THREAD_UNLOCK(&ompi_proc_lock);
}


void ompi_proc_destruct(ompi_proc_t* proc)
{
    OMPI_THREAD_LOCK(&ompi_proc_lock);
    ompi_list_remove_item(&ompi_proc_list, (ompi_list_item_t*)proc);
    OMPI_THREAD_UNLOCK(&ompi_proc_lock);
    OBJ_DESTRUCT(&proc->proc_lock);
}


int ompi_proc_init(void)
{
    ompi_process_name_t *peers;
    ompi_process_name_t *self;
    size_t i, npeers;
    int rc;

    if(OMPI_SUCCESS != (rc = mca_pcm.pcm_peers(&peers, &npeers))) {
        ompi_output(0, "ompi_proc_init: mca_pcm.pcm_peers failed with errno=%d", rc);
        return rc;
    }
    if(NULL == (self = mca_pcm.pcm_self())) {
        ompi_output(0, "ompi_proc_init: mca_pcm.pcm_self failed with errno=%d", rc);
        return rc;
    }

    for(i=0; i<npeers; i++) {
        ompi_proc_t *proc = OBJ_NEW(ompi_proc_t);
        proc->proc_name = peers[i];
        if( peers + i == self ) {
            ompi_proc_local_proc = proc;
        }
    }
    return OMPI_SUCCESS;
}


ompi_proc_t** ompi_proc_world(size_t *size)
{
    ompi_proc_t **procs = malloc(ompi_list_get_size(&ompi_proc_list) * sizeof(ompi_proc_t*));
    ompi_proc_t *proc;
    size_t count = 0;

    if(NULL == procs)
        return NULL;

    /* return only the procs that match this jobid */
    OMPI_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)ompi_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)ompi_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)ompi_list_get_next(proc)) {
        /* TSW - FIX */
        procs[count++] = proc;
    }
    OMPI_THREAD_UNLOCK(&ompi_proc_lock);
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

    OMPI_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)ompi_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)ompi_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)ompi_list_get_next(proc)) {
        OBJ_RETAIN(proc);
        procs[count++] = proc;
    }
    OMPI_THREAD_UNLOCK(&ompi_proc_lock);
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

ompi_proc_t * ompi_proc_find ( const ompi_process_name_t * name )
{
    ompi_proc_t *proc;

    /* return the proc-struct which matches this jobid+process id */
    OMPI_THREAD_LOCK(&ompi_proc_lock);
    for(proc =  (ompi_proc_t*)ompi_list_get_first(&ompi_proc_list); 
        proc != (ompi_proc_t*)ompi_list_get_end(&ompi_proc_list);
        proc =  (ompi_proc_t*)ompi_list_get_next(proc)) {
        if( proc->proc_name.cellid == name->cellid &&
            proc->proc_name.jobid == name->jobid &&
            proc->proc_name.procid == name->procid )
            { 
                break;
            }
    }
    OMPI_THREAD_UNLOCK(&ompi_proc_lock);
    return proc;
}

