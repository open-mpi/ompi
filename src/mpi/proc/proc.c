#include "lam/threads/mutex.h"
#include "mpi/proc/proc.h"


static lam_list_t  lam_proc_list;
static lam_mutex_t lam_proc_lock;
lam_proc_t* lam_proc_self = 0;


lam_class_info_t lam_proc_t_class_info = {
    "lam_proc_t", 
    CLASS_INFO(lam_list_t),
    (lam_construct_t)lam_proc_construct, 
    (lam_destruct_t)lam_proc_destruct
};

void lam_proc_construct(lam_proc_t* proc)
{
    static int init = 0;
    if(init++ == 0) {
        lam_list_construct(&lam_proc_list);
        lam_mutex_construct(&lam_proc_lock);
    }

    OBJ_CONSTRUCT_SUPER(proc, lam_list_t);
    proc->proc_job = 0;
    proc->proc_vpid = 0;
    proc->proc_pml = 0;

    THREAD_LOCK(&lam_proc_lock);
    lam_list_append(&lam_proc_list, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&lam_proc_lock);
}


void lam_proc_destruct(lam_proc_t* proc)
{
    THREAD_LOCK(&lam_proc_lock);
    lam_list_remove_item(&lam_proc_list, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&lam_proc_lock);
    OBJ_DESTRUCT_SUPER(proc, lam_list_t);
}

