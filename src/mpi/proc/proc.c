#include "lam/threads/mutex.h"
#include "mpi/proc/proc.h"


static lam_list_t  lam_proc_list;
static lam_mutex_t lam_proc_lock;
lam_proc_t* lam_proc_self = 0;


lam_class_info_t lam_proc_cls = {
    "lam_proc_t", 
    &lam_list_cls,
    (class_init_t)lam_proc_init, 
    (class_destroy_t)lam_proc_destroy
};

void lam_proc_init(lam_proc_t* proc)
{
    static int init = 0;
    if(init++ == 0) {
        lam_list_init(&lam_proc_list);
        lam_mutex_init(&lam_proc_lock);
    }

    SUPER_INIT(proc, &lam_list_cls);
    proc->proc_job = 0;
    proc->proc_vpid = 0;
    proc->proc_pml = 0;

    THREAD_LOCK(&lam_proc_lock);
    lam_list_append(&lam_proc_list, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&lam_proc_lock);
}


void lam_proc_destroy(lam_proc_t* proc)
{
    THREAD_LOCK(&lam_proc_lock);
    lam_list_remove_item(&lam_proc_list, (lam_list_item_t*)proc);
    THREAD_UNLOCK(&lam_proc_lock);
    SUPER_DESTROY(proc, &lam_list_cls);
}

