#include "proc.h"


lam_class_info_t lam_proc_cls = {
    "lam_proc_t", 
    &lam_list_cls,
    (class_init_t)lam_proc_init, 
    (class_destroy_t)lam_proc_destroy
};
                                                                                                                            

void lam_proc_init(lam_proc_t* proc)
{
    SUPER_INIT(proc, &lam_list_cls);
    proc->proc_job_id = 0;
    proc->proc_vpid = 0;
    proc->proc_pml = 0;
}

void lam_proc_destroy(lam_proc_t* proc)
{
    SUPER_DESTROY(proc, &lam_list_cls);
}

