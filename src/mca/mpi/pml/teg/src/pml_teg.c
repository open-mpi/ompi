#include "lam/util/malloc.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"

#define mca_pml_teg_param_register_int(n,v) \
    mca_base_param_lookup_int( \
        mca_base_param_register_int("pml","teg",n,0,v))


mca_pml_base_module_1_0_0_t mca_pml_teg_module = {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */
                                                                                                                            
    {
    /* Indicate that we are a pml v1.0.0 module (which also implies a
       specific MCA version) */
                                                                                                                            
    MCA_PML_BASE_VERSION_1_0_0,
                                                                                                                            
    "teg", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_pml_teg_open,  /* module open */
    mca_pml_teg_close  /* module close */
    },
                                                                                                                            
    /* Next the MCA v1.0.0 module meta data */
                                                                                                                            
    {
    /* Whether the module is checkpointable or not */
                                                                                                                            
    false
    },

    mca_pml_teg_init  /* module init */
};
                                                                                                                            

mca_pml_teg_t mca_pml_teg = {
    {
    mca_pml_teg_add_comm,
    mca_pml_teg_del_comm,
    mca_pml_teg_add_procs,
    mca_pml_teg_del_procs,
    mca_pml_teg_add_ptls,
    mca_pml_teg_fini,
    mca_pml_teg_irecv_init,
    mca_pml_teg_irecv,
    mca_pml_teg_isend_init,
    mca_pml_teg_isend,
    mca_pml_teg_progress,
    mca_pml_teg_start,
    mca_pml_teg_test,
    mca_pml_teg_wait,
    }
};

/**
 *  some comment
 *
 *  @param foo description
 *  @return 
 *
 *  long description
 */

int mca_pml_teg_open(lam_cmd_line_t* cmd_line)
{
    return LAM_SUCCESS;
}


mca_pml_t* mca_pml_teg_init(int* priority, int* min_thread, int* max_thread)
{
    *priority = 0;
    *min_thread = 0;
    *max_thread = 0;
    mca_pml_teg.teg_ptls = 0;
    mca_pml_teg.teg_num_ptls = 0;
    lam_list_init(&mca_pml_teg.teg_pending_acks);
    lam_list_init(&mca_pml_teg.teg_incomplete_sends);
    lam_mutex_init(&mca_pml_teg.teg_lock);
    return &mca_pml_teg.super;
}

int mca_pml_teg_add_ptls(struct mca_ptl_t** ptls, size_t nptls)
{
    mca_pml_teg.teg_ptls = ptls;
    mca_pml_teg.teg_num_ptls = nptls;
    return LAM_SUCCESS;
}

int mca_pml_teg_add_comm(lam_communicator_t* comm)
{
    return LAM_SUCCESS;
}

int mca_pml_teg_del_comm(lam_communicator_t* comm)
{
    return LAM_SUCCESS;
}

int mca_pml_teg_add_procs(lam_proc_t** procs, size_t nprocs)
{
    size_t i;
    /* initialize each proc */
    for(i=0; i<nprocs; i++) {
        lam_proc_t *proc = procs[i];
        if(proc->proc_pml == 0) {
            /* allocate pml specific proc data */
            mca_pml_proc_t* proc_pml = (mca_pml_proc_t*)LAM_MALLOC(sizeof(mca_pml_proc_t));
            mca_pml_teg_proc_init(proc_pml);
            /* preallocate space in array for max number of ptls */
            mca_ptl_array_reserve(&proc_pml->proc_ptl_first, mca_pml_teg.teg_num_ptls);
            mca_ptl_array_reserve(&proc_pml->proc_ptl_first, mca_pml_teg.teg_num_ptls);
            proc_pml->proc_lam = proc;
            proc->proc_pml = proc_pml;
        }
    }

    /* allow each ptl to add itself to proc array */
    for(i=0; i<mca_pml_teg.teg_num_ptls; i++) {
        mca_ptl_t* ptl = mca_pml_teg.teg_ptls[i];
        ptl->ptl_add_procs(ptl, procs, nprocs);
    }
    return LAM_SUCCESS;
}

