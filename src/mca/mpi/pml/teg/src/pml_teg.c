/*
 * $HEADER$
 */

#include "lam/mem/malloc.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_comm.h"
#include "mca/mpi/ptl/base/ptl_base_sendreq.h"
#include "mca/mpi/ptl/base/ptl_base_recvreq.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"


mca_pml_teg_t mca_pml_teg = {
    {
    mca_pml_teg_add_comm,
    mca_pml_teg_del_comm,
    mca_pml_teg_add_procs,
    mca_pml_teg_del_procs,
    mca_pml_teg_fini,
    mca_pml_teg_progress,
    mca_pml_teg_irecv_init,
    mca_pml_teg_irecv,
    mca_pml_teg_isend_init,
    mca_pml_teg_isend,
    mca_pml_teg_start,
    mca_pml_teg_test,
    mca_pml_teg_wait,
    }
};


int mca_pml_teg_add_comm(lam_communicator_t* comm)
{
    /* allocate pml specific comm data */
    struct mca_pml_comm_t* pml_comm = (mca_pml_comm_t*)LAM_MALLOC(sizeof(mca_pml_comm_t));
    mca_pml_ptl_comm_init(pml_comm, comm->c_remote_group->g_proc_count);
    comm->c_pml_comm = pml_comm;
    return LAM_SUCCESS;
}

int mca_pml_teg_del_comm(lam_communicator_t* comm)
{
    LAM_FREE(comm->c_pml_comm);
    comm->c_pml_comm = 0;
    return LAM_SUCCESS;
}

int mca_pml_teg_add_ptls(struct mca_ptl_t** ptls, size_t nptls)
{
    /* sort the ptls by exclusivity */
    mca_pml_teg.teg_ptls = ptls;
    mca_pml_teg.teg_num_ptls = nptls;
    return LAM_SUCCESS;
}

int mca_pml_teg_add_procs(lam_proc_t** procs, size_t nprocs)
{
    size_t i;
    size_t p;

    for(p=0; p<nprocs; p++) {
        lam_proc_t *proc = procs[p];

        /* initialize each proc */
        mca_pml_proc_t* proc_pml = proc->proc_pml;
        if(proc_pml == 0) {

            /* allocate pml specific proc data */
            proc_pml = (mca_pml_proc_t*)LAM_MALLOC(sizeof(mca_pml_proc_t));
            if(NULL == proc_pml) {
                lam_output(0, "mca_pml_teg_add_procs: unable to allocate resources");
                return LAM_ERR_OUT_OF_RESOURCE;
            }
            mca_pml_teg_proc_init(proc_pml);

            /* preallocate space in array for max number of ptls */
            mca_ptl_array_reserve(&proc_pml->proc_ptl_first, mca_pml_teg.teg_num_ptls);
            mca_ptl_array_reserve(&proc_pml->proc_ptl_next, mca_pml_teg.teg_num_ptls);
            proc_pml->proc_lam = proc;
            proc->proc_pml = proc_pml;
        }

        /* allow each ptl to register with the proc */
        for(i=0; i<mca_pml_teg.teg_num_ptls; i++) {
            mca_ptl_t* ptl = mca_pml_teg.teg_ptls[i];
 
            /* if the ptl can reach the destination proc it will return 
             * addressing information that will be cached on the proc
             */
            struct mca_ptl_addr_t* ptl_addr;
            int rc = ptl->ptl_add_proc(ptl, proc, &ptl_addr);
            if(rc == LAM_SUCCESS) {

                /* cache the ptl on the proc */
                mca_ptl_proc_t* ptl_proc  = mca_ptl_array_insert(&proc_pml->proc_ptl_next);
                ptl_proc->ptl = ptl;
                ptl_proc->ptl_addr = ptl_addr;
                ptl_proc->ptl_weight = 0;

                /* if this ptl supports exclusive access then don't allow 
                 * subsequent ptls to register
                 */
                if(ptl->ptl_exclusive)
                    break;
            }
        }

        /* compute a weighting factor for each ptl */
        for(i=0; i<mca_ptl_array_get_size(&proc_pml->proc_ptl_next); i++) {
            
        }
    }
    return LAM_SUCCESS;
}

int mca_pml_teg_del_procs(lam_proc_t** procs, size_t nprocs)
{
#if 0
    size_t i;
    for(i=0; i<nprocs; i++) {
        lam_proc_t *proc = procs[i];
        mca_pml_proc_t* proc_pml = proc->proc_pml;
 
        /* notify each ptl that the proc is going away */
        size_t p;
        for(p=0; p<proc_pml->proc_ptl_first.ptl_size; p++) {
            mca_ptl_info_t* ptl_info = proc_pml->proc_ptl_first.ptl_array[p];
            ptl_info->ptl->ptl_del_proc(proc);
        }
    }
#endif
    return LAM_SUCCESS;
}

int mca_pml_teg_fini(void)
{
    return LAM_SUCCESS;
}

