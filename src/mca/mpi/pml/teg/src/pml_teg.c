#include "lam/util/malloc.h"
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


int mca_pml_teg_add_comm(lam_communicator_t* comm)
{
    /* allocate pml specific comm data */
    struct mca_pml_comm_t* pml_comm = (mca_pml_comm_t*)LAM_MALLOC(sizeof(mca_pml_comm_t));
    mca_pml_ptl_comm_init(pml_comm);
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
    mca_pml_teg.teg_ptls = ptls;
    mca_pml_teg.teg_num_ptls = nptls;
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

int mca_pml_teg_module_fini(void)
{
    return LAM_SUCCESS;
}

