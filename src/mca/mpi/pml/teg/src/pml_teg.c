/*
 * $HEADER$
 */

#include <stdlib.h>
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/base.h"
#include "mca/mpi/ptl/base/ptl_base_comm.h"
#include "mca/mpi/ptl/base/ptl_base_header.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"
#include "mca/mpi/ptl/base/ptl_base_sendfrag.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_recvreq.h"
#include "pml_teg_sendreq.h"


mca_pml_teg_t mca_pml_teg = {
    {
    mca_pml_teg_add_comm,
    mca_pml_teg_del_comm,
    mca_pml_teg_add_procs,
    mca_pml_teg_del_procs,
    mca_pml_teg_add_ptls,
    mca_pml_teg_progress,
    mca_pml_teg_irecv_init,
    mca_pml_teg_irecv,
    mca_pml_teg_recv,
    mca_pml_teg_isend_init,
    mca_pml_teg_isend,
    mca_pml_teg_send,
    mca_pml_teg_start,
    mca_pml_teg_test,
    mca_pml_teg_wait,
    }
};


int mca_pml_teg_add_comm(lam_communicator_t* comm)
{
    /* allocate pml specific comm data */
    mca_pml_comm_t* pml_comm = OBJ_NEW(mca_pml_comm_t);
    if (NULL == pml_comm) {
        return LAM_ERR_OUT_OF_RESOURCE;
    }
    mca_pml_ptl_comm_init_size(pml_comm, comm->c_remote_group->grp_proc_count);
    comm->c_pml_comm = pml_comm;
    return LAM_SUCCESS;
}

int mca_pml_teg_del_comm(lam_communicator_t* comm)
{
    OBJ_RELEASE(comm->c_pml_comm);
    comm->c_pml_comm = 0;
    return LAM_SUCCESS;
}

static int ptl_exclusivity_compare(const void* arg1, const void* arg2)
{
    mca_ptl_t* ptl1 = *(struct mca_ptl_t**)arg1;
    mca_ptl_t* ptl2 = *(struct mca_ptl_t**)arg2;
    if( ptl1->ptl_exclusivity > ptl2->ptl_exclusivity )
        return 1;
    else if (ptl1->ptl_exclusivity == ptl2->ptl_exclusivity )
        return 0;
    else
        return -1;
}


int mca_pml_teg_add_ptls(lam_list_t *ptls)
{
    /* build an array of ptls and ptl modules */
    mca_ptl_base_selected_module_t* selected_ptl;
    size_t num_ptls = lam_list_get_size(ptls);
    mca_pml_teg.teg_num_ptls = 0;
    mca_pml_teg.teg_num_ptl_modules = 0;
    mca_pml_teg.teg_ptls = malloc(sizeof(mca_ptl_t*) * num_ptls);
    mca_pml_teg.teg_ptl_modules = malloc(sizeof(mca_ptl_base_module_t*) * num_ptls);
    if (NULL == mca_pml_teg.teg_ptls || NULL == mca_pml_teg.teg_ptl_modules)
        return LAM_ERR_OUT_OF_RESOURCE;

    for(selected_ptl =  (mca_ptl_base_selected_module_t*)lam_list_get_first(ptls);
        selected_ptl != (mca_ptl_base_selected_module_t*)lam_list_get_last(ptls);
        selected_ptl =  (mca_ptl_base_selected_module_t*)lam_list_get_next(selected_ptl)) {
        mca_ptl_t *ptl = selected_ptl->pbsm_actions;
        size_t i;

        mca_pml_teg.teg_ptls[mca_pml_teg.teg_num_ptls++] = ptl;
        for(i=0; i<mca_pml_teg.teg_num_ptl_modules; i++)
            if(mca_pml_teg.teg_ptl_modules[i] == ptl->ptl_module)
                break;
        if(i == mca_pml_teg.teg_num_ptl_modules)
            mca_pml_teg.teg_ptl_modules[mca_pml_teg.teg_num_ptl_modules++] = ptl->ptl_module;

         /* setup ptl */
         ptl->ptl_match = mca_ptl_base_recv_frag_match;
         ptl->ptl_send_progress = mca_pml_teg_send_request_progress;
         ptl->ptl_recv_progress = mca_pml_teg_recv_request_progress;
    }

    /* sort ptl list by exclusivity */
    qsort(mca_pml_teg.teg_ptls, mca_pml_teg.teg_num_ptls, sizeof(struct mca_ptl_t*), ptl_exclusivity_compare);
    return LAM_SUCCESS;
}

/*
 *   For each proc setup a datastructure that indicates the PTLs
 *   that can be used to reach the destination.
 *
 */

int mca_pml_teg_add_procs(lam_proc_t** procs, size_t nprocs)
{
    size_t p;
    for(p=0; p<nprocs; p++) {
        lam_proc_t *proc = procs[p];
        uint64_t total_bandwidth = 0;
        uint32_t latency = 0;
        size_t n_index, p_index; 
        size_t n_size;

        /* initialize each proc */
        mca_pml_proc_t* proc_pml = proc->proc_pml;
        if(NULL == proc_pml) {

            /* allocate pml specific proc data */
            proc_pml = OBJ_NEW(mca_pml_teg_proc_t);
            if (NULL == proc_pml) {
                lam_output(0, "mca_pml_teg_add_procs: unable to allocate resources");
                return LAM_ERR_OUT_OF_RESOURCE;
            }

            /* preallocate space in array for max number of ptls */
            mca_ptl_array_reserve(&proc_pml->proc_ptl_first, mca_pml_teg.teg_num_ptls);
            mca_ptl_array_reserve(&proc_pml->proc_ptl_next, mca_pml_teg.teg_num_ptls);
            proc_pml->proc_lam = proc;
            proc->proc_pml = proc_pml;
        }

        /* allow each ptl to register with the proc */
        for(p_index = 0; p_index < mca_pml_teg.teg_num_ptls; p_index++) {
            mca_ptl_t* ptl = mca_pml_teg.teg_ptls[p_index];
 
            /* if the ptl can reach the destination proc it will return 
             * addressing information that will be cached on the proc, if it
             * cannot reach the proc - but another peer
             */
            struct mca_ptl_base_peer_t* ptl_peer;
            int rc = ptl->ptl_add_proc(ptl, proc, &ptl_peer);
            if(rc == LAM_SUCCESS) {

                /* cache the ptl on the proc */
                mca_ptl_proc_t* ptl_proc  = mca_ptl_array_insert(&proc_pml->proc_ptl_next);
                ptl_proc->ptl = ptl;
                ptl_proc->ptl_peer = ptl_peer;
                ptl_proc->ptl_weight = 0;

                /* if this ptl supports exclusive access then don't allow 
                 * subsequent ptls to register
                 */
                if(ptl->ptl_exclusivity)
                    break;
            }
        }

        /* (1) determine the total bandwidth available across all ptls
         *     note that we need to do this here, as we may already have ptls configured
         * (2) determine the highest priority ranking for latency
         */
        n_size = mca_ptl_array_get_size(&proc_pml->proc_ptl_next); 
        for(n_index = 0; n_index < n_size; n_index++) {
            struct mca_ptl_proc_t* ptl_proc = mca_ptl_array_get_index(&proc_pml->proc_ptl_next, n_index);
            struct mca_ptl_t* ptl = ptl_proc->ptl;
            total_bandwidth += ptl_proc->ptl->ptl_bandwidth; 
            if(ptl->ptl_latency > latency)
                latency = ptl->ptl_latency;
        }

        /* (1) set the weight of each ptl as a percentage of overall bandwidth
         * (2) copy all ptl instances at the highest priority ranking into the
         *     list of ptls used for first fragments
         */

        for(n_index = 0; n_index < n_size; n_index++) {
            struct mca_ptl_proc_t* ptl_proc = mca_ptl_array_get_index(&proc_pml->proc_ptl_next, n_index);
            struct mca_ptl_t *ptl = ptl_proc->ptl;
            if(ptl->ptl_bandwidth)
                ptl_proc->ptl_weight = total_bandwidth / ptl_proc->ptl->ptl_bandwidth;

            /* check to see if this ptl is already in the array of ptls used for first
             * fragments - if not add it.
             */
            if(ptl->ptl_latency == latency) {
                size_t f_index;
                size_t f_size = mca_ptl_array_get_size(&proc_pml->proc_ptl_first);
                for(f_index=0; f_index < f_size; f_index++) {
                    struct mca_ptl_proc_t* existing_proc = mca_ptl_array_get_index(&proc_pml->proc_ptl_first, f_index);
                    if(existing_proc->ptl == ptl) {
                        *existing_proc = *ptl_proc; /* update existing definition */
                        break;
                    }
                }
                /* not found add a new entry */
                if(f_index == f_size) {
                    struct mca_ptl_proc_t* new_proc = mca_ptl_array_insert(&proc_pml->proc_ptl_first);
                    *new_proc = *ptl_proc;
                }
            }
        }
    }
    return LAM_SUCCESS;
}

/*
 * iterate through each proc and notify any PTLs associated
 * with the proc that it is/has gone away
 */

int mca_pml_teg_del_procs(lam_proc_t** procs, size_t nprocs)
{
    size_t p;
    for(p = 0; p < nprocs; p++) {
        lam_proc_t *proc = procs[p];
        mca_pml_proc_t* proc_pml = proc->proc_pml;
        size_t f_index, f_size;
        size_t n_index, n_size;
 
        /* notify each ptl that the proc is going away */
        f_size = mca_ptl_array_get_size(&proc_pml->proc_ptl_first);
        for(f_index = 0; f_index < f_size; f_index++) {
            mca_ptl_proc_t* ptl_proc = mca_ptl_array_get_index(&proc_pml->proc_ptl_first, f_index);
            mca_ptl_t* ptl = ptl_proc->ptl;
            
            ptl->ptl_del_proc(ptl,proc,ptl_proc->ptl_peer);

            /* remove this from next array so that we dont call it twice w/ 
             * the same address pointer
             */
            f_size = mca_ptl_array_get_size(&proc_pml->proc_ptl_first);
            for(n_index = 0; n_index < n_size; n_index++) {
                mca_ptl_proc_t* next_proc = mca_ptl_array_get_index(&proc_pml->proc_ptl_next, n_index);
                if(next_proc->ptl == ptl) {
                    memset(next_proc, 0, sizeof(mca_ptl_proc_t));
                    break;
                }
            }
        }

        /* notify each ptl that was not in the array of ptls for first fragments */
        n_size = mca_ptl_array_get_size(&proc_pml->proc_ptl_next);
        for(n_index = 0; n_index < n_size; n_index++) {
            mca_ptl_proc_t* ptl_proc = mca_ptl_array_get_index(&proc_pml->proc_ptl_first, n_index);
            mca_ptl_t* ptl = ptl_proc->ptl;
            if (ptl != 0)
                ptl->ptl_del_proc(ptl,proc,ptl_proc->ptl_peer);
        }
        
        /* do any required cleanup */
        OBJ_RELEASE(proc_pml);
        proc->proc_pml = NULL;
    }
    return LAM_SUCCESS;
}

int mca_pml_teg_module_fini(void)
{
    /* FIX */
    return LAM_SUCCESS;
}

