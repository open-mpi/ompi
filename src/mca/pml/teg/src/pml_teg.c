/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "pml_teg.h"
#include "pml_teg_proc.h"
#include "pml_teg_ptl.h"
#include "pml_teg_recvreq.h"
#include "pml_teg_sendreq.h"
#include "pml_teg_recvfrag.h"


mca_pml_teg_t mca_pml_teg = {
    {
    mca_pml_teg_add_procs,
    mca_pml_teg_del_procs,
    mca_pml_teg_add_ptls,
    mca_pml_teg_control,
    mca_pml_teg_progress,
    mca_pml_teg_add_comm,
    mca_pml_teg_del_comm,
    mca_pml_teg_irecv_init,
    mca_pml_teg_irecv,
    mca_pml_teg_recv,
    mca_pml_teg_isend_init,
    mca_pml_teg_isend,
    mca_pml_teg_send,
    mca_pml_teg_iprobe,
    mca_pml_teg_probe,
    mca_pml_teg_start
    }
};


int mca_pml_teg_add_comm(ompi_communicator_t* comm)
{
    /* allocate pml specific comm data */
    mca_pml_ptl_comm_t* pml_comm = OBJ_NEW(mca_pml_ptl_comm_t);
    if (NULL == pml_comm) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_pml_ptl_comm_init_size(pml_comm, comm->c_remote_group->grp_proc_count);
    comm->c_pml_comm = pml_comm;
    return OMPI_SUCCESS;
}

int mca_pml_teg_del_comm(ompi_communicator_t* comm)
{
    OBJ_RELEASE(comm->c_pml_comm);
    comm->c_pml_comm = 0;
    return OMPI_SUCCESS;
}

static int ptl_exclusivity_compare(const void* arg1, const void* arg2)
{
    mca_ptl_base_module_t* ptl1 = *(struct mca_ptl_base_module_t**)arg1;
    mca_ptl_base_module_t* ptl2 = *(struct mca_ptl_base_module_t**)arg2;
    if( ptl1->ptl_exclusivity > ptl2->ptl_exclusivity ) {
        return -1;
    } else if (ptl1->ptl_exclusivity == ptl2->ptl_exclusivity ) {
        return 0;
    } else {
        return 1;
    }
}


int mca_pml_teg_add_ptls(ompi_list_t *ptls)
{
    /* build an array of ptls and ptl modules */
    mca_ptl_base_selected_module_t* selected_ptl;
    size_t num_ptls = ompi_list_get_size(ptls);
    size_t cache_bytes = 0;
    mca_pml_teg.teg_num_ptl_modules = 0;
    mca_pml_teg.teg_num_ptl_components = 0;
    mca_pml_teg.teg_ptl_modules = (mca_ptl_base_module_t **)malloc(sizeof(mca_ptl_base_module_t*) * num_ptls);
    mca_pml_teg.teg_ptl_components = (mca_ptl_base_component_t **)malloc(sizeof(mca_ptl_base_component_t*) * num_ptls);
    if (NULL == mca_pml_teg.teg_ptl_modules || NULL == mca_pml_teg.teg_ptl_components) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(selected_ptl =  (mca_ptl_base_selected_module_t*)ompi_list_get_first(ptls);
        selected_ptl != (mca_ptl_base_selected_module_t*)ompi_list_get_end(ptls);
        selected_ptl =  (mca_ptl_base_selected_module_t*)ompi_list_get_next(selected_ptl)) {
        mca_ptl_base_module_t *ptl = selected_ptl->pbsm_module;
        size_t i;

        mca_pml_teg.teg_ptl_modules[mca_pml_teg.teg_num_ptl_modules++] = ptl;
        for(i=0; i<mca_pml_teg.teg_num_ptl_components; i++) {
          if(mca_pml_teg.teg_ptl_components[i] == ptl->ptl_component) {
                break;
          }
        }
        if(i == mca_pml_teg.teg_num_ptl_components) {
            mca_pml_teg.teg_ptl_components[mca_pml_teg.teg_num_ptl_components++] = ptl->ptl_component;
        }

        /* 
         *setup ptl 
         */

        /* set pointer to fragment matching logic routine, if this
         *   not already set by the ptl */
       if( NULL == ptl->ptl_match)
           ptl->ptl_match = mca_pml_teg_recv_frag_match;
         ptl->ptl_send_progress = mca_pml_teg_send_request_progress;
         ptl->ptl_recv_progress = mca_pml_teg_recv_request_progress;
         ptl->ptl_stack = ptl;
         ptl->ptl_base = NULL;

         /* find maximum required size for cache */
         if(ptl->ptl_cache_bytes > cache_bytes) {
             cache_bytes = ptl->ptl_cache_bytes;
         }
    }

    /* setup send fragments based on largest required send request */
    ompi_free_list_init(
        &mca_pml_teg.teg_send_requests,
        sizeof(mca_pml_teg_send_request_t) + cache_bytes,
        OBJ_CLASS(mca_pml_teg_send_request_t),
        mca_pml_teg.teg_free_list_num,
        mca_pml_teg.teg_free_list_max,
        mca_pml_teg.teg_free_list_inc,
        NULL);

    /* sort ptl list by exclusivity */
    qsort(mca_pml_teg.teg_ptl_modules, mca_pml_teg.teg_num_ptl_modules, sizeof(struct mca_ptl_t*), ptl_exclusivity_compare);
    return OMPI_SUCCESS;
}

/*
 * Pass control information through to all PTL modules.
 */

int mca_pml_teg_control(int param, void* value, size_t size)
{
    size_t i=0;
    for(i=0; i<mca_pml_teg.teg_num_ptl_components; i++) {
        if(NULL != mca_pml_teg.teg_ptl_components[i]->ptlm_control) {
            int rc = mca_pml_teg.teg_ptl_components[i]->ptlm_control(param,value,size);
            if(rc != OMPI_SUCCESS)
                return rc;
        }
    }
    return OMPI_SUCCESS;
}

/*
 *   For each proc setup a datastructure that indicates the PTLs
 *   that can be used to reach the destination.
 *
 */

int mca_pml_teg_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t p;
    ompi_bitmap_t reachable;
    struct mca_ptl_base_peer_t** ptl_peers = NULL;
    int rc;
    size_t p_index;

    OBJ_CONSTRUCT(&reachable, ompi_bitmap_t);
    rc = ompi_bitmap_init(&reachable, nprocs);
    if(OMPI_SUCCESS != rc)
        return rc;

    /* iterate through each of the procs and set the peers architecture */
    for(p=0; p<nprocs; p++) {
        uint32_t* proc_arch;
        size_t size = sizeof(uint32_t);
        rc = mca_base_modex_recv(&mca_pml_teg_component.pmlm_version, procs[p], 
            (void**)&proc_arch, &size);
        if(rc != OMPI_SUCCESS) 
            return rc;
        if(size != sizeof(uint32_t))
            return OMPI_ERROR;
        procs[p]->proc_arch = ntohl(*proc_arch);
        free(proc_arch);
    }
    
    /* attempt to add all procs to each ptl */
    ptl_peers = (mca_ptl_base_peer_t **)malloc(nprocs * sizeof(struct mca_ptl_base_peer_t*));
    for(p_index = 0; p_index < mca_pml_teg.teg_num_ptl_modules; p_index++) {
        mca_ptl_base_module_t* ptl = mca_pml_teg.teg_ptl_modules[p_index];

        /* if the ptl can reach the destination proc it sets the
         * corresponding bit (proc index) in the reachable bitmap
         * and can return addressing information for each proc
         * that is passed back to the ptl on data transfer calls
         */
        ompi_bitmap_clear_all_bits(&reachable);
        memset(ptl_peers, 0, nprocs * sizeof(struct mca_ptl_base_peer_t*));
        rc = ptl->ptl_add_procs(ptl, nprocs, procs, ptl_peers, &reachable);
        if(OMPI_SUCCESS != rc) {
            free(ptl_peers);
            return rc;
        }

        /* for each proc that is reachable - add the ptl to the procs array(s) */
        for(p=0; p<nprocs; p++) {
            if(ompi_bitmap_is_set_bit(&reachable, p)) {
                ompi_proc_t *proc = procs[p];
                mca_pml_proc_t* proc_pml = proc->proc_pml;
                mca_ptl_proc_t* proc_ptl;
                size_t size;

                /* initialize each proc */
                if(NULL == proc_pml) {

                    /* allocate pml specific proc data */
                    proc_pml = OBJ_NEW(mca_pml_teg_proc_t);
                    if (NULL == proc_pml) {
                        ompi_output(0, "mca_pml_teg_add_procs: unable to allocate resources");
                        free(ptl_peers);
                        return OMPI_ERR_OUT_OF_RESOURCE;
                    }

                    /* preallocate space in array for max number of ptls */
                    mca_ptl_array_reserve(&proc_pml->proc_ptl_first, mca_pml_teg.teg_num_ptl_modules);
                    mca_ptl_array_reserve(&proc_pml->proc_ptl_next, mca_pml_teg.teg_num_ptl_modules);
                    proc_pml->proc_ompi = proc;
                    proc->proc_pml = proc_pml;
                }

                /* dont allow an additional PTL with a lower exclusivity ranking */
                size = mca_ptl_array_get_size(&proc_pml->proc_ptl_next);
                if(size > 0) {
                    proc_ptl = mca_ptl_array_get_index(&proc_pml->proc_ptl_next, size-1);
                    /* skip this ptl if the exclusivity is less than the previous */
                    if(proc_ptl->ptl->ptl_exclusivity > ptl->ptl_exclusivity) {
                        if(ptl_peers[p] != NULL) {
                            ptl->ptl_del_procs(ptl, 1, &proc, &ptl_peers[p]);
                        }
                        continue;
                    }
                }
               
                /* cache the ptl on the proc */
                proc_ptl = mca_ptl_array_insert(&proc_pml->proc_ptl_next);
                proc_ptl->ptl = ptl;
                proc_ptl->ptl_peer = ptl_peers[p];
                proc_ptl->ptl_weight = 0;
                proc_pml->proc_ptl_flags |= ptl->ptl_flags;
            }
        }
    }
    free(ptl_peers);

    /* iterate back through procs and compute metrics for registered ptls */
    for(p=0; p<nprocs; p++) {
        ompi_proc_t *proc = procs[p];
        mca_pml_proc_t* proc_pml = proc->proc_pml;
        double total_bandwidth = 0;
        uint32_t latency = 0;
        size_t n_index;
        size_t n_size;

        /* skip over procs w/ no ptls registered */
        if(NULL == proc_pml)
            continue;

        /* (1) determine the total bandwidth available across all ptls
         *     note that we need to do this here, as we may already have ptls configured
         * (2) determine the highest priority ranking for latency
         */
        n_size = mca_ptl_array_get_size(&proc_pml->proc_ptl_next); 
        for(n_index = 0; n_index < n_size; n_index++) {
            struct mca_ptl_proc_t* proc_ptl = mca_ptl_array_get_index(&proc_pml->proc_ptl_next, n_index);
            struct mca_ptl_base_module_t* ptl = proc_ptl->ptl;
            total_bandwidth += proc_ptl->ptl->ptl_bandwidth; 
            if(ptl->ptl_latency > latency)
                latency = ptl->ptl_latency;
        }

        /* (1) set the weight of each ptl as a percentage of overall bandwidth
         * (2) copy all ptl instances at the highest priority ranking into the
         *     list of ptls used for first fragments
         */

        for(n_index = 0; n_index < n_size; n_index++) {
            struct mca_ptl_proc_t* proc_ptl = mca_ptl_array_get_index(&proc_pml->proc_ptl_next, n_index);
            struct mca_ptl_base_module_t *ptl = proc_ptl->ptl;
            double weight;

            /* compute weighting factor for this ptl */
            if(ptl->ptl_bandwidth)
                weight = proc_ptl->ptl->ptl_bandwidth / total_bandwidth;
            else
                weight = 1.0 / n_size;
            proc_ptl->ptl_weight = (int)(weight * 100);

            /*
             * save/create ptl extension for use by pml
             */
            proc_ptl->ptl_base = ptl->ptl_base;
            if (NULL == proc_ptl->ptl_base && 
                ptl->ptl_cache_bytes > 0 &&
                NULL != ptl->ptl_request_init &&
                NULL != ptl->ptl_request_fini) {

                mca_pml_base_ptl_t* ptl_base = OBJ_NEW(mca_pml_base_ptl_t);
                ptl_base->ptl = ptl;
                ptl_base->ptl_cache_size = ptl->ptl_cache_size;
                proc_ptl->ptl_base = ptl->ptl_base = ptl_base;
            }

            /* check to see if this ptl is already in the array of ptls used for first
             * fragments - if not add it.
             */
            if(ptl->ptl_latency == latency) {
                struct mca_ptl_proc_t* proc_new = mca_ptl_array_insert(&proc_pml->proc_ptl_first);
                *proc_new = *proc_ptl;
            }
        
        }
    }
    return OMPI_SUCCESS;
}

/*
 * iterate through each proc and notify any PTLs associated
 * with the proc that it is/has gone away
 */

int mca_pml_teg_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t p;
    int rc;
    for(p = 0; p < nprocs; p++) {
        ompi_proc_t *proc = procs[p];
        mca_pml_proc_t* proc_pml = proc->proc_pml;
        size_t f_index, f_size;
        size_t n_index, n_size;
 
        /* notify each ptl that the proc is going away */
        f_size = mca_ptl_array_get_size(&proc_pml->proc_ptl_first);
        for(f_index = 0; f_index < f_size; f_index++) {
            mca_ptl_proc_t* ptl_proc = mca_ptl_array_get_index(&proc_pml->proc_ptl_first, f_index);
            mca_ptl_base_module_t* ptl = ptl_proc->ptl;
            
            rc = ptl->ptl_del_procs(ptl,1,&proc,&ptl_proc->ptl_peer);
            if(OMPI_SUCCESS != rc) {
                return rc;
            }

            /* remove this from next array so that we dont call it twice w/ 
             * the same address pointer
             */
            n_size = mca_ptl_array_get_size(&proc_pml->proc_ptl_first);
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
            mca_ptl_base_module_t* ptl = ptl_proc->ptl;
            if (ptl != 0) {
                rc = ptl->ptl_del_procs(ptl,1,&proc,&ptl_proc->ptl_peer);
                if(OMPI_SUCCESS != rc)
                    return rc;
            }
        }
        
        /* do any required cleanup */
        OBJ_RELEASE(proc_pml);
        proc->proc_pml = NULL;
    }
    return OMPI_SUCCESS;
}

int mca_pml_teg_component_fini(void)
{
    /* FIX */
    return OMPI_SUCCESS;
}

