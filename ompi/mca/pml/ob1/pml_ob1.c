/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "class/ompi_bitmap.h"
#include "mca/pml/pml.h"
#include "mca/btl/btl.h"
#include "mca/btl/base/base.h"
#include "pml_ob1.h"
#include "pml_ob1_component.h"
#include "pml_ob1_comm.h"
#include "pml_ob1_proc.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_recvfrag.h"


mca_pml_ob1_t mca_pml_ob1 = {
    {
    mca_pml_ob1_add_procs,
    mca_pml_ob1_del_procs,
    mca_pml_ob1_enable,
    mca_pml_ob1_progress,
    mca_pml_ob1_add_comm,
    mca_pml_ob1_del_comm,
    mca_pml_ob1_irecv_init,
    mca_pml_ob1_irecv,
    mca_pml_ob1_recv,
    mca_pml_ob1_isend_init,
    mca_pml_ob1_isend,
    mca_pml_ob1_send,
    mca_pml_ob1_iprobe,
    mca_pml_ob1_probe,
    mca_pml_ob1_start
    }
};


int mca_pml_ob1_enable(bool enable)
{
    return OMPI_SUCCESS;
}

int mca_pml_ob1_add_comm(ompi_communicator_t* comm)
{
    /* allocate pml specific comm data */
    mca_pml_ob1_comm_t* pml_comm = OBJ_NEW(mca_pml_ob1_comm_t);
    int i;

    if (NULL == pml_comm) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    mca_pml_ob1_comm_init_size(pml_comm, comm->c_remote_group->grp_proc_count);
    comm->c_pml_comm = pml_comm;
    comm->c_pml_procs = (mca_pml_ob1_proc_t**)malloc(
        comm->c_remote_group->grp_proc_count * sizeof(mca_pml_ob1_proc_t));
    if(NULL == comm->c_pml_procs) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(i=0; i<comm->c_remote_group->grp_proc_count; i++)
        comm->c_pml_procs[i] = comm->c_remote_group->grp_proc_pointers[i]->proc_pml;
    return OMPI_SUCCESS;
}

int mca_pml_ob1_del_comm(ompi_communicator_t* comm)
{
    OBJ_RELEASE(comm->c_pml_comm);
    comm->c_pml_comm = NULL;
    if(comm->c_pml_procs != NULL)
        free(comm->c_pml_procs);
    comm->c_pml_procs = NULL;
    return OMPI_SUCCESS;
}

static int btl_exclusivity_compare(const void* arg1, const void* arg2)
{
    mca_btl_base_module_t* btl1 = *(struct mca_btl_base_module_t**)arg1;
    mca_btl_base_module_t* btl2 = *(struct mca_btl_base_module_t**)arg2;
    if( btl1->btl_exclusivity > btl2->btl_exclusivity ) {
        return -1;
    } else if (btl1->btl_exclusivity == btl2->btl_exclusivity ) {
        return 0;
    } else {
        return 1;
    }
}


int mca_pml_ob1_add_btls()
{
    /* build an array of ob1s and ob1 modules */
    opal_list_t* btls = &mca_btl_base_modules_initialized;
    mca_btl_base_selected_module_t* selected_btl;
    size_t num_btls = opal_list_get_size(btls);

    mca_pml_ob1.num_btl_modules = 0;
    mca_pml_ob1.num_btl_progress = 0;
    mca_pml_ob1.num_btl_components = 0;
    mca_pml_ob1.btl_modules = (mca_btl_base_module_t **)malloc(sizeof(mca_btl_base_module_t*) * num_btls);
    mca_pml_ob1.btl_progress = (mca_btl_base_component_progress_fn_t*)malloc(sizeof(mca_btl_base_component_progress_fn_t) * num_btls);
    mca_pml_ob1.btl_components = (mca_btl_base_component_t **)malloc(sizeof(mca_btl_base_component_t*) * num_btls);

    if (NULL == mca_pml_ob1.btl_modules || 
        NULL == mca_pml_ob1.btl_progress ||
        NULL == mca_pml_ob1.btl_components) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(selected_btl =  (mca_btl_base_selected_module_t*)opal_list_get_first(btls);
        selected_btl != (mca_btl_base_selected_module_t*)opal_list_get_end(btls);
        selected_btl =  (mca_btl_base_selected_module_t*)opal_list_get_next(selected_btl)) {
        mca_btl_base_module_t *btl = selected_btl->btl_module;
        size_t i;
        int rc;

        mca_pml_ob1.btl_modules[mca_pml_ob1.num_btl_modules++] = btl;
        for(i=0; i<mca_pml_ob1.num_btl_components; i++) {
          if(mca_pml_ob1.btl_components[i] == btl->btl_component) {
                break;
          }
        }

        /* override eager limit larger than our max */
        if(btl->btl_eager_limit > mca_pml_ob1.eager_limit) {
            btl->btl_eager_limit = mca_pml_ob1.eager_limit;
        }

        /* setup callback for receive */
        rc = btl->btl_register(btl, MCA_BTL_TAG_PML, mca_pml_ob1_recv_frag_callback, NULL);
        if(OMPI_SUCCESS != rc)
            return rc;

        if(i == mca_pml_ob1.num_btl_components) {
            mca_pml_ob1.btl_components[mca_pml_ob1.num_btl_components++] = btl->btl_component;
        }
    }

    /* initialize free list of receive buffers */
    ompi_free_list_init(
        &mca_pml_ob1.buffers,
        sizeof(mca_pml_ob1_buffer_t) + mca_pml_ob1.eager_limit,
        OBJ_CLASS(mca_pml_ob1_buffer_t),
        mca_pml_ob1.free_list_num,
        mca_pml_ob1.free_list_max,
        mca_pml_ob1.free_list_inc,
        NULL);

    /* sort ob1 list by exclusivity */
    qsort(mca_pml_ob1.btl_modules, 
          mca_pml_ob1.num_btl_modules, 
          sizeof(struct mca_btl_base_module_t*), 
          btl_exclusivity_compare);
    return OMPI_SUCCESS;
}


/*
 *   For each proc setup a datastructure that indicates the PTLs
 *   that can be used to reach the destination.
 *
 */

int mca_pml_ob1_add_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t p;
    ompi_bitmap_t reachable;
    struct mca_btl_base_endpoint_t** btl_endpoints = NULL;
    int rc;
    size_t p_index;
    
    if(nprocs == 0)
        return OMPI_SUCCESS;

    OBJ_CONSTRUCT(&reachable, ompi_bitmap_t);
    rc = ompi_bitmap_init(&reachable, nprocs);
    if(OMPI_SUCCESS != rc)
        return rc;

    /* iterate through each of the procs and set the peers architecture */
    for(p=0; p<nprocs; p++) {
        uint32_t* proc_arch;
        size_t size = sizeof(uint32_t);
        rc = mca_base_modex_recv(&mca_pml_ob1_component.pmlm_version, procs[p], 
            (void**)&proc_arch, &size);
        if(rc != OMPI_SUCCESS) 
            return rc;
        if(size != sizeof(uint32_t))
            return OMPI_ERROR;
        procs[p]->proc_arch = ntohl(*proc_arch);
        free(proc_arch);
    }
    
    /* attempt to add all procs to each ob1 */
    btl_endpoints = (struct mca_btl_base_endpoint_t **)malloc(nprocs * sizeof(struct mca_btl_base_endpoint_t*));
    for(p_index = 0; p_index < mca_pml_ob1.num_btl_modules; p_index++) {
        mca_btl_base_module_t* btl = mca_pml_ob1.btl_modules[p_index];
        int btl_inuse = 0;

        /* if the ob1 can reach the destination proc it sets the
         * corresponding bit (proc index) in the reachable bitmap
         * and can return addressing information for each proc
         * that is passed back to the ob1 on data transfer calls
         */
        ompi_bitmap_clear_all_bits(&reachable);
        memset(btl_endpoints, 0, nprocs * sizeof(struct mca_ob1_base_endpoint_t*));
        rc = btl->btl_add_procs(btl, nprocs, procs, btl_endpoints, &reachable);
        if(OMPI_SUCCESS != rc) {
            free(btl_endpoints);
            return rc;
        }

        /* for each proc that is reachable - add the ob1 to the procs array(s) */
        for(p=0; p<nprocs; p++) {
            if(ompi_bitmap_is_set_bit(&reachable, p)) {
                ompi_proc_t *proc = procs[p];
                mca_pml_ob1_proc_t* proc_pml = proc->proc_pml;
                mca_pml_ob1_endpoint_t* endpoint;
                size_t size;

                /* this btl can be used */
                btl_inuse++;

                /* initialize each proc */
                if(NULL == proc_pml) {

                    /* allocate pml specific proc data */
                    proc_pml = OBJ_NEW(mca_pml_ob1_proc_t);
                    if (NULL == proc_pml) {
                        ompi_output(0, "mca_pml_ob1_add_procs: unable to allocate resources");
                        free(btl_endpoints);
                        return OMPI_ERR_OUT_OF_RESOURCE;
                    }

                    /* preallocate space in array for max number of ob1s */
                    mca_pml_ob1_ep_array_reserve(&proc_pml->btl_eager, mca_pml_ob1.num_btl_modules);
                    mca_pml_ob1_ep_array_reserve(&proc_pml->btl_send,  mca_pml_ob1.num_btl_modules);
                    mca_pml_ob1_ep_array_reserve(&proc_pml->btl_rdma,  mca_pml_ob1.num_btl_modules);
                    proc_pml->proc_ompi = proc;
                    proc->proc_pml = proc_pml;
                }

                /* dont allow an additional PTL with a lower exclusivity ranking */
                size = mca_pml_ob1_ep_array_get_size(&proc_pml->btl_send);
                if(size > 0) {
                    endpoint = mca_pml_ob1_ep_array_get_index(&proc_pml->btl_send, size-1);
                    /* skip this btl if the exclusivity is less than the previous */
                    if(endpoint->btl->btl_exclusivity > btl->btl_exclusivity) {
                        if(btl_endpoints[p] != NULL) {
                            btl->btl_del_procs(btl, 1, &proc, &btl_endpoints[p]);
                        }
                        continue;
                    }
                }
               
                /* cache the endpoint on the proc */
                endpoint = mca_pml_ob1_ep_array_insert(&proc_pml->btl_send);
                endpoint->btl = btl;
                endpoint->btl_eager_limit = btl->btl_eager_limit;
                endpoint->btl_min_send_size = btl->btl_min_send_size;
                endpoint->btl_max_send_size = btl->btl_max_send_size;
                endpoint->btl_min_rdma_size = btl->btl_min_rdma_size;
                endpoint->btl_max_rdma_size = btl->btl_max_rdma_size;
                endpoint->btl_cache = NULL;
                endpoint->btl_endpoint = btl_endpoints[p];
                endpoint->btl_weight = 0;
                endpoint->btl_alloc = btl->btl_alloc;
                endpoint->btl_free = btl->btl_free;
                endpoint->btl_prepare_src = btl->btl_prepare_src;
                endpoint->btl_prepare_dst = btl->btl_prepare_dst;
                endpoint->btl_send = btl->btl_send;
                endpoint->btl_put = btl->btl_put;
                endpoint->btl_get = btl->btl_get;
            }
        }
        if(btl_inuse > 0 && NULL != btl->btl_component->btl_progress) {
            size_t p;
            bool found = false;
            for(p=0; p<mca_pml_ob1.num_btl_progress; p++) {
                if(mca_pml_ob1.btl_progress[p] == btl->btl_component->btl_progress) {
                    found = true;
                    break;
                }
            }
            if(found == false) {
                mca_pml_ob1.btl_progress[mca_pml_ob1.num_btl_progress] = 
                    btl->btl_component->btl_progress;
                mca_pml_ob1.num_btl_progress++;
            }
        }
    }
    free(btl_endpoints);

    /* iterate back through procs and compute metrics for registered ob1s */
    for(p=0; p<nprocs; p++) {
        ompi_proc_t *proc = procs[p];
        mca_pml_ob1_proc_t* proc_pml = proc->proc_pml;
        double total_bandwidth = 0;
        uint32_t latency = 0;
        size_t n_index;
        size_t n_size;

        /* skip over procs w/ no ob1s registered */
        if(NULL == proc_pml)
            continue;

        /* (1) determine the total bandwidth available across all ob1s
         *     note that we need to do this here, as we may already have ob1s configured
         * (2) determine the highest priority ranking for latency
         */
        n_size = mca_pml_ob1_ep_array_get_size(&proc_pml->btl_send); 
        for(n_index = 0; n_index < n_size; n_index++) {
            mca_pml_ob1_endpoint_t* endpoint = 
                mca_pml_ob1_ep_array_get_index(&proc_pml->btl_send, n_index);
            mca_btl_base_module_t* ob1 = endpoint->btl;
            total_bandwidth += endpoint->btl->btl_bandwidth; 
            if(ob1->btl_latency > latency)
                latency = ob1->btl_latency;
        }

        /* (1) set the weight of each ob1 as a percentage of overall bandwidth
         * (2) copy all ob1 instances at the highest priority ranking into the
         *     list of ob1s used for first fragments
         */

        for(n_index = 0; n_index < n_size; n_index++) {
            mca_pml_ob1_endpoint_t* endpoint = 
                mca_pml_ob1_ep_array_get_index(&proc_pml->btl_send, n_index);
            mca_btl_base_module_t *ob1 = endpoint->btl;
            double weight;

            /* compute weighting factor for this ob1 */
            if(ob1->btl_bandwidth)
                weight = endpoint->btl->btl_bandwidth / total_bandwidth;
            else
                weight = 1.0 / n_size;
            endpoint->btl_weight = (int)(weight * 100);

            /* check to see if this ob1 is already in the array of ob1s 
             * used for first fragments - if not add it.
             */
            if(ob1->btl_latency == latency) {
                mca_pml_ob1_endpoint_t* ep_new = 
                    mca_pml_ob1_ep_array_insert(&proc_pml->btl_eager);
                *ep_new = *endpoint;
            }

            /* check flags - is rdma prefered */
            if(endpoint->btl->btl_flags & MCA_BTL_FLAGS_RDMA &&
               proc->proc_arch == ompi_proc_local_proc->proc_arch) {
                mca_pml_ob1_endpoint_t* rdma_ep = mca_pml_ob1_ep_array_insert(&proc_pml->btl_rdma);
                *rdma_ep = *endpoint;
            }
        }
    }
    return OMPI_SUCCESS;
}

/*
 * iterate through each proc and notify any PTLs associated
 * with the proc that it is/has gone away
 */

int mca_pml_ob1_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t p;
    int rc;
    for(p = 0; p < nprocs; p++) {
        ompi_proc_t *proc = procs[p];
        mca_pml_ob1_proc_t* proc_pml = proc->proc_pml;
        size_t f_index, f_size;
        size_t n_index, n_size;
 
        /* notify each ob1 that the proc is going away */
        f_size = mca_pml_ob1_ep_array_get_size(&proc_pml->btl_eager);
        for(f_index = 0; f_index < f_size; f_index++) {
            mca_pml_ob1_endpoint_t* endpoint = mca_pml_ob1_ep_array_get_index(&proc_pml->btl_eager, f_index);
            mca_btl_base_module_t* ob1 = endpoint->btl;
            
            rc = ob1->btl_del_procs(ob1,1,&proc,&endpoint->btl_endpoint);
            if(OMPI_SUCCESS != rc) {
                return rc;
            }

            /* remove this from next array so that we dont call it twice w/ 
             * the same address pointer
             */
            n_size = mca_pml_ob1_ep_array_get_size(&proc_pml->btl_eager);
            for(n_index = 0; n_index < n_size; n_index++) {
                mca_pml_ob1_endpoint_t* endpoint = mca_pml_ob1_ep_array_get_index(&proc_pml->btl_send, n_index);
                if(endpoint->btl == ob1) {
                    memset(endpoint, 0, sizeof(mca_pml_ob1_endpoint_t));
                    break;
                }
            }
        }

        /* notify each ob1 that was not in the array of ob1s for first fragments */
        n_size = mca_pml_ob1_ep_array_get_size(&proc_pml->btl_send);
        for(n_index = 0; n_index < n_size; n_index++) {
            mca_pml_ob1_endpoint_t* endpoint = mca_pml_ob1_ep_array_get_index(&proc_pml->btl_eager, n_index);
            mca_btl_base_module_t* ob1 = endpoint->btl;
            if (ob1 != 0) {
                rc = ob1->btl_del_procs(ob1,1,&proc,&endpoint->btl_endpoint);
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

int mca_pml_ob1_component_fini(void)
{
    /* FIX */
    return OMPI_SUCCESS;
}

