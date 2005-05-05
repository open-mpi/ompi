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
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "pml_uniq_proc.h"
#include "pml_uniq.h"
#include "pml_uniq_component.h"
#include "pml_uniq_ptl.h"
#include "pml_uniq_recvreq.h"
#include "pml_uniq_sendreq.h"
#include "pml_uniq_recvfrag.h"


mca_pml_uniq_t mca_pml_uniq = {
    {
    mca_pml_uniq_add_procs,
    mca_pml_uniq_del_procs,
    mca_pml_uniq_add_ptls,
    mca_pml_uniq_control,
    mca_pml_uniq_progress,
    mca_pml_uniq_add_comm,
    mca_pml_uniq_del_comm,
    mca_pml_uniq_irecv_init,
    mca_pml_uniq_irecv,
    mca_pml_uniq_recv,
    mca_pml_uniq_isend_init,
    mca_pml_uniq_isend,
    mca_pml_uniq_send,
    mca_pml_uniq_iprobe,
    mca_pml_uniq_probe,
    mca_pml_uniq_start
    }
};


int mca_pml_uniq_add_comm(ompi_communicator_t* comm)
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

int mca_pml_uniq_del_comm(ompi_communicator_t* comm)
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


int mca_pml_uniq_add_ptls(ompi_list_t *ptls)
{
    /* build an array of ptls and ptl modules */
    mca_ptl_base_selected_module_t* selected_ptl;
    size_t num_ptls = ompi_list_get_size(ptls);
    size_t cache_bytes = 0;
    mca_pml_uniq.uniq_num_ptl_modules = 0;
    mca_pml_uniq.uniq_num_ptl_progress = 0;
    mca_pml_uniq.uniq_num_ptl_components = 0;
    mca_pml_uniq.uniq_ptl_modules = (mca_ptl_base_module_t **)malloc(sizeof(mca_ptl_base_module_t*) * num_ptls);
    mca_pml_uniq.uniq_ptl_progress = (mca_ptl_base_component_progress_fn_t*)malloc(sizeof(mca_ptl_base_component_progress_fn_t) * num_ptls);
    mca_pml_uniq.uniq_ptl_components = (mca_ptl_base_component_t **)malloc(sizeof(mca_ptl_base_component_t*) * num_ptls);
    if (NULL == mca_pml_uniq.uniq_ptl_modules || 
        NULL == mca_pml_uniq.uniq_ptl_progress ||
        NULL == mca_pml_uniq.uniq_ptl_components) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for(selected_ptl =  (mca_ptl_base_selected_module_t*)ompi_list_get_first(ptls);
        selected_ptl != (mca_ptl_base_selected_module_t*)ompi_list_get_end(ptls);
        selected_ptl =  (mca_ptl_base_selected_module_t*)ompi_list_get_next(selected_ptl)) {
        mca_ptl_base_module_t *ptl = selected_ptl->pbsm_module;
        size_t i;

        mca_pml_uniq.uniq_ptl_modules[mca_pml_uniq.uniq_num_ptl_modules++] = ptl;
        for(i=0; i<mca_pml_uniq.uniq_num_ptl_components; i++) {
          if(mca_pml_uniq.uniq_ptl_components[i] == ptl->ptl_component) {
                break;
          }
        }
        if(i == mca_pml_uniq.uniq_num_ptl_components) {
            mca_pml_uniq.uniq_ptl_components[mca_pml_uniq.uniq_num_ptl_components++] = ptl->ptl_component;
        }

        /* 
         *setup ptl 
         */

        /* set pointer to fragment matching logic routine, if this
         *   not already set by the ptl */
       if( NULL == ptl->ptl_match)
           ptl->ptl_match = mca_pml_uniq_recv_frag_match;
         ptl->ptl_send_progress = mca_pml_uniq_send_request_progress;
         ptl->ptl_recv_progress = mca_pml_uniq_recv_request_progress;
         ptl->ptl_stack = ptl;
         ptl->ptl_base = NULL;

         /* find maximum required size for cache */
         if(ptl->ptl_cache_bytes > cache_bytes) {
             cache_bytes = ptl->ptl_cache_bytes;
         }
    }

    /* setup send fragments based on largest required send request */
    ompi_free_list_init(
        &mca_pml_uniq.uniq_send_requests,
        sizeof(mca_pml_uniq_send_request_t) + cache_bytes,
        OBJ_CLASS(mca_pml_uniq_send_request_t),
        mca_pml_uniq.uniq_free_list_num,
        mca_pml_uniq.uniq_free_list_max,
        mca_pml_uniq.uniq_free_list_inc,
        NULL);

    /* sort ptl list by exclusivity */
    qsort(mca_pml_uniq.uniq_ptl_modules, mca_pml_uniq.uniq_num_ptl_modules, sizeof(struct mca_ptl_t*), ptl_exclusivity_compare);
    return OMPI_SUCCESS;
}

/*
 * Pass control information through to all PTL modules.
 */

int mca_pml_uniq_control(int param, void* value, size_t size)
{
   size_t i;
   for( i = 0; i < mca_pml_uniq.uniq_num_ptl_components; i++ ) {
      if(NULL != mca_pml_uniq.uniq_ptl_components[i]->ptlm_control) {
         int rc = mca_pml_uniq.uniq_ptl_components[i]->ptlm_control(param,value,size);
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

int mca_pml_uniq_add_procs(ompi_proc_t** procs, size_t nprocs)
{
   size_t p;
   ompi_bitmap_t reachable;
   struct mca_ptl_base_peer_t** ptl_peers = NULL;
   int rc;
   size_t p_index;
    
   if( nprocs == 0 )
      return OMPI_SUCCESS;

   OBJ_CONSTRUCT( &reachable, ompi_bitmap_t );
   rc = ompi_bitmap_init( &reachable, nprocs );
   if( OMPI_SUCCESS != rc )
      return rc;

   /* iterate through each of the procs and set the peers architecture */
   for( p = 0; p < nprocs; p++ ) {
      uint32_t* proc_arch;
      size_t size = sizeof(uint32_t);
      rc = mca_base_modex_recv(&mca_pml_uniq_component.pmlm_version, procs[p], 
                               (void**)&proc_arch, &size);
      if(rc != OMPI_SUCCESS) 
         return rc;
      if(size != sizeof(uint32_t))
         return OMPI_ERROR;
      procs[p]->proc_arch = ntohl(*proc_arch);
      free(proc_arch);
   }
    
   /* attempt to add all procs to each ptl */
   ptl_peers = (struct mca_ptl_base_peer_t **)malloc(nprocs * sizeof(struct mca_ptl_base_peer_t*));
   for( p_index = 0; p_index < mca_pml_uniq.uniq_num_ptl_modules; p_index++ ) {
      mca_ptl_base_module_t* ptl = mca_pml_uniq.uniq_ptl_modules[p_index];
      int ptl_inuse = 0;

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
      for( p = 0; p < nprocs; p++) {
         ompi_proc_t *proc;
         mca_pml_proc_t* proc_pml;

         if( !ompi_bitmap_is_set_bit(&reachable, p) ) continue;

         proc = procs[p];
         proc_pml = proc->proc_pml;

         /* this ptl can be used */
         ptl_inuse++;

         /* initialize each proc */
         if(NULL == proc_pml) {

            /* allocate pml specific proc data */
            proc_pml = OBJ_NEW(mca_pml_uniq_proc_t);
            if (NULL == proc_pml) {
               ompi_output(0, "mca_pml_uniq_add_procs: unable to allocate resources");
               free(ptl_peers);
               return OMPI_ERR_OUT_OF_RESOURCE;
            }

            proc_pml->proc_ompi = proc;
            proc->proc_pml = proc_pml;
            /* it's the first PTL so add it to both first and next */
            proc_pml->proc_ptl_flags |= ptl->ptl_flags;
            proc_pml->proc_ptl_first.ptl_peer = ptl_peers[p];
            proc_pml->proc_ptl_first.ptl_base = NULL;
            proc_pml->proc_ptl_first.ptl = ptl;
#if PML_UNIQ_ACCEPT_NEXT_PTL
            proc_pml->proc_ptl_next.ptl_peer = ptl_peers[p];
            proc_pml->proc_ptl_next.ptl_base = NULL;
            proc_pml->proc_ptl_next.ptl = ptl;
#endif  /* PML_UNIQ_ACCEPT_NEXT_PTL */
         } else {
            /* choose the best for first and next. For the first look at the latency when
             * for the next at the maximum bandwidth.
             */
#if PML_UNIQ_ACCEPT_NEXT_PTL
#endif  /* PML_UNIQ_ACCEPT_NEXT_PTL */
         }
         /* dont allow an additional PTL with a lower exclusivity ranking */
         if( NULL != proc_pml->proc_ptl_first.ptl ) {
            if( proc_pml->proc_ptl_first.ptl->ptl_exclusivity > ptl->ptl_exclusivity ) {
               /* skip this ptl if the exclusivity is less than the previous */
               if(ptl_peers[p] != NULL) {
                  ptl->ptl_del_procs(ptl, 1, &proc, &ptl_peers[p]);
               }
               continue;
            }
         }
         proc_pml->proc_ptl_flags |= ptl->ptl_flags;
      }

      if(ptl_inuse > 0 && NULL != ptl->ptl_component->ptlm_progress) {
         size_t p;
         bool found = false;
         for( p = 0; p < mca_pml_uniq.uniq_num_ptl_progress; p++ ) {
            if(mca_pml_uniq.uniq_ptl_progress[p] == ptl->ptl_component->ptlm_progress) {
               found = true;
               break;
            }
         }
         if(found == false) {
            mca_pml_uniq.uniq_ptl_progress[mca_pml_uniq.uniq_num_ptl_progress] = 
               ptl->ptl_component->ptlm_progress;
            mca_pml_uniq.uniq_num_ptl_progress++;
         }
      }
   }
   free(ptl_peers);

   return OMPI_SUCCESS;
}

/*
 * iterate through each proc and notify any PTLs associated
 * with the proc that it is/has gone away
 */

int mca_pml_uniq_del_procs(ompi_proc_t** procs, size_t nprocs)
{
    size_t p;
    int rc;
    for(p = 0; p < nprocs; p++) {
        ompi_proc_t *proc = procs[p];
        mca_pml_proc_t* proc_pml = proc->proc_pml;
        mca_ptl_proc_t* ptl_proc;
        mca_ptl_base_module_t* ptl;
 
        /* If the PTL used for the first fragment and the one use for the others is not
         * the same then we have to remove the processor from both of them.
         */

        ptl_proc = &(proc_pml->proc_ptl_first);
        ptl = ptl_proc->ptl;
        rc = ptl->ptl_del_procs( ptl, 1, &proc, &ptl_proc->ptl_peer );
        if( OMPI_SUCCESS != rc ) {
           return rc;
        }
#if PML_UNIQ_ACCEPT_NEXT_PTL
        if( proc_pml->proc_ptl_first.ptl != proc_pml->proc_ptl_next.ptl ) {
           ptl_proc = &(proc_pml->proc_ptl_next);
           ptl = ptl_proc->ptl;
           rc = ptl->ptl_del_procs( ptl, 1, &proc, &ptl_proc->ptl_peer );
           if( OMPI_SUCCESS != rc ) {
              return rc;
           }
        }
#endif  /* PML_UNIQ_ACCEPT_NEXT_PTL */
        
        /* do any required cleanup */
        OBJ_RELEASE(proc_pml);
        proc->proc_pml = NULL;
    }
    return OMPI_SUCCESS;
}

int mca_pml_uniq_component_fini(void)
{
    /* FIX */
    return OMPI_SUCCESS;
}

