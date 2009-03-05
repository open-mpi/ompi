/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All righs reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <sys/time.h>
#include <time.h>

#include "opal/align.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"

#include "ompi/types.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/pcie/mpool_pcie.h"

#include "btl_pcie.h"
#include "btl_pcie_endpoint.h" 
#include "btl_pcie_proc.h"
#include "btl_pcie_frag.h"

/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_btl_pcie_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl = 0;
    endpoint->endpoint_proc = 0;
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_pcie_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
}


OBJ_CLASS_INSTANCE(
    mca_btl_pcie_endpoint_t, 
    opal_list_item_t, 
    mca_btl_pcie_endpoint_construct, 
    mca_btl_pcie_endpoint_destruct);



/*
 * Initialize an endpoint 
 */ 
int mca_btl_pcie_endpoint_init(mca_btl_base_endpoint_t* endpoint)
{
    int rc;
    mca_btl_pcie_module_t* pcie_btl = 
        endpoint->endpoint_btl;
    mca_mpool_base_resources_t mpool_resources;
    size_t fifo_buffer_len, current_offset = 0;

    /* Open our device */
    rc = dd_open(endpoint->lcl_dev_name, 
                 &endpoint->pcie_adapter);
    if( 0 != rc) { 
        BTL_ERROR(("Failed to open pcie device dd_open says : %d\n", rc));
        return OMPI_ERROR;
    }

    /* fill in endpoint data for begining of resources */
    endpoint->lcl_sma_ptr  = endpoint->pcie_adapter.local_sma_address;
    if(NULL == endpoint->lcl_sma_ptr) { 
        BTL_ERROR(("Error: local sma address is null\n"));
        return OMPI_ERROR;
    }
        
    endpoint->rem_sma_ptr = endpoint->pcie_adapter.remote_sma_address;
    if(NULL == endpoint->rem_sma_ptr) { 
        BTL_ERROR(("Error: remote sma address is null\n"));
        return OMPI_ERROR;
    }

    BTL_VERBOSE(("SMA for device %s: local=0x%lx,%d remote=0x%lx,%d",
               endpoint->lcl_dev_name,
               endpoint->lcl_sma_ptr,
               endpoint->pcie_adapter.local_sma_size,
               endpoint->rem_sma_ptr,
               endpoint->pcie_adapter.remote_sma_size));

    /* 16 bytes of the buffer reserved for the 8 byte local DMA completion */
    endpoint->lcl_dma_status = ((char*) endpoint->lcl_sma_ptr) + current_offset;
    current_offset += 16;

    /* fifo_buffer_len bytes reserved for fifos */
    fifo_buffer_len = sizeof(btl_pcie_fifo_entry_t) * mca_btl_pcie_component.pcie_recv_queue_len;

    rc = ompi_btl_pcie_fifo_init_send(&(endpoint->send_fifo),
				      mca_btl_pcie_component.pcie_recv_queue_len,
				      ((char*) endpoint->rem_sma_ptr) + current_offset);
    if (OMPI_SUCCESS != rc) {
      BTL_ERROR(("Error: Failed to init send fifo: %d", rc));
      return rc;
    }

    rc = ompi_btl_pcie_fifo_init_recv(&(endpoint->recv_fifo),
				      mca_btl_pcie_component.pcie_recv_queue_len,
				      ((char*) endpoint->lcl_sma_ptr) + current_offset,
				      fifo_buffer_len);
    if (OMPI_SUCCESS != rc) {
      BTL_ERROR(("Error: Failed to init recv fifo: %d", rc));
      return rc;
    }

    current_offset += fifo_buffer_len;
    
    /* reserve rest of the space for the mpool */
    endpoint->rem_frag_base = 
        ((char*) endpoint->rem_sma_ptr) + current_offset;
    
    endpoint->lcl_frag_base = 
        ((char*) endpoint->lcl_sma_ptr) + current_offset;
    
    /* don't need to align this one as the free list */ 
    /* will take care of it. */
    mpool_resources.base = endpoint->rem_frag_base;
    mpool_resources.len = endpoint->pcie_adapter.remote_sma_size -
      current_offset;
    
    /* setup my pcie mpool */
    pcie_btl->pcie_mpool = 
        mca_mpool_base_module_create(mca_btl_pcie_component.pcie_send_mpool_name,
                                     pcie_btl,
                                     &mpool_resources);

    /* setup the modules free lists and such as we now */
    /* have enough info to setup the mpool */

    /* eager SMA communication buffers */
#if (OMPI_MAJOR_VERSION <= 1) && (OMPI_MINOR_VERSION <= 2)
    ompi_free_list_init_ex(&(pcie_btl->pcie_sma_buf_eager),
                           sizeof(mca_btl_pcie_sma_buf_eager_t) + 
                           mca_btl_pcie_module.super.btl_eager_limit,
                           sizeof(mca_btl_pcie_sma_buf_eager_t),
			   MCA_BTL_PCIE_FRAG_ALIGN,
                           OBJ_CLASS(mca_btl_pcie_sma_buf_eager_t),
                           mca_btl_pcie_component.pcie_free_list_num,
                           mca_btl_pcie_component.pcie_free_list_max,
                           mca_btl_pcie_component.pcie_free_list_inc,
                           pcie_btl->pcie_mpool);

    /* max size SMA communication buffers */
    ompi_free_list_init_ex(&(pcie_btl->pcie_sma_buf_max),
                           sizeof(mca_btl_pcie_sma_buf_max_t) + 
                           mca_btl_pcie_module.super.btl_max_send_size,
                           sizeof(mca_btl_pcie_sma_buf_max_t),
			   MCA_BTL_PCIE_FRAG_ALIGN,
                           OBJ_CLASS(mca_btl_pcie_sma_buf_max_t),
                           mca_btl_pcie_component.pcie_free_list_num,
                           mca_btl_pcie_component.pcie_free_list_max,
                           mca_btl_pcie_component.pcie_free_list_inc,
                           pcie_btl->pcie_mpool);
    
    /* User eager fragment buffer */
    ompi_free_list_init_ex(&(pcie_btl->pcie_frag_eager),
                           sizeof(mca_btl_pcie_frag_eager_t) + 
                           mca_btl_pcie_module.super.btl_eager_limit,
                           sizeof(mca_btl_pcie_frag_eager_t),
                           MCA_BTL_PCIE_FRAG_ALIGN,
                           OBJ_CLASS(mca_btl_pcie_frag_eager_t),
                           mca_btl_pcie_component.pcie_free_list_num,
                           mca_btl_pcie_component.pcie_free_list_max,
                           mca_btl_pcie_component.pcie_free_list_inc,
                           NULL);

    /* User max size fragment buffer */
    ompi_free_list_init_ex(&(pcie_btl->pcie_frag_max),
                           sizeof(mca_btl_pcie_frag_max_t) + 
                           mca_btl_pcie_module.super.btl_max_send_size,
                           sizeof(mca_btl_pcie_frag_max_t),
                           MCA_BTL_PCIE_FRAG_ALIGN,
                           OBJ_CLASS(mca_btl_pcie_frag_max_t),
                           mca_btl_pcie_component.pcie_free_list_num,
                           mca_btl_pcie_component.pcie_free_list_max,
                           mca_btl_pcie_component.pcie_free_list_inc,
                           NULL);
#else
    ompi_free_list_init_ex(&(pcie_btl->pcie_sma_buf_eager),
                           mca_btl_pcie_module.super.btl_eager_limit,
			   MCA_BTL_PCIE_FRAG_ALIGN,
                           OBJ_CLASS(mca_btl_pcie_sma_buf_eager_t),
                           mca_btl_pcie_component.pcie_free_list_num,
                           mca_btl_pcie_component.pcie_free_list_max,
                           mca_btl_pcie_component.pcie_free_list_inc,
                           pcie_btl->pcie_mpool,
                           NULL,
                           NULL);

    /* max size SMA communication buffers */
    ompi_free_list_init_ex(&(pcie_btl->pcie_sma_buf_max),
                           mca_btl_pcie_module.super.btl_max_send_size,
			   MCA_BTL_PCIE_FRAG_ALIGN,
                           OBJ_CLASS(mca_btl_pcie_sma_buf_max_t),
                           mca_btl_pcie_component.pcie_free_list_num,
                           mca_btl_pcie_component.pcie_free_list_max,
                           mca_btl_pcie_component.pcie_free_list_inc,
                           pcie_btl->pcie_mpool,
                           NULL,
                           NULL);
    
    /* User eager fragment buffer */
    ompi_free_list_init_ex(&(pcie_btl->pcie_frag_eager),
                           mca_btl_pcie_module.super.btl_eager_limit,
                           MCA_BTL_PCIE_FRAG_ALIGN,
                           OBJ_CLASS(mca_btl_pcie_frag_eager_t),
                           mca_btl_pcie_component.pcie_free_list_num,
                           mca_btl_pcie_component.pcie_free_list_max,
                           mca_btl_pcie_component.pcie_free_list_inc,
                           NULL,
                           NULL,
                           NULL);

    /* User max size fragment buffer */
    ompi_free_list_init_ex(&(pcie_btl->pcie_frag_max),
                           mca_btl_pcie_module.super.btl_max_send_size,
                           MCA_BTL_PCIE_FRAG_ALIGN,
                           OBJ_CLASS(mca_btl_pcie_frag_max_t),
                           mca_btl_pcie_component.pcie_free_list_num,
                           mca_btl_pcie_component.pcie_free_list_max,
                           mca_btl_pcie_component.pcie_free_list_inc,
                           NULL,
                           NULL,
                           NULL);
#endif
    
    /* dma frags.  note that we can only have 16 outstanding memory
       handles so we cannot currently support leave_pinned and we must
       limit the number of outstanding DMAs via the free list of DMA
       frags */
    ompi_free_list_init(&(pcie_btl->pcie_frag_dma),
                        sizeof(mca_btl_pcie_frag_dma_t),
                        OBJ_CLASS(mca_btl_pcie_frag_dma_t),
                        16,
                        16,
                        0,
                        NULL);

    /* recv frag */
    OBJ_CONSTRUCT(&(pcie_btl->pcie_recv_frag),
                  mca_btl_pcie_frag_recv_t);

    pcie_btl->endpoint = endpoint;
    pcie_btl->active = true;
    
    return OMPI_SUCCESS;
}

/*
 * Finalize an endpoint 
 */ 
int mca_btl_pcie_endpoint_fini(mca_btl_base_endpoint_t* endpoint)
{
  return OMPI_SUCCESS;
}
