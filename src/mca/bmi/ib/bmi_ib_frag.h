/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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

#ifndef MCA_BMI_IB_FRAG_H
#define MCA_BMI_IB_FRAG_H

#include "ompi_config.h"
#include "bmi_ib_priv.h"
#include "bmi_ib.h" 

#include <vapi.h> 
#include <mtl_common.h> 
#include <vapi_common.h> 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_bmi_ib_frag_t);

typedef mca_bmi_base_header_t mca_bmi_ib_header_t; 


typedef enum { 
    MCA_BMI_IB_FRAG_SEND, 
    MCA_BMI_IB_FRAG_PUT, 
    MCA_BMI_IB_FRAG_GET, 
    MCA_BMI_IB_FRAG_ACK 
} mca_bmi_ib_frag_type_t; 


/**
 * IB send fragment derived type.
 */
struct mca_bmi_ib_frag_t {
    mca_bmi_base_descriptor_t base; 
    mca_bmi_base_segment_t segment; 
    struct mca_bmi_base_endpoint_t *endpoint; 
    mca_bmi_ib_frag_type_t type; 
    size_t size; 
    int rc; 
    
    union{ 
        VAPI_rr_desc_t rr_desc; 
        VAPI_sr_desc_t sr_desc; 
    }; 
    VAPI_sg_lst_entry_t sg_entry;  
    VAPI_mr_hndl_t mem_hndl; 
    VAPI_ret_t ret;
    mca_bmi_ib_header_t *hdr; 
}; 
typedef struct mca_bmi_ib_frag_t mca_bmi_ib_frag_t; 
OBJ_CLASS_DECLARATION(mca_bmi_ib_frag_t); 


typedef struct mca_bmi_ib_frag_t mca_bmi_ib_send_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_bmi_ib_send_frag_eager_t); 

typedef struct mca_bmi_ib_frag_t mca_bmi_ib_send_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_bmi_ib_send_frag_max_t); 

typedef struct mca_bmi_ib_frag_t mca_bmi_ib_send_frag_frag_t; 
    
OBJ_CLASS_DECLARATION(mca_bmi_ib_send_frag_frag_t); 

typedef struct mca_bmi_ib_frag_t mca_bmi_ib_recv_frag_t; 
    
OBJ_CLASS_DECLARATION(mca_bmi_ib_recv_frag_t); 

    

/*
 * Allocate an IB send descriptor
 *
 */

#define MCA_BMI_IB_FRAG_ALLOC_EAGER(bmi, frag, rc)                               \
{                                                                      \
                                                                       \
    ompi_list_item_t *item;                                            \
    OMPI_FREE_LIST_WAIT(&((mca_bmi_ib_module_t*)bmi)->send_free_eager, item, rc);       \
    frag = (mca_bmi_ib_frag_t*) item;                                  \
}

#define MCA_BMI_IB_FRAG_RETURN_EAGER(bmi, frag)                                  \
{                                                                      \
    OMPI_FREE_LIST_RETURN(&((mca_bmi_ib_module_t*)bmi)->send_free_eager, (ompi_list_item_t*)(frag)); \
}


#define MCA_BMI_IB_FRAG_ALLOC_MAX(bmi, frag, rc)                               \
{                                                                      \
                                                                       \
    ompi_list_item_t *item;                                            \
    OMPI_FREE_LIST_WAIT(&((mca_bmi_ib_module_t*)bmi)->send_free_max, item, rc);       \
    frag = (mca_bmi_ib_frag_t*) item;                                  \
}

#define MCA_BMI_IB_FRAG_RETURN_MAX(bmi, frag)                                  \
{                                                                      \
    OMPI_FREE_LIST_RETURN(&((mca_bmi_ib_module_t*)bmi)->send_free_max, (ompi_list_item_t*)(frag)); \
}


#define MCA_BMI_IB_FRAG_ALLOC_FRAG(bmi, frag, rc)                               \
{                                                                      \
                                                                       \
    ompi_list_item_t *item;                                            \
    OMPI_FREE_LIST_WAIT(&((mca_bmi_ib_module_t*)bmi)->send_free_frag, item, rc);       \
    frag = (mca_bmi_ib_frag_t*) item;                                  \
}

#define MCA_BMI_IB_FRAG_RETURN_FRAG(bmi, frag)                                  \
{                                                                      \
    OMPI_FREE_LIST_RETURN(&((mca_bmi_ib_module_t*)bmi)->send_free_frag, (ompi_list_item_t*)(frag)); \
}





struct mca_bmi_ib_module_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
