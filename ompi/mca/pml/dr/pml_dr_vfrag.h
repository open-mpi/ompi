/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
/**
 * @file
 */
#ifndef OMPI_PML_DR_VFRAG_H_
#define OMPI_PML_DR_VFRAG_H_

#include "ompi_config.h"
#include "opal/event/event.h"
#include "pml_dr.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_dr_vfrag_t {
    opal_list_item_t super;
    ompi_ptr_t vf_send;
    ompi_ptr_t vf_recv;
    uint32_t   vf_id;
    uint16_t   vf_idx;
    uint16_t   vf_len;
    size_t     vf_offset;
    size_t     vf_size;
    size_t     vf_max_send_size;
    uint64_t   vf_ack;
    uint64_t   vf_mask;
    opal_event_t vf_event;
};
typedef struct mca_pml_dr_vfrag_t mca_pml_dr_vfrag_t;

OBJ_CLASS_DECLARATION(mca_pml_dr_vfrag_t);

#define MCA_PML_DR_VFRAG_ALLOC(vfrag,rc)                                   \
do {                                                                       \
    opal_list_item_t* item;                                                \
    OMPI_FREE_LIST_WAIT(&mca_pml_dr.vfrags, item, rc);                     \
    vfrag = (mca_pml_dr_vfrag_t*)item;                                     \
} while(0)


#define MCA_PML_DR_VFRAG_RETURN(vfrag)                                     \
do {                                                                       \
    OMPI_FREE_LIST_RETURN(&mca_pml_dr.vfrags, (opal_list_item_t*)vfrag);   \
} while(0)

#define MCA_PML_DR_VFRAG_WDOG_START(vfrag)                                 \
do {                                                                       \
                                                                           \
} while(0)                                                                          

#define MCA_PML_DR_VFRAG_WDOG_STOP(vfrag)                                  \
do {                                                                       \
                                                                           \
} while(0)


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

