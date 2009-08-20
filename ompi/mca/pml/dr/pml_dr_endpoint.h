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
#ifndef MCA_PML_ENDPOINT_H
#define MCA_PML_ENDPOINT_H

#include "ompi/mca/bml/bml.h"
#include "ompi/class/ompi_seq_tracker.h"

BEGIN_C_DECLS
/**
 *  This is the pml level endpoint
 *   simply inherity the bml_base_endpoint and 
 *   add whatever else is needed
 */
struct mca_pml_dr_endpoint_t {
    opal_object_t super;
    ompi_proc_t *proc_ompi; /* back pointer to proc structure */
    mca_bml_base_endpoint_t *bml_endpoint; /* pointer to related bml endpoint */
    int32_t local;  /* local view of the rank */
    int32_t src;   /* peers view of the src rank */
    int32_t dst;   /* peers destination rank */
    ompi_seq_tracker_t seq_sends; /**< Tracks the send vfrags that have been acked */ 
    ompi_seq_tracker_t seq_recvs; /**< Tracks the receive vfrags that have been acked */ 
    ompi_seq_tracker_t seq_recvs_matched; /**< Tracks the received vfrags that have been matched */ 
    int32_t vfrag_seq;             /**< current virtual fragment identifier sequence */
};
typedef struct mca_pml_dr_endpoint_t mca_pml_dr_endpoint_t;
OBJ_CLASS_DECLARATION(mca_pml_dr_endpoint_t);


END_C_DECLS
#endif

