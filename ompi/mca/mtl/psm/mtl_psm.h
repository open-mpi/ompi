/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_PSM_H_HAS_BEEN_INCLUDED
#define MTL_PSM_H_HAS_BEEN_INCLUDED

#include "opal/threads/threads.h"
#include "opal/threads/condition.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/util/cmd_line.h"
#include "ompi/request/request.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include <psm.h>
#include <psm_mq.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/* MTL interface functions */
extern int ompi_mtl_psm_add_procs(struct mca_mtl_base_module_t* mtl, 
                          size_t nprocs,
                          struct ompi_proc_t** procs, 
                          struct mca_mtl_base_endpoint_t **mtl_peer_data);
    
extern int ompi_mtl_psm_del_procs(struct mca_mtl_base_module_t* mtl, 
                                 size_t nprocs,
                                 struct ompi_proc_t** procs, 
                                 struct mca_mtl_base_endpoint_t **mtl_peer_data);

int
ompi_mtl_psm_send(struct mca_mtl_base_module_t* mtl, 
                 struct ompi_communicator_t* comm,
                 int dest,
                 int tag,
                 struct ompi_convertor_t *convertor,
                 mca_pml_base_send_mode_t mode);

extern int ompi_mtl_psm_isend(struct mca_mtl_base_module_t* mtl, 
                             struct ompi_communicator_t* comm,
                             int dest,
                             int tag,
                             struct ompi_convertor_t *convertor,
                             mca_pml_base_send_mode_t mode,
                             bool blocking,
                             mca_mtl_request_t * mtl_request);

extern int ompi_mtl_psm_irecv(struct mca_mtl_base_module_t* mtl,
                             struct ompi_communicator_t *comm,
                             int src,
                             int tag,
                             struct ompi_convertor_t *convertor,
                             struct mca_mtl_request_t *mtl_request);
    
    
extern int ompi_mtl_psm_iprobe(struct mca_mtl_base_module_t* mtl, 
                              struct ompi_communicator_t *comm,
                              int src,
                              int tag,
                              int *flag,
                              struct ompi_status_public_t *status);

extern int ompi_mtl_psm_cancel(struct mca_mtl_base_module_t* mtl,
                              struct mca_mtl_request_t *mtl_request, 
                              int flag);
    
extern int ompi_mtl_psm_finalize(struct mca_mtl_base_module_t* mtl);

int ompi_mtl_psm_module_init(void);
    

   
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* MTL_PSM_H_HAS_BEEN_INCLUDED */

