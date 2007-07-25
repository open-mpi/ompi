/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef __INCLUDE_V_PROTOCOL_H_
#define __INCLUDE_V_PROTOCOL_H_

#include "ompi_config.h"
#include "opal/mca/mca.h" 
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * PML_V->PROTOCOL Called by MCA_PML_V framework to initialize the component.
 * 
 * @param priority (OUT) Relative priority or ranking used by MCA to
 * select a component.
 *
 * @param enable_progress_threads (IN) Whether this component is
 * allowed to run a hidden/progress thread or not.
 *
 * @param enable_mpi_threads (IN) Whether support for multiple MPI
 * threads is enabled or not (i.e., MPI_THREAD_MULTIPLE), which
 * indicates whether multiple threads may invoke this component
 * simultaneously or not.
 */
typedef struct mca_pml_v_protocol_base_module_1_0_0_t * (*mca_pml_v_protocol_base_component_init_fn_t)(
    int *priority, 
    bool enable_progress_threads,
    bool enable_mpi_threads);
typedef int (*mca_pml_v_protocol_base_component_finalize_fn_t)(void);

/**
 * PML_V->PROTOCOL Called by VPROTOCOL to get the actual address of the 
 * protocol specific part of the request
 *
 * @param request (IN) the address of an ompi_request
 * @return address of the mca_vprotocol_base_request_t associated with the request
 */ 
#define VPROTOCOL_REQ(req) \
  ((((mca_pml_base_request_t *) req)->req_type == MCA_PML_REQUEST_SEND) \
  ? VPROTOCOL_SEND_REQ(req) \
  : VPROTOCOL_RECV_REQ(req))
#define VPROTOCOL_RECV_REQ(req) \
  (((char *) req) + mca_pml_v.host_pml_req_recv_size)
#define VPROTOCOL_SEND_REQ(req) \
  (((char *) req) + mca_pml_v.host_pml_req_send_size)
  

/**
 * PML_V_PROTOCOL component version and interface functions.
 */
typedef struct mca_pml_v_protocol_base_1_0_0_t
{
  mca_base_component_t pmlm_version;
  mca_base_component_data_1_0_0_t pmlm_data;
  mca_pml_v_protocol_base_component_init_fn_t pmlm_init;
  mca_pml_v_protocol_base_component_finalize_fn_t pmlm_finalize;
} mca_pml_v_protocol_base_component_1_0_0_t;
typedef mca_pml_v_protocol_base_component_1_0_0_t mca_pml_v_protocol_base_component_t;


typedef struct mca_pml_v_protocol_base_module_1_0_0_t
{
  /* PML module stuff */ 
  mca_pml_base_module_add_procs_fn_t        add_procs;
  mca_pml_base_module_del_procs_fn_t        del_procs;
  mca_pml_base_module_enable_fn_t           enable;
  mca_pml_base_module_progress_fn_t         progress;
  mca_pml_base_module_add_comm_fn_t         add_comm;
  mca_pml_base_module_del_comm_fn_t         del_comm;
  mca_pml_base_module_irecv_init_fn_t       irecv_init;
  mca_pml_base_module_irecv_fn_t            irecv;
  mca_pml_base_module_recv_fn_t             recv;
  mca_pml_base_module_isend_init_fn_t       isend_init;
  mca_pml_base_module_isend_fn_t            isend;
  mca_pml_base_module_send_fn_t             send;
  mca_pml_base_module_iprobe_fn_t           iprobe;
  mca_pml_base_module_probe_fn_t            probe;
  mca_pml_base_module_start_fn_t            start;
  mca_pml_base_module_dump_fn_t             dump;

  /* Request extra data */
  opal_class_t *                            req_recv_class;
  opal_class_t *                            req_send_class;
  } mca_pml_v_protocol_base_module_1_0_0_t;
typedef mca_pml_v_protocol_base_module_1_0_0_t mca_pml_v_protocol_base_module_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* __INCLUDE_V_PROTOCOL_H_ */
