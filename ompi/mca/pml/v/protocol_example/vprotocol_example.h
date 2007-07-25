/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef __INCLUDE_VPROTOCOL_EXAMPLE_H__
#define __INCLUDE_VPROTOCOL_EXAMPLE_H__

#include "ompi_config.h"
#include "../pml_v.h"
#include "../pml_v_protocol.h"

#include "vprotocol_example_wait.h"
#include "ompi/communicator/communicator.h"

typedef struct mca_vprotocol_example_module_t {
  mca_pml_v_protocol_base_module_t super;
/**
  * Insert here your own protocol structures
  */
} mca_vprotocol_example_module_t;

extern mca_vprotocol_example_module_t mca_vprotocol_example;

OMPI_DECLSPEC int mca_vprotocol_example_add_procs(struct ompi_proc_t **procs, size_t nprocs);
OMPI_DECLSPEC int mca_vprotocol_example_del_procs(struct ompi_proc_t **procs, size_t nprocs);
OMPI_DECLSPEC int mca_vprotocol_example_enable(bool enable);
OMPI_DECLSPEC int mca_vprotocol_example_progress(void);
OMPI_DECLSPEC int mca_vprotocol_example_add_comm(struct ompi_communicator_t* comm);
OMPI_DECLSPEC int mca_vprotocol_example_del_comm(struct ompi_communicator_t* comm);

OMPI_DECLSPEC int mca_vprotocol_example_irecv_init(void *buf,                           
                                            size_t count,                         
                                            struct ompi_datatype_t *datatype,              
                                            int src,
                                            int tag,                                
                                            struct ompi_communicator_t* comm,
                                            struct ompi_request_t **request );
OMPI_DECLSPEC int mca_vprotocol_example_irecv(void *addr,
                                            size_t count,
                                            ompi_datatype_t * datatype,
                                            int src,
                                            int tag,
                                            struct ompi_communicator_t *comm,
                                            struct ompi_request_t **request );
OMPI_DECLSPEC int mca_vprotocol_example_recv(void *addr,
                                            size_t count,
                                            ompi_datatype_t * datatype,
                                            int src,
                                            int tag,
                                            struct ompi_communicator_t *comm,
                                            ompi_status_public_t * status );

OMPI_DECLSPEC int mca_vprotocol_example_isend_init(void *buf,
                                            size_t count,
                                            struct ompi_datatype_t *datatype,
                                            int dst,
                                            int tag,
                                            mca_pml_base_send_mode_t mode,
                                            struct ompi_communicator_t* comm,
                                            struct ompi_request_t **request );
OMPI_DECLSPEC int mca_vprotocol_example_isend(void *buf,
                                            size_t count,
                                            ompi_datatype_t* datatype,
                                            int dst,
                                            int tag,
                                            mca_pml_base_send_mode_t sendmode,
                                            ompi_communicator_t* comm,
                                            ompi_request_t** request );
OMPI_DECLSPEC int mca_vprotocol_example_send(void *buf,
                                            size_t count,
                                            ompi_datatype_t* datatype,
                                            int dst,
                                            int tag,
                                            mca_pml_base_send_mode_t sendmode,
                                            ompi_communicator_t* comm );

OMPI_DECLSPEC int mca_vprotocol_example_iprobe(int src, int tag,
                                            struct ompi_communicator_t *comm,
                                            int *matched, ompi_status_public_t * status );
OMPI_DECLSPEC int mca_vprotocol_example_probe(int src, int tag,
                                            struct ompi_communicator_t *comm,
                                            ompi_status_public_t * status );

OMPI_DECLSPEC int mca_vprotocol_example_start(size_t count,
                                            struct ompi_request_t** requests );

OMPI_DECLSPEC int mca_vprotocol_example_dump(struct ompi_communicator_t* comm,
                                            int verbose );

#endif /* __INCLUDE_VPROTOCOL_EXAMPLE_H__ */
