/*
 * Copyright (c) 2006-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_EXAMPLE_H_HAS_BEEN_INCLUDED
#define PML_EXAMPLE_H_HAS_BEEN_INCLUDED

#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/ptl/ptl.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_pml_example_t {
    mca_pml_base_module_t super;

    mca_ptl_base_component_t **example_ptl_components;
    size_t example_num_ptl_components;

    mca_ptl_base_module_t** example_ptl_modules;
    size_t example_num_ptl_modules;

    opal_list_t  example_procs;
    opal_mutex_t example_lock;

    /* list of pending send requests */
    opal_list_t example_send_pending;
};
typedef struct mca_pml_example_t mca_pml_example_t;

extern mca_pml_example_t mca_pml_example;

/*
 * PML interface functions.
 */
extern int mca_pml_example_add_comm( struct ompi_communicator_t* comm );
extern int mca_pml_example_del_comm( struct ompi_communicator_t* comm );

extern int mca_pml_example_add_procs( struct ompi_proc_t **procs, size_t nprocs );
extern int mca_pml_example_del_procs( struct ompi_proc_t **procs, size_t nprocs );

extern int mca_pml_example_add_ptls( opal_list_t *ptls );

extern int mca_pml_example_control( int param, void *size, size_t value );

extern int mca_pml_example_iprobe( int dst,
                               int tag,
                               struct ompi_communicator_t* comm,
                               int *matched,
                               ompi_status_public_t* status );

extern int mca_pml_example_probe( int dst,
                              int tag,
                              struct ompi_communicator_t* comm,
                              ompi_status_public_t* status );

extern int mca_pml_example_cancel( ompi_request_t* request );
extern int mca_pml_example_cancelled( ompi_request_t* request, int *flag );

extern int mca_pml_example_isend_init( void *buf,
                                   size_t count,
                                   ompi_datatype_t *datatype,
                                   int dst,
                                   int tag,
                                   mca_pml_base_send_mode_t mode,
                                   struct ompi_communicator_t* comm,
                                   struct ompi_request_t **request );

extern int mca_pml_example_isend( void *buf,
                              size_t count,
                              ompi_datatype_t *datatype,
                              int dst,
                              int tag,
                              mca_pml_base_send_mode_t mode,
                              struct ompi_communicator_t* comm,
                              struct ompi_request_t **request );

extern int mca_pml_example_send( void *buf,
                             size_t count,
                             ompi_datatype_t *datatype,
                             int dst,
                             int tag,
                             mca_pml_base_send_mode_t mode,
                             struct ompi_communicator_t* comm );

extern int mca_pml_example_irecv_init( void *buf,
                                   size_t count,
                                   ompi_datatype_t *datatype,
                                   int src,
                                   int tag,
                                   struct ompi_communicator_t* comm,
                                   struct ompi_request_t **request );

extern int mca_pml_example_irecv( void *buf,
                              size_t count,
                              ompi_datatype_t *datatype,
                              int src,
                              int tag,
                              struct ompi_communicator_t* comm,
                              struct ompi_request_t **request );

extern int mca_pml_example_recv( void *buf,
                             size_t count,
                             ompi_datatype_t *datatype,
                             int src,
                             int tag,
                             struct ompi_communicator_t* comm,
                             ompi_status_public_t* status );

extern int mca_pml_example_progress(void);

extern int mca_pml_example_start( size_t count, ompi_request_t** requests );

extern int mca_pml_example_ft_event(int state);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* PML_EXAMPLE_H_HAS_BEEN_INCLUDED */
