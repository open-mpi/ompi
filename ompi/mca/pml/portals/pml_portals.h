/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#ifndef PML_PORTALS_H_HAS_BEEN_INCLUDED
#define PML_PORTALS_H_HAS_BEEN_INCLUDED

#include "opal/threads/threads.h"
#include "opal/threads/condition.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/util/cmd_line.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/datatype/datatype.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_pml_portals_t {
    mca_pml_base_module_t super;

    /* free list of requests */
    ompi_free_list_t portals_send_requests;
    ompi_free_list_t portals_recv_requests;

    /* list of pending send requests */
    opal_list_t portals_send_pending;
};
typedef struct mca_pml_portals_t mca_pml_portals_t;
extern mca_pml_portals_t mca_pml_portals;


/*
 * PML interface functions.
 */

extern int mca_pml_portals_add_procs(struct ompi_proc_t **procs, size_t nprocs);
extern int mca_pml_portals_del_procs(struct ompi_proc_t **procs, size_t nprocs);

extern int mca_pml_portals_enable(bool enable);
extern int mca_pml_portals_progress(void);

extern int mca_pml_portals_add_comm(struct ompi_communicator_t* comm);
extern int mca_pml_portals_del_comm(struct ompi_communicator_t* comm);

extern int mca_pml_portals_irecv_init(void *buf,
                                      size_t count,
                                      ompi_datatype_t *datatype,
                                      int src,
                                      int tag,
                                      struct ompi_communicator_t* comm,
                                      struct ompi_request_t **request);

extern int mca_pml_portals_irecv(void *buf,
                                 size_t count,
                                 ompi_datatype_t *datatype,
                                 int src,
                                 int tag,
                                 struct ompi_communicator_t* comm,
                                 struct ompi_request_t **request);

extern int mca_pml_portals_recv(void *buf,
                                size_t count,
                                ompi_datatype_t *datatype,
                                int src,
                                int tag,
                                struct ompi_communicator_t* comm,
                                ompi_status_public_t* status );

extern int mca_pml_portals_isend_init(void *buf,
                                      size_t count,
                                      ompi_datatype_t *datatype,
                                      int dst,
                                      int tag,
                                      mca_pml_base_send_mode_t mode,
                                      struct ompi_communicator_t* comm,
                                      struct ompi_request_t **request);

extern int mca_pml_portals_isend(void *buf,
                                 size_t count,
                                 ompi_datatype_t *datatype,
                                 int dst,
                                 int tag,
                                 mca_pml_base_send_mode_t mode,
                                 struct ompi_communicator_t* comm,
                                 struct ompi_request_t **request);

extern int mca_pml_portals_send(void *buf,
                                size_t count,
                                ompi_datatype_t *datatype,
                                int dst,
                                int tag,
                                mca_pml_base_send_mode_t mode,
                                struct ompi_communicator_t* comm);

extern int mca_pml_portals_iprobe(int dst,
                                  int tag,
                                  struct ompi_communicator_t* comm,
                                  int *matched,
                                  ompi_status_public_t* status);

extern int mca_pml_portals_probe(int dst,
                                 int tag,
                                 struct ompi_communicator_t* comm,
                                 ompi_status_public_t* status);

extern int mca_pml_portals_start(size_t count, ompi_request_t** requests);


extern int mca_pml_portals_dump(struct ompi_communicator_t* comm,
                                int verbose);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* PML_PORTALS_H_HAS_BEEN_INCLUDED */
