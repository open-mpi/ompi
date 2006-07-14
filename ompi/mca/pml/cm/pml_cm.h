/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_CM_H
#define PML_CM_H

#include "ompi/class/ompi_free_list.h"
#include "opal/util/cmd_line.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/convertor.h"
#include "opal/class/opal_free_list.h"
#include "ompi/mca/mtl/mtl.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_mtl_request_t;

struct ompi_pml_cm_t {
    mca_pml_base_module_t super;
    /** free list of send request structures */
    ompi_free_list_t cm_thin_send_requests;
    ompi_free_list_t cm_hvy_send_requests;
    /** free list of recv request structures */
    ompi_free_list_t cm_thin_recv_requests;
    ompi_free_list_t cm_hvy_recv_requests;
};
typedef struct ompi_pml_cm_t ompi_pml_cm_t;
extern ompi_pml_cm_t ompi_pml_cm;

/* PML interface functions */
extern int mca_pml_cm_add_procs(struct ompi_proc_t **procs, size_t nprocs);
extern int mca_pml_cm_del_procs(struct ompi_proc_t **procs, size_t nprocs);

extern int mca_pml_cm_enable(bool enable);
extern int mca_pml_cm_progress(void);

extern int mca_pml_cm_add_comm(struct ompi_communicator_t* comm);
extern int mca_pml_cm_del_comm(struct ompi_communicator_t* comm);

extern int mca_pml_cm_irecv_init(void *buf,
                                      size_t count,
                                      ompi_datatype_t *datatype,
                                      int src,
                                      int tag,
                                      struct ompi_communicator_t* comm,
                                      struct ompi_request_t **request);

extern int mca_pml_cm_irecv(void *buf,
                                 size_t count,
                                 ompi_datatype_t *datatype,
                                 int src,
                                 int tag,
                                 struct ompi_communicator_t* comm,
                                 struct ompi_request_t **request);

extern int mca_pml_cm_recv(void *buf,
                                size_t count,
                                ompi_datatype_t *datatype,
                                int src,
                                int tag,
                                struct ompi_communicator_t* comm,
                                ompi_status_public_t* status );

extern int mca_pml_cm_isend_init(void *buf,
                                      size_t count,
                                      ompi_datatype_t *datatype,
                                      int dst,
                                      int tag,
                                      mca_pml_base_send_mode_t mode,
                                      struct ompi_communicator_t* comm,
                                      struct ompi_request_t **request);

extern int mca_pml_cm_isend(void *buf,
                                 size_t count,
                                 ompi_datatype_t *datatype,
                                 int dst,
                                 int tag,
                                 mca_pml_base_send_mode_t mode,
                                 struct ompi_communicator_t* comm,
                                 struct ompi_request_t **request);

extern int mca_pml_cm_send(void *buf,
                                size_t count,
                                ompi_datatype_t *datatype,
                                int dst,
                                int tag,
                                mca_pml_base_send_mode_t mode,
                                struct ompi_communicator_t* comm);

extern int mca_pml_cm_iprobe(int dst,
                                  int tag,
                                  struct ompi_communicator_t* comm,
                                  int *matched,
                                  ompi_status_public_t* status);

extern int mca_pml_cm_probe(int dst,
                                 int tag,
                                 struct ompi_communicator_t* comm,
                                 ompi_status_public_t* status);

extern int mca_pml_cm_start(size_t count, ompi_request_t** requests);


extern int mca_pml_cm_dump(struct ompi_communicator_t* comm,
                                int verbose);

extern int mca_pml_cm_cancel(struct ompi_request_t *request, int flag);

extern void mca_pml_cm_thin_send_request_completion(struct mca_mtl_request_t *mtl_request);
extern void mca_pml_cm_hvy_send_request_completion(struct mca_mtl_request_t *mtl_request);

extern void mca_pml_cm_thin_recv_request_completion(struct mca_mtl_request_t *mtl_request);
extern void mca_pml_cm_hvy_recv_request_completion(struct mca_mtl_request_t *mtl_request);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* PML_CM_H_HAS_BEEN_INCLUDED */
