/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_OFI_H_HAS_BEEN_INCLUDED
#define MTL_OFI_H_HAS_BEEN_INCLUDED

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "opal/datatype/opal_convertor.h"

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_tagged.h>

BEGIN_C_DECLS

/* MTL interface functions */
extern int ompi_mtl_ofi_finalize(struct mca_mtl_base_module_t *mtl);

extern int ompi_mtl_ofi_add_procs(struct mca_mtl_base_module_t *mtl,
                                  size_t nprocs,
                                  struct ompi_proc_t **procs);

extern int ompi_mtl_ofi_del_procs(struct mca_mtl_base_module_t *mtl,
                                  size_t nprocs,
                                  struct ompi_proc_t **procs);

extern int ompi_mtl_ofi_send(struct mca_mtl_base_module_t *mtl,
                             struct ompi_communicator_t *comm,
                             int dest,
                             int tag,
                             struct opal_convertor_t *convertor,
                             mca_pml_base_send_mode_t mode);

extern int ompi_mtl_ofi_isend(struct mca_mtl_base_module_t *mtl,
                              struct ompi_communicator_t *comm,
                              int dest,
                              int tag,
                              struct opal_convertor_t *convertor,
                              mca_pml_base_send_mode_t mode,
                              bool blocking,
                              mca_mtl_request_t *mtl_request);

extern int ompi_mtl_ofi_irecv(struct mca_mtl_base_module_t *mtl,
                              struct ompi_communicator_t *comm,
                              int src,
                              int tag,
                              struct opal_convertor_t *convertor,
                              mca_mtl_request_t *mtl_request);

extern int ompi_mtl_ofi_iprobe(struct mca_mtl_base_module_t *mtl,
                               struct ompi_communicator_t *comm,
                               int src,
                               int tag,
                               int *flag,
                               struct ompi_status_public_t *status);

extern int ompi_mtl_ofi_imrecv(struct mca_mtl_base_module_t *mtl,
                               struct opal_convertor_t *convertor,
                               struct ompi_message_t **message,
                               struct mca_mtl_request_t *mtl_request);

extern int ompi_mtl_ofi_improbe(struct mca_mtl_base_module_t *mtl,
                                struct ompi_communicator_t *comm,
                                int src,
                                int tag,
                                int *matched,
                                struct ompi_message_t **message,
                                struct ompi_status_public_t *status);

extern int ompi_mtl_ofi_cancel(struct mca_mtl_base_module_t *mtl,
                               mca_mtl_request_t *mtl_request,
                               int flag);

extern int ompi_mtl_ofi_add_comm(struct mca_mtl_base_module_t *mtl,
                                 struct ompi_communicator_t *comm);

extern int ompi_mtl_ofi_del_comm(struct mca_mtl_base_module_t *mtl,
                                 struct ompi_communicator_t *comm);

extern int ompi_mtl_ofi_progress(void);

extern int ompi_mtl_ofi_get_error(int fi_error);

END_C_DECLS

#endif  /* MTL_OFI_H_HAS_BEEN_INCLUDED */
