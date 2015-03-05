/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_PML_MONITORING_H
#define MCA_PML_MONITORING_H

BEGIN_C_DECLS

#include <ompi_config.h>
#include <ompi/communicator/communicator.h>
#include <ompi/datatype/ompi_datatype.h>
#include <ompi/mca/pml/pml.h>
#include <ompi/mca/pml/pml.h>

typedef mca_pml_base_module_t mca_pml_monitoring_module_t;

extern mca_pml_base_component_t pml_selected_component;
extern mca_pml_base_module_t pml_selected_module;
extern mca_pml_monitoring_module_t mca_pml_monitoring;
OMPI_DECLSPEC extern mca_pml_base_component_2_0_0_t mca_pml_monitoring_component;

/*
 * PML interface functions.
 */

extern int mca_pml_monitoring_add_comm(struct ompi_communicator_t* comm);

extern int mca_pml_monitoring_del_comm(struct ompi_communicator_t* comm);

extern int mca_pml_monitoring_add_procs(struct ompi_proc_t **procs,
                                        size_t nprocs);

extern int mca_pml_monitoring_del_procs(struct ompi_proc_t **procs,
                                        size_t nprocs);

extern int mca_pml_monitoring_enable(bool enable);

extern int mca_pml_monitoring_iprobe(int dst,
                                     int tag,
                                     struct ompi_communicator_t* comm,
                                     int *matched,
                                     ompi_status_public_t* status );

extern int mca_pml_monitoring_probe(int dst,
                                    int tag,
                                    struct ompi_communicator_t* comm,
                                    ompi_status_public_t* status );

extern int mca_pml_monitoring_improbe(int dst,
                                      int tag,
                                      struct ompi_communicator_t* comm,
                                      int *matched,
                                      struct ompi_message_t **message,
                                      ompi_status_public_t* status );

extern int mca_pml_monitoring_mprobe(int dst,
                                     int tag,
                                     struct ompi_communicator_t* comm,
                                     struct ompi_message_t **message,
                                     ompi_status_public_t* status );

extern int mca_pml_monitoring_isend_init(void *buf,
                                         size_t count,
                                         ompi_datatype_t *datatype,
                                         int dst,
                                         int tag,
                                         mca_pml_base_send_mode_t mode,
                                         struct ompi_communicator_t* comm,
                                         struct ompi_request_t **request);

extern int mca_pml_monitoring_isend(void *buf,
                                    size_t count,
                                    ompi_datatype_t *datatype,
                                    int dst,
                                    int tag,
                                    mca_pml_base_send_mode_t mode,
                                    struct ompi_communicator_t* comm,
                                    struct ompi_request_t **request);

extern int mca_pml_monitoring_send(void *buf,
                                   size_t count,
                                   ompi_datatype_t *datatype,
                                   int dst,
                                   int tag,
                                   mca_pml_base_send_mode_t mode,
                                   struct ompi_communicator_t* comm);

extern int mca_pml_monitoring_irecv_init(void *buf,
                                         size_t count,
                                         ompi_datatype_t *datatype,
                                         int src,
                                         int tag,
                                         struct ompi_communicator_t* comm,
                                         struct ompi_request_t **request);

extern int mca_pml_monitoring_irecv(void *buf,
                                    size_t count,
                                    ompi_datatype_t *datatype,
                                    int src,
                                    int tag,
                                    struct ompi_communicator_t* comm,
                                    struct ompi_request_t **request);

extern int mca_pml_monitoring_recv(void *buf,
                                   size_t count,
                                   ompi_datatype_t *datatype,
                                   int src,
                                   int tag,
                                   struct ompi_communicator_t* comm,
                                   ompi_status_public_t* status);

extern int mca_pml_monitoring_imrecv(void *buf,
                                     size_t count,
                                     ompi_datatype_t *datatype,
                                     struct ompi_message_t **message,
                                     struct ompi_request_t **request);

extern int mca_pml_monitoring_mrecv(void *buf,
                                    size_t count,
                                    ompi_datatype_t *datatype,
                                    struct ompi_message_t **message,
                                    ompi_status_public_t* status);

extern int mca_pml_monitoring_dump(struct ompi_communicator_t* comm,
                                   int verbose);

extern int mca_pml_monitoring_start(size_t count,
                                    ompi_request_t** requests);

END_C_DECLS

#endif  /* MCA_PML_MONITORING_H */
