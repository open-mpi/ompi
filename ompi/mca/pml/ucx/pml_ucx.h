/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_UCX_H_
#define PML_UCX_H_

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "opal/mca/common/ucx/common_ucx.h"

#include <ucp/api/ucp.h>
#include "pml_ucx_freelist.h"

#define PML_UCX_ASSERT  MCA_COMMON_UCX_ASSERT
#define PML_UCX_ERROR   MCA_COMMON_UCX_ERROR
#define PML_UCX_VERBOSE MCA_COMMON_UCX_VERBOSE


typedef struct mca_pml_ucx_module           mca_pml_ucx_module_t;
typedef struct pml_ucx_persistent_request   mca_pml_ucx_persistent_request_t;
typedef struct pml_ucx_convertor            mca_pml_ucx_convertor_t;

/*
 * TODO version check
 */

struct mca_pml_ucx_module {
    mca_pml_base_module_t     super;

    /* UCX global objects */
    ucp_context_h             ucp_context;
    ucp_worker_h              ucp_worker;

    /* Datatypes */
    int                       datatype_attr_keyval;
    ucp_datatype_t            predefined_types[OMPI_DATATYPE_MPI_MAX_PREDEFINED];

    /* Requests */
    mca_pml_ucx_freelist_t    persistent_reqs;
    ompi_request_t            completed_send_req;
    size_t                    request_size;
    int                       num_disconnect;

    /* Converters pool */
    mca_pml_ucx_freelist_t    convs;

    int                       priority;
};

extern mca_pml_base_component_2_0_0_t mca_pml_ucx_component;
extern mca_pml_ucx_module_t ompi_pml_ucx;

int mca_pml_ucx_open(void);
int mca_pml_ucx_close(void);
int mca_pml_ucx_init(int enable_mpi_threads);
int mca_pml_ucx_cleanup(void);

int mca_pml_ucx_add_procs(struct ompi_proc_t **procs, size_t nprocs);
int mca_pml_ucx_del_procs(struct ompi_proc_t **procs, size_t nprocs);

int mca_pml_ucx_enable(bool enable);
int mca_pml_ucx_progress(void);

int mca_pml_ucx_add_comm(struct ompi_communicator_t* comm);
int mca_pml_ucx_del_comm(struct ompi_communicator_t* comm);

int mca_pml_ucx_irecv_init(void *buf, size_t count, ompi_datatype_t *datatype,
                             int src, int tag, struct ompi_communicator_t* comm,
                             struct ompi_request_t **request);

int mca_pml_ucx_irecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        int src, int tag, struct ompi_communicator_t* comm,
                        struct ompi_request_t **request);

int mca_pml_ucx_recv(void *buf, size_t count, ompi_datatype_t *datatype, int src,
                       int tag, struct ompi_communicator_t* comm,
                       ompi_status_public_t* status);

int mca_pml_ucx_isend_init(const void *buf, size_t count, ompi_datatype_t *datatype,
                             int dst, int tag, mca_pml_base_send_mode_t mode,
                             struct ompi_communicator_t* comm,
                             struct ompi_request_t **request);

int mca_pml_ucx_isend(const void *buf, size_t count, ompi_datatype_t *datatype,
                        int dst, int tag, mca_pml_base_send_mode_t mode,
                        struct ompi_communicator_t* comm,
                        struct ompi_request_t **request);

int mca_pml_ucx_send(const void *buf, size_t count, ompi_datatype_t *datatype, int dst,
                       int tag, mca_pml_base_send_mode_t mode,
                       struct ompi_communicator_t* comm);

int mca_pml_ucx_iprobe(int src, int tag, struct ompi_communicator_t* comm,
                         int *matched, ompi_status_public_t* status);

int mca_pml_ucx_probe(int src, int tag, struct ompi_communicator_t* comm,
                        ompi_status_public_t* status);

int mca_pml_ucx_improbe(int src, int tag, struct ompi_communicator_t* comm,
                          int *matched, struct ompi_message_t **message,
                          ompi_status_public_t* status);

int mca_pml_ucx_mprobe(int src, int tag, struct ompi_communicator_t* comm,
                         struct ompi_message_t **message,
                         ompi_status_public_t* status);

int mca_pml_ucx_imrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                         struct ompi_message_t **message,
                         struct ompi_request_t **request);

int mca_pml_ucx_mrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        struct ompi_message_t **message,
                        ompi_status_public_t* status);

int mca_pml_ucx_start(size_t count, ompi_request_t** requests);

int mca_pml_ucx_dump(struct ompi_communicator_t* comm, int verbose);


#endif /* PML_UCX_H_ */
