/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2024 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl.h
 *
 * UBCL PML
 *
 * For the standard PML interface, see omp/mca/pml/pml.h
 *
 * For now, pml/ubcl only expose one module which sole purpose is to set the API
 * functions. It then uses its component all the way through.
 */

#ifndef MCA_PML_UBCL_H
#define MCA_PML_UBCL_H

#include "ompi/mca/pml/pml.h"
#include "opal/class/opal_free_list.h"

#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/proc/proc.h"
#include "ompi/request/request.h"
#include "opal/mca/mca.h"
#include "opal/mca/threads/mutex.h"

#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_endpoint.h"

#include <ubcl_api.h>

#define container_of(ptr, type, member) ((type *) ((char *) (ptr) -offsetof(type, member)))

#define PML_UBCL_THREAD_ONLY   if (OPAL_UNLIKELY(mca_pml_ubcl_component.thread_multiple_enabled))
#define pml_ubcl_lock(_lock)   PML_UBCL_THREAD_ONLY opal_atomic_lock(_lock)
#define pml_ubcl_unlock(_lock) PML_UBCL_THREAD_ONLY opal_atomic_unlock(_lock)

/* Because UBCL_MAX_TAG overflows if put in an int */
#if (UBCL_MAX_TAG < INT_MAX)
    #define PML_UBCL_MAX_TAG UBCL_MAX_TAG
#else /* (UBCL_MAX_TAG < INT_MAX) */
    #define PML_UBCL_MAX_TAG INT_MAX
#endif /* (UBCL_MAX_TAG < INT_MAX) */

/* Because UBCL_MAX_CID overflows if put into an uint32_t */
#if (UBCL_MAX_CID < UINT32_MAX)
    #define PML_UBCL_MAX_CID UBCL_MAX_CID
#else /* (UBCL_MAX_CID < INT_MAX) */
    #define PML_UBCL_MAX_CID UINT32_MAX
#endif /* (UBCL_MAX_CID < INT_MAX) */

/**
 * Module structure
 */
struct mca_pml_ubcl_module_t {
    mca_pml_base_module_t super;
};
typedef struct mca_pml_ubcl_module_t mca_pml_ubcl_module_t;

/**
 * Component structure
 */
struct mca_pml_ubcl_component_t {
    mca_pml_base_component_t super;

    /** Functionnal fields **/
    char is_init;                 /**< Whether we have been initialized, for proper close */
    int output;                   /**< Output stream */
    char thread_multiple_enabled; /**< Multithreading support */
    size_t nprocs;                /**< Number of known processes */
    void **stack_addr_buffer;     /**< Buffer to store stack on component error */
    int n_addr;                   /**< Number of void * addresses in #stack_addr_buffer*/

    /** MCA parameters **/
    int priority;             /**< Priority of the component */
    int verbose;              /**< Verbosity level of the component */
    char force_intranode_bxi; /**< Whether to force intranode communication *
                               * via ubcl cards*/
    char force_cuda_custom_dt; /**< Wether to force custom datatype use for CUDA
                                * instead of using ADGE for contiguous CUDA buffers */
    char can_progress;        /**< Allow PML to call opal_progress() once at the end of
                               * each primitive */
    char gdb_attach;          /**< Allow to attach a debugger by looping indefinitly on
                               * this value until 0.*/
    unsigned int max_req;     /**< Maximum number of requests */
    unsigned int min_req;     /**< Minimum (and inititial) number of requests */
    unsigned int incr_req;     /**< Increasing (and inititial) number of requests */
    unsigned int pad_req;

    char check_recv_rsend;    /**< Warn if a rsend did not immediatly match a recv */
    char warn_on_truncate;    /**< Warn if Recv are truncate */
    char abort_on_truncate;   /**< Abort if Recv are truncate */
    char use_mpi_wildcards;   /**< Activate MPI_ANY_SOURCE and MPI_ANY_TAG support */
    char accelerator_is_cuda; /**< True if the current accelerator is 'cuda' */

    /** UBCL endpoint type capabilities **/
    ubcl_endpoint_capabilities_t endpoint_capabilities[UBCL_ENDPOINT_TYPE_SIZE];

    opal_free_list_t pml_req_free_list;
};
typedef struct mca_pml_ubcl_component_t mca_pml_ubcl_component_t;

/*
 * mca_pml_comm_t is an anonymous structure used in ompi_comm_t. Each pml can
 * provide its own declaration of mca_pml_comm_t.
 * Don't change this name.
 */
struct mca_pml_comm_t {
    uint64_t *array;
    uint32_t size;
    uint16_t is_inter;
    uint16_t pad0;
};
typedef struct mca_pml_comm_t mca_pml_ubcl_comm_t;

/** Sole PML module **/
extern mca_pml_ubcl_module_t mca_pml_ubcl_module;

/** PML UBCL component **/
OMPI_DECLSPEC extern mca_pml_ubcl_component_t mca_pml_ubcl_component;

/**
 * Internal API
 */
void mca_pml_ubcl_isend_start(struct ompi_request_t **request);
void mca_pml_ubcl_irecv_prepare(void *buf, size_t count, ompi_datatype_t *datatype, int src,
                                int tag, struct ompi_communicator_t *comm,
                                struct ompi_request_t **request, bool persistent, bool probe,
                                struct ompi_message_t *message);
void mca_pml_ubcl_irecv_start(struct ompi_request_t **request);

size_t pml_ubcl_datatype_pack(void *pack_buf, const void *usr_handle, size_t pack_size,
                              size_t offset);

size_t pml_ubcl_datatype_unpack(void *usr_handle, const void *pack_buf, size_t pack_size,
                                size_t offset);

size_t pml_ubcl_datatype_mem_size(const void *usr_handle, size_t offset);

void pml_ubcl_datatype_finish(void *usr_handle);

/**
 * PML component API (see pml_ubcl_component.c)
 */
int mca_pml_ubcl_component_open(void);
int mca_pml_ubcl_component_close(void);
int mca_pml_ubcl_component_register(void);
mca_pml_base_module_t *mca_pml_ubcl_component_init(int *priority, bool enable_progress_threads,
                                                   bool enable_mpi_threads);
int mca_pml_ubcl_component_finalize(void);

/**
 * PML API (see pml_ubcl.c)
 */
int mca_pml_ubcl_add_comm(struct ompi_communicator_t *comm);
int mca_pml_ubcl_del_comm(struct ompi_communicator_t *comm);
int mca_pml_ubcl_enable(bool enable);
int mca_pml_ubcl_progress(void);
int mca_pml_ubcl_iprobe(int src, int tag, struct ompi_communicator_t *comm, int *matched,
                        ompi_status_public_t *status);
int mca_pml_ubcl_probe(int src, int tag, struct ompi_communicator_t *comm,
                       ompi_status_public_t *status);
int mca_pml_ubcl_improbe(int src, int tag, struct ompi_communicator_t *comm, int *matched,
                         struct ompi_message_t **message, ompi_status_public_t *status);
int mca_pml_ubcl_mprobe(int src, int tag, struct ompi_communicator_t *comm,
                        struct ompi_message_t **message, ompi_status_public_t *status);
int mca_pml_ubcl_isend_init(const void *buf, size_t count, ompi_datatype_t *datatype, int dst,
                            int tag, mca_pml_base_send_mode_t mode,
                            struct ompi_communicator_t *comm, struct ompi_request_t **request);
int mca_pml_ubcl_isend(const void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                       mca_pml_base_send_mode_t mode, struct ompi_communicator_t *comm,
                       struct ompi_request_t **request);
int mca_pml_ubcl_send(const void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                      mca_pml_base_send_mode_t mode, struct ompi_communicator_t *comm);
int mca_pml_ubcl_irecv_init(void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                            struct ompi_communicator_t *comm, struct ompi_request_t **request);
int mca_pml_ubcl_irecv(void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                       struct ompi_communicator_t *comm, struct ompi_request_t **request);
int mca_pml_ubcl_recv(void *buf, size_t count, ompi_datatype_t *datatype, int src, int tag,
                      struct ompi_communicator_t *comm, ompi_status_public_t *status);
int mca_pml_ubcl_imrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        struct ompi_message_t **message, struct ompi_request_t **request);
int mca_pml_ubcl_mrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                       struct ompi_message_t **message, ompi_status_public_t *status);
int mca_pml_ubcl_dump(struct ompi_communicator_t *comm, int verbose);
int mca_pml_ubcl_start(size_t count, ompi_request_t **requests);
int mca_pml_ubcl_ft_event(int state);

#endif /* MCA_PML_UBCL_H */
