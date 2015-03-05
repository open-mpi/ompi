/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_YALLA_H_
#define PML_YALLA_H_

#include "pml_yalla_freelist.h"

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"

#include <mxm/api/mxm_api.h>

typedef struct mca_pml_yalla_module    mca_pml_yalla_module_t;
typedef struct pml_yalla_base_request  mca_pml_yalla_base_request_t;
typedef struct pml_yalla_send_request  mca_pml_yalla_send_request_t;
typedef struct pml_yalla_bsend_request mca_pml_yalla_bsend_request_t;
typedef struct pml_yalla_recv_request  mca_pml_yalla_recv_request_t;
typedef struct pml_yalla_convertor     mca_pml_yalla_convertor_t;

#if MXM_API < MXM_VERSION(2,0)
#  error "MXM 2.0 or above is required"
#endif

struct mca_pml_yalla_module {
    mca_pml_base_module_t    super;

    /* MXM global objects */
    mxm_context_opts_t       *ctx_opts;
    mxm_ep_opts_t            *ep_opts;
    mxm_h                    mxm_context;
    mxm_ep_h                 mxm_ep;

    /* MXM requests */
    mca_pml_yalla_freelist_t send_reqs;
    mca_pml_yalla_freelist_t bsend_reqs;
    mca_pml_yalla_freelist_t recv_reqs;

    /* Convertors pool */
    mca_pml_yalla_freelist_t convs;

    int                      using_mem_hooks;
    int                      priority;
    int                      verbose;
    int                      output;
};

extern mca_pml_base_component_2_0_0_t mca_pml_yalla_component;
extern mca_pml_yalla_module_t ompi_pml_yalla;


/* Debugging */
#define PML_YALLA_ENABLE_DEBUG OPAL_ENABLE_DEBUG
#if PML_YALLA_ENABLE_DEBUG
#  define PML_YALLA_MAX_VERBOSE  9
#  define PML_YALLA_ASSERT(_x)   assert(_x)
#else
#  define PML_YALLA_MAX_VERBOSE  2
#  define PML_YALLA_ASSERT(_x)
#endif


#define PML_YALLA_ERROR(format, ... ) \
    opal_output_verbose(0, ompi_pml_yalla.output, "Error: %s:%d - %s() " format, \
                        __FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define PML_YALLA_VERBOSE(_level, format, ... ) \
    if (((_level) <= PML_YALLA_MAX_VERBOSE) && ((_level) <= ompi_pml_yalla.verbose)) { \
        opal_output_verbose(_level, ompi_pml_yalla.output, "%s:%d - %s() " format, \
                            __FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__); \
    }

int mca_pml_yalla_open(void);
int mca_pml_yalla_close(void);
int mca_pml_yalla_init(void);
int mca_pml_yalla_cleanup(void);

int mca_pml_yalla_add_procs(struct ompi_proc_t **procs, size_t nprocs);
int mca_pml_yalla_del_procs(struct ompi_proc_t **procs, size_t nprocs);

int mca_pml_yalla_enable(bool enable);
int mca_pml_yalla_progress(void);

int mca_pml_yalla_add_comm(struct ompi_communicator_t* comm);
int mca_pml_yalla_del_comm(struct ompi_communicator_t* comm);

int mca_pml_yalla_irecv_init(void *buf, size_t count, ompi_datatype_t *datatype,
                             int src, int tag, struct ompi_communicator_t* comm,
                             struct ompi_request_t **request);

int mca_pml_yalla_irecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        int src, int tag, struct ompi_communicator_t* comm,
                        struct ompi_request_t **request);

int mca_pml_yalla_recv(void *buf, size_t count, ompi_datatype_t *datatype, int src,
                       int tag, struct ompi_communicator_t* comm,
                       ompi_status_public_t* status);

int mca_pml_yalla_isend_init(void *buf, size_t count, ompi_datatype_t *datatype,
                             int dst, int tag, mca_pml_base_send_mode_t mode,
                             struct ompi_communicator_t* comm,
                             struct ompi_request_t **request);

int mca_pml_yalla_isend(void *buf, size_t count, ompi_datatype_t *datatype,
                        int dst, int tag, mca_pml_base_send_mode_t mode,
                        struct ompi_communicator_t* comm,
                        struct ompi_request_t **request);

int mca_pml_yalla_send(void *buf, size_t count, ompi_datatype_t *datatype, int dst,
                       int tag, mca_pml_base_send_mode_t mode,
                       struct ompi_communicator_t* comm);

int mca_pml_yalla_iprobe(int src, int tag, struct ompi_communicator_t* comm,
                         int *matched, ompi_status_public_t* status);

int mca_pml_yalla_probe(int src, int tag, struct ompi_communicator_t* comm,
                        ompi_status_public_t* status);

int mca_pml_yalla_improbe(int src, int tag, struct ompi_communicator_t* comm,
                          int *matched, struct ompi_message_t **message,
                          ompi_status_public_t* status);

int mca_pml_yalla_mprobe(int src, int tag, struct ompi_communicator_t* comm,
                         struct ompi_message_t **message,
                         ompi_status_public_t* status);

int mca_pml_yalla_imrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                         struct ompi_message_t **message,
                         struct ompi_request_t **request);

int mca_pml_yalla_mrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        struct ompi_message_t **message,
                        ompi_status_public_t* status);

int mca_pml_yalla_start(size_t count, ompi_request_t** requests);

int mca_pml_yalla_dump(struct ompi_communicator_t* comm, int verbose);

#endif /* PML_YALLA_H_ */
