/* -*- C -*-
 *
 * $HEADER$
 */
/** @file **/

/** 
 *  \brief Out of Band Messaging Interface
 *
 * OMPI/MPI provides a simple point-to-point tagged messaging system
 * intended for out-of-band communication.  This interface should be
 * used minimally in general OMPI code and should not be used
 * explicitly in the MPI layer.  Not all run-time environments provide
 * a sufficient out-of-band messaging system, so some environments may
 * choose not to implement this interface, at the cost of reduced
 * functionality.
 *
 * This interface can be brought up as soon as the process control
 * interface is initiated.  The process control interface is not
 * strictly required, but it is unclear how one could determine the
 * processes' parallel_job_id and vpid without asking the process
 * control interface.  It should not be a requirement of this
 * interface that MPI exist or be properly initialized for a send to
 * complete.  One can possibly envision using a ptl progression engine
 * for out-of-band messaging, but it must not put any MPI requirements
 * on the interface.
 *
 * The out-of-band messaging interface is actually implemented through
 * the oob mca module - details of a particular implementation
 * will be found there.
 */

#ifndef MCA_OOB_H_
#define MCA_OOB_H_

#include "ompi_config.h"

#include "include/types.h"
#include "mca/mca.h"

/*
 * Global constants / types
 */

  /* "Special" tags */
#define MCA_OOB_ANY_TAG      -1
#define MCA_OOB_REGISTRY_TAG -2

  /* "Special" vpids */
#define MCA_OOB_MPIRUN       -1

typedef void (*mca_oob_base_recv_cb_t)(ompi_job_handle_t job_handle, int tag, 
                                       int vpid, void* data, size_t data_len, int status);


/*
 * Functions every module instance will have to provide
 */
typedef struct mca_oob_1_0_0_t*
  (*mca_oob_base_init_fn_t)(int *priority, bool *allow_multi_user_threads,
                            bool *have_hidden_threads);
typedef int (*mca_oob_base_send_fn_t)(ompi_job_handle_t job_handle, int vpid, int tag, 
                                      void* data, size_t data_len);
typedef int (*mca_oob_base_recv_fn_t)(ompi_job_handle_t job_handle,  int vpid, int* tag, 
                                      void** data, size_t* data_len);
typedef int (*mca_oob_base_recv_nb_fn_t)(ompi_job_handle_t job_handle, int vpid, int* tag,  
                                         void** data, size_t* data_len);
typedef int (*mca_oob_base_recv_cb_fn_t)(ompi_job_handle_t job_handle, int vpid, int tag, 
                                         mca_oob_base_recv_cb_t callback);
typedef int (*mca_oob_base_finalize_fn_t)(void);


/*
 * Ver 1.0.0
 */
struct mca_oob_base_module_1_0_0_t {
  mca_base_module_t oobm_version;
  mca_base_module_data_1_0_0_t oobm_data;

  mca_oob_base_init_fn_t oobm_init;
  mca_oob_base_finalize_fn_t oobm_finalize;
};
typedef struct mca_oob_base_module_1_0_0_t mca_oob_base_module_1_0_0_t;

struct mca_oob_1_0_0_t {
  mca_oob_base_send_fn_t oob_send;
  mca_oob_base_recv_fn_t oob_recv;
  mca_oob_base_recv_nb_fn_t oob_recv_nb;
  mca_oob_base_recv_cb_fn_t oob_recv_cb;
};
typedef struct mca_oob_1_0_0_t mca_oob_1_0_0_t;

typedef mca_oob_base_module_1_0_0_t mca_oob_base_module_t;
typedef mca_oob_1_0_0_t mca_oob_t;

/*
 * Macro for use in modules that are of type coll v1.0.0
 */
#define MCA_OOB_BASE_VERSION_1_0_0 \
  /* oob v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* oob v1.0 */ \
  "oob", 1, 0, 0

extern mca_oob_t mca_oob;


#endif
