/* -*- C -*-
 *
 * $HEADER$
 */

/** 
 *  \brief Out of Band Messaging Interface
 *
 * LAM/MPI provides a simple point-to-point tagged messaging system
 * intended for out-of-band communication.  This interface should be
 * used minimally in general LAM code and should not be used
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
 * the lam/oob mca module - details of a particular implementation
 * will be found there.
 */

#ifndef MCA_OOB_H_
#define MCA_OOB_H_

#include "lam_config.h"

#include "mca/mca.h"

#include <sys/types.h>

/*
 * Global constants / types
 */

  /* "Special" tags */
#define MCA_OOB_ANY_TAG  -1
#define MCA_OOB_REGISTRY_TAG -2

  /* "Special" vpids */
#define MCA_OOB_MPIRUN  -1

typedef void (*mca_oob_recv_cb_t)(char* parallel_job_id, int tag, 
                                  int vpid, void* data, size_t data_len, int status);


/*
 * Functions every module instance will have to provide
 */
typedef int (*mca_oob_query_fn_t)(int *priority);
typedef struct mca_oob_1_0_0* (*mca_oob_init_fn_t)(void);
typedef int (*mca_oob_send_fn_t)(char* parallel_job_id, int vpid, int tag, 
                                 void* data, size_t data_len);
typedef int (*mca_oob_recv_fn_t)(char* parallel_job_id, int* tag, int* vpid, 
                                 void** data, size_t* data_len);
typedef int (*mca_oob_recv_nb_fn_t)(char* parallel_job_id, int* tag, int* vpid, 
                                    void** data, size_t* data_len);
typedef int (*mca_oob_recv_cb_fn_t)(char* parallel_job_id, int tag, 
                                    mca_oob_recv_cb_t callback);
typedef int (*mca_oob_finalize_fn_t)(void);


/*
 * Ver 1.0.0
 */
struct mca_oob_module_1_0_0_t {
  mca_module_1_0_0_t super;

  mca_oob_query_fn_t oobm_query;
  mca_oob_init_fn_t oobm_init;
  mca_oob_finalize_fn_t oob_finalize;
};
typedef struct mca_oob_module_1_0_0_t mca_oob_module_1_0_0_t;

struct mca_oob_1_0_0_t {
  mca_oob_send_fn_t oob_send;
  mca_oob_recv_fn_t oob_recv;
  mca_oob_recv_nb_fn_t oob_recv_nb;
  mca_oob_recv_cb_fn_t oob_recv_cb;
};
typedef struct mca_oob_1_0_0_t mca_oob_1_0_0_t;

typedef mca_oob_module_1_0_0_t mca_oob_module_t;
typedef mca_oob_1_0_0_t mca_oob_t;


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_oob_base_open(lam_cmd_line_t *cmd);
  int mca_oob_base_close(void);

  bool mca_oob_base_is_checkpointable(void);

  int mca_oob_base_checkpoint(void);
  int mca_oob_base_continue(void);
  int mca_oob_base_restart(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Global struct holding the selected module's function pointers
 */
#if 0
extern mca_oob_t mca_oob;
#endif /* #if 0 */

#endif
