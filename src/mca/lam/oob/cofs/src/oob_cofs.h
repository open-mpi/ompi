/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#include "lam_config.h"

#include "mca/lam/oob/oob.h"
#include "lam/types.h"

/*
 * Module open / close
 */
int mca_oob_cofs_open(void);
int mca_oob_cofs_close(void);


/*
 * Startup / Shutdown
 */
int mca_oob_cofs_query(int *priority);
struct mca_oob_1_0_0_t* mca_oob_cofs_init(void);
int mca_oob_cofs_finalize(void);


/*
 * "Action" functions
 */
int mca_oob_cofs_send(lam_job_handle_t job_handle, int vpid, int tag, 
                      void* data, size_t data_len);
int mca_oob_cofs_recv(lam_job_handle_t job_handle, int vpid, int* tag,
                      void** data, size_t* data_len);
int mca_oob_cofs_recv_nb(lam_job_handle_t job_handle,  int vpid, int* tag,
                         void** data, size_t* data_len);
int mca_oob_cofs_recv_cb(lam_job_handle_t job_handle, int vpid, int tag, 
                         mca_oob_base_recv_cb_t callback);


extern char mca_oob_cofs_comm_loc[LAM_PATH_MAX]; /* location for file drop-off */
extern int mca_oob_cofs_my_vpid;
extern uint64_t mca_oob_cofs_serial;
