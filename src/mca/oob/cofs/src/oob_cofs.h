/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#include "ompi_config.h"

#include "mca/oob/oob.h"
#include "include/types.h"

/*
 * Module open / close
 */
int mca_oob_cofs_open(void);
int mca_oob_cofs_close(void);


/*
 * Startup / Shutdown
 */
struct mca_oob_1_0_0_t* mca_oob_cofs_init(int *priority,
                                          bool *allow_multi_user_threads,
                                          bool *have_hidden_threads);
int mca_oob_cofs_finalize(void);


/*
 * "Action" functions
 */
int mca_oob_cofs_send(ompi_job_handle_t job_handle, int vpid, int tag, 
                      void* data, size_t data_len);
int mca_oob_cofs_recv(ompi_job_handle_t job_handle, int vpid, int* tag,
                      void** data, size_t* data_len);
int mca_oob_cofs_recv_nb(ompi_job_handle_t job_handle,  int vpid, int* tag,
                         void** data, size_t* data_len);
int mca_oob_cofs_recv_cb(ompi_job_handle_t job_handle, int vpid, int tag, 
                         mca_oob_base_recv_cb_t callback);


extern char mca_oob_cofs_comm_loc[OMPI_PATH_MAX]; /* location for file drop-off */
extern int mca_oob_cofs_my_vpid;
extern uint64_t mca_oob_cofs_serial;
