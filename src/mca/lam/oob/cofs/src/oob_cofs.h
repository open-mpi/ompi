/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "mca/lam/oob/oob.h"

/*
 * Module open / close
 */
int mca_oob_cofs_open(lam_cmd_line_t *cmd);
int mca_oob_cofs_close(void);


/*
 * Startup / Shutdown
 */
int mca_oob_cofs_query(int *priority);
struct mca_oob_1_0_0* mca_oob_cofs_init(void);
int mca_oob_cofs_finalize(void);


/*
 * "Action" functions
 */
int mca_oob_cofs_send(char* parallel_job_id, int vpid, int tag, 
                      void* data, size_t data_len);
int mca_oob_cofs_recv(char* parallel_job_id, int* tag, int* vpid, 
                      void** data, size_t* data_len);
int mca_oob_cofs_recv_nb(char* parallel_job_id, int* tag, int* vpid, 
                         void** data, size_t* data_len);
int mca_oob_cofs_recv_cb(char* parallel_job_id, int tag, 
                         mca_oob_recv_cb_t callback);
