/*
 * $HEADER$
 */

#ifndef LAM_PML_TEG_SEND_REQUEST_H
#define LAM_PML_TEG_SEND_REQUEST_H

#include "mca/mpi/pml/base/pml_base_sendreq.h"
#include "mca/mpi/pml/base/pml_base_sendfrag.h"

int mca_pml_teg_send_request_alloc(
    mca_pml_proc_t*,
    mca_pml_base_send_request_t**
);

int mca_pml_teg_send_request_start(mca_pml_base_send_request_t* req);
int mca_pml_teg_send_request_schedule(mca_pml_base_send_request_t* req, bool* complete);
int mca_pml_teg_send_request_progress(void);


#endif

