/** @file 
 *
 *  
 */

/*
 * $HEADER$
 */

#ifndef MCA_PML_TEG_H_
#define MCA_PML_TEG_H

#include "lam/mem/free_list.h"
#include "lam/util/cmd_line.h"
#include "mpi/request/request.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"


/**
 * TEG PML Interface
 */

struct mca_pml_teg_t {
    mca_pml_t super; 

    mca_ptl_base_module_t **teg_ptl_modules;
    size_t teg_num_ptl_modules;

    mca_ptl_t** teg_ptls;
    size_t teg_num_ptls;

    lam_list_t  teg_incomplete_sends; 
    lam_list_t  teg_pending_acks;
    lam_mutex_t teg_lock;
};
typedef struct mca_pml_teg_t mca_pml_teg_t;

extern mca_pml_teg_t mca_pml_teg;


/*
 * PML module functions.
 */

extern mca_pml_base_module_1_0_0_t mca_pml_teg_module_1_0_0_0;


extern int mca_pml_teg_open(void);
extern int mca_pml_teg_close(void);

extern mca_pml_t* mca_pml_teg_init(
    int *priority, 
    int *max_tag, 
    int *max_cid
);


/*
 * PML interface functions.
 */

extern int mca_pml_teg_add_comm(
    struct lam_communicator_t* comm
);

extern int mca_pml_teg_del_comm(
    struct lam_communicator_t* comm
);

extern int mca_pml_teg_add_procs(
    struct lam_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_teg_del_procs(
    struct lam_proc_t **procs,
    size_t nprocs
);

extern int mca_pml_teg_add_ptls(
    struct mca_ptl_t **ptls,
    size_t nptls
);

extern int mca_pml_teg_fini(void);

extern int mca_pml_teg_isend_init(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    bool persistent,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

extern int mca_pml_teg_isend(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int dst,
    int tag,
    mca_pml_base_send_mode_t mode,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

extern int mca_pml_teg_irecv_init(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int src,
    int tag,
    bool persistent,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

extern int mca_pml_teg_irecv(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int src,
    int tag,
    struct lam_communicator_t* comm,
    struct lam_request_t **request
);

extern int mca_pml_teg_progress(void);

extern int mca_pml_teg_start(
    lam_request_t* request
);
                                                                                                                          
extern int mca_pml_teg_test(
    lam_request_t** request,
    int count,
    int *completed
);
                                                                                                                          
extern int mca_pml_teg_wait(
    lam_request_t* request,
    mca_pml_base_status_t* status
);
                                                                                                                          

#endif

