/*
 * $HEADER$
 */

#ifndef MCA_PML_TEG_H_
#define MCA_PML_TEG_H

#include "lam/mem/free_list.h"
#include "lam/util/cmd_line.h"
#include "mpi/request/request.h"
#include "mca/mpi/pml/pml.h"


/*
 * PML module functions.
 */

extern mca_pml_base_module_1_0_0_t mca_pml_teg_module_1_0_0_0;


extern int mca_pml_teg_open(
    lam_cmd_line_t*
);

extern int mca_pml_teg_close(void);

extern mca_pml_1_0_0_t* mca_pml_teg_init(
    int *priority, 
    int *max_tag, 
    int *max_cid
);



/*
 * TEG PML Interface
 */

struct mca_pml_teg_t {
    mca_pml_t super;

    /* incomplete posted sends */
    lam_list_t  teg_incomplete_sends;
    lam_mutex_t teg_lock;
};
typedef struct mca_pml_teg_t mca_pml_teg_t;

extern mca_pml_teg_t mca_pml_teg;

/*
 * PML interface functions.
 */

extern int mca_pml_teg_add_procs(
    struct lam_proc_t **procs,
    int nprocs
);

extern int mca_pml_teg_add_ptls(
    struct mca_ptl_t **ptls,
    int nptls
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

extern int mca_pml_teg_progress(
    mca_pml_base_tstamp_t tstamp
);

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

