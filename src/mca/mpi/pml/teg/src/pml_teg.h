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

extern int mca_pml_teg_query(
    int *priority, 
    int *min_thread, 
    int* max_thread
);

extern mca_pml_1_0_0_t* mca_pml_teg_init(
    lam_proc_t **procs, 
    int nprocs, 
    int *max_tag, 
    int *max_cid
);


/*
 * TEG PML Interface
 *
 * JMS: Tim--Note that you don't have to do versioning here.
 * Versioning is only for the MCA framework (i.e., able to load
 * modules of different versions).  This type is going to be used
 * specifically within your module, and the framework will never see
 * it.  Ergo, no other teg modules (even those of different versions)
 * will ever see it, either.  So while you can do the 1_0_0 stuff
 * here, it isn't strictly necessary.
 */

struct mca_pml_teg_1_0_0_t {
    mca_pml_1_0_0_t super;
    lam_free_list_t teg_send_requests;
    lam_free_list_t teg_recv_requests;
};
typedef struct mca_pml_teg_1_0_0_t mca_pml_teg_1_0_0_t;

/*
 * PML interface functions.
 */

extern mca_pml_teg_1_0_0_t mca_pml_teg_1_0_0_0;

extern int mca_pml_teg_isend(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int dest,
    int tag,
    struct lam_communicator_t* comm,
    mca_pml_base_request_type_t req_type,
    struct lam_request_t **request
);

extern int mca_pml_teg_progress(
    mca_pml_base_tstamp_t tstamp
);

extern int mca_pml_teg_addprocs(
    lam_proc_t **procs,
    int nprocs
);

#endif

