/*
 * $HEADER$
 */

#ifndef MCA_PML_TEG_H_
#define MCA_PML_TEG_H

#include "mca/mpi/pml/pml.h"

extern mca_pml_module_1_0_0_t mca_pml_teg_module_1_0_0_0;

/*
 * PML module query/init
 */

extern int mca_pml_teg_open(
    lam_cmd_line_t*
);

extern int mca_pml_teg_close(void);

extern int mca_pml_teg_query(
    int *priority, 
    int *min_thread, 
    int* max_thread
);

extern mca_pml_t* mca_pml_teg_init(
    struct lam_proc_t **procs, 
    int nprocs, 
    int *max_tag, 
    int *max_cid
);

/*
 * PML interface functions.
 */

extern int mca_pml_teg_isend(
    void *buf,
    size_t size,
    struct lam_datatype_t *datatype,
    int dest,
    int tag,
    struct lam_communicator_t* comm,
    mca_pml_request_type_t req_type,
    struct lam_request_t **request
);

extern int mca_pml_teg_progress(
    mca_pml_tstamp_t
);

extern int mca_pml_teg_addprocs(
    lam_proc_t **procs,
    int nprocs
);

#endif

