/** @file 
 *
 *  TCP PTL
 */

/*
 * $HEADER$
 */

#ifndef MCA_PTL_TCP_H_
#define MCA_PTL_TCP_H

#include "lam/util/reactor.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"


/*
 * TCP PTL module.
 */

struct mca_ptl_tcp_module_1_0_0_t {
    mca_ptl_base_module_1_0_0_t super;
    lam_reactor_t tcp_reactor;
};
typedef struct mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module_1_0_0_t;
typedef struct mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module_t;

extern mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module_1_0_0_0;

extern int mca_ptl_tcp_module_open(void);
extern int mca_ptl_tcp_module_close(void);

extern mca_ptl_t** mca_ptl_tcp_module_init(
    int *num_ptls, 
    int *thread_min, 
    int *thread_max

);

extern void mca_ptl_tcp_module_progress(
    mca_ptl_base_tstamp_t tstamp
);


/**
 * TCP PTL Interface
 * 
 *
 */

struct mca_ptl_tcp_t {
    mca_ptl_t super; /**< comment */
};
typedef struct mca_ptl_tcp_t mca_ptl_tcp_t;

extern mca_ptl_tcp_t mca_ptl_tcp;


extern int mca_ptl_tcp_fini(
    struct mca_ptl_t* ptl
);

extern int mca_ptl_tcp_add_procs(
    struct mca_ptl_t* ptl,
    struct lam_proc_t **procs,
    size_t nprocs
);
                                                                                                                  
extern int mca_ptl_tcp_request_alloc(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_send_request_t**
);
                                                                                                 
extern int mca_ptl_tcp_send(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_send_request_t*,
    size_t size,
    bool* complete
);
                                                                                                 

#endif

