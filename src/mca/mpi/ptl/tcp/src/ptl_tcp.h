/* @file
 *
 * $HEADER$
 */

#ifndef MCA_PTL_TCP_H_
#define MCA_PTL_TCP_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "lam/util/reactor.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"


/*
 * TCP PTL module.
 */

struct mca_ptl_tcp_module_1_0_0_t {
    mca_ptl_base_module_1_0_0_t super;
    struct mca_ptl_tcp_t** tcp_ptls;
    size_t tcp_num_ptls;         /**< number of ptls actually used */
    size_t tcp_max_ptls;         /**< maximum number of ptls - available kernel ifs */
    lam_reactor_t tcp_reactor;
    int tcp_listen;
    unsigned short tcp_port;
    char* tcp_if_include;        /**< comma seperated list of interface to include */
    char* tcp_if_exclude;        /**< comma seperated list of interface to exclude */
};
typedef struct mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module_1_0_0_t;
typedef struct mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module_t;

extern mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module;

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
 */

struct mca_ptl_tcp_t {
    mca_ptl_t super; 
    int tcp_ifindex;
    struct sockaddr_in tcp_addr;
};
typedef struct mca_ptl_tcp_t mca_ptl_tcp_t;

extern mca_ptl_tcp_t mca_ptl_tcp;


extern int mca_ptl_tcp_create(
    int if_index
);

extern int mca_ptl_tcp_fini(
    struct mca_ptl_t* ptl
);

extern int mca_ptl_tcp_add_proc(
    struct mca_ptl_t* ptl,
    struct lam_proc_t *procs,
    struct mca_ptl_peer_t** addr
);

extern int mca_ptl_tcp_del_proc(
    struct mca_ptl_t* ptl,
    struct lam_proc_t *procs,
    struct mca_ptl_peer_t* addr
);

extern int mca_ptl_tcp_request_alloc(
    struct mca_ptl_t* ptl,
    struct mca_ptl_base_send_request_t**
);
                                                                                                 
extern int mca_ptl_tcp_send(
    struct mca_ptl_t* ptl,
    struct mca_ptl_peer_t* ptl_peer,
    struct mca_ptl_base_send_request_t*,
    size_t size,
    bool* complete
);
                                                                                                 

#endif

