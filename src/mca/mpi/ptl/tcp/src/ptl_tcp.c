#include "lam/util/malloc.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "ptl_tcp.h"


mca_ptl_tcp_t mca_ptl_tcp = {
    {
    &mca_ptl_tcp_module_1_0_0_0,
    0, /* ptl_frag_first_size */
    0, /* ptl_frag_min_size */
    0, /* ptl_frag_max_size */
    0, /* ptl_endpoint_latency */
    0, /* ptl_endpoint_bandwidth */
    0, /* ptl_endpoint_count */
    mca_ptl_tcp_add_procs,
    mca_ptl_tcp_fini,
    mca_ptl_tcp_send,
    mca_ptl_tcp_request_alloc
    }
};

