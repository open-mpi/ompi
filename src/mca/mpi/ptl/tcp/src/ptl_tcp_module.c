/*
 * $HEADER$
 */
#include <errno.h>
#include "lam/constants.h"
#include "lam/util/if.h"
#include "lam/util/argv.h"
#include "lam/util/output.h"
#include "lam/mem/malloc.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/lam/base/param.h"
#include "mca/lam/base/module_exchange.h"
#include "ptl_tcp.h"

mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module = {
    {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */
                                                                                                                            
    {
    /* Indicate that we are a pml v1.0.0 module (which also implies a
       specific MCA version) */
                                                                                                                            
    MCA_PML_BASE_VERSION_1_0_0,
                                                                                                                            
    "tcp", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_ptl_tcp_module_open,  /* module open */
    mca_ptl_tcp_module_close  /* module close */
    },
                                                                                                                            
    /* Next the MCA v1.0.0 module meta data */
                                                                                                                            
    {
    /* Whether the module is checkpointable or not */
                                                                                                                            
    false
    },

    mca_ptl_tcp_module_init,    /* module init */
    mca_ptl_tcp_module_progress /* module progress */
    }
};


/*
 * data structure for receiving reactor callbacks
 */

static void mca_ptl_tcp_module_recv(int sd, void*);
static void mca_ptl_tcp_module_send(int sd, void*);
static void mca_ptl_tcp_module_except(int sd, void*);

static lam_reactor_listener_t mca_ptl_tcp_module_listener = {
    mca_ptl_tcp_module_recv,
    mca_ptl_tcp_module_send,
    mca_ptl_tcp_module_except,
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_ptl_tcp_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","tcp",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_ptl_tcp_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("ptl","tcp",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
                                                                                                                            
/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int mca_ptl_tcp_module_open(void)
{
    /* register TCP module parameters */
    mca_ptl_tcp_module.tcp_if_include =
        mca_ptl_tcp_param_register_string("if-include", "");
    mca_ptl_tcp_module.tcp_if_exclude =
        mca_ptl_tcp_param_register_string("if-exclude", "");
    mca_ptl_tcp.super.ptl_exclusivity =
        mca_ptl_tcp_param_register_int("exclusivity", 0);
    mca_ptl_tcp.super.ptl_first_frag_size =
        mca_ptl_tcp_param_register_int("first-frag-size", 16*1024);
    mca_ptl_tcp.super.ptl_min_frag_size = 
        mca_ptl_tcp_param_register_int("min-frag-size", 64*1024);
    mca_ptl_tcp.super.ptl_max_frag_size =
        mca_ptl_tcp_param_register_int("max-frag-size", -1);
    return LAM_SUCCESS;
}

int mca_ptl_tcp_module_close(void)
{
    return LAM_SUCCESS;
}


/*
 * Create a TCP PTL instance for either:
 * (1) all interfaces specified by the user
 * (2) all available interfaces 
 * (3) all available interfaces except for those excluded by the user
 */

static int mca_ptl_tcp_module_create_instances(void)
{
    int if_count = lam_ifcount();
    int if_index;
    char **include;
    char **exclude;
    char **argv;

    if(if_count <= 0)
        return LAM_ERROR;

    /* allocate memory for ptls */
    mca_ptl_tcp_module.tcp_max_ptls = if_count;
    mca_ptl_tcp_module.tcp_ptls = (mca_ptl_tcp_t**)LAM_MALLOC(if_count * sizeof(mca_ptl_tcp_t*));
    if(NULL == mca_ptl_tcp_module.tcp_ptls)
        return LAM_ERR_OUT_OF_RESOURCE;

    /* if the user specified an interface list - use these only */
    argv = include = lam_argv_split(mca_ptl_tcp_module.tcp_if_include,'\'');
    while(argv && *argv) {
        char* if_name = *argv;
        int if_index = lam_ifnametoindex(if_name);
        if(if_index < 0) {
            lam_output(0,"mca_ptl_tcp_module_init: invalid interface \"%s\"", if_name);
        } else {
            mca_ptl_tcp_create(if_index);
        }
        argv++;
    }
    lam_argv_free(include);

    /* if the interface list was not specified by the user, create 
     * a PTL for each interface that was not excluded.
    */
    exclude = lam_argv_split(mca_ptl_tcp_module.tcp_if_exclude,'\'');
    for(if_index = lam_ifbegin(); if_index >= 0; if_index = lam_ifnext(if_index)) {
        char if_name[32];
        lam_ifindextoname(if_index, if_name, sizeof(if_name));

        /* check to see if this interface exists in the exclude list */
        argv = exclude;
        while(argv && *argv) {
            if(strcmp(*argv,if_name) == 0)
                break;
            argv++;
        }
        /* if this interface was not found in the excluded list - create a PTL */
        if(argv == 0 || *argv == 0)
            mca_ptl_tcp_create(if_index);
    }
    lam_argv_free(exclude);
    return LAM_SUCCESS;
}

/*
 * Create a listen socket and bind to all interfaces
 */

static int mca_ptl_tcp_module_create_listen(void)
{
    /* create a listen socket for incoming connections */
    mca_ptl_tcp_module.tcp_listen = socket(AF_INET, SOCK_STREAM, 0);
    if(mca_ptl_tcp_module.tcp_listen < 0) {
        lam_output(0,"mca_ptl_tcp_module_init: socket() failed with errno=%d", errno);
        return LAM_ERROR;
    }
                                                                                                      
    /* bind to all addresses and dynamically assigned port */
    struct sockaddr_in inaddr;
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = 0;
                                                                                                      
    if(bind(mca_ptl_tcp_module.tcp_listen, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        lam_output(0,"mca_ptl_tcp_module_init: bind() failed with errno=%d", errno);
        return LAM_ERROR;
    }
                                                                                                      
    /* resolve system assignend port */
#if defined(__linux__)
    socklen_t addrlen = sizeof(struct sockaddr_in);
#else
    int addrlen = sizeof(struct sockaddr_in);
#endif
    if(getsockname(mca_ptl_tcp_module.tcp_listen, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        lam_output(0, "mca_ptl_tcp_module_init: getsockname() failed with errno=%d", errno);
        return LAM_ERROR;
    }
    mca_ptl_tcp_module.tcp_port = inaddr.sin_port;

    /* initialize reactor and register listen port */
    lam_reactor_init(&mca_ptl_tcp_module.tcp_reactor);
    lam_reactor_insert(
        &mca_ptl_tcp_module.tcp_reactor, 
        mca_ptl_tcp_module.tcp_listen,
        &mca_ptl_tcp_module_listener,
        0,
        LAM_NOTIFY_RECV|LAM_NOTIFY_EXCEPT);

    return LAM_SUCCESS;
}

/*
 *  Register TCP module addressing information. The MCA framework
 *  will make this available to all peers. 
 *
 *  FIX: just pass around sockaddr_in for now
 */

static int mca_ptl_tcp_module_exchange(void)
{
     size_t i;
     struct sockaddr_in* addrs = (struct sockaddr_in*)LAM_MALLOC
         (mca_ptl_tcp_module.tcp_num_ptls * sizeof(struct sockaddr_in));
     for(i=0; i<mca_ptl_tcp_module.tcp_num_ptls; i++) {
         mca_ptl_tcp_t* ptl = mca_ptl_tcp_module.tcp_ptls[i];
         addrs[i] = ptl->tcp_addr;
         addrs[i].sin_port = mca_ptl_tcp_module.tcp_listen;
     }
     return mca_base_modex_send(&mca_ptl_tcp_module.super.ptlm_version,
         addrs, sizeof(struct sockaddr_in),mca_ptl_tcp_module.tcp_num_ptls);
}

/*
 *  TCP module initialization:
 *  (1) read interface list from kernel and compare against module parameters
 *      then create a PTL instance for selected interfaces
 *  (2) setup TCP listen socket for incoming connection attempts
 *  (3) register PTL parameters with the MCA
 */
mca_ptl_t** mca_ptl_tcp_module_init(int* num_ptls, int* thread_min, int* thread_max)
{
    *num_ptls = 0;
    *thread_min = MPI_THREAD_MULTIPLE;
    *thread_max = MPI_THREAD_MULTIPLE;

    /* create a PTL TCP module for selected interfaces */
    if(mca_ptl_tcp_module_create_instances() != LAM_SUCCESS)
        return 0;

    /* create a TCP listen socket for incoming connection attempts */
    if(mca_ptl_tcp_module_create_listen() != LAM_SUCCESS)
        return 0;

    /* register TCP parameters with the MCA framework */
    if(mca_ptl_tcp_module_exchange() != LAM_SUCCESS)
        return 0;

    *num_ptls = mca_ptl_tcp_module.tcp_num_ptls;
    return (mca_ptl_t**)mca_ptl_tcp_module.tcp_ptls;
}


/*
 *  All TCP progress is handled via an event loop based on select. Events
 *  are dispatched to the appropriate callbacks as file descriptors become
 *  available for read/write.
 */

void mca_ptl_tcp_module_progress(mca_ptl_base_tstamp_t tstamp)
{
    lam_reactor_poll(&mca_ptl_tcp_module.tcp_reactor);
}

                                                                                                                
/*
 *  Called by mca_ptl_tcp_module_recv() when the TCP listen
 *  socket has pending connection requests. Accept incoming
 *  requests and queue for completion of the connection handshake.
 *  We wait for the peer to send a 4 byte global process ID(rank)
 *  to complete the connection.
*/

static void mca_ptl_tcp_module_accept(void)
{
#if defined(__linux__)
    socklen_t addrlen = sizeof(struct sockaddr_in);
#else
    int addrlen = sizeof(struct sockaddr_in);
#endif
    while(true) {
        struct sockaddr_in addr;
        int sd = accept(mca_ptl_tcp_module.tcp_listen, (struct sockaddr*)&addr, &addrlen);
        if(sd < 0) {
            if(errno == EINTR)
                continue;
            if(errno != EAGAIN || errno != EWOULDBLOCK)
                lam_output(0, "mca_ptl_tcp_module_accept: accept() failed with errno %d.", errno);
            return;
        }
 
        /* wait for receipt of data to complete the connect */
        lam_reactor_insert(
            &mca_ptl_tcp_module.tcp_reactor, 
            sd,
            &mca_ptl_tcp_module_listener,
            0,
            LAM_NOTIFY_RECV|LAM_NOTIFY_EXCEPT);
    }
}


/*
 * Called by reactor when registered socket is ready to read.
 */
static void mca_ptl_tcp_module_recv(int sd, void* user)
{
    /* accept new connections on the listen socket */
    if(mca_ptl_tcp_module.tcp_listen == sd) {
        mca_ptl_tcp_module_accept();
        return;
    }
    lam_reactor_remove(&mca_ptl_tcp_module.tcp_reactor, sd, LAM_NOTIFY_ALL);
    
}

static void mca_ptl_tcp_module_send(int sd, void* user)
{
}

static void mca_ptl_tcp_module_except(int sd, void* user)
{
}

