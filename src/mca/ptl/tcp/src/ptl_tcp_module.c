/*
 * $HEADER$
 */
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_tcp.h"
#include "ptl_tcp_addr.h"
#include "ptl_tcp_proc.h"
#include "ptl_tcp_recvfrag.h"
#include "ptl_tcp_sendfrag.h"
#include "ptl_tcp_sendreq.h"


mca_ptl_tcp_module_1_0_0_t mca_ptl_tcp_module = {
    {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */
                                                                                                                            
    {
    /* Indicate that we are a pml v1.0.0 module (which also implies a
       specific MCA version) */
                                                                                                                            
    MCA_PTL_BASE_VERSION_1_0_0,
                                                                                                                            
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

    mca_ptl_tcp_module_init   /* module init */
    }
};


/*
 * functions for receiving event callbacks
 */

static void mca_ptl_tcp_module_recv_handler(int, short, void*);


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
    /* initialize state */
    mca_ptl_tcp_module.tcp_ptls = NULL;
    mca_ptl_tcp_module.tcp_num_ptls = 0;

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_ptl_tcp_module.tcp_lock, lam_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_module.tcp_procs, lam_list_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_module.tcp_pending_acks, lam_list_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_module.tcp_send_requests, lam_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_module.tcp_send_frags, lam_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_tcp_module.tcp_recv_frags, lam_free_list_t);

    /* register TCP module parameters */
    mca_ptl_tcp_module.tcp_if_include =
        mca_ptl_tcp_param_register_string("if_include", "");
    mca_ptl_tcp_module.tcp_if_exclude =
        mca_ptl_tcp_param_register_string("if_exclude", "");
    mca_ptl_tcp_module.tcp_free_list_num =
        mca_ptl_tcp_param_register_int("free_list_num", 256);
    mca_ptl_tcp_module.tcp_free_list_max =
        mca_ptl_tcp_param_register_int("free_list_max", -1);
    mca_ptl_tcp_module.tcp_free_list_inc =
        mca_ptl_tcp_param_register_int("free_list_inc", 256);
    mca_ptl_tcp.super.ptl_exclusivity =
        mca_ptl_tcp_param_register_int("exclusivity", 0);
    mca_ptl_tcp.super.ptl_first_frag_size =
        mca_ptl_tcp_param_register_int("first_frag_size", 64*1024);
    mca_ptl_tcp.super.ptl_min_frag_size = 
        mca_ptl_tcp_param_register_int("min_frag_size", 64*1024);
    mca_ptl_tcp.super.ptl_max_frag_size =
        mca_ptl_tcp_param_register_int("max_frag_size", -1);

    return LAM_SUCCESS;
}

int mca_ptl_tcp_module_close(void)
{
    free(mca_ptl_tcp_module.tcp_if_include);
    free(mca_ptl_tcp_module.tcp_if_exclude);
    if (NULL != mca_ptl_tcp_module.tcp_ptls)
        free(mca_ptl_tcp_module.tcp_ptls);
 
    OBJ_DESTRUCT(&mca_ptl_tcp_module.tcp_procs);
    OBJ_DESTRUCT(&mca_ptl_tcp_module.tcp_send_requests);
    OBJ_DESTRUCT(&mca_ptl_tcp_module.tcp_send_frags);
    OBJ_DESTRUCT(&mca_ptl_tcp_module.tcp_recv_frags);
    OBJ_DESTRUCT(&mca_ptl_tcp_module.tcp_lock);
    return LAM_SUCCESS;
}


/*
 *  Create a ptl instance and add to modules list.
 */

static int mca_ptl_tcp_create(int if_index)
{
    mca_ptl_tcp_t* ptl = malloc(sizeof(mca_ptl_tcp_t));
    if(NULL == ptl)
        return LAM_ERR_OUT_OF_RESOURCE;
    memcpy(ptl, &mca_ptl_tcp, sizeof(mca_ptl_tcp));
    mca_ptl_tcp_module.tcp_ptls[mca_ptl_tcp_module.tcp_num_ptls++] = ptl;
                                                                                                                                
    /* initialize the ptl */
    ptl->ptl_ifindex = if_index;
    lam_ifindextoaddr(if_index, (struct sockaddr*)&ptl->ptl_ifaddr, sizeof(ptl->ptl_ifaddr));
    lam_ifindextomask(if_index, (struct sockaddr*)&ptl->ptl_ifmask, sizeof(ptl->ptl_ifmask));
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
    mca_ptl_tcp_module.tcp_ptls = malloc(if_count * sizeof(mca_ptl_tcp_t*));
    if(NULL == mca_ptl_tcp_module.tcp_ptls)
        return LAM_ERR_OUT_OF_RESOURCE;

    /* if the user specified an interface list - use these exclusively */
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
    int flags;
    struct sockaddr_in inaddr; 
    lam_socklen_t addrlen;

    /* create a listen socket for incoming connections */
    mca_ptl_tcp_module.tcp_listen_sd = socket(AF_INET, SOCK_STREAM, 0);
    if(mca_ptl_tcp_module.tcp_listen_sd < 0) {
        lam_output(0,"mca_ptl_tcp_module_init: socket() failed with errno=%d", errno);
        return LAM_ERROR;
    }
                                                                                                      
    /* bind to all addresses and dynamically assigned port */
    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = 0;
                                                                                                      
    if(bind(mca_ptl_tcp_module.tcp_listen_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        lam_output(0,"mca_ptl_tcp_module_init: bind() failed with errno=%d", errno);
        return LAM_ERROR;
    }
                                                                                                      
    /* resolve system assignend port */
    addrlen = sizeof(struct sockaddr_in);
    if(getsockname(mca_ptl_tcp_module.tcp_listen_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        lam_output(0, "mca_ptl_tcp_module_init: getsockname() failed with errno=%d", errno);
        return LAM_ERROR;
    }
    mca_ptl_tcp_module.tcp_listen_port = inaddr.sin_port;

    /* setup listen backlog to maximum allowed by kernel */
    if(listen(mca_ptl_tcp_module.tcp_listen_sd, SOMAXCONN) < 0) {
        lam_output(0, "mca_ptl_tcp_module_init: listen() failed with errno=%d", errno);
        return LAM_ERROR;
    }
                                                                                                                   
    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(mca_ptl_tcp_module.tcp_listen_sd, F_GETFL, 0)) < 0) {
        lam_output(0, "mca_ptl_tcp_module_init: fcntl(F_GETFL) failed with errno=%d", errno);
        return LAM_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(mca_ptl_tcp_module.tcp_listen_sd, F_SETFL, flags) < 0) {
            lam_output(0, "mca_ptl_tcp_module_init: fcntl(F_SETFL) failed with errno=%d", errno);
            return LAM_ERROR;
        }
    }
   
    /* register listen port */
    lam_event_set(
        &mca_ptl_tcp_module.tcp_recv_event,
        mca_ptl_tcp_module.tcp_listen_sd, 
        LAM_EV_READ|LAM_EV_PERSIST, 
        mca_ptl_tcp_module_recv_handler, 
        0);
    lam_event_add(&mca_ptl_tcp_module.tcp_recv_event, 0);
    return LAM_SUCCESS;
}

/*
 *  Register TCP module addressing information. The MCA framework
 *  will make this available to all peers. 
 */

static int mca_ptl_tcp_module_exchange(void)
{
     int rc;
     size_t i;
     size_t size = mca_ptl_tcp_module.tcp_num_ptls * sizeof(mca_ptl_tcp_addr_t);
     mca_ptl_tcp_addr_t *addrs = malloc(size);
     for(i=0; i<mca_ptl_tcp_module.tcp_num_ptls; i++) {
         mca_ptl_tcp_t* ptl = mca_ptl_tcp_module.tcp_ptls[i];
         addrs[i].addr_inet = ptl->ptl_ifaddr.sin_addr;
         addrs[i].addr_port = mca_ptl_tcp_module.tcp_listen_port;
         addrs[i].addr_inuse = 0;
     }
     rc =  mca_base_modex_send(&mca_ptl_tcp_module.super.ptlm_version, addrs, size);
     free(addrs);
     return rc;
}

/*
 *  TCP module initialization:
 *  (1) read interface list from kernel and compare against module parameters
 *      then create a PTL instance for selected interfaces
 *  (2) setup TCP listen socket for incoming connection attempts
 *  (3) register PTL parameters with the MCA
 */
mca_ptl_t** mca_ptl_tcp_module_init(int *num_ptls, 
                                    bool *allow_multi_user_threads,
                                    bool *have_hidden_threads)
{
    mca_ptl_t **ptls;
    int rc;

    *num_ptls = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = true;

    if((rc = lam_event_init()) != LAM_SUCCESS) {
        lam_output(0, "mca_ptl_tcp_module_init: unable to initialize event dispatch thread: %d\n", rc);
        return NULL;
    }

    /* initialize free lists */
    lam_free_list_init(&mca_ptl_tcp_module.tcp_send_requests, 
        sizeof(mca_ptl_tcp_send_request_t),
        OBJ_CLASS(mca_ptl_tcp_send_request_t),
        mca_ptl_tcp_module.tcp_free_list_num,
        mca_ptl_tcp_module.tcp_free_list_max,
        mca_ptl_tcp_module.tcp_free_list_inc,
        NULL); /* use default allocator */

    lam_free_list_init(&mca_ptl_tcp_module.tcp_send_frags, 
        sizeof(mca_ptl_tcp_send_frag_t),
        OBJ_CLASS(mca_ptl_tcp_send_frag_t),
        mca_ptl_tcp_module.tcp_free_list_num,
        mca_ptl_tcp_module.tcp_free_list_max,
        mca_ptl_tcp_module.tcp_free_list_inc,
        NULL); /* use default allocator */

    lam_free_list_init(&mca_ptl_tcp_module.tcp_recv_frags, 
        sizeof(mca_ptl_tcp_recv_frag_t),
        OBJ_CLASS(mca_ptl_tcp_recv_frag_t),
        mca_ptl_tcp_module.tcp_free_list_num,
        mca_ptl_tcp_module.tcp_free_list_max,
        mca_ptl_tcp_module.tcp_free_list_inc,
        NULL); /* use default allocator */

    /* create a PTL TCP module for selected interfaces */
    if(mca_ptl_tcp_module_create_instances() != LAM_SUCCESS)
        return 0;

    /* create a TCP listen socket for incoming connection attempts */
    if(mca_ptl_tcp_module_create_listen() != LAM_SUCCESS)
        return 0;

    /* publish TCP parameters with the MCA framework */
    if(mca_ptl_tcp_module_exchange() != LAM_SUCCESS)
        return 0;

    ptls = malloc(mca_ptl_tcp_module.tcp_num_ptls * sizeof(mca_ptl_t*));
    if(NULL == ptls)
        return NULL;

    memcpy(ptls, mca_ptl_tcp_module.tcp_ptls, mca_ptl_tcp_module.tcp_num_ptls*sizeof(mca_ptl_tcp_t*));
    *num_ptls = mca_ptl_tcp_module.tcp_num_ptls;
    return ptls;
}

/*
 *  Called by mca_ptl_tcp_module_recv() when the TCP listen
 *  socket has pending connection requests. Accept incoming
 *  requests and queue for completion of the connection handshake.
*/

static void mca_ptl_tcp_module_accept(void)
{
    while(true) {
        lam_socklen_t addrlen = sizeof(struct sockaddr_in);
        struct sockaddr_in addr;
        lam_event_t* event;
        int sd = accept(mca_ptl_tcp_module.tcp_listen_sd, (struct sockaddr*)&addr, &addrlen);
        if(sd < 0) {
            if(errno == EINTR)
                continue;
            if(errno != EAGAIN || errno != EWOULDBLOCK)
                lam_output(0, "mca_ptl_tcp_module_accept: accept() failed with errno %d.", errno);
            return;
        }

        /* wait for receipt of peers process identifier to complete this connection */
        event = malloc(sizeof(lam_event_t));
        lam_event_set(event, sd, LAM_EV_READ|LAM_EV_PERSIST, mca_ptl_tcp_module_recv_handler, event);
        lam_event_add(event, 0);
    }
}


/*
 * Event callback when there is data available on the registered 
 * socket to recv.
 */
static void mca_ptl_tcp_module_recv_handler(int sd, short flags, void* user)
{
    void* guid; 
    uint32_t size;
    struct sockaddr_in addr;
    int retval;
    mca_ptl_tcp_proc_t* ptl_proc;
    lam_socklen_t addr_len = sizeof(addr);

    /* accept new connections on the listen socket */
    if(mca_ptl_tcp_module.tcp_listen_sd == sd) {
        mca_ptl_tcp_module_accept();
        return;
    }
    lam_event_del((lam_event_t*)user);
    free(user);

    /* recv the size of the process identifier */
    retval = recv(sd, &size, sizeof(size), 0);
    if(retval == 0) {
        close(sd);
        return;
    }
    if(retval != sizeof(size)) {
        lam_output(0, "mca_ptl_tcp_module_recv_handler: recv() return value %d != %d, errno = %d", 
            retval, sizeof(size), errno);
        close(sd);
        return;
    }

    /* recv the identifier */
    size = ntohl(size);
    guid = malloc(size);
    if(guid == 0) {
        close(sd);
        return;
    }
    retval = recv(sd, guid, size, 0);
    if(retval != size) {
        lam_output(0, "mca_ptl_tcp_module_recv_handler: recv() return value %d != %d, errno = %d", 
            retval, sizeof(size), errno);
        close(sd);
        return;
    }

    /* lookup the corresponding process */
    ptl_proc = mca_ptl_tcp_proc_lookup(guid, size);
    if(NULL == ptl_proc) {
        lam_output(0, "mca_ptl_tcp_module_recv_handler: unable to locate process");
        close(sd);
        return;
    }

    /* lookup peer address */
    if(getpeername(sd, (struct sockaddr*)&addr, &addr_len) != 0) {
        lam_output(0, "mca_ptl_tcp_module_recv_handler: getpeername() failed with errno=%d", errno);
        close(sd);
        return;
    }

    /* are there any existing peer instances will to accept this connection */
    if(mca_ptl_tcp_proc_accept(ptl_proc, &addr, sd) == false) {
        close(sd);
        return;
    }
}

