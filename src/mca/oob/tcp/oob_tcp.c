/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include "util/output.h"
#include "mca/oob/tcp/oob_tcp.h"


static int  mca_oob_tcp_create_listen(void);
static void mca_oob_tcp_recv_handler(int sd, short flags, void* user);
static void mca_oob_tcp_accept(void);


/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_tcp_component_t mca_oob_tcp_component = {
  {
    {
        MCA_OOB_BASE_VERSION_1_0_0,
        "tcp", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_oob_tcp_open,  /* module open */
        mca_oob_tcp_close /* module close */
    },
    {
        false /* checkpoint / restart */
    },
    mca_oob_tcp_init,    /* module init */
    mca_oob_tcp_finalize
  }
};

static mca_oob_t mca_oob_tcp = {
    mca_oob_tcp_send,
    mca_oob_tcp_recv,
    mca_oob_tcp_send_nb,
    mca_oob_tcp_recv_nb
};


/*
 * Utility function to register/lookup module parameters.
 */

static inline int mca_oob_tcp_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("ptl","tcp",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/*
 * Initialize global variables used w/in this module.
 */
int mca_oob_tcp_open(void)
{
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_list,  ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_tree,  ompi_rb_tree_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_peer_free,  ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msgs,       ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_lock,       ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msg_post,   ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_msg_recv,   ompi_list_t);
    OBJ_CONSTRUCT(&mca_oob_tcp_component.tcp_match_lock, ompi_mutex_t);

    /* register oob module parameters */
    mca_oob_tcp_component.tcp_peer_limit =
        mca_oob_tcp_param_register_int("peer_limit", -1);
    mca_oob_tcp_component.tcp_peer_retries =
        mca_oob_tcp_param_register_int("peer_retries", 60);

    /* initialize state */
    mca_oob_tcp_component.tcp_listen_sd = -1;
    return OMPI_SUCCESS;
}


/*
 * Cleanup of global variables used by this module.
 */

int mca_oob_tcp_close(void)
{
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_list);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_tree);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_peer_free);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msgs);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_lock);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msg_post);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_msg_recv);
    OBJ_DESTRUCT(&mca_oob_tcp_component.tcp_match_lock);
    return OMPI_SUCCESS;
}


/*
 *  Called by mca_oob_tcp_recv_handler() when the TCP listen
 *  socket has pending connection requests. Accept incoming
 *  requests and queue for completion of the connection handshake.
*/

static void mca_oob_tcp_accept(void)
{
    while(true) {
        ompi_socklen_t addrlen = sizeof(struct sockaddr_in);
        struct sockaddr_in addr;
        ompi_event_t* event;
        int sd = accept(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&addr, &addrlen);
        if(sd < 0) {
            if(errno == EINTR)
                continue;
            if(errno != EAGAIN || errno != EWOULDBLOCK)
                ompi_output(0, "mca_oob_tcp_accept: accept() failed with errno %d.", errno);
            return;
        }
                                                                                                                   
        /* wait for receipt of peers process identifier to complete this connection */
        event = malloc(sizeof(ompi_event_t));
        ompi_event_set(event, sd, OMPI_EV_READ|OMPI_EV_PERSIST, mca_oob_tcp_recv_handler, event);
        ompi_event_add(event, 0);
    }
}

/*
 * Create a listen socket and bind to all interfaces
 */
                                                                                                                   
static int mca_oob_tcp_create_listen(void)
{
    int flags;
    struct sockaddr_in inaddr;
    ompi_socklen_t addrlen;
                                                                                                                   
    /* create a listen socket for incoming connections */
    mca_oob_tcp_component.tcp_listen_sd = socket(AF_INET, SOCK_STREAM, 0);
    if(mca_oob_tcp_component.tcp_listen_sd < 0) {
        ompi_output(0,"mca_oob_tcp_component_init: socket() failed with errno=%d", errno);
        return OMPI_ERROR;
    }
                                                                                                                   
    /* bind to all addresses and dynamically assigned port */
    memset(&inaddr, 0, sizeof(inaddr));
    inaddr.sin_family = AF_INET;
    inaddr.sin_addr.s_addr = INADDR_ANY;
    inaddr.sin_port = htons(5000+mca_oob_name_self.vpid);
                                                                                                                   
    if(bind(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, sizeof(inaddr)) < 0) {
        ompi_output(0,"mca_oob_tcp_create_listen: bind() failed with errno=%d", errno);
        return OMPI_ERROR;
    }
                                                                                                                   
    /* resolve system assignend port */
    addrlen = sizeof(struct sockaddr_in);
    if(getsockname(mca_oob_tcp_component.tcp_listen_sd, (struct sockaddr*)&inaddr, &addrlen) < 0) {
        ompi_output(0, "mca_oob_tcp_create_listen: getsockname() failed with errno=%d", errno);
        return OMPI_ERROR;
    }
    mca_oob_tcp_component.tcp_listen_port = inaddr.sin_port;
                                                                                                                   
    /* setup listen backlog to maximum allowed by kernel */
    if(listen(mca_oob_tcp_component.tcp_listen_sd, SOMAXCONN) < 0) {
        ompi_output(0, "mca_oob_tcp_component_init: listen() failed with errno=%d", errno);
        return OMPI_ERROR;
    }
                                                                                                                   
    /* set socket up to be non-blocking, otherwise accept could block */
    if((flags = fcntl(mca_oob_tcp_component.tcp_listen_sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_oob_tcp_component_init: fcntl(F_GETFL) failed with errno=%d", errno);
        return OMPI_ERROR;
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(mca_oob_tcp_component.tcp_listen_sd, F_SETFL, flags) < 0) {
            ompi_output(0, "mca_oob_tcp_component_init: fcntl(F_SETFL) failed with errno=%d", errno);
            return OMPI_ERROR;
        }
    }
                                                                                                                   
    /* register listen port */
    ompi_event_set(
        &mca_oob_tcp_component.tcp_recv_event,
        mca_oob_tcp_component.tcp_listen_sd,
        OMPI_EV_READ|OMPI_EV_PERSIST,
        mca_oob_tcp_recv_handler,
        0);
    ompi_event_add(&mca_oob_tcp_component.tcp_recv_event, 0);
    return OMPI_SUCCESS;
}


/*
 * Event callback when there is data available on the registered
 * socket to recv.
 */

static void mca_oob_tcp_recv_handler(int sd, short flags, void* user)
{
    ompi_process_name_t name;
    mca_oob_tcp_peer_t* peer;
    int rc;

    /* accept new connections on the listen socket */
    if(mca_oob_tcp_component.tcp_listen_sd == sd) {
        mca_oob_tcp_accept();
        return;
    }
    ompi_event_del((ompi_event_t*)user);
    free(user);

    /* recv the process identifier */
    rc = recv(sd, &name, sizeof(name), 0);
    if(rc != sizeof(name)) {
        ompi_output(0, "mca_oob_tcp_recv_handler: recv() return value %d != %d, errno = %d",
            rc, sizeof(name), errno);
        close(sd);
        return;
    }
    OMPI_PROCESS_NAME_NTOH(name);

    /* now set socket up to be non-blocking */
    if((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        ompi_output(0, "mca_oob_tcp_recv_handler: fcntl(F_GETFL) failed with errno=%d", errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(sd, F_SETFL, flags) < 0) {
            ompi_output(0, "mca_oob_tcp_recv_handler: fcntl(F_SETFL) failed with errno=%d", errno);
        }
    }

    /* lookup the corresponding process */
    peer = mca_oob_tcp_peer_lookup(&name, true);
    if(NULL == peer) {
        ompi_output(0, "mca_oob_tcp_recv_handler: unable to locate peer");
        close(sd);
        return;
    }
    /* is the peer instance willing to accept this connection */
    if(mca_oob_tcp_peer_accept(peer, sd) == false) {
        ompi_output(0, "mca_oob_tcp_recv_handler: peer instance not willing to accept connection.");
        close(sd);
        return;
    }
}

/*
 * Module initialization.
 * (1) initialize static resources
 * (2) create listen socket
 */
mca_oob_t* mca_oob_tcp_init(bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    /* initialize data structures */
    ompi_rb_tree_init(&mca_oob_tcp_component.tcp_peer_tree, (ompi_rb_tree_comp_fn_t)mca_oob_tcp_process_name_compare);

    ompi_free_list_init(&mca_oob_tcp_component.tcp_peer_free,
        sizeof(mca_oob_tcp_peer_t),
        OBJ_CLASS(mca_oob_tcp_peer_t),
        8,  /* initial number */
        mca_oob_tcp_component.tcp_peer_limit, /* maximum number */
        8,  /* increment to grow by */
        NULL); /* use default allocator */

    ompi_free_list_init(&mca_oob_tcp_component.tcp_msgs,
        sizeof(mca_oob_tcp_msg_t),
        OBJ_CLASS(mca_oob_tcp_msg_t),
        8,  /* initial number */
       -1,  /* maximum number */
        8,  /* increment to grow by */
        NULL); /* use default allocator */

#if 0
    /* intialize event library */
    memset(&mca_oob_tcp_component.tcp_recv_event, 0, sizeof(ompi_event_t));
    memset(&mca_oob_tcp_component.tcp_send_event, 0, sizeof(ompi_event_t));
    if(ompi_event_init() != OMPI_SUCCESS) {
        ompi_output(0, "mca_oob_tcp_init: unable to initialize event library\n");
        return NULL;
    }

    /* create a listen socket */
    if(mca_oob_tcp_create_listen() != OMPI_SUCCESS) {
        ompi_output(0, "mca_oob_tcp_init: unable to create listen socket\n");
        return NULL;
    }
    return &mca_oob_tcp;
#else
    return NULL;
#endif
}


/*
 * Module cleanup.
 */
int mca_oob_tcp_finalize(void)
{
    int optval;
    mca_oob_tcp_peer_t * peer;
    if (mca_oob_tcp_component.tcp_listen_sd >= 0) {
        optval = 1;
        if(setsockopt(mca_oob_tcp_component.tcp_listen_sd, SOL_SOCKET, 
                      SO_REUSEADDR, &optval, sizeof(optval)) < 0) {
            ompi_output(0,
                "mca_oob_tcp_finalize: setsockopt(SO_REUSEADDR) failed with errno=%d\n",
                 errno);
        }
        /*** temporarily disable the removal of the revieve event
         * to prevent segfaults */
         ompi_event_del(&mca_oob_tcp_component.tcp_recv_event); 
        if(0 != close(mca_oob_tcp_component.tcp_listen_sd)) {
            ompi_output(0, "mca_oob_tcp_finalize: error closing listen socket. errno=%d", errno);
        }
    }
    /* TODO: need to cleanup all peers - check for pending send/recvs. etc. */
    while(NULL != (peer = (mca_oob_tcp_peer_t *) 
        ompi_list_remove_first(&mca_oob_tcp_component.tcp_peer_list))) {
        OBJ_DESTRUCT(peer);
    }
    ompi_event_fini();
 
    return OMPI_SUCCESS;
}

/*
* Compare two process names for equality.
*
* @param  n1  Process name 1.
* @param  n2  Process name 2.
* @return     (-1 for n1<n2 0 for equality, 1 for n1>n2)
*
* Note that the definition of < or > is somewhat arbitrary -
* just needs to be consistently applied to maintain an ordering
* when process names are used as indices.
*/


int mca_oob_tcp_process_name_compare(const ompi_process_name_t* n1, const ompi_process_name_t* n2)
{
   if(n1->cellid < n2->cellid)
       return -1;
   else if(n1->cellid > n2->cellid)
       return 1;
   else if(n1->jobid < n2->jobid)
       return -1;
   else if(n1->jobid > n2->jobid)
       return 1;
   else if(n1->vpid < n2->vpid)
       return -1;
   else if(n1->vpid > n2->vpid)
       return 1;
   return(0);
}

