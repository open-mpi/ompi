/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <netinet/in.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"
#include "mca/ns/base/base.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_join = PMPI_Comm_join
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_join";

static int ompi_socket_send (int fd, char *buf, int len );
static int ompi_socket_recv (int fd, char *buf, int len );

int MPI_Comm_join(int fd, MPI_Comm *intercomm) 
{
    int rc;
    size_t size;
    uint32_t len, rlen;
    int send_first;
    char *rname, *name;

    ompi_proc_t **myproc=NULL;
    ompi_communicator_t *newcomp;
    ompi_ns_cmp_bitmask_t mask;
    ompi_process_name_t *port_proc_name=NULL, *port_pname=NULL;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( NULL == intercomm ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
    }
    
    /* sendrecv OOB-name (port-name) through the socket connection.
       Need to determine somehow how to avoid a potential deadlock
       here. */
    myproc = ompi_proc_self (&size);
    name   = ompi_name_server.get_proc_name_string (&(myproc[0]->proc_name));
    len    = htonl(strlen(name));
    
    ompi_socket_send( fd, (char *) &len, sizeof(uint32_t));
    ompi_socket_recv (fd, (char *) &rlen, sizeof(uint32_t));

    rlen  = ntohl(rlen);
    rname = (char *) malloc (rlen);
    if ( NULL == rname ) {
        *intercomm = MPI_COMM_NULL;
        return MPI_ERR_INTERN;
    }

    /* Assumption: socket_send should not block, even if the socket 
       is not configured to be non-blocking, because the message length are
       so short. */
    ompi_socket_send (fd, name, len);
    ompi_socket_recv (fd, rname, rlen);
    
#if 0
    port_pname = ompi_name_server.convert_string_to_process_name(rname);
#else
    port_pname = NULL;
#endif

    mask = OMPI_NS_CMP_CELLID | OMPI_NS_CMP_JOBID | OMPI_NS_CMP_VPID;
    rc = ompi_name_server.compare (mask, &myproc[0]->proc_name, port_proc_name );
    if ( rc > 0 ) {
        send_first = 0;
    }
    else {
        send_first = 1;
        port_proc_name = port_pname;
    }

    rc = ompi_comm_connect_accept (MPI_COMM_SELF, 0, port_proc_name,
                                   send_first, &newcomp);
    
    
    free ( name );
    free ( rname);
    free ( port_pname );
    free ( myproc );

    *intercomm = newcomp;
    OMPI_ERRHANDLER_RETURN (rc, MPI_COMM_SELF, rc, FUNC_NAME);
}


static int ompi_socket_send (int fd, char *buf, int len )
{
    size_t s_num;
    ssize_t a;
    char *c_ptr;
    int ret = OMPI_SUCCESS;
    
    s_num      = (size_t) len;
    c_ptr      = buf;

    do {
        a = write ( fd, c_ptr, s_num ); 
        if ( a == -1 ) {
            if ( errno == EINTR ) {
                /* Catch EINTR on, mainly on IBM RS6000 */
                continue;
	    }
#ifdef __SR8000
            else if ( errno == EWOULDBLOCK ) {
                /*Catch EWOULDBLOCK on Hitachi SR8000 */
                continue;
            }
            else if ( errno == EAGAIN ) {
                /* Catch EAGAIN on Hitachi SR8000 */
                continue;
            }
#endif
            else {
                /* Another error occured */
                fprintf (stderr,"read_socket: error while reading from socket" 
                         " error:%s", strerror (errno) );
                return ( OMPI_ERROR);
	    }
	}
        s_num      -= (size_t) a;
        c_ptr      += a;
    }   while ( s_num > 0 );
    

  if ( s_num < 0 )  {
      fprintf (stderr, "read_socket: more data read then available");
      ret = OMPI_ERROR;
  }
    
  return ret;
}

static int ompi_socket_recv (int fd, char *buf, int len )
{
    size_t s_num;
    ssize_t a;
    char *c_ptr;
    int ret = OMPI_SUCCESS;
    
    s_num      = (size_t) len;
    c_ptr      = buf;

    do {
        a = read ( fd, c_ptr, s_num ); 
        if ( a == -1 ) {
            if ( errno == EINTR ) {
                /* Catch EINTR on, mainly on IBM RS6000 */
                continue;
	    }
#ifdef __SR8000
            else if ( errno == EWOULDBLOCK ) {
                /*Catch EWOULDBLOCK on Hitachi SR8000 */
                continue;
            }
            else if ( errno == EAGAIN ) {
                /* Catch EAGAIN on Hitachi SR8000 */
                continue;
            }
#endif
            else {
                /* Another error occured */
                fprintf (stderr,"read_socket: error while reading from socket" 
                         " error:%s", strerror (errno) );
                return ( OMPI_ERROR);
	    }
	}
        s_num      -= (size_t) a;
        c_ptr      += a;
    }   while ( s_num > 0 );
    

  if ( s_num < 0 )  {
      fprintf (stderr, "read_socket: more data read then available");
      ret = OMPI_ERROR;
  }
    

  return ret;
}
