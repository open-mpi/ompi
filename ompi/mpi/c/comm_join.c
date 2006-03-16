/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#include <errno.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "ompi/mpi/c/bindings.h"
#include "orte/mca/ns/ns.h"
#include "ompi/proc/proc.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_join = PMPI_Comm_join
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_join";

static int ompi_socket_send (int fd, char *buf, int len );
static int ompi_socket_recv (int fd, char *buf, int len );

int MPI_Comm_join(int fd, MPI_Comm *intercomm) 
{
    int rc, tag=OMPI_COMM_JOIN_TAG;
    size_t size;
    uint32_t len, rlen, llen, lrlen;
    int send_first=1;
    char *rname, *name;

    ompi_proc_t **myproc=NULL;
    ompi_communicator_t *newcomp;
    orte_process_name_t *port_proc_name=NULL;

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
    if (ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string (&name, &(myproc[0]->proc_name)))) {
        return rc;
    }
    llen   = strlen(name)+1;
    len    = htonl(llen);
    
    ompi_socket_send( fd, (char *) &len, sizeof(uint32_t));
    ompi_socket_recv (fd, (char *) &rlen, sizeof(uint32_t));

    lrlen  = ntohl(rlen);
    rname = (char *) malloc (lrlen);
    if ( NULL == rname ) {
        *intercomm = MPI_COMM_NULL;
        return MPI_ERR_INTERN;
    }

    /* Assumption: socket_send should not block, even if the socket 
       is not configured to be non-blocking, because the message length are
       so short. */
    ompi_socket_send (fd, name, llen);
    ompi_socket_recv (fd, rname, lrlen);
    
    if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_process_name(&port_proc_name, rname))) {
        return rc;
    }
    rc = ompi_comm_connect_accept (MPI_COMM_SELF, 0, port_proc_name,
                                   send_first, &newcomp, tag);
    
    
    free ( name );
    free ( rname);
    free ( port_proc_name );
    free ( myproc );

    *intercomm = newcomp;
    OMPI_ERRHANDLER_RETURN (rc, MPI_COMM_SELF, rc, FUNC_NAME);
}


static int ompi_socket_send (int fd, char *buf, int len )
{
    int num;
    size_t s_num;
    ssize_t a;
    char *c_ptr;
    int ret = OMPI_SUCCESS;
    
    num      = (size_t) len;
    c_ptr      = buf;

    do {
	s_num = (size_t) num;
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
                fprintf (stderr,"ompi_socket_send: error while writing to socket" 
                         " error:%s", strerror (errno) );
                return MPI_ERR_OTHER;
	    }
	}
        num      -= a;
        c_ptr    += a;
    }   while ( num > 0 );
    

  if ( num < 0 )  {
      fprintf (stderr, "ompi_socket_send: more data written then available");
      ret = MPI_ERR_INTERN;
  }
    
  return ret;
}

static int ompi_socket_recv (int fd, char *buf, int len )
{
    int num;
    size_t s_num;
    ssize_t a;
    char *c_ptr;
    int ret = MPI_SUCCESS;
    
    num      = len;
    c_ptr      = buf;

    do {
	s_num = (size_t ) num;
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
                fprintf (stderr,"ompi_socket_recv: error while reading from socket" 
                         " error:%s", strerror (errno) );
                return MPI_ERR_OTHER;
	    }
	}
        num    -= a;
        c_ptr  += a;
    }   while ( num > 0 );

  if ( num < 0 )  {
      fprintf (stderr, "ompi_socket_recv: more data read then available");
      ret = MPI_ERR_INTERN;
  }

  return ret;
}
