/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>
#include <sys/uio.h>
#include "mpi.h"

#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "proc/proc.h"
#include "threads/mutex.h"
#include "util/bit_ops.h"
#include "util/bufpack.h"
#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pml/pml.h"
#include "mca/ns/base/base.h"

#include "mca/pml/pml.h"
#include "mca/oob/base/base.h"

int ompi_comm_connect_accept ( ompi_communicator_t *comm, int root,
                               ompi_process_name_t *port, int send_first,
                               ompi_communicator_t **newcomm, int tag )
{
    int size, rsize, rank, rc;
    int namebuflen, rnamebuflen;
    void *namebuf=NULL, *rnamebuf=NULL;

    ompi_buffer_t sbuf;
    ompi_buffer_t rbuf;
    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t **rprocs=NULL;
    ompi_group_t *group=comm->c_local_group;
    ompi_process_name_t *rport=NULL;
    ompi_buffer_t nbuf, nrbuf;

    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );

    if ( rank == root ) {
        /* The process receiving first does not have yet the contact 
           information of the remote process. Therefore, we have to
           exchange that.
        */
	if ( OMPI_COMM_JOIN_TAG != tag ) {
	    rport = ompi_comm_get_rport (port,send_first,
					 group->grp_proc_pointers[rank], tag);
	}
	else {
	    rport = port;
	}
	    

        /* Exchange number of processes and msg length on both sides */
	ompi_buffer_init (&nbuf, size*sizeof(ompi_process_name_t));
        ompi_proc_get_namebuf (group->grp_proc_pointers, size, nbuf);
	ompi_buffer_get(nbuf, &namebuf, &namebuflen);

        ompi_buffer_init(&sbuf, 64);
        ompi_pack(sbuf, &size, 1, OMPI_INT32);
        ompi_pack(sbuf, &namebuflen, 1, OMPI_INT32);

        if ( send_first ) {
            rc = mca_oob_send_packed(rport, sbuf, tag, 0);
            rc = mca_oob_recv_packed (rport, &rbuf, &tag);
        }
        else {
            rc = mca_oob_recv_packed(rport, &rbuf, &tag);
            rc = mca_oob_send_packed(rport, sbuf, tag, 0);
        }

        ompi_unpack(rbuf, &rsize, 1, OMPI_INT32);
        ompi_unpack(rbuf, &rnamebuflen, 1, OMPI_INT32);

        ompi_buffer_free(sbuf);
        ompi_buffer_free(rbuf);
    }

    /* bcast the information to all processes in the local comm */
    rc = comm->c_coll.coll_bcast (&rsize, 1, MPI_INT, root, comm );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
    rc = comm->c_coll.coll_bcast (&rnamebuflen, 1, MPI_INT, root, comm );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( rank == root ) {
        /* Exchange list of processes in the groups */
        
        if ( send_first ) {
            rc = mca_oob_send_packed(rport, nbuf, tag, 0);
            rc = mca_oob_recv_packed (rport, &nrbuf, &tag);
        }
        else {
            rc = mca_oob_recv_packed(rport, &nrbuf, &tag);
            rc = mca_oob_send_packed(rport, nbuf, tag, 0);
        }
	ompi_buffer_get(nrbuf, &rnamebuf, &rnamebuflen);
    }
    else {
	/* non root processes need to allocate the buffer manually */
	rnamebuf = (char *) malloc(rnamebuflen);
	if ( NULL == rnamebuf ) {
	    rc = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
	}
	ompi_buffer_init_preallocated(&nrbuf, rnamebuf, rnamebuflen);
    }
    /* bcast list of processes to all procs in local group 
       and reconstruct the data. Note that proc_get_proclist
       adds processes, which were not known yet to our
       process pool.
    */
    rc = comm->c_coll.coll_bcast (rnamebuf, rnamebuflen, MPI_BYTE, root, comm );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
    rc = ompi_proc_get_proclist (nrbuf, rsize, &rprocs);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    ompi_buffer_free (nrbuf);
    if ( rank == root ) {
	ompi_buffer_free (nbuf);
    }

    /* allocate comm-structure */
    newcomp = ompi_comm_allocate ( size, rsize );
    if ( NULL == newcomp ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    /* allocate comm_cid */
    rc = ompi_comm_nextcid ( newcomp,                 /* new communicator */
                             comm,                    /* old communicator */
                             NULL,                    /* bridge comm */
                             &root,                   /* local leader */
                             rport,                   /* remote leader */
                             OMPI_COMM_CID_INTRA_OOB, /* mode */
                             send_first );            /* send or recv first */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* set up communicator structure */
    rc = ompi_comm_set ( newcomp,                  /* new comm */
                         comm,                     /* old comm */
                         group->grp_proc_count,    /* local_size */
                         group->grp_proc_pointers, /* local_procs*/
                         rsize,                    /* remote_size */
                         rprocs,                   /* remote_procs */
                         NULL,                     /* attrs */
                         comm->error_handler,      /* error handler */
                         NULL                      /* topo component */
                         );


    /* activate comm and init coll-component */
    rc = ompi_comm_activate ( newcomp,                 /* new communicator */
                              comm,                    /* old communicator */
                              NULL,                    /* bridge comm */
                              &root,                   /* local leader */
                              rport,                   /* remote leader */
                              OMPI_COMM_CID_INTRA_OOB, /* mode */
                              send_first,              /* send or recv first */
                              NULL );                  /* coll component */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* Question: do we have to re-start some low level stuff
       to enable the usage of fast communication devices
       between the two worlds ? 
    */
    
    
 exit:

    if ( NULL != rprocs ) {
        free ( rprocs );
    }
    if ( OMPI_SUCCESS != rc ) {
        if ( MPI_COMM_NULL != newcomp ) {
            OBJ_RETAIN(newcomp);
            newcomp = MPI_COMM_NULL;
        }
    }

    *newcomm = newcomp;
    return rc;
}


/*
 * This routine is necessary, since in the connect/accept case, the processes
 * executing the connect operation have the OOB contact information of the
 * leader of the remote group, however, the processes executing the 
 * accept get their own port_name = OOB contact information passed in as 
 * an argument. This is however useless.
 * 
 * Therefore, the two root processes exchange this information at this point.
 *
 */
ompi_process_name_t *ompi_comm_get_rport (ompi_process_name_t *port, int send_first, 
                                          ompi_proc_t *proc, int tag)
{
    int rc;
    ompi_process_name_t *rport, tbuf;
    ompi_proc_t *rproc=NULL;

    if ( send_first ) {
        ompi_buffer_t sbuf;

        ompi_buffer_init(&sbuf, sizeof(ompi_process_name_t));
        ompi_pack(sbuf, &(proc->proc_name), 1, OMPI_NAME);
        rc = mca_oob_send_packed(port, sbuf, tag, 0);
        ompi_buffer_free(sbuf);

        rport = port;
    }
    else {
        ompi_buffer_t rbuf;

        rc = mca_oob_recv_packed(MCA_OOB_NAME_ANY, &rbuf, &tag);
        ompi_unpack(rbuf, &tbuf, 1, OMPI_NAME);
        ompi_buffer_free(rbuf);

	rproc = ompi_proc_find_and_add(&tbuf);
        rport = &(rproc->proc_name);
    }
    
    return rport;
}
