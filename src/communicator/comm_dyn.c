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
#include "util/pack.h"
#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pml/pml.h"
#include "mca/ns/base/base.h"

#include "mca/pml/pml.h"
#include "mca/oob/base/base.h"

int ompi_comm_connect_accept ( ompi_communicator_t *comm, int root,
                               ompi_process_name_t *port, int send_first,
                               ompi_communicator_t **newcomm )
{
    int size, rsize, rank, rc;
    int namebuflen, rnamebuflen;
    char *namebuf=NULL, *rnamebuf=NULL;

    ompi_buffer_t sbuf;
    ompi_buffer_t rbuf;
    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t **rprocs=NULL;
    ompi_group_t *group=comm->c_local_group;
    ompi_process_name_t *rport=NULL;

    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );

    if ( rank == root ) {
        /* The process receiving first does not have yet the contact 
           information of the remote process. Therefore, we have to
           exchange that.
        */
        rport = ompi_comm_get_rport (port,send_first,group->grp_proc_pointers[rank]);

        /* Exchange number of processes and msg length on both sides */
        ompi_proc_get_namebuf_by_proc (group->grp_proc_pointers,
                                       size, &namebuf, &namebuflen);

        ompi_buffer_init(&sbuf, 128);
        ompi_pack(sbuf, &size, 1, OMPI_INT32);
        ompi_pack(sbuf, &namebuflen, 1, OMPI_INT32);
        ompi_pack(sbuf, &rsize, 1, OMPI_INT32);
        ompi_pack(sbuf, &rnamebuflen, 1, OMPI_INT32);

        if ( send_first ) {
            rc = mca_oob_send_packed(rport, sbuf, 0, 0);
            rc = mca_oob_recv_packed (rport, &rbuf, NULL);
        }
        else {
            rc = mca_oob_recv_packed(rport, &rbuf, NULL);
            rc = mca_oob_send_packed(rport, sbuf, 0, 0);
        }

        ompi_unpack(rbuf, &size, 1, OMPI_INT32);
        ompi_unpack(rbuf, &namebuflen, 1, OMPI_INT32);
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
    rnamebuf = (char *) malloc (rnamebuflen);
    if ( NULL == rnamebuf ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    if ( rank == root ) {
        /* Exchange list of processes in the groups */
        
        
        ompi_buffer_init(&sbuf, 128);
        ompi_pack(sbuf, namebuf, namebuflen, OMPI_BYTE);
        
        if ( send_first ) {
            rc = mca_oob_send_packed(rport, sbuf, 0, 0);
            rc = mca_oob_recv_packed (rport, &rbuf, NULL);
        }
        else {
            rc = mca_oob_recv_packed(rport, &rbuf, NULL);
            rc = mca_oob_send_packed(rport, sbuf, 0, 0);
        }

        ompi_unpack(rbuf, rnamebuf, rnamebuflen, OMPI_BYTE);

        ompi_buffer_free(sbuf);
        ompi_buffer_free(rbuf);
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
    rc = ompi_proc_get_proclist (rnamebuf, rnamebuflen, rsize, &rprocs);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
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
    if ( NULL != rnamebuf ) {
        free ( rnamebuf );
    }
    if ( NULL != namebuf ) {
        ompi_proc_namebuf_returnbuf (namebuf);
    }
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
                                          ompi_proc_t *proc)
{
    int namebuflen, rc;
    char *namebuf=NULL;
    ompi_proc_t **rproc;
    ompi_process_name_t *rport;
    

    if ( send_first ) {
        ompi_buffer_t sbuf;
        ompi_proc_get_namebuf_by_proc(&proc, 1, &namebuf, &namebuflen );

        ompi_buffer_init(&sbuf, sizeof(int));
        ompi_pack(sbuf, &namebuflen, 1, OMPI_INT32);
        rc = mca_oob_send_packed(port, sbuf, 0, 0);
        ompi_buffer_free(sbuf);

        ompi_buffer_init(&sbuf, namebuflen);
        ompi_pack(sbuf, namebuf, namebuflen, OMPI_BYTE);
        rc = mca_oob_send_packed(port, sbuf, 0, 0);
        ompi_buffer_free(sbuf);

        ompi_proc_namebuf_returnbuf (namebuf);
        rport = port;
    }
    else {
        ompi_buffer_t rbuf;
        rc = mca_oob_recv_packed(MCA_OOB_NAME_ANY, &rbuf, NULL);
        ompi_unpack(rbuf, &namebuflen, 1, OMPI_INT32);
        ompi_buffer_free(rbuf);

        namebuf = (char *) malloc (namebuflen);
        if ( NULL != namebuf ) {
            return NULL;
        }

        rc = mca_oob_recv_packed(MCA_OOB_NAME_ANY, &rbuf, NULL);
        ompi_unpack(rbuf, namebuf, namebuflen, OMPI_BYTE);
        ompi_buffer_free(rbuf);

        ompi_proc_get_proclist (namebuf, namebuflen, 1, &rproc);
        rport = &(rproc[0]->proc_name);
        free (rproc);
        free (namebuf);
    }
    
    return rport;
}
