/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>
#include "mpi.h"

#include "communicator/communicator.h"
#include "op/op.h"
#include "proc/proc.h"
#include "include/constants.h"
#include "class/ompi_pointer_array.h"
#include "mca/pcm/pcm.h"
#include "mca/pml/pml.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


#define OMPI_COMM_CID_TAG 1011
#define OMPI_MAX_COMM 32768

/**
 * These functions make sure, that we determine the global result over 
 * an intra communicators (simple), an inter-communicator and a
 * pseudo inter-communicator described by two separate intra-comms
 * and a bridge-comm (intercomm-create scenario).
 */

typedef int ompi_comm_cid_allredfct (int *inbuf, int* outbuf, 
                                     int count, ompi_op_t *op, 
                                     ompi_communicator_t *comm,
                                     ompi_communicator_t *bridgecomm, 
                                     void* lleader, void* rleader );

static int ompi_comm_allreduce_intra (int *inbuf, int* outbuf, 
                                      int count, ompi_op_t *op, 
                                      ompi_communicator_t *intercomm,
                                      ompi_communicator_t *bridgecomm, 
                                      void* local_leader, 
                                      void* remote_ledaer );

static int ompi_comm_allreduce_inter (int *inbuf, int *outbuf, 
                                      int count, ompi_op_t *op, 
                                      ompi_communicator_t *intercomm,
                                      ompi_communicator_t *bridgecomm, 
                                      void* local_leader, 
                                      void* remote_leader);

static int ompi_comm_allreduce_intra_bridge(int *inbuf, int* outbuf, 
                                            int count, ompi_op_t *op, 
                                            ompi_communicator_t *intercomm,
                                            ompi_communicator_t *bridgecomm, 
                                            void* local_leader, 
                                            void* remote_leader);

static int ompi_comm_allreduce_intra_oob (int *inbuf, int* outbuf, 
                                          int count, ompi_op_t *op, 
                                          ompi_communicator_t *intercomm,
                                          ompi_communicator_t *bridgecomm, 
                                          void* local_leader, 
                                          void* remote_leader);


int ompi_comm_nextcid ( ompi_communicator_t* newcomm, 
                        ompi_communicator_t* comm, 
                        ompi_communicator_t* bridgecomm, 
                        void* local_leader,
                        void* remote_leader,
                        int mode )
{

    int nextlocal_cid;
    int nextcid;
    int done=0;
    int response=0, glresponse=0;
    int flag;
    int start=ompi_mpi_communicators.lowest_free;
    int i;
    
    ompi_comm_cid_allredfct* allredfnct;

    /** 
     * Determine which implementation of allreduce we have to use
     * for the current scenario 
     */
    switch (mode) 
        {
        case OMPI_COMM_CID_INTRA: 
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra;
            break;
        case OMPI_COMM_CID_INTER:
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_inter;
            break;
        case OMPI_COMM_CID_INTRA_BRIDGE: 
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra_bridge;
            break;
#if 0 
        case OMPI_COMM_CID_INTRA_OOB: 
            allredfnct=(ompi_comm_cid_allredfct*)ompi_comm_allreduce_intra_oob;
            break;
#endif
        default: 
            return MPI_UNDEFINED;
            break;
        }

    /**
     * This is the real algorithm described in the doc 
     */
    while ( !done ){
        for (i=start; i<OMPI_MAX_COMM ;i++) {
            flag=ompi_pointer_array_test_and_set_item(&ompi_mpi_communicators, i, comm);
            if (true == flag) {
                nextlocal_cid = i;
                break;
            }
        }

        (allredfnct)(&nextlocal_cid, &nextcid, 1, MPI_MAX, comm, bridgecomm,
                     local_leader, remote_leader );
        if (nextcid == nextlocal_cid) {
            response = 1; /* fine with me */
        }
        else {
            ompi_pointer_array_set_item(&ompi_mpi_communicators, 
                                        nextlocal_cid, NULL);
            flag = ompi_pointer_array_test_and_set_item(&ompi_mpi_communicators, 
                                                         nextcid, comm );
            if (true == flag) {
                response = 1; /* works as well */
            }
            else {
                response = 0; /* nope, not acceptable */
                start  = nextcid+1; /* that's where we can start the next round */
            }
        }
        
        (allredfnct)(&response, &glresponse, 1, MPI_MIN, comm, bridgecomm,
                         local_leader, remote_leader );
        if (glresponse == 1) {
            done = 1;             /* we are done */
            break;
        }
    }
    
    /* set the according values to the newcomm */
    newcomm->c_contextid = nextcid;
    newcomm->c_f_to_c_index = newcomm->c_contextid;
    ompi_pointer_array_set_item (&ompi_mpi_communicators, nextcid, newcomm);
    
    return (MPI_SUCCESS);
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/
/* Arguments not used in this implementation:
 *  - bridgecomm
 *  - local_leader
 *  - remote_leader
 */
static int ompi_comm_allreduce_intra ( int *inbuf, int *outbuf, 
                                       int count, ompi_op_t *op, 
                                       ompi_communicator_t *comm,
                                       ompi_communicator_t *bridgecomm, 
                                       void* local_leader, 
                                       void* remote_leader )
{
    return comm->c_coll.coll_allreduce ( inbuf, outbuf, count, MPI_INT, 
                                         op,comm );
}

/* Arguments not used in this implementation:
 *  - bridgecomm
 *  - local_leader
 *  - remote_leader
 */
static int ompi_comm_allreduce_inter ( int *inbuf, int *outbuf, 
                                       int count, ompi_op_t *op, 
                                       ompi_communicator_t *intercomm,
                                       ompi_communicator_t *bridgecomm, 
                                       void* local_leader, 
                                       void* remote_leader )
{
    int local_rank, rsize;
    int i, rc;
    int *sbuf;
    int *tmpbuf=NULL;
    int *rcounts=NULL, scount=0;
    int *rdisps=NULL;

    if ( &ompi_mpi_op_sum != op && &ompi_mpi_op_prod != op &&
         &ompi_mpi_op_max != op && &ompi_mpi_op_min  != op ) {
        return MPI_ERR_OP;
    }

    if ( !OMPI_COMM_IS_INTER (intercomm)) {
        return MPI_ERR_COMM;
    }

    /* Allocate temporary arrays */
    rsize      = ompi_comm_remote_size (intercomm);
    local_rank = ompi_comm_rank ( intercomm );

    tmpbuf  = (int *) malloc ( count * sizeof(int));
    rdisps  = (int *) calloc ( rsize, sizeof(int));
    rcounts = (int *) calloc ( rsize, sizeof(int) );
    if ( NULL == tmpbuf || NULL == rdisps || NULL == rcounts ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    /* Execute the inter-allreduce: the result of our group will
       be in the buffer of the remote group */
    rc = intercomm->c_coll.coll_allreduce ( inbuf, tmpbuf, count, MPI_INT,
                                            op, intercomm );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( 0 == local_rank ) {
        MPI_Request req;

        /* for the allgatherv later */
        scount = count;

        /* local leader exchange their data and determine the overall result
           for both groups */
        rc = mca_pml.pml_irecv (outbuf, count, MPI_INT, 0, 
                                OMPI_COMM_CID_TAG, intercomm, &req );
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }
        rc = mca_pml.pml_send (tmpbuf,count,MPI_INT,0,OMPI_COMM_CID_TAG, 
                               MCA_PML_BASE_SEND_STANDARD, intercomm );
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }
        rc = mca_pml.pml_wait_all ( 1, &req, MPI_STATUS_IGNORE );
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        if ( &ompi_mpi_op_max == op ) {
            for ( i = 0 ; i < count; i++ ) {
                if (tmpbuf[i] > outbuf[i]) outbuf[i] = tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_min == op ) {
            for ( i = 0 ; i < count; i++ ) {
                if (tmpbuf[i] < outbuf[i]) outbuf[i] = tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_sum == op ) {
            for ( i = 0 ; i < count; i++ ) {
                outbuf[i] += tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_prod == op ) {
            for ( i = 0 ; i < count; i++ ) {
                outbuf[i] *= tmpbuf[i];
            }
        }
    }
 
    /* distribute the overall result to all processes in the other group.
       Instead of using bcast, we are using here allgatherv, to avoid the
       possible deadlock. Else, we need an algorithm to determine, 
       which group sends first in the inter-bcast and which receives 
       the result first.
    */
    rcounts[0] = count;
    sbuf       = outbuf;
    rc = intercomm->c_coll.coll_allgatherv (sbuf, scount, MPI_INT, outbuf,
                                            rcounts, rdisps, MPI_INT, 
                                            intercomm);
    
 exit:
    if ( NULL != tmpbuf ) {
        free ( tmpbuf );
    }
    if ( NULL != rcounts ) {
        free ( rcounts );
    }
    if ( NULL != rdisps ) {
        free ( rdisps );
    }
    
    return (rc);
}

/* Arguments not used in this implementation:
   all arguments are in use.
*/
static int ompi_comm_allreduce_intra_bridge (int *inbuf, int *outbuf, 
                                             int count, ompi_op_t *op, 
                                             ompi_communicator_t *comm,
                                             ompi_communicator_t *bcomm, 
                                             void* lleader, void* rleader )
{
    int *tmpbuf=NULL;
    int local_rank;
    int i;
    int rc;
    int local_leader, remote_leader;
    
    local_leader  = (*((int*)lleader));
    remote_leader = (*((int*)rleader));

    if ( &ompi_mpi_op_sum != op && &ompi_mpi_op_prod != op &&
         &ompi_mpi_op_max != op && &ompi_mpi_op_min  != op ) {
        return MPI_ERR_OP;
    }
    
    local_rank = ompi_comm_rank ( comm );
    tmpbuf     = (int *) malloc ( count * sizeof(int));
    if ( NULL == tmpbuf ) {
        return MPI_ERR_INTERN;
    }

    /* Intercomm_create */
    rc = comm->c_coll.coll_allreduce ( inbuf, tmpbuf, count, MPI_INT,
                                       op, comm );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if (local_rank == local_leader ) {
        MPI_Request req;
        
        rc = mca_pml.pml_irecv ( outbuf, count, MPI_INT, remote_leader,
                                 OMPI_COMM_CID_TAG, bcomm, &req );
        if ( OMPI_SUCCESS != rc ) {
            goto exit;       
        }
        rc = mca_pml.pml_send (tmpbuf, count, MPI_INT, remote_leader, 
                               OMPI_COMM_CID_TAG, 
                               MCA_PML_BASE_SEND_STANDARD,  bcomm );
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }
        rc = mca_pml.pml_wait_all ( 1, &req, MPI_STATUS_IGNORE);
        if ( OMPI_SUCCESS != rc ) {
            goto exit;
        }

        if ( &ompi_mpi_op_max == op ) {
            for ( i = 0 ; i < count; i++ ) {
                if (tmpbuf[i] > outbuf[i]) outbuf[i] = tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_min == op ) {
            for ( i = 0 ; i < count; i++ ) {
                if (tmpbuf[i] < outbuf[i]) outbuf[i] = tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_sum == op ) {
            for ( i = 0 ; i < count; i++ ) {
                outbuf[i] += tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_prod == op ) {
            for ( i = 0 ; i < count; i++ ) {
                outbuf[i] *= tmpbuf[i];
            }
        }
        
    }
    
    rc = comm->c_coll.coll_bcast ( outbuf, count, MPI_INT, local_leader, 
                                   comm);

 exit:
    if (NULL != tmpbuf ) {
        free (tmpbuf);
    }

    return (rc);
}

#ifdef HAVE_OOB
/* Arguments not used in this implementation:
 *    - bridgecomm
 *
 * lleader and rleader are the OOB contact information of the
 * root processes.
 */
static int ompi_comm_allreduce_intra_oob (int *inbuf, int *outbuf, 
                                          int count, ompi_op_t *op, 
                                          ompi_communicator_t *comm,
                                          ompi_communicator_t *bridgecomm, 
                                          void* lleader, void* rleader )
{
    int *tmpbuf=NULL;
    int i;
    int rc;
    uint32_t local_leader, remote_leader;
    uint32_t local_rank;
    
    local_leader  = (*((uint32_t*)lleader));
    remote_leader = (*((uint32_t*)rleader));

    if ( &ompi_mpi_op_sum != op && &ompi_mpi_op_prod != op &&
         &ompi_mpi_op_max != op && &ompi_mpi_op_min  != op ) {
        return MPI_ERR_OP;
    }
    
    /* 
     * To be done: determine my own OOB contact information.
     *             store it in local_rank.
     */

    tmpbuf     = (int *) malloc ( count * sizeof(int));
    if ( NULL == tmpbuf ) {
        return MPI_ERR_INTERN;
    }

    /* Intercomm_create */
    rc = comm->c_coll.coll_allreduce_intra ( inbuf, tmpbuf, count, MPI_INT,
                                             op, comm );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if (local_rank == local_leader ) {
        MPI_Request req;
        MPI_Status status;
        
        /* 
         * To be done:: OOB sendrecv between the two leaders the local 
         * result.
         */
        if ( &ompi_mpi_op_max == op ) {
            for ( i = 0 ; i < count; i++ ) {
                if (tmpbuf[i] > outbuf[i]) outbuf[i] = tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_min == op ) {
            for ( i = 0 ; i < count; i++ ) {
                if (tmpbuf[i] < outbuf[i]) outbuf[i] = tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_sum == op ) {
            for ( i = 0 ; i < count; i++ ) {
                outbuf[i] += tmpbuf[i];
            }
        }
        else if ( &ompi_mpi_op_prod == op ) {
            for ( i = 0 ; i < count; i++ ) {
                outbuf[i] *= tmpbuf[i];
            }
        }
        
    }
    
    rc = comm->c_coll.coll_bcast_intra ( outbuf, count, MPI_INT, 
                                         local_leader, comm);

 exit:
    if (NULL != tmpbuf ) {
        free (tmpbuf);
    }

    return (rc);
}


#endif
