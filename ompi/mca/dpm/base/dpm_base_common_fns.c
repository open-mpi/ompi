/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 University of Houston. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Cisco, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>

#include "ompi/request/request.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/pml/pml.h"

#include "ompi/mca/dpm/base/base.h"


char* ompi_dpm_base_dyn_init (void)
{
    char *envvarname=NULL, *port_name=NULL;

    /* check for appropriate env variable */
    asprintf(&envvarname, "OMPI_PARENT_PORT");
    port_name = getenv(envvarname);
    free (envvarname);

    return port_name;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* this routine runs through the list of communicators
   and does the disconnect for all dynamic communicators */
int ompi_dpm_base_dyn_finalize (void)
{
    int i,j=0, max=0;
    ompi_dpm_base_disconnect_obj **objs=NULL;
    ompi_communicator_t *comm=NULL;

    if ( 1 <ompi_comm_num_dyncomm ) {
        objs = (ompi_dpm_base_disconnect_obj **)malloc (ompi_comm_num_dyncomm*
                               sizeof(ompi_dpm_base_disconnect_obj*));
        if ( NULL == objs ) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        max = opal_pointer_array_get_size(&ompi_mpi_communicators);
        for ( i=3; i<max; i++ ) {
            comm = (ompi_communicator_t*)opal_pointer_array_get_item(&ompi_mpi_communicators,i);
            if (NULL != comm &&  OMPI_COMM_IS_DYNAMIC(comm)) {
                objs[j++]=ompi_dpm_base_disconnect_init(comm);
            }
        }

        if ( j != ompi_comm_num_dyncomm+1 ) {
            free (objs);
            return OMPI_ERROR;
        }

        ompi_dpm_base_disconnect_waitall (ompi_comm_num_dyncomm, objs);
        free (objs);
    }


    return OMPI_SUCCESS;
}

/* the next two routines implement a kind of non-blocking barrier.
the only difference is, that you can wait for the completion
of more than one initiated ibarrier. This is required for waiting
for all still connected processes in MPI_Finalize.

ompi_comm_disconnect_init returns a handle, which has to be passed in
to ompi_comm_disconnect_waitall. The second routine blocks, until
all non-blocking barriers described by the handles are finished.
The communicators can than be released.
*/

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

ompi_dpm_base_disconnect_obj *ompi_dpm_base_disconnect_init ( ompi_communicator_t *comm)
{
    ompi_dpm_base_disconnect_obj *obj=NULL;
    int ret;
    int i;

    obj = (ompi_dpm_base_disconnect_obj *) calloc(1,sizeof(ompi_dpm_base_disconnect_obj));
        if ( NULL == obj ) {
        return NULL;
    }

    if ( OMPI_COMM_IS_INTER(comm) ) {
        obj->size = ompi_comm_remote_size (comm);
    } else {
        obj->size = ompi_comm_size (comm);
    }

    obj->comm = comm;
    obj->reqs = (ompi_request_t **) malloc(2*obj->size*sizeof(ompi_request_t *));
    if ( NULL == obj->reqs ) {
        free (obj);
        return NULL;
    }

    /* initiate all isend_irecvs. We use a dummy buffer stored on
       the object, since we are sending zero size messages anyway. */
    for ( i=0; i < obj->size; i++ ) {
        ret = MCA_PML_CALL(irecv (&(obj->buf), 0, MPI_INT, i,
                     OMPI_COMM_BARRIER_TAG, comm,
                     &(obj->reqs[2*i])));

        if ( OMPI_SUCCESS != ret ) {
            free (obj->reqs);
            free (obj);
            return NULL;
        }

        ret = MCA_PML_CALL(isend (&(obj->buf), 0, MPI_INT, i,
                     OMPI_COMM_BARRIER_TAG,
                     MCA_PML_BASE_SEND_SYNCHRONOUS,
                     comm, &(obj->reqs[2*i+1])));

        if ( OMPI_SUCCESS != ret ) {
            free (obj->reqs);
            free (obj);
            return NULL;
        }
    }

    /* return handle */
    return obj;
}
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* - count how many requests are active
 * - generate a request array large enough to hold
     all active requests
 * - call waitall on the overall request array
 * - free the objects
 */
void ompi_dpm_base_disconnect_waitall (int count, ompi_dpm_base_disconnect_obj **objs)
{

    ompi_request_t **reqs=NULL;
    char *treq=NULL;
    int totalcount = 0;
    int i;
    int ret;

    for (i=0; i<count; i++) {
        if (NULL == objs[i]) {
            printf("Error in comm_disconnect_waitall\n");
            return;
        }

        totalcount += objs[i]->size;
    }

    reqs = (ompi_request_t **) malloc (2*totalcount*sizeof(ompi_request_t *));
    if ( NULL == reqs ) {
        printf("ompi_comm_disconnect_waitall: error allocating memory\n");
        return;
    }

    /* generate a single, large array of pending requests */
    treq = (char *)reqs;
    for (i=0; i<count; i++) {
        memcpy (treq, objs[i]->reqs, 2*objs[i]->size * sizeof(ompi_request_t *));
        treq += 2*objs[i]->size * sizeof(ompi_request_t *);
    }

    /* force all non-blocking all-to-alls to finish */
    ret = ompi_request_wait_all (2*totalcount, reqs, MPI_STATUSES_IGNORE);

    /* Finally, free everything */
    for (i=0; i< count; i++ ) {
        if (NULL != objs[i]->reqs ) {
            free (objs[i]->reqs );
            free (objs[i]);
        }
    }

    free (reqs);

    /* decrease the counter for dynamic communicators by 'count'.
       Attention, this approach now requires, that we are just using
       these routines for communicators which have been flagged dynamic */
    ompi_comm_num_dyncomm -=count;

    return;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
#define OMPI_DPM_BASE_MAXJOBIDS 64
void ompi_dpm_base_mark_dyncomm (ompi_communicator_t *comm)
{
    int i, j, numjobids=0;
    int size, rsize;
    int found;
    orte_jobid_t jobids[OMPI_DPM_BASE_MAXJOBIDS], thisjobid;
    ompi_group_t *grp=NULL;
    ompi_proc_t *proc = NULL;

    /* special case for MPI_COMM_NULL */
    if ( comm == MPI_COMM_NULL ) {
        return;
    }

    size  = ompi_comm_size (comm);
    rsize = ompi_comm_remote_size(comm);

    /* loop over all processes in local group and count number
       of different jobids.  */
    grp = comm->c_local_group;
    for (i=0; i< size; i++) {
        proc = ompi_group_peer_lookup(grp,i);
        thisjobid = proc->proc_name.jobid;
        found = 0;
        for ( j=0; j<numjobids; j++) {
            if (thisjobid == jobids[j]) {
                found = 1;
                break;
            }
        }
        if (!found ) {
            jobids[numjobids++] = thisjobid;
        }
    }

    /* if inter-comm, loop over all processes in remote_group
       and count number of different jobids */
    grp = comm->c_remote_group;
    for (i=0; i< rsize; i++) {
        proc = ompi_group_peer_lookup(grp,i);
        thisjobid = proc->proc_name.jobid;
        found = 0;
        for ( j=0; j<numjobids; j++) {
            if ( thisjobid == jobids[j]) {
                found = 1;
                break;
            }
        }
        if (!found ) {
            jobids[numjobids++] = thisjobid;
        }
    }

    /* if number of joibds larger than one, set the disconnect flag*/
    if ( numjobids > 1 ) {
        ompi_comm_num_dyncomm++;
        OMPI_COMM_SET_DYNAMIC(comm);
    }

    return;
}
