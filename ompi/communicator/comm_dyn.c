/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include <string.h>
#include <stdio.h>
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mpi.h"
#include "dps/dps.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "group/group.h"
#include "proc/proc.h"
#include "info/info.h"
#include "opal/threads/mutex.h"
#include "util/proc_info.h"
#include "util/bit_ops.h"
#include "util/argv.h"
#include "include/constants.h"
#include "mca/pml/pml.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/oob/oob_types.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rmgr/rmgr.h"

#include "mca/pml/pml.h"
#include "mca/rml/rml.h"

#include "runtime/runtime.h"
#include "util/printf.h"

int ompi_comm_connect_accept ( ompi_communicator_t *comm, int root,
                               orte_process_name_t *port, int send_first,
                               ompi_communicator_t **newcomm, orte_rml_tag_t tag )
{
    int size, rsize, rank, rc;
    size_t num_vals;
    size_t rnamebuflen;
    void *rnamebuf=NULL;

    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t **rprocs=NULL;
    ompi_group_t *group=comm->c_local_group;
    orte_process_name_t *rport=NULL;
    orte_buffer_t *nbuf=NULL, *nrbuf=NULL;

    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );

    if ( rank == root ) {
        /* The process receiving first does not have yet the contact 
           information of the remote process. Therefore, we have to
           exchange that.
        */
	if ( OMPI_COMM_JOIN_TAG != (int)tag ) {
	    rport = ompi_comm_get_rport (port,send_first,
					 group->grp_proc_pointers[rank], tag);
	}
	else {
	    rport = port;
	}
	
	/* Generate the message buffer containing the number of processes and the list of
	   participating processes */
	nbuf = OBJ_NEW(orte_buffer_t);
	if (NULL == nbuf) {
	    return OMPI_ERROR;
	}

        /* tell the progress engine to tick the event library more
           often, to make sure that the OOB messages get sent */
        opal_progress_event_increment();

	if (ORTE_SUCCESS != (rc = orte_dps.pack(nbuf, &size, 1, ORTE_INT))) {
	    goto exit;
	}
	ompi_proc_get_namebuf (group->grp_proc_pointers, size, nbuf);
	
	nrbuf = OBJ_NEW(orte_buffer_t);
	if (NULL == nrbuf ) {
	    rc = OMPI_ERROR;
	    goto exit;
	}
	
	/* Exchange the number and the list of processes in the groups */
	if ( send_first ) {
	    rc = orte_rml.send_buffer(rport, nbuf, tag, 0);
	    rc = orte_rml.recv_buffer(rport, nrbuf, tag);
	}
	else {
	    rc = orte_rml.recv_buffer(rport, nrbuf, tag);
	    rc = orte_rml.send_buffer(rport, nbuf, tag, 0);
	}
	
	if (ORTE_SUCCESS != (rc = orte_dps.unload(nrbuf, &rnamebuf, &rnamebuflen))) {
	    goto exit;
	}
    }
    
    /* bcast the buffer-length to all processes in the local comm */
    rc = comm->c_coll.coll_bcast (&rnamebuflen, 1, MPI_INT, root, comm );
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    if ( rank != root ) {
	/* non root processes need to allocate the buffer manually */
	rnamebuf = (char *) malloc(rnamebuflen);
	if ( NULL == rnamebuf ) {
	    rc = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
	}
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

    nrbuf = OBJ_NEW(orte_buffer_t);
    if (NULL == nrbuf) {
	goto exit;
    }
    if ( ORTE_SUCCESS != ( rc = orte_dps.load(nrbuf, rnamebuf, rnamebuflen))) {
	goto exit;
    }

    num_vals = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(nrbuf, &rsize, &num_vals, ORTE_INT))) {
	goto exit;
    }

    rc = ompi_proc_get_proclist (nrbuf, rsize, &rprocs);
    if ( OMPI_SUCCESS != rc ) {
	goto exit;
    }
    
    OBJ_RELEASE(nrbuf);
    if ( rank == root ) {
	OBJ_RELEASE(nbuf);
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
    /* done with OOB and such - slow our tick rate again */
    opal_progress();
    opal_progress_event_decrement();


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

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
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
orte_process_name_t *ompi_comm_get_rport (orte_process_name_t *port, int send_first, 
                                          ompi_proc_t *proc, orte_rml_tag_t tag)
{
    int rc;
    size_t num_vals;
    orte_process_name_t *rport, tbuf;
    ompi_proc_t *rproc=NULL;
    bool isnew = false;


    if ( send_first ) {
        orte_buffer_t *sbuf;

        rproc = ompi_proc_find_and_add(port, &isnew);
        sbuf = OBJ_NEW(orte_buffer_t);
        if (NULL == sbuf) {
            return NULL;
        }
        if (ORTE_SUCCESS != orte_dps.pack(sbuf, &(proc->proc_name), 1, ORTE_NAME)) {
            return NULL;
        }
        rc = orte_rml.send_buffer(port, sbuf, tag, 0);
        OBJ_RELEASE(sbuf);

        rport = port;
    }
    else {
        orte_buffer_t *rbuf;

        rbuf = OBJ_NEW(orte_buffer_t);
        if (NULL == rbuf) {
            return NULL;
        }
        rc = orte_rml.recv_buffer(ORTE_RML_NAME_ANY, rbuf, tag);
        num_vals = 1;
        if (ORTE_SUCCESS != orte_dps.unpack(rbuf, &tbuf, &num_vals, ORTE_NAME)) {
            return NULL;
        }
        OBJ_RELEASE(rbuf);
        rproc = ompi_proc_find_and_add(&tbuf, &isnew);
        rport = &(rproc->proc_name);

    }
    if (isnew) {
        MCA_PML_CALL(add_procs(&rproc, 1));
    }

    return rport;
}


/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int
ompi_comm_start_processes(int count, char **array_of_commands,
                          char ***array_of_argv, 
                          int *array_of_maxprocs, 
                          MPI_Info *array_of_info, 
                          char *port_name)
{
    int rc, i, j;
    int have_wdir=0;
    int valuelen=OMPI_PATH_MAX, flag=0;
    char cwd[OMPI_PATH_MAX];

    orte_jobid_t new_jobid;
    orte_app_context_t **apps=NULL;


    /* parse the info object */
    /* check potentially for: 
       - "host": desired host where to spawn the processes
       - "arch": desired architecture
       - "wdir": directory, where executable can be found
       - "path": list of directories where to look for the executable
       - "file": filename, where additional information is provided.
       - "soft": see page 92 of MPI-2.
    */

    /* make sure the progress engine properly trips the event library */
    opal_progress_event_increment();

    /* Convert the list of commands to an array of orte_app_context_t
       pointers */
    apps = (orte_app_context_t**)malloc(count * sizeof(orte_app_context_t *));
    if (NULL == apps) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i = 0; i < count; ++i) {
        apps[i] = OBJ_NEW(orte_app_context_t);
        if (NULL == apps[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            /* rollback what was already done */
            for (j=0; j < i; j++) OBJ_RELEASE(apps[j]);
            opal_progress_event_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* copy over the name of the executable */
        apps[i]->app = strdup(array_of_commands[i]);
        if (NULL == apps[i]->app) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            /* rollback what was already done */
            for (j=0; j < i; j++) OBJ_RELEASE(apps[j]);
            opal_progress_event_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* record the number of procs to be generated */
        apps[i]->num_procs = array_of_maxprocs[i];

        /* copy over the argv array */
	apps[i]->argc = 1;	

        if (MPI_ARGVS_NULL != array_of_argv &&
            MPI_ARGV_NULL != array_of_argv[i]) {
            /* first need to find out how many entries there are */
            j=0;
            while (NULL != array_of_argv[i][j]) {
                j++;
            }
            apps[i]->argc += j;
	}
	
	/* now copy them over, ensuring to NULL terminate the array */
	apps[i]->argv = (char**)malloc((1 + apps[i]->argc) * sizeof(char*));
	if (NULL == apps[i]->argv) {
	    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
	    /* rollback what was already done */
	    for (j=0; j < i; j++) {
		OBJ_RELEASE(apps[j]);
	    }
            opal_progress_event_decrement();
	    return ORTE_ERR_OUT_OF_RESOURCE;
	}
	apps[i]->argv[0] = strdup(array_of_commands[i]);
	for (j=1; j < apps[i]->argc; j++) {
	    apps[i]->argv[j] = strdup(array_of_argv[i][j-1]);
	}
	apps[i]->argv[apps[i]->argc] = NULL;

	    
        /* the environment gets set by the launcher
         * all we need to do is add the specific values
         * needed for comm_spawn
         */
        /* Add environment variable with the contact information for the 
           child processes. 
	*/
        apps[i]->num_env = 1;
        apps[i]->env = (char**)malloc((1+apps[i]->num_env) * sizeof(char*));
        if (NULL == apps[i]->env) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            /* rollback what was already done */
            for (j=0; j < i; j++) OBJ_RELEASE(apps[j]);
            opal_progress_event_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        asprintf(&(apps[i]->env[0]), "OMPI_PARENT_PORT=%s", port_name);
        apps[i]->env[1] = NULL;
        /* Check for the 'wdir' and later potentially for the
           'path' Info object */
        have_wdir = 0; 
        if ( array_of_info != NULL && array_of_info[i] != MPI_INFO_NULL ) {
            ompi_info_get (array_of_info[i], "wdir", valuelen, cwd, &flag);
            if ( flag ) {
                apps[i]->cwd = cwd;
                have_wdir = 1;
            }
        }
        
        /* default value: If the user did not tell us where to look for the 
           executable, we assume the current working directory */
        if ( !have_wdir ) {
            getcwd(cwd, OMPI_PATH_MAX);
            apps[i]->cwd = strdup(cwd);
        }
        /* leave the map info alone - the launcher will
         * decide where to put things
         */
    } /* for (i = 0 ; i < count ; ++i) */


    /* spawn procs */
    if (ORTE_SUCCESS != (rc = orte_rmgr.spawn(apps, count, &new_jobid,
                                    NULL))) {
    	ORTE_ERROR_LOG(rc);
        opal_progress_event_decrement();
        return MPI_ERR_SPAWN;
    }
    
    /* clean up */
    opal_progress_event_decrement();
    for ( i=0; i<count; i++) {
	OBJ_RELEASE(apps[i]);
    }
    free (apps);

    return OMPI_SUCCESS;
}
			       
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_dyn_init (void)
{
    char *envvarname=NULL, *port_name=NULL;
    char *oob_port=NULL;
    int root=0, send_first=1, rc;
    orte_rml_tag_t tag;
    ompi_communicator_t *newcomm=NULL;
    orte_process_name_t *port_proc_name=NULL;
    ompi_group_t *group = NULL;
    ompi_errhandler_t *errhandler = NULL;

    /* check for appropriate env variable */
    asprintf(&envvarname, "OMPI_PARENT_PORT");
    port_name = getenv(envvarname);
    free (envvarname);
    
    /* if env-variable is set, parse port and call comm_connect_accept */
    if (NULL != port_name ) {
	ompi_communicator_t *oldcomm;

	/* split the content of the environment variable into 
	   its pieces, which are : port_name and tag */
	oob_port = ompi_parse_port (port_name, &tag);
	if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_process_name(&port_proc_name, oob_port))) {
          return rc;
        }

	ompi_comm_connect_accept (MPI_COMM_WORLD, root, port_proc_name,  
				  send_first, &newcomm, tag );
	/* Set the parent communicator */
	ompi_mpi_comm_parent = newcomm;

	/* originally, we set comm_parent to comm_null (in comm_init),
	 * now we have to decrease the reference counters to the according
	 * objects 
	 */

	oldcomm = &ompi_mpi_comm_null;
	OBJ_RELEASE(oldcomm);
        group = &ompi_mpi_group_null;
	OBJ_RELEASE(group);
	errhandler = &ompi_mpi_errors_are_fatal;
	OBJ_RELEASE(errhandler);

	/* Set name for debugging purposes */
	snprintf(newcomm->c_name, MPI_MAX_OBJECT_NAME, "MPI_COMM_PARENT");
    }
    
    return OMPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
/* this routine runs through the list of communicators and
   and does the disconnect for all dynamic communicators */
int ompi_comm_dyn_finalize (void)
{
    int i,j=0, max=0;
    ompi_comm_disconnect_obj **objs=NULL;
    ompi_communicator_t *comm=NULL;

    if ( 1 <ompi_comm_num_dyncomm ) {
	objs = (ompi_comm_disconnect_obj **)malloc (ompi_comm_num_dyncomm*
						   sizeof(ompi_comm_disconnect_obj*));
	if ( NULL == objs ) {
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}

	max = ompi_pointer_array_get_size(&ompi_mpi_communicators);
	for ( i=3; i<max; i++ ) {
	    comm = (ompi_communicator_t*)ompi_pointer_array_get_item(&ompi_mpi_communicators,i);
	    if ( OMPI_COMM_IS_DYNAMIC(comm)) {
		objs[j++]=ompi_comm_disconnect_init(comm);
	    }
	}
    
	if ( j != ompi_comm_num_dyncomm+1 ) {
	    free (objs);
	    return OMPI_ERROR;
	}

	ompi_comm_disconnect_waitall (ompi_comm_num_dyncomm, objs);
	free (objs);
    }
    
    
    return OMPI_SUCCESS;
}

/**********************************************************************/
/**********************************************************************/
/**********************************************************************/

ompi_comm_disconnect_obj *ompi_comm_disconnect_init ( ompi_communicator_t *comm)
{
    ompi_comm_disconnect_obj *obj=NULL;
    int ret;
    int i;

    obj = (ompi_comm_disconnect_obj *) calloc(1,sizeof(ompi_comm_disconnect_obj));
    if ( NULL == obj ) {
	return NULL;
    }

    if ( OMPI_COMM_IS_INTER(comm) ) {
	obj->size = ompi_comm_remote_size (comm);
    }
    else {
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
				 MCA_PML_BASE_SEND_STANDARD,
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
void ompi_comm_disconnect_waitall (int count, ompi_comm_disconnect_obj **objs)
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
#define OMPI_COMM_MAXJOBIDS 64
void ompi_comm_mark_dyncomm (ompi_communicator_t *comm)
{
    int i, j, numjobids=0;
    int size, rsize;
    int found;
    orte_jobid_t jobids[OMPI_COMM_MAXJOBIDS], thisjobid;
    ompi_group_t *grp=NULL;

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
	if (ORTE_SUCCESS != orte_ns.get_jobid(&thisjobid, &(grp->grp_proc_pointers[i]->proc_name))) {
        return;
    }
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

    /* if inter-comm, loop over all processes in remote_group
       and count number of different jobids */
    grp = comm->c_remote_group;
    for (i=0; i< rsize; i++) {
    if (ORTE_SUCCESS != orte_ns.get_jobid(&thisjobid, &(grp->grp_proc_pointers[i]->proc_name))) {
        return;
    }
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
