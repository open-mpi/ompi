/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "mpi.h"

#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "errhandler/errhandler.h"
#include "group/group.h"
#include "proc/proc.h"
#include "info/info.h"
#include "threads/mutex.h"
#include "util/proc_info.h"
#include "util/bit_ops.h"
#include "util/bufpack.h"
#include "util/argv.h"
#include "include/constants.h"
#include "mca/pcm/base/base.h"
#include "mca/pml/pml.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"

#include "mca/pml/pml.h"
#include "mca/oob/base/base.h"

#include "runtime/runtime.h"
#include "util/printf.h"
extern char **environ;

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
ompi_process_name_t *ompi_comm_get_rport (ompi_process_name_t *port, int send_first, 
                                          ompi_proc_t *proc, int tag)
{
    int rc;
    ompi_process_name_t *rport, tbuf;
    ompi_proc_t *rproc=NULL;
    bool isnew = false;

    if ( send_first ) {
        ompi_buffer_t sbuf;

        rproc = ompi_proc_find_and_add(port, &isnew);
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
        rproc = ompi_proc_find_and_add(&tbuf, &isnew);
        rport = &(rproc->proc_name);

    }
    if (isnew) {
        mca_pml.pml_add_procs(&rproc, 1);
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
    mca_ns_base_jobid_t new_jobid;
    ompi_rte_node_schedule_t *sched;
    ompi_rte_spawn_handle_t *spawn_handle;
    ompi_list_t **nodelists = NULL;
    ompi_list_t schedlist;
    char *tmp, *envvarname, *segment, *my_contact_info;
    char cwd[MAXPATHLEN];
    int have_wdir=0;
    ompi_registry_notify_id_t rc_tag;
    int i, valuelen=MAXPATHLEN, flag=0;
    int total_start_procs = 0;
    int requires;

    /* parse the info object */
    /* check potentially for: 
       - "host": desired host where to spawn the processes
       - "arch": desired architecture
       - "wdir": directory, where executable can be found
       - "path": list of directories where to look for the executable
       - "file": filename, where additional information is provided.
       - "soft": see page 92 of MPI-2.
    */

    /* get the jobid for the new processes */
    new_jobid = ompi_name_server.create_jobid();

    /* get the spawn handle to start spawning stuff */
    requires = OMPI_RTE_SPAWN_FROM_MPI | OMPI_RTE_SPAWN_HIGH_QOS;
    if (count > 1) requires |= OMPI_RTE_SPAWN_MPMD;
    spawn_handle = ompi_rte_get_spawn_handle(requires, true);
    if (NULL == spawn_handle) {
        printf("show_help: get_spawn_handle failed\n");
        return OMPI_ERROR;
    }

    /* get our allocations and set them up, one by one */
    OBJ_CONSTRUCT(&schedlist,  ompi_list_t);
    nodelists = malloc(sizeof(ompi_list_t*) * count);
    if (NULL == nodelists) {
        return OMPI_ERROR;
    }

    /* iterate through all the counts, creating an app schema entry
       for each one */
    for (i = 0 ; i < count ; ++i) {
        nodelists[i] = ompi_rte_allocate_resources(spawn_handle, 
                                                   new_jobid, 0, 
                                                   array_of_maxprocs[i]);
        if (NULL == nodelists[i]) {
            /* BWB - XXX - help - need to unwind what already done */
            return OMPI_ERROR;
        }
        total_start_procs += array_of_maxprocs[i];

        
        /*
         * Process mapping
         */
        sched = OBJ_NEW(ompi_rte_node_schedule_t);
        ompi_argv_append (&(sched->argc), &(sched->argv), 
                          array_of_commands[i]);
    
        if (array_of_argv != MPI_ARGVS_NULL && 
            array_of_argv[i] != MPI_ARGV_NULL ) {
            int j = 0;
            char *arg = array_of_argv[i][j];
	
            while (arg != NULL) {
                ompi_argv_append(&(sched->argc), &(sched->argv), arg);
                arg = array_of_argv[i][++j];
            } 
        }

        /*
         * build environment to be passed
         */
        mca_pcm_base_build_base_env(environ, &(sched->envc), &(sched->env));

        /* set initial contact info */
        if (ompi_process_info.seed) {
            my_contact_info = mca_oob_get_contact_info();
        } else {
            my_contact_info = strdup(ompi_universe_info.ns_replica);
        }

        asprintf(&tmp, "OMPI_MCA_ns_base_replica=%s", my_contact_info);
        ompi_argv_append(&(sched->envc), &(sched->env), tmp);
        free(tmp);

        asprintf(&tmp, "OMPI_MCA_gpr_base_replica=%s", my_contact_info);
        ompi_argv_append(&(sched->envc), &(sched->env), tmp);
        free(tmp);

        if (NULL != ompi_universe_info.name) {
            asprintf(&tmp, "OMPI_universe_name=%s", ompi_universe_info.name);
            ompi_argv_append(&(sched->envc), &(sched->env), tmp);
            free(tmp);
        }

        /* Add environment variable with the contact information for the 
           child processes. 
	   12.23.2004 EG: the content of the environment variable
	   does know hold additionally to the oob contact information
	   also the information, which application in spawn/spawn_multiple
	   it has been. This information is needed to construct the 
	   attribute MPI_APPNUM on the children side (an optional 
	   MPI-2 attribute. */
        asprintf(&envvarname, "OMPI_PARENT_PORT_%u", new_jobid);
        asprintf(&tmp, "%s=%s:%d", envvarname, port_name, i);
        ompi_argv_append(&(sched->envc), &(sched->env), tmp);
        free(tmp);
        free(envvarname);

	/* Check for the 'wdir' and later potentially for the
	   'path' Info object */
	have_wdir = 0; 
	if ( array_of_info != NULL && array_of_info[i] != MPI_INFO_NULL ) {
	    ompi_info_get (array_of_info[i], "wdir", valuelen, cwd, &flag);
	    if ( flag ) {
		sched->cwd = cwd;
		have_wdir = 1;
	    }
	}
	
	/* default value: If the user did not tell us where to look for the 
	   executable, we assume the current working directory */
	if ( !have_wdir ) {
	    getcwd(cwd, MAXPATHLEN);
	    sched->cwd = strdup(cwd);
	}
	sched->nodelist = nodelists[i];
	    
        if (sched->argc == 0) {
            printf("no app to start\n");
            return MPI_ERR_ARG;
        }
        ompi_list_append(&schedlist, (ompi_list_item_t*) sched);
    } /* for (i = 0 ; i < count ; ++i) */


    /*
     * register to monitor the startup
     */
    /* setup segment for this job */
    asprintf(&segment, "%s-%s", OMPI_RTE_JOB_STATUS_SEGMENT,
	     ompi_name_server.convert_jobid_to_string(new_jobid));

    /* register a synchro on the segment so we get notified when everyone registers */
    rc_tag = ompi_registry.synchro(
	     OMPI_REGISTRY_SYNCHRO_MODE_LEVEL|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT|
	     OMPI_REGISTRY_SYNCHRO_MODE_STARTUP,
	     OMPI_REGISTRY_OR,
	     segment,
	     NULL,
             total_start_procs,
	     ompi_rte_all_procs_registered, NULL);
         
	free(segment);
    /*
     * spawn procs
     */
    if (OMPI_SUCCESS != ompi_rte_spawn_procs(spawn_handle, new_jobid, &schedlist)) {
	printf("show_help: woops!  we didn't spawn :( \n");
	return MPI_ERR_SPAWN;
    }
    
    if (OMPI_SUCCESS != ompi_rte_monitor_procs_registered()) {
		ompi_output(0, "[%d,%d,%d] procs didn't all register - returning an error",
					OMPI_NAME_ARGS(*ompi_rte_get_self()));
		return MPI_ERR_SPAWN;
    }  

    /*
     * tell processes okay to start by sending startup msg
     */
    ompi_rte_job_startup(new_jobid);

    /*
     * Clean up
     */
    if (NULL != nodelists) {
        for (i = 0 ; i < count ; ++i) {
            if (NULL != nodelists[i]) {
                ompi_rte_deallocate_resources(spawn_handle, 
                                              new_jobid, nodelists[i]);
            }
        }
        free(nodelists);
    }
    if (NULL != spawn_handle) OBJ_RELEASE(spawn_handle); 
    OBJ_DESTRUCT(&schedlist);

    return OMPI_SUCCESS;
}
			       
/**********************************************************************/
/**********************************************************************/
/**********************************************************************/
int ompi_comm_dyn_init (void)
{
    uint32_t jobid;
    char *envvarname=NULL, *port_name=NULL;
    char *oob_port=NULL;
    int tag, root=0, send_first=1;
    ompi_communicator_t *newcomm=NULL;
    ompi_process_name_t *port_proc_name=NULL;
    ompi_group_t *group = NULL;
    ompi_errhandler_t *errhandler = NULL;
    char remainder[128];
    int appnum=0;

    /* get jobid */
    /* JMS: Previous was using ompi_proc_self() here, which
       incremented the refcount.  That would be fine, but we would
       have to OBJ_RELEASE it as well.  The global
       ompi_proc_local_proc seemed to have been created for exactly
       this kind of purpose, so I took the liberty of using it. */
    jobid = ompi_name_server.get_jobid(&(ompi_proc_local_proc->proc_name));


    /* check for appropriate env variable */
    asprintf(&envvarname, "OMPI_PARENT_PORT_%u", jobid);
    port_name = getenv(envvarname);
    free (envvarname);
    
    /* if env-variable is set, parse port and call comm_connect_accept */
    if (NULL != port_name ) {
	ompi_communicator_t *oldcomm;

	/* split the content of the environment variable into 
	   its pieces, which are : port_name, tag, and mpi_appnum. */
	oob_port = ompi_parse_port (port_name, &tag, &(remainder[0]));
	sscanf (remainder, "%d", &appnum);
	
	port_proc_name = ompi_name_server.convert_string_to_process_name(oob_port);
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
	ret = mca_pml.pml_irecv (&(obj->buf), 0, MPI_INT, i,
				 OMPI_COMM_BARRIER_TAG, comm, 
				 &(obj->reqs[2*i]));
				 
	if ( OMPI_SUCCESS != ret ) {
	    free (obj->reqs);
	    free (obj);
	    return NULL;
	}

	ret = mca_pml.pml_isend (&(obj->buf), 0, MPI_INT, i,
				 OMPI_COMM_BARRIER_TAG, 
				 MCA_PML_BASE_SEND_STANDARD,
				 comm, &(obj->reqs[2*i+1]));
				 
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
    uint32_t jobids[OMPI_COMM_MAXJOBIDS], thisjobid;
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
	thisjobid = ompi_name_server.get_jobid(&(grp->grp_proc_pointers[i]->proc_name));
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
	thisjobid = ompi_name_server.get_jobid(&(grp->grp_proc_pointers[i]->proc_name));
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
