/*
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
#include "proc/proc.h"
#include "threads/mutex.h"
#include "util/bit_ops.h"
#include "util/bufpack.h"
#include "util/argv.h"
#include "include/constants.h"
#include "mca/pcm/base/base.h"
#include "mca/pml/pml.h"
#include "mca/ns/base/base.h"

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


int ompi_comm_start_processes (char *command, char **argv, int maxprocs, 
			       MPI_Info info,  char *port_name )
{
    mca_ns_base_jobid_t new_jobid;
    ompi_rte_node_schedule_t *sched;
    ompi_rte_spawn_handle_t *spawn_handle;
    ompi_list_t *nodelist=NULL;
    ompi_list_t schedlist;
    char *tmp, *envvarname, *segment, *my_contact_info;
    char cwd[MAXPATHLEN];
    int rc;
    
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
    spawn_handle =
        ompi_rte_get_spawn_handle(OMPI_RTE_SPAWN_FROM_MPI|OMPI_RTE_SPAWN_HIGH_QOS, true);
    if (NULL == spawn_handle) {
        printf("show_help: get_spawn_handle failed\n");
        return -1;
    }

    /* BWB - fix jobid, procs, and nodes */
    nodelist = ompi_rte_allocate_resources(spawn_handle, new_jobid, 0, maxprocs);
    if (NULL == nodelist) {
	/* BWB show_help */
	printf("show_help: ompi_rte_allocate_resources failed\n");
	return -1;
    }
    
    /*
     * Process mapping
     */
    OBJ_CONSTRUCT(&schedlist,  ompi_list_t);
    sched = OBJ_NEW(ompi_rte_node_schedule_t);
    ompi_list_append(&schedlist, (ompi_list_item_t*) sched);
    /*  ompi_cmd_line_get_tail(cmd_line, &(sched->argc), &(sched->argv)); */
    ompi_argv_append (&(sched->argc), &(sched->argv), command);
    
    if (argv != MPI_ARGV_NULL ) {
	int i=0;
	char *arg=argv[i];
	
	while ( arg!=NULL ) {
	    ompi_argv_append(&(sched->argc), &(sched->argv), arg);
	    arg = argv[++i];
	} 
    }

    /*
     * build environment to be passed
     */
    mca_pcm_base_build_base_env(environ, &(sched->envc), &(sched->env));

    /* set initial contact info */
    my_contact_info = strdup(ompi_universe_info.ns_replica);

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
       child processes */
    asprintf(&envvarname, "OMPI_PARENT_PORT_%u", new_jobid);
    asprintf(&tmp, "%s=%s", envvarname, port_name);
    ompi_argv_append(&(sched->envc), &(sched->env), tmp);
    free(tmp);
    free(envvarname);
    
    getcwd(cwd, MAXPATHLEN);
    sched->cwd = strdup(cwd);
    sched->nodelist = nodelist;

    if (sched->argc == 0) {
	printf("no app to start\n");
	return MPI_ERR_ARG;
    }


    /*
     * register to monitor the startup and shutdown processes
     */
    /* setup segment for this job */
    asprintf(&segment, "ompi-job-%d", new_jobid);

    /* register a synchro on the segment so we get notified when everyone registers */
    rc = ompi_registry.synchro(
	 OMPI_REGISTRY_SYNCHRO_MODE_LEVEL|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT,
	 OMPI_REGISTRY_OR,
	 segment,
	 NULL,
	 maxprocs,
	 ompi_rte_all_procs_registered, NULL);


    /*
     * spawn procs
     */
    if (OMPI_SUCCESS != ompi_rte_spawn_procs(spawn_handle, new_jobid, &schedlist)) {
	printf("show_help: woops!  we didn't spawn :( \n");
	return MPI_ERR_SPAWN;
    }
    
    if (OMPI_SUCCESS != ompi_rte_monitor_procs_registered()) {
	printf("procs didn't all register - returning an error\n");
	return MPI_ERR_SPAWN;
    }  

   
    /*
     * Clean up
     */
    if (NULL != nodelist) ompi_rte_deallocate_resources(spawn_handle, 
							new_jobid, nodelist);
    if (NULL != spawn_handle) OBJ_RELEASE(spawn_handle); 
    OBJ_DESTRUCT(&schedlist);

    return OMPI_SUCCESS;
}
			       

int ompi_comm_dyn_init (void)
{
    uint32_t jobid;
    size_t size;
    ompi_proc_t **myproc=NULL;
    char *envvarname=NULL, *port_name=NULL;
    char *oob_port=NULL;
    int tag, root=0, send_first=1;
    ompi_communicator_t *newcomm=NULL;
    ompi_process_name_t *port_proc_name=NULL;

    /* get jobid */
    myproc = ompi_proc_self(&size);
    jobid = ompi_name_server.get_jobid(&(myproc[0]->proc_name));

    /* check for appropriate env variable */
    asprintf(&envvarname, "OMPI_PARENT_PORT_%u", jobid);
    port_name = getenv(envvarname);
    free (envvarname);
    
    /* if env-variable is set, parse port and call comm_connect_accept */
    if (NULL != port_name ) {
	/* we have been spawned */
	oob_port = ompi_parse_port (port_name, &tag);
	port_proc_name = ompi_name_server.convert_string_to_process_name(oob_port);
	ompi_comm_connect_accept (MPI_COMM_WORLD, root, port_proc_name,  
				  send_first, &newcomm, tag );
	/* Set the parent communicator */
	ompi_mpi_comm_parent = newcomm;

	/* originally, we set comm_parent to comm_null (in comm_init),
	 * now we have to decrease the reference counters to the according
	 * objects 
	 */
/*	OBJ_RELEASE(&ompi_mpi_comm_null);
	OBJ_RELEASE(&ompi_mpi_group_null);
	OBJ_RELEASE(&ompi_mpi_group_null);
	OBJ_RELEASE(&ompi_mpi_errors_are_fatal);
*/
    }
    
    return OMPI_SUCCESS;
}
