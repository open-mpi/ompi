/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_getcwd.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_data_server.h"

#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/info/info.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "ompi/mca/dpm/base/base.h"
#include "dpm_orte.h"

/* Local static variables */
static opal_mutex_t ompi_dpm_port_mutex;
static orte_rml_tag_t next_tag;


/*
 * Init the module
 */
static int init(void)
{    
    OBJ_CONSTRUCT(&ompi_dpm_port_mutex, opal_mutex_t);
    next_tag = OMPI_RML_TAG_DYNAMIC;

    return OMPI_SUCCESS;
}

static int get_rport (orte_process_name_t *port,
                      int send_first, struct ompi_proc_t *proc,
                      orte_rml_tag_t tag, orte_process_name_t *rport);


static int connect_accept ( ompi_communicator_t *comm, int root,
                            orte_process_name_t *port, bool send_first,
                            ompi_communicator_t **newcomm, orte_rml_tag_t tag )
{
    int size, rsize, rank, rc;
    orte_std_cntr_t num_vals;
    orte_std_cntr_t rnamebuflen = 0;
    int rnamebuflen_int = 0;
    void *rnamebuf=NULL;

    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t **rprocs=NULL;
    ompi_group_t *group=comm->c_local_group;
    orte_process_name_t *rport=NULL, tmp_port_name;
    opal_buffer_t *nbuf=NULL, *nrbuf=NULL;
    ompi_proc_t **proc_list=NULL, **new_proc_list;
    int i,j, new_proc_len;
    ompi_group_t *new_group_pointer;

    
    OPAL_OUTPUT_VERBOSE((1, ompi_dpm_base_output,
                         "%s dpm:orte:connect_accept with port %s %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(port),
                         send_first ? "sending first" : "recv first"));
    
    size = ompi_comm_size ( comm );
    rank = ompi_comm_rank ( comm );

    /* tell the progress engine to tick the event library more
       often, to make sure that the OOB messages get sent */
    opal_progress_event_users_increment();

    if ( rank == root ) {
        /* The process receiving first does not have yet the contact
           information of the remote process. Therefore, we have to
           exchange that.
        */

        if(!OMPI_GROUP_IS_DENSE(group)) { 
            proc_list = (ompi_proc_t **) calloc (group->grp_proc_count, 
                                                 sizeof (ompi_proc_t *));
            for(i=0 ; i<group->grp_proc_count ; i++)
                proc_list[i] = ompi_group_peer_lookup(group,i);
            
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_output,
                                 "%s dpm:orte:connect_accept adding %s to proc list",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc_list[i]->proc_name)));
        }

        if ( OMPI_COMM_JOIN_TAG != tag ) {
            if(OMPI_GROUP_IS_DENSE(group)){
                rc = get_rport(port,send_first,
                               group->grp_proc_pointers[rank], tag,
                               &tmp_port_name);
            }
            else {
                rc = get_rport(port,send_first,
                               proc_list[rank], tag,
                               &tmp_port_name);
            }
            if (OMPI_SUCCESS != rc) {
                return rc;
            }
            rport = &tmp_port_name;
        } else {
            rport = port;
        }

        /* Generate the message buffer containing the number of processes and the list of
           participating processes */
        nbuf = OBJ_NEW(opal_buffer_t);
        if (NULL == nbuf) {
            return OMPI_ERROR;
        }

        if (ORTE_SUCCESS != (rc = opal_dss.pack(nbuf, &size, 1, OPAL_INT))) {
            ORTE_ERROR_LOG(rc);
            goto exit;
        }

        if(OMPI_GROUP_IS_DENSE(group)) {
            ompi_proc_pack(group->grp_proc_pointers, size, nbuf);
        }
        else {
            ompi_proc_pack(proc_list, size, nbuf);
        }
        
        nrbuf = OBJ_NEW(opal_buffer_t);
        if (NULL == nrbuf ) {
            rc = OMPI_ERROR;
            goto exit;
        }

        /* Exchange the number and the list of processes in the groups */
        if ( send_first ) {
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_output,
                                 "%s dpm:orte:connect_accept sending first to %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(rport)));
            rc = orte_rml.send_buffer(rport, nbuf, tag, 0);
            rc = orte_rml.recv_buffer(rport, nrbuf, tag, 0);
        } else {
            OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_output,
                                 "%s dpm:orte:connect_accept recving first from %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(rport)));
            rc = orte_rml.recv_buffer(rport, nrbuf, tag, 0);
            rc = orte_rml.send_buffer(rport, nbuf, tag, 0);
        }

        if (ORTE_SUCCESS != (rc = opal_dss.unload(nrbuf, &rnamebuf, &rnamebuflen))) {
            ORTE_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* First convert the size_t to an int so we can cast in the bcast to a void *
     * if we don't then we will get badness when using big vs little endian
     * THIS IS NO LONGER REQUIRED AS THE LENGTH IS NOW A STD_CNTR_T, WHICH
     * CORRELATES TO AN INT32
     */
    rnamebuflen_int = (int)rnamebuflen;

    /* bcast the buffer-length to all processes in the local comm */
    rc = comm->c_coll.coll_bcast (&rnamebuflen_int, 1, MPI_INT, root, comm,
                                  comm->c_coll.coll_bcast_module);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }
    rnamebuflen = rnamebuflen_int;

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
    rc = comm->c_coll.coll_bcast (rnamebuf, rnamebuflen_int, MPI_BYTE, root, comm,
                                  comm->c_coll.coll_bcast_module);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    nrbuf = OBJ_NEW(opal_buffer_t);
    if (NULL == nrbuf) {
        goto exit;
    }
    if ( ORTE_SUCCESS != ( rc = opal_dss.load(nrbuf, rnamebuf, rnamebuflen))) {
        ORTE_ERROR_LOG(rc);
        goto exit;
    }

    num_vals = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(nrbuf, &rsize, &num_vals, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        goto exit;
    }

    rc = ompi_proc_unpack(nrbuf, rsize, &rprocs, &new_proc_len, &new_proc_list);
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* If we added new procs, we need to do the modex and then call
       PML add_procs */
    if (new_proc_len > 0) {
        opal_list_t all_procs;
        orte_namelist_t *name;

        OBJ_CONSTRUCT(&all_procs, opal_list_t);

        if (send_first) {
            for (i = 0 ; i < rsize ; ++i) {
                name = OBJ_NEW(orte_namelist_t);
                name->name = rprocs[i]->proc_name;
                opal_list_append(&all_procs, &name->item);
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_output,
                                     "%s dpm:orte:connect_accept send first adding %s to allgather list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name->name)));
            }
            for (i = 0 ; i < group->grp_proc_count ; ++i) {
                name = OBJ_NEW(orte_namelist_t);
                name->name = ompi_group_peer_lookup(group, i)->proc_name;
                opal_list_append(&all_procs, &name->item);
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_output,
                                     "%s dpm:orte:connect_accept send first adding %s to allgather list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name->name)));
            }

        } else {
            for (i = 0 ; i < group->grp_proc_count ; ++i) {
                name = OBJ_NEW(orte_namelist_t);
                name->name = ompi_group_peer_lookup(group, i)->proc_name;
                opal_list_append(&all_procs, &name->item);
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_output,
                                     "%s dpm:orte:connect_accept recv first adding %s to allgather list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name->name)));
            }
            for (i = 0 ; i < rsize ; ++i) {
                name = OBJ_NEW(orte_namelist_t);
                name->name = rprocs[i]->proc_name;
                opal_list_append(&all_procs, &name->item);
                OPAL_OUTPUT_VERBOSE((3, ompi_dpm_base_output,
                                     "%s dpm:orte:connect_accept recv first adding %s to allgather list",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name->name)));
            }

        }

        if (OMPI_SUCCESS != (rc = orte_grpcomm.modex(&all_procs))) {
            ORTE_ERROR_LOG(rc);
            goto exit;
        }

        /*
        while (NULL != (item = opal_list_remove_first(&all_procs))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&all_procs);
        */

        MCA_PML_CALL(add_procs(new_proc_list, new_proc_len));
    }

    OBJ_RELEASE(nrbuf);
    if ( rank == root ) {
        OBJ_RELEASE(nbuf);
    }

    new_group_pointer=ompi_group_allocate(rsize);
    if( NULL == new_group_pointer ) {
      return MPI_ERR_GROUP;
    }

    /* put group elements in the list */
    for (j = 0; j < rsize; j++) {
        new_group_pointer->grp_proc_pointers[j] = rprocs[j]; 
    }                           /* end proc loop */

    /* increment proc reference counters */
    ompi_group_increment_proc_count(new_group_pointer);
    
    /* set up communicator structure */
    rc = ompi_comm_set ( &newcomp,                 /* new comm */
                         comm,                     /* old comm */
                         group->grp_proc_count,    /* local_size */
                         NULL,                     /* local_procs */
                         rsize,                    /* remote_size */
                         NULL  ,                   /* remote_procs */
                         NULL,                     /* attrs */
                         comm->error_handler,      /* error handler */
                         NULL,                     /* topo component */
                         group,                    /* local group */
                         new_group_pointer         /* remote group */
                         );
    if ( NULL == newcomp ) {
        rc = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    ompi_group_decrement_proc_count (new_group_pointer);
    OBJ_RELEASE(new_group_pointer);
    new_group_pointer = MPI_GROUP_NULL;

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

    /* activate comm and init coll-component */
    rc = ompi_comm_activate ( newcomp,                 /* new communicator */
                              comm,                    /* old communicator */
                              NULL,                    /* bridge comm */
                              &root,                   /* local leader */
                              rport,                   /* remote leader */
                              OMPI_COMM_CID_INTRA_OOB, /* mode */
                              send_first,              /* send or recv first */
                              0);                      /* sync_flag */
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
    opal_progress_event_users_decrement();

    if ( NULL != rprocs ) {
        free ( rprocs );
    }
    if ( NULL != proc_list ) {
        free ( proc_list );
    }
    if ( OMPI_SUCCESS != rc ) {
        if ( MPI_COMM_NULL != newcomp && NULL != newcomp ) {
            OBJ_RETAIN(newcomp);
            newcomp = MPI_COMM_NULL;
        }
    }

    *newcomm = newcomp;
    return rc;
}

static void disconnect(ompi_communicator_t *comm)
{
    ompi_dpm_base_disconnect_obj *dobj;
    
    dobj = ompi_dpm_base_disconnect_init (comm);
    ompi_dpm_base_disconnect_waitall(1, &dobj);
    
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
 * Therefore, the two root processes exchange this information at this
 * point.
 *
 */
int get_rport(orte_process_name_t *port, int send_first,
              ompi_proc_t *proc, orte_rml_tag_t tag, 
              orte_process_name_t *rport_name)
{
    int rc;
    orte_std_cntr_t num_vals;

    if ( send_first ) {
        opal_buffer_t *sbuf;
        
        OPAL_OUTPUT_VERBOSE((1, ompi_dpm_base_output,
                             "%s dpm:orte:get_rport sending to %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(port)));
                            
        sbuf = OBJ_NEW(opal_buffer_t);
        if (NULL == sbuf) {
            return OMPI_ERROR;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(sbuf, &(proc->proc_name), 1, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(sbuf);
            return rc;
        }

        rc = orte_rml.send_buffer(port, sbuf, tag, 0);
        OBJ_RELEASE(sbuf);
        if ( 0 > rc ) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        *rport_name = *port;
    } else {
        opal_buffer_t *rbuf;

        OPAL_OUTPUT_VERBOSE((1, ompi_dpm_base_output,
                             "%s dpm:orte:get_rport waiting to recv",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
        rbuf = OBJ_NEW(opal_buffer_t);
        if (NULL == rbuf) {
            return ORTE_ERROR;
        }
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, rbuf, tag, 0))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(rbuf);
            return rc;
        }

        num_vals = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(rbuf, rport_name, &num_vals, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(rbuf);
            return rc;
        }
        OBJ_RELEASE(rbuf);
        
        OPAL_OUTPUT_VERBOSE((1, ompi_dpm_base_output,
                             "%s dpm:orte:get_rport recv'd name %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(rport_name)));
    }

    return OMPI_SUCCESS;
}


static int spawn(int count, char **array_of_commands,
                 char ***array_of_argv,
                 int *array_of_maxprocs,
                 MPI_Info *array_of_info,
                 char *port_name)
{
    int rc, i, j, counter;
    int have_wdir=0;
    bool have_prefix;
    int valuelen=OMPI_PATH_MAX, flag=0;
    char cwd[OMPI_PATH_MAX];
    char host[OMPI_PATH_MAX];  /*** should define OMPI_HOST_MAX ***/
    char prefix[OMPI_PATH_MAX];
    char *base_prefix=NULL;

    orte_job_t *jdata;
    orte_app_context_t *app;
    bool local_spawn, non_mpi;
    
    /* parse the info object */
    /* check potentially for:
       - "host": desired host where to spawn the processes
       - "hostfile": hostfile containing hosts where procs are
                     to be spawned
       - "add-host": add the specified hosts to the known list
                     of available resources and spawn these
                     procs on them
       - "add-hostfile": add the hosts in the hostfile to the
                     known list of available resources and spawn
                     these procs on them
       - "prefix": the path to the root of the directory tree where ompi
                   executables and libraries can be found on all nodes
                   used to spawn these procs
       - "arch": desired architecture
       - "wdir": directory, where executable can be found
       - "path": list of directories where to look for the executable
       - "file": filename, where additional information is provided.
       - "soft": see page 92 of MPI-2.
    */

    /* make sure the progress engine properly trips the event library */
    opal_progress_event_users_increment();
    
    /* setup the job object */
    jdata = OBJ_NEW(orte_job_t);
    
    /* Convert the list of commands to an array of orte_app_context_t
       pointers */
    for (i = 0; i < count; ++i) {
        app = OBJ_NEW(orte_app_context_t);
        if (NULL == app) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(jdata);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* add the app to the job data */
        opal_pointer_array_add(jdata->apps, app);
        app->idx = i;
        jdata->num_apps++;
        
        /* copy over the name of the executable */
        app->app = strdup(array_of_commands[i]);
        if (NULL == app->app) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(jdata);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* record the number of procs to be generated */
        app->num_procs = array_of_maxprocs[i];

        /* copy over the argv array */
        counter = 1;

        if (MPI_ARGVS_NULL != array_of_argv &&
            MPI_ARGV_NULL != array_of_argv[i]) {
            /* first need to find out how many entries there are */
            j=0;
            while (NULL != array_of_argv[i][j]) {
                j++;
            }
            counter += j;
        }

        /* now copy them over, ensuring to NULL terminate the array */
        app->argv = (char**)malloc((1 + counter) * sizeof(char*));
        if (NULL == app->argv) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(jdata);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        app->argv[0] = strdup(array_of_commands[i]);
        for (j=1; j < counter; j++) {
            app->argv[j] = strdup(array_of_argv[i][j-1]);
        }
        app->argv[counter] = NULL;


        /* the environment gets set by the launcher
         * all we need to do is add the specific values
         * needed for comm_spawn
         */
        /* Add environment variable with the contact information for the
           child processes.
        */
        counter = 1;
        app->env = (char**)malloc((1+counter) * sizeof(char*));
        if (NULL == app->env) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(jdata);
            opal_progress_event_users_decrement();
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        asprintf(&(app->env[0]), "OMPI_PARENT_PORT=%s", port_name);
        app->env[1] = NULL;
        for (j = 0; NULL != environ[j]; ++j) {
            if (0 == strncmp("OMPI_", environ[j], 5)) {
                opal_argv_append_nosize(&app->env, environ[j]);
            }
        }

        /* Check for well-known info keys */
        have_wdir = 0;
        have_prefix = false;
        if ( array_of_info != NULL && array_of_info[i] != MPI_INFO_NULL ) {

            /* check for 'host' */
            ompi_info_get (array_of_info[i], "host", sizeof(host), host, &flag);
            if ( flag ) {
                opal_argv_append_nosize(&app->dash_host, host);
            }
 
            /* check for 'hostfile' */
            ompi_info_get (array_of_info[i], "hostfile", sizeof(host), host, &flag);
            if ( flag ) {
                app->hostfile = strdup(host);
            }
            
            /* check for 'add-hostfile' */
            ompi_info_get (array_of_info[i], "add-hostfile", sizeof(host), host, &flag);
            if ( flag ) {
                app->add_hostfile = strdup(host);
            }
            
            /* 'path', 'arch', 'file', 'soft', 'add-host'  -- to be implemented */ 
            
            /* check for 'ompi_prefix' (OMPI-specific -- to effect the same
             * behavior as --prefix option to orterun)
             */
            ompi_info_get (array_of_info[i], "ompi_prefix", sizeof(prefix), prefix, &flag);
            if ( flag ) {
                app->prefix_dir = strdup(prefix);
                have_prefix = true;
            }

            /* check for 'wdir' */ 
            ompi_info_get (array_of_info[i], "wdir", valuelen, cwd, &flag);
            if ( flag ) {
                app->cwd = strdup(cwd);
                have_wdir = 1;
            }
            
            /* check for 'ompi_local_slave' - OMPI-specific -- indicates that
             * the specified app is to be launched by the local orted as a
             * "slave" process, typically to support an attached co-processor
             */
            ompi_info_get_bool(array_of_info[i], "ompi_local_slave", &local_spawn, &flag);
            if ( local_spawn ) {
                jdata->controls |= ORTE_JOB_CONTROL_LOCAL_SPAWN;
            }
         
            /* see if this is a non-mpi job - if so, then set the flag so ORTE
             * knows what to do
             */
            ompi_info_get_bool(array_of_info[i], "ompi_non_mpi", &non_mpi, &flag);
            if (non_mpi) {
                jdata->controls |= ORTE_JOB_CONTROL_NON_ORTE_JOB;
            }
        }

        /* default value: If the user did not tell us where to look for the
           executable, we assume the current working directory  */
        if ( !have_wdir ) {
            if (OMPI_SUCCESS != (rc = opal_getcwd(cwd, OMPI_PATH_MAX))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(jdata);
                opal_progress_event_users_decrement();
                return rc;
            }
            app->cwd = strdup(cwd);
        }
        
        /* if the user told us a new prefix, then we leave it alone. otherwise, if
         * a prefix had been provided before, copy that one into the new app_context
         * for use by the spawned children
         */
        if ( !have_prefix && NULL != base_prefix) {
            app->prefix_dir = strdup(base_prefix);
        }
        
        /* leave the map info alone - the launcher will
         * decide where to put things
         */
    } /* for (i = 0 ; i < count ; ++i) */

    /* cleanup */
    if (NULL != base_prefix) {
        free(base_prefix);
    }

    /* spawn procs */
    rc = orte_plm.spawn(jdata);
    OBJ_RELEASE(jdata);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        opal_progress_event_users_decrement();
        return MPI_ERR_SPAWN;
    }

    /* clean up */
    opal_progress_event_users_decrement();

    return OMPI_SUCCESS;
}

static int open_port(char *port_name)
{
    char *rml_uri, *ptr, tag[12];
    int rc;
    
    /*
     * The port_name is equal to the OOB-contact information
     * and an RML tag. The reason for adding the tag is
     * to make the port unique for multi-threaded scenarios.
     */
    
    if (NULL == (rml_uri = orte_rml.get_contact_info())) {
        return OMPI_ERR_NOT_AVAILABLE;
    }
    
    sprintf(tag, "%d", (int)next_tag);
    
    /* if the overall port name is too long, we try to truncate the rml uri */
    rc = 0;
    while ((strlen(rml_uri)+strlen(tag)) > (MPI_MAX_PORT_NAME-2)) {
        /* if we have already tried several times, punt! */
        if (4 < rc) {
            free(rml_uri);
            return OMPI_ERROR;
        }
        /* find the trailing uri and truncate there */
        ptr = strrchr(rml_uri, ';');
        *ptr = '\0';
        ++rc;
    }
    
    OPAL_THREAD_LOCK(&ompi_dpm_port_mutex);
    sprintf (port_name, "%s:%s", rml_uri, tag);
    next_tag++;
    OPAL_THREAD_UNLOCK(&ompi_dpm_port_mutex);
    
    free ( rml_uri );
    
    return OMPI_SUCCESS;
}

/* takes a port_name and separates it into the RML URI
* and the tag
*/
static char *parse_port (char *port_name, orte_rml_tag_t *tag)
{
    char *tmp_string, *ptr;
    
    /* find the ':' demarking the RML tag we added to the end */
    if (NULL == (ptr = strrchr(port_name, ':'))) {
        return NULL;
    }
    
    /* terminate the port_name at that location */
    *ptr = '\0';
    ptr++;
    
    /* convert the RML tag */
    sscanf(ptr,"%d", (int*)tag);
    
    /* see if the length of the RML uri is too long - if so,
        * truncate it
        */
    if (strlen(port_name) > MPI_MAX_PORT_NAME) {
        port_name[MPI_MAX_PORT_NAME] = '\0';
    }
    
    /* copy the RML uri so we can return a malloc'd value
        * that can later be free'd
        */
    tmp_string = strdup(port_name);
    
    return tmp_string;
}

static int close_port(char *port_name)
{
    return OMPI_SUCCESS;
}

static int dyn_init(void)
{
    char *oob_port=NULL;
    char *port_name=NULL;
    int root=0, rc;
    bool send_first = true;
    orte_rml_tag_t tag;
    ompi_communicator_t *newcomm=NULL;
    orte_process_name_t port_proc_name;
    ompi_group_t *group = NULL;
    ompi_errhandler_t *errhandler = NULL;
    
    ompi_communicator_t *oldcomm;
    
    /* if env-variable is set, we are a dynamically spawned
        * child - parse port and call comm_connect_accept */
    if (NULL == (port_name = ompi_dpm_base_dyn_init())) {
        /* nothing to do */
        return OMPI_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((1, ompi_dpm_base_output,
                         "%s dpm:orte:dyn_init with port %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         port_name));
    
    /* split the content of the environment variable into
    its pieces, which are RML-uri:tag */
    oob_port = parse_port (port_name, &tag);
    
    /* set the contact info into the local hash table */
    if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(oob_port))) {
        ORTE_ERROR_LOG(rc);
        free(oob_port);
        return(rc);
    }
    
    /* process the RML uri to get the port's process name */
    if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(oob_port, &port_proc_name, NULL))) {
        ORTE_ERROR_LOG(rc);
        free(oob_port);
        return rc;
    }
    free(oob_port);  /* done with this */
    
    /* update the route to this process - in this case, we always give it
     * as direct since we were given the contact info. We trust the
     * selected routed component to do the Right Thing for its own mode
     * of operation
     */
    if (ORTE_SUCCESS != (rc = orte_routed.update_route(&port_proc_name, &port_proc_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    OPAL_OUTPUT_VERBOSE((1, ompi_dpm_base_output,
                         "%s dpm:orte:dyn_init calling connect_accept to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&port_proc_name)));
    
    rc = connect_accept (MPI_COMM_WORLD, root, &port_proc_name,
                         send_first, &newcomm, tag );
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
    
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
    
    return OMPI_SUCCESS;
}


/*
 * finalize the module
 */
static int finalize(void)
{
    OBJ_DESTRUCT(&ompi_dpm_port_mutex);
    return OMPI_SUCCESS;
}

/*
 * instantiate the module
 */
ompi_dpm_base_module_t ompi_dpm_orte_module = {
    init,
    connect_accept,
    disconnect,
    spawn,
    dyn_init,
    ompi_dpm_base_dyn_finalize,
    ompi_dpm_base_mark_dyncomm,
    open_port,
    parse_port,
    close_port,
    finalize
};


