/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Dynamic Process Management Interface
 *
 */

#ifndef OMPI_MCA_DPM_H
#define OMPI_MCA_DPM_H

#include "ompi_config.h"

#include <time.h>
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "ompi/mca/mca.h"
#include "opal/mca/base/base.h"

#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"

BEGIN_C_DECLS

/*
 * Initialize a module
 */
typedef int (*ompi_dpm_base_module_init_fn_t)(void);

/*
 * Connect/accept communications
 */
typedef int (*ompi_dpm_base_module_connect_accept_fn_t)(ompi_communicator_t *comm, int root,
                                                        const char *port, bool send_first,
                                                        ompi_communicator_t **newcomm);

/* define a callback function for use by non-blocking persistent connect/accept operations */
typedef void (*ompi_dpm_base_paccept_connect_callback_fn_t)(ompi_communicator_t *newcomm,
                                                            ompi_proc_t *remote_proc,
                                                            void *cbdata);

/*
 * Create a persistent connection point for accepting non-blocking connection requests.
 * The accept is persistent and will remain open until explicitly closed, or during
 * dpm_framework_close. Any incoming connection request will be used to create a new
 * communicator which will be returned via callback, along with the process name.
 *
 * In both cases, the callback function will return the new communicator plus the
 * user's original cbdata.
 *
 * paccept requires a port (typically obtained by a prior call to MPI_Open_port).
 * This must be published so it can be found by processes wanting to
 * connect to this process, and is passed by those processes as the "port" argument for
 * pconnect.
 *
 * Calls to pconnect are also non-blocking, with callback upon completion. Periodic
 * attempts to complete the connection may be made at the discretion of the implementation.
 * Failure to connect will be indicated by a callback returning a NULL communicator. Callers
 * should use the cbdata to track the corresponding pconnect request. A timeout
 * is provided to avoid hanging should the other process not have an active paccept
 * on the specified port (e.g., the process may have closed it). A NULL value for
 * the timeout argument indicates that the pconnect operation should not timeout,
 * and will regularly retry the connection forever.
 *
 * Processes may create and publish as many ports, and call paccept as many times, as
 * they like. When a process no longer wishes to accept connect requests, it can "close"
 * a paccept request by passing in the port used when calling paccept. A call to "close"
 * with a NULL argument will close *all* currently registered paccept channels.
 */
typedef int (*ompi_dpm_base_module_paccept_fn_t)(char *port,
                                                 ompi_dpm_base_paccept_connect_callback_fn_t cbfunc,
                                                 void *cbdata);

typedef int (*ompi_dpm_base_module_pconnect_fn_t)(char *port,
                                                  struct timeval *timeout,
                                                  ompi_dpm_base_paccept_connect_callback_fn_t cbfunc,
                                                  void *cbdata);

typedef void (*ompi_dpm_base_module_pclose_fn_t)(char *port);


/**
 * Executes internally a disconnect on all dynamic communicators
 * in case the user did not disconnect them.
 */
typedef int (*ompi_dpm_base_module_disconnect_fn_t)(ompi_communicator_t *comm);

/*
 * Dynamically spawn processes
 */
typedef int (*ompi_dpm_base_module_spawn_fn_t)(int count, char const *array_of_commands[],
                                               char **array_of_argv[],
                                               const int array_of_maxprocs[],
                                               const MPI_Info array_of_info[],
                                               const char *port_name);

/*
 * This routine checks, whether an application has been spawned
 * by another MPI application, or has been independently started.
 * If it has been spawned, it establishes the parent communicator.
 * Since the routine has to communicate, it should be among the last
 * steps in MPI_Init, to be sure that everything is already set up.
 */
typedef int (*ompi_dpm_base_module_dyn_init_fn_t)(void);

/*
 * Interface for mpi_finalize to call to ensure dynamically spawned procs
 * collectively finalize
 */
typedef int (*ompi_dpm_base_module_dyn_finalize_fn_t)(void);

/* this routine counts the number of different jobids of the processes
   given in a certain communicator. If there is more than one jobid,
   we mark the communicator as 'dynamic'. This is especially relevant
   for the MPI_Comm_disconnect *and* for MPI_Finalize, where we have
   to wait for all still connected processes.
*/
typedef void (*ompi_dpm_base_module_mark_dyncomm_fn_t)(ompi_communicator_t *comm);

/*
 * Open a port to interface to a dynamically spawned job - if the
 * specified tag is valid, then it will be used to form the port. Otherwise,
 * a dynamically assigned tag that is unique to this request will be provided
 */
typedef int (*ompi_dpm_base_module_open_port_fn_t)(char *port_name, ompi_rml_tag_t tag);

/*
 * Converts an opaque port string to a RML process nane and tag.
 */
typedef int (*ompi_dpm_base_module_parse_port_name_t)(const char *port_name,
                                                      char **hnp_uri, char **rml_uri,
                                                      ompi_rml_tag_t *tag);

/*
 * Update the routed component to make sure that the RML can send messages to
 * the remote port
 */
typedef int (*ompi_dpm_base_module_route_to_port_t)(char *rml_uri, ompi_process_name_t *rproc);


/*
 * Close a port
 */
typedef int (*ompi_dpm_base_module_close_port_fn_t)(const char *port_name);

/*
 * Finalize a module
 */
typedef int (*ompi_dpm_base_module_finalize_fn_t)(void);

/**
* Structure for DPM modules
 */
struct ompi_dpm_base_module_1_0_0_t {
    /** Initialization Function */
    ompi_dpm_base_module_init_fn_t              init;
    /* connect/accept */
    ompi_dpm_base_module_connect_accept_fn_t    connect_accept;
    /* disconnect */
    ompi_dpm_base_module_disconnect_fn_t        disconnect;
    /* spawn processes */
    ompi_dpm_base_module_spawn_fn_t             spawn;
    /* dyn_init */
    ompi_dpm_base_module_dyn_init_fn_t          dyn_init;
    /* dyn_finalize */
    ompi_dpm_base_module_dyn_finalize_fn_t      dyn_finalize;
    /* mark dyncomm */
    ompi_dpm_base_module_mark_dyncomm_fn_t      mark_dyncomm;
    /* open port */
    ompi_dpm_base_module_open_port_fn_t         open_port;
    /* parse port string */
    ompi_dpm_base_module_parse_port_name_t      parse_port;
    /* update route to a port */
    ompi_dpm_base_module_route_to_port_t        route_to_port;
    /* close port */
    ompi_dpm_base_module_close_port_fn_t        close_port;
    /* finalize */
    ompi_dpm_base_module_finalize_fn_t          finalize;
    /* pconnect/accept */
    ompi_dpm_base_module_pconnect_fn_t          pconnect;
    ompi_dpm_base_module_paccept_fn_t           paccept;
    ompi_dpm_base_module_pclose_fn_t            pclose;
};
typedef struct ompi_dpm_base_module_1_0_0_t ompi_dpm_base_module_1_0_0_t;
typedef struct ompi_dpm_base_module_1_0_0_t ompi_dpm_base_module_t;

OMPI_DECLSPEC extern ompi_dpm_base_module_t ompi_dpm;


/**
 * Structure for DPM components.
 */
struct ompi_dpm_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};
typedef struct ompi_dpm_base_component_2_0_0_t ompi_dpm_base_component_2_0_0_t;
typedef struct ompi_dpm_base_component_2_0_0_t ompi_dpm_base_component_t;

/**
 * Macro for use in components that are of type DPM
 */
#define OMPI_DPM_BASE_VERSION_2_0_0 \
    OMPI_MCA_BASE_VERSION_2_1_0("dpm", 2, 0, 0)


END_C_DECLS

#endif /* OMPI_MCA_DPM_H */
