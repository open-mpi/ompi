/*
 * $HEADER$
 */

/**
 * @file
 *
 * Types for interface into the Open MPI Run Time Environment
 */

#ifndef OMPI_RUNTIME_TYPES_H
#define OMPI_RUNTIME_TYPES_H

#include "class/ompi_list.h"
#include "mca/ns/ns.h"

#include <sys/param.h>
#include <netdb.h>


/**
 * Container for key = value pairs from the node allocation container
 *
 * Container used for the \c info member of the \c
 * ompi_rte_node_allocation_t structure.  Ownership of char* strings must be
 * give to the container, who will \c free() them when the
 * container is destroyed.
 */
struct ompi_rte_valuepair_t {
    /** make us an instance of a list item */
    ompi_list_item_t super;
    /** key string for the info pair */
    char *key;
    /** value string for the info pair */
    char *value;
};
/** shorten ompi_rte_valuepair_t declarations */
typedef struct ompi_rte_valuepair_t ompi_rte_valuepair_t;
/** create the required instance information */
OBJ_CLASS_DECLARATION(ompi_rte_valuepair_t);


/**
 * Container for node allocation information.
 *
 * Container for allocation and deallocation of resources used to
 * launch parallel jobs.
 * 
 */
struct ompi_rte_node_allocation_t {
    /** make us an instance of list item */
    ompi_list_item_t super;
    /** hostname for this node.  Can be used as generic description
        field if hostnames aren't used on this platform */
    char hostname[MAXHOSTNAMELEN];
    /** number of MPI processes Open MPI can start on this host */
    int count;
    /** generic key=value storage mechanism */
    ompi_list_t *info;    
};
/** shorten ompi_rte_allocation_t declarations */
typedef struct ompi_rte_node_allocation_t ompi_rte_node_allocation_t;
/** create the required instance information */
OBJ_CLASS_DECLARATION(ompi_rte_node_allocation_t);


/**
 * Container use for process startup information
 *
 * Container describing a job to be launched.  A job consists of a
 * number of processes started on a number of nodes.  Each process
 * type (a unique argv/envp/cwd) is given its own instance of \c
 * ompi_rte_node_schedule_t and its own unique list of hosts to start
 * on.
 *
 * All memory associated with \c argv, \c env, \c cwd, and \c nodelist
 * is given to the instance of \c ompi_rte_node_schedule_t and will be
 * freed when the instance of \c ompi_rte_node_schedule_t is
 * destructed.
 */
struct ompi_rte_node_schedule_t {
    /** make us an instance of list item */
    ompi_list_item_t super;
    /** argv array for process to start (NULL terminated array) */
    char **argv;
    /** length of argv */
    int argc;
    /** environ array for process to start (NULL terminated array) */
    char **env;
    /** working directory in which to start the application */
    char *cwd;
    /** list of nodes to start the process on */
    ompi_list_t *nodelist;
};
/** shorten ompi_rte_node_schedule_t declarations */
typedef struct ompi_rte_node_schedule_t ompi_rte_node_schedule_t;
/** create the required instance information */
OBJ_CLASS_DECLARATION(ompi_rte_node_schedule_t);


/**
 * VPID type
 */
typedef pid_t ompi_vpid_t;


/**
 * Monitor callback type
 *
 * Typedef for callback function when there is a state change in any
 * of the processes that were spawned locally.  This function will
 * only be called while the library is in its progress function (ie,
 * not from signal handler context).
 */
typedef int (*ompi_rte_monitor_fn_t)(ompi_process_name_t* name,
                                     int newstate, 
                                     int status);

#endif /* OMPI_RUNTIME_TYPES_H */
