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


/**
 * Container for key = value pairs from the node allocation container
 *
 * Container used for the \c info member of the \c
 * ompi_rte_node_allocation_t structure.  Ownership of char* strings must be
 * give to the container, who will \c free() them when the
 * container is destroyed.
 */
struct ompi_rte_valuepair_t {
    ompi_list_item_t super;
    char *key;
    char *value;
};
typedef struct ompi_rte_valuepair_t ompi_rte_valuepair_t;
OBJ_CLASS_DECLARATION(ompi_rte_valuepair_t);


/**
 * Container for node allocation information.
 *
 * Container for allocation and deallocation of resources used to
 * launch parallel jobs.
 * 
 */
struct ompi_rte_node_allocation_t {
    ompi_list_item_t super;
    char hostname[MAXHOSTNAMELEN];
    int count;
    ompi_list_t *info;    
};
typedef struct ompi_rte_node_allocation_t ompi_rte_node_allocation_t;
OBJ_CLASS_DECLARATION(ompi_rte_node_allocation_t);


/**
 * Container use for process startup information
 *
 */
struct ompi_rte_node_schedule_t {
    ompi_list_item_t super;
    char **argv;
    int argc;
    char **env;
    char *cwd;
    ompi_list_t *nodelist;
};
typedef struct ompi_rte_node_schedule_t ompi_rte_node_schedule_t;
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
