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
#include "mca/pcm/pcm.h"

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 * Spawn Handle
 *
 * Private container for the Run-Time Environment to store mappings of
 * spawn criteria to PCM components
 */
struct ompi_rte_spawn_handle_t {
    /** make us an object instance */
    ompi_object_t super;
    /** criteria used to select this set of pcms */
    int criteria;
    /** pointers to the pcms */
    mca_pcm_base_module_t **modules;
    /** lentgh of modules */
    size_t modules_len;
};
/** shorten ompi_rte_spawn_handle_t declarations */
typedef struct ompi_rte_spawn_handle_t ompi_rte_spawn_handle_t;
/** create the required instance information */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_rte_spawn_handle_t);


/**
 * Process startup description container
 *
 * Container used to descript the processes to be launched as part of
 * a job.  A job consists of a number of process started on some set
 * of resources (specified by \c nodelist).  Each process type (a
 * unique argv/envp/cwd) is given its own instance of \c
 * ompi_rte_node_schedule_t and its own unique list of resources to
 * utilize.
 *
 * All memory associated with \c argv, \c env, \c cwd, and \c nodelist
 * is given to the instance of \c ompi_rte_node_schedule_t and will be
 * freed when the instance of \c ompi_rte_node_schedule_t is
 * destructed.
 *
 * if nodelist is not NULL, the contents of the list will be
 * destructed when the instance of the \c ompi_rte_node_schedule_t is
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
    /** length of env */
    int envc;
    /** working directory in which to start the application */
    char *cwd;
    /** list of nodes to start the process on (list of 
        \c ompi_rte_node_allocation_t) */
    ompi_list_t *nodelist;
};
/** shorten ompi_rte_node_schedule_t declarations */
typedef struct ompi_rte_node_schedule_t ompi_rte_node_schedule_t;
/** create the required instance information */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_rte_node_schedule_t);


/**
 * Base container for node-related information
 *
 * Base container type for holding llm/pcm private information on a \c
 * ompi_rte_node_allocation_t container.
 */
struct ompi_rte_node_allocation_data_t {
    /** make us an instance of object so our constructors go boom */
    ompi_object_t super;
};
/** shorten ompi_rte_node_allocation_data_t declarations */
typedef struct ompi_rte_node_allocation_data_t ompi_rte_node_allocation_data_t;
/** create the required instance information */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_rte_node_allocation_data_t);


/**
 * Resource allocation container
 *
 * Container for passing information between the resource allocator,
 * the resource/job mapper, and the job starter portions of the
 * run-time environment.
 *
 * \c count has a strange meaning.  If \c nodes is 0, \c count is the
 * total number of cpus available in this block of resources.  If \c
 * nodes is non-zero, \c count is the number of cpus available per
 * node.
 *
 * \c start provides an integer number of where in the job the
 * resource is available.  If you had two node_allocation_t elements
 * returned from a call to allocate resources, one with
 * nodes=4,count=2 and one with nodes=2,count=4, start would be 0 for
 * the first element and 8 for the second.
 *
 * The contents of the structure (with the exception of \c data) may
 * be examined by the process mapping functions.  However, the fields
 * should be considered read-only.  The \c data field may contain
 * private data that reflects the status of the \c nodes and \c count
 * fields.  The \c ompi_rte_node_* functions are available for
 * manipulating \c ompi_rte_node_allocation_t structures.
 */
struct ompi_rte_node_allocation_t {
    /** make us an instance of list item */
    ompi_list_item_t super;
    /** start of allocation numbers for this block of nodes */
    int start;
    /** number of nodes in this allocation - 0 means unknown */
    int nodes;
    /** number of "process slots" (places to start a process) that
        are allocated as part of this block of processes */
    int count;
    /** data store for use by the Open MPI run-time environment */
    ompi_rte_node_allocation_data_t *data;
};
/** shorten ompi_rte_allocation_t declarations */
typedef struct ompi_rte_node_allocation_t ompi_rte_node_allocation_t;
/** create the required instance information */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_rte_node_allocation_t);


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
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_rte_valuepair_t);


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

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OMPI_RUNTIME_TYPES_H */
