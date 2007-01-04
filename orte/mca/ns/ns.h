/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
/** @file:
 *
 * The Open MPI Name Server
 *
 * The Open MPI Name Server provides unique name ranges for processes
 * within the universe. Each universe will have one name server
 * running within the seed daemon.  This is done to prevent the
 * inadvertent duplication of names.
 */

#ifndef MCA_NS_H
#define MCA_NS_H

/*
 * includes
 */

#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "orte/dss/dss.h"

#include "opal/mca/mca.h"
#include "orte/mca/rml/rml_types.h"

#include "ns_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


/*
 * Component functions - all MUST be provided!
 */

/* Init the selected module
 */
typedef int (*orte_ns_base_module_init_fn_t)(void);

/****   CELL FUNCTIONS   ****/
/**
 * Create a new cell id.
 * Allocates a new cell id for use by the caller. The function returns an
 * existing cellid if the specified site/resource already has been assigned
 * one.
 *
 * @param site The name of the site where the cell is located.
 * @param resource The name of the resource associated with this cell (e.g., the name
 * of the cluster).
 * @param cellid The location where the cellid is to be stored.
 *
 * @retval ORTE_SUCCESS A cellid was created and returned.
 * @retval ORTE_ERROR_VALUE An error code indicative of the problem.
 *
 * @endcode
 */
typedef int (*orte_ns_base_module_create_cellid_fn_t)(orte_cellid_t *cellid,
                                char *site, char *resource);

/**
 * Get cell info
 * Retrieve the site and resource info on a cell.
 *
 * @param cellid The id of the cell who's info is being requested.
 * @param site Returns a pointer to a strdup'd string containing the site name.
 * @param resource Returns a pointer to a strdup'd string containg the resource name.
 * @retval ORTE_SUCCESS A cellid was created and returned.
 * @retval ORTE_ERROR_VALUE An error code indicative of the problem.
 */
typedef int (*orte_ns_base_module_get_cell_info_fn_t)(orte_cellid_t cellid,
                                char **site, char **resource);

/**
 * Get the cell id as a character string.
 * The get_cellid_string() function returns the cell id in a character string
 * representation. The string is created by expressing the field in hexadecimal. Memory
 * for the string is allocated by the function - releasing that allocation is the
 * responsibility of the calling program.
 *
 * @param *name A pointer to the name structure containing the name to be
 * "translated" to a string.
 *
 * @retval *name_string A pointer to the character string representation of the
 * cell id.
 * @retval NULL Indicates an error occurred - either no memory could be allocated
 * or the caller provided an incorrect name pointer (e.g., NULL).
 *
 * @code
 * cellid-string = ompi_name_server.get_cellid_string(&name)
 * @endcode
 */
typedef int (*orte_ns_base_module_get_cellid_string_fn_t)(char **cellid_string, const orte_process_name_t* name);

/**
 * Convert cellid to character string
 * Returns the cellid in a character string representation. The string is created
 * by expressing the provided cellid in hexadecimal. Memory for the string is
 * allocated by the function - releasing that allocation is the responsibility of
 * the calling program.
 *
 * @param cellid The cellid to be converted.
 *
 * @retval *cellid_string A pointer to a character string representation of the cellid.
 * @retval NULL Indicates an error occurred - probably no memory could be allocated.
 *
 * @code
 * cellid-string = ompi_name_server.convert_cellid_to_string(cellid);
 * @endcode
 */
 typedef int (*orte_ns_base_module_convert_cellid_to_string_fn_t)(char **cellid_string, const orte_cellid_t cellid);

 /**
  * Convert a string to a cellid.
  * Converts a characters string into a cellid. The character string must be a
  * hexadecimal representation of a valid cellid.
  *
  * @param cellidstring The string to be converted.
  *
  * @retval cellid The resulting cellid
  * @retval MCA_NS_BASE_CELLID_MAX String could not be converted.
  *
  * @code
  * cellid = ompi_name_server.convert_string_to_cellid(cellidstring);
  * @endcode
  */
typedef int (*orte_ns_base_module_convert_string_to_cellid_fn_t)(orte_cellid_t *cellid, const char *cellidstring);


/****    NODE FUNCTIONS    ****/
/*
 * Get an array of node id's
 * Given the cell and a NULL-terminated array of names of nodes within it, this function assigns an id to represent
 * each node within the cell.
 */
typedef int (*orte_ns_base_module_create_nodeids_fn_t)(orte_nodeid_t **nodes, orte_std_cntr_t *nnodes,
                                                       orte_cellid_t cellid, char **nodename);

/*
 * Get node info
 * Retrieve the names of an array of nodes given their cellid and nodeids. The cellid
 * is required as the nodeids are only unique within a given cell.
 *
 * @param cellid The id of the cell of the node.
 * @param nodeids The ids of the node.
 * @param nodenames Returns a pointer to a NULL-terminated array of strdup'd strings containing the node names.
 * @retval ORTE_SUCCESS The nodename was created and returned.
 * @retval ORTE_ERROR_VALUE An error code indicative of the problem.
 */
typedef int (*orte_ns_base_module_get_node_info_fn_t)(char ***nodename, orte_cellid_t cellid,
                                                      orte_std_cntr_t num_nodes, orte_nodeid_t *nodeids);

/*
 * Convert nodeid to character string
 * Returns the nodeid in a character string representation. The string is created
 * by expressing the provided nodeid in decimal. Memory for the string is
 * allocated by the function - releasing that allocation is the responsibility of
 * the calling program.
 *
 * @param nodeid The nodeid to be converted.
 *
 * @param *nodeid_string A pointer to a character string representation of the nodeid.
 * @retval ORTE_SUCCESS The string was created and returned.
 * @retval ORTE_ERROR_VALUE An error code indicative of the problem.
 */
typedef int (*orte_ns_base_module_convert_nodeid_to_string_fn_t)(char **nodeid_string, const orte_nodeid_t nodeid);

/*
 * Convert a string to a nodeid.
 * Converts a characters string into a nodeid. The character string must be a
 * decimal representation of a valid nodeid.
 *
 * @param nodeidstring The string to be converted.
 *
 * @param nodeid A pointer to a location where the resulting nodeid is to be stored.
 * @retval ORTE_SUCCESS The string was created and returned.
 * @retval ORTE_ERROR_VALUE An error code indicative of the problem.
 */
typedef int (*orte_ns_base_module_convert_string_to_nodeid_fn_t)(orte_nodeid_t *nodeid, const char *nodeidstring);


/****   JOB ID FUNCTIONS   ****/
/**
 * Create a new job id.
 * Allocate a new job id for use by the caller.
 * 
 * The 0 job id is reserved for daemons within the system and will not be allocated.
 * Developers should therefore assume that the daemon job id is automatically allocated
 * and proceed to request names against it.
 *
 * @param None
 * @param jobid A pointer to the location where the jobid is to be returned.
 * @param attrs A list of attributes that describe any conditions to be placed on
 * the assigned jobid. For example, specifying USE_PARENT indicates that the specified
 * jobid is to be identified as the parent of the new jobid. USE_ROOT indicates that
 * the root of the job family of the specified jobid is to be identified as the parent.
 */
typedef int (*orte_ns_base_module_create_jobid_fn_t)(orte_jobid_t *jobid, opal_list_t *attrs);

/*
 * Get job descendants
 * Given a jobid, return the array of jobids that descend from this one.
 */
typedef int (*orte_ns_base_module_get_job_descendants_fn_t)(orte_jobid_t** descendants,
                                                            orte_std_cntr_t *num_desc,
                                                            orte_jobid_t job);
             
/*
 * Get job children
 * Given a jobid, return the array of jobids that are direct children of that job
 */
typedef int (*orte_ns_base_module_get_job_children_fn_t)(orte_jobid_t** children,
                                                         orte_std_cntr_t *num_childs,
                                                         orte_jobid_t job);

/*
 * Get root job from job family
 * Given a jobid, return the jobid at the head of this job's family. If the jobid provided is the
 * root for that family, that value will be returned.
 */
typedef  int (*orte_ns_base_module_get_root_job_fn_t)(orte_jobid_t *root_job, orte_jobid_t job);

/*
 * Get parent jobid
 * Given a jobid, return the parent job from which it descended. If the provided jobid is the
 * root (i.e., has no parent), this function will return that same value.
 */
typedef int (*orte_ns_base_module_get_parent_job_fn_t)(orte_jobid_t *parent, orte_jobid_t job);

/**
 * Reserve a range of process id's.
 * The reserve_range() function reserves a range of vpid's for the given jobid.
 * Note that the cellid does not factor into this request - jobid's span the entire universe,
 * hence the cell where the process is currently executing is irrelevant to this request.
 *
 * @param jobid The id of the job for which the vpid's are to be reserved.
 * @param range The number of vpid's to be reserved. The function will find the
 * next available process id and assign range-number of sequential id's to the caller.
 * These id's will be reserved - i.e., they cannot be assigned to any subsequent caller.
 *
 * @retval startid The starting value of the reserved range of vpid's. A value of MCA_NS_BASE_VPID_MAX
 * indicates that an error occurred.
 *
 * @code
 * starting_procid = ompi_name_server.reserve_range(jobid, range)
 * @endcode
 */
typedef int (*orte_ns_base_module_reserve_range_fn_t)(orte_jobid_t job,
                                                      orte_vpid_t range,
                                                      orte_vpid_t *startvpid);

/**
 * Get the job id as a character string.
 * The get_jobid_string() function returns the job id in a character string
 * representation. The string is created by expressing the field in hexadecimal. Memory
 * for the string is allocated by the function - releasing that allocation is the
 * responsibility of the calling program.
 *
 * @param *name A pointer to the name structure containing the name to be
 * "translated" to a string.
 *
 * @retval *name_string A pointer to the character string representation of the
 * job id.
 * @retval NULL Indicates an error occurred - either no memory could be allocated
 * or the caller provided an incorrect name pointer (e.g., NULL).
 *
 * @code
 * jobid-string = ompi_name_server.get_jobid_string(&name)
 * @endcode
 */
typedef int (*orte_ns_base_module_get_jobid_string_fn_t)(char **jobid_string, const orte_process_name_t* name);

/**
 * Convert jobid to character string
 * The convert_jobid_to_string() function returns the jobid in a character string representation.
 * The string is created by expressing the provided jobid in hexadecimal. Memory
 * for the string is allocated by the function - releasing that allocation is the
 * responsibility of the calling program.
 *
 * @param jobid The jobid to be converted.
 *
 * @retval *jobid_string A pointer to a character string representation of the
 * jobid.
 * @retval NULL Indicates an error occurred - probably no memory could be allocated.
 *
 * @code
 * jobid-string = ompi_name_server.convert_jobid_to_string(jobid);
 * @endcode
 */
typedef int (*orte_ns_base_module_convert_jobid_to_string_fn_t)(char **jobid_string, const orte_jobid_t jobid);

/**
 * Convert a string to a jobid
 * Converts a character string into a jobid. The character string must be a hexadecimal
 * representation of a valid jobid.
 *
 * @param jobidstring The string to be converted.
 *
 * @retval jobid The resulting jobid.
 * @retval MCA_NS_BASE_JOBID_MAX String could not be converted.
 *
 * @code
 * jobid = ompi_name_server.convert_string_to_jobid(jobidstring);
 * @endcode
 *
 */
typedef int (*orte_ns_base_module_convert_string_to_jobid_fn_t)(orte_jobid_t *jobid, const char* jobidstring);



/**** NAME FUNCTIONS ****/
/**
 * Obtain a single new process name.
 * The create_process_name() function creates a single process name structure and fills the
 * fields with the provided values.
 *
 * @param cell The cell for which the process name is intended. Usually, this is
 * the id of the cell where the process is initially planning to be spawned.
 * @param job The id of the job to which the process will belong. Process id's are
 * tracked according to jobid, but not cellid. Thus, two processes
 * can have the same process id if and only if they have different jobid's. However,
 * two processes in the same jobid cannot have the same process id, regardless
 * of whether or not they are in the same cell.
 * @param vpid The virtual process id for the name. Note that no check is made for uniqueness -
 * the caller is responsible for ensuring that the requested name is, in fact, unique
 * by first requesting reservation of an appropriate range of virtual process id's.
 *
 * @retval *name Pointer to an ompi_process_name_t structure containing the name.
 * @retval NULL Indicates an error, probably due to inability to allocate memory for
 * the name structure.
 *
 * @code
 * new_name = ompi_name_server.create_process_name(cell, job, vpid);
 * @endcode
 */
typedef int (*orte_ns_base_module_create_proc_name_fn_t)(orte_process_name_t **name,
                                                         orte_cellid_t cell,
                                                         orte_jobid_t job,
                                                         orte_vpid_t vpid);

/*
 * Create my name
 * If a process is a singleton, then it needs to create a name for itself. When
 * a persistent daemon is present, this requires a communication to that daemon.
 * Since the RML uses process names as its index into the RML communicator table,
 * the RML automatically assigns a name to each process when it first attempts
 * to communicate. This function takes advantage of that behavior to ensure that
 * one, and ONLY one, name gets assigned to the process
 */
typedef int (*orte_ns_base_module_create_my_name_fn_t)(void);

/**
 * Convert a string representation to a process name.
 * The convert_string_to_process_name() function converts a string representation of a process
 * name into an Open MPI name structure. The string must be of the proper form - i.e., it
 * must be in the form "cellid.jobid.vpid", where each field is expressed in hexadecimal form.
 *
 * @param *name_string A character string representation of a process name.
 *
 * @retval *name Pointer to an ompi_process_name_t structure containing the name.
 * @retval NULL Indicates an error, probably due to inability to allocate memory for
 * the name structure.
 *
 * @code
 * name = ompi_name_server.convert_string_to_process_name(name_string);
 * @endcode
 */
typedef int (*orte_ns_base_module_convert_string_to_process_name_fn_t)(orte_process_name_t **name,
                                                                       const char* name_string);


/**
 * Get the process name as a character string.
 * The get_proc_name_string() function returns the entire process name in a
 * character string representation. The string is created by expressing each
 * field in hexadecimal separated by periods, as follows:
 *
 * sprintf(string_name, "%x.%x.%x", cellid, jobid, vpid)
 *
 * The memory required for the string is allocated by the function - releasing
 * that allocation is the responsibility of the calling program.
 *
 * @param *name A pointer to the name structure containing the name to be
 * "translated" to a string.
 *
 * @retval *name_string A pointer to the character string representation of the
 * full name.
 * @retval NULL Indicates an error occurred - either no memory could be allocated
 * or the caller provided an incorrect name pointer (e.g., NULL).
 *
 * @code
 * name-string = ompi_name_server.get_proc_name_string(&name)
 * @endcode
 */
typedef int (*orte_ns_base_module_get_proc_name_string_fn_t)(char **name_string,
                                                             const orte_process_name_t* name);

/**
 * Compare two name values.
 * The compare() function checks the value of the fields in the two
 * provided names, and returns a value indicating if the first one is less than, greater
 * than, or equal to the second. The value of each field is compared in a hierarchical
 * fashion, with cellid first, followed by jobid and vpid in sequence. The bit-mask
 * indicates which fields are to be included in the comparison. Fields not included via the
 * bit-mask are ignored. Thus, the caller may request that any combination of the three fields
 * be included in the comparison.
 *
 * @param fields A bit-mask indicating which fields are to be included in the comparison. The
 * comparison is performed on a hierarchical basis, with cellid being first, followed by
 * jobid and then vpid. Each field can be included separately, thus allowing the caller
 * to configure the comparison to meet their needs.
 * @param *name1 A pointer to the first name structure.
 * @param *name2 A pointer to the second name structure.
 *
 * @retval -1 The indicated fields of the first provided name are less than the same
 * fields of the second provided name.
 * @retval 0 The indicated fields of the two provided names are equal.
 * @retval +1 The indicated fields of the first provided name is greater than the same
 * fields of the second provided name.
 *
 * The function returns a large negative value if there is an error.
 *
 * @code
 * result = ompi_name_server.compare(bit_mask, &name1, &name2)
 * @endcode
 */
typedef int (*orte_ns_base_module_compare_fields_fn_t)(orte_ns_cmp_bitmask_t fields,
                                                       const orte_process_name_t* name1,
                                                       const orte_process_name_t* name2);


/****   VPID FUNCTIONS   ****/
/**
 * Get the virtual process id as a character string.
 * The get_vpid_string() function returns the vpid in a character string
 * representation. The string is created by expressing the field in hexadecimal. Memory
 * for the string is allocated by the function - releasing that allocation is the
 * responsibility of the calling program.
 *
 * @param *name A pointer to the name structure containing the name to be
 * "translated" to a string.
 *
 * @retval *name_string A pointer to the character string representation of the
 * vpid.
 * @retval NULL Indicates an error occurred - either no memory could be allocated
 * or the caller provided an incorrect name pointer (e.g., NULL).
 *
 * @code
 * vpid-string = ompi_name_server.get_vpid_string(&name)
 * @endcode
 */
typedef int (*orte_ns_base_module_get_vpid_string_fn_t)(char **vpid_string, const orte_process_name_t* name);

/**
 * Convert vpid to character string
 * Returns the vpid in a character string representation. The string is created
 * by expressing the provided vpid in hexadecimal. Memory for the string is
 * allocated by the function - releasing that allocation is the responsibility of
 * the calling program.
 *
 * @param vpid The vpid to be converted.
 *
 * @retval *vpid_string A pointer to a character string representation of the vpid.
 * @retval NULL Indicates an error occurred - probably no memory could be allocated.
 *
 * @code
 * vpid-string = ompi_name_server.convert_vpid_to_string(vpid);
 * @endcode
 */
 typedef int (*orte_ns_base_module_convert_vpid_to_string_fn_t)(char **vpid_string, const orte_vpid_t vpid);

 /**
  * Convert a string to a vpid.
  * Converts a characters string into a vpid. The character string must be a
  * hexadecimal representation of a valid vpid.
  *
  * @param vpidstring The string to be converted.
  *
  * @retval vpid The resulting vpid
  * @retval MCA_NS_BASE_VPID_MAX String could not be converted.
  *
  * @code
  * vpid = ompi_name_server.convert_string_to_vpid(vpidstring);
  * @endcode
  */
typedef int (*orte_ns_base_module_convert_string_to_vpid_fn_t)(orte_vpid_t *vpid, const char* vpidstring);



/**** TAG SERVER ****/
/*
 * Allocate a tag
 * If name is NULL, tag server provides next unique tag but cannot look
 * that number up again for anyone else.
 */
typedef int (*orte_ns_base_module_assign_rml_tag_fn_t)(orte_rml_tag_t *tag,
                                                   char *name);

/**** DATA TYPE SERVER ****/
/* This function defines a new data type and gives it a system-wide unique
 * identifier for use in the data packing subsystem. Only called from the
 * dps when needing a new identifier.
 */
typedef int (*orte_ns_base_module_define_data_type_fn_t)(
                     const char *name,
                     orte_data_type_t *type);


/****    PEER RETRIEVAL    ****/
/**
 * Get the process names of all processes in the specified conditions. It is
 * sometimes necessary for a process to communicate to all processes of a
 * given job, all processes in a given cell or on a given node, etc. The RML
 * communication system utilizes the process name as its "pointer" for
 * sending messages to another process. This function returns an array of
 * process name pointers that contains the names of all processes that
 * meet the specified combination of attributes.
 *
 * @param procs The location where the address of the array of pointers
 * is to be stored. The function will dynamically allocate space for the
 * array - the caller is responsible for releasing this space.
 * @param num_procs The location where the number of entries in the
 * returned array is to be stored.
 * @param attributes A list of conditions to be used in defining the
 * peers to be included in the returned array. This can include a
 * request that all peers for the parent job be returned, for example.
 * More common options would be to specify a cell or job.
 *
 * NOTE The combination of ORTE_CELLID_WILDCARD and ORTE_JOBID_WILDCARD
 * in the attribute list will cause the function to return the names of *all*
 * processes currently active in the universe.
 *
 */
typedef int (*orte_ns_base_module_get_peers_fn_t)(orte_process_name_t **procs,
                                                  orte_std_cntr_t *num_procs,
                                                  opal_list_t *attributes);


/*
 * DIAGNOSTIC INTERFACES
 */
typedef int (*orte_ns_base_module_dump_cells_fn_t)(void);

typedef int (*orte_ns_base_module_dump_jobs_fn_t)(void);

typedef int (*orte_ns_base_module_dump_tags_fn_t)(void);

typedef int (*orte_ns_base_module_dump_datatypes_fn_t)(void);


/*
 * Ver 2.0
 */
struct mca_ns_base_module_2_0_0_t {
    /* init */
    orte_ns_base_module_init_fn_t                           init;
    /* cell functions */
    orte_ns_base_module_create_cellid_fn_t                  create_cellid;
    orte_ns_base_module_get_cell_info_fn_t                  get_cell_info;
    orte_ns_base_module_get_cellid_string_fn_t              get_cellid_string;
    orte_ns_base_module_convert_cellid_to_string_fn_t       convert_cellid_to_string;
    orte_ns_base_module_convert_string_to_cellid_fn_t       convert_string_to_cellid;
    /** node functions */
    orte_ns_base_module_create_nodeids_fn_t                 create_nodeids;
    orte_ns_base_module_get_node_info_fn_t                  get_node_info;
    orte_ns_base_module_convert_nodeid_to_string_fn_t       convert_nodeid_to_string;
    orte_ns_base_module_convert_string_to_nodeid_fn_t       convert_string_to_nodeid;
    /* jobid functions */
    orte_ns_base_module_create_jobid_fn_t                   create_jobid;
    orte_ns_base_module_get_job_descendants_fn_t            get_job_descendants;
    orte_ns_base_module_get_job_children_fn_t               get_job_children;
    orte_ns_base_module_get_root_job_fn_t                   get_root_job;
    orte_ns_base_module_get_parent_job_fn_t                 get_parent_job;
    orte_ns_base_module_get_jobid_string_fn_t               get_jobid_string;
    orte_ns_base_module_convert_jobid_to_string_fn_t        convert_jobid_to_string;
    orte_ns_base_module_convert_string_to_jobid_fn_t        convert_string_to_jobid;
    orte_ns_base_module_reserve_range_fn_t                  reserve_range;
    /* vpid functions */
    orte_ns_base_module_get_vpid_string_fn_t                get_vpid_string;
    orte_ns_base_module_convert_vpid_to_string_fn_t         convert_vpid_to_string;
    orte_ns_base_module_convert_string_to_vpid_fn_t         convert_string_to_vpid;
    /* name functions */
    orte_ns_base_module_create_proc_name_fn_t               create_process_name;
    orte_ns_base_module_create_my_name_fn_t                 create_my_name;
    orte_ns_base_module_convert_string_to_process_name_fn_t convert_string_to_process_name;
    orte_ns_base_module_get_proc_name_string_fn_t           get_proc_name_string;
    orte_ns_base_module_compare_fields_fn_t                 compare_fields;
    /* peer functions */
    orte_ns_base_module_get_peers_fn_t                      get_peers;
    /* tag server functions */
    orte_ns_base_module_assign_rml_tag_fn_t                 assign_rml_tag;
    /* data type functions */
    orte_ns_base_module_define_data_type_fn_t               define_data_type;
    /* diagnostic functions */
    orte_ns_base_module_dump_cells_fn_t                     dump_cells;
    orte_ns_base_module_dump_jobs_fn_t                      dump_jobs;
    orte_ns_base_module_dump_tags_fn_t                      dump_tags;
    orte_ns_base_module_dump_datatypes_fn_t                 dump_datatypes;
};

typedef struct mca_ns_base_module_2_0_0_t mca_ns_base_module_2_0_0_t;
typedef mca_ns_base_module_2_0_0_t mca_ns_base_module_t;

/*
 * NS Component
 */
/**
 * Initialize the selected component.
 */
typedef mca_ns_base_module_t* (*mca_ns_base_component_init_fn_t)(int *priority);

/**
 * Finalize the selected module
 */
typedef int (*mca_ns_base_component_finalize_fn_t)(void);


/*
 * the standard component data structure
 */

struct mca_ns_base_component_2_0_0_t {
    mca_base_component_t ns_version;
    mca_base_component_data_1_0_0_t ns_data;

    mca_ns_base_component_init_fn_t ns_init;
    mca_ns_base_component_finalize_fn_t ns_finalize;
};
typedef struct mca_ns_base_component_2_0_0_t mca_ns_base_component_2_0_0_t;
typedef mca_ns_base_component_2_0_0_t mca_ns_base_component_t;



/*
 * Macro for use in components that are of type ns v2.0.0
 */
#define MCA_NS_BASE_VERSION_2_0_0 \
  /* ns v2.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* ns v2.0 */ \
  "ns", 2, 0, 0

/* Global structure for accessing name server functions
 */
ORTE_DECLSPEC extern mca_ns_base_module_t orte_ns;  /* holds selected module's function pointers */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
