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
#include "include/constants.h"

#include "mca/mca.h"
#include "mca/oob/oob_types.h"

#include "ns_types.h"


/*
 * Component functions - all MUST be provided!
 */

/* Init the selected module
 */
typedef int (*orte_ns_base_module_init_fn_t)(void);

/**
 * Create a new cell id.
 * The create_cellid() function allocates a new cell id for use by the caller.
 * The function checks to find the next available cell id, reserves it, and returns that
 * number. No memory for names is allocated by this process. The range of answers is from
 * 1 to MCA_NS_BASE_CELLID_MAX-1 (zero is reserved for the seed name and cannot therefore be
 * allocated).
 *
 * @param None
 * @retval cellid The numerical value of the allocated cell id. A value of
 * MCA_NS_BASE_CELLID_MAX indicates
 * that an error occurred - this represents a very unlikely
 * event meaning that the system ran out of cell id's. This probably indicates
 * an error in the calling program as the number of available cell id's is extremely large.
 *
 * @code
 * new_cellid = ompi_name_server.create_cellid()
 * @endcode
 */
typedef int (*orte_ns_base_module_create_cellid_fn_t)(orte_cellid_t *cellid);

/**
 * Get the cell id for a process.
 * The cellid designator represents the physical location of the process - it is associated with
 * the hardware/system where the process is executing. Each process name contains this identifier
 * so that the system can issue commands (e.g., "die") to a collection of processes that are
 * executing on a common platform.
 *
 * Given that usage, it is necessary that the system have a way of telling a process its cellid.
 * The create_cellid() function is used by the system to associate a "cellid" identifier with
 * each platform. This function - assign_cellid_to_process() - is used to inform the process
 * of its cellid.
 *
 * Given a process name, this function will lookup its current platform and update the name with the
 * cellid.
 *
 * @param name Pointer to an ompi_process_name structure. The function will update the cellid
 * entry in the structure.
 *
 * @retval OMPI_SUCCESS Update was successful.
 * @retval OMPI_ERROR Update failed, most likely due to either a NULL process name pointer or the
 * inability to locate the process name in the lookup table.
 *
 * @code
 * return_value = ompi_name_server.assign_cellid_to_process(ompi_process_name_t* name);
 * @endcode
 */
typedef int (*orte_ns_base_module_assign_cellid_to_process_fn_t)(orte_process_name_t* name);

/**
 * Create a new job id.
 * The create_jobid() function  allocates a new job id for use by the caller.
 * The function checks to find the next available job id, reserves it, and returns that
 * number. No memory for names is allocated by this process. The range of answers is from
 * 1 to MCA_NS_BASE_JOBID_MAX-1 (zero is reserved for the seed name and cannot therefore be
 * allocated).

 *
 * The 0 job id is reserved for daemons within the system and will not be allocated.
 * Developers should therefore assume that the daemon job id is automatically allocated
 * and proceed to request names against it.
 *
 * @param None
 * @retval jobid The numerical value of the allocated job id. A value of
 * MCA_NS_BASE_JOBID_MAX indicates
 * that an error occurred - this represents a very unlikely
 * event meaning that the system ran out of job id's. This probably indicates
 * an error in the calling program as the number of available job id's is extremely large.
 *
 * @code
 * new_jobid = ompi_name_server.create_jobid()
 * @endcode
 */
typedef int (*orte_ns_base_module_create_jobid_fn_t)(orte_jobid_t *jobid);

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

/**
 * Derive a process vpid.
 * Given a base vpid and an offset, return the computed equivalent vpid. This function
 * is required because the vpid may not be an integer - need to provide a means for
 * computing the resulting vpid in case it isn't.
 */
typedef int (*orte_ns_base_module_derive_vpid_fn_t)(orte_vpid_t *vpid,
                                                    orte_vpid_t base_vpid,
                                                    int offset);

/**
 * Make a copy of a process name.
 * Given a process name, this function creates a copy of it and returns a pointer
 * to the duplicate structure.
 *
 * @param *name Pointer to an existing process name structure.
 *
 * @retval *newname Pointer to the duplicate structure, with all fields transferred.
 * @retval NULL Indicates an error - most likely due to a NULL process name
 * pointer being supplied as input.
 */
typedef int (*orte_ns_base_module_copy_proc_name_fn_t)(orte_process_name_t **dest,
                                                       orte_process_name_t* src);

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
 * Free (release) a process name.
 * The free_name() function releases the process name from the "used" list
 * maintained within the name server for the jobid contained in the specified
 * name. The memory for the name is also released at that time.
 *
 * Name values are currently \em not re-used. Hence, free-ing a name
 * does not provide any noticeable benefit other than releasing the memory. In
 * the future, names may be re-used if this becomes desirable.
 *
 * @param *name A pointer to the name structure containing the name being released.
 *
 * @retval OMPI_SUCCESS Indicates the release was succesfully accomplished.
 * @retval OMPI_ERROR Indicates the release failed - most likely due to an
 * error when free-ing the memory allocation.
 *
 * @code
 * if (OMPI_ERROR == ompi_name_server.free_name(&name) {
 *     report error
 *     }
 * @endcode
 */
typedef int (*orte_ns_base_module_free_name_fn_t)(orte_process_name_t **name);

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

/**
 * Get the virtual process id as a numeric value.
 * The get_vpid() function returns the vpid in a numeric representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name.
 *
 * @retval vpid The vpid field of the provided name.
 * @retval MCA_NS_BASE_VPID_MAX Indicates that an error occurred - in this case, that
 * the name variable provided was NULL.
 *
 * @code
 * vpid = ompi_name_server.get_vpid(&name)
 * @endcode
 */
typedef int (*orte_ns_base_module_get_vpid_fn_t)(orte_vpid_t *vpid, const orte_process_name_t *name);

/**
 * Get the job id as a numeric value.
 * The get_jobid() function returns the job id in a numeric representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name.
 *
 * @retval jobid The job id field of the provided name.
 * @retval MCA_NS_BASE_JOBID_MAX Indicates that an error occurred - in this case, that
 * the name variable provided was NULL.
 *
 * @code
 * jobid = ompi_name_server.get_jobid(&name)
 * @endcode
 */
typedef int (*orte_ns_base_module_get_jobid_fn_t)(orte_jobid_t *jobid, const orte_process_name_t* name);

/**
 * Get the cell id as a numberic value.
 * The get_cellid() function returns the cell id in a numeric representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name.
 *
 * @retval cellid The cell id field of the provided name.
 * @retval MCA_NS_BASE_CELLID_MAX Indicates that an error occurred - in this case, that
 * the name variable provided was NULL.
 *
 * @code
 * cellid = ompi_name_server.get_cellid(&name)
 * @endcode
 */
typedef int (*orte_ns_base_module_get_cellid_fn_t)(orte_cellid_t *cellid, const orte_process_name_t* name);

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
typedef int (*orte_ns_base_module_compare_fn_t)(orte_ns_cmp_bitmask_t fields,
                                                const orte_process_name_t* name1,
                                                const orte_process_name_t* name2);

/*
 * Allocate a tag
 * If name is NULL, tag server provides next unique tag but cannot look
 * that number up again for anyone else.
 */
typedef int (*orte_ns_base_module_assign_rml_tag_fn_t)(orte_rml_tag_t *tag,
                                                   char *name);

/*
 * Discover my name
 * Upon startup, each process must discover its official ORTE process name. There are
 * several ways this name could be passed to the process. This typicall involves an
 * environmental parameter of some appropriate name, possibly followed by some
 * computation of the vpid based on process rank. This function checks the different
 * environmental parameters to find the one that has been set with the appropriate
 * value, determines (based on that) the name of this process, and then sets that
 * value in the orte_system_info global structure.
 */
typedef int (*orte_ns_base_module_set_my_name_fn_t)(void);

/*
 * Get my peers
 * 
 * THIS FUNCTION MAY BE ELIMINATED IN FUTURE VERSIONS TO REMOVE MULTIPLE STORAGE
 * OF O(N) ARRAYS IN THE SYSTEM
 */
typedef int (*orte_ns_base_module_get_peers_fn_t)(orte_process_name_t **procs, 
                                  size_t *num_procs, size_t *self);

 
/*
 * Ver 1.0.0
 */
struct mca_ns_base_module_1_0_0_t {
    orte_ns_base_module_init_fn_t init;
    orte_ns_base_module_create_cellid_fn_t create_cellid;
    orte_ns_base_module_assign_cellid_to_process_fn_t assign_cellid_to_process;
    orte_ns_base_module_create_jobid_fn_t create_jobid;
    orte_ns_base_module_create_proc_name_fn_t create_process_name;
    orte_ns_base_module_copy_proc_name_fn_t copy_process_name;
    orte_ns_base_module_convert_string_to_process_name_fn_t convert_string_to_process_name;
    orte_ns_base_module_reserve_range_fn_t reserve_range;
    orte_ns_base_module_free_name_fn_t free_name;
    orte_ns_base_module_get_proc_name_string_fn_t get_proc_name_string;
    orte_ns_base_module_get_vpid_string_fn_t get_vpid_string;
    orte_ns_base_module_convert_vpid_to_string_fn_t convert_vpid_to_string;
    orte_ns_base_module_convert_string_to_vpid_fn_t convert_string_to_vpid;
    orte_ns_base_module_get_jobid_string_fn_t get_jobid_string;
    orte_ns_base_module_convert_jobid_to_string_fn_t convert_jobid_to_string;
    orte_ns_base_module_convert_string_to_jobid_fn_t convert_string_to_jobid;
    orte_ns_base_module_get_cellid_string_fn_t get_cellid_string;
    orte_ns_base_module_convert_cellid_to_string_fn_t convert_cellid_to_string;
    orte_ns_base_module_convert_string_to_cellid_fn_t convert_string_to_cellid;
    orte_ns_base_module_get_vpid_fn_t get_vpid;
    orte_ns_base_module_get_jobid_fn_t get_jobid;
    orte_ns_base_module_get_cellid_fn_t get_cellid;
    orte_ns_base_module_compare_fn_t compare;
    orte_ns_base_module_derive_vpid_fn_t derive_vpid;
    orte_ns_base_module_assign_rml_tag_fn_t assign_rml_tag;
    orte_ns_base_module_set_my_name_fn_t set_my_name;
    orte_ns_base_module_get_peers_fn_t get_peers;
};

typedef struct mca_ns_base_module_1_0_0_t mca_ns_base_module_1_0_0_t;
typedef mca_ns_base_module_1_0_0_t mca_ns_base_module_t;

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

struct mca_ns_base_component_1_0_0_t {
    mca_base_component_t ns_version;
    mca_base_component_data_1_0_0_t ns_data;

    mca_ns_base_component_init_fn_t ns_init;
    mca_ns_base_component_finalize_fn_t ns_finalize;
};
typedef struct mca_ns_base_component_1_0_0_t mca_ns_base_component_1_0_0_t;
typedef mca_ns_base_component_1_0_0_t mca_ns_base_component_t;



/*
 * Macro for use in components that are of type ns v1.0.0
 */
#define MCA_NS_BASE_VERSION_1_0_0 \
  /* ns v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* ns v1.0 */ \
  "ns", 1, 0, 0

/* Global structure for accessing name server functions
 */
OMPI_DECLSPEC extern mca_ns_base_module_t orte_ns;  /* holds selected module's function pointers */

#endif
