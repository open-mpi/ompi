/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI Name Server
 *
 * The Open MPI Name Server provides unique name ranges for processes within the
 * universe. Each universe will have one name server running within the seed daemon.
 * This is done to prevent the inadvertent duplication of names.
 *
 */

#ifndef MCA_NS_H
#define MCA_NS_H

/*
 * includes
 */
#include <sys/types.h>
#include <stdint.h>


#include "ompi_config.h"
#include "include/types.h"
#include "class/ompi_list.h"

#include "mca/mca.h"


/*
 * general typedefs & structures
 */
typedef uint32_t ompi_process_id_t;  /**< Set the allowed range for id's in each space */
                                                                                                                      

struct ompi_process_name_t {
    ompi_process_id_t cellid;  /**< Cell number */
    ompi_process_id_t jobid; /**< Job number */
    ompi_process_id_t procid;  /**< Process number */
};
typedef struct ompi_process_name_t ompi_process_name_t;


/*
 * Component functions - all MUST be provided!
 */

/**
 * Create a new cell id.
 * The create_cellid() function allocates a new cell id for use by the caller.
 * The function checks to find the next available cell id, reserves it, and returns that
 * number. No memory for names is allocated by this process.
 *
 * @param None
 * @retval cellid The ompi_process_id_t value of the allocated cell id. There currently
 * is no error indication that a cell id could not be allocated - this represents a very unlikely
 * event meaning that the system ran out of cell id's. This probably indicates
 * an error in the calling program as the number of available cell id's is extremely large.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * new_cellid = ompi_name_server.create_cellid()
 * @endcode
 */
typedef ompi_process_id_t (*mca_ns_create_cellid_t)(void);

/**
 * Create a new job id.
 * The create_jobid() function  allocates a new job id for use by the caller.
 * The function checks to find the next available job id, reserves it, and returns that
 * number. No memory for names is allocated by this process.
 *
 * The 0 job id is reserved for daemons within the system and will not be allocated.
 * Developers should therefore assume that the daemon job id is automatically allocated
 * and proceed to request names against it.
 *
 * @param None
 * @retval jobid The ompi_process_id_t value of the allocated job id. There currently
 * is no error indication that a job id could not be allocated - this represents a very unlikely
 * event meaning that the system ran out of job id's. This probably indicates
 * an error in the calling program as the number of available job id's is extremely large.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * new_jobid = ompi_name_server.create_jobid()
 * @endcode
 */
typedef ompi_process_id_t (*mca_ns_create_jobid_t)(void);

/**
 * Obtain a single new process name.
 * The create_process_name() function obtains a single new process name. The
 * function checks to find the next available process id, allocates memory
 * for the name, and then fills in the fields.
 *
 * @param cell The cell for which the process name is intended. Usually, this is
 * the id of the cell where the process is initially planning to be spawned.
 * @param job The id of the job to which the process will belong. Process id's are
 * tracked according to jobid, but not cellid. Thus, two processes
 * can have the same process id if and only if they have different jobid's. However,
 * two processes in the same jobid cannot have the same process id, regardless
 * of whether or not they are in the same cell.
 *
 * @retval *name Pointer to an ompi_process_name_t structure containing the name.
 * @retval NULL Indicates that the name server is out of names for that jobid. This is
 * an unlikely event given the extremely large number of available process id's. Thus,
 * it probably indicates an error in the calling program.
 *
 * @code
 * new_name = ompi_name_server.create_process_name(cell, job);
 * @endcode
 */
typedef ompi_process_name_t* (*mca_ns_create_proc_name_t)(ompi_process_id_t cell, ompi_process_id_t job);
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
typedef int (*mca_ns_free_name_t)(ompi_process_name_t *name);

/**
 * Get the process name as a character string.
 * The get_proc_name_string() function returns the entire process name in a
 * character string representation. The string is created by expressing each
 * field in hexadecimal separated by periods, as follows:
 *
 * sprintf(string_name, "%x.%x.%x", cellid, jobid, processid)
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
typedef char* (*mca_ns_get_proc_name_string_t)(ompi_process_name_t *name);

/**
 * Get the process id as a character string.
 * The get_procid_string() function returns the process id in a character string
 * representation. The string is created by expressing the field in hexadecimal. Memory
 * for the string is allocated by the function - releasing that allocation is the
 * responsibility of the calling program.
 *
 * @param *name A pointer to the name structure containing the name to be
 * "translated" to a string.
 *
 * @retval *name_string A pointer to the character string representation of the
 * process id.
 * @retval NULL Indicates an error occurred - either no memory could be allocated
 * or the caller provided an incorrect name pointer (e.g., NULL).
 *
 * @code
 * procid-string = ompi_name_server.get_procid_string(&name)
 * @endcode
 */
typedef char* (*mca_ns_get_procid_string_t)(ompi_process_name_t *name);

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
typedef char* (*mca_ns_get_jobid_string_t)(ompi_process_name_t *name);

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
typedef char* (*mca_ns_get_cellid_string_t)(ompi_process_name_t *name);

/**
 * Get the process id as an ompi_process_id_t value.
 * The get_procid() function returns the process id in an ompi_process_id_t representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name to be
 * "translated" to a string.
 *
 * @retval procid The process id field of the provided name. There currently
 * is no error indication that this function failed.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * procid = ompi_name_server.get_procid(&name)
 * @endcode
 */
typedef ompi_process_id_t (*mca_ns_get_procid_t)(ompi_process_name_t *name);

/**
 * Get the job id as an ompi_process_id_t value.
 * The get_jobid() function returns the job id in an ompi_process_id_t representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name to be
 * "translated" to a string.
 *
 * @retval jobid The job id field of the provided name. There currently
 * is no error indication that this function failed.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * jobid = ompi_name_server.get_jobid(&name)
 * @endcode
 */
typedef ompi_process_id_t (*mca_ns_get_jobid_t)(ompi_process_name_t *name);

/**
 * Get the cell id as an ompi_process_id_t value.
 * The get_cellid() function returns the cell id in an ompi_process_id_t representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name to be
 * "translated" to a string.
 *
 * @retval cellid The cell id field of the provided name. There currently
 * is no error indication that this function failed.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * cellid = ompi_name_server.get_cellid(&name)
 * @endcode
 */
typedef ompi_process_id_t (*mca_ns_get_cellid_t)(ompi_process_name_t *name);

/**
 * Compare two name values.
 * The compare() function checks the value of the process id fields ONLY in the two
 * provided names, and returns a value indicating if the first one is less than, greater
 * than, or equal to the second.
 *
 * @param *name1 A pointer to the first name structure.
 * @param *name2 A pointer to the second name structure.
 *
 * @retval -1 The process id field of the first provided name is less than the process id
 * field of the second provided name.
 * @retval 0 The process id fields of the two provided names are equal.
 * @retval +1 The process id field of the first provided name is greater than the process id
 * field of the second provided name.
 *
 * There currently
 * is no error indication that this function failed.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * result = ompi_name_server.compare(&name1, &name2)
 * @endcode
 */
typedef int (*mca_ns_compare_t)(ompi_process_name_t *name1, ompi_process_name_t *name2);

/*
 * Ver 1.0.0
 */
struct mca_ns_base_module_1_0_0_t {
    mca_base_module_t nsc_version;
    mca_base_module_data_1_0_0_t nsc_data;
#if 0
    mca_ns_base_init_fn_t nsc_init;
    mca_ns_base_finalize_fn_t nsc_finalize;
#endif
};
typedef struct mca_ns_base_module_1_0_0_t mca_ns_base_module_1_0_0_t;

struct mca_ns_1_0_0_t {
    mca_ns_create_cellid_t create_cellid;
    mca_ns_create_jobid_t create_jobid;
    mca_ns_create_proc_name_t create_process_name;
    mca_ns_free_name_t free_name;
    mca_ns_get_proc_name_string_t get_proc_name_string;
    mca_ns_get_procid_string_t get_procid_string;
    mca_ns_get_jobid_string_t get_jobid_string;
    mca_ns_get_cellid_string_t get_cellid_string;
    mca_ns_get_procid_t get_procid;
    mca_ns_get_jobid_t get_jobid;
    mca_ns_get_cellid_t get_cellid;
    mca_ns_compare_t compare;
};
typedef struct mca_ns_1_0_0_t mca_ns_1_0_0_t;

typedef mca_ns_base_module_1_0_0_t mca_ns_base_module_t;
typedef mca_ns_1_0_0_t mca_ns_t;


/*
 * Macro for use in modules that are of type ns v1.0.0
 */
#define MCA_NS_BASE_VERSION_1_0_0 \
  /* ns v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* ns v1.0 */ \
  "ns", 1, 0, 0

#endif
