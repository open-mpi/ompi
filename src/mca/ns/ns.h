/*
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
#include <sys/types.h>
#include <stdint.h>
#include <limits.h>

#include "ompi_config.h"
#include "include/types.h"
#include "class/ompi_list.h"

#include "mca/mca.h"


/*
 * useful defines for bit-masks
 */
#define OMPI_NS_CMP_CELLID     0x01
#define OMPI_NS_CMP_JOBID      0x02
#define OMPI_NS_CMP_VPID       0x04
#define OMPI_NS_CMP_ALL        0Xff

/*
 * define maximum value for id's in any field
 */
#define OMPI_NAME_SERVICE_MAX UINT32_MAX

/*
 * general typedefs & structures
 */
/**< Set the allowed range for ids in each space */
typedef uint32_t mca_ns_base_jobid_t;
typedef uint32_t mca_ns_base_cellid_t;
typedef uint32_t mca_ns_base_vpid_t;
typedef uint8_t ompi_ns_cmp_bitmask_t;  /**< Bit mask for comparing process names */

struct ompi_process_name_t {
    mca_ns_base_cellid_t cellid;  /**< Cell number */
    mca_ns_base_jobid_t jobid; /**< Job number */
    mca_ns_base_vpid_t vpid;  /**< Process number */
};
typedef struct ompi_process_name_t ompi_process_name_t;

/* declare the class */
OBJ_CLASS_DECLARATION(ompi_process_name_t);

/*
 * define the command names/ids for use in OOB buffers.
 * only needed for remotely executed commands.
 */
#define OMPI_NS_CREATE_CELLID     0x01
#define OMPI_NS_CREATE_JOBID      0x02
#define OMPI_NS_RESERVE_RANGE     0x04
#define OMPI_NS_FREE_NAME         0x08

typedef uint8_t ompi_ns_cmd_bitmask_t;

/*
 * define the actual name server message buffer
 */

struct ompi_ns_msg_buffer_t {
    ompi_ns_cmd_bitmask_t command;
    int buflen;
    uint8_t *buf;
};
typedef struct ompi_ns_msg_buffer_t ompi_ns_msg_buffer_t;

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
typedef mca_ns_base_cellid_t (*mca_ns_create_cellid_fn_t)(void);

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
typedef mca_ns_base_jobid_t (*mca_ns_create_jobid_fn_t)(void);

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
typedef ompi_process_name_t* (*mca_ns_create_proc_name_fn_t)(mca_ns_base_cellid_t cell, mca_ns_base_jobid_t job, mca_ns_base_vpid_t vpid);


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
 * @retval startid The starting value of the reserved range of vpid's. At this time,
 * no means for returning an error condition is available. This will be rectified in the
 * near future.
 *
 * @code
 * starting_procid = ompi_name_server.reserve_range(jobid, range)
 * @endcode
 */
typedef mca_ns_base_vpid_t (*mca_ns_reserve_range_fn_t)(mca_ns_base_jobid_t job, mca_ns_base_vpid_t range);


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
typedef int (*mca_ns_free_name_fn_t)(ompi_process_name_t *name);

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
typedef char* (*mca_ns_get_proc_name_string_fn_t)(const ompi_process_name_t *name);

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
typedef char* (*mca_ns_get_vpid_string_fn_t)(const ompi_process_name_t *name);

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
typedef char* (*mca_ns_get_jobid_string_fn_t)(const ompi_process_name_t *name);

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
typedef char* (*mca_ns_get_cellid_string_fn_t)(const ompi_process_name_t *name);

/**
 * Get the virtual process id as an ompi_process_id_t value.
 * The get_vpid() function returns the vpid in an ompi_process_id_t representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name.
 *
 * @retval vpid The vpid field of the provided name. There currently
 * is no error indication that this function failed.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * vpid = ompi_name_server.get_vpid(&name)
 * @endcode
 */
typedef mca_ns_base_vpid_t (*mca_ns_get_vpid_fn_t)(const ompi_process_name_t *name);

/**
 * Get the job id as an ompi_process_id_t value.
 * The get_jobid() function returns the job id in an ompi_process_id_t representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name.
 *
 * @retval jobid The job id field of the provided name. There currently
 * is no error indication that this function failed.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * jobid = ompi_name_server.get_jobid(&name)
 * @endcode
 */
typedef mca_ns_base_jobid_t (*mca_ns_get_jobid_fn_t)(const ompi_process_name_t *name);

/**
 * Get the cell id as an ompi_process_id_t value.
 * The get_cellid() function returns the cell id in an ompi_process_id_t representation -
 * i.e., in an integer form.
 *
 * @param *name A pointer to the name structure containing the name.
 *
 * @retval cellid The cell id field of the provided name. There currently
 * is no error indication that this function failed.
 * Some means of returning a value indicative of an error will be devised in the future.
 *
 * @code
 * cellid = ompi_name_server.get_cellid(&name)
 * @endcode
 */
typedef mca_ns_base_cellid_t (*mca_ns_get_cellid_fn_t)(const ompi_process_name_t *name);

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
typedef int (*mca_ns_compare_fn_t)(ompi_ns_cmp_bitmask_t fields, const ompi_process_name_t *name1, const ompi_process_name_t *name2);

/*
 * Ver 1.0.0
 */
struct mca_ns_1_0_0_t {
    mca_ns_create_cellid_fn_t create_cellid;
    mca_ns_create_jobid_fn_t create_jobid;
    mca_ns_create_proc_name_fn_t create_process_name;
    mca_ns_reserve_range_fn_t reserve_range;
    mca_ns_free_name_fn_t free_name;
    mca_ns_get_proc_name_string_fn_t get_proc_name_string;
    mca_ns_get_vpid_string_fn_t get_vpid_string;
    mca_ns_get_jobid_string_fn_t get_jobid_string;
    mca_ns_get_cellid_string_fn_t get_cellid_string;
    mca_ns_get_vpid_fn_t get_vpid;
    mca_ns_get_jobid_fn_t get_jobid;
    mca_ns_get_cellid_fn_t get_cellid;
    mca_ns_compare_fn_t compare;
};
typedef struct mca_ns_1_0_0_t mca_ns_1_0_0_t;
typedef mca_ns_1_0_0_t mca_ns_t;

/*
 * NS Component
 */

typedef mca_ns_t* (*mca_ns_base_init_fn_t)(
    bool *allow_multi_user_threads,
    bool *have_hidden_threads,
    int *priority);

typedef int (*mca_ns_base_finalize_fn_t)(void);
 
/*
 * the standard component data structure
 */

struct mca_ns_base_component_1_0_0_t {
    mca_base_module_t ns_version;
    mca_base_module_data_1_0_0_t ns_data;
    mca_ns_base_init_fn_t ns_init;
    mca_ns_base_finalize_fn_t ns_finalize;
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

#endif
