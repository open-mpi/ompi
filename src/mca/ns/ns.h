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
#include "mca/ns/base/base.h"

/*
 * typedefs
 */
typedef uint32_t ompi_process_id_t;  /**< Set the allowed range for id's in each space */


/*
 * Component functions - all MUST be provided!
 */

typedef ompi_process_id_t (*mca_ns_create_cellid_t)(void);
typedef ompi_process_id_t (*mca_ns_create_jobid_t)(void);
typedef ompi_process_name_t (*mca_ns_create_proc_name_t)(ompi_process_id_t cell, ompi_process_id_t job);

typedef char* (*mca_ns_get_proc_name_string)(ompi_process_name_t *name);
typedef char* (*mca_ns_get_procid_string)(ompi_process_name_t *name);
typedef char* (*mca_ns_get_jobid_string)(ompi_process_name_t *name);
typedef char* (*mca_ns_get_cellid_string)(ompi_process_name_t *name);

typedef ompi_process_id_t (*mca_ns_get_procid_t)(ompi_process_name_t *name);
typedef ompi_process_id_t (*mca_ns_get_jobid_t)(ompi_process_name_t *name);
typedef ompi_process_id_t (*mca_ns_get_cellid_t)(ompi_process_name_t *name);

typedef int (*mca_ns_compare_t)(ompi_process_name_t *name1, ompi_process_name_t *name2);

/*
 * Ver 1.0.0
 */
struct mca_ns_base_module_1_0_0_t {
    mca_base_module_t nsc_version;
    mca_base_module_data_1_0_0_t nsc_data;

    mca_ns_base_init_fn_t nsc_init;
    mca_ns_base_finalize_fn_t nsc_finalize;
};
typedef struct mca_ns_base_module_1_0_0_t mca_ns_base_module_1_0_0_t;

struct mca_ns_1_0_0_t {
    mca_ns_create_cellid_t create_cellid;
    mca_ns_create_jobid_t create_jobid;
    mca_ns_create_proc_name_t create_process_name;
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

/**
 * Obtain a single new process name.
 * The ompi_process_name_new() function obtains a single new process name.
 *
 * @return name An ompi_process_name_t value of the name. A value of
 * 0 indicates that the name server is out of names for that cell/jobid.
 */

ompi_process_name_t ompi_ns_create_proc_name(bool daemon, ompi_process_id_t cell, ompi_process_id_t jobid);

/**
 * Obtain a range of process names.
 * The ompi_process_name_get_range() function requests reservation of a range of
 * names. 
 *
 * @return name An ompi_process_name_t value of the name. A value of
 * 0 indicates that the name server is out of names.
 */

ompi_process_name_t ompi_ns_reserve_name_range(bool daemon, ompi_process_id_t cell, ompi_process_id_t jobid, ompi_process_id_t range);

/**
 * Releases a process name.
 * The ompi_process_name_free() function will release a name for re-use. At this
 * time, this function does nothing!! It will simply return OMPI_SUCCESS. This is
 * here solely to reserve the function for further definition.
 *
 * @param name An ompi_process_name_t value of the name to be released.
 * @return OMPI_SUCCESS At this time, this is always returned.
 */

int ompi_ns_free_name(ompi_process_name_t *name);

/**
 * Release a range of process names.
 * The ompi_process_name_free_range() function releases a range of names for re-use.
 * At this time, this function does nothing!! This is here solely to reserve the
 * function for further definition.
 *
 * @param name An ompi_process_name_t value indicating
 * start of the range being freed.
 * @param range An ompi_process_name_t value indicating how many names are being released.
 *
 * @return OMPI_SUCCESS Always returned at this time.
 */

int ompi_ns_free_name_range(ompi_process_name_t *name, ompi_process_id_t range);

/**
 * Return the process name as a string.
 *
 * In a number of places within Open MPI (e.g., the General Purpose Registry), it
 * is helpful/required that the process name be treated as a string. This function
 * converts the name into a string by expressing the name in hex.
 *
 * @param name Pointer to the ompi_process_name_t structure whose name is desired.
 *
 * @return name_string Pointer to the name as a string expressed in hex format.
 *
 */

char *ompi_get_procid_string(ompi_process_name_t *name);

ompi_process_id_t ompi_get_procid(ompi_process_name_t *name);


/**
 * Return the process name fields as an array of strings.
 *
 * In a number of places within Open MPI (e.g., the General Purpose Registry), it
 * is helpful/required that one or more of the process name's fields be treated
 * as a string. This function
 * converts the name fields into an array of strings by expressing them in hex.
 *
 * @param name Pointer to the ompi_process_name_t structure whose name is desired.
 *
 * @return name_strings Pointer to an array of strings, each representing one field
 * in the name structure expressed in hex format. The array is terminated with a
 * NULL pointer.
 *
 */

char *ompi_get_jobid_string(ompi_process_name_t *name);

ompi_process_id_t ompi_get_jobid(ompi_process_name_t *name);

char *ompi_get_cellid_string(ompi_process_name_t *name);

ompi_process_id_t ompi_get_cellid(ompi_process_name_t *name);

#endif

