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

/*
 * includes
 */
#include <sys/types.h>
#include <stdint.h>


#include "ompi_config.h"

/*
 * typedefs
 */

typedef uint64_t ompi_process_name_t;

/**
 * Obtain a single new process name.
 * The ompi_process_name_new() function obtains a single new process name.
 *
 * @return name An ompi_process_name_t value of the name. A value of
 * 0 indicates that the name server is out of names.
 */

ompi_process_name_t ompi_process_name_new(void);

/**
 * Obtain a range of process names.
 * The ompi_process_name_get_range() function requests reservation of a range of
 * names. 
 *
 * @return name An ompi_process_name_t value of the name. A value of
 * 0 indicates that the name server is out of names.
 */

ompi_process_name_t ompi_process_name_get_range(ompi_process_name_t range);

/**
 * Releases a process name.
 * The ompi_process_name_free() function will release a name for re-use. At this
 * time, this function does nothing!! It will simply return OMPI_SUCCESS. This is
 * here solely to reserve the function for further definition.
 *
 * @param name An ompi_process_name_t value of the name to be released.
 * @return OMPI_SUCCESS At this time, this is always returned.
 */

int ompi_process_name_free(ompi_process_name_t name);

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

int ompi_process_name_free_range(ompi_process_name_t name, ompi_process_name_t range);

/**
 * Convert the process name to a string.
 *
 * In a number of places within Open MPI (e.g., the General Purpose Registry), it
 * is helpful/required that the process name be treated as a string. This function
 * converts the name into a string by expressing the name in hex.
 *
 * @param name The ompi_process_name_t value to be converted.
 *
 * @return name_string The name converted to a string expressed in hex format.
 *
 */

char *ompi_convert_process_name_to_string(ompi_process_name_t name);
