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
#include "ompi_config.h"
#include "include/constants.h"

#include "mca/ns/base/base.h"
#include "mca/ns/ns.h"

/*
 * defines
 */
#define OMPI_SUCCESS 1

/**
 * globals
 */
ompi_process_id_t ompi_name_service = 0;
ompi_process_id_t OMPI_NAME_SERVICE_MAX = 0xffff;

ompi_process_id_t ompi_process_name_new(void)
{
    if (OMPI_NAME_SERVICE_MAX > ompi_name_service) {
	ompi_name_service = ompi_name_service + 1;
	return(ompi_name_service);
    } else {
	return(0);
    }
}

ompi_process_id_t ompi_process_name_get_range (ompi_process_id_t range)
{
    if ((OMPI_NAME_SERVICE_MAX-range) > ompi_name_service) {
	ompi_name_service = ompi_name_service + range;
	return(ompi_name_service);
    } else {
	return(0);
    }
}

int ompi_process_name_free(ompi_process_id_t name)
{
    return OMPI_SUCCESS;
}

int ompi_process_name_free_range(ompi_process_id_t name, ompi_process_id_t range)
{
    return OMPI_SUCCESS;
}

char *ompi_convert_process_name_to_string(ompi_process_id_t name)
{
    char * name_string;
    uint32_t *name32;

    name32 = (uint32_t*) &name;
    sprintf(name_string, "%x%x", name32[0], name32[1]);
    return(name_string);
}
 
