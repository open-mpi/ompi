/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>
#include "mpi.h"

#include "communicator/communicator.h"
#include "proc/proc.h"
#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pml/pml.h"

#ifndef HAVE_REGISTRY
/* just to keep the linker happy */
int ompi_comm_namepublish ( char *service_name, char *port_name ) 
{
    return OMPI_SUCCESS;
}
char* ompi_comm_namelookup ( char *service_name )
{
    return NULL;
}
int ompi_comm_nameunpublish ( char *service_name )
{
    return OMPI_SUCCESS;
}
#else
#include "mca/gpr/gpr.h"

/* 
 * publish the port_name using the service_name as a token
 * jobid and vpid are used later to make
 * sure, that only this process can unpublish the information.
 */
int ompi_comm_namepublish ( char *service_name, char *port_name ) 
{
    return (ompi_registry_put((uint8_t *)port_name, strlen(service_name), 
                              "universe", service_name, NULL));
}

char* ompi_comm_namelookup ( char *service_name )
{
    ompi_registry_value_t *tmp;
    char *stmp=NULL;

    tmp = ompi_registry_get("universe", service_name, NULL);
    if ( NULL != tmp ) {
        stmp = (char*)tmp->object;
    }

    return (stmp);
}

/* 
 * delete the entry. Just the process who has published
 * the service_name, has the right to remove this 
 * service. Will be done later, by adding jobid and vpid
 * as tokens
 */
int ompi_comm_nameunpublish ( char *service_name )
{
    return (ompi_registry_del("universe", service_name, NULL)); 
}

#endif
