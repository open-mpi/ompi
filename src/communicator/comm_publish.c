/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
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
#include "mca/ns/ns.h"
#include "mca/gpr/base/base.h"
#include "mca/gpr/gpr.h"

static ompi_mutex_t ompi_port_lock;
static int port_id=MCA_OOB_TAG_USER;

int ompi_open_port(char *port_name)
{
    ompi_proc_t **myproc=NULL;
    char *name=NULL;
    size_t size=0;
    int lport_id=-1;

    /*
     * The port_name is equal to the OOB-contact information
     * and an integer. The reason for adding the integer is
     * to make the port unique for multi-threaded scenarios.
     */
  
    myproc = ompi_proc_self (&size);
    name = ompi_name_server.get_proc_name_string (&(myproc[0]->proc_name));

    OMPI_THREAD_LOCK(&ompi_port_lock);
    lport_id = port_id++;
    OMPI_THREAD_UNLOCK(&ompi_port_lock);

    sprintf (port_name, "%s:%d", name, lport_id);
    free ( myproc );
    free ( name );
    
    return OMPI_SUCCESS;
}

/* takes a port_name and separates it into the process_name 
   and the tag
*/
char *ompi_parse_port (char *port_name, int *tag) 
{
    char tmp_port[MPI_MAX_PORT_NAME], *tmp_string;

    tmp_string = (char *) malloc (MPI_MAX_PORT_NAME);
    if (NULL ==  tmp_string ) {
	return NULL;
    }

    strncpy (tmp_port, port_name, MPI_MAX_PORT_NAME);
    strncpy (tmp_string, strtok(tmp_port, ":"), MPI_MAX_PORT_NAME);
    sscanf( strtok(NULL, ":"),"%d", tag);
    
    return tmp_string;
}

/* 
 * publish the port_name using the service_name as a token
 * jobid and vpid are used later to make
 * sure, that only this process can unpublish the information.
 */
int ompi_comm_namepublish ( char *service_name, char *port_name ) 
{

    char *key[2];
    
    key[0] = service_name;
    key[1] = NULL;
    return ompi_registry.put(OMPI_REGISTRY_OVERWRITE, "ompi_name_publish", 
			     key, port_name, (strlen(port_name)+1));
}

char* ompi_comm_namelookup ( char *service_name )
{
    char *key[2];
    ompi_list_t *tmp=NULL;
    ompi_registry_value_t *vtmp=NULL;
    char *stmp=NULL, *stmp2=NULL;

    key[0] = service_name;
    key[1] = NULL;
    tmp = ompi_registry.get(OMPI_REGISTRY_NONE, "ompi_name_publish", key);
    if ( NULL != tmp ) {
        vtmp = (ompi_registry_value_t *) ompi_list_get_first(tmp);
	if (NULL != vtmp) {
	    stmp  = (char *)vtmp->object;
	    if ( NULL != stmp) {
		stmp2 = strdup(stmp);
		OBJ_RELEASE(vtmp);
	    }
}
	OBJ_RELEASE(tmp);
    }

    return (stmp2);
}

/* 
 * delete the entry. Just the process who has published
 * the service_name, has the right to remove this 
 * service. Will be done later, by adding jobid and vpid
 * as tokens
 */
int ompi_comm_nameunpublish ( char *service_name )
{
    char *key[2];
    key[0] = service_name;
    key[1] = NULL;
    return ompi_registry.delete_object(OMPI_REGISTRY_NONE, "ompi_name_publish", key); 
}
