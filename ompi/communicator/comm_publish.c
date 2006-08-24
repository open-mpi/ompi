/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>

#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rml/rml_types.h"

#if OMPI_HAVE_THREAD_SUPPORT
static opal_mutex_t ompi_port_lock;
#endif  /* OMPI_HAVE_THREAD_SUPPORT */

#define OMPI_COMM_PORT_KEY  "ompi-port-name"


int ompi_open_port(char *port_name)
{
    ompi_proc_t **myproc=NULL;
    char *name=NULL;
    size_t size=0;
    orte_rml_tag_t lport_id=0;
    int rc;

    /*
     * The port_name is equal to the OOB-contact information
     * and an integer. The reason for adding the integer is
     * to make the port unique for multi-threaded scenarios.
     */

    myproc = ompi_proc_self (&size);
    if (ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string (&name, &(myproc[0]->proc_name)))) {
        return rc;
    }

    OPAL_THREAD_LOCK(&ompi_port_lock);
    if (ORTE_SUCCESS != (rc = orte_ns.assign_rml_tag(&lport_id, NULL))) {
            OPAL_THREAD_UNLOCK(&ompi_port_lock);
        return rc;
    }
    OPAL_THREAD_UNLOCK(&ompi_port_lock);

    sprintf (port_name, "%s:%d", name, lport_id);
    free ( myproc );
    free ( name );

    return OMPI_SUCCESS;
}

/* takes a port_name and separates it into the process_name
   and the tag
*/
char *ompi_parse_port (char *port_name, orte_rml_tag_t *tag)
{
    char tmp_port[MPI_MAX_PORT_NAME], *tmp_string;

    tmp_string = (char *) malloc (MPI_MAX_PORT_NAME);
    if (NULL ==  tmp_string ) {
       return NULL;
    }

    strncpy (tmp_port, port_name, MPI_MAX_PORT_NAME);
    strncpy (tmp_string, strtok(tmp_port, ":"), MPI_MAX_PORT_NAME);
    sscanf( strtok(NULL, ":"),"%d", (int*)tag);

    return tmp_string;
}

/*
 * publish the port_name using the service_name as a token
 * jobid and vpid are used later to make
 * sure, that only this process can unpublish the information.
 */
int ompi_comm_namepublish ( char *service_name, char *port_name )
{
    orte_gpr_value_t *value;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_TOKENS_AND | ORTE_GPR_OVERWRITE,
                                                    OMPI_NAMESPACE_SEGMENT, 1, 1))) {
       ORTE_ERROR_LOG(rc);
       return rc;
    }
    
    value->tokens[0] = strdup(service_name);

    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), OMPI_COMM_PORT_KEY, ORTE_STRING, port_name))) {
       ORTE_ERROR_LOG(rc);
       OBJ_RELEASE(value);
       return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
       ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(value);
    return rc;
}

char* ompi_comm_namelookup ( char *service_name )
{
    char *token[2], *key[2];
    orte_gpr_keyval_t **keyvals=NULL;
    orte_gpr_value_t **values;
    orte_std_cntr_t cnt=0;
    char *stmp=NULL;
    int ret;

    token[0] = service_name;
    token[1] = NULL;

    key[0] = strdup(OMPI_COMM_PORT_KEY);
    key[1] = NULL;

    ret = orte_gpr.get(ORTE_GPR_TOKENS_AND, OMPI_NAMESPACE_SEGMENT,
                            token, key, &cnt, &values);
    if (ORTE_SUCCESS != ret) {
        return NULL;
    }
    if ( 0 < cnt && NULL != values[0] ) {  /* should be only one, if any */
        keyvals = values[0]->keyvals;
        stmp = strdup((const char*)keyvals[0]->value->data);
        OBJ_RELEASE(values[0]);
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
    char *token[2];

    token[0] = service_name;
    token[1] = NULL;
#if 0
    return orte_gpr.delete_entries(ORTE_GPR_TOKENS_AND,
                                        OMPI_NAMESPACE_SEGMENT,
                                        token, NULL);
#endif
    return OMPI_SUCCESS;
}
