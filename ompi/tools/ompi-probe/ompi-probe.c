/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>

#include "opal/dss/dss.h"
#include "opal/mca/base/base.h"
#include "opal/util/opal_environ.h"
#include "opal/runtime/opal.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"

/*
 * Globals
 */

int main(int argc, char *argv[])
{
    char * tmp_env_var = NULL;
    char *rml_uri;
    opal_buffer_t buffer;
    char *attr = "oob.tcp";
    int32_t len;
    int rc;

    /* init enough of opal to use a few utilities */
    if (OPAL_SUCCESS != opal_init_util()) {
        fprintf(stderr, "OPAL failed to initialize -- ompi-probe aborting\n");
        exit(1);
    }
    
#if OPAL_ENABLE_FT == 1
    /* Disable the checkpoint notification routine for this
     * tool. As we will never need to checkpoint this tool.
     * Note: This must happen before opal_init().
     */
    opal_cr_set_enabled(false);
    
    /* Select the none component, since we don't actually use a checkpointer */
    tmp_env_var = mca_base_param_env_var("crs");
    opal_setenv(tmp_env_var,
                "none",
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    /* Mark as a tool program */
    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "1",
                true, &environ);
    free(tmp_env_var);
#endif
    tmp_env_var = NULL; /* Silence compiler warning */

    /* open up and select all the frameworks - this will generate the
     * profiled output
     */
    MPI_Init(NULL, NULL);
    
    /* get our RML uri */
    rml_uri = orte_rml.get_contact_info();
    
    if (NULL != rml_uri) {
        char *ptr, *endip, *ipout=NULL, *tmp;
        endip = rml_uri;
        /* remove the non-IP info */
        while (NULL != (ptr = strchr(endip, '/'))) {
            /* next position is the second '/' */
            ptr += 2;
            /* now look for ':' */
            endip = strchr(ptr, ':');
            if (NULL == endip) {
                /* got an error - just dump this */
                free(rml_uri);
                goto CLEANUP;
            }
            *endip = '\0';
            if (NULL == ipout) {
                ipout = strdup(ptr);
            } else {
                asprintf(&tmp, "%s:%s", ipout, ptr);
                free(ipout);
                ipout = tmp;
            }
            ptr = endip + 1;
        }
        /* send the result to the HNP*/
        OBJ_CONSTRUCT(&buffer, opal_buffer_t);
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &orte_process_info.nodename, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto skip;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &attr, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            goto skip;
        }
        len = strlen(ipout);
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &len, 1, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            goto skip;
        }
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &ipout, len, OPAL_BYTE))) {
            ORTE_ERROR_LOG(rc);
            goto skip;
        }
        orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_GRPCOMM_PROFILE, 0);
    skip:
        OBJ_DESTRUCT(&buffer);
        /* cleanup */
        free(rml_uri);
    }
   
CLEANUP:
    /* Finalize and clean up ourselves */
    MPI_Finalize();
    
    return 0;
}
