/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_environ.h"

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/util/pre_condition_transports.h"

#include <unistd.h>
#include <sys/types.h> 
#include <fcntl.h>
/* some network transports require a little bit of information to
 * "pre-condition" them - i.e., to setup their individual transport
 * connections so they can generate their endpoint addresses. This
 * function provides a means for doing so. The resulting info is placed
 * into the app_context's env array so it will automatically be pushed
 * into the environment of every MPI process when launched.
 */

int orte_pre_condition_transports(orte_app_context_t **app_context, size_t num_context)
{
    size_t i;
    char **env;
    char *cs_env;
    int fd_rand;
    uint64_t unique_key[2];
    char string_key[ORTE_TRANSPORT_KEY_LEN + 1]; /* key + null */
	
    size_t bytes_read; 
    
    /* put the number here - or else create an appropriate string. this just needs to
     * eventually be a string variable
     */
    
    fd_rand = open("/dev/random", O_RDONLY);
    bytes_read = read(fd_rand, (char *) unique_key, 16);
    if(bytes_read != 16) {
        opal_output(0,"Open MPI error reading /dev/random for transport pre-conditioning\n");
    }
    close(fd_rand);

    sprintf(string_key, ORTE_TRANSPORT_KEY_FMT, unique_key[0], unique_key[1]);
    string_key[sizeof string_key - 1] = '\0';
    
    if (NULL == (cs_env = mca_base_param_environ_variable("orte_precondition_transports",NULL,NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    for (i=0; i < num_context; i++) {
	opal_setenv(cs_env, string_key, true, &app_context[i]->env);
    }

    free(cs_env);

    return ORTE_SUCCESS;
}
