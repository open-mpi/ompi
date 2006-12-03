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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/opal_environ.h"

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/util/pre_condition_transports.h"

/* some network transports require a little bit of information to
 * "pre-condition" them - i.e., to setup their individual transport
 * connections so they can generate their endpoint addresses. This
 * function provides a means for doing so. The resulting info is placed
 * into the app_context's env array so it will automatically be pushed
 * into the environment of every MPI process when launched.
 */

static inline void orte_pre_condition_transports_use_rand(uint64_t* unique_key) { 
    srand((unsigned int)time(NULL));
    unique_key[1] = rand();
    unique_key[2] = rand();
}

int orte_pre_condition_transports(orte_app_context_t **app_context, size_t num_context)
{
    size_t i;
    char *cs_env;
    uint64_t unique_key[2];
    char string_key[ORTE_TRANSPORT_KEY_LEN + 1]; /* key + null */
	
#if !defined(__WINDOWS__)
    int fd_rand;
    size_t bytes_read; 
    struct stat buf;

    /* put the number here - or else create an appropriate string. this just needs to
     * eventually be a string variable
     */
    if(0 != stat("/dev/urandom", &buf)) { 
        /* file doesn't exist! */
        orte_pre_condition_transports_use_rand(unique_key); 
    }
        
    if(-1 == (fd_rand = open("/dev/urandom", O_RDONLY))) {
        orte_pre_condition_transports_use_rand(unique_key); 
    } else { 
        bytes_read = read(fd_rand, (char *) unique_key, 16);
        if(bytes_read != 16) {
            orte_pre_condition_transports_use_rand(unique_key); 
        } else { 
            close(fd_rand);
        }
    }
#else
    {
        unsigned int random_value;
        rand_s( &random_value );
        unique_key[0] = (uint64_t)random_value;
        rand_s( &random_value );
        unique_key[1] = (uint64_t)random_value;
    }
#endif  /* !defined(__WINDOWS__) */

    sprintf(string_key, ORTE_TRANSPORT_KEY_FMT, (long long unsigned)unique_key[0], 
            (long long unsigned)unique_key[1]);
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
