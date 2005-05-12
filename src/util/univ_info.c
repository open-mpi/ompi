/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#include <stdlib.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <sys/stat.h>


#include "include/orte_constants.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/ns/ns_types.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "util/sys_info.h"

#include "util/univ_info.h"

orte_universe_t orte_universe_info = {
    /* .init =                */    false,
    /* .path =                */    NULL,
    /* .name =                */    "default-universe",
    /* .host =                */    NULL,
    /* .uid =                 */    NULL,
    /* .persistence =         */    false,
    /* .scope =               */    NULL,
    /* .console =             */    false,
    /* .seed_uri =            */    NULL,
    /* .console_connected =   */    false,
    /* .scriptfile =          */    NULL,
};


int orte_univ_info(void)
{
    int id, tmp;
    char *tmpname=NULL, *tptr, *ptr;
    
    if (!orte_universe_info.init) {
        id = mca_base_param_register_string("universe", "path", NULL, NULL, orte_universe_info.path);
        mca_base_param_lookup_string(id, &(orte_universe_info.path));
    
        id = mca_base_param_register_string("universe", NULL, NULL, NULL, NULL);
        mca_base_param_lookup_string(id, &tmpname);
        
        if (NULL != tmpname) {            
            /* Universe name info is passed as userid@hostname:univ_name */
            /* extract the userid from the universe option, if provided */
            tptr = tmpname;
            if (NULL != (ptr = strchr(tptr, '@'))) {
                *ptr = '\0';
                orte_universe_info.uid = strdup(tptr);
                ptr++;
                tptr = ptr;
            } else {
                if (NULL == orte_system_info.user) {
                    orte_sys_info();
                }
                orte_universe_info.uid = strdup(orte_system_info.user);
            }
            
            /* extract the hostname, if provided */
            if (NULL != (ptr = strchr(tptr, ':'))) {
                *ptr = '\0';
                orte_universe_info.host = strdup(tptr);
                ptr++;
                tptr = ptr;
            } else {
                orte_universe_info.host = strdup(orte_system_info.nodename);
            }
            
            /* now copy the universe name into the universe_info structure */
            orte_universe_info.name = strdup(tptr);
        } else {
            /* if nothing was provided, then initialize the user and nodename
             * to the local values
             */
            orte_universe_info.uid = strdup(orte_system_info.user);
            orte_universe_info.host = strdup(orte_system_info.nodename);
        }

        id = mca_base_param_register_int("universe", "persistence", NULL, NULL, orte_universe_info.persistence);
        mca_base_param_lookup_int(id, &tmp);
        orte_universe_info.persistence = (tmp ? true : false);
    
        id = mca_base_param_register_string("universe", "scope", NULL, NULL, orte_universe_info.scope);
        mca_base_param_lookup_string(id, &(orte_universe_info.scope));
    
        id = mca_base_param_register_int("universe", "console", NULL, NULL, orte_universe_info.console);
        mca_base_param_lookup_int(id, &tmp);
        orte_universe_info.console = (tmp ? true : false);
    
        id = mca_base_param_register_string("universe", "uri", NULL, NULL, orte_universe_info.seed_uri);
        mca_base_param_lookup_string(id, &(orte_universe_info.seed_uri));
    
        /* console connected is set elsewhere */
        id = mca_base_param_register_string("universe", "script", NULL, NULL, orte_universe_info.scriptfile);
        mca_base_param_lookup_string(id, &(orte_universe_info.scriptfile));

        orte_universe_info.init = true;
    }
    
    return(ORTE_SUCCESS);
}


int orte_univ_info_finalize(void)
{
    if (NULL != orte_universe_info.path) free(orte_universe_info.path);
    
    if (NULL != orte_universe_info.name) free(orte_universe_info.name);
    
    if (NULL != orte_universe_info.host) free(orte_universe_info.host);
    
    if (NULL != orte_universe_info.uid) free(orte_universe_info.uid);
    
    if (NULL != orte_universe_info.scope) free(orte_universe_info.scope);
    
    if (NULL != orte_universe_info.seed_uri) free(orte_universe_info.seed_uri);
    
    if (NULL != orte_universe_info.scriptfile) free(orte_universe_info.scriptfile);
    
    return ORTE_SUCCESS;
}
