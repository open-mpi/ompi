/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "util/univ_info.h"

orte_universe_t orte_universe_info = {
    /* .init =                */    false,
    /* .path =                */    NULL,
    /* .name =                */    NULL,
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
    
    if (!orte_universe_info.init) {
        id = mca_base_param_register_string("universe", "path", NULL, NULL, NULL);
        mca_base_param_lookup_string(id, &(orte_universe_info.path));
    
        id = mca_base_param_register_string("universe", "name", NULL, NULL, "default-universe");
        mca_base_param_lookup_string(id, &(orte_universe_info.name));
    
        id = mca_base_param_register_string("universe", "host", NULL, NULL, NULL);
        mca_base_param_lookup_string(id, &(orte_universe_info.host));
    
        /* uid is not set via parameter, but is determined elsewhere */
        
        id = mca_base_param_register_int("universe", "persistence", NULL, NULL, (int)false);
        mca_base_param_lookup_int(id, &tmp);
        if (tmp) {
            orte_universe_info.persistence = true;
        } else {
            orte_universe_info.persistence = false;
        }
    
        id = mca_base_param_register_string("universe", "scope", NULL, NULL, NULL);
        mca_base_param_lookup_string(id, &(orte_universe_info.scope));
    
        id = mca_base_param_register_int("universe", "console", NULL, NULL, (int)false);
        mca_base_param_lookup_int(id, &tmp);
        if (tmp) {
            orte_universe_info.console = true;
        } else {
            orte_universe_info.console = false;
        }
    
        id = mca_base_param_register_string("universe", "uri", NULL, NULL, NULL);
        mca_base_param_lookup_string(id, &(orte_universe_info.seed_uri));
    
        /* console connected is set elsewhere */
        
        id = mca_base_param_register_string("universe", "script", NULL, NULL, NULL);
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
