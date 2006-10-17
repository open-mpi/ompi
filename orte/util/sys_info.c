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


#include "orte/mca/errmgr/errmgr.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/orte_constants.h"


#include "orte/util/sys_info.h"

orte_sys_info_t orte_system_info = {
                 /* .init =        */            false,
                 /* .sysname =     */            NULL,
	             /* .nodename =    */            NULL,
                 /* .release =     */            NULL,
                 /* .version =     */            NULL,
                 /* .machine =     */            NULL,
                 /* .user =        */            NULL,
                 /* .suffix =      */            NULL};

int orte_sys_info(void)
{
    struct utsname sys_info;

#ifndef __WINDOWS__
    int uid;
	struct passwd *pwdent;
#else
    #define INFO_BUF_SIZE 256
    TCHAR info_buf[INFO_BUF_SIZE];
    DWORD info_buf_length = INFO_BUF_SIZE;
#endif

	if (orte_system_info.init) {
        return ORTE_SUCCESS;
    }

    if (0 > uname(&sys_info)) {  /* have an error - set utsname values to indicate */
       if (NULL != orte_system_info.sysname) {
            free(orte_system_info.sysname);
            orte_system_info.sysname = NULL;
        }
        if (NULL != orte_system_info.nodename) {
            free(orte_system_info.nodename);
            orte_system_info.nodename = NULL;
        }
        if (NULL != orte_system_info.release) {
            free(orte_system_info.release);
            orte_system_info.release = NULL;
        }
        if (NULL != orte_system_info.version) {
            free(orte_system_info.version);
            orte_system_info.version = NULL;
        }
        if (NULL != orte_system_info.machine) {
            free(orte_system_info.machine);
            orte_system_info.machine = NULL;
        }
        return ORTE_ERROR;
    } else {
        orte_system_info.sysname = strdup(sys_info.sysname);
        if (NULL == orte_system_info.nodename) {
            /* make sure we weren't given a nodename by environment */
            int id = mca_base_param_register_string("orte", "base", "nodename",
                                                    NULL, sys_info.nodename);
            mca_base_param_lookup_string(id, &(orte_system_info.nodename));
        }
        orte_system_info.release = strdup(sys_info.release);
        orte_system_info.version = strdup(sys_info.version);
        orte_system_info.machine = strdup(sys_info.machine);
    }

	/* get the name of the user */
#ifndef __WINDOWS__
    uid = getuid();
#ifdef HAVE_GETPWUID
    pwdent = getpwuid(uid);
#else
    pwdent = NULL;
#endif
    if (NULL != pwdent) {
        orte_system_info.user = strdup(pwdent->pw_name);
    } else {
        if (0 > asprintf(&(orte_system_info.user), "%d", uid)) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }
#else 
    if (!GetUserName(info_buf, &info_buf_length)) {
        orte_system_info.user = strdup("unknown");
    } else {
        orte_system_info.user = strdup(info_buf);
    }
#endif

    /* set the init flag */
    orte_system_info.init = true;  /* only indicates that we have been through here once - still have to test for NULL values */

    return(ORTE_SUCCESS);
}

int orte_sys_info_finalize(void)
{
    if (NULL != orte_system_info.sysname) {
        free(orte_system_info.sysname);
        orte_system_info.sysname = NULL;
    }

    if (NULL != orte_system_info.nodename) {
        free(orte_system_info.nodename);
        orte_system_info.nodename = NULL;
    }

    if (NULL != orte_system_info.release) {
        free(orte_system_info.release);
        orte_system_info.release = NULL;
    }

    if (NULL != orte_system_info.version) {
        free(orte_system_info.version);
        orte_system_info.version = NULL;
    }
        
    if (NULL != orte_system_info.machine) {
        free(orte_system_info.machine);
        orte_system_info.machine = NULL;
    }
        
    if (NULL != orte_system_info.user) {
        free(orte_system_info.user);
        orte_system_info.user = NULL;
    }
        
    if (NULL != orte_system_info.suffix) {
        free(orte_system_info.suffix);
        orte_system_info.suffix = NULL;
    }
    
    orte_system_info.init = false;
    
    return ORTE_SUCCESS;
}
