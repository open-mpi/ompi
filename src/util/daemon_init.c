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
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/stat.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>

#include "include/orte_constants.h"
#include "util/daemon_init.h"

int orte_daemon_init(char *working_dir)
{
#ifndef WIN32
    /* it seems that there is an entirely different way to write daemons in 
       WINDOWS land. Firstly, they are called services and the way to 
       go about it is to get a service handle annd then call CreateService()
       So, I am guessing that this piece of code is called only by UNIX versions */

    pid_t pid;

    if ((pid = fork()) < 0) {
	return ORTE_ERROR;
    } else if (pid != 0) {
	exit(0);   /* parent goes bye-bye */
    }
    /* child continues */
    setsid();  /* become session leader */

    if (NULL != working_dir) {
	chdir(working_dir);  /* change working directory */
    }

    umask(0);  /* clear file mode creation mask */
    return ORTE_SUCCESS;
#else
    printf ("This function has not been implemented in windows yet, file %s line %d\n", __FILE__, __LINE__);
    abort();
#endif
}
