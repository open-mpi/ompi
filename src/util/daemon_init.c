/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

#include "include/constants.h"
#include "util/daemon_init.h"

int daemon_init(char *working_dir)
{
    pid_t pid;

    if ((pid = fork()) < 0) {
	return OMPI_ERROR;
    } else if (pid != 0) {
	exit(0);   /* parent goes bye-bye */
    }
    /* child continues */
    setsid();  /* become session leader */

    if (NULL != working_dir) {
	chdir(working_dir);  /* change working directory */
    }

    umask(0);  /* clear file mode creation mask */

    return OMPI_SUCCESS;
}
