/*
 * $HEADER$
 *
 */

#include "ompi_config.h"
#include "include/constants.h"

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>

#include "util/os_path.h"
#include "runtime/universe_connect.h"

int
ompi_universe_connect(char *tmpdir)
{
    char *uri, tmp[100], *oob_info_path;
    FILE *fp;

    if (NULL == tmpdir) { /* protect against errors */
	return(OMPI_ERROR);
    }

    oob_info_path = ompi_os_path(false, tmpdir, "oob", NULL);
    fp = fopen(oob_info_path, "r");
    if (NULL == fp) {
	return(OMPI_ERROR);
    }

    fscanf(fp, "%s", tmp);
    uri = strdup(tmp);
    fclose(fp);
    return(OMPI_SUCCESS);
}
