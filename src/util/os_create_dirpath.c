/*
 * $HEADER$
 */


#include "ompi_config.h"

#include <string.h>
#include <unistd.h>
#include <libgen.h>
#include <stdlib.h>

#include "include/constants.h"
#include "util/os_create_dirpath.h"
#include "util/sys_info.h"

int ompi_os_create_dirpath(const char *path, const mode_t mode)
{
    char *pth, *bottom_up, *tmp;
    struct stat buf;

    if (NULL == path) { /* protect ourselves from errors */
	return(OMPI_ERROR);
    }

    if (0 == stat(path, &buf)) { /* already exists */
	if (mode == (mode & buf.st_mode)) { /* has correct mode */
	    return(OMPI_SUCCESS);
	}
	if (0 == chmod(path, (buf.st_mode | mode))) { /* successfully change mode */
	    return(OMPI_SUCCESS);
	}
	return(OMPI_ERROR); /* can't set correct mode */
    }

    /* quick -- try to make directory */
    if (0 == mkdir(path, mode)) {
	return(OMPI_SUCCESS);
    }

    /* didnt work, so now have to build our way down the tree */
    /* ensure system info is valid */
    ompi_sys_info();

    pth = strdup(path); /* make working copy of path */
    if(NULL == pth) {
	return(OMPI_ERROR);
    }

    bottom_up = (char *)malloc(strlen(path)+1); /* create space for bottom_up */
    if (NULL == bottom_up) { /* can't get the space */
	free(pth);
	return(OMPI_ERROR);
    }

    /* start by building bottoms-up tree of directories */
    strcpy(bottom_up, ompi_system_info.path_sep);
    while (strcmp(pth, ".") != 0 && stat(pth, &buf) != 0) { /* see if directory exists, or if we've reached the top */
	strcat(bottom_up, basename(pth));  /* doesn't exist yet, so save this name */
	strcat(bottom_up, ompi_system_info.path_sep);
	tmp = strdup(pth);
	strcpy(pth, dirname(tmp)); /* "pop" the directory tree */
	free(tmp);
    }

    /* okay, ready to build from the top down */
    while (strlen(bottom_up) > 1) {
	strcat(pth, ompi_system_info.path_sep);
	strcat(pth, basename(bottom_up));
	/* try to make the next layer - return error if can't & directory doesn't exist */
	/* if directory already exists, then that means somebody beat us to it - not an error */
	if ((0 != mkdir(pth, mode)) && (stat(pth, &buf) != 0)) { 
	    free(pth);
	    free(bottom_up);
	    return(OMPI_ERROR);
	}
	strcpy(bottom_up, dirname(bottom_up)); /* "pop" the directory tree */
   }

    free(pth);
    free(bottom_up);
    return(OMPI_SUCCESS);
}
