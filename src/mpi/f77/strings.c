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

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>

#include "include/constants.h"
#include "util/argv.h"
#include "mpi/f77/strings.h"


/*
 * creates a C string from an F77 string
 */
int ompi_fortran_string_f2c(char *fstr, int len, char **cstr)
{
    char *end;
    int i;

    /* Leading and trailing blanks are discarded. */

    end = fstr + len - 1;

    for (i = 0; (i < len) && (' ' == *fstr); ++i, ++fstr) {
        continue;
    }

    if (i >= len) {
	len = 0;
    } else {
	for (; (end > fstr) && (' ' == *end); --end) {
            continue;
        }

	len = end - fstr + 1;
    }

    /* Allocate space for the C string. */

    if (NULL == (*cstr = malloc(len + 1))) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Copy F77 string into C string and NULL terminate it. */

    if (len > 0) {
	strncpy(*cstr, fstr, len);
    }
    (*cstr)[len] = '\0';

    return OMPI_SUCCESS;
}


/*
 * copy a C string into a Fortran string
 */
int ompi_fortran_string_c2f(char *cstr, char *fstr, int len)
{
    int i;

    strncpy(fstr, cstr, len);
    for (i = strlen(cstr); i < len; ++i) {
	fstr[i] = ' ';
    }

    return OMPI_SUCCESS;
}


/*
 * creates a C argument vector from an F77 array of strings
 * (terminated by a blank string)
 */
int ompi_fortran_argv_f2c(char *array, int len, char ***argv)
{
    int err, argc = 0;
    char *cstr;

    /* Fortran lines up strings in memory, each delimited by \0.  So
       just convert them until we hit an extra \0. */

    *argv = NULL;
    while (1) {
	if (OMPI_SUCCESS != (err = ompi_fortran_string_f2c(array, len, 
                                                           &cstr))) {
	    ompi_argv_free(*argv);
	    return err;
	}

	if ('\0' == *cstr) {
	    break;
	}

	if (OMPI_SUCCESS != (err = ompi_argv_append(&argc, argv, cstr))) {
	    ompi_argv_free(*argv);
	    return err;
	}

	free(cstr);
	array += len;
    }

    return OMPI_SUCCESS;
}


/*
 * creates a C argument vector from an F77 array of argvs. The
 * returned array needs to be freed by the caller
 */
int ompi_fortran_multiple_argvs_f2c(int count, char *array, int len,
				    char ****argv)
{
    char ***argv_array;
    int i;
    char *current_array = array;
    int ret;

    argv_array = (char ***) malloc (count * sizeof(char **));

    for (i = 0; i < count; ++i) {
	ret = ompi_fortran_argv_f2c(current_array, len, &argv_array[i]);
	if (OMPI_SUCCESS != ret) {
	    return ret;
	}
	current_array += len * i;
    }
    *argv = argv_array;
    return OMPI_SUCCESS;
}
