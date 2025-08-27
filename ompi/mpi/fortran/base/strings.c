/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2025      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "opal/util/argv.h"
#include "opal/util/string_copy.h"

#include "ompi/constants.h"
#include "ompi/mpi/fortran/base/fortran_base_strings.h"


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

    if (NULL == (*cstr = (char *) malloc(len + 1))) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Copy F77 string into C string and NULL terminate it. */

    if (len > 0) {
        opal_string_copy(*cstr, fstr, len + 1);
    } else {
        (*cstr)[0] = '\0';
    }

    return OMPI_SUCCESS;
}


/*
 * Copy a C string into a Fortran string.  Note that when Fortran
 * copies strings, even if it operates on subsets of the strings, it
 * is expected to zero out the rest of the string with spaces.  Hence,
 * when calling this function, the "len" parameter should be the
 * compiler-passed length of the entire string, even if you're copying
 * over less than the full string.  Specifically:
 *
 * http://www.ibiblio.org/pub/languages/fortran/ch2-13.html
 *
 * "Whole operations 'using' only 'part' of it, e.g. assignment of a
 * shorter string, or reading a shorter record, automatically pads the
 * rest of the string with blanks."
 */
int ompi_fortran_string_c2f(const char *cstr, char *fstr, int len)
{
    int i;

    opal_string_copy(fstr, cstr, len);

    // If len < len(cstr), then opal_string_copy() will have copied a
    // trailing \0 into the last position in fstr.  This is not what
    // Fortran wants; overwrite that \0 with the actual last character
    // that will fit into fstr.
    if (len <= strlen(cstr)) {
        fstr[len - 1] = cstr[len - 1];
    } else {
        // Otherwise, pad the end of the resulting Fortran string with
        // spaces.
        for (i = strlen(cstr); i < len; ++i) {
            fstr[i] = ' ';
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Creates a C argument vector from an F77 array of strings.  The
 * array is terminated by a blank string.
 *
 * This function is quite similar to ompi_fortran_argv_count_f2c(),
 * that it looks for a blank string to know when it has finished
 * traversing the entire array (vs. having the length of the array
 * passed in as a parameter).
 *
 * This function is used to convert "argv" in MPI_COMM_SPAWN (which is
 * defined to be terminated by a blank string).
 */
int ompi_fortran_argv_blank_f2c(char *array, int string_len, int advance,
                                char ***argv)
{
    int err, argc = 0;
    char *cstr;

    /* Fortran lines up strings in memory, each delimited by \0.  So
       just convert them until we hit an extra \0. */

    *argv = NULL;
    while (1) {
	if (OMPI_SUCCESS != (err = ompi_fortran_string_f2c(array, string_len,
                                                           &cstr))) {
	    opal_argv_free(*argv);
	    return err;
	}

	if ('\0' == *cstr) {
	    break;
	}

	if (OMPI_SUCCESS != (err = opal_argv_append(&argc, argv, cstr))) {
	    opal_argv_free(*argv);
            free(cstr);
	    return err;
	}

	free(cstr);
	array += advance;
    }

    free(cstr);
    return OMPI_SUCCESS;
}


/*
 * Creates a C argument vector from an F77 array of array_len strings.
 *
 * This function is quite similar to ompi_fortran_argv_blank_f2c(),
 * except that the length of the array is a parameter (vs. looking for
 * a blank line to end the array).
 *
 * This function is used to convert "array_of_commands" in
 * MPI_COMM_SPAWN_MULTIPLE (which is not precisely defined, but is
 * assumed to be of length "count", and *not* terminated by a blank
 * line).
 */
int ompi_fortran_argv_count_f2c(char *array, int array_len, int string_len, int advance,
                                char ***argv)
{
    int err, argc = 0;
    char *cstr;

    /* Fortran lines up strings in memory, each delimited by \0.  So
       just convert them until we hit an extra \0. */

    *argv = NULL;
    for (int i = 0; i < array_len; ++i) {
	if (OMPI_SUCCESS != (err = ompi_fortran_string_f2c(array, string_len,
                                                           &cstr))) {
	    opal_argv_free(*argv);
	    return err;
	}

	if (OMPI_SUCCESS != (err = opal_argv_append(&argc, argv, cstr))) {
	    opal_argv_free(*argv);
            free(cstr);
	    return err;
	}

	free(cstr);
	array += advance;
    }

    return OMPI_SUCCESS;
}


/*
 * Creates a set of C argv arrays from an F77 array of argv's (where
 * each argv array is terminated by a blank string).  The returned
 * arrays need to be freed by the caller.
 */
int ompi_fortran_multiple_argvs_f2c(int num_argv_arrays, char *array,
                                    int string_len, char ****argv)
{
    char ***argv_array;
    int i;
    char *current_array = array;
    int ret;

    argv_array = (char ***) malloc (num_argv_arrays * sizeof(char **));

    for (i = 0; i < num_argv_arrays; ++i) {
        ret = ompi_fortran_argv_blank_f2c(current_array, string_len,
                                          string_len * num_argv_arrays,
                                          &argv_array[i]);
        if (OMPI_SUCCESS != ret) {
            free(argv_array);
            return ret;
        }
        current_array += string_len;
    }
    *argv = argv_array;
    return OMPI_SUCCESS;
}
