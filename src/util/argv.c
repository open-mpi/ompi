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
#include <stdlib.h>
#include <string.h>

#include "include/constants.h"
#include "util/argv.h"
#include "util/strncpy.h"

#define ARGSIZE 128


/*
 * Append a string to the end of a new or existing argv array.
 */
int ompi_argv_append(int *argc, char ***argv, const char *arg)
{
  /* Create new argv. */

  if (NULL == *argv) {
      *argv = (char**) malloc(2 * sizeof(char *));
    if (NULL == *argv)
      return OMPI_ERROR;
    *argc = 0;
    (*argv)[0] = NULL;
    (*argv)[1] = NULL;
  }

  /* Extend existing argv. */

  else {
      *argv = (char**) realloc(*argv, (*argc + 2) * sizeof(char *));
    if (NULL == *argv)
      return OMPI_ERROR;
  }

  /* Set the newest element to point to a copy of the arg string */

  (*argv)[*argc] = (char*) malloc(strlen(arg) + 1);
  if (NULL == (*argv)[*argc])
    return OMPI_ERROR;

  strcpy((*argv)[*argc], arg);
  *argc = *argc + 1;
  (*argv)[*argc] = NULL;

  return OMPI_SUCCESS;
}


/*
 * Free a NULL-terminated argv array.
 */
void ompi_argv_free(char **argv)
{
  char **p;

  if (NULL == argv)
    return;

  for (p = argv; NULL != *p; ++p) {
    free(*p);
  }

  free(argv);
}


/*
 * Split a string into a NULL-terminated argv array.
 */
char **ompi_argv_split(const char *src_string, int delimiter)
{
  char arg[ARGSIZE];
  char **argv = NULL;
  const char *p;
  char *argtemp;
  int argc = 0;
  size_t arglen;

  while (src_string && *src_string) {
    p = src_string;
    arglen = 0;

    while (('\0' != *p) && (*p != delimiter)) {
      ++p;
      ++arglen;
    }

    /* zero length argument, skip */

    if (src_string == p) {
      ++p;
    }

    /* tail argument, add straight from the original string */

    else if ('\0' == *p) {
      if (OMPI_ERROR == ompi_argv_append(&argc, &argv, src_string))
	return NULL;
    }

    /* long argument, malloc buffer, copy and add */

    else if (arglen > (ARGSIZE - 1)) {
        argtemp = (char*) malloc(arglen + 1);
      if (NULL == argtemp)
	return NULL;

      strncpy(argtemp, src_string, arglen);
      argtemp[arglen] = '\0';

      if (OMPI_ERROR == ompi_argv_append(&argc, &argv, argtemp)) {
	free(argtemp);
	return NULL;
      }

      free(argtemp);
    }

    /* short argument, copy to buffer and add */

    else {
      strncpy(arg, src_string, arglen);
      arg[arglen] = '\0';

      if (OMPI_ERROR == ompi_argv_append(&argc, &argv, arg))
	return NULL;
    }

    src_string = p;
  }

  /* All done */

  return argv;
}


/*
 * Return the length of a NULL-terminated argv array.
 */
int ompi_argv_count(char **argv)
{
  char **p;
  int i;

  if (NULL == argv)
    return 0;

  for (i = 0, p = argv; *p; i++, p++)
    continue;

  return i;
}


/*
 * Join all the elements of an argv array into a single
 * newly-allocated string.
 */
char *ompi_argv_join(char **argv, int delimiter)
{
  char **p;
  char *pp;
  char *str;
  size_t str_len = 0;
  size_t i;

  /* Bozo case */

  if (NULL == argv || NULL == argv[0]) {
      return strdup("");
  }

  /* Find the total string length in argv including delimiters.  The
     last delimiter is replaced by the NULL character. */

  for (p = argv; *p; ++p) {
    str_len += strlen(*p) + 1;
  }

  /* Allocate the string. */

  if (NULL == (str = (char*) malloc(str_len)))
    return NULL;

  /* Loop filling in the string. */

  str[--str_len] = '\0';
  p = argv;
  pp = *p;

  for (i = 0; i < str_len; ++i) {
    if ('\0' == *pp) {

      /* End of a string, fill in a delimiter and go to the next
         string. */

      str[i] = (char) delimiter;
      ++p;
      pp = *p;
    } else {
      str[i] = *pp++;
    }
  }

  /* All done */

  return str;
}


/*
 * Return the number of bytes consumed by an argv array.
 */
size_t ompi_argv_len(char **argv)
{
  char **p;
  size_t length;

  if (NULL == argv)
    return (size_t) 0;

  length = sizeof(char *);

  for (p = argv; *p; ++p) {
    length += strlen(*p) + 1 + sizeof(char *);
  }

  return length;
}


/*
 * Copy a NULL-terminated argv array.
 */
char **ompi_argv_copy(char **argv)
{
  char **dupv = NULL;
  int dupc = 0;

  if (NULL == argv)
    return NULL;

  while (NULL != *argv) {
    if (OMPI_ERROR == ompi_argv_append(&dupc, &dupv, *argv)) {
      ompi_argv_free(dupv);
      return NULL;
    }

    ++argv;
  }

  /* All done */

  return dupv;
}


int ompi_argv_delete(int *argc, char ***argv, int start, int num_to_delete)
{
    int i;
    int count;
    int suffix_count;
    char **tmp;

    /* Check for the bozo cases */

    count = ompi_argv_count(*argv);
    if (NULL == argv || NULL == *argv || start > count || 0 == num_to_delete) {
        return OMPI_SUCCESS;
    } else if (start < 0 || num_to_delete < 0) {
        return OMPI_ERR_BAD_PARAM;
    }

    /* Ok, we have some tokens to delete.  Calculate the new length of
       the argv array. */

    suffix_count = count - (start + num_to_delete);
    if (suffix_count < 0) {
        suffix_count = 0;
    }

    /* Free all items that are being deleted */

    for (i = start; i < count && i < start + num_to_delete; ++i) {
        free((*argv)[i]);
    }

    /* Copy the suffix over the deleted items */

    for (i = start; i < start + suffix_count; ++i) {
        (*argv)[i] = (*argv)[i + num_to_delete];
    }

    /* Add the trailing NULL */

    (*argv)[i] = NULL;

    /* adjust the argv array */
    tmp = realloc(*argv, sizeof(char**) * (i + 1));
    if (NULL != tmp) *argv = tmp;

    return OMPI_SUCCESS;
}


int ompi_argv_insert(char ***target, int start, char **source)
{
    int i, source_count, target_count;
    int suffix_count;

    /* Check for the bozo cases */
    
    if (NULL == target || NULL == *target || start < 0) {
        return OMPI_ERR_BAD_PARAM;
    } else if (NULL == source) {
        return OMPI_SUCCESS;
    }

    /* Easy case: appending to the end */

    target_count = ompi_argv_count(*target);
    source_count = ompi_argv_count(source);
    if (start > target_count) {
        for (i = 0; i < source_count; ++i) {
            ompi_argv_append(&target_count, target, source[i]);
        }
    }

    /* Harder: insertting into the middle */

    else {

        /* Alloc out new space */

        *target = (char**) realloc(*target, 
                                   sizeof(char *) * (target_count + source_count + 1));

        /* Move suffix items down to the end */

        suffix_count = target_count - start;
        for (i = suffix_count - 1; i >= 0; --i) {
            (*target)[start + source_count + i] =
                (*target)[start + i];
        }
        (*target)[start + suffix_count + source_count] = NULL;

        /* Strdup in the source argv */

        for (i = start; i < start + source_count; ++i) {
            (*target)[i] = strdup(source[i - start]);
        }
    }

    /* All done */

    return OMPI_SUCCESS;
}
