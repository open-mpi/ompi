/*
 * $HEADER$
 */

/** @file **/

#include <stdlib.h>
#include <string.h>

#include "lam/constants.h"
#include "lam/mem/malloc.h"
#include "lam/util/argv.h"
#include "lam/util/strncpy.h"

#define ARGSIZE 128

/**
 * Append a string (by value) to an new or existing NULL-terminated
 * argv array.
 *
 * @param argc Pointer to the length of the argv array.  Must not be
 * NULL.
 * @param argv Pointer to an argv array.
 * @param str Pointer to the string to append.
 *
 * @retval LAM_SUCCESS On success
 * @retval LAM_ERROR On failure
 *
 * This function adds a string to an argv array of strings by value;
 * it is permissable to pass a string on the stack as the str argument
 * to this function.
 *
 * To add the first entry to an argv array, call this function with
 * (*argv == NULL).  This function will allocate an array of length 2;
 * the first entry will point to a copy of the string passed in arg,
 * the second entry will be set to NULL.
 *
 * If (*argv != NULL), it will be realloc'ed to be 1 (char*) larger,
 * and the next-to-last entry will point to a copy of the string
 * passed in arg.  The last entry will be set to NULL.
 *
 * Just to reinforce what was stated above: the string is copied by
 * value into the argv array; there is no need to keep the original
 * string (i.e., the arg parameter) after invoking this function.
 */
int
lam_argv_append(int *argc, char ***argv, const char *arg)
{
  /* Create new argv. */

  if (NULL == *argv) {
    *argv = LAM_MALLOC(2 * sizeof(char *));
    if (NULL == *argv)
      return LAM_ERROR;
    *argc = 0;
    (*argv)[0] = NULL;
    (*argv)[1] = NULL;
  }

  /* Extend existing argv. */

  else {
    *argv = realloc(*argv, (*argc + 2) * sizeof(char *));
    if (NULL == *argv)
      return LAM_ERROR;
  }

  /* Set the newest element to point to a copy of the arg string */

  (*argv)[*argc] = LAM_MALLOC(strlen(arg) + 1);
  if (NULL == (*argv)[*argc])
    return LAM_ERROR;

  strcpy((*argv)[*argc], arg);
  *argc = *argc + 1;
  (*argv)[*argc] = NULL;

  return LAM_SUCCESS;
}


/**
 * Free a NULL-terminated argv array.
 *
 * @param argv Argv array to free.
 *
 * This function frees an argv array and all of the strings that it
 * contains.  Since the argv parameter is passed by value, it is not
 * set to NULL in the caller's scope upon return.
 *
 * It is safe to invoke this function with a NULL pointer.  It is not
 * safe to invoke this function with a non-NULL-terminated argv array.
 */
void
lam_argv_free(char **argv)
{
  char **p;

  if (NULL == argv)
    return;

  for (p = argv; NULL != *p; ++p) {
    LAM_FREE(*p);
  }

  LAM_FREE(argv);
}


/**
 * Split a string into a NULL-terminated argv array.
 *
 * @param src_string Input string.
 * @param delimiter Delimiter character.
 *
 * @retval argv pointer to new argv array on success
 * @retval NULL on error
 *
 * All strings are insertted into the argv array by value; the
 * newly-allocated array makes no references to the src_string
 * argument (i.e., it can be freed after calling this function without
 * invalidating the output argv).
 */
char **
lam_argv_split(const char *src_string, int delimiter)
{
  char arg[ARGSIZE];
  char **argv = NULL;
  const char *p;
  char *argtemp;
  int argc = 0;
  size_t arglen;

  while (*src_string) {
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
      if (LAM_ERROR == lam_argv_append(&argc, &argv, src_string))
	return NULL;
    }

    /* long argument, malloc buffer, copy and add */

    else if (arglen > (ARGSIZE - 1)) {
      argtemp = LAM_MALLOC(arglen + 1);
      if (NULL == argtemp)
	return NULL;

      strncpy(argtemp, src_string, arglen);
      argtemp[arglen] = '\0';

      if (LAM_ERROR == lam_argv_append(&argc, &argv, argtemp)) {
	LAM_FREE(argtemp);
	return NULL;
      }

      LAM_FREE(argtemp);
    }

    /* short argument, copy to buffer and add */

    else {
      strncpy(arg, src_string, arglen);
      arg[arglen] = '\0';

      if (LAM_ERROR == lam_argv_append(&argc, &argv, arg))
	return NULL;
    }

    src_string = p;
  }

  /* All done */

  return argv;
}


/**
 * Return the length of a NULL-terminated argv array.
 *
 * @param argv The input argv array.
 *
 * @retval 0 If NULL is passed as argv.
 * @retval count Number of entries in the argv array.
 *
 * The argv array must be NULL-terminated.
 */
int
lam_argv_count(char **argv)
{
  char **p;
  int i;

  if (NULL == argv)
    return 0;

  for (i = 0, p = argv; *p; i++, p++)
    continue;

  return i;
}


/**
 * Join all the elements of an argv array into a single
 * newly-allocated string.
 *
 * @param argv The input argv array.
 * @param delimiter Delimiter character placed between each argv string.
 *
 * @retval new_string Output string on success.
 * @retval NULL On failure.
 *
 * Similar to the Perl join function, this function takes an input
 * argv and joins them into into a single string separated by the
 * delimiter character.
 *
 * It is the callers responsibility to free the returned string.
 */
char *
lam_argv_join(char **argv, int delimiter)
{
  char **p;
  char *pp;
  char *str;
  size_t str_len = 0;
  size_t i;

  /* Find the total string length in argv including delimiters.  The
     last delimiter is replaced by the NULL character. */

  for (p = argv; *p; ++p) {
    str_len += strlen(*p) + 1;
  }

  /* Allocate the string. */

  if (NULL == (str = LAM_MALLOC(str_len)))
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


/**
 * Return the number of bytes consumed by an argv array.
 *
 * @param argv The input argv array.
 *
 * Count the number of bytes consumed by a NULL-terminated argv array.
 * This includes the number of bytes used by each of the strings as
 * well as the pointers used in the argv array.
 */
size_t
lam_argv_len(char **argv)
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


/**
 * Copy a NULL-terminated argv array.
 *
 * @param argv The input argv array.
 *
 * @retval argv Copied argv array on success.
 * @retval NULL On failure.
 *
 * Copy an argv array, including copying all off its strings.
 * Specifically, the output argv will be an array of the same length
 * as the input argv, and strcmp(argv_in[i], argv_out[i]) will be 0.
 */
char **
lam_argv_copy(char **argv)
{
  char **dupv = NULL;
  int dupc = 0;

  if (NULL == argv)
    return NULL;

  while (NULL != *argv) {
    if (LAM_ERROR == lam_argv_append(&dupc, &dupv, *argv)) {
      lam_argv_free(dupv);
      return NULL;
    }

    ++argv;
  }

  /* All done */

  return dupv;
}
