/*
 * $HEADER$
 */

#include <stdlib.h>
#include <string.h>

#include "include/constants.h"
#include "util/argv.h"
#include "util/strncpy.h"

#define ARGSIZE 128


/*
 * Append a string to the end of a new or existing argv array.
 */
int lam_argv_append(int *argc, char ***argv, const char *arg)
{
  /* Create new argv. */

  if (NULL == *argv) {
    *argv = malloc(2 * sizeof(char *));
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

  (*argv)[*argc] = malloc(strlen(arg) + 1);
  if (NULL == (*argv)[*argc])
    return LAM_ERROR;

  strcpy((*argv)[*argc], arg);
  *argc = *argc + 1;
  (*argv)[*argc] = NULL;

  return LAM_SUCCESS;
}


/*
 * Free a NULL-terminated argv array.
 */
void lam_argv_free(char **argv)
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
char **lam_argv_split(const char *src_string, int delimiter)
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
      argtemp = malloc(arglen + 1);
      if (NULL == argtemp)
	return NULL;

      strncpy(argtemp, src_string, arglen);
      argtemp[arglen] = '\0';

      if (LAM_ERROR == lam_argv_append(&argc, &argv, argtemp)) {
	free(argtemp);
	return NULL;
      }

      free(argtemp);
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


/*
 * Return the length of a NULL-terminated argv array.
 */
int lam_argv_count(char **argv)
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
char *lam_argv_join(char **argv, int delimiter)
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

  if (NULL == (str = malloc(str_len)))
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
size_t lam_argv_len(char **argv)
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
char **lam_argv_copy(char **argv)
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
