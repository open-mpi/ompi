/*
 * $HEADER$
 */

/**
 * @file
 *
 * Generic routines for "argv"-like handling.  Helpful for creating
 * arrays of strings, especially when creating command lines.
 */

#ifndef LAM_ARGV_H
#define LAM_ARGV_H

#include "include/types.h"

#ifdef __cplusplus
extern "C" {
#endif
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
   * it is permissable to pass a string on the stack as the str
   * argument to this function.
   *
   * To add the first entry to an argv array, call this function with
   * (*argv == NULL).  This function will allocate an array of length
   * 2; the first entry will point to a copy of the string passed in
   * arg, the second entry will be set to NULL.
   *
   * If (*argv != NULL), it will be realloc'ed to be 1 (char*) larger,
   * and the next-to-last entry will point to a copy of the string
   * passed in arg.  The last entry will be set to NULL.
   *
   * Just to reinforce what was stated above: the string is copied by
   * value into the argv array; there is no need to keep the original
   * string (i.e., the arg parameter) after invoking this function.
   */
  int lam_argv_append(int *argc, char ***argv, const char *arg);

  /**
   * Free a NULL-terminated argv array.
   *
   * @param argv Argv array to free.
   *
   * This function frees an argv array and all of the strings that it
   * contains.  Since the argv parameter is passed by value, it is not
   * set to NULL in the caller's scope upon return.
   *
   * It is safe to invoke this function with a NULL pointer.  It is
   * not safe to invoke this function with a non-NULL-terminated argv
   * array.
   */
  void lam_argv_free(char **argv);
  
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
   * argument (i.e., it can be freed after calling this function
   * without invalidating the output argv).
   */
  char **lam_argv_split(const char *src_string, int delimiter);

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
  int lam_argv_count(char **argv);

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
  char *lam_argv_join(char **argv, int delimiter);

  /**
   * Return the number of bytes consumed by an argv array.
   *
   * @param argv The input argv array.
   *
   * Count the number of bytes consumed by a NULL-terminated argv
   * array.  This includes the number of bytes used by each of the
   * strings as well as the pointers used in the argv array.
   */
  size_t lam_argv_len(char **argv);

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
  char **lam_argv_copy(char **argv);
#ifdef __cplusplus
}
#endif

#endif /* LAM_ARGV_H */
