/*
 * $HEADER$
 */

/**
 * @file 
 *
 * General command line parsing facility for use throughout Open MPI.
 */

#ifndef OMPI_CMD_LINE_H
#define OMPI_CMD_LINE_H

#include "ompi_config.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "util/argv.h"

/**
 * \internal
 *
 * Top-level descriptor.  This type is actually internal to the
 * cmd_line interface, but we can't have the top-level convenience
 * typedef without it.  You should not use the internal members of
 * this struct; please use the interface described in this file.
 */
struct ompi_cmd_line_t {
  ompi_mutex_t lcl_mutex;
  /**< Thread safety */

  ompi_list_t lcl_options;
  /**< List of cmd_line_option_t's, below */

  int lcl_argc;
  /**< Duplicate of argc from ompi_cmd_line_parse() */
  char **lcl_argv;
  /**< Duplicate of argv from ompi_cmd_line_parse() */

  ompi_list_t lcl_params;
  /**< Parsed output; list of cmd_line_param_t's, below */

  int lcl_tail_argc;
  /**< List of tail (unprocessed) arguments */
  char **lcl_tail_argv;
  /**< List of tail (unprocessed) arguments */
};
/**
 * Convenience typedef.
 */
typedef struct ompi_cmd_line_t ompi_cmd_line_t;


#ifdef __cplusplus
extern "C" {
#endif

  /**
   * Create a OMPI command line handle.
   *
   * @retval NULL Upon failure.
   * @retval cmd A pointer to an empty command line handle upon success.
   *
   * This is the first function invoked to start command line parsing.
   * It creates an independant handle that is used in all other
   * ompi_cmd_line*() functions.  Multiple handles can be created and
   * simultaneously processed; each handle is independant from others.
   *
   * The ompi_cmd_line_t handles are [simplisticly] thread safe;
   * processing is guaranteed to be mutually exclusive if multiple
   * threads invoke functions on the same handle at the same time --
   * access will be serialized in an unspecified order.
   *
   * It is necessary to call ompi_cmd_line_free() with the handle
   * returned from this function in order to free all memory associated
   * with it.
   */
  ompi_cmd_line_t *ompi_cmd_line_create(void);

  /*
   * Free a (ompi_cmd_line_t*) that was initially allocated via
   * ompi_cmd_line_create().
   *
   * @param cmd The OMPI command line handle to free.
   *
   * This function frees a OMPI command line handle and all its
   * associated memory.  This function can be called with any non-NULL
   * pointer that was returned from ompi_cmd_line_create().  Once called,
   * the handle is no longer valid.
   *
   * Access into this function is not thread safe.  Since, by
   * definition, calling this function will destroy the handle, it does
   * not make sense to try to have thread A invoke a different
   * ompi_cmd_line*() function and thread B invoke ompi_cmd_line_free() --
   * even if the calls are serialized, there's a race condition where
   * thread A may try to use a now-invalid handle.
   */
  void ompi_cmd_line_free(ompi_cmd_line_t *cmd);

  /**
   * Create a command line option.
   *
   * @param cmd OMPI command line handle.
   * @param short_name "Short" name of the command line option.
   * @param long_name "Long" name of the command line option.
   * @param num_params How many parameters this option takes.
   * @param dest Short string description of this option.
   *
   * @retval OMPI_ERR_OUT_OF_RESOURCE If out of memory.
   * @retval OMPI_ERR_BAD_PARAM If bad parameters passed.
   * @retval OMPI_SUCCESS Upon success.
   *
   * Adds a command line option to the list of options that a a OMPI
   * command line handle will accept.  The short_name may take the
   * special value '\0' to not have a short name.  Likewise, the
   * long_name may take the special value NULL to not have a long name.
   * However, either the long or the short name must take valid value.
   *
   * num_params indicates how many parameters this option takes.  It
   * must be greater than or equal to 0.
   *
   * Finally, desc is a short string description of this option.  It is
   * used to generate the output from ompi_cmd_line_get_usage_msg().
   */
  int ompi_cmd_line_make_opt(ompi_cmd_line_t *cmd, char short_name, 
                            const char *long_name, int num_params, 
                            const char *desc);

  /**
   * Parse a command line according to a pre-built OMPI command line
   * handle.
   *
   * @param cmd OMPI command line handle.
   * @param ignore_unknown Whether to ignore unknown tokens in the
   * middle of the command line or not.
   * @param argc Length of the argv array.
   * @param argv Array of strings from the command line.
   *
   * @retval OMPI_SUCCESS Upon success.
   *
   * Parse a series of command line tokens according to the option
   * descriptions from a OMPI command line handle.  The OMPI command line
   * handle can then be queried to see what options were used, what
   * their parameters were, etc.
   *
   * The contents of argc and argv are not changed during parsing.
   * argv[0] is assumed to be the executable name, and is ignored during
   * parsing.  It can later be retrieved with
   *
   * Parsing will stop in the following conditions:
   *
   * - all argv tokens are processed
   * - the token "--" is found
   * - an unrecognized token is found and ignore_unknown is false
   *
   * Note that "--" is ignored if it is found in the middle an expected
   * number of arguments.  For example, if "--foo" is expected to have 3
   * arguments, and the command line is:
   *
   * executable --foo a b -- other arguments
   *
   * This will result in an error, because "--" will be parsed as the
   * third parameter to the first instance of "foo", and "other" will be
   * an unrecognized option.
   *
   * Unprocessed tokens are known as the "tail."  Any unprocessed tokens
   * can be obtained from the ompi_cmd_line_get_tail() function.
   *
   * Invoking this function multiple times on different sets of argv
   * tokens is safe, but will erase any previous parsing results.
   */
  int ompi_cmd_line_parse(ompi_cmd_line_t *cmd, bool ignore_unknown,
                         int argc, char **argv);

  /**
   * Return a consolidated "usage" message for a OMPI command line handle.
   *
   * @param cmd OMPI command line handle.
   *
   * @retval str Usage message.
   *
   * Returns a formatted string suitable for printing that lists the
   * expected usage message and a short description of each option on
   * the OMPI command line handle.  Options that passed a NULL
   * description to ompi_cmd_line_make_opt() will not be included in the
   * display (to allow for undocumented options).
   *
   * This function is typically only invoked internally by the
   * ompi_show_help() function.
   *
   * This function should probably be fixed up to produce prettier
   * output.
   *
   * The returned string must be freed by the caller.
   */
  char *ompi_cmd_line_get_usage_msg(ompi_cmd_line_t *cmd);
  /**
   * Test if a given option was taken on the parsed command line.
   *
   * @param cmd OMPI command line handle.
   * @param opt Short or long name of the option to check for.
   *
   * @retval true If the command line option was found during
   * ompi_cmd_line_parse().
   *
   * @retval false If the command line option was not found during
   * ompi_cmd_line_parse(), or ompi_cmd_line_parse() was not invoked on
   * this handle.
   *
   * This function should only be called after ompi_cmd_line_parse().  
   *
   * The function will return true if the option matching opt was found
   * (either by its short or long name) during token parsing.
   * Otherwise, it will return false.
   */
  bool ompi_cmd_line_is_taken(ompi_cmd_line_t *cmd, const char *opt);
  /**
   * Return the number of instances of an option found during parsing.
   *
   * @param cmd OMPI command line handle.
   * @param opt Short or long name of the option to check for.
   *
   * @retval num Number of instances (to include 0) of a given potion
   * found during ompi_cmd_line_parse().
   *
   * @retval OMPI_ERR If the command line option was not found during
   * ompi_cmd_line_parse(), or ompi_cmd_line_parse() was not invoked on
   * this handle.
   *
   * This function should only be called after ompi_cmd_line_parse().
   *
   * The function will return the number of instances of a given option
   * (either by its short or long name) -- to include 0 -- or OMPI_ERR if
   * either the option was not specified as part of the OMPI command line
   * handle, or ompi_cmd_line_parse() was not invoked on this handle.
   */
  int ompi_cmd_line_get_ninsts(ompi_cmd_line_t *cmd, const char *opt);

  /**
   * Return a specific parameter for a specific instance of a option
   * from the parsed command line.
   *
   * @param cmd OMPI command line handle.
   * @param opt Short or long name of the option to check for.
   * @param instance_num Instance number of the option to query.
   * @param param_num Which parameter to return.
   *
   * @retval param String of the parameter.
   * @retval NULL If any of the input values are invalid.
   *
   * This function should only be called after ompi_cmd_line_parse().  
   *
   * This function returns the Nth parameter for the Ith instance of a
   * given option on the parsed command line (both N and I are
   * zero-indexed).  For example, on the command line:
   *
   * executable --foo bar1 bar2 --foo bar3 bar4
   *
   * The call to ompi_cmd_line_get_param(cmd, "foo", 1, 1) would return
   * "bar4".  ompi_cmd_line_get_param(cmd, "bar", 0, 0) would return
   * NULL, as would ompi_cmd_line_get_param(cmd, "foo", 2, 2);
   *
   * The returned string should \em not be modified or freed by the
   * caller.
   */
  char *ompi_cmd_line_get_param(ompi_cmd_line_t *cmd, const char *opt, 
                               int instance_num, int param_num);
#ifdef __cplusplus
}
#endif


/**
 * Return the number of arguments parsed on a OMPI command line handle.
 *
 * @param cmd A pointer to the OMPI command line handle.
 *
 * @retval OMPI_ERROR If cmd is NULL.
 * @retval argc Number of arguments previously added to the handle.
 *
 * Arguments are added to the handle via the ompi_cmd_line_parse()
 * function.
 */
static inline int ompi_cmd_line_get_argc(ompi_cmd_line_t *cmd)
{
  return (NULL != cmd) ? cmd->lcl_argc : OMPI_ERROR;
}


/**
 * Return a string argument parsed on a OMPI command line handle.
 *
 * @param cmd A pointer to the OMPI command line handle.
 * @param index The nth argument from the command line (0 is argv[0], etc.).
 *
 * @retval NULL If cmd is NULL or index is invalid
 * @retval argument String of original argv[index]
 *
 * This function returns a single token from the arguments parsed on
 * this handle.  Arguments are added bia the ompi_cmd_line_parse()
 * function.
 *
 * What is returned is a pointer to the actual string that is on the
 * handle; it should not be modified or freed.
 */
static inline char *ompi_cmd_line_get_argv(ompi_cmd_line_t *cmd, int index)
{
  return (NULL == cmd) ? NULL :
    (index >= cmd->lcl_argc || index < 0) ? NULL : cmd->lcl_argv[index];
}


/**
 * Return the entire "tail" of unprocessed argv from a OMPI command
 * line handle.
 *
 * @param cmd A pointer to the OMPI command line handle.
 * @param tailc Pointer to the output length of the null-terminated
 * tail argv array.
 * @param tailv Pointer to the output null-terminated argv of all
 * unprocessed arguments from the command line.
 *
 * @retval OMPI_ERROR If cmd is NULL or otherwise invalid.
 * @retval OMPI_SUCCESS Upon success.
 *
 * The "tail" is all the arguments on the command line that were not
 * processed for some reason.  Reasons for not processing arguments
 * include:
 *
 * \sa The argument was not recognized
 * \sa The argument "--" was seen, and therefore all arguments
 * following it were not processed
 *
 * The output tailc parameter will be filled in with the integer
 * length of the null-terminated tailv array (length including the
 * final NULL entry).  The output tailv parameter will be a copy of
 * the tail parameters, and must be freed (likely with a call to
 * ompi_argv_free()) by the caller.
 */
static inline int ompi_cmd_line_get_tail(ompi_cmd_line_t *cmd, int *tailc, 
                                        char ***tailv)
{
  if (NULL != cmd) {
    ompi_mutex_lock(&cmd->lcl_mutex);
    *tailc = cmd->lcl_tail_argc;
    *tailv = ompi_argv_copy(cmd->lcl_tail_argv);
    ompi_mutex_unlock(&cmd->lcl_mutex);
    return OMPI_SUCCESS;
  } else {
    return OMPI_ERROR;
  }
}

#endif /* OMPI_CMD_LINE_H */
