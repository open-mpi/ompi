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

/**
 * @file 
 *
 * General command line parsing facility for use throughout Open MPI.
 *
 * This scheme is inspired by the GNU getopt package.  Command line
 * options are registered.  Each option can have up to three different
 * matching tokens: a "short" name, a "single dash" name, and a "long"
 * name.  Each option can also take 0 or more arguments.  Finally,
 * each option can be repeated on the command line an arbitrary number
 * of times.
 *
 * The "short" name can only be a single letter, and will be found
 * after a single dash (e.g., "-a").  Multiple "short" names can be
 * combined into a single command line argument (e.g., "-abc" can be
 * equivalent to "-a -b -c").
 *
 * The "single dash" name is a multi-character name that only
 * requires a single dash.  This only exists to provide backwards
 * compatability for some well-known command line options in prior
 * MPI implementations (e.g., "mpirun -np 3").  It should be used
 * sparingly.
 *
 * The "long" name is a multi-character name that is found after a
 * pair of dashes.  For example, "--some-option-name".  
 *
 * A command line option is a combination of 1 or more of a short
 * name, single dash name, and a long name.  Any of the names may be
 * used on the command line; they are treated as synonyms.  For
 * example, say the following was used in for an executable named
 * "foo":
 *
 * \code
 * ompi_cmd_line_make_opt3(cmd, 'a', NULL, 'add', 1, "Add a user");
 * \endcode
 *
 * In this case, the following command lines are exactly equivalent:
 *
 * \verbatim
 * shell$ foo -a jsmith
 * shell$ foo --add jsmith
 * \endverbatim
 *
 * Note that this interface can also track multiple invocations of the
 * same option.  For example, the following is both legal and able to
 * be retrieved through this interface:
 *
 * \verbatim
 * shell$ foo -a jsmith -add bjones
 * \endverbatim
 *
 * The caller to this interface creates a command line handle
 * (ompi_cmd_line_t) with OBJ_NEW() and then uses it to register the
 * desired parameters via ompi_cmd_line_make_opt3() (or the deprecated
 * ompi_cmd_line_make_opt()).  Once all the parameters have been
 * registered, the user invokes ompi_cmd_line_parse() with the command
 * line handle and the argv/argc pair to be parsed (typically the
 * arguments from main()).  The parser will examine the argv and find
 * registered options and parameters.  It will stop parsing when it
 * runs into an recognized string token or the special "--" token.
 *
 * After the parse has occurred, various accessor functions can be
 * used to determine which options were selected, what parameters were
 * passed to them, etc.:
 *
 * - ompi_cmd_line_get_usage_msg() returns a string suitable for "help"
 *   kinds of messages.
 * - ompi_cmd_line_is_taken() returns a true or false indicating
 *   whether a given command line option was found on the command
 *   line.
 * - ompi_cmd_line_get_argc() returns the number of tokens parsed on
 *   the handle.
 * - ompi_cmd_line_get_argv() returns any particular string from the
 *   original argv.
 * - ompi_cmd_line_get_ninsts() returns the number of times a
 *   particular option was found on a command line.
 * - ompi_cmd_line_get_param() returns the Nth parameter in the Mth
 *   instance of a given parameter.
 * - ompi_cmd_line_get_tail() returns an array of tokens not parsed
 *   (i.e., if the parser ran into "--" or an unrecognized token).
 *
 * Note that a shortcut to creating a large number of options exists
 * -- one can make a table of ompi_cmd_line_init_t instances and the
 * table to ompi_cmd_line_create().  This creates an ompi_cmd_line_t
 * handle that is pre-seeded with all the options from the table
 * without the need to repeatedly invoke ompi_cmd_line_make_opt3() (or
 * equivalent).  This ompi_cmd_line_t instance is just like any other;
 * it is still possible to add more options via
 * ompi_cmd_line_make_opt3(), etc.
 */

#ifndef OMPI_CMD_LINE_H
#define OMPI_CMD_LINE_H

#include "ompi_config.h"

#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "threads/mutex.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * \internal
     *
     * Main top-level handle.  This interface should not be used by users!
     */
    struct ompi_cmd_line_t {
        /** Make this an OBJ handle */
        ompi_object_t super;
        
        /** Thread safety */
        ompi_mutex_t lcl_mutex;
        
        /** List of cmd_line_option_t's (defined internally) */
        ompi_list_t lcl_options;
        
        /** Duplicate of argc from ompi_cmd_line_parse() */
        int lcl_argc;
        /** Duplicate of argv from ompi_cmd_line_parse() */
        char **lcl_argv;
        
        /** Parsed output; list of cmd_line_param_t's (defined internally) */
        ompi_list_t lcl_params;
        
        /** List of tail (unprocessed) arguments */
        int lcl_tail_argc;
        /** List of tail (unprocessed) arguments */
        char **lcl_tail_argv;
    };
    /**
     * \internal
     *
     * Convenience typedef
     */
    typedef struct ompi_cmd_line_t ompi_cmd_line_t;

    /**
     * Data types supported by the parser
     */
    enum ompi_cmd_line_type_t {
        OMPI_CMD_LINE_TYPE_NULL,
        OMPI_CMD_LINE_TYPE_STRING,
        OMPI_CMD_LINE_TYPE_INT,
        OMPI_CMD_LINE_TYPE_BOOL,

        OMPI_CMD_LINE_TYPE_MAX
    };
    /**
     * \internal
     *
     * Convenience typedef
     */
    typedef enum ompi_cmd_line_type_t ompi_cmd_line_type_t;

    /**
     * Datatype used to construct a command line handle; see
     * ompi_cmd_line_create().
     */
    struct ompi_cmd_line_init_t {
        /** If want to set an MCA parameter, set its type name here.
            WARNING: This MCA tuple (type, component, param) will
            eventually be replaced with a single name! */
        const char *ocl_mca_type_name;
        /** If want to set an MCA parameter, set its component name
            here.  WARNING: This MCA tuple (type, component, param)
            will eventually be replaced with a single name! */
        const char *ocl_mca_component_name;
        /** If want to set an MCA parameter, set its parameter name
            here.  WARNING: This MCA tuple (type, component, param)
            will eventually be replaced with a single name! */
        const char *ocl_mca_param_name;

        /** "Short" name (i.e., "-X", where "X" is a single letter) */
        char ocl_cmd_short_name;
        /** "Single dash" name (i.e., "-foo").  The use of these are
            discouraged. */
        const char *ocl_cmd_single_dash_name;
        /** Long name (i.e., "--foo"). */
        const char *ocl_cmd_long_name;

        /** Number of parameters that this option takes */
        int ocl_num_params;

        /** If this parameter is encountered, its *first* parameter it
            saved here.  If the parameter is encountered again, the
            value is overwritten. */
        void *ocl_variable_dest;
        /** If an ocl_variable_dest is given, its datatype must be
            supplied as well. */
        ompi_cmd_line_type_t ocl_variable_type;

        /** Description of the command line option, to be used with
            ompi_cmd_line_get_usage_msg(). */
        const char *ocl_description;
    };
    /**
     * \internal
     *
     * Convenience typedef
     */
    typedef struct ompi_cmd_line_init_t ompi_cmd_line_init_t;
    
    /**
     * Top-level command line handle.
     *
     * This handle is used for accessing all command line functionality
     * (i.e., all ompi_cmd_line*() functions).  Multiple handles can be
     * created and simultaneously processed; each handle is independant
     * from others.
     *
     * The ompi_cmd_line_t handles are [simplisticly] thread safe;
     * processing is guaranteed to be mutually exclusive if multiple
     * threads invoke functions on the same handle at the same time --
     * access will be serialized in an unspecified order.
     *
     * Once finished, handles should be released with OBJ_RELEASE().  The
     * destructor for ompi_cmd_line_t handles will free all memory
     * associated with the handle.
     */
    OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_cmd_line_t);

    /**
     * Make a command line handle from a table of initializers.
     *
     * @param cmd OMPI command line handle.
     * @param table Table of ompi_cmd_line_init_t instances for all
     * the options to be included in the resulting command line
     * handler.
     *
     * @retval OMPI_SUCCESS Upon success.
     *
     * This function takes a table of ompi_cmd_line_init_t instances
     * to pre-seed an OMPI command line handle.  The last instance in
     * the table must have '\0' for the short name and NULL for the
     * single-dash and long names.  The handle is expected to have
     * been OBJ_NEW'ed or OBJ_CONSTRUCT'ed already.
     *
     * Upon return, the command line handle is just like any other.  A
     * sample using this syntax:
     *
     * \code
     * ompi_cmd_line_init_t cmd_line_init[] = {
     *    { NULL, NULL, NULL, 'h', NULL, "help", 0, 
     *      &orterun_globals.help, OMPI_CMD_LINE_TYPE_BOOL,
     *      "This help message" },
     *
     *    { NULL, NULL, NULL, '\0', NULL, "wd", 1,
     *      &orterun_globals.wd, OMPI_CMD_LINE_TYPE_STRING,
     *      "Set the working directory of the started processes" },
     *
     *    { NULL, NULL, NULL, '\0', NULL, NULL, 0,
     *      NULL, OMPI_CMD_LINE_TYPE_NULL, NULL }
     * };
     * \endcode
     */
    OMPI_DECLSPEC int ompi_cmd_line_create(ompi_cmd_line_t *cmd,
                                           ompi_cmd_line_init_t *table);

    /**
     * Create a command line option.
     *
     * This function is an older [deprecated] form of
     * ompi_cmd_line_make_opt3().  It is exactly equivalent to
     * ompi_cmd_line_make_opt3(cmd, short_name, NULL, long_name,
     * num_params, desc).
     */
    OMPI_DECLSPEC int ompi_cmd_line_make_opt(ompi_cmd_line_t *cmd,
                                             char short_name, 
                                             const char *long_name,
                                             int num_params, 
                                             const char *desc);

    /**
     * Create a command line option.
     *
     * @param cmd OMPI command line handle.
     * @param short_name "Short" name of the command line option.
     * @param sd_name "Single dash" name of the command line option.
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
     * sd_name and long_name may take the special value NULL to not have
     * a single dash or long name, respectively.  However, one of the
     * three must have a name.
     *
     * num_params indicates how many parameters this option takes.  It
     * must be greater than or equal to 0.
     *
     * Finally, desc is a short string description of this option.  It is
     * used to generate the output from ompi_cmd_line_get_usage_msg().
     *
     */
    OMPI_DECLSPEC int ompi_cmd_line_make_opt3(ompi_cmd_line_t *cmd, 
                                              char short_name, 
                                              const char *sd_name,
                                              const char *long_name, 
                                              int num_params, 
                                              const char *desc);

    /**
     * Parse a command line according to a pre-built OMPI command line
     * handle.
     *
     * @param cmd OMPI command line handle.
     * @param ignore_unknown Whether to print an error message upon
     * finding an unknown token or not
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
     * If an unknown token is found in the command line (i.e., a token
     * that is not a parameter or a registered option), the parsing will
     * stop (see below).  If ignore_unknown is false, an error message
     * is displayed.  If ignore_unknown is true, the error message is
     * not displayed.
     *
     * The contents of argc and argv are not changed during parsing.
     * argv[0] is assumed to be the executable name, and is ignored during
     * parsing.  It can later be retrieved with
     *
     * Parsing will stop in the following conditions:
     *
     * - all argv tokens are processed
     * - the token "--" is found
     * - an unrecognized token is found
     *
     * Upon any of these conditions, any remaining tokens will be placed
     * in the "tail" (and therefore not examined by the parser),
     * regardless of the value of ignore_unknown.  The set of tail
     * tokens is available from the ompi_cmd_line_get_tail() function.
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
     * Invoking this function multiple times on different sets of argv
     * tokens is safe, but will erase any previous parsing results.
     */
    OMPI_DECLSPEC int ompi_cmd_line_parse(ompi_cmd_line_t *cmd, 
                                          bool ignore_unknown,
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
    OMPI_DECLSPEC char *ompi_cmd_line_get_usage_msg(ompi_cmd_line_t *cmd);

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
    OMPI_DECLSPEC bool ompi_cmd_line_is_taken(ompi_cmd_line_t *cmd, 
                                              const char *opt);

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
    OMPI_DECLSPEC int ompi_cmd_line_get_argc(ompi_cmd_line_t *cmd);

    /**
     * Return a string argument parsed on a OMPI command line handle.
     *
     * @param cmd A pointer to the OMPI command line handle.
     * @param index The nth argument from the command line (0 is
     * argv[0], etc.).
     *
     * @retval NULL If cmd is NULL or index is invalid
     * @retval argument String of original argv[index]
     *
     * This function returns a single token from the arguments parsed
     * on this handle.  Arguments are added bia the
     * ompi_cmd_line_parse() function.
     *
     * What is returned is a pointer to the actual string that is on
     * the handle; it should not be modified or freed.
     */
    OMPI_DECLSPEC char *ompi_cmd_line_get_argv(ompi_cmd_line_t *cmd, 
                                               int index);

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
    OMPI_DECLSPEC int ompi_cmd_line_get_ninsts(ompi_cmd_line_t *cmd, 
                                               const char *opt);

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
    OMPI_DECLSPEC char *ompi_cmd_line_get_param(ompi_cmd_line_t *cmd, 
                                                const char *opt, 
                                                int instance_num,
                                                int param_num);

    /**
     * Return the entire "tail" of unprocessed argv from a OMPI
     * command line handle.
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
     * The "tail" is all the arguments on the command line that were
     * not processed for some reason.  Reasons for not processing
     * arguments include:
     *
     * \sa The argument was not recognized
     * \sa The argument "--" was seen, and therefore all arguments
     * following it were not processed
     *
     * The output tailc parameter will be filled in with the integer
     * length of the null-terminated tailv array (length including the
     * final NULL entry).  The output tailv parameter will be a copy
     * of the tail parameters, and must be freed (likely with a call
     * to ompi_argv_free()) by the caller.
     */
    OMPI_DECLSPEC int ompi_cmd_line_get_tail(ompi_cmd_line_t *cmd, int *tailc, 
                                             char ***tailv);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


#endif /* OMPI_CMD_LINE_H */
