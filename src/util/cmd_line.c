/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "include/constants.h"
#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "util/argv.h"
#include "util/cmd_line.h"
#include "util/strncpy.h"
#include "util/output.h"


/*
 * Description of a command line option
 */
struct cmd_line_option_t {
    ompi_list_item_t super;

    char clo_short_name;
    char *clo_single_dash_name;
    char *clo_long_name;
    int clo_num_params;
    char *clo_description;
};
typedef struct cmd_line_option_t cmd_line_option_t;
static void option_constructor(cmd_line_option_t *cmd);
static void option_destructor(cmd_line_option_t *cmd);
static OBJ_CLASS_INSTANCE(cmd_line_option_t,
                          ompi_list_item_t,
                          option_constructor, option_destructor);

/*
 * An option that was used in the argv that was parsed
 */
struct cmd_line_param_t {
    ompi_list_item_t super;

    /* Note that clp_arg points to storage "owned" by someone else; it
       has the original option string by referene, not by value.
       Hence, it should not be free()'ed. */

    char *clp_arg;

    /* Pointer to the existing option.  This is also by reference; it
       should not be free()ed. */

    cmd_line_option_t *clp_option;

    /* This argv array is a list of all the parameters of this option.
       It is owned by this parameter, and should be freed when this
       param_t is freed. */

    int clp_argc;
    char **clp_argv;
};
typedef struct cmd_line_param_t cmd_line_param_t;
static void param_constructor(cmd_line_param_t *cmd);
static void param_destructor(cmd_line_param_t *cmd);
static OBJ_CLASS_INSTANCE(cmd_line_param_t,
                          ompi_list_item_t,
                          param_constructor, param_destructor);

/*
 * Instantiate the ompi_cmd_line_t class
 */
static void cmd_line_constructor(ompi_cmd_line_t *cmd);
static void cmd_line_destructor(ompi_cmd_line_t *cmd);
OBJ_CLASS_INSTANCE(ompi_cmd_line_t,
                   ompi_object_t,
                   cmd_line_constructor,
                   cmd_line_destructor);

/*
 * Private variables
 */
static char special_empty_token[] = {
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, '\0'
};

/*
 * Private functions
 */
static void free_parse_results(ompi_cmd_line_t *cmd);
static int split_shorts(ompi_cmd_line_t *cmd,
                        char *token, char **args, 
                        int *output_argc, char ***output_argv, 
                        int *num_args_used, bool ignore_unknown);
static cmd_line_option_t *find_option(ompi_cmd_line_t *cmd, 
                                      const char *option_name);


/*
 * Create a command line option, --long-name and/or -s (short name).
 */
int ompi_cmd_line_make_opt(ompi_cmd_line_t *cmd, char short_name, 
                          const char *long_name, int num_params, 
                          const char *desc)
{
    return ompi_cmd_line_make_opt3(cmd, short_name, NULL, long_name, 
                                   num_params, desc);
}


/*
 * Create a command line option, --long-name and/or -s (short name).
 */
int ompi_cmd_line_make_opt3(ompi_cmd_line_t *cmd, char short_name, 
                            const char *sd_name, const char *long_name, 
                            int num_params, const char *desc)
{
    cmd_line_option_t *option;

    /* Bozo check */

    if ('\0' == short_name && NULL == sd_name && NULL == long_name) {
        return OMPI_ERR_BAD_PARAM;
    } else if (NULL == cmd) {
        return OMPI_ERR_BAD_PARAM;
    } else if (num_params < 0) {
        return OMPI_ERR_BAD_PARAM;
    }

    /* Allocate and fill an option item */

    option = OBJ_NEW(cmd_line_option_t);
    if (NULL == option) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    option->clo_short_name = short_name;
    if (NULL != sd_name) {
        option->clo_single_dash_name = strdup(sd_name);
    }
    if (NULL != long_name) {
        option->clo_long_name = strdup(long_name);
    }
    option->clo_num_params = num_params;
    if (NULL != desc) {
        option->clo_description = strdup(desc);
    }

    /* Append the item, serializing thread access */

    ompi_mutex_lock(&cmd->lcl_mutex);
    ompi_list_append(&cmd->lcl_options, (ompi_list_item_t*) option);
    ompi_mutex_unlock(&cmd->lcl_mutex);

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Parse a command line according to a pre-built OMPI command line
 * handle.
 */
int ompi_cmd_line_parse(ompi_cmd_line_t *cmd, bool ignore_unknown,
                        int argc, char **argv)
{
    int i, j, orig, ret;
    cmd_line_option_t *option;
    cmd_line_param_t *param;
    bool is_unknown;
    bool is_option;
    char **shortsv;
    int shortsc;
    int num_args_used;

    /* Bozo check */

    if (0 == argc || NULL == argv) {
        return OMPI_SUCCESS;
    }

    /* Thread serialization */

    ompi_mutex_lock(&cmd->lcl_mutex);

    /* Free any parsed results that are already on this handle */

    free_parse_results(cmd);

    /* Analyze each token */

    cmd->lcl_argc = argc;
    cmd->lcl_argv = ompi_argv_copy(argv);

    /* Now traverse the easy-to-parse sequence of tokens.  Note that
       incrementing i must happen elsehwere; it can't be the third
       clause in the "if" statement. */

    param = NULL;
    option = NULL;
    for (i = 1; i < cmd->lcl_argc; ) {
        is_unknown = false;
        is_option = false;

        /* Are we done?  i.e., did we find the special "--" token?  If
           so, copy everying beyond it into the tail (i.e., don't
           bother copying the "--" into the tail). */

        if (0 == strcmp(cmd->lcl_argv[i], "--")) {
            ++i;
            while (i < cmd->lcl_argc) {
                ompi_argv_append(&cmd->lcl_tail_argc, &cmd->lcl_tail_argv, 
                                 cmd->lcl_argv[i]);
                ++i;
            }

            break;
        }

        /* If it's not an option, then we've found an unrecognized
           token. */

        else if ('-' != cmd->lcl_argv[i][0]) {
            is_unknown = true;
        }

        /* Nope, this is supposedly an option.  Is it a long name? */

        else if (0 == strncmp(cmd->lcl_argv[i], "--", 2)) {
            is_option = true;
            option = find_option(cmd, cmd->lcl_argv[i] + 2);
        }

        /* It could be a short name.  Is it? */

        else {
            option = find_option(cmd, cmd->lcl_argv[i] + 1);

            /* If we didn't find it, try to split it into shorts.  If
               we find the short option, replace lcl_argv[i] and
               insert the rest into lcl_argv starting after position
               i.  If we don't find the short option, don't do
               anything to lcl_argv so that it can fall through to the
               error condition, below. */

            if (NULL == option) {
                shortsv = NULL;
                shortsc = 0;
                ret = split_shorts(cmd, cmd->lcl_argv[i] + 1, 
                                   &(cmd->lcl_argv[i + 1]),
                                   &shortsc, &shortsv, 
                                   &num_args_used, ignore_unknown);
                if (OMPI_SUCCESS == ret) {
                    option = find_option(cmd, shortsv[0] + 1);

                    if (NULL != option) {
                        ompi_argv_delete(cmd->lcl_argv, i,
                                         1 + num_args_used);
                        ompi_argv_insert(&cmd->lcl_argv, i, shortsv);
                        cmd->lcl_argc = ompi_argv_count(cmd->lcl_argv);
                    } else {
                        is_unknown = true;
                    }
                    ompi_argv_free(shortsv);
                } else {
                    is_unknown = true;
                }
            }

            if (NULL != option) {
                is_option = true;
            }
        }

        /* If we figured out above that this is an option, handle it */

        if (is_option) {
            if (NULL == option) {
                is_unknown = true;
            } else {
                is_unknown = false;
                orig = i;
                ++i;

                /* Suck down the following parameters that belong to
                   this option.  If we run out of parameters, or find
                   that any of them are the special_empty_param
                   (insertted by split_shorts()), then print an error
                   and return. */

                param = OBJ_NEW(cmd_line_param_t);
                if (NULL == param) {
                    ompi_mutex_unlock(&cmd->lcl_mutex);
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
                param->clp_arg = cmd->lcl_argv[i];
                param->clp_option = option;

                /* If we have any parameters to this option, suck down
                   tokens starting one beyond the token that we just
                   recognized */

                for (j = 0; j < option->clo_num_params; ++j, ++i) {

                    /* If we run out of parameters, error */

                    if (i >= cmd->lcl_argc) {
                        ompi_output(0, "Error: option \"%s\" did not have "
                                    "enough parameters (%d)",
                                    cmd->lcl_argv[orig],
                                    option->clo_num_params);
                        if (NULL != param->clp_argv)
                            ompi_argv_free(param->clp_argv);
                        OBJ_RELEASE(param);
                        i = cmd->lcl_argc;
                        break;
                    } else {
                        if (0 == strcmp(cmd->lcl_argv[i], 
                                        special_empty_token)) {
                            ompi_output(0, "Error: option \"%s\" did not have "
                                        "enough parameters (%d)",
                                        cmd->lcl_argv[orig],
                                        option->clo_num_params);
                            if (NULL != param->clp_argv)
                                ompi_argv_free(param->clp_argv);
                            OBJ_RELEASE(param);
                            i = cmd->lcl_argc;
                            break;
                        } 

                        /* Otherwise, save this parameter in the argv
                           on the param entry */

                        else {
                            ompi_argv_append(&param->clp_argc,
                                             &param->clp_argv, 
                                             cmd->lcl_argv[i]);
                        }
                    }
                }

                /* If we succeeded in all that, save the param to the
                   list on the ompi_cmd_line_t handle */

                if (NULL != param) {
                    ompi_list_append(&cmd->lcl_params,
                                     (ompi_list_item_t *) param);
                }
            }
        }

        /* If we figured out above that this was an unknown option,
           handle it.  Copy everything (including the current token)
           into the tail.  If we're not ignoring unknowns, then print
           an error. */

        if (is_unknown) {
            if (!ignore_unknown) {
                ompi_output(0, "Error: unknown option \"%s\"", 
                            cmd->lcl_argv[i]);
            }
            while (i < cmd->lcl_argc) {
                ompi_argv_append(&cmd->lcl_tail_argc, &cmd->lcl_tail_argv, 
                                 cmd->lcl_argv[i]);
                ++i;
            }
        }
    }

    /* Thread serialization */

    ompi_mutex_unlock(&cmd->lcl_mutex);

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Return a consolidated "usage" message for a OMPI command line handle.
 */
char *ompi_cmd_line_get_usage_msg(ompi_cmd_line_t *cmd)
{
    int i, len, prev_len;
    int argc;
    char **argv;
    char *ret, *line, *temp;
    ompi_list_item_t *item;
    cmd_line_option_t *option;

    /* Thread serialization */

    ompi_mutex_lock(&cmd->lcl_mutex);

    /* Make an argv of all the usage strings */

    prev_len = 0;
    argc = 0;
    argv = NULL;
    ret = NULL;
    line = NULL;
    temp = NULL;
    for (item = ompi_list_get_first(&cmd->lcl_options); 
         ompi_list_get_end(&cmd->lcl_options) != item;
         item = ompi_list_get_next(item)) {
        option = (cmd_line_option_t *) item;
        if (NULL != option->clo_description) {
            
            /* See how much space we need */

            len = 5 + strlen(option->clo_description);
            if ('\0' != option->clo_short_name) {
                len += 5;
            }
            if (NULL != option->clo_long_name) {
                len += strlen(option->clo_long_name);
            }
            len += option->clo_num_params * 10;

            /* Do we have enough already? */

            if (len > prev_len) {
                if (NULL != line) {
                    free(line);
                }
                line = malloc(len * 2);
                if (NULL == line) {
                    ompi_mutex_unlock(&cmd->lcl_mutex);
                    return NULL;
                }
                temp = line + len;
                prev_len = len;
            }

            /* Build up the output line */

            line[0] = '\0';
            if ('\0' != option->clo_short_name) {
                snprintf(temp, len, "-%c", option->clo_short_name);
                strcat(line, temp);
            }
            if (NULL != option->clo_long_name) {
                if ('\0' != option->clo_short_name) {
                    strcat(line, "|");
                }
                snprintf(temp, len, "--%s", option->clo_long_name);
                strcat(line, temp);
            }
            strcat(line, " ");
            for (i = 0; i < option->clo_num_params; ++i) {
                snprintf(temp, len, "<arg%d>", i);
                strcat(line, temp);
            }
            if (option->clo_num_params > 0) {
                strcat(line, " ");
            }
            strcat(line, option->clo_description);

            /* Save the line */

            ompi_argv_append(&argc, &argv, line);
        }
    }
    if (NULL != line) {
        free(line);
    }
    if (NULL != argv) {
        ret = ompi_argv_join(argv, '\n');
        ompi_argv_free(argv);
    }

    /* Thread serialization */

    ompi_mutex_unlock(&cmd->lcl_mutex);

    /* All done */
 
    return ret;
}


/*
 * Test if a given option was taken on the parsed command line.
 */
bool ompi_cmd_line_is_taken(ompi_cmd_line_t *cmd, const char *opt)
{
    return (ompi_cmd_line_get_ninsts(cmd, opt) > 0);
}


/*
 * Return the number of instances of an option found during parsing.
 */
int ompi_cmd_line_get_ninsts(ompi_cmd_line_t *cmd, const char *opt)
{
    int ret;
    ompi_list_item_t *item;
    cmd_line_param_t *param;
    cmd_line_option_t *option;

    /* Thread serialization */

    ompi_mutex_lock(&cmd->lcl_mutex);

    /* Find the corresponding option.  If we find it, look through all
       the parsed params and see if we have any matches. */

    ret = 0;
    option = find_option(cmd, opt);
    if (NULL != option) {
        for (item = ompi_list_get_first(&cmd->lcl_params); 
             ompi_list_get_end(&cmd->lcl_params) != item;
             item = ompi_list_get_next(item)) {
            param = (cmd_line_param_t *) item;
            if (param->clp_option == option) {
                ++ret;
            }
        }
    }

    /* Thread serialization */

    ompi_mutex_unlock(&cmd->lcl_mutex);

    /* All done */

    return ret;
}


/*
 * Return a specific parameter for a specific instance of a option
 * from the parsed command line.
 */
char *ompi_cmd_line_get_param(ompi_cmd_line_t *cmd, const char *opt, int inst, 
                              int idx)
{
    int num_found;
    ompi_list_item_t *item;
    cmd_line_param_t *param;
    cmd_line_option_t *option;

    /* Thread serialization */

    ompi_mutex_lock(&cmd->lcl_mutex);

    /* Find the corresponding option.  If we find it, look through all
       the parsed params and see if we have any matches. */

    num_found = 0;
    option = find_option(cmd, opt);
    if (NULL != option) {

        /* Ensure to check for the case where the user has asked for a
           parameter index greater than we will have */

        if (idx < option->clo_num_params) {
            for (item = ompi_list_get_first(&cmd->lcl_params); 
                 ompi_list_get_end(&cmd->lcl_params) != item;
                 item = ompi_list_get_next(item)) {
                param = (cmd_line_param_t *) item;
                if (param->clp_option == option) {
                    if (num_found == inst) {
                        ompi_mutex_unlock(&cmd->lcl_mutex);
                        return param->clp_argv[idx];
                    }
                    ++num_found;
                }
            }
        }
    }
    
    /* Thread serialization */
    
    ompi_mutex_unlock(&cmd->lcl_mutex);
    
    /* All done */

    return NULL;
}


/*
 * Return the number of arguments parsed on a OMPI command line handle.
 */
int ompi_cmd_line_get_argc(ompi_cmd_line_t *cmd)
{
    return (NULL != cmd) ? cmd->lcl_argc : OMPI_ERROR;
}


/*
 * Return a string argument parsed on a OMPI command line handle.
 */
char *ompi_cmd_line_get_argv(ompi_cmd_line_t *cmd, int index)
{
    return (NULL == cmd) ? NULL :
        (index >= cmd->lcl_argc || index < 0) ? NULL : cmd->lcl_argv[index];
}


/*
 * Return the entire "tail" of unprocessed argv from a OMPI command
 * line handle.
 */
int ompi_cmd_line_get_tail(ompi_cmd_line_t *cmd, int *tailc, char ***tailv)
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


/**************************************************************************
 * Static functions
 **************************************************************************/

static void option_constructor(cmd_line_option_t *o)
{
    o->clo_short_name = '\0';
    o->clo_single_dash_name = NULL;
    o->clo_long_name = NULL;
    o->clo_num_params = 0;
    o->clo_description = NULL;
}


static void option_destructor(cmd_line_option_t *o)
{
    if (NULL != o->clo_single_dash_name) {
        free(o->clo_single_dash_name);
    }
    if (NULL != o->clo_long_name) {
        free(o->clo_long_name);
    }
    if (NULL != o->clo_description) {
        free(o->clo_description);
    }
}


static void param_constructor(cmd_line_param_t *p)
{
    p->clp_arg = NULL;
    p->clp_option = NULL;
    p->clp_argc = 0;
    p->clp_argv = NULL;
}


static void param_destructor(cmd_line_param_t *p)
{
    if (NULL != p->clp_argv) {
        ompi_argv_free(p->clp_argv);
    }
}


static void cmd_line_constructor(ompi_cmd_line_t *cmd)
{
    /* Initialize the mutex.  Since we're creating (and therefore the
       only thread that has this instance), there's no need to lock it
       right now. */

    OBJ_CONSTRUCT(&cmd->lcl_mutex, ompi_mutex_t);

    /* Initialize the lists */

    OBJ_CONSTRUCT(&cmd->lcl_options, ompi_list_t);
    OBJ_CONSTRUCT(&cmd->lcl_params, ompi_list_t);

    /* Initialize the argc/argv pairs */

    cmd->lcl_argc = 0;
    cmd->lcl_argv = NULL;
    cmd->lcl_tail_argc = 0;
    cmd->lcl_tail_argv = NULL;
}


static void cmd_line_destructor(ompi_cmd_line_t *cmd)
{
    ompi_list_item_t *item;

    /* Free the contents of the options list (do not free the list
       itself; it was not allocated from the heap) */

    for (item = ompi_list_remove_first(&cmd->lcl_options);
         NULL != item;
         item = ompi_list_remove_first(&cmd->lcl_options)) {
        OBJ_RELEASE(item);
    }

    /* Free any parsed results */

    free_parse_results(cmd);

    /* Destroy the lists */

    OBJ_DESTRUCT(&cmd->lcl_options);
    OBJ_DESTRUCT(&cmd->lcl_params);

    /* Destroy the mutex */

    OBJ_DESTRUCT(&cmd->lcl_mutex);
}


static void free_parse_results(ompi_cmd_line_t *cmd)
{
    ompi_list_item_t *item;

    /* Free the contents of the params list (do not free the list
       itself; it was not allocated from the heap) */

    for (item = ompi_list_remove_first(&cmd->lcl_params);
         NULL != item; 
         item = ompi_list_remove_first(&cmd->lcl_params)) {
        OBJ_RELEASE(item);
    }

    /* Free the argv's */

    if (NULL != cmd->lcl_argv) {
        ompi_argv_free(cmd->lcl_argv);
    }
    cmd->lcl_argv = NULL;
    cmd->lcl_argc = 0;

    if (NULL != cmd->lcl_tail_argv) {
        ompi_argv_free(cmd->lcl_tail_argv);
    }
    cmd->lcl_tail_argv = NULL;
    cmd->lcl_tail_argc = 0;
}


/*
 * Traverse a token and split it into individual letter options (the
 * token has already been certified to not be a long name and not be a
 * short name).  Ensure to differentiate the resulting options from
 * "single dash" names.
 */
static int split_shorts(ompi_cmd_line_t *cmd, char *token, char **args, 
                        int *output_argc, char ***output_argv, 
                        int *num_args_used, bool ignore_unknown)
{
    int i, j, len;
    cmd_line_option_t *option;
    char fake_token[3];
    int num_args;

    /* Setup that we didn't use any of the args */

    num_args = ompi_argv_count(args);
    *num_args_used = 0;

    /* Traverse the token */

    len = strlen(token);
    fake_token[0] = '-';
    fake_token[2] = '\0';
    for (i = 0; i < len; ++i) {
        fake_token[1] = token[i];
        option = find_option(cmd, fake_token + 1);

        /* If we don't find the option, either return an error or pass
           it through unmodified to the new argv */

        if (NULL == option) {
            if (!ignore_unknown) {
                return OMPI_ERR_BAD_PARAM;
            } else {
                ompi_argv_append(output_argc, output_argv, fake_token);
            }
        } 

        /* If we do find the option, copy it and all of its parameters
           to the output args.  If we run out of paramters (i.e., no
           more tokens in the original argv), that error will be
           handled at a higher level) */

        else {
            ompi_argv_append(output_argc, output_argv, fake_token);
            for (j = 0; j < option->clo_num_params; ++j) {
                if (*num_args_used < num_args) {
                    ompi_argv_append(output_argc, output_argv,
                                     args[*num_args_used]);
                    ++(*num_args_used);
                } else {
                    ompi_argv_append(output_argc, output_argv, 
                                     special_empty_token);
                }
            }
        }
    }

    /* All done */

    return OMPI_SUCCESS;
}


static cmd_line_option_t *find_option(ompi_cmd_line_t *cmd, 
                                      const char *option_name)
{
    ompi_list_item_t *item;
    cmd_line_option_t *option;

    /* Iterate through the list of options hanging off the
       ompi_cmd_line_t and see if we find a match in either the short
       or long names */

    for (item = ompi_list_get_first(&cmd->lcl_options);
         ompi_list_get_end(&cmd->lcl_options) != item;
         item = ompi_list_get_next(item)) {
        option = (cmd_line_option_t *) item;
        if ((NULL != option->clo_long_name &&
             0 == strcmp(option_name, option->clo_long_name)) ||
            (NULL != option->clo_single_dash_name &&
             0 == strcmp(option_name, option->clo_single_dash_name)) ||
            (strlen(option_name) == 1 &&
             option_name[0] == option->clo_short_name)) {
            return option;
        }
    }

    /* Not found */
    
    return NULL;
}
