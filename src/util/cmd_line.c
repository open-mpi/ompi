/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

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
  char *clo_long_name;
  int clo_num_params;
  char *clo_description;
};
typedef struct cmd_line_option_t cmd_line_option_t;

/*
 * An option that was used in the argv that was parsed
 */
struct cmd_line_param_t {
  ompi_list_item_t super;

  /* Note that clp_arg points to storage "owned" by someone else; it
     has the original option string by referene, not by value.  Hence,
     it should not be free()'ed. */

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
static int split_shorts(ompi_cmd_line_t *cmd, bool ignore_unknown);
static cmd_line_option_t *find_option(ompi_cmd_line_t *cmd, 
                                      const char *option_name);


/*
 * Create a OMPI command line handle.
 */
ompi_cmd_line_t *ompi_cmd_line_create(void)
{
  ompi_cmd_line_t *cmd = malloc(sizeof(ompi_cmd_line_t));

  if (NULL == cmd)
    return NULL;

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

  /* All done */

  return cmd;
}


/*
 * Free a (ompi_cmd_line_t*) that was initially allocated via
 * ompi_cmd_line_create().
 */
void ompi_cmd_line_free(ompi_cmd_line_t *cmd)
{
  ompi_list_item_t *item;
  cmd_line_option_t *option;

#if 0
  /* JMS This function doesn't seem to exist...? */
  /* We don't lock the mutex; there's no point.  Just free it. */

  ompi_mutex_free(&cmd->lcl_mutex);
#endif

  /* Free the contents of the options list (do not free the list
     itself; it was not allocated from the heap) */

  for (item = ompi_list_remove_first(&cmd->lcl_options);
       NULL != item;
       item = ompi_list_remove_first(&cmd->lcl_options)) {
    option = (cmd_line_option_t *) item;
    if (NULL != option->clo_long_name)
      free(option->clo_long_name);
    if (NULL != option->clo_description)
      free(option->clo_description);
    free(item);
  }

  /* Free any parsed results */

  free_parse_results(cmd);

  /* Now free the cmd itself */

  free(cmd);
}


/*
 * Create a command line option.
 */
int ompi_cmd_line_make_opt(ompi_cmd_line_t *cmd, char short_name, 
                          const char *long_name, int num_params, 
                          const char *desc)
{
  cmd_line_option_t *option;

  /* Bozo check */

  if ('\0' == short_name && NULL == long_name)
    return OMPI_ERR_BAD_PARAM;
  if (NULL == cmd)
    return OMPI_ERR_BAD_PARAM;
  if (num_params < 0)
    return OMPI_ERR_BAD_PARAM;

  /* Allocate and fill an option item */

  option = malloc(sizeof(cmd_line_option_t));
  if (NULL == option)
    return OMPI_ERR_OUT_OF_RESOURCE;
  OBJ_CONSTRUCT((ompi_list_item_t *) option, ompi_list_item_t);

  option->clo_short_name = short_name;
  if (NULL != long_name) {
    option->clo_long_name = strdup(long_name);
  } else {
    option->clo_long_name = NULL;
  }
  option->clo_num_params = num_params;
  if (NULL != desc)
    option->clo_description = strdup(desc);
  else
    option->clo_description = NULL;

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
  ompi_list_item_t *item;
  cmd_line_option_t *option;
  cmd_line_param_t *param;
  bool is_unknown;
  bool is_option;

  /* Thread serialization */

  ompi_mutex_lock(&cmd->lcl_mutex);

  /* Free any parsed results that are already on this handle */

  free_parse_results(cmd);

  /* Analyze each token */

  cmd->lcl_argc = argc;
  cmd->lcl_argv = ompi_argv_copy(argv);

  /* Split groups of multiple short names into individual tokens */

  if (OMPI_SUCCESS != (ret = split_shorts(cmd, ignore_unknown))) {
    ompi_mutex_unlock(&cmd->lcl_mutex);
    return ret;
  }

  /* Now traverse the easy-to-parse sequence of tokens.  Note that
     incrementing i must happen elsehwere; it can't be the third
     clause in the "if" statement. */

  param = NULL;
  option = NULL;
  for (i = 1; i < cmd->lcl_argc; ) {
    is_unknown = false;
    is_option = false;

    /* Are we done?  i.e., did we find the special "--" token?  If so,
       copy everying beyond it into the tail (i.e., don't bother
       copying the "--" into the tail). */

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

    /* Nope, this must be a short name (and, as a result of
       split_shorts(), above, we know that there's only one short name
       in this token) */

    else {
      is_option = true;
      option = find_option(cmd, cmd->lcl_argv[i] + 1);
    }

    /* If we figured out above that this is an option, handle it */

    if (is_option) {
      if (NULL == option) {
        is_unknown = true;
      } else {
        is_unknown = false;
        orig = i;
        ++i;

        /* Suck down the following parameters that belong to this
           option.  If we run out of parameters, or find that any of
           them are the special_empty_param (insertted by
           split_shorts()), then print an error and return. */

        param = malloc(sizeof(cmd_line_param_t));
        if (NULL == param) {
          ompi_mutex_unlock(&cmd->lcl_mutex);
          return OMPI_ERR_OUT_OF_RESOURCE;
        }
        item = (ompi_list_item_t *) param;
        OBJ_CONSTRUCT(item, ompi_list_item_t);
        param->clp_arg = cmd->lcl_argv[i];
        param->clp_option = option;
        param->clp_argc = 0;
        param->clp_argv = NULL;

        /* If we have any parameters to this option, suck down tokens
           starting one beyond the token that we just recognized */

        for (j = 0; j < option->clo_num_params; ++j, ++i) {

          /* If we run out of parameters, error */

          if (i >= cmd->lcl_argc) {
            ompi_output(0, "Error: option \"%s\" did not have enough "
                       "parameters (%d)",
                       cmd->lcl_argv[orig], option->clo_num_params);
            if (NULL != param->clp_argv)
              ompi_argv_free(param->clp_argv);
            free(param);
            param = NULL;
            i = cmd->lcl_argc;
            break;
          } else {
            if (0 == strcmp(cmd->lcl_argv[i], special_empty_token)) {
              ompi_output(0, "Error: option \"%s\" did not have enough "
                         "parameters (%d)",
                         cmd->lcl_argv[orig], option->clo_num_params);
              if (NULL != param->clp_argv)
                ompi_argv_free(param->clp_argv);
              free(param);
              i = cmd->lcl_argc;
              break;
            } 

            /* Otherwise, save this parameter in the argv on the param
               entry */

            else {
              ompi_argv_append(&param->clp_argc, &param->clp_argv, 
                              cmd->lcl_argv[i]);
            }
          }
        }

        /* If we succeeded in all that, save the param to the list on
           the ompi_cmd_line_t handle */

        if (NULL != param) {
          ompi_list_append(&cmd->lcl_params, item);
        }
      }
    }

    /* If we figured out above that this was an unknown option, handle
       it.  Copy everything (including the current token) into the
       tail.  If we're not ignoring unknowns, then print an error. */

    if (is_unknown) {
      if (!ignore_unknown) {
        ompi_output(0, "Error: unknown option \"%s\"", cmd->lcl_argv[i]);
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
  for (item = ompi_list_get_first(&cmd->lcl_options); 
       ompi_list_get_end(&cmd->lcl_options) != item;
       item = ompi_list_get_next(item)) {
    option = (cmd_line_option_t *) item;
    if (NULL != option->clo_description) {

      /* See how much space we need */

      len = 5 + strlen(option->clo_description);
      if ('\0' != option->clo_short_name)
        len += 5;
      if (NULL != option->clo_long_name)
        len += strlen(option->clo_long_name);
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
  if (line != NULL) {
    free(line);
  }
  if (argv != NULL) {
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


/**
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


/**************************************************************************
 * Static functions
 **************************************************************************/

static void free_parse_results(ompi_cmd_line_t *cmd)
{
  ompi_list_item_t *item;

  /* Free the contents of the params list (do not free the list
     itself; it was not allocated from the heap) */

  for (item = ompi_list_remove_first(&cmd->lcl_params);
       NULL != item; 
       item = ompi_list_remove_first(&cmd->lcl_params)) {
    free(item);
  }

  /* Free the argv's */

  if (NULL != cmd->lcl_argv)
    ompi_argv_free(cmd->lcl_argv);
  cmd->lcl_argv = NULL;
  cmd->lcl_argc = 0;

  if (NULL != cmd->lcl_tail_argv)
    ompi_argv_free(cmd->lcl_tail_argv);
  cmd->lcl_tail_argv = NULL;
  cmd->lcl_tail_argc = 0;
}


/*
 * Look for collections of short names and split them into individual
 * short options
 */
static int split_shorts(ompi_cmd_line_t *cmd, bool ignore_unknown)
{
  int i, j, k, len;
  int argc;
  char **argv;
  char *token;
  bool changed;
  char new_token[3];
  cmd_line_option_t *option;

  /* Traverse all the tokens looking for "-multiple_letters".  Note
     that incrementing i must happen elsehwere; it can't be the third
     clause in the "if" statement. */

  argc = 0;
  argv = NULL;
  changed = false;
  if (cmd->lcl_argc > 0) {
    ompi_argv_append(&argc, &argv, cmd->lcl_argv[0]);
  }
  for (i = 1; i < cmd->lcl_argc; ) {
    token = cmd->lcl_argv[i];
    len = strlen(token);

    /* If we hit the special "--" token, copy the rest into the new
       argv */

    if (0 == strcmp(token, "--")) {
      while (i < cmd->lcl_argc) {
        ompi_argv_append(&argc, &argv, cmd->lcl_argv[i]);
        ++i;
      }
    }

    /* If it's a long name, find its option and copy that many
       parmeters into the new argv */

    else if (0 == strncmp(token, "--", 2)) {
      option = find_option(cmd, token + 2);

      /* If we don't find the option, either return an error or pass
         it through unmodified to the new argv */

      if (NULL == option) {
        if (!ignore_unknown) {
          ompi_output(0, "Unrecognized option: '%s'", token);
          return OMPI_ERR_BAD_PARAM;
        } else {
          ompi_argv_append(&argc, &argv, new_token);
        }
      } 

      /* If we do find the option, copy it and all of its parameters
         to the output args.  If we run out of paramters (i.e., no
         more tokens in the original argv), that error will be handled
         at a higher level) */

      else {
        ompi_argv_append(&argc, &argv, token);
        ++i;
        for (k = 0; k < option->clo_num_params; ++k, ++i) {
          if (i < cmd->lcl_argc) {
            ompi_argv_append(&argc, &argv, cmd->lcl_argv[i]);
          } else {
            ompi_argv_append(&argc, &argv, special_empty_token);
          }
        }
      }
    }

    /* If it's a bunch of short names, handle them */

    else if ('-' == token[0] && len >= 2 && '-' != token[1]) {
      changed = true;
      ++i;
      for (j = 1; j < len; ++j) {
        new_token[0] = '-';
        new_token[1] = token[j];
        new_token[2] = '\0';

        option = find_option(cmd, new_token + 1);

        /* If we don't find the option, either return an error or pass
           it through unmodified to the new argv */

        if (NULL == option) {
          if (!ignore_unknown) {
            ompi_output(0, "Unrecognized option: '-%c'", token[j]);
            return OMPI_ERR_BAD_PARAM;
          } else {
            ompi_argv_append(&argc, &argv, new_token);
          }
        } 

        /* If we do find the option, copy it and all of its parameters
           to the output args.  If we run out of paramters (i.e., no
           more tokens in the original argv), that error will be
           handled at a higher level) */

        else {
          ompi_argv_append(&argc, &argv, new_token);
          for (k = 0; k < option->clo_num_params; ++k) {
            if (i < cmd->lcl_argc) {
              ompi_argv_append(&argc, &argv, cmd->lcl_argv[i]);
              ++i;
            } else {
              ompi_argv_append(&argc, &argv, special_empty_token);
            }
          }
        }
      }
    }

    /* It's unrecognized */

    else {
      if (!ignore_unknown) {
        ompi_output(0, "Unrecognized option: '%s'", token);
        return OMPI_ERR_BAD_PARAM;
      } else {
        ompi_argv_append(&argc, &argv, token);
      }
    }
  }

  /* If we changed anything, then replace the argc/argv on cmd */

  if (changed) {
    ompi_argv_free(cmd->lcl_argv);
    cmd->lcl_argc = argc;
    cmd->lcl_argv = argv;
  } else {
    if (NULL != argv) {
      ompi_argv_free(argv);
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
     ompi_cmd_line_t and see if we find a match in either the short or
     long names */

  for (item = ompi_list_get_first(&cmd->lcl_options);
       ompi_list_get_end(&cmd->lcl_options) != item;
       item = ompi_list_get_next(item)) {
    option = (cmd_line_option_t *) item;
    if ((NULL != option->clo_long_name &&
         0 == strcmp(option_name, option->clo_long_name)) ||
        (strlen(option_name) == 1 &&
         option_name[0] == option->clo_short_name)) {
      return option;
    }
  }

  /* Not found */

  return NULL;
}
