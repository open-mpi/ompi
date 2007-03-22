/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/class/opal_list.h"
#include "opal/util/strncpy.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/constants.h"

struct component_name_t {
  opal_list_item_t super;

  char mn_name[MCA_BASE_MAX_COMPONENT_NAME_LEN];
};
typedef struct component_name_t component_name_t;


/*
 * Local variables
 */
static bool show_errors = false;
static const char negate = '^';


/*
 * Local functions
 */
static int parse_requested(int mca_param, bool *include_mode,
                           char ***requested_component_names);
static int open_components(const char *type_name, int output_id, 
                           opal_list_t *src, opal_list_t *dest);
static int distill(bool include_mode, const char *type_name,
                   int output_id, opal_list_t *src, opal_list_t *dest,
                   char **names);


/**
 * Function for finding and opening either all MCA components, or the
 * one that was specifically requested via a MCA parameter.
 */
int mca_base_components_open(const char *type_name, int output_id,
                             const mca_base_component_t **static_components,
                             opal_list_t *components_available,
                             bool open_dso_components)
{
  int ret, param;
  opal_list_item_t *item;
  opal_list_t components_found, components_distilled;
  char **requested_component_names;
  int param_verbose = -1;
  int param_type = -1;
  int verbose_level;
  char *str;
  bool include_mode;
  bool distilled = false;

  /* Register MCA parameters */

  asprintf(&str, "Default selection set of components for the %s framework (<none> means \"use all components that can be found\")", type_name);
  param_type = 
      mca_base_param_reg_string_name(type_name, NULL, str, 
                                     false, false, NULL, NULL);
  free(str);

  asprintf(&str, "Verbosity level for the %s framework (0 = no verbosity)", type_name);
  param_verbose = 
      mca_base_param_reg_int_name(type_name, "base_verbose",
                                  str, false, false, 0, NULL);
  free(str);

  param = mca_base_param_find("mca", NULL, "component_show_load_errors");
  mca_base_param_lookup_int(param, &ret);
  show_errors = OPAL_INT_TO_BOOL(ret);

  /* Setup verbosity for this MCA type */

  mca_base_param_lookup_int(param_verbose, &verbose_level);
  if (output_id != 0) {
    opal_output_set_verbosity(output_id, verbose_level);
  }
  opal_output_verbose(10, output_id, 
                      "mca: base: components_open: Looking for %s components",
                      type_name);

  /* Find and load all available components */

  if (OPAL_SUCCESS != 
      mca_base_component_find(NULL, type_name, static_components,
                              &components_found, open_dso_components)) {
    return OPAL_ERROR;
  }

  /* See if one or more specific components were requested */

  ret = parse_requested(param_type, &include_mode, &requested_component_names);
  if (OPAL_SUCCESS == ret) {
      ret = distill(include_mode, type_name, output_id, &components_found,
                    &components_distilled, requested_component_names);
      distilled = true;
  }

  /* Now open whatever we have left */

  if (OPAL_SUCCESS == ret) {
      ret = open_components(type_name, output_id,
                            &components_distilled, components_available);
  }

  /* Free resources */

  for (item = opal_list_remove_first(&components_found); NULL != item;
       item = opal_list_remove_first(&components_found)) {
      OBJ_RELEASE(item);
  }
  OBJ_DESTRUCT(&components_found);
  if (distilled) {
      for (item = opal_list_remove_first(&components_distilled); NULL != item;
           item = opal_list_remove_first(&components_distilled)) {
          OBJ_RELEASE(item);
      }
      OBJ_DESTRUCT(&components_distilled);
  }
  if (NULL != requested_component_names) {
      opal_argv_free(requested_component_names);
  }

  /* All done */

  return ret;
}


static int parse_requested(int mca_param, bool *include_mode,
                           char ***requested_component_names)
{
  int i;
  char *requested, *requested_orig;

  *requested_component_names = NULL;
  *include_mode = true;

  /* See if the user requested anything */

  if (OPAL_ERROR == mca_base_param_lookup_string(mca_param, &requested)) {
    return OPAL_ERROR;
  }
  if (NULL == requested || 0 == strlen(requested)) {
    return OPAL_SUCCESS;
  }
  requested_orig = requested;

  /* Are we including or excluding?  We only allow the negate
     character to be the *first* character of the value (but be nice
     and allow any number of negate characters in the beginning). */

  while (negate == requested[0] && '\0' != requested[0]) {
      *include_mode = false;
      ++requested;
  }

  /* Double check to ensure that the user did not specify the negate
     character anywhere else in the value. */

  i = 0;
  while ('\0' != requested[i]) {
      if (negate == requested[i]) {
          opal_show_help("help-mca-base.txt", 
                         "framework-param:too-many-negates",
                         true, requested_orig);
          free(requested_orig);
          return OPAL_ERROR;
      }
      ++i;
  }

  /* Split up the value into individual component names */

  *requested_component_names = opal_argv_split(requested, ',');

  /* All done */

  free(requested_orig);
  return OPAL_SUCCESS;
}


/*
 * Parse the list of found components and factor in the included /
 * excluded names to come up with a distilled list of components that
 * we should try to open.
 */
static int distill(bool include_mode, const char *type_name,
                   int output_id, opal_list_t *src, opal_list_t *dest,
                   char **names)
{
    int i;
    bool good;
    opal_list_item_t *item, *next;
    const mca_base_component_t *component;
    mca_base_component_list_item_t *cli;

    opal_output_verbose(10, output_id,
                        "mca: base: components_open: "
                        "distilling %s components", type_name);
    OBJ_CONSTRUCT(dest, opal_list_t);

    /* Bozo case */

    if (NULL == names) {
        opal_output_verbose(10, output_id,
                            "mca: base: components_open: "
                            "accepting all %s components", type_name);
        opal_list_join(dest, opal_list_get_end(dest), src);
        return OPAL_SUCCESS;
    }

    /* Are we including components? */

    if (include_mode) {
        opal_output_verbose(10, output_id,
                            "mca: base: components_open: "
                            "including %s components", type_name);

        /* Go through all the components and only keep the ones that
           are specifically mentioned in the list */

        for (i = 0; NULL != names[i]; ++i) {
            good = false;

            for (item = opal_list_get_first(src);
                 opal_list_get_end(src) != item;
                 item = next) {
                next = opal_list_get_next(item);
                cli = (mca_base_component_list_item_t *) item;
                component = cli->cli_component;
                if (0 == strcmp(names[i], component->mca_component_name)) {
                    opal_list_remove_item(src, item);
                    opal_list_append(dest, item);
                    good = true;
                    break;
                }
            }

            if (good) {
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open:   "
                                    "%s --> included", names[i]);
            } else {
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open:   "
                                    "%s --> not found", names[i]);
            }
        }
    }

    /* No, we are excluding components */
   
    else {
        opal_output_verbose(10, output_id,
                            "mca: base: components_open: "
                            "excluding %s components", type_name);

        /* Go through all the components and only keep the ones that
           are specifically mentioned in the list */

        for (item = opal_list_get_first(src);
             opal_list_get_end(src) != item;
             item = next) {
            next = opal_list_get_next(item);
            good = true;
            cli = (mca_base_component_list_item_t *) item;
            component = cli->cli_component;

            for (i = 0; NULL != names[i]; ++i) {
                if (0 == strcmp(names[i], component->mca_component_name)) {
                    good = false;
                    break;
                }
            }

            if (!good) {
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open:   "
                                    "%s --> excluded", 
                                    component->mca_component_name);
            } else {
                opal_list_remove_item(src, item);
                opal_list_append(dest, item);
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open:   "
                                    "%s --> included",
                                    component->mca_component_name);
            }
        }
    }

    /* All done */

    return OPAL_SUCCESS;
}


/*
 * Traverse the entire list of found components (a list of
 * mca_base_component_t instances).  If the requested_component_names
 * array is empty, or the name of each component in the list of found
 * components is in the requested_components_array, try to open it.
 * If it opens, add it to the components_available list.
 */
static int open_components(const char *type_name, int output_id, 
                           opal_list_t *src, opal_list_t *dest)
{
    opal_list_item_t *item;
    const mca_base_component_t *component;
    mca_base_component_list_item_t *cli;
    bool called_open;
    bool opened;
    
    /* Announce */
    
    opal_output_verbose(10, output_id,
                        "mca: base: components_open: opening %s components",
                        type_name);
    
    /* Traverse the list of found components */
    
    OBJ_CONSTRUCT(dest, opal_list_t);
    for (item = opal_list_get_first(src);
         opal_list_get_end(src) != item;
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = cli->cli_component;
        
        opened = called_open = false;
        opal_output_verbose(10, output_id, 
                            "mca: base: components_open: found loaded component %s",
                            component->mca_component_name);
        
        if (NULL == component->mca_open_component) {
            opened = true; 
            opal_output_verbose(10, output_id, 
                                "mca: base: components_open: "
                                "component %s has no open function",
                                component->mca_component_name);
        } else {
            called_open = true;
            if (MCA_SUCCESS == component->mca_open_component()) {
                opened = true;
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open: "
                                    "component %s open function successful",
                                    component->mca_component_name);
            } else {
                /* We may end up displaying this twice, but it may go
                   to separate streams.  So better to be redundant
                   than to not display the error in the stream where
                   it was expected. */
                
                if (show_errors) {
                    opal_output(0, "mca: base: components_open: "
                                "component %s / %s open function failed",
                                component->mca_type_name,
                                component->mca_component_name);
                }
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open: "
                                    "component %s open function failed",
                                    component->mca_component_name);
            }
        }
        
        /* If it didn't open, close it out and get rid of it */
        
        if (!opened) {
            char *name;
            if (called_open) {
                if (NULL != component->mca_close_component) {
                    component->mca_close_component();
                }
                opal_output_verbose(10, output_id, 
                                    "mca: base: components_open: component %s closed",
                                    component->mca_component_name);
                called_open = false;
            }
            name = strdup(component->mca_component_name);
            mca_base_component_repository_release(component);
            opal_output_verbose(10, output_id, 
                                "mca: base: components_open: component %s unloaded", 
                                name);
            free(name);
        }
        
        /* If it did open, register its "priority" MCA parameter (if
           it doesn't already have one) and save it in the
           opened_components list */
        
        else {
            if (OPAL_ERROR == mca_base_param_find(type_name, 
                                                  component->mca_component_name,
                                                  "priority")) {
                mca_base_param_register_int(type_name,
                                            component->mca_component_name,
                                            "priority", NULL, 0);
            }
            
            cli = OBJ_NEW(mca_base_component_list_item_t);
            if (NULL == cli) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            cli->cli_component = component;
            opal_list_append(dest, (opal_list_item_t *) cli);
        }
    }
    
    /* All done */
    
    return OPAL_SUCCESS;
}
