/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "class/ompi_list.h"
#include "util/strncpy.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"


struct component_name_t {
  ompi_list_item_t super;

  char mn_name[MCA_BASE_MAX_COMPONENT_NAME_LEN];
};
typedef struct component_name_t component_name_t;


/*
 * Local functions
 */
static int open_components(const char *type_name, int output_id, 
                           ompi_list_t *components_found, 
                           ompi_list_t *components_available,
                           char **requested_component_names);
static int parse_requested(int mca_param, char ***requested_component_names);


/**
 * Function for finding and opening either all MCA components, or the
 * one that was specifically requested via a MCA parameter.
 */
int mca_base_components_open(const char *type_name, int output_id,
                             const mca_base_component_t **static_components,
                             ompi_list_t *components_available)
{
  int ret;
  ompi_list_item_t *item;
  ompi_list_t components_found;
  char **requested_component_names;
  int param_verbose = -1;
  int param_type = -1;
  int verbose_level;

  /* Register MCA parameters */

  param_verbose = mca_base_param_register_int(type_name, "base", 
                                              "verbose", NULL, 0);
  param_type = mca_base_param_register_string(type_name, "base", NULL, 
                                              type_name, NULL);

  /* Setup verbosity for this MCA type */

  mca_base_param_lookup_int(param_verbose, &verbose_level);
  if (output_id != 0) {
    ompi_output_set_verbosity(output_id, verbose_level);
  }
  ompi_output_verbose(10, output_id, "mca: base: open: Looking for components");

  /* Find and load all available components */

  if (OMPI_SUCCESS != 
      mca_base_component_find(NULL, type_name, static_components,
                              &components_found)) {
    return OMPI_ERROR;
  }

  /* See if one or more specific components were requested */

  ret = parse_requested(param_type, &requested_component_names);
  if (OMPI_SUCCESS == ret) {
    ret = open_components(type_name, output_id, &components_found, 
                          components_available,
                          requested_component_names);
  }

  /* Free resources */

  for (item = ompi_list_remove_first(&components_found); NULL != item;
       item = ompi_list_remove_first(&components_found)) {
    OBJ_RELEASE(item);
  }
  if (NULL != requested_component_names) {
    ompi_argv_free(requested_component_names);
  }

  /* All done */

  return ret;
}


static int parse_requested(int mca_param, char ***requested_component_names)
{
  char *requested;
  char *comma;
  char *start;
  int argc;

  *requested_component_names = NULL;

  /* See if the user requested anything */

  if (OMPI_ERROR == mca_base_param_lookup_string(mca_param, &requested)) {
    return OMPI_ERROR;
  }
  if (NULL == requested) {
    return OMPI_SUCCESS;
  }

  /* Loop over all names (yes, this could be more clever, but it's
     nice and obvious this way!) */

  start = requested;
  comma = strchr(start, ',');
  while (NULL != comma) {
    *comma = '\0';
    ompi_argv_append(&argc, requested_component_names, start);

    start = comma + 1;
    comma = strchr(start, ',');
  }

  /* The last name */

  ompi_argv_append(&argc, requested_component_names, start);

  /* All done */

  return OMPI_SUCCESS;
}


/*
 * Traverse the entire list of found components (a list of
 * mca_base_component_t instances).  If the requested_component_names
 * array is empty, or the name of each component in the list of found
 * components is in the requested_components_array, try to open it.
 * If it opens, add it to the components_available list.
 */
static int open_components(const char *type_name, int output_id, 
                           ompi_list_t *components_found, 
                           ompi_list_t *components_available,
                           char **requested_component_names)
{
  int i;
  ompi_list_item_t *item;
  const mca_base_component_t *component;
  mca_base_component_list_item_t *cli;
  bool acceptable;
  bool called_open;
  bool opened;

  /* Announce */

  if (NULL == requested_component_names) {
    ompi_output_verbose(10, output_id,
                        "mca: base: open: "
                        "looking for any %s components", type_name);
  } else {
    ompi_output_verbose(10, output_id,
                        "mca: base: open: looking for specific %s components:", 
                        type_name);
    for (i = 0; NULL != requested_component_names[i]; ++i) {
      ompi_output_verbose(10, output_id, "mca: base: open:   %s", 
                          requested_component_names[i]);
    }
  }

  /* Traverse the list of found components */

  OBJ_CONSTRUCT(components_available, ompi_list_t);
  for (item = ompi_list_get_first(components_found);
       ompi_list_get_end(components_found) != item;
       item = ompi_list_get_next(item)) {
    cli = (mca_base_component_list_item_t *) item;
    component = cli->cli_component;

    /* Do we need to check for specific components? */

    if (NULL != requested_component_names) {
      acceptable = false;
      for (i = 0; NULL != requested_component_names[i]; ++i) {
        if (0 == strcmp(requested_component_names[i], 
                        component->mca_component_name)) {
          acceptable = true;
          break;
        }
      }
    } else {
      acceptable = true;
    }

    /* If this is an acceptable component, try to open it */

    if (acceptable) {
      opened = called_open = false;
      ompi_output_verbose(10, output_id, 
                          "mca: base: open: found loaded component %s",
                          component->mca_component_name);
      
      if (NULL == component->mca_open_component) {
        opened = true; 
        ompi_output_verbose(10, output_id, 
                            "mca: base: open: "
                            "component %s has no open function",
                            component->mca_component_name);
      } else {
        called_open = true;
        if (MCA_SUCCESS == component->mca_open_component()) {
          opened = true;
          ompi_output_verbose(10, output_id, 
                              "mca: base: open: "
                              "component %s open function successful",
                              component->mca_component_name);
        } else {
          ompi_output_verbose(10, output_id, 
                              "mca: base: open: "
                              "component %s open function failed",
                              component->mca_component_name);
        }
      }

      /* If it didn't open, close it out and get rid of it */

      if (!opened) {
        if (called_open) {
          if (NULL != component->mca_close_component) {
            component->mca_close_component();
          }
          ompi_output_verbose(10, output_id, 
                              "mca: base: open: component %s closed",
                              component->mca_component_name);
          called_open = false;
        }
        mca_base_component_repository_release(component);
        ompi_output_verbose(10, output_id, 
                            "mca: base: open: component %s unloaded", 
                            component->mca_component_name);
      }

      /* If it did open, register its "priority" MCA parameter (if it
         doesn't already have one) and save it in the opened_components
         list */

      else {
        if (OMPI_ERROR == mca_base_param_find(type_name, 
                                              component->mca_component_name,
                                              "priority")) {
          mca_base_param_register_int(type_name,
                                      component->mca_component_name,
                                      "priority", NULL, 0);
        }

        cli = OBJ_NEW(mca_base_component_list_item_t);
        if (NULL == cli) {
          return OMPI_ERR_OUT_OF_RESOURCE;
        }
        cli->cli_component = component;
        ompi_list_append(components_available, (ompi_list_item_t *) cli);
      }
    }
  }

  /* All done */

  return OMPI_SUCCESS;
}
