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


struct module_name_t {
  ompi_list_item_t super;

  char mn_name[MCA_BASE_MAX_MODULE_NAME_LEN];
};
typedef struct module_name_t module_name_t;


/*
 * Local functions
 */
static int open_modules(const char *type_name, int output_id, 
                        ompi_list_t *modules_found, 
                        ompi_list_t *modules_available,
                        char **requested_module_names);
static int parse_requested(int mca_param, char ***requested_module_names);


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_base_modules_open(const char *type_name, int output_id,
                          const mca_base_module_t **static_modules,
                          ompi_list_t *modules_available)
{
  int ret;
  ompi_list_item_t *item;
  ompi_list_t modules_found;
  char **requested_module_names;
  int param_verbose = -1;
  int param_type = -1;
  int verbose_level;

  /* Register MCA parameters */

  param_verbose = mca_base_param_register_int(type_name, "base", 
                                              "verbose", NULL, 10);
  param_type = mca_base_param_register_string(type_name, "base", NULL, 
                                              type_name, NULL);

  /* Setup verbosity for this MCA type */

  mca_base_param_lookup_int(param_verbose, &verbose_level);
  if (output_id != 0) {
    ompi_output_set_verbosity(output_id, verbose_level);
  }
  ompi_output_verbose(10, output_id, "open: Looking for modules");

  /* Find and load all available modules */

  if (OMPI_SUCCESS != 
      mca_base_module_find(NULL, type_name, static_modules, &modules_found)) {
    return OMPI_ERROR;
  }

  /* See if one or more specific modules were requested */

  ret = parse_requested(param_type, &requested_module_names);
  if (OMPI_SUCCESS == ret) {
    ret = open_modules(type_name, output_id, &modules_found, modules_available,
                       requested_module_names);
  }

  /* Free resources */

  for (item = ompi_list_remove_first(&modules_found); NULL != item;
       item = ompi_list_remove_first(&modules_found)) {
    free(item);
  }
  if (NULL != requested_module_names) {
    ompi_argv_free(requested_module_names);
  }

  /* All done */

  return ret;
}


static int parse_requested(int mca_param, char ***requested_module_names)
{
  char *requested;
  char *comma;
  char *start;
  int argc;

  *requested_module_names = NULL;

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
    ompi_argv_append(&argc, requested_module_names, start);

    start = comma + 1;
    comma = strchr(start, ',');
  }

  /* The last name */

  ompi_argv_append(&argc, requested_module_names, start);

  /* All done */

  return OMPI_SUCCESS;
}


/*
 * Traverse the entire list of found modules (a list of
 * mca_base_module_t instances).  If the requested_module_names array
 * is empty, or the name of each module in the list of found modules
 * is in the requested_modules_array, try to open it.  If it opens,
 * add it to the modules_available list.
 */
static int open_modules(const char *type_name, int output_id, 
                        ompi_list_t *modules_found, 
                        ompi_list_t *modules_available,
                        char **requested_module_names)
{
  int i;
  ompi_list_item_t *item;
  const mca_base_module_t *module;
  mca_base_module_list_item_t *mli;
  bool acceptable;
  bool called_open;
  bool opened;

  /* Announce */

  if (NULL == requested_module_names) {
    ompi_output_verbose(10, output_id,
                       "open: looking for any %s modules", type_name);
  } else {
    ompi_output_verbose(10, output_id,
                       "open: looking for specific %s modules:", type_name);
    for (i = 0; NULL != requested_module_names[i]; ++i) {
      ompi_output_verbose(10, output_id, "open:   %s", 
                         requested_module_names[i]);
    }
  }

  /* Traverse the list of found modules */

  OBJ_CONSTRUCT(modules_available, ompi_list_t);
  for (item = ompi_list_get_first(modules_found);
       ompi_list_get_end(modules_found) != item;
       item = ompi_list_get_next(item)) {
    mli = (mca_base_module_list_item_t *) item;
    module = mli->mli_module;

    /* Do we need to check for specific modules? */

    if (NULL != requested_module_names) {
      acceptable = false;
      for (i = 0; NULL != requested_module_names[i]; ++i) {
        if (0 == strcmp(requested_module_names[i], module->mca_module_name)) {
          acceptable = true;
          break;
        }
      }
    } else {
      acceptable = true;
    }

    /* If this is an acceptable module, try to open it */

    if (acceptable) {
      opened = called_open = false;
      ompi_output_verbose(10, output_id, "open: found loaded module %s",
                         module->mca_module_name);

      if (NULL == module->mca_open_module) {
        opened = true; 
        ompi_output_verbose(10, output_id, 
                           "open: module %s has no open function",
                           module->mca_module_name);
      } else {
        called_open = true;
        if (MCA_SUCCESS == module->mca_open_module()) {
          opened = true;
          ompi_output_verbose(10, output_id, 
                             "open: module %s open function successful",
                             module->mca_module_name);
        } else {
          ompi_output_verbose(10, output_id, 
                             "open: module %s open function failed",
                             module->mca_module_name);
        }
      }

      /* If it didn't open, close it out and get rid of it */

      if (!opened) {
        if (called_open) {
          if (NULL != module->mca_close_module) {
            module->mca_close_module();
          }
          ompi_output_verbose(10, output_id, 
                             "open: module %s closed",
                             module->mca_module_name);
          called_open = false;
        }
        mca_base_module_repository_release(module);
        ompi_output_verbose(10, output_id, 
                           "open: module %s unloaded", 
                           module->mca_module_name);
      }

      /* If it did open, register its "priority" MCA parameter (if it
         doesn't already have one) and save it in the opened_modules
         list */

      else {
        if (OMPI_ERROR == mca_base_param_find(type_name, 
                                             module->mca_module_name,
                                             "priority")) {
          mca_base_param_register_int(type_name,
                                      module->mca_module_name,
                                      "priority", NULL, 0);
        }

        mli = malloc(sizeof(mca_base_module_list_item_t));
        if (NULL == mli) {
          return OMPI_ERROR;
        }
        OBJ_CONSTRUCT(&mli->super, ompi_list_item_t);
        mli->mli_module = module;
        ompi_list_append(modules_available, (ompi_list_item_t *) mli);
      }
    }
  }

  /* All done */

  return OMPI_SUCCESS;
}
