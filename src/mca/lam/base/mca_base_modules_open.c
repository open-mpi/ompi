/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "lam/lfc/list.h"
#include "lam/util/strncpy.h"
#include "lam/util/argv.h"
#include "lam/util/output.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"


struct module_name_t {
  lam_list_item_t super;

  char mn_name[MCA_BASE_MAX_MODULE_NAME_LEN];
};
typedef struct module_name_t module_name_t;


/*
 * Local functions
 */
static int open_modules(const char *type_name, int output_id, 
                        lam_list_t *modules_found, 
                        lam_list_t *modules_available,
                        char **requested_module_names);
static int parse_requested(int mca_param, char ***requested_module_names);


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_base_modules_open(const char *type_name, int output_id,
                          const mca_base_module_t **static_modules,
                          lam_list_t *modules_available)
{
  int ret;
  lam_list_item_t *item;
  lam_list_t modules_found;
  char **requested_module_names;
  int param_verbose = -1;
  int param_type;

  /* Register MCA parameter */

  param_verbose = mca_base_param_register_string(type_name, "base", "verbose", 
                                                 NULL, NULL);
  param_type = mca_base_param_register_string(type_name, "base", NULL, 
                                                 NULL, NULL);

  /* Setup verbosity for this MCA type */

#if 0
  mca_base_set_verbose(param_verbose, &lds,
                       &mca_pcm_verbose, &mca_pcm_did);
#endif
  lam_output_verbose(10, output_id, "open: Looking for modules");

  /* Find and load all available modules */

  if (LAM_SUCCESS != 
      mca_base_module_find(NULL, type_name, static_modules, &modules_found)) {
    return LAM_ERROR;
  }

  /* See if one or more specific modules were requested */

  ret = parse_requested(param_type, &requested_module_names);
  if (LAM_SUCCESS == ret) {
    ret = open_modules(type_name, output_id, &modules_found, modules_available,
                       requested_module_names);
  }

  /* Free resources */

  for (item = lam_list_remove_first(&modules_found); NULL != item;
       item = lam_list_remove_first(&modules_found)) {
    free(item);
  }
  if (NULL != requested_module_names) {
    lam_argv_free(requested_module_names);
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

  if (LAM_ERROR == mca_base_param_lookup_string(mca_param, &requested)) {
    return LAM_ERROR;
  }
  if (NULL == requested) {
    return LAM_SUCCESS;
  }

  /* Loop over all names (yes, this could be more clever, but it's
     nice and obvious this way!) */

  start = requested;
  comma = strchr(start, ',');
  while (NULL != comma) {
    *comma = '\0';
    lam_argv_append(&argc, requested_module_names, start);

    start = comma + 1;
    comma = strchr(start, ',');
  }

  /* The last name */

  lam_argv_append(&argc, requested_module_names, start);

  /* All done */

  return LAM_SUCCESS;
}


/*
 * Traverse the entire list of found modules (a list of
 * mca_base_module_t instances).  If the requested_module_names array
 * is empty, or the name of each module in the list of found modules
 * is in the requested_modules_array, try to open it.  If it opens,
 * add it to the modules_available list.
 */
static int open_modules(const char *type_name, int output_id, 
                        lam_list_t *modules_found, 
                        lam_list_t *modules_available,
                        char **requested_module_names)
{
  int i;
  lam_list_item_t *item;
  const mca_base_module_t *module;
  mca_base_module_list_item_t *mli;
  bool acceptable;
  bool called_open;
  bool opened;

  /* Announce */

  if (NULL == requested_module_names) {
    lam_output_verbose(10, output_id,
                       "open: looking for any %s modules", type_name);
  } else {
    lam_output_verbose(10, output_id,
                       "open: looking for specific %s modules:", type_name);
    for (i = 0; NULL != requested_module_names[i]; ++i) {
      lam_output_verbose(10, output_id, "open:   %s", 
                         requested_module_names[i]);
    }
  }

  /* Traverse the list of found modules */

  OBJ_CONSTRUCT(modules_available, lam_list_t);
  for (item = lam_list_get_first(modules_found);
       lam_list_get_end(modules_found) != item;
       item = lam_list_get_next(item)) {
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
      lam_output_verbose(10, output_id, "open: found loaded module %s",
                         module->mca_module_name);

      if (NULL == module->mca_open_module) {
        opened = true; 
        lam_output_verbose(10, output_id, 
                           "open: module %s has no open function",
                           module->mca_module_name);
      } else {
        called_open = true;
        if (MCA_SUCCESS == module->mca_open_module()) {
          opened = true;
          lam_output_verbose(10, output_id, 
                             "open: module %s open function successful",
                             module->mca_module_name);
        } else {
          lam_output_verbose(10, output_id, 
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
          lam_output_verbose(10, output_id, 
                             "open: module %s closed",
                             module->mca_module_name);
          called_open = false;
        }
        mca_base_module_registry_release(module);
        lam_output_verbose(10, output_id, 
                           "open: module %s unloaded", 
                           module->mca_module_name);
      }

      /* If it did open, register its "priority" MCA parameter (if it
         doesn't already have one) and save it in the opened_modules
         list */

      else {
        if (LAM_ERROR == mca_base_param_find(type_name, 
                                             module->mca_module_name,
                                             "priority")) {
          mca_base_param_register_int(type_name,
                                      module->mca_module_name,
                                      "priority", NULL, 0);
        }

        mli = malloc(sizeof(mca_base_module_list_item_t));
        if (NULL == mli) {
          return LAM_ERROR;
        }
        OBJ_CONSTRUCT(&mli->super, lam_list_item_t);
        mli->mli_module = module;
        lam_list_append(modules_available, (lam_list_item_t *) mli);
      }
    }
  }

  /* All done */

  return LAM_SUCCESS;
}
