/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/* Ensure to get the right <ltdl.h> */ 
#include "mca/ltdl.h"

#include "constants.h"
#include "util/output.h"
#include "lfc/lam_list.h"
#include "mca/mca.h"
#include "mca/base/base.h"


/*
 * Private types
 */
typedef enum module_status {
  UNVISITED,
  FAILED_TO_LOAD,
  CHECKING_CYCLE,
  LOADED,

  STATUS_MAX
} module_status_t;

struct module_file_item_t {
  lam_list_item_t super;

  char type[MCA_BASE_MAX_TYPE_NAME_LEN];
  char name[MCA_BASE_MAX_MODULE_NAME_LEN];
  char basename[LAM_PATH_MAX];
  char filename[LAM_PATH_MAX];
  module_status_t status;
};
typedef struct module_file_item_t module_file_item_t;

struct dependency_item_t {
  lam_list_item_t super;

  module_file_item_t *di_module_file_item;
};
typedef struct dependency_item_t dependency_item_t;

struct ltfn_data_holder_t {
  char type[MCA_BASE_MAX_TYPE_NAME_LEN];
  char name[MCA_BASE_MAX_MODULE_NAME_LEN];
};
typedef struct ltfn_data_holder_t ltfn_data_holder_t;


/*
 * Private functions
 */
static void find_dyn_modules(const char *path, const char *type, 
                             const char *name, lam_list_t *found_modules);
static int save_filename(const char *filename, lt_ptr data);
static int open_module(module_file_item_t *target_file, 
                       lam_list_t *found_modules);
static int check_laminfo(module_file_item_t *target_file, 
                         lam_list_t *dependencies,
                         lam_list_t *found_modules);
static int check_dependency(char *line, module_file_item_t *target_file, 
                            lam_list_t *dependencies, 
                            lam_list_t *found_modules);
static void free_dependency_list(lam_list_t *dependencies);


/*
 * Private variables
 */
static const char *laminfo_suffix = ".laminfo";
static const char *key_dependency = "dependency=";
static const char module_template[] = "mca_%s_";
static lam_list_t found_files;


/*
 * Function to find as many modules of a given type as possible.  This
 * includes statically-linked in modules as well as opening up a
 * directory and looking for shared-library MCA modules of the
 * appropriate type (load them if available).
 *
 * Return one consolidated array of (mca_base_module_t*) pointing to all
 * available modules.
 */
int mca_base_module_find(const char *directory, const char *type, 
                         const mca_base_module_t *static_modules[], 
                         lam_list_t *found_modules)
{
  int i;
  mca_base_module_list_item_t *mli;

  /* Find all the modules that were statically linked in */

  OBJ_CONSTRUCT(found_modules, lam_list_t);
  for (i = 0; NULL != static_modules[i]; ++i) {
    mli = malloc(sizeof(mca_base_module_list_item_t));
    if (NULL == mli) {
      return LAM_ERR_OUT_OF_RESOURCE;
    }
    OBJ_CONSTRUCT(mli, lam_list_item_t);
    mli->mli_module = static_modules[i];
    lam_list_append(found_modules, (lam_list_item_t *) mli);
  }

  /* Find any available dynamic modules in the specified directory */

  find_dyn_modules(directory, type, NULL, found_modules);

  /* All done */

  return LAM_SUCCESS;
}


/*
 * Open up all directories in a given path and search for modules of
 * the specified type (and possibly of a given name).
 *
 * Note that we use our own path iteration functionality (vs. ltdl's
 * lt_dladdsearchdir() functionality) because we need to look at
 * companion .laminfo files in the same directory as the library to
 * generate dependencies, etc.  If we use the plain lt_dlopen()
 * functionality, we would not get the directory name of the file
 * finally opened in recursive dependency traversals.
 */
static void find_dyn_modules(const char *path, const char *type_name, 
                             const char *name, lam_list_t *found_modules)
{
  ltfn_data_holder_t params;
  char *path_to_use, *dir, *end, *param;
  module_file_item_t *file;
  lam_list_item_t *cur;

  strcpy(params.type, type_name);

  if (NULL == name) {
    params.name[0] = '\0';
    lam_output_verbose(0, 40, " looking for all dynamic %s MCA modules", 
                       type_name, NULL);
  } else {
    strcpy(params.name, name);
    lam_output_verbose(0, 40,
                       " looking for dynamic %s MCA module named \"%s\"",
                       type_name, name, NULL);
  }

  /* If directory is NULL, iterate over the set of directories
     specified by the MCA param mca_base_module_path.  If path is not
     NULL, then use that as the path. */

  param = NULL;
  if (NULL == path) {
    mca_base_param_lookup_string(mca_base_param_module_path, &param);
    dir = param;
  }
  if (NULL == dir) {
    path_to_use = NULL;
  } else {
    path_to_use = strdup(dir);
  }

  /* Iterate over all the files in the directories in the path and
     make a master array of all the matching filenames that we
     find. */

  OBJ_CONSTRUCT(&found_files, lam_list_t);
  dir = path_to_use;
  do {
    end = strchr(dir, ':');
    if (NULL != end) {
      *end = '\0';
    }
    if (0 != lt_dlforeachfile(dir, save_filename, &params)) {
      break;
    }
    dir = end + 1;
  } while (NULL != end);

  /* Iterate through all the filenames that we found.  Since one
     module may [try to] call another to be loaded, only try to load
     the UNVISITED files.  Also, ignore the return code -- basically,
     give every file one chance to try to load.  If they load, great.
     If not, great. */

  for (cur = lam_list_get_first(&found_files); 
       lam_list_get_end(&found_files) != cur;
       cur = lam_list_get_next(cur)) {
    file = (module_file_item_t *) cur;
    if (UNVISITED == file->status)
      open_module(file, found_modules);
  }

  /* So now we have a final list of loaded modules.  We can free all
     the file information. */

  for (cur = lam_list_get_first(&found_files); 
       lam_list_get_end(&found_files) != cur; ) {
    file = (module_file_item_t *) cur;
    cur = lam_list_get_next(cur);
    free(file);
    lam_list_remove_first(&found_files);
  }

  /* All done */

  if (NULL != param) {
    free(param);
  }
  /* JMS This list memory management may change */
#if 0
  lam_list_destruct(&found_files);
#endif
  free(path_to_use);
}


/*
 * Given a filename, see if it appears to be of the proper filename
 * format.  If so, save it in the array so that we can process it
 * later.
 */
static int save_filename(const char *filename, lt_ptr data)
{
  int len, prefix_len, total_len;
  char *prefix;
  const char *basename;
  module_file_item_t *module_file;
  ltfn_data_holder_t *params = (ltfn_data_holder_t *) data;

  /* Check to see if the file is named what we expect it to be
     named */

  len = sizeof(module_template) + strlen(params->type) + 32;
  if (NULL != params->name) {
    len += strlen(params->name);
  }
  prefix = malloc(len);
  snprintf(prefix, len, module_template, params->type);
  prefix_len = strlen(prefix);
  if (NULL != params->name) {
    strcat(prefix, params->name);
  }
  total_len = strlen(prefix);

  basename = strrchr(filename, '/');
  if (NULL == basename) {
    basename = filename;
  } else {
    basename += 1;
  }

  if (0 != strncmp(basename, prefix, total_len)) {
    free(prefix);
    return 0;
  }

  /* Save all the info and put it in the list of found modules */

  module_file = malloc(sizeof(module_file_item_t));
  if (NULL == module_file) {
    return LAM_ERR_OUT_OF_RESOURCE;
  }
  OBJ_CONSTRUCT(module_file, lam_list_item_t);
  strcpy(module_file->type, params->type);
  strcpy(module_file->name, basename + prefix_len);
  strcpy(module_file->basename, basename);
  strcpy(module_file->filename, filename);
  module_file->status = UNVISITED;
  lam_list_append(&found_files, (lam_list_item_t *) module_file);

  /* All done */

  free(prefix);
  return 0;
}


/*
 * Open a module, chasing down its dependencies first, if possible.
 */
static int open_module(module_file_item_t *target_file, 
                       lam_list_t *found_modules)
{
  int len;
  lt_dlhandle module_handle;
  mca_base_module_t *module_struct;
  char *struct_name;
  lam_list_t dependencies;
  lam_list_item_t *cur;
  mca_base_module_list_item_t *mitem;
  dependency_item_t *ditem;

  lam_output_verbose(0, 40, " examining dyanmic %s MCA module \"%s\"",
                     target_file->type, target_file->name, NULL);
  lam_output_verbose(0, 40, " %s", target_file->filename, NULL);

  /* Was this module already loaded (e.g., via dependency)? */

  if (LOADED == target_file->status) {
    lam_output_verbose(0, 40, " already loaded (ignored)", NULL);
    return LAM_SUCCESS;
  }

  /* Ensure that this module is not already loaded (should only happen
     if it was statically loaded).  It's an error if it's already
     loaded because we're evaluating this file -- not this module.
     Hence, returning LAM_ERR_PARAM indicates that the *file* failed
     to load, not the module. */

  for (cur = lam_list_get_first(found_modules); 
       lam_list_get_end(found_modules) != cur;
       cur = lam_list_get_next(cur)) {
    mitem = (mca_base_module_list_item_t *) cur;
    if (0 == strcmp(mitem->mli_module->mca_type_name, target_file->type) &&
        0 == strcmp(mitem->mli_module->mca_module_name, target_file->name)) {
      lam_output_verbose(0, 40, " already loaded (ignored)", NULL);
      target_file->status = FAILED_TO_LOAD;
      return LAM_ERR_BAD_PARAM;
    }
  }

  /* Look at see if this module has any dependencies.  If so, load
     them.  If we can't load them, then this module must also fail to
     load. */

  OBJ_CONSTRUCT(&dependencies, lam_list_t);
  if (0 != check_laminfo(target_file, &dependencies, found_modules)) {
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return LAM_ERR_OUT_OF_RESOURCE;
  }

  /* Now try to load the module */

  module_handle = lt_dlopenext(target_file->filename);
  if (NULL == module_handle) {
    lam_output_verbose(0, 40, " unable to open: %s (ignored)", 
                       lt_dlerror(), NULL);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return LAM_ERR_BAD_PARAM;
  }

  /* Successfully opened the module; now find the public struct.
     Malloc out enough space for it. */

  len = strlen(target_file->type) + strlen(target_file->name) + 32;
  struct_name = malloc(len);
  if (NULL == struct_name) {
    lt_dlclose(module_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return LAM_ERR_OUT_OF_RESOURCE;
  }
  snprintf(struct_name, len, "mca_%s_%s_module", target_file->type,
           target_file->name);

  mitem = malloc(sizeof(mca_base_module_list_item_t));
  if (NULL == mitem) {
    free(struct_name);
    lt_dlclose(module_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return LAM_ERR_OUT_OF_RESOURCE;
  }
  OBJ_CONSTRUCT(mitem, lam_list_item_t);

  module_struct = lt_dlsym(module_handle, struct_name);
  if (NULL == module_struct) {
    lam_output_verbose(0, 40, " \"%s\" does not appear to be a valid "
                       "%s MCA dynamic module (ignored)", 
                       target_file->basename, target_file->type, NULL);
    free(mitem);
    free(struct_name);
    lt_dlclose(module_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return LAM_ERR_BAD_PARAM;
  }

  /* We found the public struct.  Save it, and register this module to
     be closed later. */

  mitem->mli_module = module_struct;
  lam_list_append(found_modules, (lam_list_item_t *) mitem);
  mca_base_module_registry_retain(target_file->type, module_handle, 
                                  module_struct);

  /* Now that that's all done, link all the dependencies in to this
     module's registry entry */

  for (cur = lam_list_remove_first(&dependencies);
       NULL != cur;
       cur = lam_list_remove_first(&dependencies)) {
    ditem = (dependency_item_t *) cur;
    mca_base_module_registry_link(target_file->type,
                                  target_file->name,
                                  ditem->di_module_file_item->type,
                                  ditem->di_module_file_item->name);
    free(ditem);
  }
  OBJ_DESTRUCT(&dependencies);

  lam_output_verbose(0, 40, " opened dynamic %s MCA module \"%s\"",
                     target_file->type, target_file->name, NULL);
  target_file->status = LOADED;
    
  /* All done */
    
  free(struct_name);
  return LAM_SUCCESS;
}


/*
 * For a given filename, see if there exists a filename.laminfo, which
 * lists dependencies that must be loaded before this module is
 * loaded.  If we find this file, try to load those modules first.
 *
 * Detect dependency cycles and error out.
 */
static int check_laminfo(module_file_item_t *target_file, 
                         lam_list_t *dependencies, lam_list_t *found_modules)
{
  int len;
  FILE *fp;
  char *depname;
  char buffer[BUFSIZ], *p;

  /* Form the filename */

  len = strlen(target_file->filename) + strlen(laminfo_suffix) + 16;
  depname = malloc(len);
  if (NULL == depname)
    return LAM_ERR_OUT_OF_RESOURCE;
  snprintf(depname, len, "%s%s", target_file->filename, laminfo_suffix);

  /* Try to open the file.  If there's no file, return success (i.e.,
     there are no dependencies). */

  if (NULL == (fp = fopen(depname, "r"))) {
    free(depname);
    return 0;
  }

  /* Otherwise, loop reading the lines in the file and trying to load
     them.  Return failure upon the first module that fails to
     load. */

  lam_output_verbose(0, 40, " opening laminfo file: %s", depname, NULL);
  while (NULL != fgets(buffer, BUFSIZ, fp)) {

    /* Perl chomp */

    buffer[BUFSIZ - 1] = '\0';
    len = strlen(buffer);
    if ('\n' == buffer[len - 1])
      buffer[len - 1] = '\0';

    /* Ignore emtpy lines and lines beginning with "#" or "//" */

    for (p = buffer; '\0' != p; ++p)
      if (!isspace(*p))
        break;

    if ('\0' == *p)
      continue;
    else if (*p == '#' || ('/' == *p && '/' == *(p + 1)))
      continue;

    /* Is it a dependency? */

    else if (0 == strncasecmp(p, key_dependency, strlen(key_dependency))) {
      if (LAM_SUCCESS != check_dependency(p + strlen(key_dependency), 
                                          target_file, dependencies, 
                                          found_modules)) {
        fclose(fp);
        free(depname);

        /* We can leave any successfully loaded dependencies; we might
           need them again later.  But free the dependency list for
           this module, because since [at least] one of them didn't
           load, we have to pretend like all of them didn't load and
           disallow loading this module.  So free the dependency
           list. */

        free_dependency_list(dependencies);
        return LAM_ERR_OUT_OF_RESOURCE;
      }
    }
  }
  lam_output_verbose(0, 40, " laminfo file closed (%s)", 
                     target_file->basename, NULL);

  /* All done -- all depenencies satisfied */

  fclose(fp);
  free(depname);
  return 0;
}


/*
 * A DEPENDENCY key was found in the laminfo file.  Chase it down: see
 * if we've already got such a module loaded, or go try to load it if
 * it's not already loaded.
 */
static int check_dependency(char *line, module_file_item_t *target_file,
                            lam_list_t *dependencies,
                            lam_list_t *found_modules)
{
  bool happiness;
  char buffer[BUFSIZ];
  char *type, *name;
  module_file_item_t *mitem;
  dependency_item_t *ditem;
  lam_list_item_t *cur;

  /* Ensure that this was a valid dependency statement */

  type = line;
  name = strchr(line, ':');
  if (NULL == name)
    return LAM_ERR_OUT_OF_RESOURCE;
  *name = '\0';
  ++name;

  /* Form the name of the module to compare to */

  if (strlen(type) + strlen(name) + 32 >= BUFSIZ) {
    target_file->status = FAILED_TO_LOAD;
    return LAM_ERR_OUT_OF_RESOURCE;
  }
  snprintf(buffer, BUFSIZ, module_template, type);
  strcat(buffer, name);

  /* Traverse down the list of files that we have, and see if we can
     find it */

  target_file->status = CHECKING_CYCLE;
  for (happiness = false, cur = lam_list_get_first(&found_files);
       lam_list_get_end(&found_files) != cur;
       cur = lam_list_get_next(cur)) {
    mitem = (module_file_item_t *) cur;

    /* Compare the name to the basename */

    if (0 != strcmp(mitem->basename, buffer))
      continue;

    /* Catch the bozo dependency on itself */

    else if (mitem == target_file) {
      lam_output_verbose(0, 40,
                         " module depends on itself (ignored dependency)", 
                         NULL);
      happiness = true;
      break;
    }

    /* If it's loaded, great -- we're done (no need to check that
       dependency sub-tree) */

    else if (LOADED == mitem->status) {
      lam_output_verbose(0, 40, " dependency has already been loaded (%s)",
                         mitem->basename, NULL);
      happiness = true;
      break;
    }

    /* If it's specifically not loaded (i.e., there was some kind of
       error when we tried to load it), then we cannot meet the
       dependencies. */

    else if (FAILED_TO_LOAD == mitem->status) {
      lam_output_verbose(0, 40, " dependency previously failed to load (%s)",
                         mitem->basename, NULL);
      break;
    }

    /* If we hit a cycle, return badness */

    else if (CHECKING_CYCLE == mitem->status) {
      lam_output_verbose(0, 40, " found cycle! (%s)",
                         mitem->basename, NULL);
      break;
    }

    /* Otherwise, this dependency has not been looked at yet.  Go try
       to load it. */

    else if (UNVISITED == mitem->status) {
      lam_output_verbose(0, 40, " loading dependency (%s)",
                         mitem->basename, NULL);
      if (LAM_SUCCESS == open_module(target_file, found_modules)) {
        happiness = true;
      } else {
        lam_output_verbose(0, 40, " dependency failed to load (%s)",
                           mitem->basename, NULL);
      }
      break;
    }
  }

  /* Did we find the dependency? */

  if (!happiness) {
    target_file->status = FAILED_TO_LOAD;
    return LAM_ERR_BAD_PARAM;
  }

  /* The dependency loaded properly.  Increment its refcount so that
     it doesn't get unloaded before we get unloaded. */

  ditem = malloc(sizeof(dependency_item_t));
  if (NULL == ditem) {
    return LAM_ERR_OUT_OF_RESOURCE;
  }
  cur = (lam_list_item_t *) ditem;
  OBJ_CONSTRUCT(cur, lam_list_item_t);
  lam_list_append(dependencies, cur);
  
  /* All done -- all depenencies satisfied */

  return LAM_SUCCESS;
}


/*
 * Free a dependency list
 */
static void free_dependency_list(lam_list_t *dependencies)
{
  lam_list_item_t *item;

  for (item = lam_list_remove_first(dependencies);
       NULL != item;
       item = lam_list_remove_first(dependencies)) {
    free(item);
  }
  OBJ_DESTRUCT(dependencies);
}
