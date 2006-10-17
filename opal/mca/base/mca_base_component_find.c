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
#include <ctype.h>
#include <stdlib.h>

#if OMPI_WANT_LIBLTDL
#include "opal/libltdl/ltdl.h"
#endif

#include "opal/util/output.h"
#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/constants.h"


/*
 * Private types
 */
typedef enum component_status {
  UNVISITED,
  FAILED_TO_LOAD,
  CHECKING_CYCLE,
  LOADED,

  STATUS_MAX
} component_status_t;

struct component_file_item_t {
  opal_list_item_t super;

  char type[MCA_BASE_MAX_TYPE_NAME_LEN];
  char name[MCA_BASE_MAX_COMPONENT_NAME_LEN];
  char basename[OMPI_PATH_MAX];
  char filename[OMPI_PATH_MAX];
  component_status_t status;
};
typedef struct component_file_item_t component_file_item_t;

static OBJ_CLASS_INSTANCE(component_file_item_t, opal_list_item_t, NULL, NULL);

struct dependency_item_t {
  opal_list_item_t super;

  component_file_item_t *di_component_file_item;
};
typedef struct dependency_item_t dependency_item_t;

static OBJ_CLASS_INSTANCE(dependency_item_t, opal_list_item_t, NULL, NULL);

struct ltfn_data_holder_t {
  char type[MCA_BASE_MAX_TYPE_NAME_LEN];
  char name[MCA_BASE_MAX_COMPONENT_NAME_LEN];
};
typedef struct ltfn_data_holder_t ltfn_data_holder_t;


#if OMPI_WANT_LIBLTDL
/*
 * Private functions
 */
static void find_dyn_components(const char *path, const char *type, 
                             const char *name, opal_list_t *found_components);
static int save_filename(const char *filename, lt_ptr data);
static int open_component(component_file_item_t *target_file, 
                       opal_list_t *found_components);
static int check_ompi_info(component_file_item_t *target_file, 
                         opal_list_t *dependencies,
                         opal_list_t *found_components);
static int check_dependency(char *line, component_file_item_t *target_file, 
                            opal_list_t *dependencies, 
                            opal_list_t *found_components);
static void free_dependency_list(opal_list_t *dependencies);


/*
 * Private variables
 */
static const char *ompi_info_suffix = ".ompi_info";
static const char *key_dependency = "dependency=";
static const char component_template[] = "mca_%s_";
static opal_list_t found_files;
#endif /* OMPI_WANT_LIBLTDL */

/*
 * Function to find as many components of a given type as possible.  This
 * includes statically-linked in components as well as opening up a
 * directory and looking for shared-library MCA components of the
 * appropriate type (load them if available).
 *
 * Return one consolidated array of (mca_base_component_t*) pointing to all
 * available components.
 */
int mca_base_component_find(const char *directory, const char *type, 
                         const mca_base_component_t *static_components[], 
                            opal_list_t *found_components,
                            bool open_dso_components)
{
  int i;
  mca_base_component_list_item_t *cli;

  /* Find all the components that were statically linked in */

  OBJ_CONSTRUCT(found_components, opal_list_t);
  for (i = 0; NULL != static_components[i]; ++i) {
    cli = OBJ_NEW(mca_base_component_list_item_t);
    if (NULL == cli) {
      return OPAL_ERR_OUT_OF_RESOURCE;
    }
    cli->cli_component = static_components[i];
    opal_list_append(found_components, (opal_list_item_t *) cli);
  }

#if OMPI_WANT_LIBLTDL
  /* Find any available dynamic components in the specified directory */
  if (open_dso_components) {
      int param, param_disable_dlopen;
      param = mca_base_param_find("mca", NULL, "component_disable_dlopen");
      mca_base_param_lookup_int(param, &param_disable_dlopen);

      if (0 == param_disable_dlopen) {
          find_dyn_components(directory, type, NULL, found_components);
      }
  } else {
    opal_output_verbose(40, 0, 
                        "mca: base: component_find: dso loading for %s MCA components disabled", 
                        type);
  }
#endif

  /* All done */

  return OPAL_SUCCESS;
}

#if OMPI_WANT_LIBLTDL

/*
 * Open up all directories in a given path and search for components of
 * the specified type (and possibly of a given name).
 *
 * Note that we use our own path iteration functionality (vs. ltdl's
 * lt_dladdsearchdir() functionality) because we need to look at
 * companion .ompi_info files in the same directory as the library to
 * generate dependencies, etc.  If we use the plain lt_dlopen()
 * functionality, we would not get the directory name of the file
 * finally opened in recursive dependency traversals.
 */
static void find_dyn_components(const char *path, const char *type_name, 
                                const char *name,
                                opal_list_t *found_components)
{
  ltfn_data_holder_t params;
  char *path_to_use, *dir, *end, *param;
  component_file_item_t *file;
  opal_list_item_t *cur;

  strcpy(params.type, type_name);

  if (NULL == name) {
    params.name[0] = '\0';
    opal_output_verbose(40, 0, "mca: base: component_find: looking for all dynamic %s MCA components", 
                       type_name, NULL);
  } else {
    strcpy(params.name, name);
    opal_output_verbose(40, 0,
                       "mca: base: component_find: looking for dynamic %s MCA component named \"%s\"",
                       type_name, name, NULL);
  }

  /* If path is NULL, iterate over the set of directories specified by
     the MCA param mca_base_component_path.  If path is not NULL, then
     use that as the path. */

  param = NULL;
  if (NULL == path) {
    mca_base_param_lookup_string(mca_base_param_component_path, &param);
    if (NULL == param) {
      /* If there's no path, then there's nothing to search -- we're
         done */
      return;
    } else {
      path_to_use = strdup(param);
    }
  } else {
    path_to_use = strdup(path);
  }

  /* Iterate over all the files in the directories in the path and
     make a master array of all the matching filenames that we
     find. */

  OBJ_CONSTRUCT(&found_files, opal_list_t);
  dir = path_to_use;
  if (NULL != dir) {
    do {
      end = strchr(dir, OPAL_ENV_SEP);
      if (NULL != end) {
        *end = '\0';
      }
      if (0 != lt_dlforeachfile(dir, save_filename, &params)) {
        break;
      }
      dir = end + 1;
    } while (NULL != end);
  }

  /* Iterate through all the filenames that we found.  Since one
     component may [try to] call another to be loaded, only try to load
     the UNVISITED files.  Also, ignore the return code -- basically,
     give every file one chance to try to load.  If they load, great.
     If not, great. */

  for (cur = opal_list_get_first(&found_files); 
       opal_list_get_end(&found_files) != cur;
       cur = opal_list_get_next(cur)) {
    file = (component_file_item_t *) cur;
    if (UNVISITED == file->status) {
      open_component(file, found_components);
    }
  }

  /* So now we have a final list of loaded components.  We can free all
     the file information. */
  
  for (cur = opal_list_remove_first(&found_files); 
       NULL != cur;
       cur = opal_list_remove_first(&found_files)) {
    OBJ_RELEASE(cur);
  }

  /* All done */

  if (NULL != param) {
    free(param);
  }
  if (NULL != path_to_use) {
    free(path_to_use);
  }
  OBJ_DESTRUCT(&found_files);
}


/*
 * Given a filename, see if it appears to be of the proper filename
 * format.  If so, save it in the array so that we can process it
 * later.
 */
static int save_filename(const char *filename, lt_ptr data)
{
  size_t len, prefix_len, total_len;
  char *prefix;
  const char *basename;
  component_file_item_t *component_file;
  ltfn_data_holder_t *params = (ltfn_data_holder_t *) data;

  /* Check to see if the file is named what we expect it to be
     named */

  len = sizeof(component_template) + strlen(params->type) + 32;
  if (NULL != params->name) {
    len += strlen(params->name);
  }
  prefix = (char*)malloc(len);
  snprintf(prefix, len, component_template, params->type);
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

  /* Save all the info and put it in the list of found components */

  component_file = OBJ_NEW(component_file_item_t);
  if (NULL == component_file) {
    return OPAL_ERR_OUT_OF_RESOURCE;
  }
  strcpy(component_file->type, params->type);
  strcpy(component_file->name, basename + prefix_len);
  strcpy(component_file->basename, basename);
  strcpy(component_file->filename, filename);
  component_file->status = UNVISITED;
  opal_list_append(&found_files, (opal_list_item_t *) component_file);

  /* All done */

  free(prefix);
  return 0;
}


/*
 * Open a component, chasing down its dependencies first, if possible.
 */
static int open_component(component_file_item_t *target_file, 
                       opal_list_t *found_components)
{
  int show_errors, param;
  lt_dlhandle component_handle;
  mca_base_component_t *component_struct;
  char *struct_name, *err;
  opal_list_t dependencies;
  opal_list_item_t *cur;
  mca_base_component_list_item_t *mitem;
  dependency_item_t *ditem;
  size_t len;

  opal_output_verbose(40, 0, "mca: base: component_find: examining dyanmic %s MCA component \"%s\"",
                     target_file->type, target_file->name, NULL);
  opal_output_verbose(40, 0, "mca: base: component_find: %s", target_file->filename, NULL);
  param = mca_base_param_find("mca", NULL, "component_show_load_errors");
  mca_base_param_lookup_int(param, &show_errors);

  /* Was this component already loaded (e.g., via dependency)? */

  if (LOADED == target_file->status) {
    opal_output_verbose(40, 0, "mca: base: component_find: already loaded (ignored)", NULL);
    return OPAL_SUCCESS;
  }

  /* Ensure that this component is not already loaded (should only happen
     if it was statically loaded).  It's an error if it's already
     loaded because we're evaluating this file -- not this component.
     Hence, returning OPAL_ERR_PARAM indicates that the *file* failed
     to load, not the component. */

  for (cur = opal_list_get_first(found_components); 
       opal_list_get_end(found_components) != cur;
       cur = opal_list_get_next(cur)) {
    mitem = (mca_base_component_list_item_t *) cur;
    if (0 == strcmp(mitem->cli_component->mca_type_name, target_file->type) &&
        0 == strcmp(mitem->cli_component->mca_component_name, target_file->name)) {
      opal_output_verbose(40, 0, "mca: base: component_find: already loaded (ignored)", NULL);
      target_file->status = FAILED_TO_LOAD;
      return OPAL_ERR_BAD_PARAM;
    }
  }

  /* Look at see if this component has any dependencies.  If so, load
     them.  If we can't load them, then this component must also fail to
     load. */

  OBJ_CONSTRUCT(&dependencies, opal_list_t);
  if (0 != check_ompi_info(target_file, &dependencies, found_components)) {
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_OUT_OF_RESOURCE;
  }

  /* Now try to load the component */

  component_handle = lt_dlopenext(target_file->filename);
  if (NULL == component_handle) {
    err = strdup(lt_dlerror());
    if (0 != show_errors) {
        opal_output(0, "mca: base: component_find: unable to open %s %s: %s (ignored)", 
                    target_file->type, target_file->name, err);
    }
    opal_output_verbose(40, 0, "mca: base: component_find: unable to open %s: %s (ignored)", 
                        target_file->filename, err, NULL);
    free(err);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_BAD_PARAM;
  }

  /* Successfully opened the component; now find the public struct.
     Malloc out enough space for it. */

  len = strlen(target_file->type) + strlen(target_file->name) + 32;
  struct_name = (char*)malloc(len);
  if (NULL == struct_name) {
    lt_dlclose(component_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_OUT_OF_RESOURCE;
  }
  snprintf(struct_name, len, "mca_%s_%s_component", target_file->type,
           target_file->name);

  mitem = OBJ_NEW(mca_base_component_list_item_t);
  if (NULL == mitem) {
    free(struct_name);
    lt_dlclose(component_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_OUT_OF_RESOURCE;
  }

  component_struct = (mca_base_component_t*)lt_dlsym(component_handle, struct_name);
  if (NULL == component_struct) {
    if (0 != show_errors) {
        opal_output(0, "mca: base: component_find: \"%s\" does not appear to be a valid "
                       "%s MCA dynamic component (ignored)", 
                       target_file->basename, target_file->type, NULL);
    }
    opal_output_verbose(40, 0, "mca: base: component_find: \"%s\" does not appear to be a valid "
                       "%s MCA dynamic component (ignored)", 
                       target_file->basename, target_file->type, NULL);
    free(mitem);
    free(struct_name);
    lt_dlclose(component_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_BAD_PARAM;
  }

  /* We found the public struct.  Save it, and register this component to
     be closed later. */

  mitem->cli_component = component_struct;
  opal_list_append(found_components, (opal_list_item_t *) mitem);
  mca_base_component_repository_retain(target_file->type, component_handle, 
                                    component_struct);

  /* Now that that's all done, link all the dependencies in to this
     component's repository entry */

  for (cur = opal_list_remove_first(&dependencies);
       NULL != cur;
       cur = opal_list_remove_first(&dependencies)) {
    ditem = (dependency_item_t *) cur;
    mca_base_component_repository_link(target_file->type,
                                       target_file->name,
                                       ditem->di_component_file_item->type,
                                       ditem->di_component_file_item->name);
    OBJ_RELEASE(ditem);
  }
  OBJ_DESTRUCT(&dependencies);

  opal_output_verbose(40, 0, "mca: base: component_find: opened dynamic %s MCA component \"%s\"",
                     target_file->type, target_file->name, NULL);
  target_file->status = LOADED;
    
  /* All done */
    
  free(struct_name);
  return OPAL_SUCCESS;
}


/*
 * For a given filename, see if there exists a filename.ompi_info, which
 * lists dependencies that must be loaded before this component is
 * loaded.  If we find this file, try to load those components first.
 *
 * Detect dependency cycles and error out.
 */
static int check_ompi_info(component_file_item_t *target_file, 
                           opal_list_t *dependencies, 
                           opal_list_t *found_components)
{
  size_t len;
  FILE *fp;
  char *depname;
  char buffer[BUFSIZ], *p;

  /* Form the filename */

  len = strlen(target_file->filename) + strlen(ompi_info_suffix) + 16;
  depname = (char*)malloc(len);
  if (NULL == depname)
    return OPAL_ERR_OUT_OF_RESOURCE;
  snprintf(depname, len, "%s%s", target_file->filename, ompi_info_suffix);

  /* Try to open the file.  If there's no file, return success (i.e.,
     there are no dependencies). */

  if (NULL == (fp = fopen(depname, "r"))) {
    free(depname);
    return 0;
  }

  /* Otherwise, loop reading the lines in the file and trying to load
     them.  Return failure upon the first component that fails to
     load. */

  opal_output_verbose(40, 0, "mca: base: component_find: opening ompi_info file: %s", depname, NULL);
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
      if (OPAL_SUCCESS != check_dependency(p + strlen(key_dependency), 
                                          target_file, dependencies, 
                                          found_components)) {
        fclose(fp);
        free(depname);

        /* We can leave any successfully loaded dependencies; we might
           need them again later.  But free the dependency list for
           this component, because since [at least] one of them didn't
           load, we have to pretend like all of them didn't load and
           disallow loading this component.  So free the dependency
           list. */

        free_dependency_list(dependencies);
        return OPAL_ERR_OUT_OF_RESOURCE;
      }
    }
  }
  opal_output_verbose(40, 0, "mca: base: component_find: ompi_info file closed (%s)", 
                     target_file->basename, NULL);

  /* All done -- all depenencies satisfied */

  fclose(fp);
  free(depname);
  return 0;
}


/*
 * A DEPENDENCY key was found in the ompi_info file.  Chase it down: see
 * if we've already got such a component loaded, or go try to load it if
 * it's not already loaded.
 */
static int check_dependency(char *line, component_file_item_t *target_file,
                            opal_list_t *dependencies,
                            opal_list_t *found_components)
{
  bool happiness;
  char buffer[BUFSIZ];
  char *type, *name;
  component_file_item_t *mitem;
  dependency_item_t *ditem;
  opal_list_item_t *cur;

  /* Ensure that this was a valid dependency statement */

  type = line;
  name = strchr(line, OPAL_ENV_SEP);
  if (NULL == name) {
    return OPAL_ERR_OUT_OF_RESOURCE;
  }
  *name = '\0';
  ++name;

  /* Form the name of the component to compare to */

  if (strlen(type) + strlen(name) + 32 >= BUFSIZ) {
    target_file->status = FAILED_TO_LOAD;
    return OPAL_ERR_OUT_OF_RESOURCE;
  }
  snprintf(buffer, BUFSIZ, component_template, type);
  strcat(buffer, name);

  /* Traverse down the list of files that we have, and see if we can
     find it */

  mitem = NULL;
  target_file->status = CHECKING_CYCLE;
  for (happiness = false, cur = opal_list_get_first(&found_files);
       opal_list_get_end(&found_files) != cur;
       cur = opal_list_get_next(cur)) {
    mitem = (component_file_item_t *) cur;

    /* Compare the name to the basename */

    if (0 != strcmp(mitem->basename, buffer))
      continue;

    /* Catch the bozo dependency on itself */

    else if (mitem == target_file) {
      opal_output_verbose(40, 0,
                         "mca: base: component_find: component depends on itself (ignored dependency)", 
                         NULL);
      happiness = true;
      break;
    }

    /* If it's loaded, great -- we're done (no need to check that
       dependency sub-tree) */

    else if (LOADED == mitem->status) {
      opal_output_verbose(40, 0, "mca: base: component_find: dependency has already been loaded (%s)",
                         mitem->basename, NULL);
      happiness = true;
      break;
    }

    /* If it's specifically not loaded (i.e., there was some kind of
       error when we tried to load it), then we cannot meet the
       dependencies. */

    else if (FAILED_TO_LOAD == mitem->status) {
      opal_output_verbose(40, 0, "mca: base: component_find: dependency previously failed to load (%s)",
                         mitem->basename, NULL);
      break;
    }

    /* If we hit a cycle, return badness */

    else if (CHECKING_CYCLE == mitem->status) {
      opal_output_verbose(40, 0, "mca: base: component_find: found cycle! (%s)",
                         mitem->basename, NULL);
      break;
    }

    /* Otherwise, this dependency has not been looked at yet.  Go try
       to load it. */

    else if (UNVISITED == mitem->status) {
      opal_output_verbose(40, 0, "mca: base: component_find: loading dependency (%s)",
                         mitem->basename, NULL);
      if (OPAL_SUCCESS == open_component(target_file, found_components)) {
        happiness = true;
      } else {
        opal_output_verbose(40, 0, "mca: base: component_find: dependency failed to load (%s)",
                           mitem->basename, NULL);
      }
      break;
    }
  }

  /* Did we find the dependency? */

  if (!happiness) {
    target_file->status = FAILED_TO_LOAD;
    return OPAL_ERR_BAD_PARAM;
  }

  /* The dependency loaded properly.  Increment its refcount so that
     it doesn't get unloaded before we get unloaded.  The (NULL !=
     mitem) check is somewhat redundant -- we won't be here in this
     function unless there's dependencies to check, but a) it's safer
     to double check, and b) it fixes a compiler warning.  :-) */

  if (NULL != mitem) {
      ditem = OBJ_NEW(dependency_item_t);
      if (NULL == ditem) {
          return OPAL_ERR_OUT_OF_RESOURCE;
      }
      ditem->di_component_file_item = mitem;
      opal_list_append(dependencies, (opal_list_item_t*) ditem);
  }
  
  /* All done -- all depenencies satisfied */

  return OPAL_SUCCESS;
}


/*
 * Free a dependency list
 */
static void free_dependency_list(opal_list_t *dependencies)
{
  opal_list_item_t *item;

  for (item = opal_list_remove_first(dependencies);
       NULL != item;
       item = opal_list_remove_first(dependencies)) {
    OBJ_RELEASE(item);
  }
  OBJ_DESTRUCT(dependencies);
}

#endif /* OMPI_WANT_LIBLTDL */
