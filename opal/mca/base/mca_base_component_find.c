/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "opal/constants.h"
#include "opal/mca/dl/base/base.h"


#if OPAL_HAVE_DL_SUPPORT
/*
 * Private types; only necessary when we're dlopening components.
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

  char type[MCA_BASE_MAX_TYPE_NAME_LEN + 1];
  char name[MCA_BASE_MAX_COMPONENT_NAME_LEN + 1];
  char basename[OPAL_PATH_MAX + 1];
  char filename[OPAL_PATH_MAX + 1];
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

#endif /* OPAL_HAVE_DL_SUPPORT */


#if OPAL_HAVE_DL_SUPPORT
/*
 * Private functions
 */
static void find_dyn_components(const char *path, const char *type, 
                                const char **names, bool include_mode,
                                opal_list_t *found_components);
static int save_filename(const char *filename, void *data);
static int open_component(component_file_item_t *target_file, 
                       opal_list_t *found_components);
static int check_opal_info(component_file_item_t *target_file, 
                         opal_list_t *dependencies,
                         opal_list_t *found_components);
static int check_dependency(char *line, component_file_item_t *target_file, 
                            opal_list_t *dependencies, 
                            opal_list_t *found_components);
static void free_dependency_list(opal_list_t *dependencies);

/*
 * Private variables
 */
static const char *opal_info_suffix = ".ompi_info";
static const char *key_dependency = "dependency=";
static const char component_template[] = "mca_%s_";
static opal_list_t found_files;
static char **found_filenames = NULL;
static char *last_path_to_use = NULL;
#endif /* OPAL_HAVE_DL_SUPPORT */

static int component_find_check (const char *framework_name, char **requested_component_names, opal_list_t *components);

/*
 * Dummy structure for casting for open_only logic
 */
struct mca_base_open_only_dummy_component_t {
    /** MCA base component */
    mca_base_component_t version;
    /** MCA base data */
    mca_base_component_data_t data;
};
typedef struct mca_base_open_only_dummy_component_t mca_base_open_only_dummy_component_t;

static char negate[] = "^";

static bool use_component(const bool include_mode,
                          const char **requested_component_names,
                          const char *component_name);


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
                            const char *requested_components,
                            opal_list_t *found_components,
                            bool open_dso_components)
{
    char **requested_component_names = NULL;
    mca_base_component_list_item_t *cli;
    bool include_mode;
    int i, ret;

    ret = mca_base_component_parse_requested (requested_components, &include_mode,
                           &requested_component_names);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    /* Find all the components that were statically linked in */
    OBJ_CONSTRUCT(found_components, opal_list_t);
    for (i = 0; NULL != static_components &&
             NULL != static_components[i]; ++i) {
        if ( use_component(include_mode,
                           (const char**)requested_component_names,
                           static_components[i]->mca_component_name) ) {
            cli = OBJ_NEW(mca_base_component_list_item_t);
            if (NULL == cli) {
                ret = OPAL_ERR_OUT_OF_RESOURCE;
                goto component_find_out;
            }
            cli->cli_component = static_components[i];
            opal_list_append(found_components, (opal_list_item_t *) cli);
        }
    }

#if OPAL_HAVE_DL_SUPPORT
    /* Find any available dynamic components in the specified directory */
    if (open_dso_components && !mca_base_component_disable_dlopen) {
        find_dyn_components(directory, type,
                            (const char**)requested_component_names,
                            include_mode, found_components); 
    } else {
        opal_output_verbose(40, 0, 
                            "mca: base: component_find: dso loading for %s MCA components disabled", 
                            type);
    }
#endif

    if (include_mode) {
        ret = component_find_check (type, requested_component_names, found_components);
    } else {
        ret = OPAL_SUCCESS;
    }

component_find_out:

    if (NULL != requested_component_names) {
        opal_argv_free(requested_component_names);
    }

    /* All done */

    return ret;
}

int mca_base_component_find_finalize(void)
{
#if OPAL_HAVE_DL_SUPPORT
    if (NULL != found_filenames) {
        opal_argv_free(found_filenames);
        found_filenames = NULL;
    }
    if (NULL != last_path_to_use) {
        free(last_path_to_use);
        last_path_to_use = NULL;
    }
#endif
    return OPAL_SUCCESS;
}

int mca_base_components_filter (const char *framework_name, opal_list_t *components, int output_id,
                                const char *filter_names, uint32_t filter_flags)
{
    mca_base_component_list_item_t *cli, *next;
    char **requested_component_names = NULL;
    bool include_mode, can_use;
    int ret;

    assert (NULL != components);

    if (0 == filter_flags && NULL == filter_names) {
        return OPAL_SUCCESS;
    }

    ret = mca_base_component_parse_requested (filter_names, &include_mode,
                           &requested_component_names);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    OPAL_LIST_FOREACH_SAFE(cli, next, components, mca_base_component_list_item_t) {
        const mca_base_component_t *component = cli->cli_component;
        mca_base_open_only_dummy_component_t *dummy =
            (mca_base_open_only_dummy_component_t *) cli->cli_component;

        can_use = use_component (include_mode, (const char **) requested_component_names,
                                 cli->cli_component->mca_component_name);

        if (!can_use || (filter_flags & dummy->data.param_field) != filter_flags) {
            if (can_use && (filter_flags & MCA_BASE_METADATA_PARAM_CHECKPOINT) &&
                !(MCA_BASE_METADATA_PARAM_CHECKPOINT & dummy->data.param_field)) {
                opal_output_verbose(10, output_id,
                                    "mca: base: components_filter: "
                                    "(%s) Component %s is *NOT* Checkpointable - Disabled",
                                    component->reserved,
                                    component->mca_component_name);
            }

            opal_list_remove_item (components, &cli->super);

            mca_base_component_unload (component, output_id);

            OBJ_RELEASE(cli);
        } else if (filter_flags & MCA_BASE_METADATA_PARAM_CHECKPOINT) {
            opal_output_verbose(10, output_id,
                                "mca: base: components_filter: "
                                "(%s) Component %s is Checkpointable",
                                component->reserved,
                                component->mca_component_name);
        }
    }

    if (include_mode) {
        ret = component_find_check (framework_name, requested_component_names, components);
    } else {
        ret = OPAL_SUCCESS;
    }

    if (NULL != requested_component_names) {
        opal_argv_free (requested_component_names);
    }

    return ret;
}

#if OPAL_HAVE_DL_SUPPORT

/*
 * Open up all directories in a given path and search for components of
 * the specified type (and possibly of a given name).
 *
 * Note that we use our own path iteration functionality because we
 * need to look at companion .ompi_info files in the same directory as
 * the library to generate dependencies, etc.
 */
static void find_dyn_components(const char *path, const char *type_name, 
                                const char **names, bool include_mode,
                                opal_list_t *found_components)
{
    int i, len;
    char *path_to_use = NULL, *dir, *end;
    component_file_item_t *file;
    opal_list_item_t *cur;
    char prefix[32 + MCA_BASE_MAX_TYPE_NAME_LEN], *basename;
    
    /* If path is NULL, iterate over the set of directories specified by
       the MCA param mca_base_component_path.  If path is not NULL, then
       use that as the path. */
  
    if (NULL == path) {
        if (NULL != mca_base_component_path) {
            path_to_use = strdup (mca_base_component_path);
        } else {
            /* If there's no path, then there's nothing to search -- we're
               done */
            return;
        }
    } else {
        path_to_use = strdup(path);
    }
    if (NULL == path_to_use) {
        /* out of memory */
        return;
    }
  
    /* If we haven't done so already, iterate over all the files in
       the directories in the path and make a master array of all the
       matching filenames that we find.  Save the filenames in an
       argv-style array.  Re-scan do this if the mca_component_path
       has changed. */
    if (NULL == found_filenames || 
        (NULL != last_path_to_use && 
         0 != strcmp(path_to_use, last_path_to_use))) {
        if (NULL != found_filenames) {
            opal_argv_free(found_filenames);
            found_filenames = NULL;
            free(last_path_to_use);
            last_path_to_use = NULL;
        }
        if (NULL == last_path_to_use) {
            last_path_to_use = strdup(path_to_use);
        }

        dir = path_to_use;
        if (NULL != dir) {
            do {
                end = strchr(dir, OPAL_ENV_SEP);
                if (NULL != end) {
                    *end = '\0';
                }
                if ((0 == strcmp(dir, "USER_DEFAULT") ||
                     0 == strcmp(dir, "USR_DEFAULT"))
                    && NULL != mca_base_user_default_path) {
                    if (0 != opal_dl_foreachfile(mca_base_user_default_path,
                                                 save_filename, NULL)) {
                        break;
                    }
                } else if (0 == strcmp(dir, "SYS_DEFAULT") ||
                           0 == strcmp(dir, "SYSTEM_DEFAULT")) {
                    if (0 != opal_dl_foreachfile(mca_base_system_default_path,
                                                 save_filename, NULL)) {
                        break;
                    }                    
                } else {
                    if (0 != opal_dl_foreachfile(dir, save_filename, NULL)) {
                        break;
                    }
                }
                dir = end + 1;
            } while (NULL != end);
        }
    }
    
    /* Look through the list of found files and find those that match
       the desired framework name */
    snprintf(prefix, sizeof(prefix) - 1, component_template, type_name);
    len = strlen(prefix);
    OBJ_CONSTRUCT(&found_files, opal_list_t);
    for (i = 0; NULL != found_filenames && NULL != found_filenames[i]; ++i) {
        basename = strrchr(found_filenames[i], '/');
        if (NULL == basename) {
            basename = found_filenames[i];
        } else {
            basename += 1;
        }
        
        if (0 != strncmp(basename, prefix, len)) {
            continue;
        }
        
        /* We found a match; save all the relevant details in the
           found_files list */
        file = OBJ_NEW(component_file_item_t);
        if (NULL == file) {
            free(path_to_use);
            return;
        }
        strncpy(file->type, type_name, MCA_BASE_MAX_TYPE_NAME_LEN);
        file->type[MCA_BASE_MAX_TYPE_NAME_LEN] = '\0';
        strncpy(file->name, basename + len, MCA_BASE_MAX_COMPONENT_NAME_LEN);
        file->name[MCA_BASE_MAX_COMPONENT_NAME_LEN] = '\0';
        strncpy(file->basename, basename, OPAL_PATH_MAX);
        file->basename[OPAL_PATH_MAX] = '\0';
        strncpy(file->filename, found_filenames[i], OPAL_PATH_MAX);
        file->filename[OPAL_PATH_MAX] = '\0';
        file->status = UNVISITED;

        opal_list_append(&found_files, (opal_list_item_t *) 
                         file);
    }

    /* Iterate through all the filenames that we found that matched
       the framework we were looking for.  Since one component may
       [try to] call another to be loaded, only try to load the
       UNVISITED files.  Also, ignore the return code -- basically,
       give every file one chance to try to load.  If they load,
       great.  If not, great. */
    for (cur = opal_list_get_first(&found_files); 
         opal_list_get_end(&found_files) != cur;
         cur = opal_list_get_next(cur)) {
        file = (component_file_item_t *) cur;

        if( UNVISITED == file->status ) {
            bool op = true;
            file->status = CHECKING_CYCLE;

            op = use_component(include_mode, names, file->name);
            if( true == op ) {
                open_component(file, found_components);
            }
        }
    }
    
    /* So now we have a final list of loaded components.  We can free all
       the file information. */
    for (cur = opal_list_remove_first(&found_files); 
         NULL != cur;
         cur = opal_list_remove_first(&found_files)) {
        OBJ_RELEASE(cur);
    }
    OBJ_DESTRUCT(&found_files);

    /* All done, now let's cleanup */
    free(path_to_use);
}


/*
 * Blindly save all filenames into an argv-style list.  This function
 * is the callback from lt_dlforeachfile().
 */
static int save_filename(const char *filename, void *data)
{
    opal_argv_append_nosize(&found_filenames, filename);
    return 0;
}


static int file_exists(const char *filename, const char *ext)
{
    char *final;
    struct stat buf;
    int ret;

    if (NULL != ext) {
        asprintf(&final, "%s.%s", filename, ext);
    } else {
        final = strdup(filename);
    }
    if (NULL == final) {
        return 0;
    }
    ret = stat(final, &buf);
    free(final);
    return (0 == ret ? 1 : 0);
}


/*
 * Open a component, chasing down its dependencies first, if possible.
 */
static int open_component(component_file_item_t *target_file, 
                       opal_list_t *found_components)
{
  opal_dl_handle_t *component_handle;
  mca_base_component_t *component_struct;
  char *struct_name;
  opal_list_t dependencies;
  opal_list_item_t *cur;
  mca_base_component_list_item_t *mitem;
  dependency_item_t *ditem;
  size_t len;
  int vl;

  opal_output_verbose(40, 0, "mca: base: component_find: examining dyanmic %s MCA component \"%s\"",
                     target_file->type, target_file->name);
  opal_output_verbose(40, 0, "mca: base: component_find: %s", target_file->filename);

  vl = mca_base_component_show_load_errors ? 0 : 40;

  /* Was this component already loaded (e.g., via dependency)? */

  if (LOADED == target_file->status) {
    opal_output_verbose(40, 0, "mca: base: component_find: already loaded (ignored)");
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
      opal_output_verbose(40, 0, "mca: base: component_find: already loaded (ignored)");
      target_file->status = FAILED_TO_LOAD;
      return OPAL_ERR_BAD_PARAM;
    }
  }

  /* Look at see if this component has any dependencies.  If so, load
     them.  If we can't load them, then this component must also fail to
     load. */

  OBJ_CONSTRUCT(&dependencies, opal_list_t);
  if (0 != check_opal_info(target_file, &dependencies, found_components)) {
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_OUT_OF_RESOURCE;
  }

  /* Now try to load the component */

  char *err_msg;
  if (OPAL_SUCCESS !=
      opal_dl_open(target_file->filename, true, false, &component_handle,
                   &err_msg)) {
      if (NULL != err_msg) {
          err_msg = strdup(err_msg);
      } else {
          err_msg = strdup("opal_dl_open() error message was NULL!");
      }
      /* Because libltdl erroneously says "file not found" for any
         type of error -- which is especially misleading when the file
         is actually there but cannot be opened for some other reason
         (e.g., missing symbol) -- do some simple huersitics and if
         the file [probably] does exist, print a slightly better error
         message. */
      if (0 == strcmp("file not found", err_msg) &&
          (file_exists(target_file->filename, "lo") ||
           file_exists(target_file->filename, "so") ||
           file_exists(target_file->filename, "dylib") ||
           file_exists(target_file->filename, "dll"))) {
          free(err_msg);
          err_msg = strdup("perhaps a missing symbol, or compiled for a different version of Open MPI?");
      }
      opal_output_verbose(vl, 0, "mca: base: component_find: unable to open %s: %s (ignored)", 
                          target_file->filename, err_msg);
      free(err_msg);
      target_file->status = FAILED_TO_LOAD;
      free_dependency_list(&dependencies);
      return OPAL_ERR_BAD_PARAM;
  }

  /* Successfully opened the component; now find the public struct.
     Malloc out enough space for it. */

  len = strlen(target_file->type) + strlen(target_file->name) + 32;
  struct_name = (char*)malloc(len);
  if (NULL == struct_name) {
    opal_dl_close(component_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_OUT_OF_RESOURCE;
  }
  snprintf(struct_name, len, "mca_%s_%s_component", target_file->type,
           target_file->name);

  mitem = OBJ_NEW(mca_base_component_list_item_t);
  if (NULL == mitem) {
    free(struct_name);
    opal_dl_close(component_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_OUT_OF_RESOURCE;
  }

  if (OPAL_SUCCESS != opal_dl_lookup(component_handle, struct_name,
                                     (void**) &component_struct, &err_msg) ||
      NULL == component_struct) {
      if (NULL == err_msg) {
          err_msg = "opal_dl_loookup() error message was NULL!";
      }
      opal_output_verbose(vl, 0, "mca: base: component_find: \"%s\" does not appear to be a valid "
                          "%s MCA dynamic component (ignored): %s",
                          target_file->basename, target_file->type, err_msg);
      free(mitem);
      free(struct_name);
      opal_dl_close(component_handle);
      target_file->status = FAILED_TO_LOAD;
      free_dependency_list(&dependencies);
      return OPAL_ERR_BAD_PARAM;
  }

  /* We found the public struct.  Make sure its MCA major.minor
     version is the same as ours. */
  if (!(MCA_BASE_VERSION_MAJOR == component_struct->mca_major_version &&
        MCA_BASE_VERSION_MINOR == component_struct->mca_minor_version)) {
      opal_output_verbose(vl, 0, "mca: base: component_find: %s \"%s\" uses an MCA interface that is not recognized (component MCA v%d.%d.%d != supported MCA v%d.%d.%d) -- ignored",
                          target_file->type, target_file->basename, 
                          component_struct->mca_major_version,
                          component_struct->mca_minor_version,
                          component_struct->mca_release_version,
                          MCA_BASE_VERSION_MAJOR,
                          MCA_BASE_VERSION_MINOR,
                          MCA_BASE_VERSION_RELEASE);
    free(mitem);
    free(struct_name);
    opal_dl_close(component_handle);
    target_file->status = FAILED_TO_LOAD;
    free_dependency_list(&dependencies);
    return OPAL_ERR_BAD_PARAM;
  }

  /* Also check that the component struct framework and component
     names match the expected names from the filename */
  if (0 != strcmp(component_struct->mca_type_name, target_file->type) ||
      0 != strcmp(component_struct->mca_component_name, target_file->name)) {
      opal_output_verbose(vl, 0, "Component file data does not match filename: %s (%s / %s) != %s %s -- ignored",
                          target_file->filename, target_file->type, target_file->name,
                          component_struct->mca_type_name, 
                          component_struct->mca_component_name);
      free(mitem);
      free(struct_name);
      opal_dl_close(component_handle);
      target_file->status = FAILED_TO_LOAD;
      free_dependency_list(&dependencies);
      return OPAL_ERR_BAD_PARAM;
  }

  /* Alles gut.  Save the component struct, and register this
     component to be closed later. */

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
                     target_file->type, target_file->name);
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
static int check_opal_info(component_file_item_t *target_file, 
                           opal_list_t *dependencies, 
                           opal_list_t *found_components)
{
  size_t len;
  FILE *fp;
  char *depname;
  char buffer[BUFSIZ], *p;

  /* Form the filename */

  len = strlen(target_file->filename) + strlen(opal_info_suffix) + 16;
  depname = (char*)malloc(len);
  if (NULL == depname)
    return OPAL_ERR_OUT_OF_RESOURCE;
  snprintf(depname, len, "%s%s", target_file->filename, opal_info_suffix);

  /* Try to open the file.  If there's no file, return success (i.e.,
     there are no dependencies). */

  if (NULL == (fp = fopen(depname, "r"))) {
    free(depname);
    return 0;
  }

  /* Otherwise, loop reading the lines in the file and trying to load
     them.  Return failure upon the first component that fails to
     load. */

  opal_output_verbose(40, 0, "mca: base: component_find: opening .ompi_info file: %s", depname);
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
                     target_file->basename);

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
  int len;
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
  len = strlen(buffer);
  strncat(buffer, name, BUFSIZ - len);

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
                         "mca: base: component_find: component depends on itself (ignored dependency)");
      happiness = true;
      break;
    }

    /* If it's loaded, great -- we're done (no need to check that
       dependency sub-tree) */

    else if (LOADED == mitem->status) {
      opal_output_verbose(40, 0, "mca: base: component_find: dependency has already been loaded (%s)",
                         mitem->basename);
      happiness = true;
      break;
    }

    /* If it's specifically not loaded (i.e., there was some kind of
       error when we tried to load it), then we cannot meet the
       dependencies. */

    else if (FAILED_TO_LOAD == mitem->status) {
      opal_output_verbose(40, 0, "mca: base: component_find: dependency previously failed to load (%s)",
                         mitem->basename);
      break;
    }

    /* If we hit a cycle, return badness */

    else if (CHECKING_CYCLE == mitem->status) {
      opal_output_verbose(40, 0, "mca: base: component_find: found cycle! (%s)",
                         mitem->basename);
      break;
    }

    /* Otherwise, this dependency has not been looked at yet.  Go try
       to load it. */

    else if (UNVISITED == mitem->status) {
      opal_output_verbose(40, 0, "mca: base: component_find: loading dependency (%s)",
                         mitem->basename);
      if (OPAL_SUCCESS == open_component(target_file, found_components)) {
        happiness = true;
      } else {
        opal_output_verbose(40, 0, "mca: base: component_find: dependency failed to load (%s)",
                           mitem->basename);
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

#endif /* OPAL_HAVE_DL_SUPPORT */

static bool use_component(const bool include_mode,
                          const char **requested_component_names,
                          const char *component_name)
{
    bool found = false;
    const char **req_comp_name = requested_component_names;
    
    /*
     * If no selection is specified then we use all components
     * we can find.
     */
    if (NULL == req_comp_name) {
        return true;
    }

    while ( *req_comp_name != NULL ) {
        if ( strcmp(component_name, *req_comp_name) == 0 ) {
            found = true;
            break;
        }
        req_comp_name++;
    }

    /*
     * include_mode  found |   use
     * --------------------+------
     *            0      0 |  true
     *            0      1 | false
     *            1      0 | false
     *            1      1 |  true
     *
     * -> inverted xor
     * As xor is a binary operator let's implement it manually before
     * a compiler screws it up.
     */

    return (include_mode && found) || !(include_mode || found);
}

/* Ensure that *all* requested components exist.  Print a warning
   and abort if they do not. */
static int component_find_check (const char *framework_name, char **requested_component_names, opal_list_t *components)
{
    mca_base_component_list_item_t *cli;
    int i;

    for (i = 0; NULL != requested_component_names && 
             NULL != requested_component_names[i]; ++i) {
        bool found = false;

        OPAL_LIST_FOREACH(cli, components, mca_base_component_list_item_t) {
            if (0 == strcmp(requested_component_names[i], 
                            cli->cli_component->mca_component_name)) {
                found = true;
                break;
            }
        }

        if (!found) {
            char h[MAXHOSTNAMELEN];
            gethostname(h, sizeof(h));
            opal_show_help("help-mca-base.txt", 
                           "find-available:not-valid", true,
                           h, framework_name, requested_component_names[i]);
            return OPAL_ERR_NOT_FOUND;
        }
    }

    return OPAL_SUCCESS;
}

int mca_base_component_parse_requested (const char *requested, bool *include_mode,
                                        char ***requested_component_names)
{
    const char *requested_orig = requested;

    *requested_component_names = NULL;
    *include_mode = true;

    /* See if the user requested anything */
    if (NULL == requested || 0 == strlen (requested)) {
        return OPAL_SUCCESS;
    }

    /* Are we including or excluding?  We only allow the negate
       character to be the *first* character of the value (but be nice
       and allow any number of negate characters in the beginning). */
    *include_mode = requested[0] != negate[0];

    /* skip over all negate symbols at the beginning */
    requested += strspn (requested, negate);

    /* Double check to ensure that the user did not specify the negate
       character anywhere else in the value. */
    if (NULL != strstr (requested, negate)) {
        opal_show_help("help-mca-base.txt", 
                       "framework-param:too-many-negates",
                       true, requested_orig);
        return OPAL_ERROR;
    }

    /* Split up the value into individual component names */
    *requested_component_names = opal_argv_split(requested, ',');

    /* All done */
    return OPAL_SUCCESS;
}
