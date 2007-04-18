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

#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/os_path.h"
#include "opal/class/opal_value_array.h"
#include "opal/util/show_help.h"
#include "opal/class/opal_hash_table.h"
#if 0
/* JMS commented out for now -- see lookup_keyvals() below for an
   explanation */
#include "ompi/attribute/attribute.h"
#endif
#include "opal/util/printf.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/base/mca_base_param_internal.h"
#include "opal/constants.h"


/*
 * Public variables
 *
 * This variable is public, but not advertised in mca_base_param.h.
 * It's only public so that the file parser can see it.
 */
opal_list_t mca_base_param_file_values;


/*
 * local variables
 */
static opal_value_array_t mca_base_params;
static const char *mca_prefix = "OMPI_MCA_";
static char *home = NULL;
static bool initialized = false;


/*
 * local functions
 */
static int read_files(char *file_list);
static int param_register(const char *type_name,
                          const char *component_name,
                          const char *param_name,
                          const char *help_msg,
                          mca_base_param_type_t type,
                          bool internal,
                          bool read_only,
                          mca_base_param_storage_t *default_value,
                          mca_base_param_storage_t *file_value,
                          mca_base_param_storage_t *override_value,
                          mca_base_param_storage_t *current_value);
static bool param_lookup(size_t index, mca_base_param_storage_t *storage,
                         opal_hash_table_t *attrs);
static bool param_set_override(size_t index, 
                               mca_base_param_storage_t *storage,
                               mca_base_param_type_t type);
static bool lookup_override(mca_base_param_t *param,
                            mca_base_param_storage_t *storage);
static bool lookup_keyvals(mca_base_param_t *param,
                           mca_base_param_storage_t *storage,
                           opal_hash_table_t *attrs);
static bool lookup_env(mca_base_param_t *param,
                       mca_base_param_storage_t *storage);
static bool lookup_file(mca_base_param_t *param,
                        mca_base_param_storage_t *storage);
static bool lookup_default(mca_base_param_t *param,
                           mca_base_param_storage_t *storage);
static bool set(mca_base_param_type_t type,
                mca_base_param_storage_t *dest, mca_base_param_storage_t *src);
static void param_constructor(mca_base_param_t *p);
static void param_destructor(mca_base_param_t *p);
static void fv_constructor(mca_base_param_file_value_t *p);
static void fv_destructor(mca_base_param_file_value_t *p);
static void info_constructor(mca_base_param_info_t *p);
static void info_destructor(mca_base_param_info_t *p);


/*
 * Make the class instance for mca_base_param_t
 */
OBJ_CLASS_INSTANCE(mca_base_param_t, opal_object_t, 
                   param_constructor, param_destructor);
OBJ_CLASS_INSTANCE(mca_base_param_file_value_t, opal_list_item_t,
                   fv_constructor, fv_destructor);
OBJ_CLASS_INSTANCE(mca_base_param_info_t, opal_list_item_t,
                   info_constructor, info_destructor);



/*
 * Set it up
 */
int mca_base_param_init(void)
{
    int id;
    char *files, *new_files = NULL;

    if (!initialized) {

        /* Init the value array for the param storage */

        OBJ_CONSTRUCT(&mca_base_params, opal_value_array_t);
        opal_value_array_init(&mca_base_params, sizeof(mca_base_param_t));

        /* Init the file param value list */

        OBJ_CONSTRUCT(&mca_base_param_file_values, opal_list_t);

        /* Set this before we register the parameter, below */

        initialized = true;

        /* We may need this later */
#if !defined(__WINDOWS__)
        home = getenv("HOME");
        asprintf(&files,
                 "%s"OPAL_PATH_SEP".openmpi"OPAL_PATH_SEP"mca-params.conf:%s"OPAL_PATH_SEP"openmpi-mca-params.conf",
                 home, opal_install_dirs.sysconfdir);
#else
        home = getenv("USERPROFILE");
        asprintf(&files,
                 "%s"OPAL_PATH_SEP".openmpi"OPAL_PATH_SEP"mca-params.conf;%s"OPAL_PATH_SEP"openmpi-mca-params.conf",
                 home, opal_install_dirs.sysconfdir);
#endif  /* !defined(__WINDOWS__) */

        /* Initialize a parameter that says where MCA param files can
           be found */

        id = mca_base_param_reg_string_name("mca", "param_files",
                                            "Path for MCA configuration files containing default parameter values",
                                            false, false, files, &new_files);
        read_files(new_files);
        free(files);
        free(new_files);
    }

    return OPAL_SUCCESS;
}


/*
 * Register an integer MCA parameter 
 */
int mca_base_param_reg_int(const mca_base_component_t *component,
                           const char *param_name, 
                           const char *help_msg,
                           bool internal,
                           bool read_only,
                           int default_value,
                           int *current_value)
{
    int ret;
    mca_base_param_storage_t storage;
    mca_base_param_storage_t lookup;

    storage.intval = default_value;
    ret = param_register(component->mca_type_name, 
                         component->mca_component_name, 
                         param_name, help_msg, 
                         MCA_BASE_PARAM_TYPE_INT, internal, read_only,
                         &storage, NULL, NULL, &lookup);
    if (ret >= 0 && NULL != current_value) {
        *current_value = lookup.intval;
    }
    return ret;
}


/*
 * Register an integer MCA parameter that is not associated with a
 * component
 */
int mca_base_param_reg_int_name(const char *type,
                                const char *param_name, 
                                const char *help_msg,
                                bool internal,
                                bool read_only,
                                int default_value,
                                int *current_value)
{
    int ret;
    mca_base_param_storage_t storage;
    mca_base_param_storage_t lookup;

    storage.intval = default_value;
    ret = param_register(type, NULL, param_name, help_msg, 
                         MCA_BASE_PARAM_TYPE_INT, internal, read_only,
                         &storage, NULL, NULL, &lookup);
    if (ret >= 0 && NULL != current_value) {
        *current_value = lookup.intval;
    }
    return ret;
}


/*
 * Register a string MCA parameter.
 */
int mca_base_param_reg_string(const mca_base_component_t *component,
                              const char *param_name, 
                              const char *help_msg,
                              bool internal,
                              bool read_only,
                              const char *default_value,
                              char **current_value)
{
    int ret;
    mca_base_param_storage_t storage;
    mca_base_param_storage_t lookup;
    
    if (NULL != default_value) {
        storage.stringval = (char *) default_value;
    } else {
        storage.stringval = NULL;
    }
    ret = param_register(component->mca_type_name,
                         component->mca_component_name,
                         param_name, help_msg, 
                         MCA_BASE_PARAM_TYPE_STRING, internal, read_only,
                         &storage, NULL, NULL, 
                         (NULL != current_value) ? &lookup : NULL);
    if (ret >= 0 && NULL != current_value) {
        *current_value = lookup.stringval;
    }
    return ret;
}


/*
 * Register a string MCA parameter that is not associated with a
 * component
 */
int mca_base_param_reg_string_name(const char *type,
                                   const char *param_name, 
                                   const char *help_msg,
                                   bool internal,
                                   bool read_only,
                                   const char *default_value,
                                   char **current_value)
{
    int ret;
    mca_base_param_storage_t storage;
    mca_base_param_storage_t lookup;
    
    if (NULL != default_value) {
        storage.stringval = (char *) default_value;
    } else {
        storage.stringval = NULL;
    }
    ret = param_register(type, NULL, param_name, help_msg, 
                         MCA_BASE_PARAM_TYPE_STRING, internal, read_only,
                         &storage, NULL, NULL, 
                         (NULL != current_value) ? &lookup : NULL);
    if (ret >= 0 && NULL != current_value) {
        *current_value = lookup.stringval;
    }
    return ret;
}


/*
 * Register an integer MCA parameter 
 * (deprecated)
 */
int mca_base_param_register_int(const char *type_name, 
                                const char *component_name,
                                const char *param_name, 
                                const char *mca_param_name, 
                                int default_value)
{
    int ret;
    mca_base_param_storage_t storage;

    storage.intval = default_value;
    ret = param_register(type_name, component_name, param_name, mca_param_name,
                         MCA_BASE_PARAM_TYPE_INT, false, false,
                         &storage, NULL, NULL, NULL);
    return ret;
}


/*
 * Register a string MCA parameter.
 * (deprecated)
 */
int mca_base_param_register_string(const char *type_name, 
                                   const char *component_name,
                                   const char *param_name, 
                                   const char *mca_param_name,
                                   const char *default_value)
{
    int ret;
    mca_base_param_storage_t storage;

    if (NULL != default_value) {
        storage.stringval = (char *) default_value;
    } else {
        storage.stringval = NULL;
    }
    ret = param_register(type_name, component_name, param_name, mca_param_name,
                         MCA_BASE_PARAM_TYPE_STRING, false, false,
                         &storage, NULL, NULL, NULL);
    return ret;
}


/*
 * Associate a keyval with a parameter index
 */
int mca_base_param_kv_associate(int index, int keyval)
{
  size_t len;
  mca_base_param_t *array;

  if (!initialized) {
    return OPAL_ERROR;
  }

  len = opal_value_array_get_size(&mca_base_params);
  if (((size_t) index) > len) {
    return OPAL_ERROR;
  }

  /* We have a valid entry (remember that we never delete MCA
     parameters, so if the index is >0 and <len, it must be good), so
     save the keyval */

  array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
  array[index].mbp_keyval = keyval;

  /* All done */

  return OPAL_SUCCESS;
}


/*
 * Look up an integer MCA parameter.
 */
int mca_base_param_lookup_int(int index, int *value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, NULL)) {
    *value = storage.intval;
    return OPAL_SUCCESS;
  }
  return OPAL_ERROR;
}


/*
 * Look up an integer MCA parameter, including in attributes
 */
int mca_base_param_kv_lookup_int(int index, opal_hash_table_t *attrs, 
                                 int *value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, attrs)) {
    *value = storage.intval;
    return OPAL_SUCCESS;
  }
  return OPAL_ERROR;
}


/*
 * Set an integer parameter
 */
int mca_base_param_set_int(int index, int value)
{
    mca_base_param_storage_t storage;

    mca_base_param_unset(index);
    storage.intval = value;
    param_set_override(index, &storage, MCA_BASE_PARAM_TYPE_INT);
    return OPAL_SUCCESS;
}


/*
 * Look up a string MCA parameter.
 */
int mca_base_param_lookup_string(int index, char **value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, NULL)) {
    *value = storage.stringval;
    return OPAL_SUCCESS;
  }
  return OPAL_ERROR;
}


/*
 * Look up a string MCA parameter, including in attributes.
 */
int mca_base_param_kv_lookup_string(int index, opal_hash_table_t *attrs, 
                                    char **value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, attrs)) {
    *value = storage.stringval;
    return OPAL_SUCCESS;
  }
  return OPAL_ERROR;
}


/*
 * Set an string parameter
 */
int mca_base_param_set_string(int index, char *value)
{
    mca_base_param_storage_t storage;

    mca_base_param_unset(index);
    storage.stringval = strdup(value);
    param_set_override(index, &storage, MCA_BASE_PARAM_TYPE_STRING);
    return OPAL_SUCCESS;
}


/*
 * Unset a parameter
 */
int mca_base_param_unset(int index)
{
    size_t len;
    mca_base_param_t *array;

    if (!initialized) {
        return OPAL_ERROR;
    }

    len = opal_value_array_get_size(&mca_base_params);
    if (((size_t) index) > len) {
        return OPAL_ERROR;
    }

    /* We have a valid entry (remember that we never delete MCA
       parameters, so if the index is >0 and <len, it must be good),
       so save the internal flag */

    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    if (array[index].mbp_override_value_set) {
        if (MCA_BASE_PARAM_TYPE_STRING == array[index].mbp_type &&
            NULL != array[index].mbp_override_value.stringval) {
            free(array[index].mbp_override_value.stringval);
            array[index].mbp_override_value.stringval = NULL;
        }
    }
    array[index].mbp_override_value_set = false;
  
    /* All done */

    return OPAL_SUCCESS;
}


char *mca_base_param_env_var(const char *param_name)
{
    char *name;

    asprintf(&name, "%s%s", mca_prefix, param_name);

    return name;
}

/*
 * Make a string suitable for the environment, setting an MCA param
 */
char *mca_base_param_environ_variable(const char *type,
                                      const char *component,
                                      const char *param)
{
    size_t len;
    int id;
    char *ret = NULL, *name;
    mca_base_param_t *array;

    if (NULL == type) {
        return NULL;
    }

    id = mca_base_param_find(type, component, param);
    if (OPAL_ERROR != id) {
        array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
        ret = strdup(array[id].mbp_env_var_name);
    } else {
        len = strlen(mca_prefix) + strlen(type) + 16;
        if (NULL != component) {
            len += strlen(component);
        }
        if (NULL != param) {
            len += strlen(param);
        }
        name = (char*)malloc(len);
        if (NULL == name) {
            return NULL;
        }
        name[0] = '\0';
        snprintf(name, len, "%s%s", mca_prefix, type);
        if (NULL != component) {
            strcat(name, "_");
            strcat(name, component);
        }
        if (NULL != param) {
            strcat(name, "_");
            strcat(name, param);
        }
        ret = name;
    }

    /* All done */

    return ret;
}


/*
 * Find the index for an MCA parameter based on its names.
 */
int mca_base_param_find(const char *type_name, const char *component_name, 
                        const char *param_name) 
{
  size_t i, size;
  mca_base_param_t *array;

  /* Check for bozo cases */

  if (!initialized) {
    return OPAL_ERROR;
  }

  /* Loop through looking for a parameter of a given
     type/component/param */

  size = opal_value_array_get_size(&mca_base_params);
  array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
  for (i = 0; i < size; ++i) {
    if (((NULL == type_name && NULL == array[i].mbp_type_name) ||
         (NULL != type_name && NULL != array[i].mbp_type_name &&
          (0 == strcmp(type_name, array[i].mbp_type_name)))) &&
        ((NULL == component_name && NULL == array[i].mbp_component_name) ||
         (NULL != component_name && NULL != array[i].mbp_component_name &&
          0 == strcmp(component_name, array[i].mbp_component_name))) &&
        ((NULL == param_name && NULL == array[i].mbp_param_name) ||
         (NULL != param_name && NULL != array[i].mbp_param_name &&
          0 == strcmp(param_name, array[i].mbp_param_name)))) {
      return (int)i;
    }
  }

  /* Didn't find it */

  return OPAL_ERROR;
}


int mca_base_param_set_internal(int index, bool internal)
{
    size_t len;
    mca_base_param_t *array;

    /* Check for bozo cases */
    
    if (!initialized) {
        return OPAL_ERROR;
    }

    len = opal_value_array_get_size(&mca_base_params);
    if (((size_t) index) > len) {
        return OPAL_ERROR;
    }

    /* We have a valid entry (remember that we never delete MCA
       parameters, so if the index is >0 and <len, it must be good),
       so save the internal flag */

    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    array[index].mbp_internal = internal;
  
    /* All done */

    return OPAL_SUCCESS;
}


/*
 * Return a list of info of all currently registered parameters
 */
int mca_base_param_dump(opal_list_t **info, bool internal)
{
    size_t i, len;
    mca_base_param_info_t *p;
    mca_base_param_t *array;

    /* Check for bozo cases */
    
    if (!initialized) {
        return OPAL_ERROR;
    }

    if (NULL == info) {
        return OPAL_ERROR;
    }
    *info = OBJ_NEW(opal_list_t);

    /* Iterate through all the registered parameters */

    len = opal_value_array_get_size(&mca_base_params);
    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    for (i = 0; i < len; ++i) {
        if(array[i].mbp_internal == internal || internal) {
            p = OBJ_NEW(mca_base_param_info_t);
            p->mbpp_index = (int)i;
            p->mbpp_type_name = array[i].mbp_type_name;
            p->mbpp_component_name = array[i].mbp_component_name;
            p->mbpp_param_name = array[i].mbp_param_name;
            p->mbpp_full_name = array[i].mbp_full_name;
            p->mbpp_read_only = array[i].mbp_read_only;
            p->mbpp_type = array[i].mbp_type;
            p->mbpp_help_msg = array[i].mbp_help_msg;
            
            opal_list_append(*info, (opal_list_item_t*) p);
        }
    }

    /* All done */

    return OPAL_SUCCESS;
}


/*
 * Make an argv-style list of strings suitable for an environment
 */
int mca_base_param_build_env(char ***env, int *num_env, bool internal)
{
    size_t i, len;
    mca_base_param_t *array;
    char *str;
    mca_base_param_storage_t storage;

    /* Check for bozo cases */
    
    if (!initialized) {
        return OPAL_ERROR;
    }

    /* Iterate through all the registered parameters */

    len = opal_value_array_get_size(&mca_base_params);
    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    for (i = 0; i < len; ++i) {
        /* Don't output read-only values */
        if (array[i].mbp_read_only) {
            continue;
        }

        if (array[i].mbp_internal == internal || internal) {
            if (param_lookup(i, &storage, NULL)) {
                if (MCA_BASE_PARAM_TYPE_INT == array[i].mbp_type) {
                    asprintf(&str, "%s=%d", array[i].mbp_env_var_name, 
                             storage.intval);
                    opal_argv_append(num_env, env, str);
                    free(str);
                } else if (MCA_BASE_PARAM_TYPE_STRING == array[i].mbp_type) {
                    if (NULL != storage.stringval) {
                        asprintf(&str, "%s=%s", array[i].mbp_env_var_name, 
                                 storage.stringval);
                        free(storage.stringval);
                        opal_argv_append(num_env, env, str);
                        free(str);
                    } 
                } else {
                    goto cleanup;
                }
            } else {
                goto cleanup;
            }
        }
    }

    /* All done */

    return OPAL_SUCCESS;

    /* Error condition */

 cleanup:
    if (*num_env > 0) {
        opal_argv_free(*env);
        *num_env = 0;
        *env = NULL;
    }
    return OPAL_ERR_NOT_FOUND;
}


/*
 * Free a list -- and all associated memory -- that was previously
 * returned from mca_base_param_dump()
 */
int mca_base_param_dump_release(opal_list_t *info)
{
    opal_list_item_t *item;

    for (item = opal_list_remove_first(info); NULL != item;
         item = opal_list_remove_first(info)) {
        OBJ_RELEASE(item);
    }
    OBJ_RELEASE(info);

    return OPAL_SUCCESS;
}


/*
 * Shut down the MCA parameter system (normally only invoked by the
 * MCA framework itself).
 */
int mca_base_param_finalize(void)
{
    opal_list_item_t *item;
    mca_base_param_t *array;

    if (initialized) {

        /* This is slow, but effective :-) */

        array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
        while (opal_value_array_get_size(&mca_base_params) > 0) {
            OBJ_DESTRUCT(&array[0]);
            opal_value_array_remove_item(&mca_base_params, 0);
        }
        OBJ_DESTRUCT(&mca_base_params);

        for (item = opal_list_remove_first(&mca_base_param_file_values);
             NULL != item;
             item = opal_list_remove_first(&mca_base_param_file_values)) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&mca_base_param_file_values);
        initialized = false;
    }

    /* All done */

    return OPAL_SUCCESS;
}


/*************************************************************************/

static int read_files(char *file_list)
{
    int i, count;
    char **files;

    /* Iterate through all the files passed in -- read them in reverse
       order so that we preserve unix/shell path-like semantics (i.e.,
       the entries farthest to the left get precedence) */

    files = opal_argv_split(file_list, OPAL_ENV_SEP);
    count = opal_argv_count(files);

    for (i = count - 1; i >= 0; --i) {
        mca_base_parse_paramfile(files[i]);
    }
    opal_argv_free(files);

    return OPAL_SUCCESS;
}


static int param_register(const char *type_name,
                          const char *component_name,
                          const char *param_name,
                          const char *help_msg,
                          mca_base_param_type_t type,
                          bool internal,
                          bool read_only,
                          mca_base_param_storage_t *default_value,
                          mca_base_param_storage_t *file_value,
                          mca_base_param_storage_t *override_value,
                          mca_base_param_storage_t *current_value)
{
  int ret;
  size_t i, len;
  mca_base_param_t param, *array;

  /* There are data holes in the param struct */
  OMPI_DEBUG_ZERO(param);

  /* Initialize the array if it has never been initialized */

  if (!initialized) {
      mca_base_param_init();
  }

  /* Create a parameter entry.  If a keyval is to be used, it will be
     registered elsewhere.  We simply assign -1 here. */

  OBJ_CONSTRUCT(&param, mca_base_param_t);
  param.mbp_type = type;
  param.mbp_keyval = -1;
  param.mbp_internal = internal;
  param.mbp_read_only = read_only;
  if (NULL != help_msg) {
      param.mbp_help_msg = strdup(help_msg);
  }

  if (NULL != type_name) {
      param.mbp_type_name = strdup(type_name);
      if (NULL == param.mbp_type_name) {
          OBJ_DESTRUCT(&param);
          return OPAL_ERR_OUT_OF_RESOURCE;
      }
  }
  if (NULL != component_name) {
      param.mbp_component_name = strdup(component_name);
      if (NULL == param.mbp_component_name) {
          OBJ_DESTRUCT(&param);
          return OPAL_ERR_OUT_OF_RESOURCE;
      }
  }
  param.mbp_param_name = NULL;
  if (NULL != param_name) {
      param.mbp_param_name = strdup(param_name);
      if (NULL == param.mbp_param_name) {
          OBJ_DESTRUCT(&param);
          return OPAL_ERR_OUT_OF_RESOURCE;
      }
  }

  param.mbp_env_var_name = NULL;
  len = 16;
  if (NULL != type_name) {
      len += strlen(type_name);
  }
  if (NULL != param.mbp_component_name) {
      len += strlen(param.mbp_component_name);
  }
  if (NULL != param.mbp_param_name) {
      len += strlen(param.mbp_param_name);
  }
  
  param.mbp_full_name = (char*)malloc(len);
  if (NULL == param.mbp_full_name) {
      OBJ_DESTRUCT(&param);
      return OPAL_ERROR;
  }
  
  /* Copy the name over in parts */
  
  param.mbp_full_name[0] = '\0';
  if (NULL != type_name) {
      strncat(param.mbp_full_name, type_name, len);
  }
  if (NULL != component_name) {
      if ('\0' != param.mbp_full_name[0]) {
          strcat(param.mbp_full_name, "_");
      }
      strcat(param.mbp_full_name, component_name);
  }
  if (NULL != param_name) {
      if ('\0' != param.mbp_full_name[0]) {
          strcat(param.mbp_full_name, "_");
      }
      strcat(param.mbp_full_name, param_name);
  }

  /* Create the environment name */

  len = strlen(param.mbp_full_name) + strlen(mca_prefix) + 16;
  param.mbp_env_var_name = (char*)malloc(len);
  if (NULL == param.mbp_env_var_name) {
    OBJ_DESTRUCT(&param);
    return OPAL_ERROR;
  }
  snprintf(param.mbp_env_var_name, len, "%s%s", mca_prefix, 
           param.mbp_full_name);

  /* Figure out the default value; zero it out if a default is not
     provided */

  if (NULL != default_value) {
    if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
        NULL != default_value->stringval) {
      param.mbp_default_value.stringval = strdup(default_value->stringval);
    } else {
      param.mbp_default_value = *default_value;
    }
  } else {
    memset(&param.mbp_default_value, 0, sizeof(param.mbp_default_value));
  }

  /* Figure out the file value; zero it out if a file is not
     provided */

  if (NULL != file_value) {
    if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
        NULL != file_value->stringval) {
      param.mbp_file_value.stringval = strdup(file_value->stringval);
    } else {
      param.mbp_file_value = *file_value;
    }
    param.mbp_file_value_set = true;
  } else {
    memset(&param.mbp_file_value, 0, sizeof(param.mbp_file_value));
    param.mbp_file_value_set = false;
  }

  /* Figure out the override value; zero it out if a override is not
     provided */

  if (NULL != override_value) {
    if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
        NULL != override_value->stringval) {
      param.mbp_override_value.stringval = strdup(override_value->stringval);
    } else {
      param.mbp_override_value = *override_value;
    }
    param.mbp_override_value_set = true;
  } else {
    memset(&param.mbp_override_value, 0, sizeof(param.mbp_override_value));
    param.mbp_override_value_set = false;
  }

  /* See if this entry is already in the array */

  len = opal_value_array_get_size(&mca_base_params);
  array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
  for (i = 0; i < len; ++i) {
    if (0 == strcmp(param.mbp_full_name, array[i].mbp_full_name)) {

        /* We found an entry with the same param name.  Check to see
           if we're changing types */
        /* Easy case: both are INT */

      if (MCA_BASE_PARAM_TYPE_INT == array[i].mbp_type &&
          MCA_BASE_PARAM_TYPE_INT == param.mbp_type) {
          if (NULL != default_value) {
              array[i].mbp_default_value.intval =
                  param.mbp_default_value.intval;
          }
          if (NULL != file_value) {
              array[i].mbp_file_value.intval =
                  param.mbp_file_value.intval;
              array[i].mbp_file_value_set = true;
          }
          if (NULL != override_value) {
              array[i].mbp_override_value.intval =
                  param.mbp_override_value.intval;
              array[i].mbp_override_value_set = true;
          }
      } 

      /* Both are STRING */

      else if (MCA_BASE_PARAM_TYPE_STRING == array[i].mbp_type &&
               MCA_BASE_PARAM_TYPE_STRING == param.mbp_type) {
          if (NULL != default_value) {
              if (NULL != array[i].mbp_default_value.stringval) {
                  free(array[i].mbp_default_value.stringval);
                  array[i].mbp_default_value.stringval = NULL;
              }
              if (NULL != param.mbp_default_value.stringval) {
                  array[i].mbp_default_value.stringval =
                      strdup(param.mbp_default_value.stringval);
              }
          }

          if (NULL != file_value) {
              if (NULL != array[i].mbp_file_value.stringval) {
                  free(array[i].mbp_file_value.stringval);
                  array[i].mbp_file_value.stringval = NULL;
              }
              if (NULL != param.mbp_file_value.stringval) {
                  array[i].mbp_file_value.stringval =
                      strdup(param.mbp_file_value.stringval);
              }
              array[i].mbp_file_value_set = true;
          }

          if (NULL != override_value) {
              if (NULL != array[i].mbp_override_value.stringval) {
                  free(array[i].mbp_override_value.stringval);
                  array[i].mbp_override_value.stringval = NULL;
              }
              if (NULL != param.mbp_override_value.stringval) {
                  array[i].mbp_override_value.stringval =
                      strdup(param.mbp_override_value.stringval);
              }
              array[i].mbp_override_value_set = true;
          }
      } 

      /* Original is INT, new is STRING */

      else if (MCA_BASE_PARAM_TYPE_INT == array[i].mbp_type &&
               MCA_BASE_PARAM_TYPE_STRING == param.mbp_type) {
          if (NULL != default_value && 
              NULL != param.mbp_default_value.stringval) {
              array[i].mbp_default_value.stringval =
                  strdup(param.mbp_default_value.stringval);
          }

          if (NULL != file_value &&
              NULL != param.mbp_file_value.stringval) {
              array[i].mbp_file_value.stringval =
                  strdup(param.mbp_file_value.stringval);
              array[i].mbp_file_value_set = true;
          }

          if (NULL != override_value &&
              NULL != param.mbp_override_value.stringval) {
              array[i].mbp_override_value.stringval =
                  strdup(param.mbp_override_value.stringval);
              array[i].mbp_override_value_set = true;
          }

          array[i].mbp_type = param.mbp_type;
      } 

      /* Original is STRING, new is INT */

      else if (MCA_BASE_PARAM_TYPE_STRING == array[i].mbp_type &&
                 MCA_BASE_PARAM_TYPE_INT == param.mbp_type) {
          if (NULL != default_value) {
              if (NULL != array[i].mbp_default_value.stringval) {
                  free(array[i].mbp_default_value.stringval);
              }
              array[i].mbp_default_value.intval =
                  param.mbp_default_value.intval;
          }
          if (NULL != file_value) {
              if (NULL != array[i].mbp_file_value.stringval) {
                  free(array[i].mbp_file_value.stringval);
              }
              array[i].mbp_file_value.intval =
                  param.mbp_file_value.intval;
              array[i].mbp_file_value_set = true;
          }
          if (NULL != override_value) {
              if (NULL != array[i].mbp_override_value.stringval) {
                  free(array[i].mbp_override_value.stringval);
              }
              array[i].mbp_override_value.intval =
                  param.mbp_override_value.intval;
              array[i].mbp_override_value_set = true;
          }

          array[i].mbp_type = param.mbp_type;
      }

      /* Now delete the newly-created entry (since we just saved the
         value in the old entry) */

      OBJ_DESTRUCT(&param);

      /* Finally, if we have a lookup value, look it up */
      
      if (NULL != current_value) {
          if (!param_lookup(i, current_value, NULL)) {
              return OPAL_ERR_NOT_FOUND;
          }
      }

      /* Return the new index */

      return (int)i;
    }
  }

  /* Add it to the array */

  if (OPAL_SUCCESS != 
      (ret = opal_value_array_append_item(&mca_base_params, &param))) {
    return ret;
  }
  ret = (int)opal_value_array_get_size(&mca_base_params) - 1;

  /* Finally, if we have a lookup value, look it up */

  if (NULL != current_value) {
      if (!param_lookup(ret, current_value, NULL)) {
          return OPAL_ERR_NOT_FOUND;
      }
  }

  /* All done */

  return ret;
}


/*
 * Set an override
 */
static bool param_set_override(size_t index, 
                               mca_base_param_storage_t *storage,
                               mca_base_param_type_t type)
{
    size_t size;
    mca_base_param_t *array;

    /* Lookup the index and see if it's valid */

    if (!initialized) {
        return false;
    }
    size = opal_value_array_get_size(&mca_base_params);
    if (index > size) {
        return false;
    }

    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    if (MCA_BASE_PARAM_TYPE_INT == type) {
        array[index].mbp_override_value.intval = storage->intval;
    } else if (MCA_BASE_PARAM_TYPE_STRING == type) {
        if (NULL != storage->stringval) {
            array[index].mbp_override_value.stringval = 
                strdup(storage->stringval);
        } else {
            array[index].mbp_override_value.stringval = NULL;
        }
    }
    array[index].mbp_override_value_set = true;

    return true;
}


/*
 * Lookup a parameter in multiple places
 */
static bool param_lookup(size_t index, mca_base_param_storage_t *storage,
                         opal_hash_table_t *attrs)
{
    size_t size;
    mca_base_param_t *array;
    char *p, *q;
    bool found;

    /* Lookup the index and see if it's valid */

    if (!initialized) {
        return false;
    }
    size = opal_value_array_get_size(&mca_base_params);
    if (index > size) {
        return false;
    }
    array = OPAL_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);

    /* Ensure that MCA param has a good type */

    if (MCA_BASE_PARAM_TYPE_INT != array[index].mbp_type &&
        MCA_BASE_PARAM_TYPE_STRING != array[index].mbp_type) {
        return false;
    }

    /* Check all the places that the param may be hiding, in priority
       order -- but if read_only is true, then only look at the
       default location. */

    if (array[index].mbp_read_only) {
        if (lookup_override(&array[index], storage) ||
             lookup_keyvals(&array[index], storage, attrs) ||
             lookup_env(&array[index], storage) ||
             lookup_file(&array[index], storage)) {
            opal_show_help("help-mca-param.txt", "read-only-param-set",
                           true, array[index].mbp_full_name);
        }
        found = lookup_default(&array[index], storage);
    } else {
        found = (lookup_override(&array[index], storage) ||
                 lookup_keyvals(&array[index], storage, attrs) ||
                 lookup_env(&array[index], storage) ||
                 lookup_file(&array[index], storage) ||
                 lookup_default(&array[index], storage));
    }
    if (found) {
        
        /* If we're returning a string, replace all instances of "~/"
           with the user's home directory */

        if (MCA_BASE_PARAM_TYPE_STRING == array[index].mbp_type &&
            NULL != storage->stringval) {
            if (0 == strncmp(storage->stringval, "~/", 2)) {
                if( NULL == home ) {
                    asprintf(&p, "%s", storage->stringval + 2);
                } else {
                    p = opal_os_path( false, home, storage->stringval + 2, NULL );
                }
                free(storage->stringval);
                storage->stringval = p;
            }

            p = strstr(storage->stringval, ":~/");
            while (NULL != p) {
                *p = '\0';
                if( NULL == home ) {
                    asprintf(&q, "%s:%s", storage->stringval, p + 2);
                } else {
                    asprintf(&q, "%s:%s%s", storage->stringval, home, p + 2);
                }
                free(storage->stringval);
                storage->stringval = q;
                p = strstr(storage->stringval, ":~/");
            }
        }

        return true;
    }

    /* Didn't find it.  Doh! */
  
    return false;
}


/*
 * Lookup a param in the overrides section
 */
static bool lookup_override(mca_base_param_t *param,
                            mca_base_param_storage_t *storage)
{
    if (param->mbp_override_value_set) {
        if (MCA_BASE_PARAM_TYPE_INT == param->mbp_type) {
            storage->intval = param->mbp_override_value.intval;
        } else if (MCA_BASE_PARAM_TYPE_STRING == param->mbp_type) {
            storage->stringval = strdup(param->mbp_override_value.stringval);
        }

        return true;
    }

    /* Don't have an override */

    return false;
}


/*
 * Lookup a param in the set of attributes/keyvals
 */
static bool lookup_keyvals(mca_base_param_t *param,
                           mca_base_param_storage_t *storage,
                           opal_hash_table_t *attrs)
{
#if 1
    /* JMS: Comment this out for now, because it drags in all of
       libmpi.  This is undesirable for programs like mpirun, etc.
       Need a better solution for this -- perhaps a registration kind
       of thing...? */
    return false;
#else
  int err, flag;

  /* If this param has a keyval and we were provided with a hash
     table, look it up and see if we can find a value */

  if (-1 != param->mbp_keyval) {

    /* Use the stringval member of the union because it's definitely
       big enough to handle both (int) and (char*) */

    err = ompi_attr_get(attrs, param->mbp_keyval, 
                        &storage->stringval, &flag);
    if (OPAL_SUCCESS == err && 1 == flag) {

      /* Because of alignment weirdness between (void*) and int, we
         must grab the lower sizeof(int) bytes from the (char*) in
         stringval, in case sizeof(int) != sizeof(char*). */

      if (MCA_BASE_PARAM_TYPE_INT == param->mbp_type) {
        storage->intval = *((int *) (storage->stringval +
                                     sizeof(void *) - sizeof(int)));
      }

      /* Nothing to do for string -- we already have the value loaded
         in the right place */

      return true;
    }
  }

  /* Either this param has not keyval or we didn't find the keyval */

  return false;
#endif
}


/*
 * Lookup a param in the environment
 */
static bool lookup_env(mca_base_param_t *param,
                       mca_base_param_storage_t *storage)
{
  char *env;

  if (NULL != param->mbp_env_var_name &&
      NULL != (env = getenv(param->mbp_env_var_name))) {
    if (MCA_BASE_PARAM_TYPE_INT == param->mbp_type) {
      storage->intval = atoi(env);
    } else if (MCA_BASE_PARAM_TYPE_STRING == param->mbp_type) {
      storage->stringval = strdup(env);
    }

    return true;
  }

  /* Didn't find it */

  return false;
}


/*
 * Lookup a param in the files
 */
static bool lookup_file(mca_base_param_t *param,
                        mca_base_param_storage_t *storage)
{
    opal_list_item_t *item;
    mca_base_param_file_value_t *fv;

    /* See if we previously found a match from a file.  If so, just
       return that */

    if (param->mbp_file_value_set) {
        return set(param->mbp_type, storage, &param->mbp_file_value);
    }

    /* Scan through the list of values read in from files and try to
       find a match.  If we do, cache it on the param (for future
       lookups) and save it in the storage. */

    for (item = opal_list_get_first(&mca_base_param_file_values);
         opal_list_get_end(&mca_base_param_file_values) != item;
         item = opal_list_get_next(item)) {
        fv = (mca_base_param_file_value_t *) item;
        if (0 == strcmp(fv->mbpfv_param, param->mbp_full_name)) {
            if (MCA_BASE_PARAM_TYPE_INT == param->mbp_type) {
                if (NULL != fv->mbpfv_value) {
                    param->mbp_file_value.intval = atoi(fv->mbpfv_value);
                } else {
                    param->mbp_file_value.intval = 0;
                }
            } else {
                param->mbp_file_value.stringval = fv->mbpfv_value;
                fv->mbpfv_value = NULL;
            }
            param->mbp_file_value_set = true;

            /* Since this is now cached on the param, we might as well
               remove it from the list and make future file lookups
               faster */

            opal_list_remove_item(&mca_base_param_file_values, 
                                  (opal_list_item_t *) fv);
            OBJ_RELEASE(fv);

            return set(param->mbp_type, storage, &param->mbp_file_value);
        }
    }

    return false;
}


/*
 * Return the default value for a param
 */
static bool lookup_default(mca_base_param_t *param,
                           mca_base_param_storage_t *storage)
{
    return set(param->mbp_type, storage, &param->mbp_default_value);
}


static bool set(mca_base_param_type_t type,
                mca_base_param_storage_t *dest, mca_base_param_storage_t *src)
{
    switch (type) {
    case MCA_BASE_PARAM_TYPE_INT:
        dest->intval = src->intval;
        break;
        
    case MCA_BASE_PARAM_TYPE_STRING:
        if (NULL != src->stringval) {
            dest->stringval = strdup(src->stringval);
        } else {
            dest->stringval = NULL;
        }
        break;
        
    default:
        return false;
        break;
    }

    return true;
}


/*
 * Create an empty param container
 */
static void param_constructor(mca_base_param_t *p)
{
    p->mbp_type = MCA_BASE_PARAM_TYPE_MAX;
    p->mbp_internal = false;
    p->mbp_read_only = false;

    p->mbp_type_name = NULL;
    p->mbp_component_name = NULL;
    p->mbp_param_name = NULL;
    p->mbp_full_name = NULL;
    p->mbp_help_msg = NULL;

    p->mbp_keyval = -1;
    p->mbp_env_var_name = NULL;

    p->mbp_default_value.stringval = NULL;
    p->mbp_file_value_set = false;
    p->mbp_file_value.stringval = NULL;
    p->mbp_override_value_set = false;
    p->mbp_override_value.stringval = NULL;
}


/*
 * Free all the contents of a param container
 */
static void param_destructor(mca_base_param_t *p)
{
    if (NULL != p->mbp_type_name) {
        free(p->mbp_type_name);
    }
    if (NULL != p->mbp_component_name) {
        free(p->mbp_component_name);
    }
    if (NULL != p->mbp_param_name) {
        free(p->mbp_param_name);
    }
    if (NULL != p->mbp_env_var_name) {
        free(p->mbp_env_var_name);
    }
    if (NULL != p->mbp_full_name) {
        free(p->mbp_full_name);
    }
    if (NULL != p->mbp_help_msg) {
        free(p->mbp_help_msg);
    }
    if (MCA_BASE_PARAM_TYPE_STRING == p->mbp_type) {
        if (NULL != p->mbp_default_value.stringval) {
            free(p->mbp_default_value.stringval);
        }
        if (p->mbp_file_value_set &&
            NULL != p->mbp_file_value.stringval) {
            free(p->mbp_file_value.stringval);
        }
        if (p->mbp_override_value_set && 
            NULL != p->mbp_override_value.stringval) {
            free(p->mbp_override_value.stringval);
        }
    }
    param_constructor(p);
}


static void fv_constructor(mca_base_param_file_value_t *f)
{
    f->mbpfv_param = NULL;
    f->mbpfv_value = NULL;
}


static void fv_destructor(mca_base_param_file_value_t *f)
{
    if (NULL != f->mbpfv_param) {
        free(f->mbpfv_param);
    }
    if (NULL != f->mbpfv_value) {
        free(f->mbpfv_value);
    }
    fv_constructor(f);
}

static void info_constructor(mca_base_param_info_t *p)
{
    p->mbpp_index = -1;
    p->mbpp_type = MCA_BASE_PARAM_TYPE_MAX;

    p->mbpp_type_name = NULL;
    p->mbpp_component_name = NULL;
    p->mbpp_param_name = NULL;
    p->mbpp_full_name = NULL;

    p->mbpp_read_only = false;
    p->mbpp_help_msg = NULL;
}

static void info_destructor(mca_base_param_info_t *p)
{
    /* No need to free any of the strings -- the pointers were copied
       by value from their corresponding parameter registration */

    info_constructor(p);
}
