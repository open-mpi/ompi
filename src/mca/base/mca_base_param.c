/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "include/constants.h"
#include "class/ompi_value_array.h"
#include "class/ompi_hash_table.h"
#include "attribute/attribute.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"


/*
 * Public variables
 *
 * This variable is public, but not advertised in mca_base_param.h.
 * It's only public so that ompi_info can see it.  The relevant module
 * in ompi_info will provide an extern to see this variable.
 */
ompi_value_array_t mca_base_params;


/*
 * local variables
 */
static char *mca_prefix = "OMPI_MCA_";
static bool initialized = false;


/*
 * local functions
 */
static int param_register(const char *type_name, const char *module_name, 
                          const char *param_name,
                          const char *mca_param_name,
                          mca_base_param_type_t type,
                          mca_base_param_storage_t *default_value);
static bool param_lookup(int index, mca_base_param_storage_t *storage,
                         ompi_hash_table_t *attrs);
static void param_free(mca_base_param_t *p);


/*
 * Register an integer MCA parameter 
 */
int mca_base_param_register_int(const char *type_name, 
                                const char *module_name,
                                const char *param_name, 
                                const char *mca_param_name, 
                                int default_value)
{
  mca_base_param_storage_t storage;

  storage.intval = default_value;
  return param_register(type_name, module_name, param_name, mca_param_name,
                        MCA_BASE_PARAM_TYPE_INT, &storage);
}


/*
 * Register a string MCA parameter.
 */
int mca_base_param_register_string(const char *type_name, 
                                   const char *module_name,
                                   const char *param_name, 
                                   const char *mca_param_name,
                                   const char *default_value)
{
  mca_base_param_storage_t storage;
  if (NULL != default_value) {
    storage.stringval = (char *) default_value;
  } else {
    storage.stringval = NULL;
  }
  return param_register(type_name, module_name, param_name, mca_param_name,
                        MCA_BASE_PARAM_TYPE_STRING, &storage);
}


/*
 * Associate a keyval with a parameter index
 */
int mca_base_param_kv_associate(int index, int keyval)
{
  size_t len;
  mca_base_param_t *array;

  if (!initialized) {
    return OMPI_ERROR;
  }

  len = ompi_value_array_get_size(&mca_base_params);
  if (0 > index || index > len) {
    return OMPI_ERROR;
  }

  /* We have a valid entry (remember that we never delete MCA
     parameters, so if the index is >0 and <len, it must be good), so
     save the keyval */

  array = OMPI_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
  array[index].mbp_keyval = keyval;

  /* All done */

  return OMPI_SUCCESS;
}

/*
 * Look up an integer MCA parameter.
 */
int mca_base_param_lookup_int(int index, int *value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, NULL)) {
    *value = storage.intval;
    return OMPI_SUCCESS;
  }
  return OMPI_ERROR;
}


/*
 * Look up an integer MCA parameter, including in attributes
 */
int mca_base_param_kv_lookup_int(int index, ompi_hash_table_t *attrs, 
                                 int *value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, attrs)) {
    *value = storage.intval;
    return OMPI_SUCCESS;
  }
  return OMPI_ERROR;
}


/*
 * Look up a string MCA parameter.
 */
int mca_base_param_lookup_string(int index, char **value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, NULL)) {
    *value = storage.stringval;
    return OMPI_SUCCESS;
  }
  return OMPI_ERROR;
}


/*
 * Look up a string MCA parameter, including in attributes.
 */
int mca_base_param_kv_lookup_string(int index, ompi_hash_table_t *attrs, 
                                    char **value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage, attrs)) {
    *value = storage.stringval;
    return OMPI_SUCCESS;
  }
  return OMPI_ERROR;
}


/*
 * Find the index for an MCA parameter based on its names.
 */
int mca_base_param_find(const char *type_name, const char *module_name, 
                        const char *param_name) 
{
  size_t i, size;
  mca_base_param_t *array;

  /* Check for bozo cases */

  if (!initialized) {
    return OMPI_ERROR;
  }
  if (NULL == type_name || NULL == param_name) {
    return OMPI_ERROR;
  }

  /* Loop through looking for a parameter of a given
     type/module/param */

  size = ompi_value_array_get_size(&mca_base_params);
  array = OMPI_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
  for (i = 0; i < size; ++i) {
    if (0 == strcmp(type_name, array[i].mbp_type_name) &&
        ((NULL == module_name && NULL == array[i].mbp_module_name) ||
         (NULL != module_name && NULL != array[i].mbp_module_name &&
          0 == strcmp(module_name, array[i].mbp_module_name))) &&
        0 == strcmp(param_name, array[i].mbp_param_name)) {
      return i;
    }
  }

  /* Didn't find it */

  return OMPI_ERROR;
}


/*
 * Shut down the MCA parameter system (normally only invoked by the
 * MCA framework itself).
 */
int mca_base_param_finalize(void)
{
  mca_base_param_t *array;

  if (initialized) {
    array = OMPI_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
    while (0 < ompi_value_array_get_size(&mca_base_params)) {
      param_free(&array[0]);
      ompi_value_array_remove_item(&mca_base_params, 0);
    }
    OBJ_DESTRUCT(&mca_base_params);
    initialized = false;
  }

  return OMPI_SUCCESS;
}


/*************************************************************************/

static int param_register(const char *type_name, const char *module_name, 
                          const char *param_name, const char *mca_param_name,
                          mca_base_param_type_t type,
                          mca_base_param_storage_t *default_value)
{
  size_t i, len;
  mca_base_param_t param, *array;

  /* Initialize the array if it has never been initialized */

  if (!initialized) {
    OBJ_CONSTRUCT(&mca_base_params, ompi_value_array_t);
    ompi_value_array_init(&mca_base_params, sizeof(mca_base_param_t));
    initialized = true;
  }

  /* Create a parameter entry.  If a keyval is to be used, it will be
     registered elsewhere.  We simply assign -1 here. */

  param.mbp_type = type;
  param.mbp_keyval = -1;

  param.mbp_type_name = strdup(type_name);
  if (NULL == param.mbp_type_name) {
    return OMPI_ERROR;
  }
  if (NULL != module_name) {
    param.mbp_module_name = strdup(module_name);
    if (NULL == param.mbp_module_name) {
      free(param.mbp_type_name);
      return OMPI_ERROR;
    }
  } else {
    param.mbp_module_name = NULL;
  }
  if (param_name != NULL) {
    param.mbp_param_name = strdup(param_name);
    if (NULL == param.mbp_param_name) {
      free(param.mbp_type_name);
      free(param.mbp_module_name);
      return OMPI_ERROR;
    }
  } else {
    param.mbp_param_name = NULL;
  }

  /* The full parameter name may have been specified by the caller.
     If it was, use that (only for backwards compatability).
     Otherwise, derive it from the type, module, and parameter
     name. */

  param.mbp_env_var_name = NULL;
  if (MCA_BASE_PARAM_INFO != mca_param_name && NULL != mca_param_name) {
    param.mbp_full_name = strdup(mca_param_name);
  } else {
    len = 16 + strlen(type_name);

    if (NULL != module_name) {
      len += strlen(module_name);
    }
    if (NULL != param_name) {
      len += strlen(param_name);
    }

    param.mbp_full_name = malloc(len);
    if (NULL == param.mbp_full_name) {
      if (NULL != param.mbp_type_name) {
        free(param.mbp_type_name);
      }
      if (NULL != param.mbp_module_name) {
        free(param.mbp_module_name);
      }
      if (NULL != param.mbp_param_name) {
        free(param.mbp_param_name);
      }
      return OMPI_ERROR;
    }
    strncpy(param.mbp_full_name, type_name, len);

    if (NULL != module_name) {
      strcat(param.mbp_full_name, "_");
      strcat(param.mbp_full_name, module_name);
    }
    if (NULL != param_name) {
      strcat(param.mbp_full_name, "_");
      strcat(param.mbp_full_name, param_name);
    }
  }

  /* If mca_param_name isn't MCA_BASE_PARAM_INFO, then it's a
     lookup-able value.  So alloc the environment variable name as
     well. */

  if (MCA_BASE_PARAM_INFO != mca_param_name) {
    len = strlen(param.mbp_full_name) + strlen(mca_prefix) + 16;
    param.mbp_env_var_name = malloc(len);
    if (NULL == param.mbp_env_var_name) {
      free(param.mbp_full_name);
      free(param.mbp_type_name);
      free(param.mbp_module_name);
      free(param.mbp_param_name);
      return OMPI_ERROR;
    }
    snprintf(param.mbp_env_var_name, len, "%s%s", mca_prefix, 
             param.mbp_full_name);
  }

  /* Figure out the default value */

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

  /* See if this entry is already in the Array */

  len = ompi_value_array_get_size(&mca_base_params);
  array = OMPI_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);
  for (i = 0; i < len; ++i) {
    if (0 == strcmp(param.mbp_full_name, array[i].mbp_full_name)) {

      /* Copy in the new default value to the old entry */

      if (MCA_BASE_PARAM_TYPE_STRING == array[i].mbp_type &&
          NULL != array[i].mbp_default_value.stringval) {
        free(array[i].mbp_default_value.stringval);
      }
      if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
          NULL != param.mbp_default_value.stringval) {
        array[i].mbp_default_value.stringval =
          strdup(param.mbp_default_value.stringval);
      }

      param_free(&param);
      return i;
    }
  }

  /* Add it to the array */

  if (OMPI_SUCCESS != ompi_value_array_append_item(&mca_base_params, &param)) {
    return OMPI_ERROR;
  }
  return ompi_value_array_get_size(&mca_base_params) - 1;
}


/*
 * DO NOT MODIFY THIS FUNCTION WITHOUT ALSO MODIFYING mca_base_param.c!
 *
 * This function appears in libompi.  Because of unix linker semantics,
 * it's simply easier to essentially duplicate this function in libmpi
 * because in libmpi, we need to lookup on a keyval before looking in
 * the environment.  The logic is simpler if we just duplicate/alter
 * the code in mca_base_param.c rather than try to make this a) public,
 * and b) more general (to accomodate looking up keyvals while not
 * linking to MPI_Comm_get_attr() in libmpi).
 */
static bool param_lookup(int index, mca_base_param_storage_t *storage,
                         ompi_hash_table_t *attrs)
{
  size_t size;
  char *env;
  int err, flag;
  mca_base_param_t *array;

  /* Lookup the index and see if it's valid */

  if (!initialized) {
    return false;
  }
  if (ompi_value_array_get_size(&mca_base_params) < index) {
    return false;
  }
  size = ompi_value_array_get_size(&mca_base_params);
  array = OMPI_VALUE_ARRAY_GET_BASE(&mca_base_params, mca_base_param_t);

  /* Ensure that MCA param has a good type */

  if (MCA_BASE_PARAM_TYPE_INT != array[index].mbp_type &&
      MCA_BASE_PARAM_TYPE_STRING != array[index].mbp_type) {
    return false;
  }

  /* If this param has a keyval and we were provided with a hash
     table, look it up and see if we can find a value */

  if (-1 != array[index].mbp_keyval) {

    /* Use the stringval member of the union because it's definitely
       big enough to handle both (int) and (char*) */

    err = ompi_attr_get(attrs, array[index].mbp_keyval, 
                        &storage->stringval, &flag);
    if (OMPI_SUCCESS == err && 1 == flag) {

      /* Because of alignment weirdness between (void*) and int, it's
         simpler to just call ompi_attr_get with the right storage
         vehicle vs. trying to cast (extra) to (storage) */

      if (MCA_BASE_PARAM_TYPE_INT == array[index].mbp_type) {
        err = ompi_attr_get(attrs, array[index].mbp_keyval, 
                            &storage->intval, &flag);
      }

      /* Nothing to do for string -- we already have the value loaded
         in the right place */

      return true;
    }
  }

  /* We either don't have a keyval or didn't find it.  So look in the
     environment. */

  if (NULL != array[index].mbp_env_var_name &&
      NULL != (env = getenv(array[index].mbp_env_var_name))) {
    if (MCA_BASE_PARAM_TYPE_INT == array[index].mbp_type) {
      storage->intval = atoi(env);
    } else if (MCA_BASE_PARAM_TYPE_STRING == array[index].mbp_type) {
      storage->stringval = strdup(env);
    }

    return true;
  }

  /* Didn't find it; use the default value. */

  switch (array[index].mbp_type) {
  case MCA_BASE_PARAM_TYPE_INT:
    storage->intval = array[index].mbp_default_value.intval;
    break;

  case MCA_BASE_PARAM_TYPE_STRING:
    if (NULL != array[index].mbp_default_value.stringval) {
      storage->stringval = strdup(array[index].mbp_default_value.stringval);
    } else {
      storage->stringval = NULL;
    }
    break;

  default:
    return false;
  }

  /* All done */

  return true;
}


static void param_free(mca_base_param_t *p)
{
  if (NULL != p->mbp_type_name) {
    free(p->mbp_type_name);
  }
  if (NULL != p->mbp_module_name) {
    free(p->mbp_module_name);
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
  if (MCA_BASE_PARAM_TYPE_STRING == p->mbp_type &&
      NULL != p->mbp_default_value.stringval) {
    free(p->mbp_default_value.stringval);
  }
}
