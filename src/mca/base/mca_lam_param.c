/*
 * $HEADER$
 */

/** @file **/

#include "lam_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "constants.h"
#include "lfc/array.h"
#include "mca/mca.h"
#include "mca/base/mca_lam_param.h"


typedef enum {
  MCA_BASE_PARAM_TYPE_INT,
  MCA_BASE_PARAM_TYPE_STRING,

  MCA_BASE_PARAM_TYPE_MAX
} mca_base_param_type_t;

struct mca_base_param_t {
  lam_array_item_t super;

  mca_base_param_type_t mbp_type;
  char *mbp_type_name;
  char *mbp_module_name;
  char *mbp_param_name;
  char *mbp_full_name;

  int mbp_keyval;
  char *mbp_env_var_name;

  mca_base_param_storage_t mbp_default_value;
};
typedef struct mca_base_param_t mca_base_param_t;


/*
 * local variables
 */
static lam_array_t mca_base_params;
static char *mca_prefix = "LAM_MPI_MCA_";
static bool initialized = false;


/*
 * local functions
 */
static int param_register(const char *type_name, const char *module_name, const char *param_name,
                          const char *mca_param_name,
                          mca_base_param_type_t type,
                          mca_base_param_storage_t *default_value);
static bool param_lookup(int index, mca_base_param_storage_t *storage);
static int param_compare(const void *a, const void *b);
static void param_free(mca_base_param_t *p);


/**
 * Register an integer MCA parameter.
 *
 * @param type_name The MCA type (string).
 * @param module_name The name of the module (string).
 * @param param_name The name of the parameter being registered (string).
 * @param mca_param_name If NULL, the user-visible name of the
 * parameter is {type_name}_{module_name}_{param_name}.  If this
 * parameter is non-NULL, it is used instead of the default name.
 * @param default_value The value that is used for this parameter if
 * the user does not supply one.
 *
 * @retval LAM_ERROR Upon failure to register the parameter.
 * @retval index Index value that can be used with
 * mca_base_param_lookup_int() to retrieve the value of the parameter.
 *
 * This function registers an integer MCA parameter and associates it
 * with a specific module.
 *
 * In most cases, mca_param_name should be NULL.  Only in rare cases
 * is it necessary (or advisable) to override the default name.
 */
int mca_base_param_register_int(const char *type_name, const char *module_name,
                                const char *param_name, const char *mca_param_name, 
                                int default_value)
{
  mca_base_param_storage_t storage;

  storage.intval = default_value;
  return param_register(type_name, module_name, param_name, mca_param_name,
                        MCA_BASE_PARAM_TYPE_INT, &storage);
}


/**
 * Register a string MCA parameter.
 *
 * @param type_name The MCA type (string).
 * @param module_name The name of the module (string).
 * @param param_name The name of the parameter being registered (string).
 * @param mca_param_name If NULL, the user-visible name of the
 * parameter is {type_name}_{module_name}_{param_name}.  If this
 * parameter is non-NULL, it is used instead of the default name.
 * @param default_value The value that is used for this parameter if
 * the user does not supply one.
 *
 * @retval LAM_ERROR Upon failure to register the parameter.
 * @retval index Index value that can be used with
 * mca_base_param_lookup_string() to retrieve the value of the
 * parameter.
 *
 * This function registers an string MCA parameter and associates it
 * with a specific module.
 *
 * In most cases, mca_param_name should be NULL.  Only in rare cases
 * is it necessary (or advisable) to override the default name.
 */
int mca_base_param_register_string(const char *type_name, const char *module_name,
                                   const char *param_name, const char *mca_param_name,
                                   const char *default_value)
{
  mca_base_param_storage_t storage;
  storage.stringval = strdup(default_value);
  return param_register(type_name, module_name, param_name, mca_param_name,
                        MCA_BASE_PARAM_TYPE_STRING, &storage);
}


/**
 * Look up an integer MCA parameter.
 *
 * @param index Index previous returned from
 * mca_base_param_register_int().
 * @param value Pointer to int where the parameter value will be
 * stored.
 *
 * @retvalue LAM_ERROR Upon failure.  The contents of value are
 * undefined.
 * @retvalue LAM_SUCCESS Upon success.  value will be filled with the
 * parameter's current value.
 *
 * The value of a specific MCA parameter can be looked up using the
 * return value from mca_base_param_register_int().
 */
int mca_base_param_lookup_int(int index, int *value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage)) {
    *value = storage.intval;
    return LAM_SUCCESS;
  }
  return LAM_ERROR;
}


/**
 * Look up a string MCA parameter.
 *
 * @param index Index previous returned from
 * mca_base_param_register_string().
 * @param value Pointer to (char *) where the parameter value will be
 * stored.
 *
 * @retvalue LAM_ERROR Upon failure.  The contents of value are
 * undefined.
 * @retvalue LAM_SUCCESS Upon success.  value will be filled with the
 * parameter's current value.
 *
 * The value of a specific MCA parameter can be looked up using the
 * return value from mca_base_param_register_string().
 */
int mca_base_param_lookup_string(int index, char **value)
{
  mca_base_param_storage_t storage;
  
  if (param_lookup(index, &storage)) {
    *value = storage.stringval;
    return LAM_SUCCESS;
  }
  return LAM_ERROR;
}


/**
 * Find the index for an MCA parameter based on its names.
 *
 * @param type_name Name of the type containing the parameter.
 * @param module_name Name of the module containing the parameter.
 * @param param_name Name of the parameter.
 *
 * @retval LAM_ERROR If the parameter was not found.
 * @retval index If the parameter was found.
 *
 * It is not always convenient to widely propagate a parameter's index
 * value, or it may be necessary to look up the parameter from a
 * different module -- where it is not possible to have the return
 * value from mca_base_param_register_int() or
 * mca_base_param_register_string().  This function can be used to
 * look up the index of any registered parameter.  The returned index
 * can be used with mca_base_param_lookup_int() and
 * mca_base_param_lookup_string().
 */
int mca_base_param_find(const char *type_name, const char *module_name, const char *param_name) 
{
  size_t i, size;
  mca_base_param_t **array;

  /* Check for bozo cases */

  if (!initialized)
    return LAM_ERROR;
  if (NULL == type_name || NULL == param_name)
    return LAM_ERROR;

  /* Loop through looking for a parameter of a given
     type/module/param */

  array = (mca_base_param_t**) lam_arr_get_c_array(&mca_base_params, &size);
  for (i = 0; i < size; ++i) {
    if (0 == strcmp(type_name, array[i]->mbp_type_name) &&
        ((NULL == module_name && NULL == array[i]->mbp_module_name) ||
         (NULL != module_name && NULL != array[i]->mbp_module_name &&
          0 == strcmp(module_name, array[i]->mbp_module_name))) &&
        0 == strcmp(param_name, array[i]->mbp_param_name))
      return i;
  }

  /* Didn't find it */

  return LAM_ERROR;
}


/**
 * Shut down the MCA parameter system (normally only invoked by the
 * MCA framework itself).
 *
 * @returns LAM_SUCCESS This function never fails.
 *
 * This function shuts down the MCA parameter registry and frees all
 * associated memory.  No other mca_base_param*() functions can be
 * invoked after this function.
 *
 * This function is normally only invoked by the MCA framework itself
 * when the process is shutting down (e.g., during MPI_FINALIZE).  It
 * is only documented here for completeness.
 */
int mca_base_param_finalize(void)
{
  size_t i, size;
  mca_base_param_t **array;

  if (initialized) {
    array = (mca_base_param_t**) lam_arr_get_c_array(&mca_base_params, &size);
    for (i = 0; i < size; ++i)
      param_free(array[i]);

    lam_arr_destruct(&mca_base_params);
    initialized = false;
  }

  return LAM_SUCCESS;
}


/*************************************************************************/

static int 
param_register(const char *type_name, const char *module_name, const char *param_name,
               const char *mca_param_name,
               mca_base_param_type_t type,
               mca_base_param_storage_t *default_value)
{
  size_t i, len;
  mca_base_param_t param, **array;

  /* Initialize the array if it has never been initialized */

  if (!initialized) {
    lam_arr_construct(&mca_base_params);
    initialized = true;
  }

  /* Create a parameter entry.  If a keyval is to be used, it will be
     registered elsewhere.  We simply assign -1 here. */

  param.mbp_type = type;
  param.mbp_keyval = -1;

  param.mbp_type_name = strdup(type_name);
  if (NULL == param.mbp_type_name)
    return LAM_ERROR;
  if (NULL != module_name) {
    param.mbp_module_name = strdup(module_name);
    if (NULL == param.mbp_module_name) {
      LAM_FREE(param.mbp_type_name);
      return LAM_ERROR;
    }
  } else
    param.mbp_module_name = NULL;
  if (param_name != NULL) {
    param.mbp_param_name = strdup(param_name);
    if (NULL == param.mbp_param_name) {
      LAM_FREE(param.mbp_type_name);
      LAM_FREE(param.mbp_module_name);
      return LAM_ERROR;
    }
  } else
    param.mbp_param_name = NULL;

  /* The full parameter name may have been specified by the caller.
     If it was, use that (only for backwards compatability).
     Otherwise, derive it from the type, module, and parameter
     name. */

  param.mbp_env_var_name = NULL;
  if (MCA_BASE_PARAM_INFO != mca_param_name && NULL != mca_param_name) {
    param.mbp_full_name = strdup(mca_param_name);
  } else {
    len = 16 + strlen(type_name);

    if (NULL != module_name)
      len += strlen(module_name);
    if (NULL != param_name)
      len += strlen(param_name);

    param.mbp_full_name = LAM_MALLOC(len);
    if (NULL != param.mbp_full_name) {
      LAM_FREE(param.mbp_type_name);
      LAM_FREE(param.mbp_module_name);
      LAM_FREE(param.mbp_param_name);
      return LAM_ERROR;
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
     lookup-able value.  So amcagn the environment variable name as
     well. */

  if (MCA_BASE_PARAM_INFO != mca_param_name) {
    len = strlen(param.mbp_full_name) + strlen(mca_prefix) + 16;
    param.mbp_env_var_name = LAM_MALLOC(len);
    if (NULL == param.mbp_env_var_name) {
      LAM_FREE(param.mbp_full_name);
      LAM_FREE(param.mbp_type_name);
      LAM_FREE(param.mbp_module_name);
      LAM_FREE(param.mbp_param_name);
      return LAM_ERROR;
    }
    snprintf(param.mbp_env_var_name, len, "%s%s", mca_prefix, 
             param.mbp_full_name);
  }

  /* Figure out the default value */

  if (NULL != default_value) {
    if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
        NULL != default_value->stringval)
      param.mbp_default_value.stringval = strdup(default_value->stringval);
    else
      param.mbp_default_value = *default_value;
  } else
    memset(&param.mbp_default_value, 0, sizeof(param.mbp_default_value));

  /* See if this entry is already in the Array */

  array = (mca_base_param_t**) lam_arr_get_c_array(&mca_base_params, &len);
  for (i = 0; i < len; ++i)
    if (param_compare(&param, array[i]) == 0) {

      /* Copy in the new default value to the old entry */

      if (MCA_BASE_PARAM_TYPE_STRING == array[i]->mbp_type &&
          NULL != array[i]->mbp_default_value.stringval)
        LAM_FREE(array[i]->mbp_default_value.stringval);
      if (MCA_BASE_PARAM_TYPE_STRING == param.mbp_type &&
          NULL != param.mbp_default_value.stringval)
        array[i]->mbp_default_value.stringval =
          strdup(param.mbp_default_value.stringval);

      param_free(&param);
      return i;
    }

  /* Add it to the array */

  if (!lam_arr_append_item(&mca_base_params, (lam_object_t*) &param))
    return LAM_ERROR;
  return lam_arr_get_size(&mca_base_params) - 1;
}


/*
 * DO NOT MODIFY THIS FUNCTION WITHOUT ALSO MODIFYING mca_base_param.c!
 *
 * This function appears in liblam.  Because of unix linker semantics,
 * it's simply easier to essentially duplicate this function in libmpi
 * because in libmpi, we need to lookup on a keyval before looking in
 * the environment.  The logic is simpler if we just duplicate/alter
 * the code in mca_base_param.c rather than try to make this a) public,
 * and b) more general (to accomodate looking up keyvals while not
 * linking to MPI_Comm_get_attr() in libmpi).
 */
static bool
param_lookup(int index, mca_base_param_storage_t *storage)
{
  size_t size;
  char *env;
  mca_base_param_t *p;

  /* Lookup the index and see if it's valid */

  if (!initialized)
    return false;
  if (lam_arr_get_size(&mca_base_params) < index)
    return false;
  p = ((mca_base_param_t*) lam_arr_get_c_array(&mca_base_params, 
                                               &size)) + index;

  /* We either don't have a keyval or didn't find it.  So look in the
     environment. */

  if (NULL != p->mbp_env_var_name &&
      NULL != (env = getenv(p->mbp_env_var_name))) {
    if (MCA_BASE_PARAM_TYPE_INT == p->mbp_type)
      storage->intval = atoi(env);
    else if (MCA_BASE_PARAM_TYPE_STRING == p->mbp_type)
      storage->stringval = strdup(env);
    else
      return false;

    return true;
  }

  /* Didn't find it; use the default value. */

  switch (p->mbp_type) {
  case MCA_BASE_PARAM_TYPE_INT:
    storage->intval = p->mbp_default_value.intval;
    break;

  case MCA_BASE_PARAM_TYPE_STRING:
    storage->stringval = p->mbp_default_value.stringval;
    break;

  default:
    return false;
  }

  /* All done */

  return true;
}


static int 
param_compare(const void *a, const void *b)
{
  const mca_base_param_t *aa = (const mca_base_param_t*) a;
  const mca_base_param_t *bb = (const mca_base_param_t*) b;

  return strcmp(aa->mbp_full_name, bb->mbp_full_name);
}


static void 
param_free(mca_base_param_t *p)
{
  if (NULL != p->mbp_type_name)
    LAM_FREE(p->mbp_type_name);
  if (NULL != p->mbp_module_name)
    LAM_FREE(p->mbp_module_name);
  if (NULL != p->mbp_param_name)
    LAM_FREE(p->mbp_param_name);
  if (NULL != p->mbp_env_var_name)
    LAM_FREE(p->mbp_env_var_name);
  if (NULL != p->mbp_full_name)
    LAM_FREE(p->mbp_full_name);
  if (MCA_BASE_PARAM_TYPE_STRING == p->mbp_type &&
      NULL != p->mbp_default_value.stringval)
    LAM_FREE(p->mbp_default_value.stringval);
}
