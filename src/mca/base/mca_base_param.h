/*
 * $HEADER$
 */

/** @file 
 * This file presents the MCA parameter interface.
 *
 * Note that there are two scopes for MCA parameters: "normal" and
 * attributes.  Specifically, all MCA parameters are "normal" -- some
 * are special and may also be found on attributes on communicators,
 * datatypes, or windows.
 *
 * In general, these functions are intended to be used as follows:
 *
 * - Creating MCA parameters
 * -# Register a parameter, get an index back
 * -# Optionally associate that index with an attribute keyval
 * - Using MCA parameters
 * -# Lookup a "normal" parameter value on a specific index, or
 * -# Lookup an attribute parameter on a specific index and
 *    communicator / datatype / window.
 */

#ifndef OMPI_MCA_BASE_PARAM_H
#define OMPI_MCA_BASE_PARAM_H

#include "mpi.h"

#include "class/ompi_hash_table.h"

/**
 * \internal
 *
 * Types for MCA parameters.
 */
typedef union {
  int intval;
  /**< Integer value */
  char *stringval;
  /**< String value */
} mca_base_param_storage_t;

/** \internal
 *
 * Special name used to indicate that this is an "info" value.
 */
#define MCA_BASE_PARAM_INFO ((void*) -1)


/**
 * \internal
 *
 * The following types are really in this public .h file so that
 * ompi_info can see them.  No one else should use them!
 */
typedef enum {
  MCA_BASE_PARAM_TYPE_INT,
  /**< The parameter is of type integer. */
  MCA_BASE_PARAM_TYPE_STRING,
  /**< The parameter is of type string. */

  MCA_BASE_PARAM_TYPE_MAX
  /**< Maximum parameter type. */
} mca_base_param_type_t;

/**
 * \internal
 *
 * Entry for holding the information about an MCA parameter and its
 * default value.
 */
struct mca_base_param_t {
  mca_base_param_type_t mbp_type;
  /**< Enum indicating the type of the parameter (integer or string) */
  char *mbp_type_name;
  /**< String of the type name, or NULL */
  char *mbp_module_name;
  /**< String of the component name */
  char *mbp_param_name;
  /**< String of the parameter name */
  char *mbp_full_name;
  /**< Full parameter name, in case it is not
     <type>_<component>_<param> */

  int mbp_keyval;
  /**< Keyval value for MPI attribute parameters */
  char *mbp_env_var_name;
  /**< Environment variable name */

  mca_base_param_storage_t mbp_default_value;
  /**< Default value of the parameter */
};
/**
 * \internal
 *
 * Convenience typedef.
 */
typedef struct mca_base_param_t mca_base_param_t;


/*
 * Global functions for MCA
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  /**
   * Register an integer MCA parameter.
   *
   * @param type_name[in] The MCA type (string).
   * @param module_name[in] The name of the module (string).
   * @param param_name[in] The name of the parameter being registered
   * (string).
   * @param mca_param_name[in] If NULL, the user-visible name of the
   * parameter is {type_name}_{module_name}_{param_name}.  If this
   * parameter is non-NULL, it is used instead of the default name.
   * @param default_value[in] The value that is used for this
   * parameter if the user does not supply one.
   *
   * @retval OMPI_ERROR Upon failure to register the parameter.
   * @retval index Index value that can be used with
   * mca_base_param_lookup_int() to retrieve the value of the parameter.
   *
   * This function registers an integer MCA parameter and associates it
   * with a specific module.
   *
   * In most cases, mca_param_name should be NULL.  Only in rare cases
   * is it necessary (or advisable) to override the default name.
   */
  int mca_base_param_register_int(const char *type_name, 
                                  const char *module_name,
                                  const char *param_name, 
                                  const char *mca_param_name,
                                  int default_value);

  /**
   * Register a string MCA parameter.
   *
   * @param type_name[in] The MCA type (string).
   * @param module_name[in] The name of the module (string).
   * @param param_name[in] The name of the parameter being registered
   * (string).
   * @param mca_param_name[in] If NULL, the user-visible name of the
   * parameter is {type_name}_{module_name}_{param_name}.  If this
   * parameter is non-NULL, it is used instead of the default name.
   * @param default_value[in] The value that is used for this
   * parameter if the user does not supply one.
   *
   * @retval OMPI_ERROR Upon failure to register the parameter.
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
  int mca_base_param_register_string(const char *type_name, 
                                     const char *module_name,
                                     const char *param_name, 
                                     const char *mca_param_name,
                                     const char *default_value);

  /**
   * Associate a communicator/datatype/window keyval with an MCA
   * parameter.
   *
   * @param index The index of the parameter to use.
   * @param keyval The keyval to associate it with.
   *
   * @returns OMPI_SUCCESS Upon success.
   * @returns OMPI_ERROR If the index value is invalid.
   *
   * For an index value that was previously returned by
   * mca_base_param_register_int() or
   * mca_base_param_register_string(), the corresponding MCA parameter
   * can be associated with a communicator, datatype, or window
   * attribute keyval.  
   *
   * After using this function, you can use any of the four lookup
   * functions (mca_base_param_lookup_int(),
   * mca_base_param_lookup_string(), mca_base_param_kv_lookup_int(),
   * and mca_base_param_kv_lookup_string()), but only the "kv"
   * versions will cross reference and attempt to find parameter
   * values on attributes.
   */
  int mca_base_param_kv_associate(int index, int keyval);

  /**
   * Look up an integer MCA parameter.
   *
   * @param index Index previous returned from
   * mca_base_param_register_int().
   * @param value Pointer to int where the parameter value will be
   * stored.
   *
   * @retvalue OMPI_ERROR Upon failure.  The contents of value are
   * undefined.
   * @retvalue OMPI_SUCCESS Upon success.  value will be filled with the
   * parameter's current value.
   *
   * The value of a specific MCA parameter can be looked up using the
   * return value from mca_base_param_register_int().
   */
  int mca_base_param_lookup_int(int index, int *value);

  /**
   * Look up an integer MCA parameter, to include looking in
   * attributes.
   *
   * @param index Index previous returned from
   * mca_base_param_register_int().
   * @param attr Object containing attributes to be searched.
   * @param value Pointer to int where the parameter value will
   * be stored.
   *
   * @retvalue OMPI_ERROR Upon failure.  The contents of value are
   * undefined.
   * @retvalue OMPI_SUCCESS Upon success.  value will be filled with the
   * parameter's current value.
   *
   * This function is identical to mca_base_param_lookup_int() except
   * that it looks in attributes \em first to find the parameter
   * value.  The function mca_base_param_kv_associate() must have been
   * called first to associate a keyval with the index.
   */
  int mca_base_param_kv_lookup_int(int index, struct ompi_hash_table_t *attrs, 
                                   int *value);

  /**
   * Look up a string MCA parameter.
   *
   * @param index Index previous returned from
   * mca_base_param_register_string().
   * @param value Pointer to (char *) where the parameter value will be
   * stored.
   *
   * @retvalue OMPI_ERROR Upon failure.  The contents of value are
   * undefined.
   * @retvalue OMPI_SUCCESS Upon success.  value will be filled with the
   * parameter's current value.
   *
   * The value of a specific MCA parameter can be looked up using the
   * return value from mca_base_param_register_string().
   */
  int mca_base_param_lookup_string(int index, char **value);

  /**
   * Look up a string MCA parameter, to include looking in attributes.
   *
   * @param index[in] Index previous returned from
   * mca_base_param_register_string().
   * @param attr[in] Object containing attributes to be searched.
   * @param value[out] Pointer to (char *) where the parameter value
   * will be stored.
   *
   * @retvalue OMPI_ERROR Upon failure.  The contents of value are
   * undefined.
   * @retvalue OMPI_SUCCESS Upon success.  value will be filled with the
   * parameter's current value.
   *
   * This function is identical to mca_base_param_lookup_string()
   * except that it looks in attributes \em first to find the
   * parameter value.  The function mca_base_param_kv_associate() must
   * have been called first to associate a keyval with the index.
   */
  int mca_base_param_kv_lookup_string(int index, 
                                      struct ompi_hash_table_t *attrs, 
                                      char **value);

  /**
   * Find the index for an MCA parameter based on its names.
   *
   * @param type_name Name of the type containing the parameter.
   * @param module_name Name of the module containing the parameter.
   * @param param_name Name of the parameter.
   *
   * @retval OMPI_ERROR If the parameter was not found.
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
  int mca_base_param_find(const char *type, const char *module, 
                          const char *param);

  /**
   * Shut down the MCA parameter system (normally only invoked by the
   * MCA framework itself).
   *
   * @returns OMPI_SUCCESS This function never fails.
   *
   * This function shuts down the MCA parameter repository and frees all
   * associated memory.  No other mca_base_param*() functions can be
   * invoked after this function.
   *
   * This function is normally only invoked by the MCA framework itself
   * when the process is shutting down (e.g., during MPI_FINALIZE).  It
   * is only documented here for completeness.
   */
  int mca_base_param_finalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_MCA_BASE_PARAM_H */
