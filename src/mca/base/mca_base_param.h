/*
 * $HEADER$
 */

/** @file **/

#ifndef LAM_MCA_BASE_PARAM_H
#define LAM_MCA_BASE_PARAM_H

#include "mpi.h"

/*
 * Types for MCA parameters
 */

typedef union {
  int intval;
  char *stringval;
} mca_base_param_storage_t;

#define MCA_BASE_PARAM_INFO ((void*) -1)


/*
 * The following types are really in this public .h file so that
 * laminfo can see them.  No one else should use them!
 */
typedef enum {
  MCA_BASE_PARAM_TYPE_INT,
  MCA_BASE_PARAM_TYPE_STRING,

  MCA_BASE_PARAM_TYPE_MAX
} mca_base_param_type_t;

struct mca_base_param_t {
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
 * Global functions for MCA
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
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
  int mca_base_param_register_int(const char *type_name, 
                                  const char *module_name,
                                  const char *param_name, 
                                  const char *mca_param_name,
                                  int default_value);
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
  int mca_base_param_register_string(const char *type_name, 
                                     const char *module_name,
                                     const char *param_name, 
                                     const char *mca_param_name,
                                     const char *default_value);
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
  int mca_base_param_lookup_int(int index, int *value);
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
  int mca_base_param_lookup_string(int index, char **value);
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
  int mca_base_param_find(const char *type, const char *module, 
                          const char *param);
  /**
   * Shut down the MCA parameter system (normally only invoked by the
   * MCA framework itself).
   *
   * @returns LAM_SUCCESS This function never fails.
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

#if 0
  /* JMS these are currently unimplemented */
  int mca_base_param_kv_associate(int index, int keyval);
  int mca_base_param_kv_lookup_int(int index, MPI_Comm comm);
  char *mca_base_param_kv_lookup_string(int index, MPI_Comm comm);
#endif
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_MCA_BASE_PARAM_H */
