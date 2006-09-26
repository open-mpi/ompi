/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 *
 * MCA parameters can be defined in multiple different places.  As
 * such, parameters are \em resolved to find their value.  The order
 * of resolution is as follows:
 *
 * - An "override" location that is only available to be set via the
 *   mca_base_param API.
 * - If the parameter has an MPI attribute keyval associated with it,
 *   see if there is a value assigned that can be used.
 * - Look for an environment variable corresponding to the MCA
 *   parameter.
 * - See if a file contains the MCA parameter (MCA parameter files are
 *   read only once -- when the first time any mca_param_t function is
 *   invoked).
 * - If nothing else was found, use the parameter's default value.
 *
 * Note that there is a second header file (mca_base_param_internal.h)
 * that contains several internal type delcarations for the parameter
 * system.  The internal file is only used within the parameter system
 * itself; it should not be required by any other Open MPI entities.
 */

#ifndef OMPI_MCA_BASE_PARAM_H
#define OMPI_MCA_BASE_PARAM_H

#include "opal_config.h"

#include "opal/class/opal_value_array.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/mca.h"


/**
 * The types of MCA parameters.
 */
typedef enum {
    /** The parameter is of type integer. */
    MCA_BASE_PARAM_TYPE_INT,
    /** The parameter is of type string. */
    MCA_BASE_PARAM_TYPE_STRING,
    
    /** Maximum parameter type. */
    MCA_BASE_PARAM_TYPE_MAX
} mca_base_param_type_t;


/**
 * Struct for holding name/type info.  Used in mca_base_param_dump(),
 * below.
 */
struct mca_base_param_info_t {
    /** So that we can be in a list */
    opal_list_item_t super;

    /** Index of this parameter */
    int mbpp_index;
    /** Enum indicating the back-end type of the parameter */
    mca_base_param_type_t mbpp_type;

    /** String name of the type of this component */
    char *mbpp_type_name;
    /** String name of the component of the parameter */
    char *mbpp_component_name;
    /** String name of the parameter of the parameter */
    char *mbpp_param_name;
    /** Full, assembled parameter name */
    char *mbpp_full_name;

    /** Is this value changable? */
    bool mbpp_read_only;
    /** Help message associated with this parameter */
    char *mbpp_help_msg;
};
/**
 * Convenience typedef
 */
typedef struct mca_base_param_info_t mca_base_param_info_t;

/*
 * Global functions for MCA
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * Make a real object for the info
     */
    OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_base_param_info_t);

    /**
     * Initialize the MCA parameter system.
     *
     * @retval OPAL_SUCCESS
     *
     * This function initalizes the MCA parameter system.  It is
     * invoked internally (by mca_base_open()) and is only documented
     * here for completeness.
     */
    OPAL_DECLSPEC int mca_base_param_init(void);

    /**
     * Register an integer MCA parameter.
     *
     * @param component [in] Pointer to the componet for which the
     * parameter is being registered (string), or NULL.
     * @param param_name [in] The name of the parameter being
     * registered (string).
     * @param help_msg [in] A string describing the use and valid
     * values of the parameter (string).
     * @param internal [in] Indicates whether the parameter is internal
     * (i.e., not to be shown to users) or not (bool).
     * @param read_only [in] Indicates whether the parameter value can
     * ever change (bool).
     * @param default_value [in] The value that is used for this
     * parameter if the user does not supply one.
     * @param current_value [out] After registering the parameter, look
     * up its current value and return it unless current_value is
     * NULL.
     *
     * @retval OPAL_ERROR Upon failure to register the parameter.
     * @retval index Index value that can be used with
     * mca_base_param_lookup_int() to retrieve the value of the parameter.
     *
     * This function registers an integer MCA parameter and associates it
     * with a specific component.
     *
     * If the {component} pointer is not NULL, the type name and
     * component name are automatically prefixed to the parameter
     * name.  Otherwise, the {param_name} is used as the full
     * parameter name.
     *
     * The {help_msg} is a string of arbitrary length (verbose is
     * good!) for explaining what the parameter is for and what its
     * valid values are.  This message is used in help messages, such
     * as the output from the ompi_info executable.
     *
     * If {internal} is set to true, this parameter is now shown by
     * default in the output of ompi_info.  That is, this parameter is
     * considered internal to the Open MPI implementation and is not
     * supposed to be viewed / changed by the user.
     *
     * If {read_only} is true, then the registered {default_value}
     * will be the only value ever returned when this parameter is
     * looked up.  That is, command line, environment, and file
     * overrides will be ignored.  This is useful, for example, for
     * reporting information to the user (e.g., the version of the GM
     * library that was linked against).
     *
     * If the {current_value} is not NULL, when the registration is
     * complete, the parameter system will look up the current value
     * of the parameter and return it in {current_value}.
     */
    OPAL_DECLSPEC int mca_base_param_reg_int(const mca_base_component_t *component,
                                             const char *param_name, 
                                             const char *help_msg,
                                             bool internal,
                                             bool read_only,
                                             int default_value,
                                             int *current_value);

    /**
     * Register an integer MCA parameter that is not associated with a
     * component.
     *
     * @param type [in] Although this parameter is not associated with
     * a component, it still must have a string type name that will
     * act as a prefix (string).
     * @param param_name [in] The name of the parameter being
     * registered (string).
     * @param help_msg [in] A string describing the use and valid
     * values of the parameter (string).
     * @param internal [in] Indicates whether the parameter is internal
     * (i.e., not to be shown to users) or not (bool).
     * @param read_only [in] Indicates whether the parameter value can
     * ever change (bool).
     * @param default_value [in] The value that is used for this
     * parameter if the user does not supply one.
     * @param current_value [out] After registering the parameter, look
     * up its current value and return it unless current_value is
     * NULL.
     *
     * @retval OPAL_ERROR Upon failure to register the parameter.
     * @retval index Index value that can be used with
     * mca_base_param_lookup_string() to retrieve the value of the
     * parameter.
     *
     * This function is identical to mca_base_param_reg_int() except
     * that it registers parameters that are not associated with
     * components.  For example, it can be used to register parameters
     * associated with a framework base or an overall layer (e.g., the
     * MPI layer, or the MCA base system framework itself).  Typical
     * "type" strings are:
     *
     * "mca": for the MCA base framework itself
     * framework name: for any given framework
     * "mpi": for parameters that apply to the overall MPI layer
     * "orte": for parameters that apply to the overall ORTE layer
     * "btl": for parameters to the OMPI BTL framework
     * ...etc.
     *
     * Note that the type should always be a framework or a level name
     * (e.g., "btl" or "mpi") -- it should not include the component
     * name, even if the componet is the base of a framework.  Hence,
     * "btl_base" is not a valid type name.  Specifically, registering
     * a parameter with an unrecognized type is not an error, but
     * ompi_info has a hard-coded list of frameworks and levels;
     * parameters that have recongized types, although they can be
     * used by the user, will not be displayed by ompi_info.
     *
     * Note that if you use mca_base_param_find() to lookup the index
     * of the registered parameter, the "component" argument should be
     * NULL (because it is not specified in this registration
     * function, and is therefore registered with a NULL value).
     */
    OPAL_DECLSPEC int mca_base_param_reg_int_name(const char *type,
                                                  const char *param_name, 
                                                  const char *help_msg,
                                                  bool internal,
                                                  bool read_only,
                                                  int default_value,
                                                  int *current_value);
    
    /**
     * Register a string MCA parameter.
     *
     * @param component [in] Pointer to the componet for which the
     * parameter is being registered (string), or NULL.
     * @param param_name [in] The name of the parameter being
     * registered (string).
     * @param help_msg [in] A string describing the use and valid
     * values of the parameter (string).
     * @param internal [in] Indicates whether the parameter is internal
     * (i.e., not to be shown to users) or not (bool).
     * @param read_only [in] Indicates whether the parameter value can
     * ever change (bool).
     * @param default_value [in] The value that is used for this
     * parameter if the user does not supply one.
     * @param current_value [out] After registering the parameter, look
     * up its current value and return it unless current_value is
     * NULL.
     *
     * @retval OPAL_ERROR Upon failure to register the parameter.
     * @retval index Index value that can be used with
     * mca_base_param_lookup_string() to retrieve the value of the
     * parameter.
     *
     * Note that if a string value is read in from a file then it will
     * never be NULL. It will always have a value, even if that value is
     * the empty string.
     *
     * This function is identical to mca_base_param_reg_int() except
     * that you are registering a string parameter with an associated
     * string default value (which is \em not allowed to be NULL).
     * See mca_base_param_reg_int() for all other details.
     */
    OPAL_DECLSPEC int mca_base_param_reg_string(const mca_base_component_t *component,
                                                const char *param_name,
                                                const char *help_msg,
                                                bool internal,
                                                bool read_only,
                                                const char *default_value,
                                                char **current_value);


    /**
     * Register a string MCA parameter that is not associated with a
     * component.
     *
     * @param type [in] Although this parameter is not associated with
     * a component, it still must have a string type name that will
     * act as a prefix (string).
     * @param param_name [in] The name of the parameter being
     * registered (string).
     * @param help_msg [in] A string describing the use and valid
     * values of the parameter (string).
     * @param internal [in] Indicates whether the parameter is internal
     * (i.e., not to be shown to users) or not (bool).
     * @param read_only [in] Indicates whether the parameter value can
     * ever change (bool).
     * @param default_value [in] The value that is used for this
     * parameter if the user does not supply one.
     * @param current_value [out] After registering the parameter, look
     * up its current value and return it unless current_value is
     * NULL.
     *
     * @retval OPAL_ERROR Upon failure to register the parameter.
     * @retval index Index value that can be used with
     * mca_base_param_lookup_string() to retrieve the value of the
     * parameter.
     *
     * Note that if a string value is read in from a file then it will
     * never be NULL. It will always have a value, even if that value is
     * the empty string.
     *
     * This function is identical to mca_base_param_reg_string()
     * except that it registers parameters that are not associated
     * with components.  For example, it can be used to register
     * parameters associated with a framework base or an overall layer
     * (e.g., the MPI layer, or the MCA base system framework itself).
     * Typical "type" strings are:
     *
     * "mca": for the MCA base framework itself
     * framework name: for any given framework
     * "mpi": for parameters that apply to the overall MPI layer
     * "orte": for parameters that apply to the overall ORTE layer
     * "btl": for parameters to the OMPI BTL framework
     * ...etc.
     *
     * Note that the type should always be a framework or a level name
     * (e.g., "btl" or "mpi") -- it should not include the component
     * name, even if the componet is the base of a framework.  Hence,
     * "btl_base" is not a valid type name.  Specifically, registering
     * a parameter with an unrecognized type is not an error, but
     * ompi_info has a hard-coded list of frameworks and levels;
     * parameters that have recongized types, although they can be
     * used by the user, will not be displayed by ompi_info.
     *
     * Note that if you use mca_base_param_find() to lookup the index
     * of the registered parameter, the "component" argument should be
     * NULL (because it is not specified in this registration
     * function, and is therefore registered with a NULL value).
     */
    OPAL_DECLSPEC int mca_base_param_reg_string_name(const char *type,
                                                     const char *param_name,
                                                     const char *help_msg,
                                                     bool internal,
                                                     bool read_only,
                                                     const char *default_value,
                                                     char **current_value);

    /**
     * Associate a communicator/datatype/window keyval with an MCA
     * parameter.
     *
     * @param index The index of the parameter to use.
     * @param keyval The keyval to associate it with.
     *
     * @returns OPAL_SUCCESS Upon success.
     * @returns OPAL_ERROR If the index value is invalid.
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
    OPAL_DECLSPEC int mca_base_param_kv_associate(int index, int keyval);

    /**
     * Look up an integer MCA parameter.
     *
     * @param index Index previous returned from
     * mca_base_param_register_int().
     * @param value Pointer to int where the parameter value will be
     * stored.
     *
     * @return OPAL_ERROR Upon failure.  The contents of value are
     * undefined.
     * @return OPAL_SUCCESS Upon success.  value will be filled with the
     * parameter's current value.
     *
     * The value of a specific MCA parameter can be looked up using the
     * return value from mca_base_param_register_int().
     */
    OPAL_DECLSPEC int mca_base_param_lookup_int(int index, int *value);
    
    /**
     * Look up an integer MCA parameter, to include looking in
     * attributes.
     *
     * @param index Index previous returned from
     * mca_base_param_register_int().
     * @param attrs Object containing attributes to be searched.
     * @param value Pointer to int where the parameter value will
     * be stored.
     *
     * @return OPAL_ERROR Upon failure.  The contents of value are
     * undefined.
     * @return OPAL_SUCCESS Upon success.  value will be filled with the
     * parameter's current value.
     *
     * This function is identical to mca_base_param_lookup_int() except
     * that it looks in attributes \em first to find the parameter
     * value.  The function mca_base_param_kv_associate() must have been
     * called first to associate a keyval with the index.
     */
    OPAL_DECLSPEC int mca_base_param_kv_lookup_int(int index,
                                                   struct opal_hash_table_t *attrs, 
                                                   int *value);
    
    /**
     * Look up a string MCA parameter.
     *
     * @param index Index previous returned from
     * mca_base_param_register_string().
     * @param value Pointer to (char *) where the parameter value will be
     * stored.
     *
     * @return OPAL_ERROR Upon failure.  The contents of value are
     * undefined.
     * @return OPAL_SUCCESS Upon success.  value will be filled with the
     * parameter's current value.
     *
     * Note that if a string value is read in from a file then it will
     * never be NULL. It will always have a value, even if that value is
     * the empty string.
     * 
     * Strings returns in the \em value parameter should later be
     * free()'ed.
     *
     * The value of a specific MCA parameter can be looked up using the
     * return value from mca_base_param_register_string().
     */
    OPAL_DECLSPEC int mca_base_param_lookup_string(int index, char **value);

    /**
     * Look up a string MCA parameter, to include looking in attributes.
     *
     * @param index [in] Index previous returned from
     * mca_base_param_register_string().
     * @param attrs [in] Object containing attributes to be searched.
     * @param value [out] Pointer to (char *) where the parameter value
     * will be stored.
     *
     * @return OPAL_ERROR Upon failure.  The contents of value are
     * undefined.
     * @return OPAL_SUCCESS Upon success.  value will be filled with the
     * parameter's current value.
     *
     * This function is identical to mca_base_param_lookup_string()
     * except that it looks in attributes \em first to find the
     * parameter value.  The function mca_base_param_kv_associate() must
     * have been called first to associate a keyval with the index.
     */
    OPAL_DECLSPEC int mca_base_param_kv_lookup_string(int index, 
                                                      struct opal_hash_table_t *attrs, 
                                                      char **value);

    /**
     * Sets an "override" value for an integer MCA parameter.
     *
     * @param index [in] Index of MCA parameter to set
     * @param value [in] The integer value to set
     *
     * @retval OPAL_ERROR If the parameter was not found.
     * @retval OPAL_SUCCESS Upon success.
     *
     * This function sets an integer value on the MCA parmeter
     * indicated by the index value index.  This value will be used in
     * lieu of any other value from any other MCA source (environment
     * variable, file, etc.) until the value is unset with
     * mca_base_param_unset().
     *
     * This function may be invoked multiple times; each time, the
     * last "set" value is replaced with the newest value.
     */
    OPAL_DECLSPEC int mca_base_param_set_int(int index, int value);

    /**
     * Sets an "override" value for an string MCA parameter.
     *
     * @param index [in] Index of MCA parameter to set
     * @param value [in] The string value to set
     *
     * @retval OPAL_ERROR If the parameter was not found.
     * @retval OPAL_SUCCESS Upon success.
     *
     * This function sets a string value on the MCA parmeter
     * indicated by the index value index.  This value will be used in
     * lieu of any other value from any other MCA source (environment
     * variable, file, etc.) until the value is unset with
     * mca_base_param_unset().  
     *
     * The string is copied by value; the string "value" parameter
     * does not become "owned" by the parameter subsystem.
     *
     * This function may be invoked multiple times; each time, the
     * last "set" value is replaced with the newest value (the old
     * value is discarded).
     */
    OPAL_DECLSPEC int mca_base_param_set_string(int index, char *value);

    /**
     * Unset a parameter that was previously set by
     * mca_base_param_set_int() or mca_base_param_set_string().
     *
     * @param index [in] Index of MCA parameter to set
     *
     * @retval OPAL_ERROR If the parameter was not found.
     * @retval OPAL_SUCCESS Upon success.
     *
     * Resets previous value that was set (if any) on the given MCA
     * parameter.
     */
    OPAL_DECLSPEC int mca_base_param_unset(int index);

    /**
     * Get the string name corresponding to the MCA parameter
     * value in the environment.
     *
     * @param param_name Name of the type containing the parameter.
     *
     * @retval string A string suitable for setenv() or appending to
     * an environ-style string array.
     * @retval NULL Upon failure.
     *
     * The string that is returned is owned by the caller; if
     * appropriate, it must be eventually freed by the caller.
     */
    OPAL_DECLSPEC char *mca_base_param_env_var(const char *param_name);

    /**
     * Find the index for an MCA parameter based on its names.
     *
     * @param type Name of the type containing the parameter.
     * @param component Name of the component containing the parameter.
     * @param param Name of the parameter.
     *
     * @retval OPAL_ERROR If the parameter was not found.
     * @retval index If the parameter was found.
     *
     * It is not always convenient to widely propagate a parameter's index
     * value, or it may be necessary to look up the parameter from a
     * different component -- where it is not possible to have the return
     * value from mca_base_param_register_int() or
     * mca_base_param_register_string().  This function can be used to
     * look up the index of any registered parameter.  The returned index
     * can be used with mca_base_param_lookup_int() and
     * mca_base_param_lookup_string().
     */
    OPAL_DECLSPEC int mca_base_param_find(const char *type, 
                                          const char *component, 
                                          const char *param);

    /**
     * Set the "internal" flag on an MCA parameter to true or false.
     *
     * @param index [in] Index previous returned from
     * mca_base_param_register_string() or mca_base_param_register_int(). 
     * @param internal [in] Boolean indicating whether the MCA
     * parameter is internal (private) or public.
     *
     * @returns OPAL_SUCCESS If it can find the parameter to reset
     * @returns OPAL_ERROR Otherwise
     *
     * "Internal" MCA parameters are ones that are not intentended to
     * be seen or modified by users or user applications.  These
     * include values that are set at run time, such as TCP ports, IP
     * addresses, etc.  By setting the "internal" flag, internal MCA
     * parameters are not displayed during the output of ompi_info and
     * MPI_INIT (at least, they're not displayed by default), thus
     * keeping them away from prying user eyes.
     */
    OPAL_DECLSPEC int mca_base_param_set_internal(int index, bool internal);

    /**
     * Obtain a list of all the MCA parameters currently defined as
     * well as their types.  
     *
     * @param info [out] An opal_list_t of mca_base_param_info_t
     * instances.
     * @param internal [in] Whether to include the internal parameters
     * or not.
     *
     * @retval OPAL_SUCCESS Upon success.
     * @retval OPAL_ERROR Upon failure.
     *
     * This function is used to obtain a list of all the currently
     * registered MCA parameters along with their associated types
     * (currently: string or integer).  The results from this function
     * can be used to repeatedly invoke mca_base_param_lookup_int()
     * and/or mca_base_param_lookup_string() to obtain a comprehensive
     * list of all MCA parameters and their current values.
     *
     * Releasing the list, and all the items in the list, is a
     * relatively complicated process.  Use the companion function
     * mca_base_param_dump_release() when finished with the returned
     * info list to release all associated memory.
     */
    OPAL_DECLSPEC int mca_base_param_dump(opal_list_t **info, bool internal);

    /**
     * Obtain a list of all the MCA parameters currently defined as
     * well as their types.  
     *
     * @param env [out] A pointer to an argv-style array of key=value
     * strings, suitable for use in an environment
     * @param num_env [out] A pointer to an int, containing the length
     * of the env array (not including the final NULL entry).
     * @param internal [in] Whether to include the internal parameters
     * or not.
     *
     * @retval OPAL_SUCCESS Upon success.
     * @retval OPAL_ERROR Upon failure.
     *
     * This function is similar to mca_base_param_dump() except that
     * its output is in terms of an argv-style array of key=value
     * strings, suitable for using in an environment.
     */
    OPAL_DECLSPEC int mca_base_param_build_env(char ***env, int *num_env,
                                               bool internal);

    /**
     * Release the memory associated with the info list returned from
     * mca_base_param_dump().
     *
     * @param info [in/out] An opal_list_t previously returned from
     * mca_base_param_dump().
     *
     * @retval OPAL_SUCCESS Upon success.
     * @retval OPAL_ERROR Upon failure.
     * 
     * This function is intended to be used to free the info list
     * returned from mca_base_param_dump().  There are a bunch of
     * strings and other associated memory in the list making it
     * cumbersome for the caller to free it all properly.  Hence, once
     * the caller is finished with the info list, invoke this
     * function and all memory associated with the list will be freed.
     */
    OPAL_DECLSPEC int mca_base_param_dump_release(opal_list_t *info);

    /**
     * Shut down the MCA parameter system (normally only invoked by the
     * MCA framework itself).
     *
     * @returns OPAL_SUCCESS This function never fails.
     *
     * This function shuts down the MCA parameter repository and frees all
     * associated memory.  No other mca_base_param*() functions can be
     * invoked after this function.
     *
     * This function is normally only invoked by the MCA framework itself
     * when the process is shutting down (e.g., during MPI_FINALIZE).  It
     * is only documented here for completeness.
     */
    OPAL_DECLSPEC int mca_base_param_finalize(void);

    /***************************************************************
     * Deprecated interface
     ***************************************************************/

    /**
     * \deprecated
     *
     * Register an integer MCA parameter (deprecated).
     *
     * @param type_name [in] The MCA type (string).
     * @param component_name [in] The name of the component (string).
     * @param param_name [in] The name of the parameter being registered
     * (string).
     * @param mca_param_name [in] Optional parameter to override the
     * user-visible name of this parameter (string).
     * @param default_value [in] The value that is used for this
     * parameter if the user does not supply one.
     *
     * @retval OPAL_ERROR Upon failure to register the parameter.
     * @retval index Index value that can be used with
     * mca_base_param_lookup_int() to retrieve the value of the parameter.
     *
     * This function is deprecated.  Use mca_base_param_reg_int() instead.
     *
     * This function registers an integer MCA parameter and associates it
     * with a specific component.
     *
     * The default resulting MCA parameter name is
     * {type_name}[_{component_name}][_{param_name}].
     *
     * {component_name} is only included if it is non-NULL.  All
     * components an should include their name; component frameworks
     * should pass "base".  It is only permissible for the MCA base
     * itself to pass NULL for the component_name.
     *
     * Likewise, {param_name} is also only included if it is non-NULL.
     * Components and frameworks can pass NULL for this parameter if
     * they wish.
     *
     * In most cases, mca_param_name should be NULL, in which case the
     * user-visible name of this parameter will be the default form (as
     * described above).  Only in rare cases is it necessary (or
     * advisable) to override the default name -- its use is strongly
     * discouraged.
     *
     * It is permissable to register a (type_name, component_name,
     * param_name) triple more than once; the same index value will be
     * returned, but the default value will be changed to reflect the
     * last registration.
     */
    OPAL_DECLSPEC int mca_base_param_register_int(const char *type_name, 
                                                  const char *component_name,
                                                  const char *param_name, 
                                                  const char *mca_param_name,
                                                  int default_value);
    
    /**
     * \deprecated
     *
     * Register a string MCA parameter (deprecated).
     *
     * @param type_name [in] The MCA type (string).
     * @param component_name [in] The name of the component (string).
     * @param param_name [in] The name of the parameter being registered
     * (string).
     * @param mca_param_name [in] Optional parameter to override the
     * user-visible name of this parameter (string).
     * @param default_value [in] The value that is used for this
     * parameter if the user does not supply one.
     *
     * @retval OPAL_ERROR Upon failure to register the parameter.
     * @retval index Index value that can be used with
     * mca_base_param_lookup_string() to retrieve the value of the
     * parameter.
     *
     * This function is deprecated.  Use mca_base_param_reg_string()
     * instead.
     *
     * Note that if a string value is read in from a file then it will
     * never be NULL. It will always have a value, even if that value is
     * the empty string.
     *
     * This function is identical to mca_base_param_register_int()
     * except that you are registering a string parameter with an
     * associated string default value (which is \em not allowed to be NULL).
     * See mca_base_param_register_int() for all other details.
     */
    OPAL_DECLSPEC int mca_base_param_register_string(const char *type_name, 
                                                     const char *component_name,
                                                     const char *param_name, 
                                                     const char *mca_param_name,
                                                     const char *default_value);

    /**
     * \deprecated
     *
     * Get the string name corresponding to the MCA parameter
     * value in the environment (deprecated).
     *
     * @param type Name of the type containing the parameter.
     * @param comp Name of the component containing the parameter.
     * @param param Name of the parameter.
     *
     * @retval string A string suitable for setenv() or appending to
     * an environ-style string array.
     * @retval NULL Upon failure.
     *
     * This function is deprecated.  Use mca_base_param_env_var()
     * instead.
     *
     * The string that is returned is owned by the caller; if
     * appropriate, it must be eventually freed by the caller.
     */
    OPAL_DECLSPEC char *mca_base_param_environ_variable(const char *type,
                                                        const char *comp,
                                                        const char *param);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_MCA_BASE_PARAM_H */
