/*
 * $HEADER$
 */

/** @file This is the private declarations for the MCA parameter
 * system.  This file is internal to the MCA parameter system and
 * should not need to be used by any other elements in Open MPI except
 * the special case of the ompi_info command.
 */

#ifndef OMPI_MCA_BASE_PARAM_INTERNAL_H
#define OMPI_MCA_BASE_PARAM_INTERNAL_H

#include "mpi.h"

#include "class/ompi_object.h"
#include "class/ompi_hash_table.h"

/**
 * Types for MCA parameters.
 */
typedef union {
  int intval;
  /**< Integer value */
  char *stringval;
  /**< String value */
} mca_base_param_storage_t;


/**
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
 * Entry for holding the information about an MCA parameter and its
 * default value.
 */
struct mca_base_param_t {
  ompi_object_t mbp_super;
  /**< Allow this to be an OMPI OBJ */

  mca_base_param_type_t mbp_type;
  /**< Enum indicating the type of the parameter (integer or string) */
  char *mbp_type_name;
  /**< String of the type name, or NULL */
  char *mbp_component_name;
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
 * Convenience typedef.
 */
typedef struct mca_base_param_t mca_base_param_t;

/**
 * Object delcataion for mca_base_param_t
 */
OBJ_CLASS_DECLARATION(mca_base_param_t);


#endif /* OMPI_MCA_BASE_PARAM_INTERNAL_H */
