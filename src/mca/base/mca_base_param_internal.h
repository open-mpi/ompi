/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** 
 * @file 
 *
 * This is the private declarations for the MCA parameter system.
 * This file is internal to the MCA parameter system and should not
 * need to be used by any other elements in Open MPI except the
 * special case of the ompi_info command.
 *
 * All the rest of the doxygen documentation in this file is marked as
 * "internal" and won't show up unless you specifically tell doxygen
 * to generate internal documentation (by default, it is skipped).
 */

#ifndef OMPI_MCA_BASE_PARAM_INTERNAL_H
#define OMPI_MCA_BASE_PARAM_INTERNAL_H

#include "mpi.h"

#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "class/ompi_hash_table.h"
#include "mca/base/mca_base_param.h"

/**
 * \internal
 *
 * Types for MCA parameters.
 */
typedef union {
    /** Integer value */
    int intval;
    /** String value */
    char *stringval;
} mca_base_param_storage_t;


/**
 * \internal
 *
 * Entry for holding the information about an MCA parameter and its
 * default value.
 */
struct mca_base_param_t {
    /** Allow this to be an OMPI OBJ */
    ompi_object_t mbp_super;

    /** Enum indicating the type of the parameter (integer or string) */
    mca_base_param_type_t mbp_type;
    /** String of the type name, or NULL */
    char *mbp_type_name;
    /** String of the component name */
    char *mbp_component_name;
    /** String of the parameter name */
    char *mbp_param_name;
    /** Full parameter name, in case it is not
       <type>_<component>_<param> */
    char *mbp_full_name;
    
    /** Whether this is internal (not meant to be seen / modified by
        users) or not */
    bool mbp_internal;

    /** Keyval value for MPI attribute parameters */
    int mbp_keyval;
    /** Environment variable name */
    char *mbp_env_var_name;

    /** Default value of the parameter */
    mca_base_param_storage_t mbp_default_value;

    /** Whether or not we have a file value */
    bool mbp_file_value_set;
    /** Value of the parameter found in a file */
    mca_base_param_storage_t mbp_file_value;

    /** Whether or not we have an override value */
    bool mbp_override_value_set;
    /** Value of the parameter override set via API */
    mca_base_param_storage_t mbp_override_value;
};
/**
 * \internal
 *
 * Convenience typedef.
 */
typedef struct mca_base_param_t mca_base_param_t;

/**
 * \internal
 *
 * Object delcataion for mca_base_param_t
 */
OBJ_CLASS_DECLARATION(mca_base_param_t);


/**
 * \internal
 *
 * Structure for holding param names and values read in from files.
 */
struct mca_base_param_file_value_t {
    /** Allow this to be an OMPI OBJ */
    ompi_list_item_t super;
    
    /** Parameter name */
    char *mbpfv_param;
    /** Parameter value */
    char *mbpfv_value;
};
/**
 * \internal
 *
 * Convenience typedef
 */
typedef struct mca_base_param_file_value_t mca_base_param_file_value_t;

/**
 * Object declaration for mca_base_param_file_value_t
 */
OBJ_CLASS_DECLARATION(mca_base_param_file_value_t);


/**
 * \internal
 *
 * Global list of params and values read in from MCA parameter files
 */
OMPI_DECLSPEC extern ompi_list_t mca_base_param_file_values;


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /**
     * \internal
     *
     * Parse a parameter file.
     */
  int mca_base_parse_paramfile(const char *paramfile);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
    
#endif /* OMPI_MCA_BASE_PARAM_INTERNAL_H */
