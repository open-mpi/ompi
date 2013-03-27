/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights
 *                         reserved.
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

#ifndef OPAL_MCA_BASE_PARAM_INTERNAL_H
#define OPAL_MCA_BASE_PARAM_INTERNAL_H

#include "opal_config.h"

#include "opal/class/opal_object.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_value_array.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/base/mca_base_var.h"

BEGIN_C_DECLS

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
    /** Allow this to be an OPAL OBJ */
    opal_object_t super;

    /* Backing store for the variable value */
    mca_base_param_storage_t *param_value;

    /* For debugging purposes */
    int var_index;
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
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_base_param_t);


/**
 * \internal
 *
 * Global list of params and values read in from MCA parameter files
 */
OPAL_DECLSPEC extern opal_list_t mca_base_param_file_values;

/**
 * \internal
 *
 * Parse a parameter file.
 */
OPAL_DECLSPEC int mca_base_parse_paramfile(const char *paramfile, opal_list_t *list);

END_C_DECLS
    
#endif /* OPAL_MCA_BASE_PARAM_INTERNAL_H */
