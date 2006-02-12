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
/** 
 * @file 
 *
 * Top-level interface for \em all MCA components.
 */

#ifndef OMPI_MCA_H
#define OMPI_MCA_H

#include "opal_config.h"

#include "opal/class/opal_list.h"
#include "opal/util/cmd_line.h"

/**
 * MCA component open function.
 *
 * @retval MCA_SUCCESS This component can be used in the process. 
 *
 * @retval anything_else The MCA will ignore this component for the
 * duration of the process.
 *
 * All MCA components can have an "open" function that is invoked once
 * per process, when the component is located and loaded.  This function
 * should register any MCA parameters (using
 * mca_base_param_register_int() and mca_base_param_register_string())
 * that will be used by the component.  Parameter registrations should
 * occur in this function because the ompi_info command can be used by
 * users to display all available MCA parameters (and their default
 * values).  However, the ompi_info command \em only invokes this open
 * function on all components (i.e., no other component API methods).
 *
 * This function can also be used to allocate any resources necessary
 * for the component (e.g., heap memory).
 *
 * This function should return MCA_SUCCESS if it wishes to remain
 * loaded in the process.  Any other return value will cause the MCA
 * base to unload the component.  Although most components do not use
 * this mechanism to force themselves to be unloaded (because if they
 * are immediately unloaded, ompi_info will not display them), the
 * mechanism is available should the need arise.
 *
 * If the component a) has no MCA parameters to register, b) no
 * resources to allocate, and c) can always be used in a process
 * (albiet perhaps not selected), it may provide NULL for this
 * function.  In this cause, the MCA will act as if it called the open
 * function and it returned MCA_SUCCESS.
 */
typedef int (*mca_base_open_component_fn_t)(void);

/** MCA component close function
 *
 * @retval MCA_SUCCESS The component successfully shut down.
 *
 * @retval any_other_value Some error occurred, but is likely to be
 * ignored.
 *
 * This function is invoked on a component after all of its modules
 * have been finalized (according to the rules of its framework) and
 * the component will never be used in the process again; the
 * component may be unloaded from the process memory after the close
 * function has been invoked.
 *
 * This function is typically used to release any resources still in
 * use by the component.
 *
 * If the component has no resources to free, it may provide NULL for
 * this function.  In this case, the MCA will act as if it called the
 * close function and it returned MCA_SUCCESS.
 */
typedef int (*mca_base_close_component_fn_t)(void);


/**
 * Maximum length of MCA framework string names.
 */
#define MCA_BASE_MAX_TYPE_NAME_LEN 32
/**
 * Maximum length of MCA component string names.
 */
#define MCA_BASE_MAX_COMPONENT_NAME_LEN 64

/**
 * Common type for all MCA components.
 *
 * An instance of this type is always the first element in MCA
 * components, allowing the component to be associated with a
 * particular version of a specific framework, and to publish its own
 * name and version.
 */
struct mca_base_component_t {

  int mca_major_version; 
  /**< Major number of the MCA. */
  int mca_minor_version;
  /**< Minor number of the MCA. */
  int mca_release_version;
  /**< Release number of the MCA. */

  char mca_type_name[MCA_BASE_MAX_TYPE_NAME_LEN];
  /**< String name of the framework that this component belongs to. */
  int mca_type_major_version;
  /**< Major version number of the framework that this component
     belongs to. */
  int mca_type_minor_version;
  /**< Minor version number of the framework that this component
     belongs to. */
  int mca_type_release_version;
  /**< Release version number of the framework that this component
     belongs to. */

  char mca_component_name[MCA_BASE_MAX_COMPONENT_NAME_LEN];
  /**< This comopnent's string name. */
  int mca_component_major_version;
  /**< This component's major version number. */
  int mca_component_minor_version;
  /**< This component's minor version number. */
  int mca_component_release_version;
  /**< This component's release version number. */
  
  mca_base_open_component_fn_t mca_open_component;
  /**< Method for opening this component. */
  mca_base_close_component_fn_t mca_close_component;
  /**< Method for closing this component. */
};
/**
 * Convenience typedef.
 */
typedef struct mca_base_component_t mca_base_component_t;

/**
 * Meta data for MCA v1.0.0 components.
 */
struct mca_base_component_data_1_0_0_t {
  bool mca_is_checkpointable;
  /**< Indicates whether this component is checkpointable or not. */
};
/**
 * Convenience typedef.
 */
typedef struct mca_base_component_data_1_0_0_t mca_base_component_data_1_0_0_t;

/**
 * Macro for framework author convenience.  
 *
 * This macro is used by frameworks defining their component types,
 * indicating that they subscribe to the MCA version 1.0.0.  See
 * component header files (e.g., coll.h) for examples of its usage.
 */
#define MCA_BASE_VERSION_1_0_0 1, 0, 0


/**
 * MCA return codes.
 */
enum {
  MCA_SUCCESS = 0,
  /**< Success. */
  MCA_ERROR = -1,
  /**< General error. */
  MCA_ERR_OUT_OF_RESOURCE = -2,
  /**< Out of resources; a fatal error. */
  MCA_ERR_TEMP_OUT_OF_RESOURCE = -3,
  /**< Out of resources; try again later. */
  MCA_ERR_BAD_PARAM = -5,
  /**< Equivalent to MPI_ERR_ARG error code. */
  MCA_ERR_NOT_IMPLEMENTED = -10,
  /**< Returned by functions or functionality that has not yet been
     implemented */
  MCA_ERR_NOT_SUPPORTED = -11,
  /**< Returned by functionality that is not supported. */

  MCA_MAX_ERROR = -20
  /**< Maximum error code. */
};

#endif /* OMPI_MCA_H */
