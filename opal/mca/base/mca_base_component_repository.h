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
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BASE_COMPONENT_REPOSITORY_H
#define MCA_BASE_COMPONENT_REPOSITORY_H

#include "opal_config.h"

#include "opal/mca/dl/dl.h"
#include "opal/mca/dl/base/base.h"

BEGIN_C_DECLS

    OPAL_DECLSPEC int mca_base_component_repository_init(void);

/* This file provide the external interface to our base component
 * module.  Most of the components that depend on it, will use the
 * retain_component() function to increase the reference count on a
 * particular component (as opposed to the retain() function, which is
 * internal to the opal/mca/base).  But it's convenient to have all
 * the functions exported from one header file rather than to separate
 * retain_component() and retain() into two separate header files
 * (i.e., have a separate header file just for retain()).
 */
    OPAL_DECLSPEC int mca_base_component_repository_retain(char *type, 
                              opal_dl_handle_t *component_handle,
                              const mca_base_component_t *component_struct);

    OPAL_DECLSPEC int mca_base_component_repository_retain_component(const char *type, 
                              const char *name);
    OPAL_DECLSPEC int mca_base_component_repository_link(const char *src_type, 
                              const char *src_name,
                              const char *depend_type,
                              const char *depend_name);
    OPAL_DECLSPEC void mca_base_component_repository_release(const mca_base_component_t *component);
    OPAL_DECLSPEC void mca_base_component_repository_finalize(void);
    
END_C_DECLS

#endif /* MCA_BASE_COMPONENT_REPOSITORY_H */
