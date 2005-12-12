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

#ifndef MCA_BASE_COMPONENT_REPOSITORY_H
#define MCA_BASE_COMPONENT_REPOSITORY_H

#include "ompi_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    OMPI_DECLSPEC int mca_base_component_repository_init(void);

/* This file provide the external interface to our base component module.
 * Most of the components that depend on it, will use the retain_component
 * function to increase the reference count on a particular component. But
 * it's convenient to have all the functions exported from one source file
 * in the same header. That's why we keep the retain function here. However,
 * because this function require the definition of the lt_dlhandle type,
 * therefore it will be included only when the ltdl.h has been previously
 * included in the headers list.
 */
#if defined(LTDL_H)
    OMPI_DECLSPEC int mca_base_component_repository_retain(char *type, 
                              lt_dlhandle component_handle, 
                              const mca_base_component_t *component_struct);
#endif

    OMPI_DECLSPEC int mca_base_component_repository_retain_component(const char *type, 
                              const char *name);
    OMPI_DECLSPEC int mca_base_component_repository_link(const char *src_type, 
                              const char *src_name,
                              const char *depend_type,
                              const char *depend_name);
    OMPI_DECLSPEC void mca_base_component_repository_release(const mca_base_component_t *component);
    OMPI_DECLSPEC void mca_base_component_repository_finalize(void);
    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_BASE_COMPONENT_REPOSITORY_H */
