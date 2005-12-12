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

/* This file provide the external interface to our base component module.
 * At this level we don't need a proper definition for the lt_dlhandle,
 * just a void* is enought. Except for all file that really require the
 * header ltdl.h because they use functions declared inside. As the ltdl.h
 * header file define LTDL_H when it get included, we can use it to detect
 * if we have to furnish our fake definition for the lt_dlhandle. However,
 * all files needing the ltdl.h header have to includ it before including
 * this one.
 */
#if !defined(LTDL_H)
typedef void* lt_dlhandle;
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    OMPI_DECLSPEC int mca_base_component_repository_init(void);
    OMPI_DECLSPEC int mca_base_component_repository_retain(char *type, 
                              lt_dlhandle component_handle, 
                              const mca_base_component_t *component_struct);
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
