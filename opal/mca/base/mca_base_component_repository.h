/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#if OMPI_WANT_LIBLTDL
/* Ensure to get the right <ltdl.h> -- a -I should be setup in this
   directory's Makefile.am to get opal/libltdl */ 
#include "ltdl.h"
#else
/* if we don't have libltdl, provide dummy handle type */
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
