/*
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/constants.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/runtime/opal.h"

static int installdirs_runtime_open(void);

opal_installdirs_base_component_t mca_installdirs_runtime_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {OPAL_INSTALLDIRS_BASE_VERSION_2_0_0,

     /* Component name and version */
     "runtime", OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION, OPAL_RELEASE_VERSION,

     /* Component open and close functions */
     installdirs_runtime_open, NULL},
    {/* This component is checkpointable */
     MCA_BASE_METADATA_PARAM_CHECKPOINT},

    /* Next the opal_install_dirs_t install_dirs_data information */
    {
        NULL,
    },
};

#include <dlfcn.h>
#include "opal/util/basename.h"
#include "opal/mca/installdirs/install_dirs.h"

/**
 * We are trying to solve a particular use case here, when the entire install tree
 * of Open MPI (and its dependencies) has been moved into another location. Nothing
 * fancy, the entire install tree has maintained his shape but changed the prefix.
 */
static int installdirs_runtime_open(void)
{
    Dl_info info;
    void* opal_fct;
    char* libname = NULL;
    const char* base_prefix_path = OPAL_LIBDIR;

    /* Casting from void* to fct pointer according to POSIX.1-2001 and POSIX.1-2008 */
    *(void **)&opal_fct = dlsym(RTLD_DEFAULT, "opal_init_util");

    if( 0 == dladdr(opal_fct, &info) ) {
        goto bail_out_with_no_data;
    }

    /* If this build was both static and shared then this compoenent will be build and will exists
     * even in the static library. We need to prevent setting a prefix for the OMPI library that
     * is actually the application path. Check, if the name points to a library.
     */
    libname = opal_basename(info.dli_fname);
    if( strncmp(libname, "lib", 3)) {  /* not a shared library */
        base_prefix_path = OPAL_BINDIR;
    } else {
#if defined(OPAL_LIB_NAME)
        /* Extra check using the installed name of the OPAL library */
        if( strncmp(libname+3, OPAL_LIB_NAME, strlen(OPAL_LIB_NAME)) ) {  /* not a shared library */
            goto bail_out_with_no_data;
        }
#endif  /* defined(OPAL_LIB_NAME) */
    }
    /* Remove the shared library name and it's first dirname to obtain a prefix. This
     * is true in most cases, especially when the install directory was just moved
     * moved around, but it is not necessarily always true.
     */
    char *prefix = NULL, *dname = opal_dirname(info.dli_fname);
    int dname_idx = strlen(dname), dname_token = dname_idx;
    for( int i = strlen(base_prefix_path); (i > 0) && (dname_idx > 0); i-- ) {
        if( dname[dname_idx] != base_prefix_path[i] ) {
            dname[dname_token] = '\0';
            prefix = dname;
            dname = NULL;  /* the string is now attached to the component, prevent it from being freed */
            break;
        }
        if( dname[dname_idx] == OPAL_PATH_SEP[0] )
            dname_token = dname_idx;
        dname_idx--;
    }

    mca_installdirs_runtime_component.install_dirs_data.prefix = prefix;

    /* If we goto here, there is some error. Unfortunately, we can't return an error from
     * this function, the MCA infrastructure is not yet completely setup, and a call to
     * mca_base_component_close will break.
     * So, return success but provide no meaningfull data in the component.
     */
  bail_out_with_no_data:

    free(libname);
    free(dname);

    return OPAL_SUCCESS;
}
