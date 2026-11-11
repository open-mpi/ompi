/*
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2025-2026 NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "opal_config.h"

#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>

#include "opal/constants.h"
#include "opal/mca/installdirs/install_dirs.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/basename.h"

#if !defined(OPAL_LIB_NAME)
#    error The installdirs/runtime component needs OPAL_LIB_NAME to recognize its own library
#endif

static int installdirs_runtime_open(void);
static int installdirs_runtime_close(void);

opal_installdirs_base_component_t mca_installdirs_runtime_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {OPAL_INSTALLDIRS_BASE_VERSION_2_0_0,

     /* Component name and version */
     "runtime", OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION, OPAL_RELEASE_VERSION,

     /* Component open and close functions */
     installdirs_runtime_open, installdirs_runtime_close},
    {/* This component is checkpointable */
     MCA_BASE_METADATA_PARAM_CHECKPOINT},

    /* Next the opal_install_dirs_t install_dirs_data information */
    {
        NULL,
    },
};
MCA_BASE_COMPONENT_INIT(opal, installdirs, runtime)

/* install_dirs.h keeps the configured paths exactly as Autoconf left them, so
 * libdir normally still reads "${exec_prefix}/lib" and is a literal path only
 * when the user passed --libdir by hand.  prefix is always literal. */
#define EXEC_PREFIX_TOKEN "${exec_prefix}"
#define PREFIX_TOKEN      "${prefix}"

#define STARTS_WITH(_str, _head) (0 == strncmp((_str), (_head), strlen(_head)))

/**
 * Return the trailing part libdir adds to the directory it is anchored on, and
 * report through @p on_exec_prefix which of the two anchors that is.  Returns
 * NULL when libdir sits under neither, which means the install tree has no
 * single prefix to recover.
 *
 * Matching the whole suffix rather than stripping one directory level is what
 * keeps a deep libdir working: configured with --libdir='${prefix}/lib/ompi',
 * the library found at /new/tree/lib/ompi still yields /new/tree.
 */
static const char *libdir_suffix(bool *on_exec_prefix)
{
    const char *libdir = OPAL_LIBDIR;

    if (STARTS_WITH(libdir, EXEC_PREFIX_TOKEN)) {
        *on_exec_prefix = true;
        return libdir + strlen(EXEC_PREFIX_TOKEN);
    }
    if (STARTS_WITH(libdir, PREFIX_TOKEN)) {
        *on_exec_prefix = false;
        return libdir + strlen(PREFIX_TOKEN);
    }
    /* An explicit --libdir is substituted literally; find which configured
     * directory it was written under.  exec_prefix has to be tried first: when
     * the two are equal, anchoring on it covers both. */
    if (!STARTS_WITH(OPAL_EXEC_PREFIX, "$") && STARTS_WITH(libdir, OPAL_EXEC_PREFIX)) {
        *on_exec_prefix = true;
        return libdir + strlen(OPAL_EXEC_PREFIX);
    }
    if (STARTS_WITH(libdir, OPAL_PREFIX)) {
        *on_exec_prefix = false;
        return libdir + strlen(OPAL_PREFIX);
    }
    return NULL;
}

/**
 * We are trying to solve a particular use case here, when the entire install
 * tree of Open MPI (and its dependencies) has been moved into another location.
 * Nothing fancy, the entire install tree has kept its shape but changed prefix.
 */
static int installdirs_runtime_open(void)
{
    const char *suffix;
    char *libname = NULL, *dname = NULL;
    size_t dname_len, suffix_len;
    bool on_exec_prefix = false;
    Dl_info info;
    void *opal_fct;

    /* Casting from void* to fct pointer according to POSIX.1-2001 and POSIX.1-2008 */
    *(void **) &opal_fct = dlsym(RTLD_DEFAULT, "opal_init_util");

    if (0 == dladdr(opal_fct, &info)) {
        goto bail_out_with_no_data;
    }

    /* Only a shared libopen-pal locates the install tree.  A static build
     * resolves the symbol into the executable itself -- whose path says nothing
     * about where Open MPI was installed -- and those users have to set
     * OPAL_PREFIX.  This component is not built when --enable-shared is off,
     * but --enable-static --enable-shared builds both, so an application can
     * reach this code having linked the static copy.
     */
    libname = opal_basename(info.dli_fname);
    if (NULL == libname || !STARTS_WITH(libname, "lib" OPAL_LIB_NAME ".")) {
        goto bail_out_with_no_data;
    }

    suffix = libdir_suffix(&on_exec_prefix);
    if (NULL == suffix) {
        goto bail_out_with_no_data;
    }

    /* Strip that suffix off the directory the library was actually found in.
     * If it is not there the tree was not relocated as a whole, and guessing
     * past that point would hand out paths that silently do not exist. */
    dname = opal_dirname(info.dli_fname);
    if (NULL == dname) {
        goto bail_out_with_no_data;
    }
    dname_len = strlen(dname);
    suffix_len = strlen(suffix);
    if (dname_len <= suffix_len || 0 != strcmp(dname + dname_len - suffix_len, suffix)) {
        goto bail_out_with_no_data;
    }
    dname[dname_len - suffix_len] = '\0';

    if (on_exec_prefix) {
        mca_installdirs_runtime_component.install_dirs_data.exec_prefix = dname;
        /* exec_prefix defaults to ${prefix}: left at that default, the
         * directory just recovered is the new prefix too.  Split apart with
         * --exec-prefix, only the half anchoring libdir can be proven, and the
         * paths rooted at prefix keep their configured values. */
        if (0 == strcmp(OPAL_EXEC_PREFIX, PREFIX_TOKEN)) {
            mca_installdirs_runtime_component.install_dirs_data.prefix = strdup(dname);
        }
    } else {
        mca_installdirs_runtime_component.install_dirs_data.prefix = dname;
    }
    dname = NULL; /* handed to the component, keep it from being freed below */

    /* If we goto here, there is some error. Unfortunately, we can't return an error from
     * this function, the MCA infrastructure is not yet completely setup, and a call to
     * mca_base_component_close will break.
     * So, return success but provide no meaningful data in the component.
     */
bail_out_with_no_data:

    free(libname);
    free(dname);

    return OPAL_SUCCESS;
}

/* The base copies these pointers out and then replaces them with the expanded
 * form (opal_install_dirs_expand_setup() always returns a fresh string), so
 * what is freed here is only ever this component's own allocation. */
static int installdirs_runtime_close(void)
{
    free(mca_installdirs_runtime_component.install_dirs_data.prefix);
    mca_installdirs_runtime_component.install_dirs_data.prefix = NULL;
    free(mca_installdirs_runtime_component.install_dirs_data.exec_prefix);
    mca_installdirs_runtime_component.install_dirs_data.exec_prefix = NULL;

    return OPAL_SUCCESS;
}
