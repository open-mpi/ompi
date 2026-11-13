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
#include <sys/stat.h>

#if defined(__APPLE__)
#    include <mach-o/dyld.h>
#endif

#include "opal/constants.h"
#include "opal/mca/installdirs/install_dirs.h"
#include "opal/mca/installdirs/installdirs.h"
#include "opal/util/basename.h"
#include "opal/util/printf.h"

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
#define LIBDIR_TOKEN      "${libdir}"

#define STARTS_WITH(_str, _head) (0 == strncmp((_str), (_head), strlen(_head)))

/**
 * Return the trailing part @p path adds to the directory it is anchored on, and
 * report through @p on_exec_prefix which of the two anchors that is.  Returns
 * NULL when @p path sits under neither, which means the install tree has no
 * single prefix to recover.
 *
 * Matching the whole suffix rather than stripping one directory level is what
 * keeps a deep libdir working: configured with --libdir='${prefix}/lib/ompi',
 * the library found at /new/tree/lib/ompi still yields /new/tree.
 */
static const char *anchored_suffix(const char *path, bool *on_exec_prefix)
{
    if (STARTS_WITH(path, EXEC_PREFIX_TOKEN)) {
        *on_exec_prefix = true;
        return path + strlen(EXEC_PREFIX_TOKEN);
    }
    if (STARTS_WITH(path, PREFIX_TOKEN)) {
        *on_exec_prefix = false;
        return path + strlen(PREFIX_TOKEN);
    }
    /* An explicit --libdir or --bindir is substituted literally; find which
     * configured directory it was written under.  exec_prefix has to be tried
     * first: when the two are equal, anchoring on it covers both. */
    if (!STARTS_WITH(OPAL_EXEC_PREFIX, "$") && STARTS_WITH(path, OPAL_EXEC_PREFIX)) {
        *on_exec_prefix = true;
        return path + strlen(OPAL_EXEC_PREFIX);
    }
    if (STARTS_WITH(path, OPAL_PREFIX)) {
        *on_exec_prefix = false;
        return path + strlen(OPAL_PREFIX);
    }
    return NULL;
}

/**
 * Ask the OS for the absolute path of the running executable.  Returns a
 * malloc'ed string, or NULL when this platform offers no reliable answer --
 * dladdr() cannot be used for this, because for a symbol that lives in the
 * executable glibc reports argv[0], which is whatever the caller typed.
 */
static char *executable_path(void)
{
#if defined(__APPLE__)
    char stackbuf[1024], *heapbuf = NULL, *buf = stackbuf, *resolved;
    uint32_t len = sizeof(stackbuf);

    if (0 != _NSGetExecutablePath(buf, &len)) {
        /* Did not fit; len now holds the size we need. */
        heapbuf = malloc(len);
        if (NULL == heapbuf) {
            return NULL;
        }
        if (0 != _NSGetExecutablePath(heapbuf, &len)) {
            free(heapbuf);
            return NULL;
        }
        buf = heapbuf;
    }
    /* The answer may still be relative or contain symlinks and "..". */
    resolved = realpath(buf, NULL);
    free(heapbuf);

    return resolved;
#else
    /* Linux, and the other systems carrying a Linux-shaped /proc. */
    return realpath("/proc/self/exe", NULL);
#endif
}

/**
 * Confirm that @p root really heads an Open MPI install tree, by looking for
 * the directory our MCA components are installed in.  Only the executable path
 * below needs this: a binary that statically linked us can sit in any bindir
 * at all, and without a marker every such application in /usr/local/bin would
 * claim /usr/local as the Open MPI prefix.
 *
 * @p root_is_exec_prefix says which of the two directories @p root names.
 */
static bool tree_holds_our_components(const char *root, bool root_is_exec_prefix)
{
    const char *pkglibdir = OPAL_PKGLIBDIR;
    const char *lib_suffix, *pkg_tail;
    bool lib_on_exec_prefix = false;
    struct stat info;
    char *probe;
    int rc;

    /* OPAL_PKGLIBDIR is "${libdir}/openmpi"; the tail is what hangs off
     * libdir.  Anything else is a shape we cannot rebuild from here. */
    if (!STARTS_WITH(pkglibdir, LIBDIR_TOKEN)) {
        return false;
    }
    pkg_tail = pkglibdir + strlen(LIBDIR_TOKEN);

    lib_suffix = anchored_suffix(OPAL_LIBDIR, &lib_on_exec_prefix);
    if (NULL == lib_suffix) {
        return false;
    }
    /* root was recovered from one anchor while libdir may hang off the other.
     * Those name the same directory unless --exec-prefix was given. */
    if (lib_on_exec_prefix != root_is_exec_prefix
        && 0 != strcmp(OPAL_EXEC_PREFIX, PREFIX_TOKEN)) {
        return false;
    }

    if (0 > opal_asprintf(&probe, "%s%s%s", root, lib_suffix, pkg_tail)) {
        return false;
    }
    rc = stat(probe, &info);
    free(probe);

    return (0 == rc) && S_ISDIR(info.st_mode);
}

/**
 * We are trying to solve a particular use case here, when the entire install
 * tree of Open MPI (and its dependencies) has been moved into another location.
 * Nothing fancy, the entire install tree has kept its shape but changed prefix.
 */
static int installdirs_runtime_open(void)
{
    const char *suffix;
    char *libname = NULL, *dname = NULL, *self = NULL;
    size_t dname_len, suffix_len;
    bool on_exec_prefix = false, from_executable = false;
    Dl_info info;
    void *opal_fct;

    /* Casting from void* to fct pointer according to POSIX.1-2001 and POSIX.1-2008 */
    *(void **) &opal_fct = dlsym(RTLD_DEFAULT, "opal_init_util");

    if (0 == dladdr(opal_fct, &info)) {
        goto bail_out_with_no_data;
    }

    libname = opal_basename(info.dli_fname);
    if (NULL == libname) {
        goto bail_out_with_no_data;
    }

    if (STARTS_WITH(libname, "lib" OPAL_LIB_NAME ".")) {
        /* The usual case: we are in the shared library, which lives in libdir. */
        suffix = anchored_suffix(OPAL_LIBDIR, &on_exec_prefix);
        dname = opal_dirname(info.dli_fname);
    } else {
        /* opal was linked straight into the running executable, so there is no
         * library of ours to locate.  mpirun and the wrapper compilers do this
         * deliberately (they pull in libopen-pal_core), and they are precisely
         * the commands a user runs first out of a relocated tree -- so fall
         * back to where the executable itself is, which for anything we install
         * is bindir.  The marker check below keeps unrelated applications that
         * happen to link us statically from claiming their own bindir. */
        self = executable_path();
        if (NULL == self) {
            goto bail_out_with_no_data;
        }
        suffix = anchored_suffix(OPAL_BINDIR, &on_exec_prefix);
        dname = opal_dirname(self);
        from_executable = true;
    }

    if (NULL == suffix || NULL == dname) {
        goto bail_out_with_no_data;
    }

    /* Strip that suffix off the directory we were actually found in.  If it is
     * not there the tree was not relocated as a whole, and guessing past that
     * point would hand out paths that silently do not exist. */
    dname_len = strlen(dname);
    suffix_len = strlen(suffix);
    if (dname_len <= suffix_len || 0 != strcmp(dname + dname_len - suffix_len, suffix)) {
        goto bail_out_with_no_data;
    }
    dname[dname_len - suffix_len] = '\0';

    if (from_executable && !tree_holds_our_components(dname, on_exec_prefix)) {
        goto bail_out_with_no_data;
    }

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
    free(self);

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
