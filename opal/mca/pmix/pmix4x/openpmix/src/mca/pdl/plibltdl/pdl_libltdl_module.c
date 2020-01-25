/*
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include "pmix/constants.h"
#include "pmix/mca/pdl/pdl.h"

#include "pdl_libltdl.h"


static int plibltpdl_open(const char *fname, bool use_ext, bool private_namespace,
                          pmix_pdl_handle_t **handle, char **err_msg)
{
    assert(handle);

    *handle = NULL;
    if (NULL != err_msg) {
        *err_msg = NULL;
    }

    lt_dlhandle local_handle;

#if PMIX_DL_LIBLTDL_HAVE_LT_DLADVISE
    pmix_pdl_plibltpdl_component_t *c = &mca_pdl_plibltpdl_component;

    if (use_ext && private_namespace) {
        local_handle = lt_dlopenadvise(fname, c->advise_private_ext);
    } else if (use_ext && !private_namespace) {
        local_handle = lt_dlopenadvise(fname, c->advise_public_ext);
    } else if (!use_ext && private_namespace) {
        local_handle = lt_dlopenadvise(fname, c->advise_private_noext);
    } else if (!use_ext && !private_namespace) {
        local_handle = lt_dlopenadvise(fname, c->advise_public_noext);
    }
#else
    if (use_ext) {
        local_handle = lt_dlopenext(fname);
    } else {
        local_handle = lt_dlopen(fname);
    }
#endif

    if (NULL != local_handle) {
        *handle = calloc(1, sizeof(pmix_pdl_handle_t));
        (*handle)->ltpdl_handle = local_handle;

#if PMIX_ENABLE_DEBUG
        if( NULL != fname ) {
            (*handle)->filename = strdup(fname);
        }
        else {
            (*handle)->filename = strdup("(null)");
        }
#endif

        return PMIX_SUCCESS;
    }

    if (NULL != err_msg) {
        *err_msg = (char*) lt_dlerror();
    }
    return PMIX_ERROR;
}


static int plibltpdl_lookup(pmix_pdl_handle_t *handle, const char *symbol,
                            void **ptr, char **err_msg)
{
    assert(handle);
    assert(handle->ltpdl_handle);
    assert(symbol);
    assert(ptr);

    if (NULL != err_msg) {
        *err_msg = NULL;
    }

    *ptr = lt_dlsym(handle->ltpdl_handle, symbol);
    if (NULL != *ptr) {
        return PMIX_SUCCESS;
    }

    if (NULL != err_msg) {
        *err_msg = (char*) lt_dlerror();
    }
    return PMIX_ERROR;
}


static int plibltpdl_close(pmix_pdl_handle_t *handle)
{
    assert(handle);

    int ret;
    ret = lt_dlclose(handle->ltpdl_handle);

#if PMIX_ENABLE_DEBUG
    free(handle->filename);
#endif
    free(handle);

    return ret;
}

static int plibltpdl_foreachfile(const char *search_path,
                                 int (*func)(const char *filename, void *data),
                                 void *data)
{
    assert(search_path);
    assert(func);

    int ret = lt_dlforeachfile(search_path, func, data);
    return (0 == ret) ? PMIX_SUCCESS : PMIX_ERROR;
}


/*
 * Module definition
 */
pmix_pdl_base_module_t pmix_pdl_plibltpdl_module = {
    .open = plibltpdl_open,
    .lookup = plibltpdl_lookup,
    .close = plibltpdl_close,
    .foreachfile = plibltpdl_foreachfile
};
