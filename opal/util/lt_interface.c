/*
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include <stdio.h>
#include "opal/constants.h"
#include "opal/util/lt_interface.h"

OPAL_DECLSPEC int opal_lt_dlinit(void)
{
#if OPAL_WANT_LIBLTDL
    return lt_dlinit();
#else /* OPAL_WANT_LIBLTDL */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC int opal_lt_dladdsearchdir(const char *search_dir) {
#if OPAL_WANT_LIBLTDL
    return lt_dladdsearchdir(search_dir);
#else /* OPAL_WANT_LIBLTDL */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC int opal_lt_dlinsertsearchdir(const char *before,
                                            const char *search_dir) {
#if OPAL_WANT_LIBLTDL
    return lt_dlinsertsearchdir(before, search_dir);
#else /* OPAL_WANT_LIBLTDL */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC int opal_lt_dlsetsearchpath(const char *search_path) {
#if OPAL_WANT_LIBLTDL
    return lt_dlsetsearchpath(search_path);
#else /* OPAL_WANT_LIBLTDL */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC const char *opal_lt_dlgetsearchpath(void) {
#if OPAL_WANT_LIBLTDL
    return lt_dlgetsearchpath();
#else /* OPAL_WANT_LIBLTDL */
    return NULL;
#endif /* OPAL_WANT_LIBLTDL */
}
OPAL_DECLSPEC int opal_lt_dlforeachfile(const char *search_path,
                                        int (*func) (const char *filename, void *data),
                                        void *data) {
#if OPAL_WANT_LIBLTDL
    return lt_dlforeachfile(search_path, func, data);
#else /* OPAL_WANT_LIBLTDL */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC int opal_lt_dlexit(void) {
#if OPAL_WANT_LIBLTDL
    return lt_dlexit();
#else /* OPAL_WANT_LIBLTDL */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC void *opal_lt_dlsym(opal_lt_dlhandle handle, const char *name) {
#if OPAL_WANT_LIBLTDL
    return lt_dlsym(handle, name);
#else /* OPAL_WANT_LIBLTDL */
    return NULL;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC const char *opal_lt_dlerror(void) {
#if OPAL_WANT_LIBLTDL
    return lt_dlerror();
#else /* OPAL_WANT_LIBLTDL */
    return NULL;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC int opal_lt_dladvise_init(opal_lt_dladvise *advise) {
#if OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE
    return lt_dladvise_init(advise);
#else /* OPAL_WANT_LIBLTDL */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC int opal_lt_dladvise_ext(opal_lt_dladvise *advise) {
#if OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE
    return lt_dladvise_ext(advise);
#else /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */

}
OPAL_DECLSPEC int opal_lt_dladvise_global(opal_lt_dladvise *advise) {
#if OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE
    return lt_dladvise_global(advise);
#else /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
}

OPAL_DECLSPEC opal_lt_dlhandle opal_lt_dlopenadvise(const char *filename, opal_lt_dladvise advise) {
#if OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE
    return lt_dlopenadvise(filename, advise);
#else /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
    return NULL;
#endif /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
}

OPAL_DECLSPEC int opal_lt_dladvise_destroy(opal_lt_dladvise *advise) {
#if OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE
    return lt_dladvise_destroy(advise);
#else /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
}

OPAL_DECLSPEC int opal_lt_dladvise_resident(opal_lt_dladvise *advise) {
#if OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE
    return lt_dladvise_resident(advise);
#else /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
}

OPAL_DECLSPEC int opal_lt_dladvise_local(opal_lt_dladvise *advise) {
#if OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE
    return lt_dladvise_local(advise);
#else /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
}

OPAL_DECLSPEC int opal_lt_dladvise_preload(opal_lt_dladvise *advise) {
#if OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE
    return lt_dladvise_preload(advise);
#else /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
    return OPAL_ERR_NOT_SUPPORTED;
#endif /* OPAL_WANT_LIBLTDL && OPAL_HAVE_LTDL_ADVISE */
}

/* Portable libltdl versions of the system dlopen() API. */
OPAL_DECLSPEC opal_lt_dlhandle opal_lt_dlopen(const char *filename) {
#if OPAL_WANT_LIBLTDL
    return lt_dlopen(filename);
#else /* OPAL_WANT_LIBLTDL */
    return NULL;
#endif /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC opal_lt_dlhandle opal_lt_dlopenext(const char *filename) {
#if OPAL_WANT_LIBLTDL
    return lt_dlopenext(filename);
#else  /* OPAL_WANT_LIBLTDL */
    return 0;
#endif  /* OPAL_WANT_LIBLTDL */
}

OPAL_DECLSPEC int opal_lt_dlclose(opal_lt_dlhandle handle) {
#if OPAL_WANT_LIBLTDL
    return lt_dlclose(handle);
#else
    return OPAL_ERR_NOT_SUPPORTED;
#endif
}
