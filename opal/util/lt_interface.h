/*
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
* Wrappers to LTDL functions so that other parts of library can
* access the LTDL functionality that is embedded/linked in to Open
* MPI (vs. a LTDL library that was linked in by the application).
* The OPAL-linked LTDL library is intentionally not DECLSPEC'ed so
* that it is not visible to upper-layer applications; its functions
* are provided to the rest of the OMPI code base via the following
* OPAL wrappers.
* For functions that return int, if there are any failures, these
* functions can return both OPAL error codes and LTDL error codes.
* If the return code is negative it is OPAL, and if positive it 
* follows the LTDL convention where the number represents the number
* of errors seen.  You can then call opal_lt_dlerror() to get more
* information when you see a positive error code.
*/

/**
 * Create opal_lt_dlhandle and opal_lt_dladvise types so that
 * consumers of this code can use these opal types rather than the
 * LTDL versions.  In this way, there is no need to include the
 * underlying ltdl.h here (it is only needed in lt_interface.c) and we
 * do not need to use the OPAL_WANT_LIBLTDL and OPAL_HAVE_LTDL_ADVISE
 * macros anywhere else.  These types are effectively just pointers to
 * the underlying LTDL types.
 */
struct opal_lt_dlhandle_st;
typedef struct opal_lt_dlhandle_st* opal_lt_dlhandle;
struct opal_lt_dladvise_st;
typedef struct opal_lt_dladvise_st* opal_lt_dladvise;

/**
 * Wrappers for the ltdl library.
 */
OPAL_DECLSPEC int opal_lt_dlinit(void);
OPAL_DECLSPEC int opal_lt_dlexit(void);

/* Module search path manipulation.  */
OPAL_DECLSPEC int opal_lt_dladdsearchdir(const char *search_dir);
OPAL_DECLSPEC int opal_lt_dlinsertsearchdir(const char *before,
                                            const char *search_dir);
OPAL_DECLSPEC int opal_lt_dlsetsearchpath(const char *search_path);
OPAL_DECLSPEC const char *opal_lt_dlgetsearchpath(void);
OPAL_DECLSPEC int opal_lt_dlforeachfile(const char *search_path,
                                        int (*func) (const char *filename, void *data),
                                        void *data);
/* User module loading advisors.  */
OPAL_DECLSPEC int opal_lt_dladvise_init(opal_lt_dladvise *advise);
OPAL_DECLSPEC int opal_lt_dladvise_destroy(opal_lt_dladvise *advise);
OPAL_DECLSPEC int opal_lt_dladvise_ext(opal_lt_dladvise *advise);
OPAL_DECLSPEC int opal_lt_dladvise_resident(opal_lt_dladvise *advise);
OPAL_DECLSPEC int opal_lt_dladvise_local(opal_lt_dladvise *advise);
OPAL_DECLSPEC int opal_lt_dladvise_global(opal_lt_dladvise *advise);
OPAL_DECLSPEC int opal_lt_dladvise_preload(opal_lt_dladvise *advise);

/* Portable libltdl versions of the system dlopen() API. */
OPAL_DECLSPEC opal_lt_dlhandle opal_lt_dlopen(const char *filename);
OPAL_DECLSPEC opal_lt_dlhandle opal_lt_dlopenext(const char *filename);
OPAL_DECLSPEC void *opal_lt_dlsym(opal_lt_dlhandle handle, const char *name);
OPAL_DECLSPEC const char *opal_lt_dlerror(void);
OPAL_DECLSPEC int opal_lt_dlclose(opal_lt_dlhandle handle);
OPAL_DECLSPEC opal_lt_dlhandle opal_lt_dlopenadvise(const char *filename,
                                                    opal_lt_dladvise advise);
