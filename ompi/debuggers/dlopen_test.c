/*
 * Copyright (c) 2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include "opal/libltdl/ltdl.h"

int main(int argc, char *argv[])
{
    char *filename = "./libompi_dbg_msgq";
    lt_dlhandle dlhandle;

#if OPAL_HAVE_LTDL_ADVISE
    lt_dladvise dladvise;
#endif

    if (lt_dlinit() != 0) {
        printf("Failed to lt_dlinit\n");
        return 1;
    }

    printf("Trying to lt_dlopen file with dladvise_local: %s\n", filename);

#if OPAL_HAVE_LTDL_ADVISE
    if (lt_dladvise_init(&dladvise) ||
        lt_dladvise_ext(&dladvise) ||
        lt_dladvise_local(&dladvise)) {
        printf("lt_dladvise failed to initialize properly\n");
        return 1;
    }
    dlhandle = lt_dlopenadvise(filename, dladvise);
    lt_dladvise_destroy(&dladvise);
#else
    dlhandle = lt_dlopenext(filename);
#endif
    if (NULL != dlhandle) {
        lt_dlclose(dlhandle);
	printf("File opened with dladvise_local, all passed\n");
        return 0;
    }

    printf("Failed to open with dladvise_local: %s\n", lt_dlerror());
    printf("Retrying with dladvise_global\n");

#if OPAL_HAVE_LTDL_ADVISE
    if (lt_dladvise_init(&dladvise) ||
        lt_dladvise_ext(&dladvise) ||
        lt_dladvise_global(&dladvise)) {
        printf("lt_dladvise failed to initialize properly\n");
        return 1;
    }
    dlhandle = lt_dlopenadvise(filename, dladvise);
    lt_dladvise_destroy(&dladvise);
#else
    dlhandle = lt_dlopenext(filename);
#endif
    if (NULL != dlhandle) {
        lt_dlclose(dlhandle);
	printf("File opened with dladvise_global\n");
	return 1;
    }
    printf("File failed to open with dladvise_global: %s\n", lt_dlerror());

    return 2;
}
