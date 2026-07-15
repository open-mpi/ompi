/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <string.h>

/*
 * Test that verifies symbol resolution between libmpi.so/dylib and libopen_mpi.so/dylib
 * when OMPI_LIBMPI_REEXPORT_LDFLAGS is enabled.
 *
 * This test checks that ompi_* symbols can be found in libmpi when symbol re-export
 * is enabled, which is necessary for proper dynamic linking on macOS and other platforms.
 *
 * The test returns:
 *   0  - Success: symbols resolved correctly
 *   77 - Skipped: test cannot run (dlopen failed or re-export disabled)
 *   1  - Failure: symbols not resolved as expected
 */

int main(int argc, char *argv[])
{
    void *handle = NULL;
    void *symbol = NULL;
    const char *error = NULL;
    const char *libmpi_path = getenv("OMPI_REEXPORT_TEST_LIBMPI");

    if (NULL == libmpi_path || '\0' == libmpi_path[0]) {
        printf("Test skipped: OMPI_REEXPORT_TEST_LIBMPI is not set\n");
        return 77;
    }

    handle = dlopen(libmpi_path, RTLD_LAZY);
    if (NULL == handle) {
        printf("Test skipped: Cannot dlopen %s: %s\n", libmpi_path, dlerror());
        return 77;
    }

    /* Clear any existing error */
    dlerror();

    /* Try to find an ompi_* symbol that should be re-exported from libopen_mpi */
    symbol = dlsym(handle, "ompi_mpi_comm_world");
    error = dlerror();

    if (NULL != error || NULL == symbol) {
        printf("FAIL: Could not find ompi_mpi_comm_world in libmpi: %s\n",
               error ? error : "symbol is NULL");
        dlclose(handle);
        return 1;
    }

    printf("SUCCESS: ompi_mpi_comm_world found in libmpi\n");

    /* Try another symbol to be thorough */
    dlerror();
    symbol = dlsym(handle, "ompi_mpi_comm_self");
    error = dlerror();

    if (NULL != error || NULL == symbol) {
        printf("FAIL: Could not find ompi_mpi_comm_self in libmpi: %s\n",
               error ? error : "symbol is NULL");
        dlclose(handle);
        return 1;
    }

    printf("SUCCESS: ompi_mpi_comm_self found in libmpi\n");

    dlclose(handle);
    return 0;
}