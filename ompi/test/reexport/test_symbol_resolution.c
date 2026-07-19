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
 * Test that verifies symbol resolution between libmpi.so/dylib and
 * libopen_mpi.so/dylib: the predefined MPI handle symbols (e.g.,
 * ompi_mpi_comm_world) that moved into libopen_mpi must still be
 * reachable through libmpi, so that binaries built against Open MPI
 * v5.x continue to run.  On macOS this requires libmpi to re-export
 * libopen_mpi (LC_REEXPORT_DYLIB); on ELF platforms the flat
 * namespace resolves the symbols through libmpi's DT_NEEDED
 * dependency on libopen_mpi with no re-export mechanism at all.
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