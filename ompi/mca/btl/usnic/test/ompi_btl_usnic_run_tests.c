/*
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* A simple test runner program for the usnic BTL unit tests.  See README.test
 * for more information. */

/* for dladdr() */
#define _GNU_SOURCE

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h> /* for dirname() */
#include <mpi.h>

#include <dlfcn.h>

#define MCA_BTL_USNIC_SO "mca_btl_usnic.so"

typedef void (*run_tests_fn_t)(void);

int main(int argc, char **argv)
{
    void *mpi_handle;
    void *usnic_handle;
    void (*run_tests)(void);
    int (*init)(int *, char ***);
    int (*finalize)(void);
    Dl_info info;
    char *libmpi_path;
    char *path;
    char *to;
    int path_len;

    mpi_handle = dlopen("libmpi.so", RTLD_NOW|RTLD_GLOBAL);
    if (mpi_handle == NULL) {
        fprintf(stderr, "mpi_handle=NULL dlerror()=%s\n", dlerror());
        abort();
    }

    /* casting awfulness needed for GCC's "-pedantic" option :( */
    *(void **)(&init) = dlsym(mpi_handle, "MPI_Init");
    if (init == NULL) {
        fprintf(stderr, "init=NULL dlerror()=%s\n", dlerror());
        abort();
    }
    /* casting awfulness needed for GCC's "-pedantic" option :( */
    *(void **)(&finalize) = dlsym(mpi_handle, "MPI_Finalize");
    if (finalize == NULL) {
        fprintf(stderr, "finalize=%p dlerror()=%s\n", *(void **)(&finalize), dlerror());
        abort();
    }

    /* call MPI_Init this way to avoid build-time dependency issues */
    init(&argc, &argv);

    /* figure out where the usnic BTL shared object is relative to libmpi.so */
    if (!dladdr(*(void **)(&init), &info)) {
        fprintf(stderr, "ERROR: unable to dladdr(init,...)\n");
        abort();
    }
    libmpi_path = strdup(info.dli_fname);
    assert(libmpi_path != NULL);
    path_len = strlen(libmpi_path) + strlen("/openmpi/") + strlen(MCA_BTL_USNIC_SO);
    path = calloc(path_len+1, 1);
    to = path;
    to = stpcpy(to, dirname(libmpi_path));
    to = stpcpy(to, "/openmpi/");
    to = stpcpy(to, MCA_BTL_USNIC_SO);

    usnic_handle = dlopen(path, RTLD_NOW|RTLD_LOCAL);
    if (usnic_handle == NULL) {
        fprintf(stderr, "usnic_handle=%p dlerror()=%s\n", (void *)usnic_handle, dlerror());
        abort();
    }

    free(libmpi_path);
    free(path);

    /* casting awfulness needed for GCC's "-pedantic" option :( */
    *(void **)(&run_tests) = dlsym(usnic_handle, "ompi_btl_usnic_run_tests");
    if (run_tests == NULL) {
        fprintf(stderr, "run_tests=%p dlerror()=%s\n", *(void **)(&run_tests), dlerror());
        abort();
    }
    run_tests();

    finalize();

    /* deliberately do not dlclose() either handle so that any valgrind stack
     * traces are more useful */

    return 0;
}
