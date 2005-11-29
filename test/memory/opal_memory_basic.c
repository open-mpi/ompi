/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>

#include "ompi/include/constants.h"
#include "opal/runtime/opal.h"
#include "opal/memoryhooks/memory.h"

int ret = 1; /* because of the munmap */
int size = 10 * 1024 * 1024;

static void
alloc_callback(void *buf, size_t length, void *cbdata)
{
    printf("\talloc callback with %lx, %d\n", (unsigned long) buf, (int) length);
    ret++;
}

static void
release_callback(void *buf, size_t length, void *cbdata)
{
    printf("\trelease callback with %lx, %d\n", (unsigned long) buf, (int) length);
    ret--;
}

int
main(int argc, char *argv[])
{
    void * foo, *bar;
    int retval;
    return 77;

    opal_init();
    if (0 == ((OPAL_MEMORY_FREE_SUPPORT|OPAL_MEMORY_MALLOC_SUPPORT) &
              opal_mem_hooks_support_level())) {
        printf("no memory registration supported.  skipping\n");
        return 77;
    }
    retval = opal_mem_hooks_register_release(release_callback, NULL);
    retval |= opal_mem_hooks_register_alloc(alloc_callback, NULL);
    if (retval != OMPI_SUCCESS) {
        printf("handler registration failed\n");
        return retval;
    }

    /* make some big malloc that should trip an unmap */
    foo = malloc(size);
    printf("  - free of first big buffer\n");
    free(foo);

    /* and check munmap directly */
    printf("  - munmap of small buffer\n");
    munmap(NULL, 0);

    /* see if realloc works.  This is kind of a huristic (that going
       from a small block to a big one will fail), so don't make this
       an error */
    if (ret == 0) {
        ret = 0;
        printf("  - realloc\n");
        foo = malloc(size);
        bar = malloc(10);
        foo = realloc(foo, size * 2);
        free(bar);
        free(foo);
        if (ret != 0) {
            printf("WARNING - It appears that realloc does not trigger a callback\n");
            printf("WARNING - this may be a problem or it may be a sign that your\n");
            printf("WARNING - memory manager is better than mine\n");
            printf("ret: %d\n", ret);
        }
        ret = 0;
        printf("here\n");
    }

    retval = opal_mem_hooks_unregister_release(release_callback);
    retval |= opal_mem_hooks_unregister_alloc(alloc_callback);
    if (retval != OMPI_SUCCESS) return retval;

    opal_finalize();

    printf("ret: %d\n", ret);
    return ret;
}
