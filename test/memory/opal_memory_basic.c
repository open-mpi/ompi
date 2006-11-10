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
#include <assert.h>

#include "ompi/constants.h"
#include "opal/runtime/opal.h"
#include "opal/memoryhooks/memory.h"

/* 
 * The counter variable is volatile to avoid (wrong) compiler optimisations, 
 * which can lead to wrong code.
 */
volatile int counter = 0;
const int bigsize = 100 * 1024 * 1024;

static void
alloc_callback(void *buf, size_t length, void *cbdata, bool extra)
{
    counter++;
}

static void
release_callback(void *buf, size_t length, void *cbdata, bool extra)
{
    printf("\trelease callback with %lx, %d\n", (unsigned long) buf, (int) length);
    counter--;
}


static int
free_only_test(void)
{
    /* BWB - finish me! */
    printf("test not written yet - skipping\n");
    return 77;
}


static int
alloc_free_test(void)
{
    void *foo, *bar;
    int retval;

    retval = opal_mem_hooks_register_release(release_callback, NULL);
    retval |= opal_mem_hooks_register_alloc(alloc_callback, NULL);
    if (retval != OMPI_SUCCESS) {
        printf("handler registration failed\n");
        return retval;
    }

    /* make some big malloc that should always trip a release on free */
    printf("  - malloc big buffer\n");
    counter = 0;
    foo = malloc(bigsize);
    assert(counter >= 1);
    printf("  - free of big buffer\n");
    counter = 1;
    free(foo);
    assert(counter == 0);

    /* check mmap / munmap */
    printf("  - mmap of buffer\n");
    counter = 0;
    bar = mmap(NULL, 4096, PROT_READ, MAP_ANON, -1, 0);
    if (opal_mem_hooks_support_level() & OPAL_MEMORY_MMAP_SUPPORT) {
        assert(counter >= 1);
    }

    printf("  - munmap of buffer\n");
    /* mmap might call malloc internally, so do this or we might
       appear to leak memory */
    counter = 1;
    munmap(NULL, 0);
    assert(counter == 0);

    retval = opal_mem_hooks_unregister_release(release_callback);
    retval |= opal_mem_hooks_unregister_alloc(alloc_callback);

    return OPAL_SUCCESS;
}


int
main(int argc, char *argv[])
{
    int ret;
    int support;

    opal_init();

    /* this printf needs to be here for the test to work! */
    printf("running malloc hooks test\n");

    support = opal_mem_hooks_support_level();
    if (0 == support) {
        printf("no memory registration supported.  skipping test.\n");
        ret = 77;
    } else if ((OPAL_MEMORY_FREE_SUPPORT|OPAL_MEMORY_MALLOC_SUPPORT) ==
               ((OPAL_MEMORY_FREE_SUPPORT|OPAL_MEMORY_MALLOC_SUPPORT) & support)) {
        ret = alloc_free_test();
    } else if (OPAL_MEMORY_FREE_SUPPORT & support) {
        ret  = free_only_test();
    } else {
        printf("Odd support response: %d\n", support);
        ret = 1;
    }

    opal_finalize();

    return ret;
}
