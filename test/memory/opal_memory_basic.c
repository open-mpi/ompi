/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "opal/memory/memory.h"

int ret = 1;
int size = 10 * 1024 * 1024;

static void
callback(void *buf, size_t length, void *cbdata)
{
    printf("\tcallback with %lx, %d\n", (unsigned long) buf, (int) length);
    ret = 0;
}

int
main(int argc, char *argv[])
{
    void * foo;
    int retval;

    opal_init();

    if (!opal_mem_free_is_supported()) {
        printf("no memory registration supported.  skipping\n");
        return 77;
    }

    retval = opal_mem_free_register_handler(callback, NULL);
    if (retval != OMPI_SUCCESS) return retval;

    /* make some big malloc that should trip an unmap */
    foo = malloc(size);
    free(foo);

    /* and check munmap directly */
    munmap(NULL, 0);

    retval = opal_mem_free_unregister_handler(callback);
    if (retval != OMPI_SUCCESS) return retval;

    opal_finalize();

    return ret;
}
