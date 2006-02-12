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

#include <vector>
#include <stdio.h>

#include "ompi/constants.h"
#include "opal/runtime/opal.h"
#include "opal/memoryhooks/memory.h"

using namespace std;

int ret = 1;
int size = 10 * 1024 * 1024;

extern "C" {
static void
callback(void *buf, size_t length, void *cbdata, bool extra)
{
    printf("\tcallback with %lx, %d\n", (unsigned long) buf, (int) length);
    ret--;
}
}

int
main(int argc, char *argv[])
{
    int retval;

    opal_init();

    if (0 == ((OPAL_MEMORY_FREE_SUPPORT|OPAL_MEMORY_MALLOC_SUPPORT) &
              opal_mem_hooks_support_level())) {
        printf("no memory registration supported.  skipping\n");
        return 77;
    }

    retval = opal_mem_hooks_register_release(callback, NULL);
    if (retval != OMPI_SUCCESS) return retval;

    vector<int> *big_vec;

    printf("    - allocating big vector\n");
    big_vec = new vector<int>;

    big_vec->reserve(size);
    /* touch all the locations, just to make sure C++ isn't smarter
       than I am */
    for (int i = 0 ; i < size ; ++i) {
        (*big_vec)[i] = i;
    }

    printf("    - deleting big vector\n");
    delete big_vec;

    printf("    - all done\n");

    retval = opal_mem_hooks_unregister_release(callback);
    if (retval != OMPI_SUCCESS) return retval;

    opal_finalize();

    /* some implementations of delete will call free twice */
    if (ret < 0) ret = 0;

    return ret;
}
