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
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Los Alamos National Security, LLC.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * only do this test if we have built with memkind support
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "opal_config.h"
#ifdef HAVE_MEMKIND_H
#include "opal/constants.h"
#include "opal/mca/mpool/mpool.h"
#include "opal/include/opal/frameworks.h"
#include "opal/runtime/opal.h"

#define SIZE (2 * 1024 * 1024)

const char *memory_types[] = {
  "memkind_default",
  "memkind_hbw",
   NULL
};

const char *memory_policy[] = {
   "mempolicy_bind_local",
   "mempolicy_bind_all",
   "mempolicy_perferred_local",
   "mempolicy_interleave_local",
   "mempolicy_interleave_all",
   NULL
};

const char *memory_kind_bits[] = {
   "memkind_mask_page_size_4KB",
   "memkind_mask_page_size_2MB",
   NULL
};

int main (int argc, char* argv[])
{
    int ret = 0;
    void *ptr = NULL;
    char *error = NULL;
    char **mp_ptr = NULL;
    char **mt_ptr = NULL;
    char **mk_ptr = NULL;
    const char mpool_hints[] = "mpool=memkind";
    char hints[1024];

    opal_init_util(&argc, &argv);

    if (opal_frameworks == NULL){
        error = "opal frameworks is NULL";
        goto error;
    }

    if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_allocator_base_framework, 0))) {
        error = "mca_allocator_base_open() failed";
        goto error;
    }

    if (OPAL_SUCCESS != (ret = mca_base_framework_open(&opal_mpool_base_framework, 0))) {
        error = "mca_mpool_base_open() failed";
        goto error;
    }

    /*
     * first try basic allocation
     */

    ptr = mca_mpool_base_alloc(SIZE, NULL, mpool_hints);
    if (NULL == ptr) {
        error = "mca_mpool_base_alloc() failed";
        goto error;
    }

    if (OPAL_SUCCESS != mca_mpool_base_free(ptr)) {
        error = "mca_mpool_base_free() failed";
        goto error;
    }

    /*
     * now try policies
     */

    mp_ptr = (char **)memory_policy;
    while (NULL != *mp_ptr) {

        mt_ptr = (char **)memory_types;
        while (NULL != *mt_ptr) {

            mk_ptr = (char **)memory_kind_bits;
            while (NULL != *mk_ptr) {
                snprintf(hints, sizeof(hints), "%s,policy=%s,type=%s,kind=%s", 
                         mpool_hints, *mp_ptr, *mt_ptr, *mk_ptr);
                ptr = mca_mpool_base_alloc(SIZE, NULL, hints);
                if (NULL == ptr) {
                    error = "mca_mpool_base_alloc() failed";
                    goto error;
                }

                if (OPAL_SUCCESS != mca_mpool_base_free(ptr)) {
                    error = "mca_mpool_base_free() failed";
                    goto error;
                }
                mk_ptr++;
            }
            mt_ptr++;
        }
        mp_ptr++;
    }

    if (OPAL_SUCCESS != (ret = mca_base_framework_close(&opal_mpool_base_framework))) {
        error = "mca_mpool_base_close() failed";
        goto error;
    }

    if (OPAL_SUCCESS != (ret = mca_base_framework_close(&opal_allocator_base_framework))) {
        error = "mca_mpool_base_close() failed";
        goto error;
    }

    opal_finalize();

error:
    if (NULL != error) {
        fprintf(stderr, "mpool/memkind test failed %s\n", error);
        ret = -1;
    } else {
        fprintf(stderr, "mpool/memkind test passed\n");
    }

    return ret;
}
#else
int main (int argc, char* argv[])
{
    return 77;
}
#endif /* HAVE_MEMKIND_H */
