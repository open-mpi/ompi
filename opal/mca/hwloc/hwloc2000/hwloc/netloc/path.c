/*
 * Copyright © 2016 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <stdlib.h>

#include <private/netloc.h>

netloc_path_t *netloc_path_construct(void)
{
    netloc_path_t *path = (netloc_path_t *)
        malloc(sizeof(netloc_path_t ));
    utarray_new(path->links, &ut_ptr_icd);

    return path;
}

int netloc_path_destruct(netloc_path_t *path)
{
    utarray_free(path->links);
    free(path);
    return NETLOC_SUCCESS;
}
