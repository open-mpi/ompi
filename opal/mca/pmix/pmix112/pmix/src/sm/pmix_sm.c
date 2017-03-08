/*
 * Copyright (c) 2015-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include <src/include/pmix_config.h>

#include <pmix/pmix_common.h>
#include "src/include/pmix_globals.h"
#include "pmix_sm.h"
#include "pmix_mmap.h"

/*
 * Array of all possible SMs
 */

/****  ENSURE THE FOLLOWING VALUE IS AT LEAST AS
 ****  LARGE AS THE TOTAL NUMBER OF SUPPORTED SPCs
 ****  IN THE ARRAY BELOW
 */

static pmix_sm_base_module_t *all[] = {
    &pmix_sm_mmap_module,

    /* Always end the array with a NULL */
    NULL
};

pmix_sm_base_module_t pmix_sm = {0};

int pmix_sm_init(void)
{
    pmix_sm = *all[0];
    return PMIX_SUCCESS;
}

void pmix_sm_finalize(void)
{
    return ;
}

int pmix_sm_segment_create(pmix_sm_seg_t *sm_seg, const char *file_name, size_t size)
{
    if (!pmix_sm.segment_create) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    return pmix_sm.segment_create(sm_seg, file_name, size);
}

int pmix_sm_segment_attach(pmix_sm_seg_t *sm_seg, pmix_sm_access_mode_t sm_mode)
{
    if (!pmix_sm.segment_attach) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    return pmix_sm.segment_attach(sm_seg, sm_mode);
}

int pmix_sm_segment_detach(pmix_sm_seg_t *sm_seg)
{
    if (!pmix_sm.segment_detach) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    return pmix_sm.segment_detach(sm_seg);
}

int pmix_sm_segment_unlink(pmix_sm_seg_t *sm_seg)
{
    if (!pmix_sm.segment_unlink) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    return pmix_sm.segment_unlink(sm_seg);
}
