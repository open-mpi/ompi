/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "oshmem_config.h"
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/mca/memheap/base/base.h"


/*
 * The following file was created by configure. It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "oshmem/mca/memheap/base/static-components.h"

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
    int mca_memheap_base_shmalloc_use_hugepages = 5;
#else
    int mca_memheap_base_shmalloc_use_hugepages = 1;
#endif /* MPAGE_ENABLE */

int mca_memheap_base_output = -1;
int mca_memheap_buddy_use_modex = 1;
int mca_memheap_base_mr_interleave_factor = 2;
char* mca_memheap_base_include = NULL;
char* mca_memheap_base_exclude = NULL;
opal_list_t mca_memheap_base_components_opened;
struct mca_memheap_base_module_t* mca_memheap_base_module_initialized = NULL;
int mca_memheap_base_already_opened = 0;
mca_memheap_map_t mca_memheap_base_map;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_memheap_base_open(void)
{
    int value = -1;
    mca_memheap_base_already_opened = mca_memheap_base_already_opened + 1;
    if( mca_memheap_base_already_opened > 1 ){ 
        return OSHMEM_SUCCESS;
    }

    mca_memheap_base_output = opal_output_open(NULL);
    mca_base_param_reg_int_name("memheap",
                                 "base_verbose",
                                 "Verbosity level of the MEMHEAP framework",
                                 false, false,
                                 0, &value);
    opal_output_set_verbosity(mca_memheap_base_output, value);

#if defined(MPAGE_ENABLE) && (MPAGE_ENABLE > 0)
    mca_base_param_reg_int_name("shmalloc",
        "use_hugepages",
        "0|1|2|5 - disabled, enabled with fallback to mmap(), do not fallback to mmap(), enabled mpages(default)",
        false, false,
        mca_memheap_base_shmalloc_use_hugepages, &mca_memheap_base_shmalloc_use_hugepages);
#else
    mca_base_param_reg_int_name("shmalloc",
        "use_hugepages",
        "0|1|2 - disabled, enabled(default) with fallback to mmap(), do not fallback to mmap()",
        false, false,
        mca_memheap_base_shmalloc_use_hugepages, &mca_memheap_base_shmalloc_use_hugepages);
#endif /* MPAGE_ENABLE */

    mca_base_param_reg_int_name("shmalloc",
        "use_modex",
        "0|1 - disabled, enabled(default) use modex to facilitate memory registration exchange",
        false, false,
        1, &mca_memheap_buddy_use_modex);

    mca_base_param_reg_int_name("memheap",
        "mr_interleave_factor",
        "2 - default, try to give at least N Gbytes spaces between mapped memheaps of other pes that are local to me",
        false, false,
        mca_memheap_base_mr_interleave_factor, &mca_memheap_base_mr_interleave_factor);
    /* Open up all available components */
    if (OSHMEM_SUCCESS != 
            mca_base_components_open("memheap", mca_memheap_base_output, mca_memheap_base_static_components,
                &mca_memheap_base_components_opened, true)) {
        return OSHMEM_ERROR;
    }

    /* register parameters */

    mca_base_param_reg_string_name("memheap", NULL,
            "Specify a specific memheap implementation to use",
            false, false, NULL, &mca_memheap_base_include);

    if (NULL == mca_memheap_base_include) {
        mca_memheap_base_include = getenv(SHMEM_HEAP_TYPE);
        if (NULL == mca_memheap_base_include)
            mca_memheap_base_include = strdup("");
		else
			mca_memheap_base_include = strdup(mca_memheap_base_include);
    }

    (void) mca_base_param_reg_string_name("memheap","base_exclude",NULL,false,false,NULL, &mca_memheap_base_exclude);

    memset(&mca_memheap_base_map, 0, sizeof(mca_memheap_base_map));
    mca_memheap_base_map.n_segments = 0;
    mca_memheap_base_map.num_transports = 0;

    /* All done */
    return OSHMEM_SUCCESS;
}
