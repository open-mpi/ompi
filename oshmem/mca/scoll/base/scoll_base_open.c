/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include <stdio.h>

#include "oshmem_config.h"

#include "oshmem/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "oshmem/mca/scoll/base/static-components.h"


/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
long* mca_scoll_sync_array = NULL;

int mca_scoll_base_output = -1;

bool mca_scoll_base_components_opened_valid = false;
opal_list_t mca_scoll_base_components_opened;

OBJ_CLASS_INSTANCE(mca_scoll_base_module_t, opal_object_t, NULL, NULL);

/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_scoll_base_open(void)
{
    /* Open an output stream for this framework */
    int value = -1;

    mca_scoll_base_output = opal_output_open(NULL);
    mca_base_param_reg_int_name("scoll_base","verbose",
			"Verbose level of the shmem scoll component",false,false,0,&value);
	opal_output_set_verbosity(mca_scoll_base_output, value);

    /* Open up all available components */
    if (OSHMEM_SUCCESS !=
        mca_base_components_open("scoll", mca_scoll_base_output,
                                 mca_scoll_base_static_components,
                                 &mca_scoll_base_components_opened, true)) {
        return OSHMEM_ERROR;
    }
    mca_scoll_base_components_opened_valid = true;

    /* All done */

    return OSHMEM_SUCCESS;
}


int mca_scoll_enable(void) 
{   
    int ret = OSHMEM_SUCCESS;

    if (!mca_scoll_sync_array)
    {
        void* ptr = (void*)mca_scoll_sync_array;
        int i = 0;

        MCA_MEMHEAP_CALL(private_alloc((_SHMEM_BARRIER_SYNC_SIZE * sizeof(*mca_scoll_sync_array)), &ptr));
        mca_scoll_sync_array = ptr;

        for ( i = 0; i < _SHMEM_BARRIER_SYNC_SIZE; i++ )
        {
            mca_scoll_sync_array[i] = _SHMEM_SYNC_VALUE;
        }
    }

    /* Note: it is done to support FCA only and we need to consider possibility to
     * find a way w/o this ugly hack 
     */
    if (OSHMEM_SUCCESS != (ret = mca_scoll_base_select(oshmem_group_all)))
    {
        return ret;
    }
    if (OSHMEM_SUCCESS != (ret = mca_scoll_base_select(oshmem_group_self)))
    {
        return ret;
    }

    return OSHMEM_SUCCESS;
}
