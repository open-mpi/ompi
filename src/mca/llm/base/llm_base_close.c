/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"

int mca_llm_base_close(void)
{
    /* This probably shouldn't be here, but I don't know where else to
       put it */
    OBJ_DESTRUCT(&mca_llm_base_parse_mutex);

    /* Close all remaining available modules (may be one if this is a
       OMPI RTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(mca_llm_base_output, 
                              &mca_llm_base_components_available, NULL);

  /* All done */

  return OMPI_SUCCESS;
}

