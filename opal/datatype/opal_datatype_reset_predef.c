/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stddef.h>

#include "opal/runtime/opal.h"
#include "opal/util/arch.h"
#include "opal/util/output.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_convertor_internal.h"
#include "opal/mca/base/mca_base_var.h"

int
opal_datatype_desc_update_reset(opal_datatype_t *pData)
{
    int opt_equals_desc;

    void *p1 = opal_datatype_predefined_elem_desc;
    void *p2 = &opal_datatype_predefined_elem_desc[2 * OPAL_DATATYPE_MAX_PREDEFINED];
    if ((void*)pData->desc.desc >= p1 && (void*)pData->desc.desc <= p2) {
        if (pData->desc.desc != pData->opt_desc.desc) {
            opt_equals_desc = 0;
        } else {
            opt_equals_desc = 1;
        }
/*
 *  This first section is privatizing the description.
 *  The .desc fields for the OPAL contents of the predefined
 *  OMPI types are initialized as offsets into
 *      opal_datatype_predefined_elem_desc[].
 *  with multiple OMPI types sharing the same offset.
 *
 *  In order for them to each get their own data (for .ompi_id)
 *  the starting .desc needs copied into its own space.
 */
        dt_elem_desc_t *new = malloc(2 * sizeof(dt_elem_desc_t));
        if (!new) { return -1; }
        memcpy(new, pData->desc.desc, 2 * sizeof(dt_elem_desc_t));

        pData->desc.desc = new;
        if (opt_equals_desc) {
            pData->opt_desc.desc = new;
        }
/*
 *  Now we can set the .ompi_id down in the description.
 *  For the predefined types the relevant entry is desc[0].
 *  The initial values for opal_datatype_predefined_elem_desc[]
 *  came from opal_datatype_init() where it did
 *    datatype->desc.desc[0].elem.common.type  = i;
 */
        pData->desc.desc[0].elem.common.ompi_id = pData->ompi_id;;
        pData->flags |= OPAL_DATATYPE_FLAG_DESC_WAS_REALLOCATED;
    }

    return OPAL_SUCCESS;
}

/*
 *  Free the .desc field if it was reset by
 *  opal_datatype_desc_update_reset()
 *
 *  What's going on with desc vs opt_desc is they can be
 *  equal or differ.  If they're equal they need to stay
 *  equal across these modifications of the description.
 *  Later in opal_datatype_destruct() it takes alternate
 *  paths for same vs different desc/opt_desc.
 */
void
opal_datatype_desc_update_free(opal_datatype_t *pData)
{
    int opt_equals_desc;

    if (pData->flags & OPAL_DATATYPE_FLAG_DESC_WAS_REALLOCATED) {
        if (pData->desc.desc != pData->opt_desc.desc) {
            opt_equals_desc = 0;
        } else {
            opt_equals_desc = 1;
        }

        if (pData->desc.desc) {
            free(pData->desc.desc);
            pData->desc.desc = NULL;
        }
        if (opt_equals_desc) {
            pData->opt_desc.desc = pData->desc.desc;
        }
        pData->flags ^= OPAL_DATATYPE_FLAG_DESC_WAS_REALLOCATED;
    }
}
