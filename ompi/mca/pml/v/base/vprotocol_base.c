/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "base.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "static-components.h"

opal_list_t mca_vprotocol_base_components_available;

/* Load any vprotocol MCA component and call open function of all those 
 * components.
 */
int mca_vprotocol_base_open(void)
{
    OBJ_CONSTRUCT(&mca_vprotocol_base_components_available, opal_list_t);
    return mca_base_components_open("vprotocol", 0, 
                                    mca_vprotocol_base_static_components, 
                                    &mca_vprotocol_base_components_available, 
                                    true);
}

/* Close and unload any vprotocol MCA component loaded.
 */
int mca_vprotocol_base_close(void)
{
    int ret;
    ret = mca_base_components_close(pml_v_output, 
                                    &mca_vprotocol_base_components_available, 
                                    NULL);
    OBJ_DESTRUCT(&mca_vprotocol_base_components_available);
    return ret;
}
