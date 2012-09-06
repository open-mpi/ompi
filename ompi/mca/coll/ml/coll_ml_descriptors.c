/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include "coll_ml.h"
#include "coll_ml_inlines.h"


static inline void mca_coll_ml_fragment_constructor(mca_coll_ml_fragment_t *frag)
{


}

static inline void mca_coll_ml_fragment_destructor(mca_coll_ml_fragment_t *frag)
{
}

static inline void mca_coll_ml_descriptor_constructor(mca_coll_ml_descriptor_t *descriptor)
{

 OBJ_CONSTRUCT(&(descriptor->fragment),mca_coll_ml_fragment_t);

 /* this fragment is alway associated with this message descriptor */
 descriptor->fragment.full_msg_descriptor=descriptor;

}


static inline void mca_coll_ml_descriptor_destructor(mca_coll_ml_descriptor_t *descriptor)
{
 OBJ_DESTRUCT(&(descriptor->fragment));
}

OBJ_CLASS_INSTANCE(
    mca_coll_ml_fragment_t,
    opal_list_item_t,
    mca_coll_ml_fragment_constructor,
    mca_coll_ml_fragment_destructor);

OBJ_CLASS_INSTANCE(
    mca_coll_ml_descriptor_t,
    ompi_request_t,
    mca_coll_ml_descriptor_constructor,
    mca_coll_ml_descriptor_destructor);

