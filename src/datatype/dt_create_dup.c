/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

int lam_ddt_duplicate( dt_desc_t* oldType, dt_desc_t** newType )
{
    dt_desc_t* pdt;
    void* temp;
    int desc_size = oldType->desc.used;

    if( oldType->flags & DT_FLAG_COMMITED ) desc_size++;

    pdt = lam_ddt_create( desc_size );
    temp = pdt->desc.desc; /* temporary copy of the desc pointer */
    memcpy( pdt, oldType, sizeof(dt_desc_t) );
    pdt->desc.desc = temp;
    memcpy( pdt->desc.desc, oldType->desc.desc, sizeof(dt_elem_desc_t) * desc_size );
    pdt->id  = 0;
    pdt->args = NULL;
    /* TODO: if the data was commited update the opt_desc field */
    *newType = pdt;
    return 0;
}

int lam_ddt_create_contiguous( int count, dt_desc_t* oldType, dt_desc_t** newType )
{
   dt_desc_t* pdt = lam_ddt_create( oldType->desc.used + 2 );
   lam_ddt_add( pdt, oldType, count, 0, (oldType->ub - oldType->lb) );
   *newType = pdt;
   return 0;
}
