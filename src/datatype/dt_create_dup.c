/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"

int lam_ddt_duplicate( dt_desc_t* oldType, dt_desc_t** newType )
{
   dt_desc_t* pdt = lam_ddt_create( oldType->desc.used );
   void* temp = pdt->desc.desc; /* temporary copy of the desc pointer */

   memcpy( pdt, oldType, sizeof(dt_desc_t) );
   pdt->desc.desc = temp;
   memcpy( pdt->desc.desc, oldType->desc.desc, sizeof(dt_elem_desc_t) * oldType->desc.used );
   pdt->id  = 0;
   pdt->args = NULL;
   *newType = pdt;
   return 0;
}

int lam_ddt_create_contiguous( size_t count, dt_desc_t* oldType, dt_desc_t** newType )
{
   dt_desc_t* pdt = lam_ddt_create( oldType->desc.used + 2 );
   lam_ddt_add( pdt, oldType, count, 0, (oldType->ub - oldType->lb) );
   *newType = pdt;
   return 0;
}
