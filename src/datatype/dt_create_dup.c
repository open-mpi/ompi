#include "datatype.h"

int dt_duplicate( dt_desc_t* oldType, dt_desc_t** newType )
{
   dt_desc_t* pdt = dt_create( oldType->desc.used );
   void* temp = pdt->desc.desc; /* temporary copy of the desc pointer */

   memcpy( pdt, oldType, sizeof(dt_desc_t) );
   pdt->desc.desc = temp;
   memcpy( pdt->desc.desc, oldType->desc.desc, sizeof(dt_elem_desc_t) * oldType->desc.used );
   pdt->id  = 0;
   pdt->args = NULL;
   *newType = pdt;
   return 0;
}

int dt_create_contiguous( size_t count, dt_desc_t* oldType, dt_desc_t** newType )
{
   dt_desc_t* pdt = dt_create( oldType->desc.used + 2 );
   dt_add( pdt, oldType, count, 0, (oldType->ub - oldType->lb) );
   *newType = pdt;
   return 0;
}
