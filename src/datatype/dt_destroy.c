/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

/* This function should never be called directly. It's called by the dt_decrease_ref
 * when the number of references on the data reach ZERO.
 */
int lam_ddt_destroy( dt_desc_t** dt )
{
   dt_desc_t* pData = *dt;

   if( !(pData->flags & DT_FLAG_FOREVER) )
      return LAM_ERROR;

   /* I still have the data description ? */
   if( pData->args != NULL ) {
      fprintf( stderr, "Data description has not been removed prior to data destruction" );
   }

   if( pData->opt_desc.desc != NULL ) free( pData->opt_desc.desc );
   if( pData->desc.desc != NULL ) free( pData->desc.desc );
   return 0;
}
