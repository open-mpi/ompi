/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"

/* Open questions ...
 *  - how to improuve the handling of these vectors (creating a temporary datatype
 *    can be ONLY a initial solution.
 *
 */

int lam_ddt_create_vector( int count, int bLength, long stride,
                           dt_desc_t* oldType, dt_desc_t** newType )
{
   long extent = oldType->ub - oldType->lb;
   dt_desc_t *pTempData, *pData;

   if( bLength == stride ) {
      /* the elements are contiguous */
      pData = lam_ddt_create( oldType->desc.used + 2 );
      lam_ddt_add( pData, oldType, count * bLength, 0, extent );
   } else {
      if( count > 1 ) {
         if( bLength == 1 ) {
            pData = lam_ddt_create( oldType->desc.used + 2 );
            lam_ddt_add( pData, oldType, count - 1, 0, stride * extent );
         } else {
            pTempData = lam_ddt_create( oldType->desc.used + 2 );
            pData = lam_ddt_create( oldType->desc.used + 2 + 2 );
            lam_ddt_add( pTempData, oldType, bLength, 0, extent );
            lam_ddt_add( pData, pTempData, count - 1, 0, stride * extent );
            OBJ_RELEASE( pTempData );
         }
      } else {
         pData = lam_ddt_create( oldType->desc.used + 2 );
      }
      lam_ddt_add( pData, oldType, bLength, (count - 1) * extent * stride, extent );
   }
   *newType = pData;
   return 0;
}

int lam_ddt_create_hvector( int count, int bLength, long stride,
                            dt_desc_t* oldType, dt_desc_t** newType )
{
   long extent = oldType->ub - oldType->lb;
   dt_desc_t *pTempData, *pData;

   if( (extent * bLength) == stride ) {
      /* contiguous */
      pData = lam_ddt_create( oldType->desc.used + 2 );
      lam_ddt_add( pData, oldType, count * bLength, 0, extent );
   } else {
      if( count > 1 ) {
         pTempData = lam_ddt_create( oldType->desc.used + 2 );
         pData = lam_ddt_create( oldType->desc.used + 2 + 2 );
         lam_ddt_add( pTempData, oldType, bLength, 0, extent );
         lam_ddt_add( pData, pTempData, count - 1, 0, stride );
         OBJ_RELEASE( pTempData );
      } else {
         pData = lam_ddt_create( oldType->desc.used + 2 );
      }
      lam_ddt_add( pData, oldType, bLength, (count - 1) * stride, extent );
   }
   *newType = pData;
   return 0;
}
