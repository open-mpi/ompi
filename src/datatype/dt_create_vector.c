#include "datatype.h"

/* Open questions ...
 *  - how to improuve the handling of these vectors (creating a temporary datatype
 *    can be ONLY a initial solution.
 *
 */

int dt_create_vector( size_t count, int bLength, long stride,
                      dt_desc_t* oldType, dt_desc_t** newType )
{
   long extent = oldType->ub - oldType->lb;
   dt_desc_t *pTempData, *pData;

   if( bLength == stride ) {
      /* the elements are contiguous */
      pData = dt_create( oldType->desc.used + 2 );
      dt_add( pData, oldType, count * bLength, 0, extent );
   } else {
      if( count > 1 ) {
         if( bLength == 1 ) {
            pData = dt_create( oldType->desc.used + 2 );
            dt_add( pData, oldType, count - 1, 0, stride * extent );
         } else {
            pTempData = dt_create( oldType->desc.used + 2 );
            pData = dt_create( oldType->desc.used + 2 + 2 );
            dt_add( pTempData, oldType, bLength, 0, extent );
            dt_add( pData, pTempData, count - 1, 0, stride * extent );
            dt_free( &pTempData );
         }
      } else {
         pData = dt_create( oldType->desc.used + 2 );
      }
      dt_add( pData, oldType, bLength, (count - 1) * extent * stride, extent );
   }
   *newType = pData;
   return 0;
}

int dt_create_hvector( size_t count, int bLength, long stride,
                       dt_desc_t* oldType, dt_desc_t** newType )
{
   long extent = oldType->ub - oldType->lb;
   dt_desc_t *pTempData, *pData;

   if( (extent * bLength) == stride ) {
      /* contiguous */
      pData = dt_create( oldType->desc.used + 2 );
      dt_add( pData, oldType, count * bLength, 0, extent );
   } else {
      if( count > 1 ) {
         pTempData = dt_create( oldType->desc.used + 2 );
         pData = dt_create( oldType->desc.used + 2 + 2 );
         dt_add( pTempData, oldType, bLength, 0, extent );
         dt_add( pData, pTempData, count - 1, 0, stride );
         dt_free( &pTempData );
      } else {
         pData = dt_create( oldType->desc.used + 2 );
      }
      dt_add( pData, oldType, bLength, (count - 1) * stride, extent );
   }
   *newType = pData;
   return 0;
}
