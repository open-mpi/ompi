/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "ompi_config.h"
#include "datatype.h"

/* Open questions ...
 *  - how to improuve the handling of these vectors (creating a temporary datatype
 *    can be ONLY a initial solution.
 *
 */

int ompi_ddt_create_vector( int count, int bLength, long stride,
                           dt_desc_t* oldType, dt_desc_t** newType )
{
   long extent = oldType->ub - oldType->lb;
   dt_desc_t *pTempData, *pData;

   if( bLength == stride ) {
      /* the elements are contiguous */
      pData = ompi_ddt_create( oldType->desc.used + 2 );
      ompi_ddt_add( pData, oldType, count * bLength, 0, extent );
   } else {
      if( count > 1 ) {
         if( bLength == 1 ) {
            pData = ompi_ddt_create( oldType->desc.used + 2 );
            ompi_ddt_add( pData, oldType, count - 1, 0, stride * extent );
         } else {
            pTempData = ompi_ddt_create( oldType->desc.used + 2 );
            pData = ompi_ddt_create( oldType->desc.used + 2 + 2 );
            ompi_ddt_add( pTempData, oldType, bLength, 0, extent );
            ompi_ddt_add( pData, pTempData, count - 1, 0, stride * extent );
            OBJ_RELEASE( pTempData );
         }
      } else {
         pData = ompi_ddt_create( oldType->desc.used + 2 );
      }
      ompi_ddt_add( pData, oldType, bLength, (count - 1) * extent * stride, extent );
   }
   *newType = pData;
   return OMPI_SUCCESS;
}

int ompi_ddt_create_hvector( int count, int bLength, long stride,
                            dt_desc_t* oldType, dt_desc_t** newType )
{
   long extent = oldType->ub - oldType->lb;
   dt_desc_t *pTempData, *pData;

   if( (extent * bLength) == stride ) {
      /* contiguous */
      pData = ompi_ddt_create( oldType->desc.used + 2 );
      ompi_ddt_add( pData, oldType, count * bLength, 0, extent );
   } else {
      if( count > 1 ) {
         pTempData = ompi_ddt_create( oldType->desc.used + 2 );
         pData = ompi_ddt_create( oldType->desc.used + 2 + 2 );
         ompi_ddt_add( pTempData, oldType, bLength, 0, extent );
         ompi_ddt_add( pData, pTempData, count - 1, 0, stride );
         OBJ_RELEASE( pTempData );
      } else {
         pData = ompi_ddt_create( oldType->desc.used + 2 );
      }
      ompi_ddt_add( pData, oldType, bLength, (count - 1) * stride, extent );
   }
   *newType = pData;
   return OMPI_SUCCESS;
}
