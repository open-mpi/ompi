/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"

/* We try to merge together data that are contiguous */
int lam_ddt_create_indexed( size_t count, int* pBlockLength, int* pDisp,
                            dt_desc_t* oldType, dt_desc_t** newType )
{
   dt_desc_t* pdt;
   int i, dLength, endat, disp;
   long extent = oldType->ub - oldType->lb;

   pdt = lam_ddt_create( count * (2 + oldType->desc.used) );
   disp = pDisp[0];
   dLength = pBlockLength[0];
   endat = disp + dLength;
   for( i = 1; i < count; i++ ) {
      if( endat == pDisp[i] ) {
         /* contiguous with the previsious */
         dLength += pBlockLength[i];
         endat += pBlockLength[i];
      } else {
         lam_ddt_add( pdt, oldType, dLength, disp * extent, extent );
         disp = pDisp[i];
         dLength = pBlockLength[i];
         endat = disp + pBlockLength[i];
      }
   }
   lam_ddt_add( pdt, oldType, dLength, disp * extent, extent );

   *newType = pdt;
   return 0;
}

int lam_ddt_create_hindexed( size_t count, int* pBlockLength, long* pDisp,
                             dt_desc_t* oldType, dt_desc_t** newType )
{
   dt_desc_t* pdt;
   int i, dLength;
   long extent = oldType->ub - oldType->lb;
   long disp, endat;

   pdt = lam_ddt_create( count * (2 + oldType->desc.used) );
   disp = pDisp[0];
   dLength = pBlockLength[0];
   endat = disp + dLength * extent;
   for( i = 1; i < count; i++ ) {
      if( endat == pDisp[i] ) {
         /* contiguous with the previsious */
         dLength += pBlockLength[i];
         endat += pBlockLength[i] * extent;
      } else {
         lam_ddt_add( pdt, oldType, dLength, disp, extent );
         disp = pDisp[i];
         dLength = pBlockLength[i];
         endat = disp + pBlockLength[i] * extent;
      }
   }
   lam_ddt_add( pdt, oldType, dLength, disp, extent );

   *newType = pdt;
   return 0;
}

int lam_ddt_create_indexed_block( size_t count, int bLength, int* pDisp,
                                  dt_desc_t* oldType, dt_desc_t** newType )
{
   dt_desc_t* pdt;
   int i, dLength, endat, disp;
   long extent = oldType->ub - oldType->lb;

   pdt = lam_ddt_create( count * (2 + oldType->desc.used) );
   disp = pDisp[0];
   dLength = bLength;
   endat = disp + dLength;
   for( i = 1; i < count; i++ ) {
      if( endat == pDisp[i] ) {
         /* contiguous with the previsious */
         dLength += bLength;
         endat += bLength;
      } else {
         lam_ddt_add( pdt, oldType, dLength, disp * extent, extent );
         disp = pDisp[i];
         dLength = bLength;
         endat = disp + bLength;
      }
   }
   lam_ddt_add( pdt, oldType, dLength, disp * extent, extent );

   *newType = pdt;
   return 0;
}
