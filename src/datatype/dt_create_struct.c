/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"

int lam_ddt_create_struct( int count, int* pBlockLength, long* pDisp,
                           dt_desc_t** pTypes, dt_desc_t** newType )
{
   int i;
   long disp, endto, lastExtent, lastDisp;
   int lastBlock;
   dt_desc_t *pdt, *lastType;
   /* if we compute the total number of elements before we can
    * avoid increasing the size of the desc array often.
    */
   for( disp = 0, i = 0; i < count; i++ ) {
      disp += pTypes[i]->desc.used;
      if( pBlockLength[i] != 1 ) disp += 2;
   }
   lastType = pTypes[0];
   lastBlock = pBlockLength[0];
   lastExtent = lastType->ub - lastType->lb;
   lastDisp = pDisp[0];
   endto = pDisp[0] + lastExtent * lastBlock;

   pdt = lam_ddt_create( disp );

   for( i = 1; i < count; i++ ) {
      if( (pTypes[i] == lastType) && (pDisp[i] == endto) ) {
         lastBlock += pBlockLength[i];
         endto = lastDisp + lastBlock * lastExtent;
      } else {
         lam_ddt_add( pdt, lastType, lastBlock, lastDisp, lastExtent );
         lastType = pTypes[i];
         lastExtent = lastType->ub - lastType->lb;
         lastBlock = pBlockLength[i];
         lastDisp = pDisp[i];
         endto = lastDisp + lastExtent * lastBlock;
      }
   }
   lam_ddt_add( pdt, lastType, lastBlock, lastDisp, lastExtent );

   *newType = pdt;
   return 0;
}
