/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

/* When we add a datatype we should update it's definition depending on
 * the initial displacement for the whole data, so the displacement of
 * all elements inside a datatype depend only on the loop displacement
 * and it's own displacement.
 */

/* we have 3 differents structures to update:
 * the first is the real representation of the datatype
 * the second is the internal representation using extents
 * the last is the representation used for send operations
 */
int lam_ddt_add( dt_desc_t* pdtBase, dt_desc_t* pdtAdd, unsigned int count, long disp, long extent )
{
   int newLength, place_needed = 0, i;
   short localFlags;
   dt_elem_desc_t *pLast, *pLoop = NULL;
   long lb, ub;

   /* the extent should be always be positive. So a negative
    * value here have a special meaning ie. default extent as
    * computed by ub - lb
    */
   if( extent == -1 ) extent = (pdtAdd->ub - pdtAdd->lb);

   /* first make sure that we have enought place to
    * put the new element inside */
   if( (pdtAdd->flags & DT_FLAG_BASIC) == DT_FLAG_BASIC ) {
      place_needed = 1;
      /* handle special cases for DT_LB and DT_UB */
      if( pdtAdd == &(basicDatatypes[DT_LB]) ) {
         pdtBase->bdt_used |= (1<< DT_LB);
         if( pdtBase->flags & DT_FLAG_USER_LB ) {
            pdtBase->lb = LMIN( pdtBase->lb, disp );
         } else {
            pdtBase->lb = disp;
            pdtBase->flags |= DT_FLAG_USER_LB;
         }
         return 0;
      } else if( pdtAdd == &(basicDatatypes[DT_UB]) ) {
         pdtBase->bdt_used |= (1<< DT_UB);
         if( pdtBase->flags & DT_FLAG_USER_UB ) {
            pdtBase->ub = LMAX( pdtBase->ub, disp );
         } else {
            pdtBase->ub = disp;
            pdtBase->flags |= DT_FLAG_USER_UB;
         }
         return 0;
      }
   } else {
      place_needed = pdtAdd->desc.used;
      if( count != 1 ) place_needed += 2;
   }

   OBJ_RETAIN( pdtAdd );
      
   /* compute the new memory alignement */
   pdtBase->align = IMAX( pdtBase->align, pdtAdd->align );

   pdtBase->bdt_used |= pdtAdd->bdt_used;
   newLength = pdtBase->desc.used + place_needed;
   if( newLength > pdtBase->desc.length ) {
      newLength = ((newLength / DT_INCREASE_STACK) + 1 ) * DT_INCREASE_STACK;
      printf( "increase the size of the data desc array from %d to %d (old ptr = %p ",
              pdtBase->desc.length, newLength, (void*)pdtBase->desc.desc );
      pdtBase->desc.desc   = (dt_elem_desc_t*)realloc( pdtBase->desc.desc, newLength );
      printf( "new ptr = %p\n", (void*)pdtBase->desc.desc );
      pdtBase->desc.length = newLength;
   }
   pLast = &(pdtBase->desc.desc[pdtBase->desc.used]);
   if( (pdtAdd->flags & DT_FLAG_BASIC) == DT_FLAG_BASIC ) { /* add a basic datatype */
      pLast->type   = pdtAdd->id;
      pLast->count  = count;
      pLast->disp   = disp;
      pLast->extent = extent;
      pdtBase->desc.used++;
      pdtBase->btypes[pdtAdd->id] += count;
      pLast->flags  = pdtAdd->flags ^ (DT_FLAG_FOREVER | DT_FLAG_COMMITED);
      if( extent != pdtAdd->size )
         pLast->flags |= DT_FLAG_CONTIGUOUS;
   } else {
      /* now we add a complex datatype */
      if( disp != pdtBase->ub ) { /* add the initial gap */
         if( disp < pdtBase->ub ) pdtBase->flags |= DT_FLAG_OVERLAP;
      }
      /* keep trace of the total number of basic datatypes in the datatype definition */
      pdtBase->btypes[DT_LOOP] += pdtAdd->btypes[DT_LOOP];
      for( i = 3; i < DT_END_LOOP; i++ )
         if( pdtAdd->btypes[i] != 0 ) pdtBase->btypes[i] += (count * pdtAdd->btypes[i]);
      pdtBase->btypes[DT_END_LOOP] += pdtAdd->btypes[DT_END_LOOP];

      /* if the extent of the datatype if the same as the extent of the loop
       * description of the datatype then we simply have to update the main loop.
       */
      if( count != 1 ) {
         pLoop = pLast;
         pLast->type   = DT_LOOP;
         pLast->count  = count;
         pLast->disp   = (long)pdtAdd->desc.used + 1;
         pLast->extent = extent;
         pLast->flags  = (pdtAdd->flags & ~(DT_FLAG_COMMITED | DT_FLAG_FOREVER));
         localFlags = DT_FLAG_IN_LOOP;
         pdtBase->btypes[DT_LOOP] += 2;
         pdtBase->desc.used += 2;
         pLast++;
      }

      for( i = 0; i < pdtAdd->desc.used; i++ ) {
         pLast->type   = pdtAdd->desc.desc[i].type;
         pLast->flags  = pdtAdd->desc.desc[i].flags | localFlags;
         pLast->count  = pdtAdd->desc.desc[i].count;
         pLast->extent = pdtAdd->desc.desc[i].extent;
         pLast->disp   = pdtAdd->desc.desc[i].disp;
         if( pdtAdd->desc.desc[i].type != DT_LOOP )
            pLast->disp += disp/*  + pdtAdd->lb */;
         pLast++;
      }
      pdtBase->desc.used += pdtAdd->desc.used;
      if( pLoop != NULL ) {
         pLast->type = DT_END_LOOP;
         pLast->count = pdtAdd->desc.used + 1;   /* where the loop start */
         pLast->disp = disp + (count - 1) * extent
            + (pdtAdd->true_ub - pdtAdd->true_lb) ;  /* the final extent for the loop */
         pLast->extent = pdtAdd->size;        /* the size of the data inside the loop */
         pLast->flags = pLoop->flags;
      }
      /* should I add some space until the extent of this datatype ? */
   }

   /* let's add a fake element at the end just to avoid useless comparaisons
    * in pack/unpack functions.
    */
   pLast++;
   pLast->type = 0;
   pLast->flags = 0;

   pdtBase->size += count * pdtAdd->size;
   pdtBase->true_lb = LMIN( pdtBase->true_lb, pdtAdd->true_lb + disp );
   pdtBase->true_ub = LMAX( pdtBase->true_ub,
                            disp + pdtAdd->true_lb + 
                            (count - 1) * extent + pdtAdd->true_ub );

   /* the lower bound should be inherited from the parents if and only
    * if the USER has explicitly set it. The result lb is the MIN between
    * the all lb + disp if and only if all or nobody flags's contain the LB.
    */
   if( (pdtAdd->flags ^ pdtBase->flags) & DT_FLAG_USER_LB ) {
      pdtBase->flags |= DT_FLAG_USER_LB;
      if( pdtAdd->flags & DT_FLAG_USER_LB )
         lb = pdtAdd->lb + disp;
      else
         lb = pdtBase->lb;
   } else {
      lb = LMIN( pdtBase->lb, pdtAdd->lb + disp );
   }

   /* the same apply for the upper bound except for the case where
    * either of them has the flag UB, in which case we should
    * compute the UB including the natural alignement of the data.
    */
   if( (pdtBase->flags ^ pdtAdd->flags) & DT_FLAG_USER_UB ) {
      if( pdtBase->flags & DT_FLAG_USER_UB )
         ub = pdtBase->ub;
      else {
         pdtBase->flags |= DT_FLAG_USER_UB;
         ub = disp + pdtAdd->lb + count * extent;
      }
   } else {
      if( pdtBase->flags & DT_FLAG_USER_UB )
         ub = LMAX( pdtBase->ub, disp + pdtAdd->lb + count * (extent) );
      else {
         /* we should compute the extent depending on the alignement */
         long ubN = (disp + pdtAdd->lb + count * (extent));
         ub = LMAX( ((pdtBase->ub / pdtBase->align) * pdtBase->align),
                    (((ubN + pdtBase->align - 1)/ pdtBase->align) * pdtBase->align) );
      }
   }
   /* update the extent and size */
   pdtBase->lb = lb;
   pdtBase->ub = ub;
   pdtBase->nbElems += (count * pdtAdd->nbElems);

   /* Is the data still contiguous ?
    * The only way for the data to be contiguous is to have the true extent equal to his size.
    * In other words to avoid having internal gaps between elements.
    */
   if( (pdtBase->size != (pdtBase->true_ub - pdtBase->true_lb)) ||
       !(pdtBase->flags & DT_FLAG_CONTIGUOUS) || !(pdtAdd->flags & DT_FLAG_CONTIGUOUS) )
      UNSET_CONTIGUOUS_FLAG(pdtBase->flags);

   OBJ_RELEASE( pdtAdd );

   return 0;
}
