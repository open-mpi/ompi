/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "datatype.h"
#include "datatype_internal.h"

/*   printf( "save in %s:%d at %p DT_BYTE disp %ld count %d\n", __FILE__, __LINE__, (PELEM), (DISP), (COUNT) ); \ */
#define SAVE_DESC( PELEM, DISP, COUNT ) \
do { \
  (PELEM)->flags  = DT_FLAG_BASIC; \
  (PELEM)->type   = DT_BYTE; \
  (PELEM)->count  = (COUNT); \
  (PELEM)->disp   = (DISP); \
  (PELEM)->extent = 1; \
  (PELEM)++; \
  nbElems++; \
} while(0)

/*   printf( "save in %s:%d type %d flags %x count %d disp %ld extent %d\n", \ */
/*           __FILE__, __LINE__, (TYPE), (FLAGS), (COUNT), (DISP), (EXTENT) ); \ */
#define SAVE_ELEM( PELEM, TYPE, FLAGS, COUNT, DISP, EXTENT ) \
do { \
  (PELEM)->flags  = (FLAGS); \
  (PELEM)->type   = (TYPE); \
  (PELEM)->count  = (COUNT); \
  (PELEM)->disp   = (DISP); \
  (PELEM)->extent = (EXTENT); \
  (PELEM)++; \
  nbElems++; \
} while(0)

static inline long GET_LOOP_DISP( dt_elem_desc_t* _pElem )
{
   while( _pElem->type == DT_LOOP ) ++_pElem;
   return _pElem->disp;
}

int lam_ddt_optimize_short( dt_desc_t* pData, int count, dt_type_desc_t* pTypeDesc )
{
   dt_elem_desc_t* pElemDesc;
   long lastDisp = 0;
   dt_stack_t* pStack;   /* pointer to the position on the stack */
   int pos_desc;         /* actual position in the description of the derived datatype */
   int end_loop;         /* last element in the actual loop */
   int stack_pos = 0;
   int type, lastLength = 0, nbElems = 0, changes = 0;
   long totalDisp;

   pTypeDesc->length = 2 * pData->desc.used;
   pTypeDesc->desc = pElemDesc = (dt_elem_desc_t*)malloc( sizeof(dt_elem_desc_t) * pTypeDesc->length );

   pStack = alloca( sizeof(dt_stack_t) * (pData->btypes[DT_LOOP]+1) );
   pStack->count = count;
   pStack->index = -1;
   pStack->end_loop = pData->desc.used - 1;
   pStack->disp = 0;
   pos_desc  = 0;
   
  next_loop:
   end_loop = pStack->end_loop;
   totalDisp = pStack->disp;
   while( pos_desc <= end_loop ) {
      if( pData->desc.desc[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
         dt_elem_desc_t* pStartLoop;
         if( lastLength != 0 ) {
            SAVE_DESC( pElemDesc, lastDisp, lastLength );
            lastDisp += lastLength;
            lastLength = 0;
         }
         pStartLoop = (pElemDesc - nbElems);
         SAVE_ELEM( pElemDesc, DT_END_LOOP, pData->desc.desc[pos_desc].flags,
                    nbElems, pData->desc.desc[pos_desc].disp,
                    pData->desc.desc[pos_desc].extent );
         nbElems += pStartLoop->disp;
         pStartLoop->disp = (pElemDesc - 1)->count;
         stack_pos--;
         pStack--;
         
         pos_desc++;
         goto next_loop;
      }
      if( pData->desc.desc[pos_desc].type == DT_LOOP ) {
         dt_elem_desc_t* pEndLoop = &(pData->desc.desc[pos_desc + pData->desc.desc[pos_desc].disp]);
         long loop_disp = GET_LOOP_DISP( &(pData->desc.desc[pos_desc]) );
         if( pData->desc.desc[pos_desc].flags & DT_FLAG_CONTIGUOUS ) {
            /* the loop is contiguous or composed by contiguous elements with a gap */
            if( pData->desc.desc[pos_desc].extent == pEndLoop->extent ) {
               /* the whole loop is contiguous */
               if( (lastDisp + lastLength) != (totalDisp + loop_disp) ) {
                  SAVE_DESC( pElemDesc, lastDisp, lastLength );
                  lastLength = 0;
                  lastDisp = totalDisp + loop_disp;
               }
               lastLength += pData->desc.desc[pos_desc].count * pEndLoop->extent;
            } else {
               int counter = pData->desc.desc[pos_desc].count;
               if( (lastDisp + lastLength) == (totalDisp + loop_disp) ) {
                  lastLength += pEndLoop->extent;
                  counter--;
               }
               if( lastLength != 0 ) {
                  SAVE_DESC( pElemDesc, lastDisp, lastLength );
                  lastDisp += lastLength;
                  lastLength = 0;
               }                  
               /* we have a gap in the begining or the end of the loop but the whole
                * loop can be merged in just one memcpy.
                */
               SAVE_ELEM( pElemDesc, DT_LOOP, pData->desc.desc[pos_desc].flags,
                          counter, (long)2, pData->desc.desc[pos_desc].extent );
               SAVE_DESC( pElemDesc, loop_disp, pEndLoop->extent );
               SAVE_ELEM( pElemDesc, DT_END_LOOP, pEndLoop->flags,
                          2, pEndLoop->disp, pEndLoop->extent );
            }
            pos_desc += pData->desc.desc[pos_desc].disp + 1;
            changes++;
         } else {
            if( lastLength != 0 ) {
               SAVE_DESC( pElemDesc, lastDisp, lastLength );
               lastDisp += lastLength;
               lastLength = 0;
            }                  
            SAVE_ELEM( pElemDesc, DT_LOOP, pData->desc.desc[pos_desc].flags,
                       pData->desc.desc[pos_desc].count, (long)nbElems,
                       pData->desc.desc[pos_desc].extent );
            nbElems = 1;
            PUSH_STACK( pStack, stack_pos, pos_desc, pData->desc.desc[pos_desc].count,
                        totalDisp, pos_desc + pData->desc.desc[pos_desc].disp );
            pos_desc++;
            DUMP_STACK( pStack, stack_pos, pData->desc, "advance loops" );
         }
         goto next_loop;
      }
      /* now here we have a basic datatype */
      type = pData->desc.desc[pos_desc].type;
      if( (lastDisp + lastLength) == (totalDisp + pData->desc.desc[pos_desc].disp) ) {
         lastLength += pData->desc.desc[pos_desc].count * basicDatatypes[type].size;
      } else {
         if( lastLength != 0 )
            SAVE_DESC( pElemDesc, lastDisp, lastLength );
         lastDisp = totalDisp + pData->desc.desc[pos_desc].disp;
         lastLength = pData->desc.desc[pos_desc].count * basicDatatypes[type].size;
      }
      pos_desc++;  /* advance to the next data */
   }

   if( lastLength != 0 )
      SAVE_DESC( pElemDesc, lastDisp, lastLength );
   /* cleanup the stack */
   pTypeDesc->used = nbElems;
   return 0;
}
