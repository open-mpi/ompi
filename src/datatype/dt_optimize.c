/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "ompi_config.h"

#include "datatype.h"
#include "datatype_internal.h"
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

/*   printf( "save in %s:%d at %p DT_BYTE disp %ld count %d\n", __FILE__, __LINE__, (PELEM), (DISP), (COUNT) ); \ */
#define SAVE_DESC( PELEM, DISP, COUNT ) \
do { \
  (PELEM)->flags  = DT_FLAG_BASIC | DT_FLAG_DATA; \
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

int ompi_ddt_optimize_short( dt_desc_t* pData, int count, 
			     dt_type_desc_t* pTypeDesc )
{
   dt_elem_desc_t* pElemDesc;
   long lastDisp = 0;
   dt_stack_t* pStack;   /* pointer to the position on the stack */
   int pos_desc;         /* actual position in the description of the derived datatype */
   int stack_pos = 0;
   int type, lastLength = 0, nbElems = 0, changes = 0;
   long totalDisp;

   pTypeDesc->length = 2 * pData->desc.used + 1 /* for the fake DT_END_LOOP at the end */;
   pTypeDesc->desc = pElemDesc = (dt_elem_desc_t*)malloc( sizeof(dt_elem_desc_t) * pTypeDesc->length );
   pTypeDesc->used = 0;

   if( (count == 0) || (pData->desc.used == 0) ) return 1;

   pStack = alloca( sizeof(dt_stack_t) * (pData->btypes[DT_LOOP]+1) );
   pStack->count = count;
   pStack->index = -1;
   pStack->end_loop = pData->desc.used;
   pStack->disp = 0;
   pos_desc  = 0;
   
  next_loop:
   totalDisp = pStack->disp;
   while( stack_pos >= 0 ) {
      if( pData->desc.desc[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
         dt_elem_desc_t* pStartLoop;
         if( lastLength != 0 ) {
            SAVE_DESC( pElemDesc, lastDisp, lastLength );
            lastDisp += lastLength;
            lastLength = 0;
         }
         pStartLoop = &(pTypeDesc->desc[pStack->index - 1]);
         SAVE_ELEM( pElemDesc, DT_END_LOOP, pData->desc.desc[pos_desc].flags,
                    nbElems - pStack->index + 1,  /* # of elems in this loop */
                    pData->desc.desc[pos_desc].disp,
                    pData->desc.desc[pos_desc].extent );
         stack_pos--;
         pStack--;
         if( stack_pos >= 0 ) pStartLoop->disp = (pElemDesc - 1)->count;
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
            PUSH_STACK( pStack, stack_pos, nbElems, pData->desc.desc[pos_desc].count,
                        totalDisp, pos_desc + pData->desc.desc[pos_desc].disp );
            pos_desc++;
            DUMP_STACK( pStack, stack_pos, pData->desc, "advance loops" );
         }
         goto next_loop;
      }
      /* now here we have a basic datatype */
      type = pData->desc.desc[pos_desc].type;
      if( (lastDisp + lastLength) == (totalDisp + pData->desc.desc[pos_desc].disp) ) {
         lastLength += pData->desc.desc[pos_desc].count * basicDatatypes[type]->size;
      } else {
         if( lastLength != 0 )
            SAVE_DESC( pElemDesc, lastDisp, lastLength );
         lastDisp = totalDisp + pData->desc.desc[pos_desc].disp;
         lastLength = pData->desc.desc[pos_desc].count * basicDatatypes[type]->size;
      }
      pos_desc++;  /* advance to the next data */
   }

   if( lastLength != 0 )
      SAVE_DESC( pElemDesc, lastDisp, lastLength );
   /* cleanup the stack */
   pTypeDesc->used = nbElems - 1;  /* except the last fake END_LOOP */
   return OMPI_SUCCESS;
}

#define PRINT_MEMCPY( DST, SRC, LENGTH ) \
{ \
  printf( "%5d: memcpy dst = %p src %p length %ld bytes (so far %d)[%d]\n", \
          __index++, (DST), (SRC), (long)(LENGTH), __sofar, __LINE__ ); \
  __sofar += (LENGTH); \
}

static int ompi_ddt_unroll( dt_desc_t* pData, int count )
{
   dt_stack_t* pStack;   /* pointer to the position on the stack */
   int pos_desc;         /* actual position in the description of the derived datatype */
   int type;             /* type at current position */
   int i;                /* index for basic elements with extent */
   int stack_pos = 0;    /* position on the stack */
   long lastDisp = 0, lastLength = 0;
   char* pDestBuf;
   int bConverted = 0, __index = 0, __sofar = 0;
   dt_elem_desc_t* pElems;

   pDestBuf = NULL;

   if( pData->flags & DT_FLAG_CONTIGUOUS ) {
      long extent = pData->ub - pData->lb;
      char* pSrc = (char*)pData->true_lb;

      type = count * pData->size;
      if( pData->size == extent /* true extent at this point */ ) {
         /* we can do it with just one memcpy */
         PRINT_MEMCPY( pDestBuf, pSrc, pData->size * count );
         bConverted += (pData->size * count);
      } else {
         char* pSrcBuf = (char*)pData->true_lb;
         long extent = pData->ub - pData->lb;
         for( pos_desc = 0; pos_desc < count; pos_desc++ ) {
            PRINT_MEMCPY( pDestBuf, pSrcBuf, pData->size );
            pSrcBuf += extent;
            pDestBuf += pData->size;
         }
         bConverted += type;
      }
      return (bConverted == (pData->size * count));
   }
   pStack = alloca( sizeof(dt_stack_t) * pData->btypes[DT_LOOP] );
   pStack->count = count;
   pStack->index = -1;
   pStack->disp = 0;
   pos_desc  = 0;

   if( pData->opt_desc.desc != NULL ) {
      pElems = pData->opt_desc.desc;
      pStack->end_loop = pData->opt_desc.used;
   } else {
      pElems = pData->desc.desc;
      pStack->end_loop = pData->desc.used;
   }

   DUMP_STACK( pStack, stack_pos, pElems, "starting" );
   DUMP( "remember position on stack %d last_elem at %d\n", stack_pos, pos_desc );
   DUMP( "top stack info {index = %d, count = %d}\n", 
         pStack->index, pStack->count );
  next_loop:
   while( pos_desc >= 0 ) {
      if( pElems[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
         if( --(pStack->count) == 0 ) { /* end of loop */
            pStack--;
            if( --stack_pos == -1 ) break;
         } else {
            pos_desc = pStack->index;
            if( pos_desc == -1 )
               pStack->disp += (pData->ub - pData->lb);
            else
               pStack->disp += pElems[pos_desc].extent;
         }
         pos_desc++;
         goto next_loop;
      }
      if( pElems[pos_desc].type == DT_LOOP ) {
         if( pElems[pos_desc].flags & DT_FLAG_CONTIGUOUS ) {
            dt_elem_desc_t* pLast = &( pElems[pos_desc + pElems[pos_desc].disp]);
            if( (lastDisp + lastLength) == (pStack->disp + pElems[pos_desc+1].disp) ) {
               PRINT_MEMCPY( pDestBuf, (char*)lastDisp, lastLength + pLast->extent );
               lastDisp = pStack->disp + pElems[pos_desc+1].disp + pLast->extent;
               i = 1;
            } else {
               PRINT_MEMCPY( pDestBuf, (char*)lastDisp, lastLength );
               lastDisp = pStack->disp + pElems[pos_desc + 1].disp;
               i = 0;
            }
            lastLength = pLast->extent;
            for( ; i < (pElems[pos_desc].count - 1); i++ ) {
               PRINT_MEMCPY( pDestBuf, (char*)lastDisp, lastLength );
               pDestBuf += pLast->extent;
               lastDisp += pElems[pos_desc].extent;
            }
            pos_desc += pElems[pos_desc].disp + 1;
            goto next_loop;
         } else {
            do {
               PUSH_STACK( pStack, stack_pos, pos_desc, pElems[pos_desc].count,
                           pStack->disp, pos_desc + pElems[pos_desc].disp );
               pos_desc++;
            } while( pElems[pos_desc].type == DT_LOOP ); /* let's start another loop */
         }
      }
      /* now here we have a basic datatype */
      type = pElems[pos_desc].type;
      if( (lastDisp + lastLength) == (pStack->disp + pElems[pos_desc].disp) ) {
         lastLength += pElems[pos_desc].count * basicDatatypes[type]->size;
      } else {
         PRINT_MEMCPY( pDestBuf, (char*)lastDisp, lastLength );
         pDestBuf += lastLength;
         bConverted += lastLength;
         lastDisp = pStack->disp + pElems[pos_desc].disp;
         lastLength = pElems[pos_desc].count * basicDatatypes[type]->size;
      }
      pos_desc++;  /* advance to the next data */
   }
   PRINT_MEMCPY( pDestBuf, (char*)lastDisp, lastLength );
   return OMPI_SUCCESS;
}

int ompi_ddt_commit( dt_desc_t** data )
{
    dt_desc_t* pData = (dt_desc_t*)*data;
    dt_elem_desc_t* pLast = &(pData->desc.desc[pData->desc.used]);

    if( pData->flags & DT_FLAG_COMMITED ) return OMPI_SUCCESS;
    pData->flags |= DT_FLAG_COMMITED;

    /* let's add a fake element at the end just to avoid useless comparaisons
     * in pack/unpack functions.
     */
    pLast->type   = DT_END_LOOP;
    pLast->flags  = 0;
    pLast->count  = pData->desc.used;
    pLast->disp   = pData->ub - pData->lb;
    pLast->extent = pData->size;

    /* If the data is contiguous is useless to generate an optimized version. */
    if( pData->size != (pData->true_ub - pData->true_lb) ) {
        (void)ompi_ddt_optimize_short( pData, 1, &(pData->opt_desc) );
        /* let's add a fake element at the end just to avoid useless comparaisons
         * in pack/unpack functions.
         */
        pLast = &(pData->opt_desc.desc[pData->opt_desc.used]);
        pLast->type   = DT_END_LOOP;
        pLast->flags  = 0;
        pLast->count  = pData->opt_desc.used;
        pLast->disp   = pData->ub - pData->lb;
        pLast->extent = pData->size;
    }
    return OMPI_SUCCESS;
}
