/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "lam_config.h"

#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

static int convertor_pack_general( lam_convertor_t* pConvertor, struct iovec* out, unsigned int outCount )
{
   dt_stack_t* pStack;   /* pointer to the position on the stack */
   int pos_desc;         /* actual position in the description of the derived datatype */
   int count_desc;       /* the number of items already done in the actual pos_desc */
   int end_loop;         /* last element in the actual loop */
   int type;             /* type at current position */
   unsigned int advance; /* number of bytes that we should advance the buffer */
   int rc;
   long disp_desc = 0;   /* compute displacement for truncated data */
   long disp;            /* displacement at the beging of the last loop */
   dt_desc_t *pData = pConvertor->pDesc;
   dt_elem_desc_t* pElem;
   char* pOutput = pConvertor->pBaseBuf;
   int oCount = (pData->ub - pData->lb) * pConvertor->count;
   char* pInput = out[0].iov_base;
   int iCount = out[0].iov_len;

   DUMP( "convertor_decode( %p, {%p, %d}, %d )\n", pConvertor,
         out[0].iov_base, out[0].iov_len, outCount );
   pStack = pConvertor->pStack + pConvertor->stack_pos;
   pos_desc  = pStack->index;
   disp = 0;

   if( pData->opt_desc.desc != NULL )    pElem = pData->opt_desc.desc;
   else                                  pElem = pData->desc.desc;

   if( pos_desc == -1 ) {
      pos_desc = 0;
      count_desc = pElem[0].count;
      disp_desc = pElem[0].disp;
   } else {
      count_desc = pStack->count;
      if( pElem[pos_desc].type != DT_LOOP ) {
         pConvertor->stack_pos--;
         pStack--;
         disp = pStack->disp;
         disp_desc = ( pElem[pos_desc].disp +
                       (pElem[pos_desc].count - count_desc) * pElem[pos_desc].extent);
      }
   }
   DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "starting" );
   DUMP( "remember position on stack %d last_elem at %d\n", pConvertor->stack_pos, pos_desc );
   DUMP( "top stack info {index = %d, count = %d}\n", 
         pStack->index, pStack->count );

  next_loop:
   end_loop = pStack->end_loop;
   while( pConvertor->stack_pos >= 0 ) {
      if( pos_desc == end_loop ) { /* end of the current loop */
         while( --(pStack->count) == 0 ) { /* end of loop */
            pConvertor->stack_pos--;
            pStack--;
            if( pConvertor->stack_pos == -1 )
               return 1;  /* completed */
         }
         pos_desc = pStack->index;
         if( pos_desc == -1 )
            pStack->disp += (pData->ub - pData->lb);
         else
            pStack->disp += pElem[pos_desc].extent;
         pos_desc++;
         disp = pStack->disp;
         count_desc = pElem[pos_desc].count;
         disp_desc = pElem[pos_desc].disp;
         goto next_loop;
      }
      if( pElem[pos_desc].type == DT_LOOP ) {
         do {
            PUSH_STACK( pStack, pConvertor->stack_pos,
                        pos_desc, pElem[pos_desc].count,
                        disp, pos_desc + pElem[pos_desc].disp + 1);
            pos_desc++;
         } while( pElem[pos_desc].type == DT_LOOP ); /* let's start another loop */
         DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loops" );
         /* update the current state */
         count_desc = pElem[pos_desc].count;
         disp_desc = pElem[pos_desc].disp;
         goto next_loop;
      }
      /* now here we have a basic datatype */
      type = pElem[pos_desc].type;
      rc = pConvertor->pFunctions[type]( count_desc,
                                         pOutput + disp + disp_desc, oCount, pElem[pos_desc].extent,
                                         pInput, iCount, pElem[pos_desc].extent,
                                         &advance );
      if( rc <= 0 ) {
         printf( "trash in the input buffer\n" );
         return -1;
      }
      iCount -= advance;      /* decrease the available space in the buffer */
      pInput += advance;      /* increase the pointer to the buffer */
      pConvertor->bConverted += advance;
      if( rc != count_desc ) {
         /* not all data has been converted. Keep the state */
         PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc,
                     count_desc - rc,
                     disp + rc * pElem[pos_desc].extent,
                     pos_desc );
         if( iCount != 0 )
            printf( "there is still room in the input buffer %d bytes\n", iCount );
         return 0;
      }
      pConvertor->converted += rc;  /* number of elementd converted so far */
      pos_desc++;  /* advance to the next data */
      count_desc = pElem[pos_desc].count;
      disp_desc = pElem[pos_desc].disp;
      if( iCount == 0 ) break;  /* break if there is no more data in the buffer */
   }

   /* out of the loop: we have complete the data conversion or no more space
    * in the buffer.
    */
   if( pConvertor->pStack[0].count < 0 ) return 1;  /* data succesfully converted */

   /* I complete an element, next step I should go to the next one */
   PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, pElem[pos_desc].count,
               disp, pos_desc );

   return 0;
}

static int convertor_pack_homogeneous( lam_convertor_t* pConv, struct iovec* iov, unsigned int out_size )
{
   dt_stack_t* pStack;   /* pointer to the position on the stack */
   int pos_desc;         /* actual position in the description of the derived datatype */
   int type;             /* type at current position */
   int i;                /* index for basic elements with extent */
   int stack_pos = 0;    /* position on the stack */
   long lastDisp = 0, lastLength = 0;
   char* pDestBuf;
   dt_desc_t* pData = pConv->pDesc;
   dt_elem_desc_t* pElems;

   pDestBuf = iov[0].iov_base;

   if( pData->flags & DT_FLAG_CONTIGUOUS ) {
      long extent = pData->ub - pData->lb;
      char* pSrc = pConv->pBaseBuf + pData->true_lb + pConv->bConverted;

      type = pConv->count * pData->size;
      if( pData->size == extent /* true extent at this point */ ) {
         /* we can do it with just one memcpy */
         MEMCPY( pDestBuf, pSrc, iov[0].iov_len );
         pConv->bConverted += iov[0].iov_len;
      } else {
         char* pSrcBuf = pConv->pBaseBuf + pData->true_lb;
         long extent = pData->ub - pData->lb;
         for( pos_desc = 0; pos_desc < pConv->count; pos_desc++ ) {
            MEMCPY( pDestBuf, pSrcBuf, pData->size );
            pSrcBuf += extent;
            pDestBuf += pData->size;
         }
         pConv->bConverted += type;
      }
      return (pConv->bConverted == (pData->size * pConv->count));
   }
   pStack = pConv->pStack;
   pStack->count = pConv->count;
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
   while( pos_desc <= pStack->end_loop ) {
      if( pos_desc == pStack->end_loop ) { /* end of the current loop */
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
               MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, lastLength + pLast->extent );
               i = 1;
            } else {
               MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, lastLength );
               i = 0;
            }
            pDestBuf += lastLength;
            lastLength = pLast->extent;
            for( ; i < (pElems[pos_desc].count - 1); i++ ) {
               MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, lastLength );
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
         lastLength += pElems[pos_desc].count * basicDatatypes[type].size;
      } else {
         MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, lastLength );
         pDestBuf += lastLength;
         pConv->bConverted += lastLength;
         lastDisp = pStack->disp + pElems[pos_desc].disp;
         lastLength = pElems[pos_desc].count * basicDatatypes[type].size;
      }
      pos_desc++;  /* advance to the next data */
   }

   MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, lastLength );
   pConv->bConverted += lastLength;
   /* cleanup the stack */
   return 0;
}

#define PRINT_MEMCPY( DST, SRC, LENGTH ) \
{ \
  printf( "%5d: memcpy dst = %p src %p length %ld bytes (so far %d)[%d]\n", \
          __index++, (DST), (SRC), (long)(LENGTH), __sofar, __LINE__ ); \
  __sofar += (LENGTH); \
}

int dt_unroll( dt_desc_t* pData, int count )
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
   while( pos_desc <= pStack->end_loop ) {
      if( pos_desc == pStack->end_loop ) { /* end of the current loop */
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
         lastLength += pElems[pos_desc].count * basicDatatypes[type].size;
      } else {
         PRINT_MEMCPY( pDestBuf, (char*)lastDisp, lastLength );
         pDestBuf += lastLength;
         bConverted += lastLength;
         lastDisp = pStack->disp + pElems[pos_desc].disp;
         lastLength = pElems[pos_desc].count * basicDatatypes[type].size;
      }
      pos_desc++;  /* advance to the next data */
   }
   PRINT_MEMCPY( pDestBuf, (char*)lastDisp, lastLength );
   return 0;
}

/* The pack routines should do 2 things:
 * - first if the provided iovec contains NULL pointers then they should provide
 *   buffer space. If the data is contiguous the it should provide directly pointers
 *   the the user space depending on the iov_len argument. If -1 then all the buffer
 *   can be supplied in one time, if not several steps need to be executed, it should
 *   provide the correct pointer every time. But if the user provide a buffer, then
 *   some parts of the data should be packed inside this buffer, but we still should
 *   able to have pointers to the user buf on the subsequents calls.
 * Return 0 if everything went OK and if there is still room before the complete
 *          conversion of the data (need additional call with others input buffers )
 *        1 if everything went fine and the data was completly converted
 *       -1 something wrong occurs.
 */
int lam_convertor_pack( lam_convertor_t* pConv, struct iovec* out, unsigned int out_size )
{
    dt_desc_t* pData = pConv->pDesc;
    int extent;

    if( pConv->count == 0 ) return 1;  /* nothing to do */
    if( pData->flags & DT_FLAG_CONTIGUOUS ) {
        if( pData->size == (extent = (pData->ub - pData->lb)) ) {
            size_t length = pData->size * pConv->count;
            if( out[0].iov_base == NULL ) {
                out[0].iov_base = pConv->pBaseBuf + pData->true_lb;
                if( (out[0].iov_base == 0) ||
                    ((pConv->bConverted + out[0].iov_len) > length) )
                    out[0].iov_len = length - pConv->bConverted;
            } else {
                /* contiguous data just memcpy the smallest data in the user buffer */
                out[0].iov_len = IMIN( out[0].iov_len, pData->size * pConv->count );
                MEMCPY( out[0].iov_base, pConv->pBaseBuf + pData->true_lb, out[0].iov_len);
            }
            pConv->bConverted += out[0].iov_len;
            return 0;
        }         
    }
    if( out[0].iov_base == NULL ) {
        out[0].iov_len = pConv->count * pData->size;
        out[0].iov_base = (void*)malloc( out[0].iov_len );
        pConv->freebuf = out[0].iov_base;
    }
    return lam_convertor_progress( pConv, out, out_size );
}

extern int local_sizes[DT_MAX_PREDEFINED];
int lam_convertor_init_for_send( lam_convertor_t* pConv, unsigned int flags,
                                 dt_desc_t* dt, int count,
                                 void* pUserBuf, int local_starting_point )
{
   OBJ_RETAIN( dt );
   if( pConv->pDesc != dt ) {
       pConv->pDesc = dt;
       pConv->flags = CONVERTOR_SEND;
       if( pConv->pStack != NULL ) free( pConv->pStack );
       pConv->pStack = NULL;
   }
   if( pConv->pStack == NULL ) {
       pConv->pStack = (dt_stack_t*)malloc(sizeof(dt_stack_t) * (dt->btypes[DT_LOOP] + 2) );
       pConv->stack_pos = 0;  /* just to be sure */
   }
   if( local_starting_point == 0 ) {
       pConv->stack_pos = 0;
       pConv->pStack[0].index = -1;         /* fake entry for the first step */
       pConv->pStack[0].count = count;      /* fake entry for the first step */
       pConv->pStack[0].disp  = 0;
       /* first hre we should select which data representation will be used for
        * this operation: normal one or the optimized version ? */
       pConv->pStack[0].end_loop = dt->desc.used;
   } else {
       if( pConv->bConverted != local_starting_point ) {
           lam_create_stack_with_pos( pConv, local_starting_point, local_sizes );
       } /* else we just continue from the previsious point */
   }
   pConv->pBaseBuf = pUserBuf;
   pConv->available_space = count * (dt->ub - dt->lb);
   pConv->count = count;
   pConv->pFunctions = copy_functions;
   pConv->converted = 0;
   pConv->bConverted = 0;
   if( (dt->flags & DT_FLAG_CONTIGUOUS) && (dt->size == (dt->ub - dt->lb)) )
      pConv->flags |= DT_FLAG_CONTIGUOUS;
   pConv->fAdvance = convertor_pack_homogeneous;
   if( pConv->freebuf != NULL ) {
      free( pConv->freebuf );
      pConv->freebuf = NULL;
   }
   return 0;
}

lam_convertor_t* lam_convertor_create( int remote_arch, int mode )
{
   lam_convertor_t* pConv = OBJ_NEW(lam_convertor_t);

   pConv->pStack = NULL;
   pConv->remoteArch = remote_arch;
   pConv->fAdvance = convertor_pack_homogeneous;
   return pConv;
}

static int lam_convertor_destroy( lam_convertor_t* pConv )
{
   if( pConv == NULL ) return 0;
   if( pConv->pStack != NULL ) free( pConv->pStack );
   if( pConv->pDesc != NULL ) OBJ_RELEASE( pConv->pDesc );
   if( pConv->freebuf != NULL ) free( pConv->freebuf );
   return 0;
}

OBJ_CLASS_INSTANCE(lam_convertor_t, lam_object_t, NULL, lam_convertor_destroy );

inline int lam_convertor_copy( lam_convertor_t* pSrcConv, lam_convertor_t* pDestConv )
{
   MEMCPY( pDestConv, pSrcConv, sizeof(lam_convertor_t) );
   pDestConv->pStack     = NULL;
   pDestConv->pDesc      = NULL;
   pDestConv->count      = 0;
   pDestConv->converted  = 0;
   pDestConv->bConverted = 0;
   pDestConv->freebuf    = NULL;
   return LAM_SUCCESS;
}

lam_convertor_t* lam_convertor_get_copy( lam_convertor_t* pConvertor )
{
   lam_convertor_t* pDestConv = OBJ_NEW(lam_convertor_t);
   (void)lam_convertor_copy( pConvertor, pDestConv );
   return pDestConv;
}

/* Actually we suppose that we can only do receiver side conversion */
int lam_convertor_get_packed_size( lam_convertor_t* pConv, int* pSize )
{
   if( lam_ddt_type_size( pConv->pDesc, pSize ) != 0 )
      return -1;
   /* actually *pSize contain the size of one instance of the data */
   *pSize = (*pSize) * pConv->count;
   return 0;
}

int lam_convertor_get_unpacked_size( lam_convertor_t* pConv, int* pSize )
{
   int i;
   dt_desc_t* pData = pConv->pDesc;

   if( pConv->count == 0 ) {
      *pSize = 0;
      return 0;
   }
   if( pConv->remoteArch == 0 ) {  /* same architecture */
      *pSize = pData->size * pConv->count;
      return 0;
   }
   *pSize = 0;
   for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
      if( pData->bdt_used & (1<<i) ) {
         /* TODO replace with the remote size */
         *pSize += (pData->btypes[i] * basicDatatypes[i].size);
      }
   }
   *pSize *= pConv->count;
   return 0;
}
