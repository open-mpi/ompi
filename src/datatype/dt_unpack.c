/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "ompi_config.h"
#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

void dump_stack( dt_stack_t* pStack, int stack_pos, dt_elem_desc_t* pDesc, char* name )
{
    printf( "\nStack %p stack_pos %d %s\n", (void*)pStack, stack_pos, name );
    for( ;stack_pos >= 0; stack_pos-- ) {
        printf( "%d: pos %d count %d disp %ld ", stack_pos, pStack->index,
                pStack->count, pStack->disp );
        if( pStack[stack_pos].index != -1 )
            printf( "[desc count %d disp %ld extent %d]\n",
                    pDesc[pStack->index].count,
                    pDesc[pStack->index].disp,
                    pDesc[pStack->index].extent );
        else
            printf( "\n" );
        pStack--;
    }
    printf( "\n" );
}

/*
 *  Remember that the first item in the stack (ie. position 0) is the number
 * of times the datatype is involved in the operation (ie. the count argument
 * in the MPI_ call).
 */
/* Convert data from multiple input buffers (as received from the network layer)
 * to a contiguous output buffer with a predefined size.
 * Return 0 if everything went OK and if there is still room before the complete
 *          conversion of the data (need additional call with others input buffers )
 *        1 if everything went fine and the data was completly converted
 *       -1 something wrong occurs.
 */
static int ompi_convertor_unpack_general( ompi_convertor_t* pConvertor,
                                         struct iovec* pInputv, unsigned int inputCount )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int count_desc;       /* the number of items already done in the actual pos_desc */
    int type;             /* type at current position */
    unsigned int advance; /* number of bytes that we should advance the buffer */
    int rc;
    long disp_desc = 0;   /* compute displacement for truncated data */
    long disp;            /* displacement at the beging of the last loop */
    dt_desc_t *pData = pConvertor->pDesc;
    dt_elem_desc_t* pElems;
    char* pOutput = pConvertor->pBaseBuf;
    int oCount = (pData->ub - pData->lb) * pConvertor->count;
    char* pInput = pInputv[0].iov_base;
    int iCount = pInputv[0].iov_len;

    if( pData->opt_desc.desc != NULL ) pElems = pData->opt_desc.desc;
    else                               pElems = pData->desc.desc;

    DUMP( "convertor_decode( %p, {%p, %d}, %d )\n", (void*)pConvertor,
          pInputv[0].iov_base, pInputv[0].iov_len, inputCount );
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc  = pStack->index;
    disp = 0;
    if( pos_desc == -1 ) {
        pos_desc = 0;
        count_desc = pElems[0].count;
        disp_desc = pElems[0].disp;
    } else {
        count_desc = pStack->count;
        if( pElems[pos_desc].type != DT_LOOP ) {
            pConvertor->stack_pos--;
            pStack--;
            disp = pStack->disp;
            disp_desc = ( pElems[pos_desc].disp +
                          (pElems[pos_desc].count - count_desc) * pElems[pos_desc].extent);
        }
    }
    DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElems, "starting" );
    DUMP( "remember position on stack %d last_elem at %d\n", pConvertor->stack_pos, pos_desc );
    DUMP( "top stack info {index = %d, count = %d}\n", 
          pStack->index, pStack->count );

  next_loop:
    while( pos_desc < pStack->end_loop ) {
        if( pElems[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
            if( --(pStack->count) == 0 ) { /* end of loop */
                pConvertor->stack_pos--;
                pStack--;
                if( pConvertor->stack_pos == -1 )
                    return 1;  /* completed */
            }
            pos_desc = pStack->index;
            if( pos_desc == -1 )
                pStack->disp += (pData->ub - pData->lb);
            else
                pStack->disp += pElems[pos_desc].extent;
            pos_desc++;
            disp = pStack->disp;
            count_desc = pElems[pos_desc].count;
            disp_desc = pElems[pos_desc].disp;
            goto next_loop;
        }
        if( pElems[pos_desc].type == DT_LOOP ) {
            do {
                PUSH_STACK( pStack, pConvertor->stack_pos,
                            pos_desc, pElems[pos_desc].count,
                            disp, pos_desc + pElems[pos_desc].disp + 1 );
                pos_desc++;
            } while( pElems[pos_desc].type == DT_LOOP ); /* let's start another loop */
            DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElems, "advance loops" );
            /* update the current state */
            count_desc = pElems[pos_desc].count;
            disp_desc = pElems[pos_desc].disp;
            goto next_loop;
        }
        while( pElems[pos_desc].flags & DT_FLAG_DATA ) {
            /* now here we have a basic datatype */
            type = pElems[pos_desc].type;
            rc = pConvertor->pFunctions[type]( count_desc,
                                               pInput, iCount, pElems[pos_desc].extent,
                                               pOutput + disp + disp_desc, oCount, pElems[pos_desc].extent,
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
                PUSH_STACK( pStack, pConvertor->stack_pos,
                            pos_desc, count_desc - rc,
                            disp + rc * pElems[pos_desc].extent, pos_desc );
                if( iCount != 0 )
                    printf( "there is still room in the input buffer %d bytes\n", iCount );
                return 0;
            }
            pConvertor->converted += rc;  /* number of elementd converted so far */
            pos_desc++;  /* advance to the next data */
            count_desc = pElems[pos_desc].count;
            disp_desc = pElems[pos_desc].disp;
            if( iCount == 0 ) goto end_loop;  /* break if there is no more data in the buffer */
        }
    }
  end_loop:
    /* out of the loop: we have complete the data conversion or no more space
     * in the buffer.
     */
    if( pConvertor->pStack[0].count < 0 ) return 1;  /* data succesfully converted */

    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc,
                pElems[pos_desc].count, disp, pos_desc );

    return 0;
}

static int ompi_convertor_unpack_homogeneous( ompi_convertor_t* pConv,
                                             struct iovec* iov, unsigned int out_size )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int i;                /* counter for basic datatype with extent */
    long lastDisp = 0, last_count = 0;
    int space = iov[0].iov_len;
    char* pSrcBuf;
    dt_desc_t* pData = pConv->pDesc;
    dt_elem_desc_t* pElems;
    int next_length;
    int init_bconvert = pConv->bConverted;

    pSrcBuf = iov[0].iov_base;

    if( pData->opt_desc.desc != NULL ) {
        pElems = pData->opt_desc.desc;
    } else {
        pElems = pData->desc.desc;
    }
    pStack = pConv->pStack + pConv->stack_pos;
    DUMP_STACK( pStack, pConv->stack_pos, pElems, "starting" );
    pos_desc = pStack->index;
    lastDisp = pStack->disp;
    last_count = pStack->count;
    if( pElems[pos_desc].flags & DT_FLAG_DATA ) {
        pStack--;
        pConv->stack_pos--;
    }
    DUMP( "remember position on stack %d last_elem at %d\n", pConv->stack_pos, pos_desc );
    DUMP( "top stack info {index = %d, count = %d}\n", pStack->index, pStack->count );

    while( pos_desc < pStack->end_loop ) {
        if( pElems[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
            if( --(pStack->count) == 0 ) { /* end of loop */
                pStack--;
                if( --(pConv->stack_pos) == -1 ) break;
            } else {
                pos_desc = pStack->index;
                if( pos_desc == -1 )
                    pStack->disp += (pData->ub - pData->lb);
                else
                    pStack->disp += pElems[pos_desc].extent;
            }
            pos_desc++;
            last_count = pElems[pos_desc].count * basicDatatypes[pElems[pos_desc].type].size;
        }
        while( pElems[pos_desc].type == DT_LOOP ) {
            int stop_in_loop = 0;
            if( pElems[pos_desc].flags & DT_FLAG_CONTIGUOUS ) {
                dt_elem_desc_t* pLast = &( pElems[pos_desc + pElems[pos_desc].disp]);
                last_count = pElems[pos_desc].count;
                if( (pLast->extent * last_count) > space ) {
                    last_count = space / pLast->extent;
                    stop_in_loop = 1;
                }
                for( i = 0; i < (pElems[pos_desc].count - 1); i++ ) {
                    MEMCPY( pConv->pBaseBuf + lastDisp, pSrcBuf, pLast->extent );
                    pSrcBuf += pLast->extent;
                    lastDisp += pElems[pos_desc].extent;
                }
                space -= (pLast->extent * last_count);
                pConv->bConverted += (pLast->extent * last_count);
                if( stop_in_loop != 0 ) {
                    pos_desc += pElems[pos_desc].disp + 1;
                    last_count = pElems[pos_desc].count;
                    continue;
                }
                last_count = space;
                next_length = pLast->extent - space;
                /* Save the stack with the correct last_count value. */
            }
            PUSH_STACK( pStack, pConv->stack_pos, pos_desc, last_count,
                        pStack->disp, pos_desc + pElems[pos_desc].disp );
            pos_desc++;
            last_count = pElems[pos_desc].count * basicDatatypes[pElems[pos_desc].type].size;
        }
        /* now here we have a basic datatype */
        while( pElems[pos_desc].flags & DT_FLAG_DATA ) {
            /* do we have enough space in the buffer ? */
            if( space < last_count ) {
                next_length = last_count - space;
                last_count = space;
                goto end_loop;  /* or break whatever but go out of this while */
            }
            MEMCPY( pConv->pBaseBuf + lastDisp, pSrcBuf, last_count );
            pConv->bConverted += last_count;
            space -= last_count;
            pSrcBuf += last_count;
            pos_desc++;  /* advance to the next data */
            lastDisp = pStack->disp + pElems[pos_desc].disp;
            last_count = pElems[pos_desc].count * basicDatatypes[pElems[pos_desc].type].size;
        }
    }
    last_count = 0;  /* complete the data */
 end_loop:
    if( last_count != 0 ) { /* save the internal state */
        MEMCPY( pConv->pBaseBuf + lastDisp, pSrcBuf, last_count );
        pConv->bConverted += last_count;
        lastDisp += last_count;
    }
    if( pos_desc < pStack->end_loop )  /* cleanup the stack */
        PUSH_STACK( pStack, pConv->stack_pos, pos_desc, next_length,
                    lastDisp, pos_desc );

    iov[0].iov_len = pConv->bConverted - init_bconvert;
    return 0;
}

static int ompi_convertor_unpack_homogeneous_contig( ompi_convertor_t* pConv,
                                                    struct iovec* iov, unsigned int out_size )
{
    dt_desc_t *pData = pConv->pDesc;
    char* pDstBuf = pConv->pBaseBuf + pData->true_lb + pConv->bConverted;
    char* pSrcBuf = iov[0].iov_base;
    long extent = pData->ub - pData->lb;
    int length, i;

    if( iov[0].iov_base != NULL ) {
        if( pData->size == extent ) {
            length = pConv->count * pData->size - pConv->bConverted;
        
            if( length > iov[0].iov_len )
                length = iov[0].iov_len;
            /* contiguous data or basic datatype with count */
            MEMCPY( pDstBuf, pSrcBuf, length );
        } else {
            length = iov[0].iov_len;
            for( i = 0; i < pConv->count; i++ ) {
                MEMCPY( pDstBuf, pSrcBuf, pData->size );
                pSrcBuf += pData->size;
                pDstBuf += extent;
                length -= pData->size;
            }
        }
        pConv->bConverted += length;
        return (pConv->bConverted == (pData->size * pConv->count));
    }
    return (pConv->bConverted == (pData->size * pConv->count));
}

int ompi_convertor_unpack( ompi_convertor_t* pConvertor,
                          struct iovec* pInputv,
                          unsigned int inputCount )
{
    dt_desc_t *pData = pConvertor->pDesc;
    char* pOutput = pConvertor->pBaseBuf;
    char* pInput = pInputv[0].iov_base;
    int rc;

    if( pConvertor->bConverted == (pData->size * pConvertor->count) ) {
        pInputv[0].iov_len = 0;
        return 1;  /* completed */
    }
    if( pConvertor->flags & DT_FLAG_CONTIGUOUS ) {
        if( pInputv[0].iov_base == NULL ) {
            rc = pConvertor->count * pData->size;
            pInputv[0].iov_base = pConvertor->pBaseBuf + pData->true_lb + pConvertor->bConverted;
            if( pInputv[0].iov_len == 0 ) {  /* give me the whole buffer */
                pInputv[0].iov_len = rc - pConvertor->bConverted;
            } else {  /* what about the next chunk ? */
                if( pInputv[0].iov_len > (rc - pConvertor->bConverted) )
                    pInputv[0].iov_len = rc - pConvertor->bConverted;
            }
            pConvertor->bConverted += pInputv[0].iov_len;
            return (pConvertor->bConverted == rc);
        }
    }
    if( (pInput >= pOutput) && (pInput < (pOutput + pConvertor->count * (pData->ub - pData->lb))) ) {
        return 1;
    }
    return ompi_convertor_progress( pConvertor, pInputv, inputCount );
}

/* Return value:
 *     0 : nothing has been done
 * positive value: number of item converted.
 * negative value: -1 * number of items converted, less data provided than expected
 *                and there are less data than the size on the remote host of the
 *                basic datatype.
 */
#define COPY_TYPE( TYPENAME, TYPE ) \
static int copy_##TYPENAME( unsigned int count, \
                            char* from, unsigned int from_len, long from_extent, \
                            char* to, unsigned int to_len, long to_extent, \
                            int* used )                                 \
{ \
   int i, res = 1; \
   unsigned int remote_TYPE_size = sizeof(TYPE); /* TODO */ \
\
   if( (remote_TYPE_size * count) > from_len ) { \
      count = from_len / remote_TYPE_size; \
      if( (count * remote_TYPE_size) != from_len ) { \
         DUMP( "oops should I keep this data somewhere (excedent %d bytes)?\n", \
               from_len - (count * remote_TYPE_size) ); \
         res = -1; \
      } \
      DUMP( "correct: copy %s count %d from buffer %p with length %d to %p space %d\n", \
            #TYPE, count, from, from_len, to, to_len ); \
   } else \
      DUMP( "         copy %s count %d from buffer %p with length %d to %p space %d\n", \
            #TYPE, count, from, from_len, to, to_len ); \
\
   if( (from_extent == sizeof(TYPE)) && (to_extent == sizeof(TYPE)) ) { \
      MEMCPY( to, from, count * sizeof(TYPE) ); \
   } else { \
      for( i = 0; i < count; i++ ) { \
         MEMCPY( to, from, sizeof(TYPE) ); \
         to += to_extent; \
         from += from_extent; \
      } \
   } \
   *used = count * sizeof(TYPE) ; \
   return res * count; \
}

COPY_TYPE( char, char )
COPY_TYPE( short, short )
COPY_TYPE( int, int )
COPY_TYPE( float, float )
COPY_TYPE( long, long )
/*COPY_TYPE( double, double );*/
COPY_TYPE( long_long, long long )
COPY_TYPE( long_double, long double )
COPY_TYPE( complex_float, ompi_complex_float_t )
COPY_TYPE( complex_double, ompi_complex_double_t )

static int copy_double( unsigned int count,
                        char* from, unsigned int from_len, long from_extent,
                        char* to, unsigned int to_len, long to_extent,
                        int* used )
{
   int i, res = 1;
   unsigned int remote_double_size = sizeof(double); /* TODO */

   if( (remote_double_size * count) > from_len ) {
      count = from_len / remote_double_size;
      if( (count * remote_double_size) != from_len ) {
         DUMP( "oops should I keep this data somewhere (excedent %d bytes)?\n",
               from_len - (count * remote_double_size) );
         res = -1;
      }
      DUMP( "correct: copy %s count %d from buffer %p with length %d to %p space %d\n",
            "double", count, from, from_len, to, to_len );
   } else
      DUMP( "         copy %s count %d from buffer %p with length %d to %p space %d\n",
            "double", count, from, from_len, to, to_len );

   
   if( (from_extent == sizeof(double)) && (to_extent == sizeof(double)) ) {
      MEMCPY( to, from, count * sizeof(double) );
   } else {
      for( i = 0; i < count; i++ ) {      
         MEMCPY( to, from, sizeof(double) );     
         to += to_extent;
         from += from_extent;
      }
   }
   *used = count * sizeof(double) ;
   return res * count;
}

conversion_fct_t copy_functions[DT_MAX_PREDEFINED] = {
   (conversion_fct_t)NULL,                 /*    DT_LOOP           */ 
   (conversion_fct_t)NULL,                 /*    DT_LB             */ 
   (conversion_fct_t)NULL,                 /*    DT_UB             */ 
   (conversion_fct_t)NULL,                 /*    DT_SPACE          */ 
   (conversion_fct_t)copy_char,            /*    DT_CHAR           */ 
   (conversion_fct_t)copy_char,            /*    DT_BYTE           */ 
   (conversion_fct_t)copy_short,           /*    DT_SHORT          */ 
   (conversion_fct_t)copy_int,             /*    DT_INT            */ 
   (conversion_fct_t)copy_float,           /*    DT_FLOAT          */ 
   (conversion_fct_t)copy_long,            /*    DT_LONG           */ 
   (conversion_fct_t)copy_double,          /*    DT_DOUBLE         */ 
   (conversion_fct_t)copy_long_long,       /*    DT_LONG_LONG      */ 
   (conversion_fct_t)copy_long_double,     /*    DT_LONG_DOUBLE    */ 
   (conversion_fct_t)copy_complex_float,   /*    DT_COMPLEX_FLOAT  */ 
   (conversion_fct_t)copy_complex_double,  /*    DT_COMPLEX_DOUBLE */ 
};

/* Should we supply buffers to the convertor or can we use directly
 * the user buffer ?
 */
int ompi_convertor_need_buffers( ompi_convertor_t* pConvertor )
{
   if( pConvertor->flags & DT_FLAG_CONTIGUOUS ) return 0;
   return 1;
}

extern int local_sizes[DT_MAX_PREDEFINED];
int ompi_convertor_init_for_recv( ompi_convertor_t* pConv, unsigned int flags,
                                 dt_desc_t* pData, int count,
                                 void* pUserBuf, int starting_point )
{
    OBJ_RETAIN( pData );
    if( pConv->pDesc != pData ) {
        pConv->pDesc = pData;
        pConv->flags = CONVERTOR_RECV;
        if( pConv->pStack != NULL ) free( pConv->pStack );
        pConv->pStack = NULL;
    }
    if( pConv->pStack == NULL ) {
        pConv->pStack = (dt_stack_t*)malloc(sizeof(dt_stack_t) * (pData->btypes[DT_LOOP] + 2) );
        pConv->stack_pos = 0;
    }

    pConv->pBaseBuf = pUserBuf;
    pConv->available_space = count * (pData->ub - pData->lb);
    pConv->count = count;
    pConv->pFunctions = copy_functions;
    pConv->converted = 0;
    pConv->bConverted = 0;
    /* TODO: work only on homogeneous architectures */
    if( pData->flags & DT_FLAG_CONTIGUOUS ) {
        pConv->flags |= DT_FLAG_CONTIGUOUS;
        pConv->fAdvance = ompi_convertor_unpack_homogeneous_contig;
    } else {
        pConv->fAdvance = ompi_convertor_unpack_homogeneous;
    }
    ompi_create_stack_with_pos( pConv, starting_point, local_sizes );
    return 0;
}

/* Get the number of elements from the data associated with this convertor that can be
 * retrieved from a recevied buffer with the size iSize.
 * To spped-up this function you should use it with a iSize == to the modulo
 * of the original size and the size of the data.
 * This function should be called with a initialized clean convertor.
 * Return value:
 *   positive = number of basic elements inside
 *   negative = some error occurs
 */
int ompi_ddt_get_element_count( dt_desc_t* pData, int iSize )
{
   dt_stack_t* pStack;   /* pointer to the position on the stack */
   int pos_desc;         /* actual position in the description of the derived datatype */
   int type;             /* type at current position */
   int rc, nbElems = 0;
   int stack_pos = 0;

   DUMP( "dt_count_elements( %p, %d )\n", (void*)pData, iSize );
   pStack = alloca( sizeof(pStack) * (pData->btypes[DT_LOOP] + 2) );
   pStack->count = 1;
   pStack->index = -1;
   pStack->end_loop = pData->desc.used;
   pStack->disp = 0;
   pos_desc  = 0;

   DUMP_STACK( pStack, stack_pos, pData->desc.desc, "starting" );
   DUMP( "remember position on stack %d last_elem at %d\n", stack_pos, pos_desc );
   DUMP( "top stack info {index = %d, count = %d}\n", 
         pStack->index, pStack->count );

  next_loop:
   while( pos_desc < pStack->end_loop ) {
      if( pData->desc.desc[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
         if( --(pStack->count) == 0 ) { /* end of loop */
            stack_pos--;
            pStack--;
            if( stack_pos == -1 )
               return nbElems;  /* completed */
         }
         pos_desc = pStack->index;
         if( pos_desc == -1 )
            pStack->disp += (pData->ub - pData->lb);
         else
            pStack->disp += pData->desc.desc[pos_desc].extent;
         pos_desc++;
         goto next_loop;
      }
      if( pData->desc.desc[pos_desc].type == DT_LOOP ) {
         do {
            PUSH_STACK( pStack, stack_pos, pos_desc, pData->desc.desc[pos_desc].count,
                        0, pos_desc + pData->desc.desc[pos_desc].disp );
            pos_desc++;
         } while( pData->desc.desc[pos_desc].type == DT_LOOP ); /* let's start another loop */
         DUMP_STACK( pStack, stack_pos, pData->desc.desc, "advance loops" );
         goto next_loop;
      }
      /* now here we have a basic datatype */
      type = pData->desc.desc[pos_desc].type;
      rc = pData->desc.desc[pos_desc].count * basicDatatypes[type].size;
      if( rc >= iSize ) {
         nbElems += iSize / basicDatatypes[type].size;
         break;
      }
      nbElems += pData->desc.desc[pos_desc].count;
      iSize -= rc;

      pos_desc++;  /* advance to the next data */
   }

   /* cleanup the stack */
   return nbElems;
}

int ompi_ddt_copy_content_same_ddt( dt_desc_t* pData, int count,
                                   char* pDestBuf, char* pSrcBuf )
{
   dt_stack_t* pStack;   /* pointer to the position on the stack */
   int pos_desc;         /* actual position in the description of the derived datatype */
   int type;             /* type at current position */
   int stack_pos = 0;
   long lastDisp = 0, lastLength = 0;
   dt_elem_desc_t* pElems;

   if( (pData->flags & DT_FLAG_BASIC) == DT_FLAG_BASIC ) {
       /* basic datatype with count */
       pSrcBuf += pData->true_lb;
       pDestBuf += pData->true_lb;
       if( (pData->true_ub - pData->true_lb) == pData->size ) {
           /* all the data is contiguous in the memory */
           if( pData->size * count < (512*1024) ) {
               MEMCPY( pDestBuf, pSrcBuf, pData->size * count );
           } else {
               type = 512 * 1024;
               lastLength = count * pData->size;
               while( lastLength > 0 ) {
                   if( type > lastLength ) type = lastLength;
                   MEMCPY( pDestBuf, pSrcBuf, type );
                   pDestBuf += type;
                   pSrcBuf += type;
                   lastLength -= type;
               }
           }
       } else {
           /* there are gaps between elements */
           for( type = 0; type < count; type++ ) {
               MEMCPY( pDestBuf, pSrcBuf, pData->size );
               pDestBuf += pData->size;
               pSrcBuf += (pData->ub - pData->lb);
           }
       }
       return 0;
   }

   pStack = alloca( sizeof(dt_stack_t) * (pData->btypes[DT_LOOP]+1) );
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
             pos_desc++;
         }
         goto next_loop;
      }
      if( pElems[pos_desc].type == DT_LOOP ) {
         do {
            PUSH_STACK( pStack, stack_pos, pos_desc, pElems[pos_desc].count,
                        pStack->disp, pos_desc + pElems[pos_desc].disp );
            pos_desc++;
         } while( pElems[pos_desc].type == DT_LOOP ); /* let's start another loop */
         DUMP_STACK( pStack, stack_pos, pElems, "advance loops" );
         goto next_loop;
      }
      /* now here we have a basic datatype */
      type = pElems[pos_desc].type;
      if( (lastDisp + lastLength) == (pStack->disp + pElems[pos_desc].disp) ) {
         lastLength += pElems[pos_desc].count * basicDatatypes[type].size;
      } else {
         MEMCPY( pDestBuf + lastDisp, pSrcBuf + lastDisp, lastLength );
         lastDisp = pStack->disp + pElems[pos_desc].disp;
         lastLength = pElems[pos_desc].count * basicDatatypes[type].size;
      }
      pos_desc++;  /* advance to the next data */
   }

   if( lastLength != 0 )
       MEMCPY( pDestBuf + lastDisp, pSrcBuf + lastDisp, lastLength );
   /* cleanup the stack */
   return 0;
}
