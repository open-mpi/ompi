/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "lam_config.h"

#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

static int lam_convertor_pack_general( lam_convertor_t* pConvertor,
                                       struct iovec* out, unsigned int outCount )
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
    while( pos_desc < pStack->end_loop ) {
        if( pElem[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
            if( --(pStack->count) == 0 ) {
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
        while( pElem[pos_desc].flags & DT_FLAG_DATA ) {
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
            if( iCount == 0 ) goto complete_loop;  /* break if there is no more data in the buffer */
        }
    }
  complete_loop:
    /* out of the loop: we have complete the data conversion or no more space
     * in the buffer.
     */
    if( pConvertor->pStack[0].count < 0 ) return 1;  /* data succesfully converted */

    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, pElem[pos_desc].count,
                disp, pos_desc );

    return 0;
}

/* We suppose here that we work with an already optimized version of the data
 */
static int lam_convertor_pack_homogeneous( lam_convertor_t* pConv,
                                           struct iovec* iov, unsigned int out_size )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int type;             /* type at current position */
    int i;                /* index for basic elements with extent */
    long lastDisp = 0, last_count = 0;
    int space = iov[0].iov_len;
    char* pDestBuf;
    dt_desc_t* pData = pConv->pDesc;
    dt_elem_desc_t* pElems;
    int next_length;
    int init_bconvert = pConv->bConverted;

    pDestBuf = iov[0].iov_base;

    if( pData->flags & DT_FLAG_CONTIGUOUS ) {
        long extent = pData->ub - pData->lb;
        char* pSrcBuf = pConv->pBaseBuf + pData->true_lb + pConv->bConverted;

        type = pConv->count * pData->size;
        if( pData->size == extent /* true extent at this point */ ) {
            /* we can do it with just one memcpy */
            MEMCPY( pDestBuf, pSrcBuf, iov[0].iov_len );
            space -= iov[0].iov_len;
            pConv->bConverted += iov[0].iov_len;
        } else {
            for( pos_desc = 0; pos_desc < pConv->count; pos_desc++ ) {
                MEMCPY( pDestBuf, pSrcBuf, pData->size );
                space -= pData->size;
                pSrcBuf += extent;
                pDestBuf += pData->size;
            }
            pConv->bConverted += type;
        }
        return (pConv->bConverted == (pData->size * pConv->count));
    }

    if( pData->opt_desc.desc != NULL ) {
        pElems = pData->opt_desc.desc;
    } else {
        pElems = pData->desc.desc;
    }

    pStack = pConv->pStack + pConv->stack_pos;
    pos_desc = pStack->index;
    lastDisp = pStack->disp;
    last_count = pStack->count;
    if( pElems[pos_desc].flags & DT_FLAG_DATA ) {
        pStack--;
        pConv->stack_pos--;
    }
    DUMP_STACK( pStack, pConv->stack_pos, pElems, "starting" );
    DUMP( "remember position on stack %d last_elem at %d\n", pConv->stack_pos, pos_desc );
    DUMP( "top stack info {index = %d, count = %d}\n", 
          pStack->index, pStack->count );
  next_loop:
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
            goto next_loop;
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
                for( i = 0; i < last_count; i++ ) {
                    MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, pLast->extent );
                    pDestBuf += pLast->extent;  /* size of the contiguous data */
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
            MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, last_count );
            pConv->bConverted += last_count;
            space -= last_count;
            pDestBuf += last_count;
            pos_desc++;  /* advance to the next data */
            lastDisp = pStack->disp + pElems[pos_desc].disp;
            last_count = pElems[pos_desc].count * basicDatatypes[pElems[pos_desc].type].size;
        }
    }
    last_count = 0;  /* complete the data */
 end_loop:
    if( last_count != 0 ) {  /* save the internal state */
        MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, last_count );
        pConv->bConverted += last_count;
        lastDisp += last_count;
    }
    if( pos_desc <= pStack->end_loop )  /* cleanup the stack */
        PUSH_STACK( pStack, pConv->stack_pos, pos_desc, next_length,
                    lastDisp, pos_desc );

    iov[0].iov_len = pConv->bConverted - init_bconvert; 
    return 0;
}

static int lam_convertor_pack_homogeneous_contig( lam_convertor_t* pConv,
                                                  struct iovec* out, unsigned int out_size )
{
    lam_datatype_t* pData = pConv->pDesc;
    char* pSrc = pConv->pBaseBuf + pData->true_lb + pConv->bConverted;
    size_t length = pData->size * pConv->count;
    long extent;

    if( pData->size == (extent = (pData->ub - pData->lb)) ) {
        if( out[0].iov_base == NULL ) {
            out[0].iov_base = pSrc;
            if( (pConv->bConverted + out[0].iov_len) > length )
                out[0].iov_len = length - pConv->bConverted;
        } else {
            /* contiguous data just memcpy the smallest data in the user buffer */
            out[0].iov_len = IMIN( out[0].iov_len, length );
            MEMCPY( out[0].iov_base, pSrc, out[0].iov_len);
        }
        pConv->bConverted += out[0].iov_len;
        return (pConv->bConverted == length);
    }
    return -1;  /* not yet implemented */
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

    /* TODO should use the remote size */
    if( pConv->bConverted == (pData->size * pConv->count) ) {  /* conversion completed or nothing to do */
        out[0].iov_len = 0;
        return 1;
    }
    if( !(pData->flags & DT_FLAG_CONTIGUOUS) ) {  /* TODO REMOVE ME */
        if( out[0].iov_base == NULL ) {
            out[0].iov_len = pConv->count * pData->size;
            out[0].iov_base = (void*)malloc( out[0].iov_len );
            pConv->freebuf = out[0].iov_base;
        }
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

    pConv->pBaseBuf = pUserBuf;
    pConv->available_space = count * (dt->ub - dt->lb);
    pConv->count = count;
    pConv->pFunctions = copy_functions;
    pConv->converted = 0;
    pConv->bConverted = 0;
    if( dt->flags & DT_FLAG_CONTIGUOUS ) {
        pConv->flags |= DT_FLAG_CONTIGUOUS;
        pConv->fAdvance = lam_convertor_pack_homogeneous_contig;
    } else {
        /* TODO handle the sender convert case */
        pConv->fAdvance = lam_convertor_pack_general;
        pConv->fAdvance = lam_convertor_pack_homogeneous;
    }
    if( pConv->freebuf != NULL ) {
        free( pConv->freebuf );
        pConv->freebuf = NULL;
    }
    lam_create_stack_with_pos( pConv, local_starting_point, local_sizes );
    return 0;
}

lam_convertor_t* lam_convertor_create( int remote_arch, int mode )
{
   lam_convertor_t* pConv = OBJ_NEW(lam_convertor_t);

   pConv->pDesc      = NULL;
   pConv->pStack     = NULL;
   pConv->remoteArch = remote_arch;
   pConv->fAdvance   = NULL;
   pConv->freebuf    = NULL;
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
   pDestConv->fAdvance   = NULL;
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
