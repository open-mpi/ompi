/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "datatype/datatype.h"
#include "datatype/convertor.h"
#include "datatype/datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

#define DO_DEBUG(INST)

#if OMPI_ENABLE_DEBUG
int ompi_ddt_safeguard_pointer_debug_breakpoint( const void* actual_ptr, int length,
                                                 const void* initial_ptr,
                                                 const ompi_datatype_t* pData,
                                                 int count )
{
    return 0;
}
#endif  /* OMPI_ENABLE_DEBUG */

static
int ompi_convertor_pack_general( ompi_convertor_t* pConvertor,
				 struct iovec* iov, uint32_t* out_size,
				 size_t* max_data,
				 int32_t* freeAfter )
{
    dt_stack_t* pStack;    /* pointer to the position on the stack */
    uint32_t pos_desc;     /* actual position in the description of the derived datatype */
    int count_desc;        /* the number of items already done in the actual pos_desc */
    int type = DT_CHAR;    /* type at current position */
    uint32_t advance;      /* number of bytes that we should advance the buffer */
    long disp_desc = 0;    /* compute displacement for truncated data */
    int bConverted = 0;    /* number of bytes converted this time */
    const ompi_datatype_t *pData = pConvertor->pDesc;
    dt_elem_desc_t* pElem;
    char* pOutput = pConvertor->pBaseBuf;
    char* pInput;
    int iCount, rc;
    uint32_t iov_count, total_bytes_converted = 0;

    DUMP( "convertor_decode( %p, {%p, %d}, %d )\n", (void*)pConvertor,
          iov[0].iov_base, iov[0].iov_len, *out_size );

    pElem = pData->desc.desc;

    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc   = pStack->index;
    disp_desc  = pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;

    DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "starting" );
    DUMP( "remember position on stack %d last_elem at %d\n", pConvertor->stack_pos, pos_desc );
    DUMP( "top stack info {index = %d, count = %d}\n", 
          pStack->index, pStack->count );

    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        bConverted = 0;
        if( iov[iov_count].iov_base == NULL ) {
            uint32_t length = iov[iov_count].iov_len;
            if( length <= 0 )
                length = pConvertor->count * pData->size - pConvertor->bConverted - bConverted;
            if( (*max_data) < length )
                length = *max_data;
            iov[iov_count].iov_len = length;
            iov[iov_count].iov_base = pConvertor->memAlloc_fn( &(iov[iov_count].iov_len),
                                          pConvertor->memAlloc_userdata );
            *freeAfter = (*freeAfter) | ( 1 << iov_count);
        }
        pInput = iov[iov_count].iov_base;
        iCount = iov[iov_count].iov_len;
        while( 1 ) {
            if( DT_END_LOOP == pElem[pos_desc].elem.common.type ) { /* end of the current loop */
                if( --(pStack->count) == 0 ) { /* end of loop */
                    if( pConvertor->stack_pos == 0 )
                        goto complete_loop;  /* completed */
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if( pStack->index == -1 ) {
                        pStack->disp += (pData->ub - pData->lb);
                    } else {
                        assert( DT_LOOP == pElem[pStack->index].elem.common.type );
                        pStack->disp += pElem[pStack->index].loop.extent;
                    }
                }
                count_desc = pElem[pos_desc].elem.count;
                disp_desc = pElem[pos_desc].elem.disp;
            }
            if( DT_LOOP == pElem[pos_desc].elem.common.type ) {
                do {
                    PUSH_STACK( pStack, pConvertor->stack_pos,
                                pos_desc, DT_LOOP, pElem[pos_desc].loop.loops,
                                pStack->disp, pos_desc + pElem[pos_desc].loop.items + 1);
                    pos_desc++;
                } while( DT_LOOP == pElem[pos_desc].elem.common.type ); /* let's start another loop */
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loops" );
                /* update the current state */
                count_desc = pElem[pos_desc].elem.count;
                disp_desc = pElem[pos_desc].elem.disp;
                continue;
            }
            while( pElem[pos_desc].elem.common.flags & DT_FLAG_DATA ) {
                /* now here we have a basic datatype */
                type = pElem[pos_desc].elem.common.type;
                rc = pConvertor->pFunctions[type]( count_desc,
                                                   pOutput + pStack->disp + disp_desc,
                                                   iCount, pElem[pos_desc].elem.extent,
                                                   pInput, iCount, BASIC_DDT_FROM_ELEM(pElem[pos_desc])->size );
                advance = rc * BASIC_DDT_FROM_ELEM(pElem[pos_desc])->size;
                iCount -= advance;      /* decrease the available space in the buffer */
                pInput += advance;      /* increase the pointer to the buffer */
                bConverted += advance;
                if( rc != count_desc ) {
                    /* not all data has been converted. Keep the state */
                    count_desc -= rc;
                    disp_desc += rc * pElem[pos_desc].elem.extent;
                    if( iCount != 0 )
                        printf( "pack there is still room in the input buffer %d bytes\n", iCount );
                    goto complete_loop;
                }
                pos_desc++;  /* advance to the next data */
                count_desc = pElem[pos_desc].elem.count;
                disp_desc = pElem[pos_desc].elem.disp;
                if( iCount == 0 ) goto complete_loop;  /* break if there is no more data in the buffer */
            }
        }
    complete_loop:
        pConvertor->bConverted += bConverted;  /* update the already converted bytes */
        iov[iov_count].iov_len = bConverted;   /* update the length in the iovec */
        total_bytes_converted += bConverted;
    }
    *max_data = total_bytes_converted;
    /* out of the loop: we have complete the data conversion or no more space
     * in the buffer.
     */
    if( pConvertor->pStack[0].count < 0 ) return 1;  /* data succesfully converted */

    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, type, count_desc,
		disp_desc, pos_desc );

    return (pConvertor->bConverted == (pData->size * pConvertor->count));
}

/* We suppose here that we work with an already optimized version of the data
 */
static
int ompi_convertor_pack_homogeneous_with_memcpy( ompi_convertor_t* pConv,
						 struct iovec* iov,
						 uint32_t* out_size,
						 size_t* max_data,
						 int* freeAfter )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    uint32_t pos_desc;    /* actual position in the description of the derived datatype */
    int i;                /* index for basic elements with extent */
    int bConverted = 0;   /* number of bytes converted/moved this time */
    long lastDisp = 0, last_count = 0;
    uint32_t space = iov[0].iov_len, last_blength = 0;
    char* pDestBuf;
    const ompi_datatype_t* pData = pConv->pDesc;
    dt_elem_desc_t* pElems;

    pDestBuf = iov[0].iov_base;
  
    pElems = pConv->use_desc->desc; 
   
    pStack = pConv->pStack + pConv->stack_pos;
    pos_desc = pStack->index;
    lastDisp = pStack->disp;
    last_count = pStack->count;
    pStack--;
    pConv->stack_pos--;
   
    while( 1 ) {
        if( DT_END_LOOP == pElems[pos_desc].elem.common.type ) { /* end of the current loop */
            if( --(pStack->count) == 0 ) { /* end of loop */
                if( pConv->stack_pos == 0 ) {  /* finish everything */
                    last_count = 0;
                    pos_desc = -1;
                    goto end_loop;
                }
                pStack--;
                pConv->stack_pos--;
                pos_desc++;  /* go to the next element */
            } else {
                if( pStack->index == -1 ) {
                    pStack->disp += (pData->ub - pData->lb);
                    pos_desc = 0;
                } else {
                    assert( DT_LOOP == pElems[pStack->index].elem.common.type );
                    pStack->disp += pElems[pStack->index].loop.extent;
                    pos_desc = pStack->index + 1;
                }
            }
            last_count = pElems[pos_desc].elem.count;
            last_blength = last_count;
            lastDisp = pStack->disp + pElems[pos_desc].elem.disp;
            continue;
        }
        while( DT_LOOP == pElems[pos_desc].elem.common.type ) {
            int stop_in_loop = 0;
            if( pElems[pos_desc].elem.common.flags & DT_FLAG_CONTIGUOUS ) {
                ddt_endloop_desc_t* end_loop = &(pElems[pos_desc + pElems[pos_desc].loop.items].end_loop);
                if( (end_loop->size * last_count) > space ) {
                    stop_in_loop = last_count;
                    last_count = space / end_loop->size;
                }
                for( i = 0; i < last_count; i++ ) {
                    OMPI_DDT_SAFEGUARD_POINTER( pConv->pBaseBuf + lastDisp, end_loop->size,
                                                pConv->pBaseBuf, pData, pConv->count );
                    MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, end_loop->size );
                    pDestBuf += end_loop->size;  /* size of the contiguous data */
                    lastDisp += pElems[pos_desc].loop.extent;
                }
                space -= (end_loop->size * last_count);
                bConverted += (end_loop->size * last_count);
                if( stop_in_loop == 0 ) {
                    pos_desc += pElems[pos_desc].loop.items + 1;
                    last_count = pElems[pos_desc].elem.count;
                    continue;
                }
                /* mark some of the iterations as completed */
                last_count = stop_in_loop - last_count;
                last_blength = 0;
                /* Save the stack with the correct last_count value. */
            }
            PUSH_STACK( pStack, pConv->stack_pos, pos_desc, DT_LOOP, last_count,
                        pStack->disp, pos_desc + pElems[pos_desc].loop.items );
            pos_desc++;
            last_count = pElems[pos_desc].elem.count;
        }
        /* now here we have a basic datatype */
        while( pElems[pos_desc].elem.common.flags & DT_FLAG_DATA ) {
            /* do we have enough space in the buffer ? */
            last_blength = last_count * BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size;
            if( space < last_blength ) {
                last_blength = last_count;
                last_count = space / BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size;
                space -= (last_count * BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size);
                last_blength -= last_count;
                goto end_loop;  /* or break whatever but go out of this while */
            }
            OMPI_DDT_SAFEGUARD_POINTER( pConv->pBaseBuf + lastDisp, last_count,
                                        pConv->pBaseBuf, pData, pConv->count );
            MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, last_count );
            bConverted += last_blength;
            space -= last_blength;
            pDestBuf += last_blength;
            pos_desc++;  /* advance to the next data */
            lastDisp = pStack->disp + pElems[pos_desc].elem.disp;
            last_count = pElems[pos_desc].elem.count;
        }
    }
    last_count = 0;  /* complete the data */
 end_loop:
    if( last_count != 0 ) {  /* save the internal state */
        OMPI_DDT_SAFEGUARD_POINTER( pConv->pBaseBuf + lastDisp, last_count,
                                    pConv->pBaseBuf, pData, pConv->count );
        MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, last_count );
        bConverted += last_count;
        lastDisp += last_count;
    }
    /* update the current stack position */
    PUSH_STACK( pStack, pConv->stack_pos, pos_desc, last_blength, pElems[pos_desc].elem.common.type,
		lastDisp, pos_desc );

    pConv->bConverted += bConverted;  /* update the byte converted field in the convertor */
    iov[0].iov_len = bConverted;      /* update the length in the iovec */
    *max_data = bConverted;
    return (pConv->bConverted == (pData->size * pConv->count));
}

#define IOVEC_MEM_LIMIT 8192

/* The basic idea is to pack or return iovec depending on the datatype shape. If the data
 * is scattered in memory using small chuncks then we have to allocate some space (unless the upper
 * level provide some) and pack the data inside. If the chunks of data are large enough
 * then is useless to allocate additional memory and do the memcpy operation. We can simply
 * return the pointer to the contiguous piece of memory to the upper level.
 */
static
int ompi_convertor_pack_no_conversion( ompi_convertor_t* pConv,
                                       struct iovec* iov,
                                       uint32_t *out_size,
                                       size_t* max_data,
                                       int* freeAfter )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    int pos_desc;             /* actual position in the description of the derived datatype */
    int i;                    /* index for basic elements with extent */
    uint32_t iov_pos = 0;     /* index in the iovec where we put data inside */
    int bConverted = 0;       /* number of bytes converted/moved this time */
    uint32_t space_on_iovec;  /* amount of free space on the current iovec */
    long lastDisp = 0;
    uint32_t space = *max_data, last_blength = 0, saveLength;
    char *destination, *source;
    const ompi_datatype_t* pData = pConv->pDesc;
    ddt_elem_desc_t pack_elem;
    dt_elem_desc_t* pElems;

    pElems = pConv->use_desc->desc;

    pStack = pConv->pStack + pConv->stack_pos;
    destination = iov[0].iov_base;
    source = (char*)pConv->pBaseBuf + pStack->disp;
   

    /* retrieve the context of the last call */
    pos_desc = pStack->index;
    pack_elem.count = pStack->count;
    pack_elem.common.type = pElems[pos_desc].elem.common.type;
    last_blength = pack_elem.count * BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size;
    lastDisp = pStack->disp;
    DO_DEBUG( ompi_output( 0, "pack_no_conversion stack_pos %d index %d count %d last_blength %ld lastDisp %ld savePos %p bConverted %d\n",
                           pConv->stack_pos, pStack->index, pStack->count, last_blength, lastDisp, savePos,
                           pConv->bConverted ); );
    saveLength = 0;
    pStack--;
    pConv->stack_pos--;

    *freeAfter = (*freeAfter) & ~((1 << (*out_size)) - 1);
    space_on_iovec = iov[0].iov_len;

    while( pos_desc >= 0 ) {
        if( DT_END_LOOP == pElems[pos_desc].elem.common.type ) { /* end of the current loop */
            if( --(pStack->count) == 0 ) { /* end of loop */
                if( pConv->stack_pos == 0 ) {  /* finish everything */
                    if( saveLength != 0 ) {
                        /* there is still a chunk of memory to be handled, but here we dont allocate more
                         * memory. We just copy what we can in the right place and update the values to be
                         * saved on the next round.
                         */
                        if( iov_pos < (*out_size) ) {  /* still some place in the iovec */
                            if( iov[iov_pos].iov_base == NULL ) {
                                /* prepare a new iovec */
                                iov[iov_pos].iov_base = source;
                                iov[iov_pos].iov_len = saveLength;
                                bConverted += saveLength;
                                saveLength = 0;
                                iov_pos++;
                                space_on_iovec = 0;
                                /* let's go out of here */
                            } else {
                                uint32_t copy_length = saveLength;
                                if( space_on_iovec < saveLength ) {
                                    copy_length = space_on_iovec;
                                }
                                OMPI_DDT_SAFEGUARD_POINTER( source, copy_length,
                                                            pConv->pBaseBuf, pData, pConv->count );
                                DO_DEBUG( ompi_output( 0, "1. memcpy( %p, %p, %ld ) bConverted %ld space %ld pConv->bConverted %ld\n", destination, source,
                                                       copy_length, bConverted, space_on_iovec, pConv->bConverted ); );
                                MEMCPY( destination, source, copy_length );
                                source += copy_length;
                                destination += copy_length;
                                bConverted += copy_length;
                                space_on_iovec -= copy_length;
                                saveLength -= copy_length;
                            }
                        }
                    }
                    iov[iov_pos].iov_len -= space_on_iovec;
                    pack_elem.count = 0;
                    pos_desc = -1;
                    last_blength = 0;
                    goto end_loop;
                }
                pConv->stack_pos--;
                pStack--;
            } else {
                pos_desc = pStack->index;  /* DT_LOOP index */
                if( pos_desc == -1 ) {
                    pStack->disp += (pData->ub - pData->lb);
                } else {
                    assert( DT_LOOP == pElems[pos_desc].elem.common.type );
                    pStack->disp += pElems[pos_desc].loop.extent;
                }
            }
            pos_desc++;  /* go to the next element */
            lastDisp = pStack->disp + pElems[pos_desc].elem.disp;
            pack_elem.count = pElems[pos_desc].elem.count;
            last_blength = pack_elem.count * BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size;
            continue;  /* next loop */
        }
        while( DT_LOOP == pElems[pos_desc].elem.common.type ) {
            int stop_in_loop = 0;

            /* If the loop container is contiguous then we can do some
             * optimizations.
             */
            if( pElems[pos_desc].loop.common.flags & DT_FLAG_CONTIGUOUS ) {
                /* point to the end of loop element */
                ddt_endloop_desc_t* end_loop = &(pElems[pos_desc + pElems[pos_desc].loop.items].end_loop);
                if( iov[iov_pos].iov_base == NULL ) {
                    iov[iov_pos].iov_base = pConv->memAlloc_fn( &(iov[iov_pos].iov_len),
                                                   pConv->memAlloc_userdata );
                    space_on_iovec = iov[iov_pos].iov_len;
                    destination = iov[iov_pos].iov_base;
                    (*freeAfter) |= (1 << iov_pos);
                }
                /* compute the maximum amount of data to be packed */
                if( (end_loop->size * pack_elem.count) > space_on_iovec ) {
                    stop_in_loop = pack_elem.count;
                    pack_elem.count = space_on_iovec / end_loop->size;
                }
                /* Now let's do it */
                for( i = 0; i < (int)pack_elem.count; i++ ) {
                    OMPI_DDT_SAFEGUARD_POINTER( pConv->pBaseBuf + lastDisp, end_loop->size,
                                                pConv->pBaseBuf, pData, pConv->count );
                    DO_DEBUG (ompi_output( 0, "2. memcpy( %p, %p, %ld )\n", destination, pConv->pBaseBuf + lastDisp,
                                           end_loop->size ); );
                    MEMCPY( destination, pConv->pBaseBuf + lastDisp, end_loop->size );
                    lastDisp += pElems[pos_desc].loop.extent;
                    destination += end_loop->size;
                }
                DO_DEBUG( ompi_output( 0, "\t\tbConverted %d space %d pConv->bConverted %d\n",
                                       bConverted, space_on_iovec, pConv->bConverted ); );
                i = end_loop->size * pack_elem.count;  /* temporary value */
                space_on_iovec -= i;
                space          -= i;
                bConverted     += i;
                if( stop_in_loop == 0 ) {  /* did I stop before the end */
                    /* the pElems point to the LOOP struct in the begining */
                    pos_desc += pElems[pos_desc].loop.items + 1;
                    pack_elem.count = pElems[pos_desc].elem.count;
                    last_blength = pack_elem.count * BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size;
                    lastDisp = pStack->disp + pElems[pos_desc].elem.disp;
                    continue;
                }
                /* mark some of the iterations as completed */
                pack_elem.count = stop_in_loop - pack_elem.count;
                last_blength = 0;
                /* Save the stack with the correct count value. */
            }
            PUSH_STACK( pStack, pConv->stack_pos, pos_desc, DT_LOOP, pack_elem.count,
                        pStack->disp, pos_desc + pElems[pos_desc].loop.items );
            pos_desc++;
            lastDisp = pStack->disp + pElems[pos_desc].elem.disp;
            pack_elem.count = pElems[pos_desc].elem.count;
            last_blength = pack_elem.count * BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size;
        }
        /* now here we have a basic datatype */
        while( pElems[pos_desc].elem.common.flags & DT_FLAG_DATA ) {
            /* first let's see if it's contiguous with the previous chunk of memory and
             * we still have enough room in the buffer...
             */
            if( ((source + saveLength) == (pConv->pBaseBuf + lastDisp))
                && ((saveLength + last_blength) <= space_on_iovec)
                && (pElems[pos_desc].elem.extent == (long)BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size) ) {
                /* ok still contiguous and we still have some space on the buffer */
                saveLength += last_blength;
                /* nothing else to do, we act the next time */
            } else {
                /* Now we have 2 piece of non contiguous memory. One start at source
                 * with a length of saveLength, the other start at 
                 * pConv->pBaseBuf + lastDisp with a length of last_blength bytes.
                 * First we have to pack the old buffer and then we should decide
                 * what we do with the new one.
                 */
                do {
                    if( iov[iov_pos].iov_base == NULL ) {
                        if( saveLength > IOVEC_MEM_LIMIT ) {
                            /* If the user didn't provide any memory, then we are free
                             * to handle this case as we want.
                             */
                            iov[iov_pos].iov_base = source;
                            iov[iov_pos].iov_len = saveLength;
                            source = pConv->pBaseBuf + lastDisp;
                            /* update the pack counters values */
                            bConverted += saveLength;
                            space -= saveLength;
                            saveLength = last_blength;
                            last_blength = 0;
                            if( ++iov_pos == (*out_size) ) goto end_loop;
                            destination = iov[iov_pos].iov_base;
                            space_on_iovec = iov[iov_pos].iov_len;
                            break;
                        }
                        /* Let's allocate some. */
                        iov[iov_pos].iov_base = pConv->memAlloc_fn( &(iov[iov_pos].iov_len),
                                                       pConv->memAlloc_userdata );
                        (*freeAfter) |= (1 << iov_pos);
                        destination = iov[iov_pos].iov_base;
                        space_on_iovec = iov[iov_pos].iov_len;
                    }
                    /* In all the others cases we simply copy as much data as possible */
                    if( space_on_iovec > saveLength ) {
                        OMPI_DDT_SAFEGUARD_POINTER( source, saveLength,
                                                    pConv->pBaseBuf, pData, pConv->count );
                        DO_DEBUG( ompi_output( 0, "3. memcpy( %p, %p, %ld ) bConverted %ld space %ld pConv->bConverted %ld\n", destination, source,
                                               saveLength, bConverted, space_on_iovec, pConv->bConverted ); );
                        MEMCPY( destination, source, saveLength );
                        destination += saveLength;
                        /* update the pack counters values */
                        bConverted += saveLength;
                        space -= saveLength;
                        space_on_iovec -= saveLength;
                        source = pConv->pBaseBuf + lastDisp;
                        saveLength = last_blength;
                        last_blength = 0;
                        break;
                    }
                    OMPI_DDT_SAFEGUARD_POINTER( source, space_on_iovec,
                                                pConv->pBaseBuf, pData, pConv->count );
                    DO_DEBUG( ompi_output( 0, "4. memcpy( %p, %p, %ld )  bConverted %ld space %ld pConv->bConverted %ld\n", destination, source,
                                           space_on_iovec, bConverted, space_on_iovec, pConv->bConverted ); );
                    MEMCPY( destination, source, space_on_iovec );
                    /* let's prepare for the next round. As I keep trace of the amount that I still
                     * have to pack, the next time when I came here, I'll try to append something.
                     * If I already fill-up the amount of data required by the upper level, I will
                     * simply save all informations in the stack, if not I'll take care of allocating
                     * new memory and packing the data inside.
                     */
                    source += space_on_iovec;
                    saveLength -= space_on_iovec;
                    /* update the pack counters values */
                    bConverted += space_on_iovec;
                    space -= space_on_iovec;
                    lastDisp += space_on_iovec;
                    /* check for the next step */
                    if( ++iov_pos == (*out_size) ) {  /* are there more iovecs to fill ? */
                        if( saveLength == 0 ) {
                            lastDisp -= space_on_iovec;
                            saveLength = last_blength;
                            last_blength = 0;
                        }
                        goto end_loop;
                    }
                    destination = iov[iov_pos].iov_base;
                    space_on_iovec = iov[iov_pos].iov_len;
                } while(1);  /* continue forever */
            }

            if( saveLength > space )  /* this will be the last element copied this time */
                continue;
            pos_desc++;  /* advance to the next data */
            lastDisp = pStack->disp + pElems[pos_desc].elem.disp;
            pack_elem.count = pElems[pos_desc].elem.count;
            last_blength = pack_elem.count * BASIC_DDT_FROM_ELEM(pElems[pos_desc])->size;
        }
    }
 end_loop:
    if( pos_desc >= 0 ) {  /* if the pack is not finish add a new entry in the stack */
        PUSH_STACK( pStack, pConv->stack_pos, pos_desc, pElems[pos_desc].elem.common.type,
		    saveLength, lastDisp, pos_desc );
    }
    assert( last_blength == 0 );
    pConv->bConverted += bConverted;  /* update the byte converted field in the convertor */
    *max_data = bConverted;      /* update the length in the iovec */
    if( ((*out_size) == iov_pos) || (iov[iov_pos].iov_base == NULL) ) *out_size = iov_pos;
    else *out_size = iov_pos + 1;
    assert( pConv->bConverted <= (pData->size * pConv->count) );
    DO_DEBUG( ompi_output( 0, "--------------------------------------------------------------------\n" ); );
    return (pConv->bConverted == (pData->size * pConv->count));
}

/* the contig versions does not use the stack. They can easily retrieve
 * the status with just the informations from pConvertor->bConverted.
 */
static int
ompi_convertor_pack_no_conv_contig( ompi_convertor_t* pConv,
                                    struct iovec* iov,
                                    uint32_t* out_size,
                                    size_t* max_data,
                                    int* freeAfter )
{
    const ompi_datatype_t* pData = pConv->pDesc;
    dt_stack_t* pStack = pConv->pStack;
    char *pSrc;
    size_t length = pData->size * pConv->count - pConv->bConverted;
    uint32_t iov_count, initial_amount = pConv->bConverted;

    *freeAfter = 0;
    /* There are some optimizations that can be done if the upper level
     * does not provide a buffer.
     */
    pSrc = pConv->pBaseBuf + pStack[0].disp + pStack[1].disp;
    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        if( 0 == length ) break;
        if( iov[iov_count].iov_len > length )
            iov[iov_count].iov_len = length;
        if( iov[iov_count].iov_base == NULL ) {
            iov[iov_count].iov_base = pSrc;
        } else {
            /* contiguous data just memcpy the smallest data in the user buffer */
            OMPI_DDT_SAFEGUARD_POINTER( pSrc, iov[iov_count].iov_len,
                                        pConv->pBaseBuf, pData, pConv->count );
            MEMCPY( iov[iov_count].iov_base, pSrc, iov[iov_count].iov_len);
        }
        length -= iov[iov_count].iov_len;
        pConv->bConverted += iov[iov_count].iov_len;
        pStack[0].disp += iov[iov_count].iov_len + pStack[1].disp;
        pSrc = pConv->pBaseBuf + pStack[0].disp;
    }
    /* The logic here should be quite simple. As the data is contiguous we will just copy data
     * (we dont have to do any conversion). Then the only thing that is interesting is to
     * be sure that the bConverted is the correct displacement. So we can always set the
     * stack[1].disp to ZERO and keep the stack[1].disp equal to bConverted (by lower bound) .
     */
    pStack[1].disp = 0;

    /* update the return value */
    *max_data = pConv->bConverted - initial_amount;
    *out_size = iov_count;
    return (0 == length);
}

static int
ompi_convertor_pack_no_conv_contig_with_gaps( ompi_convertor_t* pConv,
                                              struct iovec* iov,
                                              uint32_t* out_size,
                                              size_t* max_data,
                                              int* freeAfter )
{
    const ompi_datatype_t* pData = pConv->pDesc;
    dt_stack_t* pStack = pConv->pStack;
    char *pSrc, *pDest;
    size_t length = pData->size * pConv->count;
    long extent;
    uint32_t max_allowed = *max_data;
    uint32_t i, index;
    uint32_t iov_count, total_bytes_converted = 0;

    extent = pData->ub - pData->lb;
    assert( (pData->flags & DT_FLAG_CONTIGUOUS) && ((long)pData->size != extent) );

    i = pConv->bConverted / pData->size;  /* how many we already pack */
    pSrc = pConv->pBaseBuf + pStack->disp;  /* actual starting point for the conversion */
    
    *freeAfter = 0;
    /* There are some optimizations that can be done if the upper level
     * does not provide a buffer.
     */
    pSrc = pConv->pBaseBuf + pStack[0].disp + pStack[1].disp;
    for( iov_count = 0; iov_count < (*out_size); iov_count++ ) {
        if( iov[iov_count].iov_base == NULL ) {
            /* special case for small data. We avoid allocating memory if we
             * can fill the iovec directly with the address of the remaining
             * data.
             */
            if( (uint32_t)pStack->count < ((*out_size) - iov_count) ) {
                for( index = iov_count; i < pConv->count; i++, index++ ) {
                    iov[index].iov_base = pSrc + pStack[0].disp + pStack[1].disp;
                    iov[index].iov_len = pStack[1].count;
                    pStack[0].disp += extent;
                    total_bytes_converted += pStack[1].count;
                    pStack[1].disp = 0;  /* reset it for the next round */
                    pStack[1].count = pData->size;
                }
                *out_size = iov_count + index;
                pConv->bConverted += total_bytes_converted;
                *max_data = total_bytes_converted;
                return 1;  /* we're done */
            }
            /* now special case for big contiguous data with gaps around */
            if( pData->size >= IOVEC_MEM_LIMIT ) {
                /* as we dont have to copy any data, we can simply fill the iovecs 
                 * with data from the user data description.
                 */
                for( index = iov_count; (i < pConv->count) && (index < (*out_size));
                     i++, index++ ) {
                    if( max_allowed < pData->size ) {
                        iov[index].iov_base = pSrc;
                        iov[index].iov_len = max_allowed;
                        max_allowed = 0;
                        printf( "%s:%d Possible problem here\n", __FILE__, __LINE__ );
                        break;
                    } else {
                        iov[index].iov_base = pSrc;
                        iov[index].iov_len = pData->size;
                        pSrc += extent;
                    }
                    max_allowed -= iov[index].iov_len;
                    total_bytes_converted += iov[index].iov_len;
                }
                *out_size = index;
                *max_data = total_bytes_converted;
                pConv->bConverted += total_bytes_converted;
                return (pConv->bConverted == length );
            }
        }
        
        {
            uint32_t done, counter;
            
            if( iov[iov_count].iov_base == NULL ) {
                iov[iov_count].iov_base = pConv->memAlloc_fn( &(iov[iov_count].iov_len),
                                                 pConv->memAlloc_userdata );
                (*freeAfter) |= (1 << 0);
                if( max_allowed < iov[iov_count].iov_len )
                    iov[iov_count].iov_len = max_allowed;
                else
                    max_allowed = iov[iov_count].iov_len;
            }
            pDest = iov[iov_count].iov_base;
            done = pConv->bConverted - i * pData->size;  /* how much data left last time */
            pSrc += done;
            if( done != 0 ) {  /* still some data to copy from the last time */
                done = pData->size - done;
                OMPI_DDT_SAFEGUARD_POINTER( pSrc, done, pConv->pBaseBuf, pData, pConv->count );
                MEMCPY( pDest, pSrc, done );
                pDest += done;
                max_allowed -= done;
                i++;  /* just to compute the correct source pointer */
                total_bytes_converted += done;
            }
            pSrc = pConv->pBaseBuf + pData->true_lb + i * extent;
            counter = max_allowed / pData->size;
            if( counter > pConv->count ) counter = pConv->count;
            for( i = 0; i < counter; i++ ) {
                OMPI_DDT_SAFEGUARD_POINTER( pSrc, pData->size, pConv->pBaseBuf, pData, pConv->count );
                MEMCPY( pDest, pSrc, pData->size );
                pDest += pData->size;
                pSrc += extent;
            }
            max_allowed -= (counter * pData->size);
            iov[iov_count].iov_len -= max_allowed;
            total_bytes_converted += iov[iov_count].iov_len;
        }
        /* Now update the pSrc pointer. At the end of each parth we have to update
         * the pStack[0].disp field. BEWARE here we remove the pStack[1].disp as 
         * it's supposed to be useless from now.
         */
        pSrc = pConv->pBaseBuf + pStack[0].disp;
    }
    *max_data = total_bytes_converted;
    pConv->bConverted += total_bytes_converted;
    *out_size = iov_count;
    return (pConv->bConverted == length);
}

inline int32_t
ompi_convertor_prepare_for_send( ompi_convertor_t* convertor,
                                 const struct ompi_datatype_t* datatype,
                                 int32_t count,
                                 const void* pUserBuf )
{
    if( OMPI_SUCCESS != ompi_convertor_prepare( convertor, datatype,
                                                count, pUserBuf ) ) {
        return OMPI_ERROR;
    }

    convertor->flags |= CONVERTOR_SEND | CONVERTOR_HOMOGENEOUS;
    convertor->memAlloc_fn       = NULL;
    convertor->memAlloc_userdata = NULL;
    /* Just to avoid complaint from the compiler */
    convertor->fAdvance = ompi_convertor_pack_general;
    convertor->fAdvance = ompi_convertor_pack_homogeneous_with_memcpy;
    convertor->fAdvance = ompi_convertor_pack_no_conversion;

    if( datatype->flags & DT_FLAG_CONTIGUOUS ) {
        convertor->flags |= DT_FLAG_CONTIGUOUS;
        if( ((datatype->ub - datatype->lb) == (long)datatype->size)
            || (1 >= convertor->count) )  /* gaps or no gaps */
            convertor->fAdvance = ompi_convertor_pack_no_conv_contig;
        else
            convertor->fAdvance = ompi_convertor_pack_no_conv_contig_with_gaps;
    }
    return OMPI_SUCCESS;
}

int32_t
ompi_convertor_copy_and_prepare_for_send( const ompi_convertor_t* pSrcConv,
                                          const struct ompi_datatype_t* datatype,
                                          int32_t count,
                                          const void* pUserBuf,
                                          ompi_convertor_t* convertor )
{
    convertor->remoteArch      = pSrcConv->remoteArch;
    convertor->pFunctions      = pSrcConv->pFunctions;

    return ompi_convertor_prepare_for_send( convertor, datatype, count, pUserBuf );
}
