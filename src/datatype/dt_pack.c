/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

static
int ompi_convertor_pack_general( ompi_convertor_t* pConvertor,
				 struct iovec* iov, uint32_t* out_size,
				 uint32_t* max_data,
				 int32_t* freeAfter )
{
    dt_stack_t* pStack;    /* pointer to the position on the stack */
    uint32_t pos_desc;     /* actual position in the description of the derived datatype */
    int count_desc;        /* the number of items already done in the actual pos_desc */
    int type;              /* type at current position */
    uint32_t advance;      /* number of bytes that we should advance the buffer */
    long disp_desc = 0;    /* compute displacement for truncated data */
    int bConverted = 0;    /* number of bytes converted this time */
    dt_desc_t *pData = pConvertor->pDesc;
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
            iov[iov_count].iov_base = pConvertor->memAlloc_fn( &(iov[iov_count].iov_len) );
            *freeAfter = (*freeAfter) | ( 1 << iov_count);
        }
        pInput = iov[iov_count].iov_base;
        iCount = iov[iov_count].iov_len;
        while( 1 ) {
            if( pElem[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
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
                        pStack->disp += pElem[pStack->index].extent;
                    }
                }
                count_desc = pElem[pos_desc].count;
                disp_desc = pElem[pos_desc].disp;
            }
            if( pElem[pos_desc].type == DT_LOOP ) {
                do {
                    PUSH_STACK( pStack, pConvertor->stack_pos,
                                pos_desc, pElem[pos_desc].count,
                                pStack->disp, pos_desc + pElem[pos_desc].disp + 1);
                    pos_desc++;
                } while( pElem[pos_desc].type == DT_LOOP ); /* let's start another loop */
                DDT_DUMP_STACK( pConvertor->pStack, pConvertor->stack_pos, pElem, "advance loops" );
                /* update the current state */
                count_desc = pElem[pos_desc].count;
                disp_desc = pElem[pos_desc].disp;
                continue;
            }
            while( pElem[pos_desc].flags & DT_FLAG_DATA ) {
                /* now here we have a basic datatype */
                type = pElem[pos_desc].type;
                rc = pConvertor->pFunctions[type]( count_desc,
                                                   pOutput + pStack->disp + disp_desc,
                                                   iCount, pElem[pos_desc].extent,
                                                   pInput, iCount, ompi_ddt_basicDatatypes[type]->size );
                advance = rc * ompi_ddt_basicDatatypes[type]->size;
                iCount -= advance;      /* decrease the available space in the buffer */
                pInput += advance;      /* increase the pointer to the buffer */
                bConverted += advance;
                if( rc != count_desc ) {
                    /* not all data has been converted. Keep the state */
                    count_desc -= rc;
                    disp_desc += rc * pElem[pos_desc].extent;
                    if( iCount != 0 )
                        printf( "pack there is still room in the input buffer %d bytes\n", iCount );
                    goto complete_loop;
                }
                pConvertor->converted += rc;  /* number of elementd converted so far */
                pos_desc++;  /* advance to the next data */
                count_desc = pElem[pos_desc].count;
                disp_desc = pElem[pos_desc].disp;
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
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, count_desc,
		disp_desc, pos_desc );

    return (pConvertor->bConverted == (pData->size * pConvertor->count));
}

/* We suppose here that we work with an already optimized version of the data
 */
static
int ompi_convertor_pack_homogeneous_with_memcpy( ompi_convertor_t* pConv,
						 struct iovec* iov,
						 uint32_t* out_size,
						 uint32_t* max_data,
						 int* freeAfter )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    uint32_t pos_desc;    /* actual position in the description of the derived datatype */
    int i;                /* index for basic elements with extent */
    int bConverted = 0;   /* number of bytes converted/moved this time */
    long lastDisp = 0, last_count = 0;
    uint32_t space = iov[0].iov_len, last_blength = 0;
    char* pDestBuf;
    dt_desc_t* pData = pConv->pDesc;
    dt_elem_desc_t* pElems;

    pDestBuf = iov[0].iov_base;
   
    if( pData->opt_desc.desc != NULL ) {
	pElems = pData->opt_desc.desc;
    } else {
	pElems = pData->desc.desc;
    }
   
    pStack = pConv->pStack + pConv->stack_pos;
    pos_desc = pStack->index;
    lastDisp = pStack->disp;
    last_count = pStack->count;
    pStack--;
    pConv->stack_pos--;
   
    while( 1 ) {
	if( pElems[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
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
		    pStack->disp += pElems[pos_desc].extent;
                    pos_desc = pStack->index + 1;
                }
	    }
	    last_count = pElems[pos_desc].count;
	    last_blength = last_count;
	    lastDisp = pStack->disp + pElems[pos_desc].disp;
	    continue;
	}
	while( pElems[pos_desc].type == DT_LOOP ) {
	    int stop_in_loop = 0;
	    if( pElems[pos_desc].flags & DT_FLAG_CONTIGUOUS ) {
		dt_elem_desc_t* pLast = &(pElems[pos_desc + pElems[pos_desc].disp]);
		if( (pLast->extent * last_count) > space ) {
		    stop_in_loop = last_count;
		    last_count = space / pLast->extent;
		}
		for( i = 0; i < last_count; i++ ) {
                    OMPI_DDT_SAFEGUARD_POINTER( pConv->pBaseBuf + lastDisp, pLast->extent,
                                                pConv->pBaseBuf, pData, pConv->count );
		    MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, pLast->extent );
		    pDestBuf += pLast->extent;  /* size of the contiguous data */
		    lastDisp += pElems[pos_desc].extent;
		}
		space -= (pLast->extent * last_count);
		bConverted += (pLast->extent * last_count);
		if( stop_in_loop == 0 ) {
		    pos_desc += pElems[pos_desc].disp + 1;
		    last_count = pElems[pos_desc].count;
		    continue;
		}
		/* mark some of the iterations as completed */
		last_count = stop_in_loop - last_count;
		last_blength = 0;
		/* Save the stack with the correct last_count value. */
	    }
	    PUSH_STACK( pStack, pConv->stack_pos, pos_desc, last_count,
			pStack->disp, pos_desc + pElems[pos_desc].disp );
	    pos_desc++;
	    last_count = pElems[pos_desc].count;
	}
	/* now here we have a basic datatype */
	while( pElems[pos_desc].flags & DT_FLAG_DATA ) {
	    /* do we have enough space in the buffer ? */
	    last_blength = last_count * ompi_ddt_basicDatatypes[pElems[pos_desc].type]->size;
	    if( space < last_blength ) {
		last_blength = last_count;
		last_count = space / ompi_ddt_basicDatatypes[pElems[pos_desc].type]->size;
		space -= (last_count * ompi_ddt_basicDatatypes[pElems[pos_desc].type]->size);
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
	    lastDisp = pStack->disp + pElems[pos_desc].disp;
	    last_count = pElems[pos_desc].count;
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
    PUSH_STACK( pStack, pConv->stack_pos, pos_desc, last_blength, lastDisp, pos_desc );

    pConv->bConverted += bConverted;  /* update the byte converted field in the convertor */
    iov[0].iov_len = bConverted;      /* update the length in the iovec */
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
                                       uint32_t* max_data,
                                       int* freeAfter )
{
    dt_stack_t* pStack;       /* pointer to the position on the stack */
    int pos_desc;             /* actual position in the description of the derived datatype */
    int i;                    /* index for basic elements with extent */
    uint32_t iov_pos = 0;     /* index in the iovec where we put data inside */
    int bConverted = 0;       /* number of bytes converted/moved this time */
    uint32_t space_on_iovec;  /* amount of free space on the current iovec */
    long lastDisp = 0, last_count = 0;
    uint32_t space = *max_data, last_blength = 0, saveLength;
    char *pDestBuf, *savePos;
    dt_desc_t* pData = pConv->pDesc;
    dt_elem_desc_t* pElems;

    if( pData->opt_desc.desc != NULL ) {
        pElems = pData->opt_desc.desc;
    } else {
        pElems = pData->desc.desc;
    }

    pDestBuf = iov[0].iov_base;
    pStack = pConv->pStack + pConv->stack_pos;
   
    /* retrieve the context of the last call */
    pos_desc = pStack->index;
    last_count = pStack->count;
    last_blength = last_count * ompi_ddt_basicDatatypes[pElems[pos_desc].type]->size;
    lastDisp = pStack->disp;
    savePos = (char*)pConv->pBaseBuf + pStack->disp;
    saveLength = 0;
    pStack--;
    pConv->stack_pos--;

    *freeAfter = 0;
    space_on_iovec = iov[0].iov_len;

    while( pos_desc >= 0 ) {
        if( pElems[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
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
                                iov[iov_pos].iov_base = savePos;
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
                                OMPI_DDT_SAFEGUARD_POINTER( savePos, copy_length,
                                                            pConv->pBaseBuf, pData, pConv->count );
                                MEMCPY( pDestBuf, savePos, copy_length );
                                savePos += copy_length;
                                pDestBuf += copy_length;
                                bConverted += copy_length;
                                space_on_iovec -= copy_length;
                                saveLength -= copy_length;
                            }
                        }
                    }
                    iov[iov_pos].iov_len -= space_on_iovec;
                    last_count = 0;
                    pos_desc = -1;
                    goto end_loop;
                }
                pConv->stack_pos--;
                pStack--;
            } else {
                pos_desc = pStack->index;  /* DT_LOOP index */
                if( pos_desc == -1 )
                    pStack->disp += (pData->ub - pData->lb);
                else
                    pStack->disp += pElems[pos_desc].extent;
            }
            pos_desc++;  /* go to the next element */
            lastDisp = pStack->disp + pElems[pos_desc].disp;
            last_count = pElems[pos_desc].count;
            last_blength = last_count * ompi_ddt_basicDatatypes[pElems[pos_desc].type]->size;
            continue;  /* next loop */
        }
        while( pElems[pos_desc].type == DT_LOOP ) {
            int stop_in_loop = 0;

            /* If the loop container is contiguous then we can do some
             * optimizations.
             */
            if( pElems[pos_desc].flags & DT_FLAG_CONTIGUOUS ) {
                /* point to the end of loop element */
                dt_elem_desc_t* pLast = &(pElems[pos_desc + pElems[pos_desc].disp]);
                if( iov[iov_pos].iov_base == NULL ) {
                    iov[iov_pos].iov_base = pConv->memAlloc_fn( &(iov[iov_pos].iov_len) );
                    space_on_iovec = iov[iov_pos].iov_len;
                    pDestBuf = iov[iov_pos].iov_base;
                    (*freeAfter) |= (1 << iov_pos);
                }
                /* compute the maximum amount of data to be packed */
                if( (pLast->extent * last_count) > space_on_iovec ) {
                    stop_in_loop = last_count;
                    last_count = space_on_iovec / pLast->extent;
                }
                /* Now let's do it */
                for( i = 0; i < last_count; i++ ) {
                    OMPI_DDT_SAFEGUARD_POINTER( pConv->pBaseBuf + lastDisp, pLast->extent,
                                                pConv->pBaseBuf, pData, pConv->count );
                    MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, pLast->extent );
                    lastDisp += pElems[pos_desc].extent;
                    pDestBuf += pLast->extent;
                }
                i = pLast->extent * last_count;  /* temporary value */
                space_on_iovec -= i;
                space          -= i;
                bConverted     += i;
                if( stop_in_loop == 0 ) {  /* did I stop before the end */
                    /* the pElems point to the LOOP struct */
                    pos_desc += pElems[pos_desc].disp + 1;
                    last_count = pElems[pos_desc].count;
                    last_blength = last_count * ompi_ddt_basicDatatypes[pElems[pos_desc].type]->size;
                    lastDisp = pStack->disp + pElems[pos_desc].disp;
                    continue;
                }
                /* mark some of the iterations as completed */
                last_count = stop_in_loop - last_count;
                last_blength = 0;
                /* Save the stack with the correct last_count value. */
            }
            PUSH_STACK( pStack, pConv->stack_pos, pos_desc, last_count,
                        pStack->disp, pos_desc + pElems[pos_desc].disp );
            pos_desc++;
            lastDisp = pStack->disp + pElems[pos_desc].disp;
            last_count = pElems[pos_desc].count;
            last_blength = last_count * ompi_ddt_basicDatatypes[pElems[pos_desc].type]->size;
        }
        /* now here we have a basic datatype */
        while( pElems[pos_desc].flags & DT_FLAG_DATA ) {
            /* first let's see if it's contiguous with the previous chunk of memory and
             * we still have enough room in the buffer...
             */
            if( ((savePos + saveLength) == (pConv->pBaseBuf + lastDisp))
                && ((saveLength + last_blength) < space_on_iovec) ) {
                /* ok still contiguous */
                saveLength += last_blength;
                /* nothing else to do, we act the next time */
            } else {
                /* Now we have 2 piece of non contiguous memory. One start at savePos
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
                            iov[iov_pos].iov_base = savePos;
                            iov[iov_pos].iov_len = saveLength;
                            savePos = pConv->pBaseBuf + lastDisp;
                            /* update the pack counters values */
                            bConverted += saveLength;
                            space -= saveLength;
                            saveLength = last_blength;
                            if( ++iov_pos == (*out_size) ) goto end_loop;
                            last_blength = 0;
                            pDestBuf = iov[iov_pos].iov_base;
                            space_on_iovec = iov[iov_pos].iov_len;
                            break;
                        }
                        /* Let's allocate some. */
                        iov[iov_pos].iov_base = pConv->memAlloc_fn( &(iov[iov_pos].iov_len) );
                        (*freeAfter) |= (1 << iov_pos);
                        pDestBuf = iov[iov_pos].iov_base;
                        space_on_iovec = iov[iov_pos].iov_len;
                    }
                    /* In all the others cases we simply copy as much data as possible */
                    if( space_on_iovec > saveLength ) {
                        OMPI_DDT_SAFEGUARD_POINTER( savePos, saveLength,
                                                    pConv->pBaseBuf, pData, pConv->count );
                        MEMCPY( pDestBuf, savePos, saveLength );
                        pDestBuf += saveLength;
                        /* update the pack counters values */
                        bConverted += saveLength;
                        space -= saveLength;
                        space_on_iovec -= saveLength;
                        savePos = pConv->pBaseBuf + lastDisp;
                        saveLength = last_blength;
                        last_blength = 0;
                        break;
                    }
                    OMPI_DDT_SAFEGUARD_POINTER( savePos, space_on_iovec,
                                                pConv->pBaseBuf, pData, pConv->count );
                    MEMCPY( pDestBuf, savePos, space_on_iovec );
                    /* let's prepare for the next round. As I keep trace of the amount that I still
                     * have to pack, the next time when I came here, I'll try to append something.
                     * If I already fill-up the amount of data required by the upper level, I will
                     * simply save all informations in the stack, if not I'll take care of allocating
                     * new memory and packing the data inside.
                     */
                    savePos += space_on_iovec;
                    saveLength -= space_on_iovec;
                    /* update the pack counters values */
                    bConverted += space_on_iovec;
                    space -= space_on_iovec;
                    lastDisp += space_on_iovec;
                    /* check for the next step */
                    if( ++iov_pos == (*out_size) ) {  /* are there more iovecs to fill ? */
                        pDestBuf = NULL;
                        space_on_iovec = 0;
                        last_count = 0;
                        last_blength = 0;
                        goto end_loop;
                    }
                    pDestBuf = iov[iov_pos].iov_base;
                    space_on_iovec = iov[iov_pos].iov_len;
                } while(1);  /* continue forever */
            }
            if( saveLength > space )  /* this will be the last element copied this time */
                continue;
            pos_desc++;  /* advance to the next data */
            lastDisp = pStack->disp + pElems[pos_desc].disp;
            last_count = pElems[pos_desc].count;
            last_blength = last_count * ompi_ddt_basicDatatypes[pElems[pos_desc].type]->size;
	}
    }
 end_loop:
    if( pos_desc >= 0 ) {  /* if the pack is not finish add a new entry in the stack */
	PUSH_STACK( pStack, pConv->stack_pos, pos_desc, saveLength, lastDisp, pos_desc );
    }
    assert( last_blength == 0 );
    pConv->bConverted += bConverted;  /* update the byte converted field in the convertor */
    *max_data = bConverted;      /* update the length in the iovec */
    if( ((*out_size) == iov_pos) || (iov[iov_pos].iov_base == NULL) ) *out_size = iov_pos;
    else *out_size = iov_pos + 1;
    assert( pConv->bConverted <= (pData->size * pConv->count) );
    return (pConv->bConverted == (pData->size * pConv->count));
}

/* the contig versions does not use the stack. They can easily retrieve
 * the status with just the informations from pConvertor->bConverted.
 */
static int
ompi_convertor_pack_no_conv_contig( ompi_convertor_t* pConv,
                                    struct iovec* iov,
                                    uint32_t* out_size,
                                    uint32_t* max_data,
                                    int* freeAfter )
{
    dt_desc_t* pData = pConv->pDesc;
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
        if( iov[iov_count].iov_base == NULL ) {
            iov[iov_count].iov_base = pSrc;
            if( iov[iov_count].iov_len > length )
                iov[iov_count].iov_len = length;
        } else {
            /* contiguous data just memcpy the smallest data in the user buffer */
            iov[iov_count].iov_len = IMIN( iov[iov_count].iov_len, length );
            OMPI_DDT_SAFEGUARD_POINTER( pSrc, iov[iov_count].iov_len,
                                        pConv->pBaseBuf, pData, pConv->count );
            MEMCPY( iov[iov_count].iov_base, pSrc, iov[iov_count].iov_len);
        }
	length -= iov[iov_count].iov_len;
        pConv->bConverted += iov[iov_count].iov_len;
        pStack[0].disp += iov[iov_count].iov_len;
        pSrc = pConv->pBaseBuf + pStack[0].disp;
        if( 0 == length ) break;
    }
    /* the number of complete datatypes still to be copied */
    pStack[0].count = pConv->count - (pConv->bConverted / pData->size);
    /* the amount of data (in bytes) that still have to be done on the last data */
    pStack[1].count = pConv->bConverted - pData->size * (pConv->count - pStack[0].count);
    pStack[1].disp  = pData->size - pStack[1].count;
    pStack[0].disp -= (pData->size - pStack[1].count);
    /* update the return value */
    *max_data = pConv->bConverted - initial_amount;
    *out_size = iov_count;
    return (pConv->bConverted == (pData->size * pConv->count));
}

static int
ompi_convertor_pack_no_conv_contig_with_gaps( ompi_convertor_t* pConv,
                                              struct iovec* iov,
                                              uint32_t* out_size,
                                              uint32_t* max_data,
                                              int* freeAfter )
{
    dt_desc_t* pData = pConv->pDesc;
    dt_stack_t* pStack = pConv->pStack;
    char *pSrc, *pDest;
    size_t length = pData->size * pConv->count;
    long extent;
    uint32_t max_allowed = *max_data;
    uint32_t i, index;
    uint32_t iov_count, total_bytes_converted = 0;

    i = pConv->bConverted / pData->size;  /* how many we already pack */
    extent = pData->ub - pData->lb;
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
        
        if( (long)pData->size == extent ) {  /* that really contiguous */
            if( iov[iov_count].iov_base == NULL ) {
                iov[iov_count].iov_base = pSrc;
                if( (pConv->bConverted + iov[iov_count].iov_len) > length )
                    iov[iov_count].iov_len = length - pConv->bConverted;
            } else {
                /* contiguous data just memcpy the smallest data in the user buffer */
                iov[iov_count].iov_len = IMIN( iov[iov_count].iov_len, length );
                OMPI_DDT_SAFEGUARD_POINTER( pSrc, iov[iov_count].iov_len,
                                            pConv->pBaseBuf, pData, pConv->count );
                MEMCPY( iov[iov_count].iov_base, pSrc, iov[iov_count].iov_len);
            }
            total_bytes_converted += iov[iov_count].iov_len;
            total_bytes_converted += iov[iov_count].iov_len;
        } else {
            uint32_t done, counter;
            
            if( iov[iov_count].iov_base == NULL ) {
                iov[iov_count].iov_base = pConv->memAlloc_fn( &(iov[iov_count].iov_len) );
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

extern int ompi_ddt_local_sizes[DT_MAX_PREDEFINED];
int ompi_convertor_init_for_send( ompi_convertor_t* pConv,
				  uint32_t flags,
				  const dt_desc_t* datatype,
				  int count,
				  const void* pUserBuf,
				  int starting_pos,
				  memalloc_fct_t allocfn )
{
    if( !(datatype->flags & DT_FLAG_COMMITED) ) {
        /* this datatype is improper for conversion. Commit it first */
        return OMPI_ERROR;
    }
    assert( datatype != NULL );
    convertor_init_generic( pConv, datatype, count, pUserBuf );

    pConv->flags = CONVERTOR_SEND | CONVERTOR_HOMOGENEOUS;  /* by default set to homogeneous */
    pConv->pFunctions      = ompi_ddt_copy_functions;
    pConv->memAlloc_fn     = allocfn;
    /* Just to avoid complaint from the compiler */
    pConv->fAdvance = ompi_convertor_pack_general;
    pConv->fAdvance = ompi_convertor_pack_homogeneous_with_memcpy;
    if( datatype->flags & DT_FLAG_CONTIGUOUS ) {
        pConv->flags |= DT_FLAG_CONTIGUOUS;
        if( (datatype->ub - datatype->lb) == (long)datatype->size )
            pConv->fAdvance = ompi_convertor_pack_no_conv_contig;
        else
            pConv->fAdvance = ompi_convertor_pack_no_conv_contig_with_gaps;
        return ompi_convertor_create_stack_with_pos_contig( pConv, starting_pos, ompi_ddt_local_sizes );
    }
    pConv->fAdvance = ompi_convertor_pack_no_conversion;
    if( starting_pos != 0 ) {
        return ompi_convertor_create_stack_with_pos_general( pConv, starting_pos, ompi_ddt_local_sizes );
    }
    return ompi_convertor_create_stack_at_begining( pConv, ompi_ddt_local_sizes );
}

ompi_convertor_t* ompi_convertor_create( int remote_arch, int mode )
{
   ompi_convertor_t* pConv = OBJ_NEW(ompi_convertor_t);

   pConv->remoteArch  = remote_arch;
   return pConv;
}

static void ompi_convertor_construct( ompi_convertor_t* pConv )
{
    pConv->pDesc       = NULL;
    pConv->pStack      = pConv->static_stack;
    pConv->stack_size  = DT_STATIC_STACK_SIZE;
    pConv->fAdvance    = NULL;
    pConv->memAlloc_fn = NULL;
}

static void ompi_convertor_destruct( ompi_convertor_t* pConv )
{
    if( pConv->stack_size > DT_STATIC_STACK_SIZE ) {
        free( pConv->pStack );
    }

    if( pConv->pDesc != NULL ) OBJ_RELEASE( pConv->pDesc );
    pConv->pDesc = NULL;
}

OBJ_CLASS_INSTANCE(ompi_convertor_t, ompi_object_t, ompi_convertor_construct, ompi_convertor_destruct );

/* Actually we suppose that we can only do receiver side conversion */
int ompi_convertor_get_packed_size( const ompi_convertor_t* pConv, uint32_t* pSize )
{
   int ddt_size = 0;

   if( ompi_ddt_type_size( pConv->pDesc, &ddt_size ) != 0 )
      return OMPI_ERROR;
   /* actually *pSize contain the size of one instance of the data */
   *pSize = ddt_size * pConv->count;
   return OMPI_SUCCESS;
}

int ompi_convertor_get_unpacked_size( const ompi_convertor_t* pConv, uint32_t* pSize )
{
   int i;
   dt_desc_t* pData = pConv->pDesc;

   if( pConv->count == 0 ) {
      *pSize = 0;
      return OMPI_SUCCESS;
   }
   if( pConv->remoteArch == 0 ) {  /* same architecture */
      *pSize = pData->size * pConv->count;
      return OMPI_SUCCESS;
   }
   *pSize = 0;
   for( i = DT_CHAR; i < DT_MAX_PREDEFINED; i++ ) {
      if( pData->bdt_used & (((unsigned long long)1)<<i) ) {
         /* TODO replace with the remote size */
         *pSize += (pData->btypes[i] * ompi_ddt_basicDatatypes[i]->size);
      }
   }
   *pSize *= pConv->count;
   return OMPI_SUCCESS;
}
