/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "ompi_config.h"

#include "datatype.h"
#include "datatype_internal.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <stdlib.h>

static
int ompi_convertor_pack_general( ompi_convertor_t* pConvertor,
				 struct iovec* iov, unsigned int* out_size,
				 unsigned int* max_data,
				 int* freeAfter )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int count_desc;       /* the number of items already done in the actual pos_desc */
    int type;             /* type at current position */
    unsigned int advance; /* number of bytes that we should advance the buffer */
    int rc;
    long disp_desc = 0;   /* compute displacement for truncated data */
    long disp;            /* displacement at the beging of the last loop */
    int bConverted = 0;   /* number of bytes converted this time */
    dt_desc_t *pData = pConvertor->pDesc;
    dt_elem_desc_t* pElem;
    char* pOutput = pConvertor->pBaseBuf;
    int oCount = (pData->ub - pData->lb) * pConvertor->count;
    char* pInput = iov[0].iov_base;
    int iCount = iov[0].iov_len;

    DUMP( "convertor_decode( %p, {%p, %d}, %d )\n", pConvertor,
          iov[0].iov_base, iov[0].iov_len, *out_size );
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
    while( pos_desc >= 0 ) {
	if( pElem[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
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
	    bConverted += advance;
	    if( rc != count_desc ) {
		/* not all data has been converted. Keep the state */
		count_desc -= rc;
		disp += rc * pElem[pos_desc].extent;
		if( iCount != 0 )
		    printf( "there is still room in the input buffer %d bytes\n", iCount );
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
    iov[0].iov_len = bConverted;           /* update the length in the iovec */
    *max_data = bConverted;
    /* out of the loop: we have complete the data conversion or no more space
     * in the buffer.
     */
    if( pConvertor->pStack[0].count < 0 ) return 1;  /* data succesfully converted */

    /* I complete an element, next step I should go to the next one */
    PUSH_STACK( pStack, pConvertor->stack_pos, pos_desc, count_desc,
		disp, pos_desc );

    return (pConvertor->bConverted == (pData->size * pConvertor->count));
}

/* We suppose here that we work with an already optimized version of the data
 */
static
int ompi_convertor_pack_homogeneous_with_memcpy( ompi_convertor_t* pConv,
						 struct iovec* iov,
						 unsigned int* out_size,
						 unsigned int* max_data,
						 int* freeAfter )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int type;             /* type at current position */
    int i;                /* index for basic elements with extent */
    int bConverted = 0;   /* number of bytes converted/moved this time */
    long lastDisp = 0, last_count = 0;
    int space = iov[0].iov_len, last_blength = 0;
    char* pDestBuf;
    dt_desc_t* pData = pConv->pDesc;
    dt_elem_desc_t* pElems;

    pDestBuf = iov[0].iov_base;
   
    if( pData->flags & DT_FLAG_CONTIGUOUS ) {
	long extent = pData->ub - pData->lb;
	char* pSrcBuf = pConv->pBaseBuf + pData->true_lb + pConv->bConverted;
      
	type = pConv->count * pData->size;
	if( pData->size == extent /* true extent at this point */ ) {
	    /* we can do it with just one memcpy */
	    MEMCPY( pDestBuf, pSrcBuf, iov[0].iov_len );
	    space -= iov[0].iov_len;
	    bConverted += iov[0].iov_len;
	} else {
	    for( pos_desc = 0; pos_desc < pConv->count; pos_desc++ ) {
		MEMCPY( pDestBuf, pSrcBuf, pData->size );
		space -= pData->size;
		pSrcBuf += extent;
		pDestBuf += pData->size;
	    }
	    bConverted += type;
	}
	pConv->bConverted += bConverted;
	iov[0].iov_len = bConverted;
	return (pConv->bConverted == (pData->size * pConv->count));
    }
   
    if( pData->opt_desc.desc != NULL ) {
	pElems = pData->opt_desc.desc;
    } else {
	pElems = pData->desc.desc;
    }
   
    pStack = pConv->pStack + pConv->stack_pos;
    pStack--;  /* first entry never used on this case */
    pos_desc = pStack->index;
    lastDisp = pStack->disp;
    last_count = pStack->count;
    pStack--;
    pConv->stack_pos -= 2;
   
 next_loop:
    while( pos_desc >= 0 ) {
	if( pElems[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
	    if( --(pStack->count) == 0 ) { /* end of loop */
		pStack--;
		if( --(pConv->stack_pos) == -1 ) {  /* finish everything */
		    last_count = 0;
		    pos_desc = -1;
		    goto end_loop;
		}
	    } else {
		pos_desc = pStack->index;  /* DT_LOOP index */
		if( pos_desc == -1 )
		    pStack->disp += (pData->ub - pData->lb);
		else
		    pStack->disp += pElems[pos_desc].extent;
	    }
	    pos_desc++;  /* go to the next element */
	    last_count = pElems[pos_desc].count;
	    last_blength = last_count;
	    lastDisp = pStack->disp + pElems[pos_desc].disp;
	    goto next_loop;
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
	    last_blength = last_count * basicDatatypes[pElems[pos_desc].type]->size;
	    if( space < last_blength ) {
		last_blength = last_count;
		last_count = space / basicDatatypes[pElems[pos_desc].type]->size;
		space -= (last_count * basicDatatypes[pElems[pos_desc].type]->size);
		last_blength -= last_count;
		goto end_loop;  /* or break whatever but go out of this while */
	    }
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
        MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, last_count );
        bConverted += last_count;
        lastDisp += last_count;
    }
    if( pos_desc >= 0 )  /* if the pack is not finish add a new entry in the stack */
        PUSH_STACK( pStack, pConv->stack_pos, pos_desc, last_blength,
                    lastDisp, pos_desc );
    /* fake entry just to have the same behaviour as the other cases */
    PUSH_STACK( pStack, pConv->stack_pos, 0, 0, 0, 0 );

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
int ompi_convertor_pack_homogeneous( ompi_convertor_t* pConv,
				     struct iovec* iov,
				     unsigned int *out_size,
				     unsigned int* max_data,
				     int* freeAfter )
{
    dt_stack_t* pStack;   /* pointer to the position on the stack */
    int pos_desc;         /* actual position in the description of the derived datatype */
    int i;                /* index for basic elements with extent */
    int iov_pos = 0;      /* index in the iovec that we put data inside */
    int bConverted = 0;   /* number of bytes converted/moved this time */
    int space_on_iovec;   /* amount of free space on the current iovec */
    long lastDisp = 0, last_count = 0;
    int space = *max_data, last_blength = 0, saveLength;
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
    saveLength = pStack->end_loop;
    savePos = (char*)pStack->disp;
    pStack--;
    pos_desc = pStack->index;
    last_count = pStack->count;
    lastDisp = pStack->disp;
    pStack--;
    pConv->stack_pos -= 2;

    *freeAfter = 0;
    space_on_iovec = iov[0].iov_len;
    if( iov[0].iov_base == NULL )
	space_on_iovec = 0;

 next_loop:
    while( pos_desc >= 0 ) {
	if( pElems[pos_desc].type == DT_END_LOOP ) { /* end of the current loop */
	    if( --(pStack->count) == 0 ) { /* end of loop */
		pStack--;
		if( --(pConv->stack_pos) == -1 ) {  /* finish everything */
		    if( saveLength != 0 ) { 
			/* there is still a chunk of memory to be handled, but here we dont allocate more
			 * memory. We just copy what we can in the right place and update the values to be
			 * saved on the next round.
			 */
			if( iov_pos <= (*out_size) ) {  /* still some place in the iovec */
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
				if( space_on_iovec > saveLength ) {
				    MEMCPY( pDestBuf, savePos, saveLength );
				    savePos += saveLength;
				    pDestBuf += saveLength;
				    bConverted += saveLength;
				    space_on_iovec -= saveLength;
				    saveLength = 0;
				} else {
				    MEMCPY( pDestBuf, savePos, space_on_iovec );
				    savePos += space_on_iovec;
				    pDestBuf += space_on_iovec;
				    saveLength -= space_on_iovec;
				    bConverted += space_on_iovec;
				    space_on_iovec = 0;
				}
			    }
			}
		    }
		    iov[iov_pos].iov_len -= space_on_iovec;
		    last_count = 0;
		    pos_desc = -1;
		    goto end_loop;
		}
	    } else {
		pos_desc = pStack->index;  /* DT_LOOP index */
		if( pos_desc == -1 )
		    pStack->disp += (pData->ub - pData->lb);
		else
		    pStack->disp += pElems[pos_desc].extent;
	    }
	    pos_desc++;  /* go to the next element */
	    last_count = pElems[pos_desc].count;
	    last_blength = last_count;
	    lastDisp = pStack->disp + pElems[pos_desc].disp;
	    goto next_loop;
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
		    MEMCPY( pDestBuf, pConv->pBaseBuf + lastDisp, pLast->extent );
		    lastDisp += pElems[pos_desc].extent;
		    pDestBuf += pLast->extent;
		}
		i = pLast->extent * last_count;  /* temporary value */
		space_on_iovec -= i;
		space -= i;
		bConverted += i;
		if( stop_in_loop == 0 ) {  /* did I stop before the end */
		    /* the pElems point to the LOOP struct */
		    pos_desc += pElems[pos_desc].disp + 1;
		    last_count = pElems[pos_desc].count;
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
	    last_count = pElems[pos_desc].count;
	    lastDisp = pStack->disp + pElems[pos_desc].disp;
	}
	/* now here we have a basic datatype */
	while( pElems[pos_desc].flags & DT_FLAG_DATA ) {
	    /* do we have enough space in the buffer ? */
	    last_blength = last_count * basicDatatypes[pElems[pos_desc].type]->size;

	    /* first let's see if it's contiguous with the previous chunk of memory */
	    if( (savePos + saveLength) == (pConv->pBaseBuf + lastDisp) ) {
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
		    if( saveLength > IOVEC_MEM_LIMIT ) {
			/* If the user didn't provide any memory, then we are free
			 * to handle this case how we want.
			 */
			if( iov[iov_pos].iov_base == NULL ) {
			    iov[iov_pos].iov_base = savePos;
			    iov[iov_pos].iov_len = saveLength;
			    savePos = pConv->pBaseBuf + lastDisp;
			    /* update the pack counters values */
			    bConverted += saveLength;
			    space -= saveLength;
			    saveLength = last_blength;
			    iov_pos++;
			    space_on_iovec = 0;
			    break;
			}
			/* Now the user provided some memory or we allocate some on the
			 * previous round. We are force to copy some data into the user buffer.
			 */
		    } else {
			if( iov[iov_pos].iov_base == NULL ) {  /* we have to allocate some memory */
			    iov[iov_pos].iov_base = pConv->memAlloc_fn( &(iov[iov_pos].iov_len) );
			    (*freeAfter) |= (1 << iov_pos);
			    pDestBuf = iov[iov_pos].iov_base;
			    space_on_iovec = iov[iov_pos].iov_len;
			}
		    }
		    /* In all the others cases we simply copy as much data as possible */
		    if( space_on_iovec > saveLength ) {
			MEMCPY( pDestBuf, savePos, saveLength );
			pDestBuf += saveLength;
			/* update the pack counters values */
			bConverted += saveLength;
			space -= saveLength;
			space_on_iovec -= saveLength;
			savePos = pConv->pBaseBuf + lastDisp;
			saveLength = last_blength;
			break;
		    } else {
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
			/* check for the next step */
			if( iov_pos < (*out_size) ) {
			    iov_pos++;
			    pDestBuf = iov[iov_pos].iov_base;
			    space_on_iovec = iov[iov_pos].iov_len;
			} else {
			    pDestBuf = NULL;
			    space_on_iovec = 0;
			    last_count = 0;
			    last_blength = 0;
			}
		    }
		    if( iov_pos == (*out_size) ) goto end_loop;
		} while(1);  /* continue forever */
	    }
	    if( iov_pos == (*out_size) ) goto end_loop;

	    pos_desc++;  /* advance to the next data */
	    lastDisp = pStack->disp + pElems[pos_desc].disp;
	    last_count = pElems[pos_desc].count;
	}
    }
    last_count = 0;  /* complete the data */
 end_loop:

    if( pos_desc >= 0 ) {  /* if the pack is not finish add a new entry in the stack */
	PUSH_STACK( pStack, pConv->stack_pos, pos_desc, last_blength,
		    lastDisp, pos_desc );
	if( (pConv->pBaseBuf + lastDisp) == savePos )
	    PUSH_STACK( pStack, pConv->stack_pos, 0, 0, 0, 0 );
	else
	    PUSH_STACK( pStack, pConv->stack_pos, 0, 0, (long)savePos, saveLength );
    }
   
    pConv->bConverted += bConverted;  /* update the byte converted field in the convertor */
    *max_data = bConverted;      /* update the length in the iovec */
    if( ((*out_size) == iov_pos) || (iov[iov_pos].iov_base == NULL) ) *out_size = iov_pos;
    else *out_size = iov_pos + 1;
    return (pConv->bConverted == (pData->size * pConv->count));
}

static
int ompi_convertor_pack_homogeneous_contig( ompi_convertor_t* pConv,
					    struct iovec* iov,
					    unsigned int* out_size,
					    unsigned int* max_data,
					    int* freeAfter )
{
    dt_desc_t* pData = pConv->pDesc;
    char* pSrc = pConv->pBaseBuf + pData->true_lb;
    char* pDest;
    size_t length = pData->size * pConv->count;
    long extent;
    unsigned int max_allowed = *max_data;
    int i, index;

    i = pConv->bConverted / pData->size;  /* how many we already pack */
    extent = pData->ub - pData->lb;
    pSrc += i * extent;  /* the real starting point */
    *freeAfter = 0;
    /* There are some optimizations that can be done if the upper level
     * does not provide a buffer.
     */
    if( iov[0].iov_base == NULL ) {
	/* special case for small data. We avoid allocating memory if we
	 * can fill the iovec directly with the address of the remaining
	 * data.
	 */
	if( (pConv->count - i) < (*out_size) ) {
	    for( index = 0; i < pConv->count; i++, index++ ) {
		iov[index].iov_base = pSrc;
		iov[index].iov_len = pData->size;
		pSrc += extent;
	    }
	    *out_size = index;
	    *max_data = index * pData->size;
	    return 1;  /* we're done */
	}
	/* now special case for big contiguous data with gaps around */
	if( pData->size >= IOVEC_MEM_LIMIT ) {
	    /* as we dont have to copy any data, we can simply fill the iovecs 
	     * with data from the user data description.
	     */
	    for( index = 0; (i < pConv->count) && (index < (*out_size));
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
	    }
	    *out_size = index;
	    *max_data = (*max_data) - max_allowed;
	    pConv->bConverted += (*max_data);
	    return (pConv->bConverted == length );
	}
    }

    if( pData->size == extent ) {  /* that really contiguous */
	if( iov[0].iov_base == NULL ) {
	    iov[0].iov_base = pSrc; /* + pConv->bConverted; */
	    if( (pConv->bConverted + iov[0].iov_len) > length )
		iov[0].iov_len = length - pConv->bConverted;
	} else {
	    /* contiguous data just memcpy the smallest data in the user buffer */
	    iov[0].iov_len = IMIN( iov[0].iov_len, length );
	    MEMCPY( iov[0].iov_base, pSrc, iov[0].iov_len);
	}
	*max_data = iov[0].iov_len;
    } else {
	int done, counter;

	if( iov[0].iov_base == NULL ) {
	    iov[0].iov_base = pConv->memAlloc_fn( &(iov[0].iov_len) );
	    (*freeAfter) |= (1 << 0);
	    if( max_allowed < iov[0].iov_len )
		iov[0].iov_len = max_allowed;
	    else
		max_allowed = iov[0].iov_len;
	}
	pDest = iov[0].iov_base;
	done = pConv->bConverted - i * pData->size;  /* how much data left last time */
	pSrc += done;
	if( done != 0 ) {  /* still some data to copy from the last time */
	    done = pData->size - done;
	    MEMCPY( pDest, pSrc, done );
	    pDest += done;
	    max_allowed -= done;
	    i++;  /* just to compute the correct source pointer */
	}
	pSrc = pConv->pBaseBuf + pData->true_lb + i * extent;
	counter = max_allowed / pData->size;
	if( counter > pConv->count ) counter = pConv->count;
	for( i = 0; i < counter; i++ ) {
	    MEMCPY( pDest, pSrc, pData->size );
	    pDest += pData->size;
	    pSrc += extent;
	}
	max_allowed -= (counter * pData->size);
	*max_data = iov[0].iov_len - max_allowed;
	iov[0].iov_len = *max_data;
    }
    pConv->bConverted += iov[0].iov_len;
    *out_size = 1;
    return (pConv->bConverted == length);
}

/* The pack routines should do 2 things:
 * - first if the provided iovec contains NULL pointers then they should provide
 *   buffer space. If the data is contiguous the it should provide directly pointers
 *   the the user space depending on the iov_len argument. If -1 then all the buffer
 *   can be supplied in one time, if not several steps need to be executed, it should
 *   provide the correct pointer every time. But if the user provide a buffer, then
 *   some parts of the data should be packed inside this buffer, but we still should
 *   able to have pointers to the user buf on the subsequents calls.
 *
 *   The iovec provided by the upper level can have several meanings:
 *   - the iov_base field contain a address not NULL, the user have provided some memory.
 *     Then the iov_len field should be not empty too, and we have to respect the high
 *     level requirements.
 *   - if iov_base of the first iovec is NULL then the iov_len provided in the first iovec
 *     is the maximum amount of data that we will pack. If this field is set to zero,
 *     then we compute this maximum using the convertor and the amount of data already
 *     packed.
 *
 * Return 0 if everything went OK and if there is still room before the complete
 *          conversion of the data (need additional call with others input buffers )
 *        1 if everything went fine and the data was completly converted
 *       -1 something wrong occurs.
 */
int ompi_convertor_pack( ompi_convertor_t* pConv,
			 struct iovec* iov, 
			 unsigned int* out_size,
			 unsigned int* max_data,
			 int* freeAfter )
{
    dt_desc_t* pData = pConv->pDesc;
    int done = 0, index = 0;

    *freeAfter = 0;  /* nothing to free yet */
    /* TODO should use the remote size */
    if( pConv->bConverted == (pData->size * pConv->count) ) {  /* conversion completed or nothing to do */
	iov[0].iov_len = 0;
	*out_size = 0;
	return 1;  /* nothing to do */
    }
    while( index < (*out_size)) {
	if( iov[index].iov_len == 0 ) {
	    assert( iov[index].iov_base == NULL );
	    iov[index].iov_len = pConv->count * pData->size - pConv->bConverted;
	}
#if defined(ONE_STEP)
	{
	    int howMany = 1;
	    if( iov[index].iov_base == NULL ) {
		iov[index].iov_base = pConv->memAlloc_fn( &(iov[index].iov_len) );
		(*freeAfter) |= (1 << index);
	    }
	    done = convertor_progress( pConv, &(iov[index]), &howMany, max_data, freeAfter );
	    index++;
	}
#else
	/* We dont allocate any memory. The packing function should allocate it
	 * if it need. If it's possible to find iovec in the derived datatype
	 * description then we dont have to allocate any memory.
	 */
	done = ompi_convertor_progress( pConv, &(iov[index]), out_size, max_data, freeAfter );
	index += (*out_size);
#endif  /* ONE_STEP */
	if( done == 1 ) break;
    }
    *out_size = index;
    return done;
}

extern int local_sizes[DT_MAX_PREDEFINED];
int ompi_convertor_init_for_send( ompi_convertor_t* pConv,
				  unsigned int flags,
				  dt_desc_t* dt,
				  int count,
				  void* pUserBuf,
				  int starting_pos,
				  memalloc_fct_t allocfn )
{

    OBJ_RETAIN( dt );
    if( pConv->pDesc != dt ) {
        pConv->pDesc = dt;
        if( pConv->pStack != NULL ) free( pConv->pStack );
        pConv->pStack = NULL;
    }

    if( pConv->pStack == NULL ) {
        pConv->pStack = (dt_stack_t*)malloc(sizeof(dt_stack_t) * (dt->btypes[DT_LOOP] + 3) );
        pConv->stack_pos = 0;  /* just to be sure */
    }

    pConv->flags = CONVERTOR_SEND;
    pConv->pBaseBuf = pUserBuf;
    pConv->available_space = count * (dt->ub - dt->lb);
    pConv->count = count;
    pConv->pFunctions = copy_functions;
    pConv->converted = 0;
    pConv->bConverted = 0;
    pConv->memAlloc_fn = allocfn;
    if( dt->flags & DT_FLAG_CONTIGUOUS ) {
	pConv->flags |= DT_FLAG_CONTIGUOUS;
	pConv->fAdvance = ompi_convertor_pack_homogeneous_contig;
    } else {
	/* TODO handle the sender convert case */
	pConv->fAdvance = ompi_convertor_pack_general;
	pConv->fAdvance = ompi_convertor_pack_homogeneous_with_memcpy;
	pConv->fAdvance = ompi_convertor_pack_homogeneous;
#if defined(ONE_STEP)
	pConv->fAdvance = ompi_convertor_pack_homogeneous_with_memcpy;
#endif  /* ONE_STEP */
    }
    if( starting_pos != 0 ) {
	return ompi_convertor_create_stack_with_pos( pConv, starting_pos, local_sizes );
    }
    return ompi_convertor_create_stack_at_begining( pConv, local_sizes );
}

ompi_convertor_t* ompi_convertor_create( int remote_arch, int mode )
{
   ompi_convertor_t* pConv = OBJ_NEW(ompi_convertor_t);

   pConv->pDesc       = NULL;
   pConv->pStack      = NULL;
   pConv->remoteArch  = remote_arch;
   pConv->fAdvance    = NULL;
   pConv->memAlloc_fn = NULL;
   return pConv;
}

static void ompi_convertor_construct( ompi_convertor_t* pConv )
{
    pConv->pDesc = NULL;
    pConv->pStack = NULL;
    pConv->fAdvance = NULL;
    pConv->memAlloc_fn = NULL;
}

static void ompi_convertor_destruct( ompi_convertor_t* pConv )
{
   if( pConv->pStack != NULL ) free( pConv->pStack );
   pConv->pStack = NULL;
   if( pConv->pDesc != NULL ) OBJ_RELEASE( pConv->pDesc );
   pConv->pDesc = NULL;
}

OBJ_CLASS_INSTANCE(ompi_convertor_t, ompi_object_t, ompi_convertor_construct, ompi_convertor_destruct );

inline int ompi_convertor_copy( ompi_convertor_t* pSrcConv, ompi_convertor_t* pDestConv )
{
   pDestConv->remoteArch = pSrcConv->remoteArch;
   pDestConv->flags = pSrcConv->flags;
   pDestConv->pFunctions = pSrcConv->pFunctions;

   pDestConv->pStack      = NULL;
   pDestConv->stack_pos   = 0;
   pDestConv->pDesc       = NULL;
   pDestConv->count       = 0;
   pDestConv->converted   = 0;
   pDestConv->bConverted  = 0;
   pDestConv->fAdvance    = NULL;
   pDestConv->memAlloc_fn = NULL;
   return OMPI_SUCCESS;
}

ompi_convertor_t* ompi_convertor_get_copy( ompi_convertor_t* pConvertor )
{
   ompi_convertor_t* pDestConv = OBJ_NEW(ompi_convertor_t);
   (void)ompi_convertor_copy( pConvertor, pDestConv );
   return pDestConv;
}

/* Actually we suppose that we can only do receiver side conversion */
int ompi_convertor_get_packed_size( ompi_convertor_t* pConv, unsigned int* pSize )
{
   if( ompi_ddt_type_size( pConv->pDesc, pSize ) != 0 )
      return OMPI_ERROR;
   /* actually *pSize contain the size of one instance of the data */
   *pSize = (*pSize) * pConv->count;
   return OMPI_SUCCESS;
}

int ompi_convertor_get_unpacked_size( ompi_convertor_t* pConv, unsigned int* pSize )
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
         *pSize += (pData->btypes[i] * basicDatatypes[i]->size);
      }
   }
   *pSize *= pConv->count;
   return OMPI_SUCCESS;
}
