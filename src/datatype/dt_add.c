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
#include "datatype/datatype_internal.h"

/* macros to play with the flags */
#define SET_CONTIGUOUS_FLAG( INT_VALUE )     (INT_VALUE) = (INT_VALUE) | (DT_FLAG_CONTIGUOUS)
#define UNSET_CONTIGUOUS_FLAG( INT_VALUE )   (INT_VALUE) = (INT_VALUE) & (~(DT_FLAG_CONTIGUOUS))

#if defined(__GNUC__) && !defined(__STDC__)
#define LMAX(A,B)  ({ long _a = (A), _b = (B); (_a < _b ? _b : _a) })
#define LMIN(A,B)  ({ long _a = (A), _b = (B); (_a < _b ? _a : _b); })
#define IMAX(A,B)  ({ int _a = (A), _b = (B); (_a < _b ? _b : _a); })
#define IMIN(A,B)  ({ int _a = (A), _b = (B); (_a < _b ? _a : _b); })
#else
static inline long LMAX( long a, long b ) { return ( a < b ? b : a ); }
static inline long LMIN( long a, long b ) { return ( a < b ? a : b ); }
static inline int  IMAX( int a, int b ) { return ( a < b ? b : a ); }
static inline int  IMIN( int a, int b ) { return ( a < b ? a : b ); }
#endif  /* __GNU__ */

/* When we add a datatype we should update it's definition depending on
 * the initial displacement for the whole data, so the displacement of
 * all elements inside a datatype depend only on the loop displacement
 * and it's own displacement.
 */

/* we have 3 differents structures to update:
 * the first is the real representation of the datatype
 * the second is the internal representation using extents
 * the last is the representation used for send operations
 * If the count is ZERO we dont have to add the pdtAdd datatype. But we have to
 * be sure that the pdtBase datatype is correctly initialized with all fields
 * set to ZERO if it's a empty datatype.
 */
int32_t ompi_ddt_add( ompi_datatype_t* pdtBase, const ompi_datatype_t* pdtAdd, 
                      uint32_t count, long disp, long extent )
{
    uint32_t newLength, place_needed = 0, i;
    short localFlags = 0;  /* no specific options yet */
    dt_elem_desc_t *pLast, *pLoop = NULL;
    long lb, ub, old_true_ub;

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
        if( pdtAdd == ompi_ddt_basicDatatypes[DT_LB] ) {
            pdtBase->bdt_used |= (1<< DT_LB);
            if( pdtBase->flags & DT_FLAG_USER_LB ) {
                pdtBase->lb = LMIN( pdtBase->lb, disp );
            } else {
                pdtBase->lb = disp;
                pdtBase->flags |= DT_FLAG_USER_LB;
            }
            return OMPI_SUCCESS;
        } else if( pdtAdd == ompi_ddt_basicDatatypes[DT_UB] ) {
            pdtBase->bdt_used |= (1<< DT_UB);
            if( pdtBase->flags & DT_FLAG_USER_UB ) {
                pdtBase->ub = LMAX( pdtBase->ub, disp );
            } else {
                pdtBase->ub = disp;
                pdtBase->flags |= DT_FLAG_USER_UB;
            }
            return OMPI_SUCCESS;
        }
    } else {
        place_needed = pdtAdd->desc.used;
        if( count != 1 ) place_needed += 2;  /* for the loop markers */
    }

    /* 
     * the count == 0 is LEGAL only for MPI_UB and MPI_LB. I accept it just as a nice way to set
     * the soft UB for a data (without using a real UB marker). This approach can be used to create
     * the subarray and darray datatype. However from the MPI level this function should never be
     * called directly with a count set to 0.
     */
    if( count == 0 ) {
        if( pdtBase->desc.used == 0 ) {  /* empty datatype */
            pdtBase->lb = 0;
            pdtBase->ub = 0;
            pdtBase->true_lb = 0;
            pdtBase->true_ub = 0;
        } else
            pdtBase->ub += disp;
        return OMPI_SUCCESS;
    }
    /* compute the new memory alignement */
    pdtBase->align = IMAX( pdtBase->align, pdtAdd->align );
    
    pdtBase->bdt_used |= pdtAdd->bdt_used;
    newLength = pdtBase->desc.used + place_needed;
    if( newLength > pdtBase->desc.length ) {
        newLength = ((newLength / DT_INCREASE_STACK) + 1 ) * DT_INCREASE_STACK;
        pdtBase->desc.desc   = (dt_elem_desc_t*)realloc( pdtBase->desc.desc,
                                                         sizeof(dt_elem_desc_t) * newLength );
        pdtBase->desc.length = newLength;
    }
    pLast = &(pdtBase->desc.desc[pdtBase->desc.used]);
    if( (pdtAdd->flags & DT_FLAG_BASIC) == DT_FLAG_BASIC ) { /* add a basic datatype */
        pLast->elem.common.type = pdtAdd->id;
        pLast->elem.count       = count;
        pLast->elem.disp        = disp;
        pLast->elem.extent      = extent;
        pdtBase->desc.used++;
        pdtBase->btypes[pdtAdd->id] += count;
        pLast->elem.common.flags  = pdtAdd->flags & ~(DT_FLAG_FOREVER | DT_FLAG_COMMITED | DT_FLAG_CONTIGUOUS);
        if( extent == (int)pdtAdd->size )
            pLast->elem.common.flags |= DT_FLAG_CONTIGUOUS;
    } else {
        /* We handle a user defined datatype. We should make sure that the user will not have the
         * oportunity to destroy it before all datatypes derived are destroyed. As we keep pointers
         * to every datatype (for MPI_Type_get_content and MPI_Type_get_envelope) we have to make
         * sure that those datatype will be available if the user ask for them. However, there
         * is no easy way to free them in this case ...
         */
        OBJ_RETAIN( pdtAdd );
        
        /* keep trace of the total number of basic datatypes in the datatype definition */
        pdtBase->btypes[DT_LOOP]     += pdtAdd->btypes[DT_LOOP];
        pdtBase->btypes[DT_END_LOOP] += pdtAdd->btypes[DT_END_LOOP];
        pdtBase->btypes[DT_LB]       |= pdtAdd->btypes[DT_LB];
        pdtBase->btypes[DT_UB]       |= pdtAdd->btypes[DT_UB];
        for( i = 4; i < DT_MAX_PREDEFINED; i++ )
            if( pdtAdd->btypes[i] != 0 ) pdtBase->btypes[i] += (count * pdtAdd->btypes[i]);
        
        /* if the extent of the datatype if the same as the extent of the loop
         * description of the datatype then we simply have to update the main loop.
         */
        if( count != 1 ) {
            pLoop = pLast;
            pLast->loop.common.type   = DT_LOOP;
            pLast->loop.loops         = count;
            pLast->loop.items         = (long)pdtAdd->desc.used + 1;
            pLast->loop.extent        = extent;
            pLast->loop.common.flags  = (pdtAdd->flags & ~(DT_FLAG_COMMITED | DT_FLAG_FOREVER));
            localFlags = DT_FLAG_IN_LOOP;
            pdtBase->btypes[DT_LOOP] += 2;
            pdtBase->desc.used += 2;
            pLast++;
        }
        
        for( i = 0; i < pdtAdd->desc.used; i++ ) {
            pLast->elem              = pdtAdd->desc.desc[i].elem;
            pLast->elem.common.flags = pdtAdd->desc.desc[i].elem.common.flags | localFlags;
            if( DT_LOOP != pdtAdd->desc.desc[i].elem.common.type )
                pLast->elem.disp += disp /* + pdtAdd->lb */;
            pLast++;
        }
        pdtBase->desc.used += pdtAdd->desc.used;
        if( pLoop != NULL ) {
            pLast->end_loop.common.type = DT_END_LOOP;
            pLast->end_loop.items = pdtAdd->desc.used + 1;   /* where the loop start */
            pLast->end_loop.total_extent = disp + (count - 1) * extent +
                                           (pdtAdd->true_ub - pdtAdd->true_lb) ;  /* the final extent for the loop */
            pLast->end_loop.size = pdtAdd->size;        /* the size of the data inside the loop */
            pLast->end_loop.common.flags = pLoop->loop.common.flags;
        }
        /* should I add some space until the extent of this datatype ? */
    }

    /* Recompute the new true_lb and true_ub */
    if( 0 == pdtBase->nbElems ) old_true_ub = disp;
    else                        old_true_ub = pdtBase->true_ub;
    pdtBase->size += count * pdtAdd->size;
    pdtBase->true_lb = LMIN( pdtBase->true_lb, pdtAdd->true_lb + disp );
    pdtBase->true_ub = LMAX( pdtBase->true_ub,
                             disp + pdtAdd->true_lb + 
                             (count - 1) * extent + pdtAdd->true_ub );

    /* Is the data still contiguous ?
     * The only way for the data to be contiguous is to have the true extent equal to his size.
     * In other words to avoid having internal gaps between elements. And the datatypes
     * should ALWAYS follow each other.
     */
    if( disp != old_true_ub ) { /* is there a gap between the 2 datatypes ? */
        if( disp < old_true_ub ) pdtBase->flags |= DT_FLAG_OVERLAP;
        UNSET_CONTIGUOUS_FLAG(pdtBase->flags);
    } else {
        if( (pdtBase->flags & DT_FLAG_CONTIGUOUS) && (pdtAdd->flags & DT_FLAG_CONTIGUOUS)
            && (pdtBase->size == (uint32_t)(pdtBase->true_ub - pdtBase->true_lb)) ) {
            SET_CONTIGUOUS_FLAG(pdtBase->flags);
        } else {
            UNSET_CONTIGUOUS_FLAG(pdtBase->flags);
        }
    }
    
    /* the lower bound should be inherited from the parents if and only
     * if the USER has explicitly set it. The result lb is the MIN between
     * the all lb + disp if and only if all or nobody flags's contain the LB.
     */
    if( (pdtAdd->flags ^ pdtBase->flags) & DT_FLAG_USER_LB ) {
        if( pdtAdd->flags & DT_FLAG_USER_LB )
            lb = pdtAdd->lb + disp;
        else
            lb = pdtBase->lb;
        pdtBase->flags |= DT_FLAG_USER_LB;
    } else {
        /* both of them have the LB flag or both of them dont have it */
        lb = LMIN( pdtBase->lb, pdtAdd->lb + disp );
    }

    /* the same apply for the upper bound except for the case where
     * either of them has the flag UB, in which case we should
     * compute the UB including the natural alignement of the data.
     */
    if( (pdtBase->flags ^ pdtAdd->flags) & DT_FLAG_USER_UB ) {
        if( pdtAdd->flags & DT_FLAG_USER_UB )
            ub = disp + pdtAdd->lb + count * extent;
        else {
            ub = pdtBase->ub;
        }
        pdtBase->flags |= DT_FLAG_USER_UB;
    } else {
        /* both of them have the UB flag or both of them dont have it */
        /* we should compute the extent depending on the alignement */
        ub = LMAX( pdtBase->ub,
                   (disp + pdtAdd->lb + count * (extent)) );
    }
    /* update the extent and size */
    pdtBase->lb = lb;
    pdtBase->ub = ub;
    pdtBase->nbElems += (count * pdtAdd->nbElems);

    return OMPI_SUCCESS;
}
