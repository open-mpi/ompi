/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/datatype.h"
#include "ompi/datatype/datatype_internal.h"

/* macros to play with the flags */
#define SET_CONTIGUOUS_FLAG( INT_VALUE )     (INT_VALUE) = (INT_VALUE) | (DT_FLAG_CONTIGUOUS)
#define SET_NO_GAP_FLAG( INT_VALUE )         (INT_VALUE) = (INT_VALUE) | (DT_FLAG_NO_GAPS)
#define UNSET_CONTIGUOUS_FLAG( INT_VALUE )   (INT_VALUE) = (INT_VALUE) & (~(DT_FLAG_CONTIGUOUS | DT_FLAG_NO_GAPS))

#if defined(__GNUC__) && !defined(__STDC__)
#define LMAX(A,B)  ({ ptrdiff_t _a = (A), _b = (B); (_a < _b ? _b : _a) })
#define LMIN(A,B)  ({ ptrdiff_t _a = (A), _b = (B); (_a < _b ? _a : _b); })
#define IMAX(A,B)  ({ int _a = (A), _b = (B); (_a < _b ? _b : _a); })
#define IMIN(A,B)  ({ int _a = (A), _b = (B); (_a < _b ? _a : _b); })
#else
static inline ptrdiff_t LMAX( ptrdiff_t a, ptrdiff_t b ) { return ( a < b ? b : a ); }
static inline ptrdiff_t LMIN( ptrdiff_t a, ptrdiff_t b ) { return ( a < b ? a : b ); }
static inline int  IMAX( int a, int b ) { return ( a < b ? b : a ); }
static inline int  IMIN( int a, int b ) { return ( a < b ? a : b ); }
#endif  /* __GNU__ */

#define OMPI_DDT_LB_UB_CONT( _count, _disp, _old_lb, _old_ub, _old_extent, _new_lb, _new_ub ) \
{ \
    if( 0 == _count ) { \
        _new_lb = (_old_lb) + (_disp); \
        _new_ub = (_old_ub) + (_disp); \
    } else { \
        ptrdiff_t lower, upper; \
        upper = (_disp) + (_old_extent) * ((_count) - 1); \
        lower = (_disp); \
        if( lower < upper ) { \
            _new_lb = lower; \
            _new_ub = upper; \
         } else { \
            _new_lb = upper; \
            _new_ub = lower; \
         } \
         _new_lb += (_old_lb); \
         _new_ub += (_old_ub); \
    }\
}

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
                      uint32_t count, ptrdiff_t disp, ptrdiff_t extent )
{
    uint32_t newLength, place_needed = 0, i;
    short localFlags = 0;  /* no specific options yet */
    dt_elem_desc_t *pLast, *pLoop = NULL;
    ptrdiff_t lb, ub, true_lb, true_ub, epsilon, old_true_ub;

    /* the extent should always be positive. So a negative
     * value here have a special meaning ie. default extent as
     * computed by ub - lb
     */
    if( extent == -1 ) extent = (pdtAdd->ub - pdtAdd->lb);

    /* handle special cases for DT_LB and DT_UB and their duplicate */
    if( DT_LB == pdtAdd->id ) {
        pdtBase->bdt_used |= (((uint64_t)1) << DT_LB);
        if( pdtBase->flags & DT_FLAG_USER_LB ) {
            pdtBase->lb = LMIN( pdtBase->lb, disp );
        } else {
            pdtBase->lb = disp;
            pdtBase->flags |= DT_FLAG_USER_LB;
        }
        if( (pdtBase->ub - pdtBase->lb) != (ptrdiff_t)pdtBase->size ) {
            pdtBase->flags &= ~DT_FLAG_NO_GAPS;
        }
        return OMPI_SUCCESS;
    } else if( DT_UB == pdtAdd->id ) {
        pdtBase->bdt_used |= (((uint64_t)1) << DT_UB);
        if( pdtBase->flags & DT_FLAG_USER_UB ) {
            pdtBase->ub = LMAX( pdtBase->ub, disp );
        } else {
            pdtBase->ub = disp;
            pdtBase->flags |= DT_FLAG_USER_UB;
        }
        if( (pdtBase->ub - pdtBase->lb) != (ptrdiff_t)pdtBase->size ) {
            pdtBase->flags &= ~DT_FLAG_NO_GAPS;
        }
        return OMPI_SUCCESS;
    }
    if( pdtAdd->flags & DT_FLAG_PREDEFINED ) { /* add a basic datatype */
        place_needed = (extent == (ptrdiff_t)pdtAdd->size ? 1 : 3);
    } else {
        place_needed = pdtAdd->desc.used;
        if( count != 1 ) {
            if( place_needed < (MAX_DT_COMPONENT_COUNT - 2) ) {
                place_needed += 2;  /* for the loop markers */
            } else {
                /* The data-type contain too many elements. We will be unable
                 * to handle it, so let's just complain by now.
                 */
                opal_output( 0, "Too many elements in the datatype. The limit is %ud\n",
                             MAX_DT_COMPONENT_COUNT );
                return OMPI_ERROR;
            }
        }
    }

    /*
     * Compute the lower and upper bound of the datatype. We do it in 2 steps.
     * First compute the lb and ub of the new datatype taking in account the
     * count. Then update the lb value depending on the user markers and
     * update the global lb and ub.
     */
    OMPI_DDT_LB_UB_CONT( count, disp, pdtAdd->lb, pdtAdd->ub, extent, lb, ub );
    /* The true_lb and true_ub take in account the gaps at the begining and the
     * end of the datatype independing on the number of repetitions of the datatype.
     */
    true_lb = lb - (pdtAdd->lb - pdtAdd->true_lb);
    true_ub = ub - (pdtAdd->ub - pdtAdd->true_ub);
    if( true_lb > true_ub ) {
        old_true_ub = true_lb;
        true_lb = true_ub;
        true_ub = old_true_ub;
    }

    /* the lower bound should be inherited from the parent if and only
     * if the USER has explicitly set it. The result lb is the MIN between
     * the all lb + disp if and only if all or nobody flags's contain the LB.
     */
    if( (pdtAdd->flags ^ pdtBase->flags) & DT_FLAG_USER_LB ) {
        if( pdtBase->flags & DT_FLAG_USER_LB ) {
            lb = pdtBase->lb;  /* base type has a user provided lb */
        }
        pdtBase->flags |= DT_FLAG_USER_LB;
    } else {
        /* both of them have the LB flag or both of them dont have it */
        lb = LMIN( pdtBase->lb, lb );
    }

    /* the same apply for the upper bound except for the case where
     * either of them has the flag UB, in which case we should
     * compute the UB including the natural alignement of the data.
     */
    if( (pdtBase->flags ^ pdtAdd->flags) & DT_FLAG_USER_UB ) {
        if( pdtBase->flags & DT_FLAG_USER_UB ) {
            ub = pdtBase->ub;
        }
        pdtBase->flags |= DT_FLAG_USER_UB;
    } else {
        /* both of them have the UB flag or both of them dont have it */
        /* we should compute the extent depending on the alignement */
        ub = LMAX( pdtBase->ub, ub );
    }
    /* While the true_lb and true_ub have to be ordered to have the true_lb lower
     * than the true_ub, the ub and lb do not have to be ordered. They should be
     * as the user define them.
     */
    pdtBase->lb = lb;
    pdtBase->ub = ub;

    /* compute the new memory alignement */
    pdtBase->align = IMAX( pdtBase->align, pdtAdd->align );

    /* Now that we have the new ub and the alignment we should update the ub to match
     * the new alignement. We have to add an epsilon that is the least nonnegative
     * increment needed to roung the extent to the next multiple of the alignment.
     * This rule apply only if there is user specified upper bound as stated in the
     * MPI standard MPI 1.2 page 71.
     */
    if( !(pdtBase->flags & DT_FLAG_USER_UB) ) {
        epsilon = (pdtBase->ub - pdtBase->lb) % pdtBase->align;
        if( 0 != epsilon ) {
            pdtBase->ub += (pdtBase->align - epsilon);
        }
    }
    /* now we know it contain some data */
    pdtBase->flags |= DT_FLAG_DATA;

    /*
     * the count == 0 is LEGAL only for MPI_UB and MPI_LB. Therefore we support it
     * here in the upper part of this function. As an extension, the count set to
     * zero can be used to reset the alignment of the data, but not for changing
     * the true_lb and true_ub.
     */
    if( (0 == count) || (0 == pdtAdd->size) ) {
        return OMPI_SUCCESS;
    }

    /* Now, once we know everything is fine and there are some bytes in
     * the data-type we can update the size, true_lb and true_ub.
     */
    pdtBase->size += count * pdtAdd->size;
    if( 0 == pdtBase->nbElems ) old_true_ub = disp;
    else                        old_true_ub = pdtBase->true_ub;
    if( 0 != pdtBase->size ) {
        pdtBase->true_lb = LMIN( true_lb, pdtBase->true_lb );
        pdtBase->true_ub = LMAX( true_ub, pdtBase->true_ub );
    } else {
        pdtBase->true_lb = true_lb;
        pdtBase->true_ub = true_ub;
    }

    pdtBase->bdt_used |= pdtAdd->bdt_used;
    newLength = pdtBase->desc.used + place_needed;
    if( newLength > pdtBase->desc.length ) {
        newLength = ((newLength / DT_INCREASE_STACK) + 1 ) * DT_INCREASE_STACK;
        pdtBase->desc.desc   = (dt_elem_desc_t*)realloc( pdtBase->desc.desc,
                                                         sizeof(dt_elem_desc_t) * newLength );
        pdtBase->desc.length = newLength;
    }
    pLast = &(pdtBase->desc.desc[pdtBase->desc.used]);
    /* The condition to be able to use the optimized path here is to be in presence
     * of an predefined contiguous datatype. This part is unable to handle any
     * predefined non contiguous datatypes (like MPI_SHORT_INT).
     */
    if( (pdtAdd->flags & (DT_FLAG_PREDEFINED | DT_FLAG_DATA)) == (DT_FLAG_PREDEFINED | DT_FLAG_DATA) ) {
        pdtBase->btypes[pdtAdd->id] += count;
        if( (extent != (ptrdiff_t)pdtAdd->size) && (count > 1) ) {  /* gaps around the datatype */
            localFlags = pdtAdd->flags & ~(DT_FLAG_COMMITED | DT_FLAG_CONTIGUOUS | DT_FLAG_NO_GAPS);
            CREATE_LOOP_START( pLast, count, 2, extent, localFlags );
            pLast++;
            pLast->elem.common.type  = pdtAdd->id;
            pLast->elem.count        = 1;
            pLast->elem.disp         = disp;
            pLast->elem.extent       = pdtAdd->size;
            pLast->elem.common.flags = localFlags | DT_FLAG_CONTIGUOUS;
            pLast++;
            CREATE_LOOP_END( pLast, 2, disp, pdtAdd->size, localFlags );
            pdtBase->desc.used += 3;
            pdtBase->btypes[DT_LOOP]     = 1;
            pdtBase->btypes[DT_END_LOOP] = 1;
        } else {
            pLast->elem.common.type = pdtAdd->id;
            pLast->elem.count       = count;
            pLast->elem.disp        = disp;
            pLast->elem.extent      = extent;
            pdtBase->desc.used++;
            pLast->elem.common.flags  = pdtAdd->flags & ~(DT_FLAG_COMMITED);
        }
    } else {
        /* keep trace of the total number of basic datatypes in the datatype definition */
        pdtBase->btypes[DT_LOOP]     += pdtAdd->btypes[DT_LOOP];
        pdtBase->btypes[DT_END_LOOP] += pdtAdd->btypes[DT_END_LOOP];
        pdtBase->btypes[DT_LB]       |= pdtAdd->btypes[DT_LB];
        pdtBase->btypes[DT_UB]       |= pdtAdd->btypes[DT_UB];
        for( i = 4; i < DT_MAX_PREDEFINED; i++ )
            if( pdtAdd->btypes[i] != 0 ) pdtBase->btypes[i] += (count * pdtAdd->btypes[i]);

        if( (1 == pdtAdd->desc.used) && (extent == (pdtAdd->ub - pdtAdd->lb)) &&
            (extent == pdtAdd->desc.desc[0].elem.extent) ){
            pLast->elem        = pdtAdd->desc.desc[0].elem;
            pLast->elem.count *= count;
            pLast->elem.disp  += disp;
            pdtBase->desc.used++;
        } else {
            /* if the extent of the datatype is the same as the extent of the loop
             * description of the datatype then we simply have to update the main loop.
             */
            if( count != 1 ) {
                pLoop = pLast;
                CREATE_LOOP_START( pLast, count, pdtAdd->desc.used + 1, extent,
                                   (pdtAdd->flags & ~(DT_FLAG_COMMITED)) );
                pdtBase->btypes[DT_LOOP] += 2;
                pdtBase->desc.used += 2;
                pLast++;
            }

            for( i = 0; i < pdtAdd->desc.used; i++ ) {
                pLast->elem               = pdtAdd->desc.desc[i].elem;
                if( DT_FLAG_DATA & pLast->elem.common.flags )
                    pLast->elem.disp += disp;
                else if( DT_END_LOOP == pLast->elem.common.type ) {
                    pLast->end_loop.first_elem_disp += disp;
                }
                pLast++;
            }
            pdtBase->desc.used += pdtAdd->desc.used;
            if( pLoop != NULL ) {
                int index = GET_FIRST_NON_LOOP( pLoop );
                assert( pLoop[index].elem.common.flags & DT_FLAG_DATA );
                CREATE_LOOP_END( pLast, pdtAdd->desc.used + 1, pLoop[index].elem.disp,
                                 pdtAdd->size, pLoop->loop.common.flags );
            }
        }
        /* should I add some space until the extent of this datatype ? */
    }

    /* Is the data still contiguous ?
     * The only way for the data to be contiguous is to have the true extent
     * equal to his size. In other words to avoid having internal gaps between
     * elements. If any of the data are overlapping then this method will not work.
     */
    localFlags = pdtBase->flags & pdtAdd->flags;
    UNSET_CONTIGUOUS_FLAG(pdtBase->flags);
    if( disp != old_true_ub ) { /* is there a gap between the 2 datatypes ? */
        if( disp < old_true_ub ) pdtBase->flags |= DT_FLAG_OVERLAP;
    } else {
        if( (localFlags & DT_FLAG_CONTIGUOUS)        /* both have to be contiguous */
            && ( ((ptrdiff_t)pdtAdd->size == extent) /* the size and the extent of the
                                                      * added type have to match */
                 || (count < 2)) ) {                 /* if the count is bigger than 2 */
            SET_CONTIGUOUS_FLAG(pdtBase->flags);
            if( (ptrdiff_t)pdtBase->size == (pdtBase->ub - pdtBase->lb) )
                SET_NO_GAP_FLAG(pdtBase->flags);
        }
    }
    /* If the NO_GAP flag is set the contiguous have to be set too */
    if( pdtBase->flags & DT_FLAG_NO_GAPS ) {
        assert( pdtBase->flags & DT_FLAG_CONTIGUOUS );
    }
    pdtBase->nbElems += (count * pdtAdd->nbElems);

    return OMPI_SUCCESS;
}
