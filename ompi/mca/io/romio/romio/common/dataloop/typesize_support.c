/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* Notes:
 * - Not used my MPICH2
 * - "alignsz" value for non-structs is based simply on element
 *   size. This seems nonintuitive, but so far is working for
 *   MPICH2. If we start to get weird alignment problems with
 *   complex structs of non-basics, that would be a place to look.
 * - Depends on configure tests that define (if available):
 *   HAVE_LONG_LONG_INT
 *   HAVE_LONG_DOUBLE
 */

#include "./dataloop.h"
#include "typesize_support.h"

static void DLOOP_Type_calc_footprint_struct(MPI_Datatype type,
					     int combiner,
					     int *ints,
					     MPI_Aint *aints,
					     MPI_Datatype *types,
					     DLOOP_Type_footprint *tfp);
static int DLOOP_Named_type_alignsize(MPI_Datatype type, MPI_Aint disp);
static int DLOOP_Structalign_integer_max(void);
static int DLOOP_Structalign_float_max(void);
static int DLOOP_Structalign_double_max(void);
static int DLOOP_Structalign_long_double_max(void);
static int DLOOP_Structalign_double_position(void);
static int DLOOP_Structalign_llint_position(void);

/* LB/UB calculation helper macros, from MPICH2 */

/* DLOOP_DATATYPE_CONTIG_LB_UB()
 *
 * Determines the new LB and UB for a block of old types given the
 * old type's LB, UB, and extent, and a count of these types in the
 * block.
 *
 * Note: if the displacement is non-zero, the DLOOP_DATATYPE_BLOCK_LB_UB()
 * should be used instead (see below).
 */
#define DLOOP_DATATYPE_CONTIG_LB_UB(cnt_,			\
				    old_lb_,			\
				    old_ub_,			\
				    old_extent_,		\
				    lb_,			\
				    ub_)			\
    {								\
	if (cnt_ == 0) {					\
	    lb_ = old_lb_;					\
	    ub_ = old_ub_;					\
	}							\
	else if (old_ub_ >= old_lb_) {				\
	    lb_ = old_lb_;					\
	    ub_ = old_ub_ + (old_extent_) * (cnt_ - 1);		\
	}							\
	else /* negative extent */ {				\
	    lb_ = old_lb_ + (old_extent_) * (cnt_ - 1);		\
	    ub_ = old_ub_;					\
	}							\
    }

/* DLOOP_DATATYPE_VECTOR_LB_UB()
 *
 * Determines the new LB and UB for a vector of blocks of old types
 * given the old type's LB, UB, and extent, and a count, stride, and
 * blocklen describing the vectorization.
 */
#define DLOOP_DATATYPE_VECTOR_LB_UB(cnt_,			\
				    stride_,			\
				    blklen_,			\
				    old_lb_,			\
				    old_ub_,			\
				    old_extent_,		\
				    lb_,			\
				    ub_)			\
    {								\
	if (cnt_ == 0 || blklen_ == 0) {			\
	    lb_ = old_lb_;					\
	    ub_ = old_ub_;					\
	}							\
	else if (stride_ >= 0 && (old_extent_) >= 0) {		\
	    lb_ = old_lb_;					\
	    ub_ = old_ub_ + (old_extent_) * ((blklen_) - 1) +	\
		(stride_) * ((cnt_) - 1);			\
	}							\
	else if (stride_ < 0 && (old_extent_) >= 0) {		\
	    lb_ = old_lb_ + (stride_) * ((cnt_) - 1);		\
	    ub_ = old_ub_ + (old_extent_) * ((blklen_) - 1);	\
	}							\
	else if (stride_ >= 0 && (old_extent_) < 0) {		\
	    lb_ = old_lb_ + (old_extent_) * ((blklen_) - 1);	\
	    ub_ = old_ub_ + (stride_) * ((cnt_) - 1);		\
	}							\
	else {							\
	    lb_ = old_lb_ + (old_extent_) * ((blklen_) - 1) +	\
		(stride_) * ((cnt_) - 1);			\
	    ub_ = old_ub_;					\
	}							\
    }

/* DLOOP_DATATYPE_BLOCK_LB_UB()
 *
 * Determines the new LB and UB for a block of old types given the LB,
 * UB, and extent of the old type as well as a new displacement and count
 * of types.
 *
 * Note: we need the extent here in addition to the lb and ub because the
 * extent might have some padding in it that we need to take into account.
 */
#define DLOOP_DATATYPE_BLOCK_LB_UB(cnt_,				\
				   disp_,				\
				   old_lb_,				\
				   old_ub_,				\
				   old_extent_,				\
				   lb_,					\
				   ub_)					\
    {									\
	if (cnt_ == 0) {						\
	    lb_ = old_lb_ + (disp_);					\
	    ub_ = old_ub_ + (disp_);					\
	}								\
	else if (old_ub_ >= old_lb_) {					\
	    lb_ = old_lb_ + (disp_);					\
	    ub_ = old_ub_ + (disp_) + (old_extent_) * ((cnt_) - 1);	\
	}								\
	else /* negative extent */ {					\
	    lb_ = old_lb_ + (disp_) + (old_extent_) * ((cnt_) - 1);	\
	    ub_ = old_ub_ + (disp_);					\
	}								\
    }

void PREPEND_PREFIX(Type_calc_footprint)(MPI_Datatype type,
					 DLOOP_Type_footprint *tfp)
{
    int mpi_errno;
    int nr_ints, nr_aints, nr_types, combiner;
    int *ints;
    MPI_Aint *aints;
    MPI_Datatype *types;

    /* used to store parameters for constituent types */
    DLOOP_Offset size = 0, lb = 0, ub = 0, true_lb = 0, true_ub = 0;
    DLOOP_Offset extent = 0, alignsz;
    int has_sticky_lb, has_sticky_ub;

    /* used for vector/hvector/hvector_integer calculations */
    DLOOP_Offset stride;

    /* used for indexed/hindexed calculations */
    DLOOP_Offset disp;

    /* used for calculations on types with more than one block of data */
    DLOOP_Offset i, min_lb, max_ub, ntypes, tmp_lb, tmp_ub;

    /* used for processing subarray and darray types */
    int ndims;
    MPI_Datatype tmptype;

    mpi_errno = MPI_Type_get_envelope(type, &nr_ints, &nr_aints,
				       &nr_types, &combiner);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    if (combiner == MPI_COMBINER_NAMED) {
	int mpisize;
	MPI_Aint mpiextent;

	MPI_Type_size(type, &mpisize);
	MPI_Type_extent(type, &mpiextent);
	tfp->size    = (DLOOP_Offset) mpisize;
	tfp->lb      = 0;
	tfp->ub      = (DLOOP_Offset) mpiextent;
	tfp->true_lb = 0;
	tfp->true_ub = (DLOOP_Offset) mpiextent;
	tfp->extent  = (DLOOP_Offset) mpiextent;
	tfp->alignsz = DLOOP_Named_type_alignsize(type, (MPI_Aint) 0);
	tfp->has_sticky_lb = (type == MPI_LB) ? 1 : 0;
	tfp->has_sticky_ub = (type == MPI_UB) ? 1 : 0;

	goto clean_exit;
    }

    /* get access to contents; need it immediately to check for zero count */
    PREPEND_PREFIX(Type_access_contents)(type, &ints, &aints, &types);

    /* knock out all the zero count cases */
    if ((combiner == MPI_COMBINER_CONTIGUOUS ||
	 combiner == MPI_COMBINER_VECTOR ||
	 combiner == MPI_COMBINER_HVECTOR_INTEGER ||
	 combiner == MPI_COMBINER_HVECTOR ||
	 combiner == MPI_COMBINER_INDEXED_BLOCK ||
	 combiner == MPI_COMBINER_INDEXED ||
	 combiner == MPI_COMBINER_HINDEXED_INTEGER ||
	 combiner == MPI_COMBINER_STRUCT_INTEGER ||
	 combiner == MPI_COMBINER_STRUCT) && ints[0] == 0)
    {
	tfp->size = tfp->lb = tfp->ub = tfp->extent = tfp->alignsz = 0;
	tfp->true_lb = tfp->true_ub = 0;
	tfp->has_sticky_lb = tfp->has_sticky_ub = 0;
	goto clean_exit;
    }

    if (combiner != MPI_COMBINER_STRUCT &&
	combiner != MPI_COMBINER_STRUCT_INTEGER)
    {
	DLOOP_Type_footprint cfp;

	PREPEND_PREFIX(Type_calc_footprint)(types[0], &cfp);
	size    = cfp.size;
	lb      = cfp.lb;
	ub      = cfp.ub;
	true_lb = cfp.true_lb;
	true_ub = cfp.true_ub;
	extent  = cfp.extent;
	alignsz = cfp.alignsz;
	has_sticky_lb = cfp.has_sticky_lb;
	has_sticky_ub = cfp.has_sticky_ub;

	/* initialize some common values so we don't have to assign
	 * them in every case below.
	 */
	tfp->alignsz = alignsz;
	tfp->has_sticky_lb = has_sticky_lb;
	tfp->has_sticky_ub = has_sticky_ub;

    }

    switch(combiner)
    {
	case MPI_COMBINER_DUP:
	    tfp->size    = size;
	    tfp->lb      = lb;
	    tfp->ub      = ub;
	    tfp->true_lb = true_lb;
	    tfp->true_ub = true_ub;
	    tfp->extent  = extent;
	    break;
	case MPI_COMBINER_RESIZED:
	    tfp->size    = size;
	    tfp->lb      = aints[0]; /* lb */
	    tfp->ub      = aints[0] + aints[1];
	    tfp->true_lb = true_lb;
	    tfp->true_ub = true_ub;
	    tfp->extent  = aints[1]; /* extent */
	    tfp->has_sticky_lb = 1;
	    tfp->has_sticky_ub = 1;
	    break;
	case MPI_COMBINER_CONTIGUOUS:
	    DLOOP_DATATYPE_CONTIG_LB_UB(ints[0] /* count */,
					lb, ub, extent,
					tfp->lb, tfp->ub);
	    tfp->true_lb = tfp->lb + (true_lb - lb);
	    tfp->true_ub = tfp->ub + (true_ub - ub);
	    tfp->size    = ints[0] * size;
	    tfp->extent  = tfp->ub - tfp->lb;
	    break;
	case MPI_COMBINER_VECTOR:
	case MPI_COMBINER_HVECTOR:
	case MPI_COMBINER_HVECTOR_INTEGER:
	    if (combiner == MPI_COMBINER_VECTOR) stride = ints[2] * extent;
	    else if (combiner == MPI_COMBINER_HVECTOR) stride = aints[0];
	    else /* HVECTOR_INTEGER */ stride = ints[2];

	    DLOOP_DATATYPE_VECTOR_LB_UB(ints[0] /* count */,
					stride /* stride in bytes */,
					ints[1] /* blklen */,
					lb, ub, extent,
					tfp->lb, tfp->ub);
	    tfp->true_lb = tfp->lb + (true_lb - lb);
	    tfp->true_ub = tfp->ub + (true_ub - ub);
	    tfp->size    = ints[0] * ints[1] * size;
	    tfp->extent  = tfp->ub - tfp->lb;
	    break;
	case MPI_COMBINER_INDEXED_BLOCK:
	    /* prime min_lb and max_ub */
	    DLOOP_DATATYPE_BLOCK_LB_UB(ints[1] /* blklen */,
				       ints[2] * extent /* disp */,
				       lb, ub, extent,
				       min_lb, max_ub);

	    for (i=1; i < ints[0]; i++) {
		DLOOP_DATATYPE_BLOCK_LB_UB(ints[1] /* blklen */,
					   ints[i+2] * extent /* disp */,
					   lb, ub, extent,
					   tmp_lb, tmp_ub);
		if (tmp_lb < min_lb) min_lb = tmp_lb;
		if (tmp_ub > max_ub) max_ub = tmp_ub;
	    }
	    tfp->size    = ints[0] * ints[1] * size;
	    tfp->lb      = min_lb;
	    tfp->ub      = max_ub;
	    tfp->true_lb = min_lb + (true_lb - lb);
	    tfp->true_ub = max_ub + (true_ub - ub);
	    tfp->extent  = tfp->ub - tfp->lb;
	    break;
	case MPI_COMBINER_INDEXED:
	case MPI_COMBINER_HINDEXED_INTEGER:
	case MPI_COMBINER_HINDEXED:
	    /* find first non-zero blocklength element */
	    for (i=0; i < ints[0] && ints[i+1] == 0; i++);
	    if (i == ints[0]) {
		/* all zero blocklengths */
		tfp->size = tfp->lb = tfp->ub = tfp->extent = tfp->alignsz = 0;
		tfp->has_sticky_lb = tfp->has_sticky_ub = 0;
	    }
	    else {
		/* prime min_lb, max_ub, count */
		ntypes = ints[i+1];
		if (combiner == MPI_COMBINER_INDEXED)
		    disp = ints[ints[0]+i+1] * extent;
		else if (combiner == MPI_COMBINER_HINDEXED_INTEGER)
		    disp = ints[ints[0]+i+1];
		else /* MPI_COMBINER_HINDEXED */
		    disp = aints[i];

		DLOOP_DATATYPE_BLOCK_LB_UB(ints[i+1] /* blklen */,
					   disp,
					   lb, ub, extent,
					   min_lb, max_ub);

		for (i++; i < ints[0]; i++) {
		    /* skip zero blocklength elements */
		    if (ints[i+1] == 0) continue;

		    ntypes += ints[i+1];
		    if (combiner == MPI_COMBINER_INDEXED)
			disp = ints[ints[0]+i+1] * extent;
		    else if (combiner == MPI_COMBINER_HINDEXED_INTEGER)
			disp = ints[ints[0]+i+1];
		    else /* MPI_COMBINER_HINDEXED */
			disp = aints[i];

		    DLOOP_DATATYPE_BLOCK_LB_UB(ints[i+1],
					       disp,
					       lb, ub, extent,
					       tmp_lb, tmp_ub);
		    if (tmp_lb < min_lb) min_lb = tmp_lb;
		    if (tmp_ub > max_ub) max_ub = tmp_ub;
		}
		tfp->size    = ntypes * size;
		tfp->lb      = min_lb;
		tfp->ub      = max_ub;
		tfp->true_lb = min_lb + (true_lb - lb);
		tfp->true_ub = max_ub + (true_ub - ub);
		tfp->extent  = tfp->ub - tfp->lb;
	    }
	    break;
	case MPI_COMBINER_STRUCT_INTEGER:
	    DLOOP_Assert(combiner != MPI_COMBINER_STRUCT_INTEGER);
	    break;
	case MPI_COMBINER_STRUCT:
	    /* sufficiently complicated to pull out into separate fn */
	    DLOOP_Type_calc_footprint_struct(type,
					     combiner, ints, aints, types,
					     tfp);
	    break;
	case MPI_COMBINER_SUBARRAY:
	    ndims = ints[0];
	    PREPEND_PREFIX(Type_convert_subarray)(ndims,
						  &ints[1] /* sizes */,
						  &ints[1+ndims] /* subsz */,
						  &ints[1+2*ndims] /* strts */,
						  ints[1+3*ndims] /* order */,
						  types[0],
						  &tmptype);
	    PREPEND_PREFIX(Type_calc_footprint)(tmptype, tfp);
	    MPI_Type_free(&tmptype);
	    break;
	case MPI_COMBINER_DARRAY:
	    ndims = ints[2];

	    PREPEND_PREFIX(Type_convert_darray)(ints[0] /* size */,
						ints[1] /* rank */,
						ndims,
						&ints[3] /* gsizes */,
						&ints[3+ndims] /*distribs */,
						&ints[3+2*ndims] /* dargs */,
						&ints[3+3*ndims] /* psizes */,
						ints[3+4*ndims] /* order */,
						types[0],
						&tmptype);

	    PREPEND_PREFIX(Type_calc_footprint)(tmptype, tfp);
	    MPI_Type_free(&tmptype);
	    break;
	case MPI_COMBINER_F90_REAL:
	case MPI_COMBINER_F90_COMPLEX:
	case MPI_COMBINER_F90_INTEGER:
	default:
	    DLOOP_Assert(0);
	    break;
    }

 clean_exit:
    PREPEND_PREFIX(Type_release_contents)(type, &ints, &aints, &types);
    return;
}

/*
  DLOOP_Type_calc_footprint_struct - calculate size, lb, ub, extent,
                                     and alignsize for a struct type
*/
static void DLOOP_Type_calc_footprint_struct(MPI_Datatype type,
					     int struct_combiner,
					     int *ints,
					     MPI_Aint *aints,
					     MPI_Datatype *types,
					     DLOOP_Type_footprint *tfp)
{
    int i, found_sticky_lb = 0, found_sticky_ub = 0, first_iter = 1;
    DLOOP_Offset tmp_lb, tmp_ub, tmp_extent, tmp_true_lb, tmp_true_ub;
    DLOOP_Offset max_alignsz = 0, tmp_size = 0, min_lb = 0, max_ub = 0;
    DLOOP_Offset min_true_lb = 0, max_true_ub = 0;

    int nr_ints, nr_aints, nr_types, combiner;

    /* used to store parameters for constituent types */
    DLOOP_Type_footprint cfp;
    DLOOP_Offset size, lb, ub, true_lb, true_ub, extent, alignsz;
    int sticky_lb, sticky_ub;

    /* find first non-zero blocklength element */
    for (i=0; i < ints[0] && ints[i+1] == 0; i++);

    if (i == ints[0]) /* all zero-length blocks */ {
	tfp->size = tfp->lb = tfp->ub = tfp->extent = tfp->alignsz = 0;
	tfp->has_sticky_lb = tfp->has_sticky_ub = 0;
	return;
    }
    
    for (; i < ints[0]; i++) {
	/* skip zero blocklength elements */
	if (ints[i+1] == 0) continue;

	MPI_Type_get_envelope(types[i], &nr_ints, &nr_aints, &nr_types,
			       &combiner);

	/* opt: could just inline assignments for combiner == NAMED case */

	PREPEND_PREFIX(Type_calc_footprint)(types[i], &cfp);
	size      = cfp.size;
	lb        = cfp.lb;
	ub        = cfp.ub;
	true_lb   = cfp.true_lb;
	true_ub   = cfp.true_ub;
	extent    = cfp.extent;
	alignsz   = cfp.alignsz;
	sticky_lb = cfp.has_sticky_lb;
	sticky_ub = cfp.has_sticky_ub;

	DLOOP_DATATYPE_BLOCK_LB_UB(ints[i+1] /* blklen */,
				   aints[i] /* disp */,
				   lb, ub, extent,
				   tmp_lb, tmp_ub);

	tmp_true_lb = tmp_lb + (true_lb - lb);
	tmp_true_ub = tmp_ub + (true_ub - ub);
	tmp_size += size * ints[i+1];

	if (combiner == MPI_COMBINER_NAMED) {
	    /* NOTE: This is a special case. If a user creates a struct
	     *       with a named type at a non-zero displacement, the
	     *       alignment may be different than expected due to 
	     *       special compiler rules for this case. Thus we must
	     *       over-ride the value that we obtained from
	     *       Type_calc_footprint() above.
	     */
	    alignsz = DLOOP_Named_type_alignsize(types[i], aints[i]);
	}

	if (max_alignsz < alignsz) max_alignsz = alignsz;
  
	/* We save this LB if:
	 * (1) this is our first iteration where we saw a nonzero blklen,
	 * (2) we haven't found a sticky LB and this LB is lower than
	 *     any we have previously seen,
	 * (3) we haven't found a sticky LB and this one is sticky, or
	 * (4) this sticky LB is lower than any we have previously seen.
	 */
	if ((first_iter) ||
	    (!found_sticky_lb && min_lb > tmp_lb) ||
	    (!found_sticky_lb && sticky_lb) ||
	    (sticky_lb && min_lb > tmp_lb))
	{
	    min_lb = tmp_lb;
	    if (sticky_lb) found_sticky_lb = 1;
	}

	if ((first_iter) ||
	    (!found_sticky_ub && max_ub < tmp_ub) ||
	    (!found_sticky_ub && sticky_ub) ||
	    (sticky_ub && max_ub < tmp_ub))
	{
	    max_ub = tmp_ub;
	    if (sticky_ub) found_sticky_ub = 1;
	}

	if ((first_iter) ||
	    (tmp_true_lb > min_true_lb))
	{
	    min_true_lb = tmp_true_lb;
	}

	if ((first_iter) ||
	    (tmp_true_ub < max_true_ub))
	{
	    max_true_ub = tmp_true_ub;
	}

	first_iter = 0;
    }

    /* calculate extent, not including potential padding */
    tmp_extent = max_ub - min_lb;

    /* account for padding if no sticky LB/UB is found */
    if ((!found_sticky_lb) && (!found_sticky_ub)) {
	DLOOP_Offset epsilon;

	epsilon = (max_alignsz > 0) ? tmp_extent % max_alignsz : 0;

	if (epsilon) {
	    max_ub += (max_alignsz - epsilon);
	    tmp_extent = max_ub - min_lb;
	}
    }

    tfp->size    = tmp_size;
    tfp->lb      = min_lb;
    tfp->ub      = max_ub;
    tfp->true_lb = min_true_lb;
    tfp->true_ub = max_true_ub;
    tfp->extent  = tmp_extent;
    tfp->alignsz = max_alignsz;
    tfp->has_sticky_lb = found_sticky_lb;
    tfp->has_sticky_ub = found_sticky_ub;
    return;
}

/*
 DLOOP_Named_type_alignsize - calculate alignment in bytes for a struct
                              based on constituent elements.

 Returns alignment in bytes.
*/
static int DLOOP_Named_type_alignsize(MPI_Datatype type, MPI_Aint disp)
{
    int alignsize = 0;

    static int havent_tested_align_rules = 1;
    static int max_intalign = 0, max_fpalign = 0;
    static int have_double_pos_align = 0, have_llint_pos_align = 0;
    static int max_doublealign = 0, max_longdoublealign = 0;

    if (havent_tested_align_rules) {
	max_intalign          = DLOOP_Structalign_integer_max();
	max_fpalign           = DLOOP_Structalign_float_max();
	max_doublealign       = DLOOP_Structalign_double_max();
	max_longdoublealign   = DLOOP_Structalign_long_double_max();
	have_double_pos_align = DLOOP_Structalign_double_position();
	have_llint_pos_align  = DLOOP_Structalign_llint_position();

	havent_tested_align_rules = 0;
    }

    /* skip LBs, UBs, and elements with zero block length */
    if (type == MPI_LB || type == MPI_UB)
	return 0;

    MPI_Type_size(type, &alignsize);

    switch(type)
    {
	case MPI_FLOAT:
	    if (alignsize > max_fpalign)
		alignsize = max_fpalign;
	    break;
	case MPI_DOUBLE:
	    if (alignsize > max_doublealign)
		alignsize = max_doublealign;
	    
	    if (have_double_pos_align && disp != (MPI_Aint) 0)
		alignsize = 4; /* would be better to test */
	    break;
	case MPI_LONG_DOUBLE:
	    if (alignsize > max_longdoublealign)
		alignsize = max_longdoublealign;
	    break;
	default:
	    if (alignsize > max_intalign) 
		alignsize = max_intalign;
	    
	    if (have_llint_pos_align &&
		type == MPI_LONG_LONG_INT &&
		disp != (MPI_Aint) 0)
	    {
		alignsize = 4; /* would be better to test */
	    }
	    break;
    }

    return alignsize;
}


/* INTERNAL STRUCT ALIGNMENT TESTS BELOW */

/* from MPICH2 PAC_C_MAX_INTEGER_ALIGN test:
 *
 * Tests for max C struct integer alignment. Note that this is for *all*
 * integer types.
 *
 * Return value is 1, 2, 4, or 8.
 */
static int DLOOP_Structalign_integer_max()
{
    int is_packed  = 1;
    int is_two     = 1;
    int is_four    = 1;
    int is_eight   = 1;

    int size, extent;

    struct { char a; int b; } char_int;
    struct { char a; short b; } char_short;
    struct { char a; long b; } char_long;
    struct { char a; int b; char c; } char_int_char;
    struct { char a; short b; char c; } char_short_char;
#ifdef HAVE_LONG_LONG_INT
    struct { long long int a; char b; } lli_c;
    struct { char a; long long int b; } c_lli;
    int extent2;
#endif

    /* assume max integer alignment isn't 8 if we don't have
     * an eight-byte value.
     */
#ifdef HAVE_LONG_LONG_INT
    if (sizeof(int) < 8 && sizeof(long) < 8 && sizeof(long long int) < 8)
	is_eight = 0;
#else
    if (sizeof(int) < 8 && sizeof(long) < 8) is_eight = 0;
#endif

    size = sizeof(char) + sizeof(int);
    extent = sizeof(char_int);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if ( (extent % 4) != 0) is_four = 0;
    if (sizeof(int) == 8 && (extent % 8) != 0) is_eight = 0;

    size = sizeof(char) + sizeof(short);
    extent = sizeof(char_short);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if (sizeof(short) == 4 && (extent % 4) != 0) is_four = 0;
    if (sizeof(short) == 8 && (extent % 8) != 0) is_eight = 0;

    size = sizeof(char) + sizeof(long);
    extent = sizeof(char_long);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if ( (extent % 4) != 0) is_four = 0;
    if (sizeof(long) == 8 && (extent % 8) != 0) is_eight = 0;

#ifdef HAVE_LONG_LONG_INT
    size = sizeof(char) + sizeof(long long int);
    extent = sizeof(lli_c);
    extent2 = sizeof(c_lli);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(long long int) >= 8 && (extent % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
#endif

    size = sizeof(char) + sizeof(int) + sizeof(char);
    extent = sizeof(char_int_char);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if ( (extent % 4) != 0) is_four = 0;
    if (sizeof(int) == 8 && (extent % 8) != 0) is_eight = 0;

    size = sizeof(char) + sizeof(short) + sizeof(char);
    extent = sizeof(char_short_char);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if (sizeof(short) == 4 && (extent % 4) != 0) is_four = 0;
    if (sizeof(short) == 8 && (extent % 8) != 0) is_eight = 0;

    if (is_eight) { is_four = 0; is_two = 0; }
    if (is_four) is_two = 0;

    DLOOP_Assert(is_packed + is_two + is_four + is_eight == 1);

    if (is_packed) return 1;
    if (is_two)    return 2;
    if (is_four)   return 4;
    return 8;
}

/* from MPICH2 PAC_C_MAX_FP_ALIGN test:
 *
 * Checks for max C struct floating point alignment. Note that
 * in this test we are *only* testing float types, whereas in
 * the original test we were testing double and long double also.
 *
 * Return value is 1, 2, 4, 8, or 16.
 */
static int DLOOP_Structalign_float_max()
{
    int is_packed  = 1;
    int is_two     = 1;
    int is_four    = 1;
    int is_eight   = 1;
    int is_sixteen = 1;
    struct { char a; float b; } char_float;
    struct { float b; char a; } float_char;
    int size, extent1, extent2;

    size = sizeof(char) + sizeof(float);
    extent1 = sizeof(char_float);
    extent2 = sizeof(float_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(float) == 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;

    if (is_sixteen) { is_eight = 0; is_four = 0; is_two = 0; }
    if (is_eight) { is_four = 0; is_two = 0; }
    if (is_four) is_two = 0;

    DLOOP_Assert(is_packed + is_two + is_four + is_eight + is_sixteen == 1);

    if (is_packed)  return 1;
    if (is_two)     return 2;
    if (is_four)    return 4;
    if (is_eight)   return 8;
    return 16;
}

/* from MPICH2 PAC_C_MAX_DOUBLE_FP_ALIGN test:
 *
 * Determines maximum struct alignment with floats and doubles.
 *
 * Return value is 1, 2, 4, or 8.
 */
static int DLOOP_Structalign_double_max()
{
    int is_packed  = 1;
    int is_two     = 1;
    int is_four    = 1;
    int is_eight   = 1;
    struct { char a; double b; } char_double;
    struct { double b; char a; } double_char;
    int size, extent1, extent2;

    size = sizeof(char) + sizeof(double);
    extent1 = sizeof(char_double);
    extent2 = sizeof(double_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(double) == 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;

    if (is_eight) { is_four = 0; is_two = 0; }
    if (is_four) is_two = 0;

    DLOOP_Assert(is_packed + is_two + is_four + is_eight == 1);

    if (is_packed) return 1;
    if (is_two)    return 2;
    if (is_four)   return 4;
    return 8;
}

/* from MPICH2 PAC_C_MAX_LONGDOUBLE_FP_ALIGN test:
 *
 * Determines maximum alignment of structs with long doubles.
 *
 * Return value is 1, 2, 4, 8, or 16.
 */
static int DLOOP_Structalign_long_double_max()
{
    int is_packed  = 1;
    int is_two     = 1;
    int is_four    = 1;
    int is_eight   = 1;
    int is_sixteen = 1;
    struct { char a; long double b; } char_long_double;
    struct { long double b; char a; } long_double_char;
    struct { long double a; int b; char c; } long_double_int_char;
    int size, extent1, extent2;

    size = sizeof(char) + sizeof(long double);
    extent1 = sizeof(char_long_double);
    extent2 = sizeof(long_double_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(long double) >= 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
    if (sizeof(long double) > 8 && (extent1 % 16) != 0
	&& (extent2 % 16) != 0) is_sixteen = 0;

    extent1 = sizeof(long_double_int_char);
    if ( (extent1 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0) is_four = 0;
    if (sizeof(long double) >= 8 && (extent1 % 8) != 0)	is_eight = 0;
    if (sizeof(long double) > 8 && (extent1 % 16) != 0) is_sixteen = 0;

    if (is_sixteen) { is_eight = 0; is_four = 0; is_two = 0; }
    if (is_eight) { is_four = 0; is_two = 0; }
    if (is_four) is_two = 0;

    DLOOP_Assert(is_packed + is_two + is_four + is_eight + is_sixteen == 1);

    if (is_packed)  return 1;
    if (is_two)     return 2;
    if (is_four)    return 4;
    if (is_eight)   return 8;
    return 16;
}


/* from MPICH2 PAC_C_DOUBLE_POS_ALIGN test:
 *
 * Test for odd struct alignment rule that only applies max. padding when
 * double value is at front of type.
 *
 * Search for "Power alignment mode" for more details.
 * 
 * Return value is 1 or 0.
 */
static int DLOOP_Structalign_double_position()
{
    int padding_varies_by_pos = 0;
    struct { char a; double b; } char_double;
    struct { double b; char a; } double_char;
    int extent1, extent2;

    extent1 = sizeof(char_double);
    extent2 = sizeof(double_char);
    if (extent1 != extent2) padding_varies_by_pos = 1;

    if (padding_varies_by_pos) return 1;
    else                       return 0;
}

/* from MPICH2 PAC_C_LLINT_POS_ALIGN test:
 * Test for odd struct alignment rule that only applies max.
 * padding when long long int value is at front of type.
 *
 * Search for "Power alignment mode" for more details.
 * 
 * Return value is 1 or 0.
 */
static int DLOOP_Structalign_llint_position()
{
    int padding_varies_by_pos = 0;
#ifdef HAVE_LONG_LONG_INT
    struct { char a; long long int b; } char_llint;
    struct { long long int b; char a; } llint_char;
    int extent1, extent2;

    extent1 = sizeof(char_llint);
    extent2 = sizeof(llint_char);
    if (extent1 != extent2) padding_varies_by_pos = 1;
#endif

    if (padding_varies_by_pos) return 1;
    else                       return 0;
}
