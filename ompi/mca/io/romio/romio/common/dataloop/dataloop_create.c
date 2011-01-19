/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <stdlib.h>
#include <limits.h>

#include "./dataloop.h"

static void DLOOP_Dataloop_create_named(MPI_Datatype type,
					DLOOP_Dataloop **dlp_p,
					int *dlsz_p,
					int *dldepth_p,
					int flag);

void PREPEND_PREFIX(Dataloop_create)(MPI_Datatype type,
				     DLOOP_Dataloop **dlp_p,
				     int *dlsz_p,
				     int *dldepth_p,
				     int flag)
{
    int i;

    int nr_ints, nr_aints, nr_types, combiner;
    MPI_Datatype *types;
    int *ints;
    MPI_Aint *aints;

    DLOOP_Dataloop *old_dlp;
    int old_dlsz, old_dldepth;

    int dummy1, dummy2, dummy3, type0_combiner, ndims;
    MPI_Datatype tmptype;

    MPI_Aint stride;
    MPI_Aint *disps;

    MPI_Type_get_envelope(type, &nr_ints, &nr_aints, &nr_types, &combiner);

    /* some named types do need dataloops; handle separately. */
    if (combiner == MPI_COMBINER_NAMED) {
	DLOOP_Dataloop_create_named(type, dlp_p, dlsz_p, dldepth_p, flag);
	return;
    }

    /* Q: should we also check for "hasloop", or is the COMBINER
     *    check above enough to weed out everything that wouldn't
     *    have a loop?
     */
    DLOOP_Handle_get_loopptr_macro(type, old_dlp, flag);
    if (old_dlp != NULL) {
	/* dataloop already created; just return it. */
	*dlp_p = old_dlp;
	DLOOP_Handle_get_loopsize_macro(type, *dlsz_p, flag);
	DLOOP_Handle_get_loopdepth_macro(type, *dldepth_p, flag);
	return;
    }

    PREPEND_PREFIX(Type_access_contents)(type, &ints, &aints, &types);

    /* first check for zero count on types where that makes sense */
    switch(combiner) {
	case MPI_COMBINER_CONTIGUOUS:
	case MPI_COMBINER_VECTOR:
	case MPI_COMBINER_HVECTOR_INTEGER:
	case MPI_COMBINER_HVECTOR:
	case MPI_COMBINER_INDEXED_BLOCK:
	case MPI_COMBINER_INDEXED:
	case MPI_COMBINER_HINDEXED_INTEGER:
	case MPI_COMBINER_HINDEXED:
	case MPI_COMBINER_STRUCT_INTEGER:
	case MPI_COMBINER_STRUCT:
	    if (ints[0] == 0) {
		PREPEND_PREFIX(Dataloop_create_contiguous)(0,
							   MPI_INT,
							   dlp_p,
							   dlsz_p,
							   dldepth_p,
							   flag);
		goto clean_exit;
	    }
	    break;
	default:
	    break;
    }

    /* recurse, processing types "below" this one before processing
     * this one, if those type don't already have dataloops.
     *
     * note: in the struct case below we'll handle any additional
     *       types "below" the current one.
     */
    MPI_Type_get_envelope(types[0], &dummy1, &dummy2, &dummy3,
			   &type0_combiner);
    if (type0_combiner != MPI_COMBINER_NAMED)
    {
	DLOOP_Handle_get_loopptr_macro(types[0], old_dlp, flag);
	if (old_dlp == NULL)
	{
	    /* no dataloop already present; create and store one */
	    PREPEND_PREFIX(Dataloop_create)(types[0],
					    &old_dlp,
					    &old_dlsz,
					    &old_dldepth,
					    flag);

	    DLOOP_Handle_set_loopptr_macro(types[0], old_dlp, flag);
	    DLOOP_Handle_set_loopsize_macro(types[0], old_dlsz, flag);
	    DLOOP_Handle_set_loopdepth_macro(types[0], old_dldepth, flag);
	}
    }
       
    switch(combiner)
    {
	case MPI_COMBINER_DUP:
	    if (type0_combiner != MPI_COMBINER_NAMED) {
		PREPEND_PREFIX(Dataloop_dup)(old_dlp, old_dlsz, dlp_p);
		*dlsz_p    = old_dlsz;
		*dldepth_p = old_dldepth;
	    }
	    else {
		PREPEND_PREFIX(Dataloop_create_contiguous)(1,
							   types[0], 
							   dlp_p, dlsz_p,
							   dldepth_p,
							   flag);
	    }
	    break;
	case MPI_COMBINER_RESIZED:
	    if (type0_combiner != MPI_COMBINER_NAMED) {
		PREPEND_PREFIX(Dataloop_dup)(old_dlp, old_dlsz, dlp_p);
		*dlsz_p    = old_dlsz;
		*dldepth_p = old_dldepth;
	    }
	    else {
		PREPEND_PREFIX(Dataloop_create_contiguous)(1,
							   types[0], 
							   dlp_p, dlsz_p,
							   dldepth_p,
							   flag);

		(*dlp_p)->el_extent = aints[1]; /* extent */
	    }
	    break;
	case MPI_COMBINER_CONTIGUOUS:
	    PREPEND_PREFIX(Dataloop_create_contiguous)(ints[0] /* count */,
						       types[0] /* oldtype */,
						       dlp_p, dlsz_p,
						       dldepth_p,
						       flag);
	    break;
	case MPI_COMBINER_VECTOR:
	    PREPEND_PREFIX(Dataloop_create_vector)(ints[0] /* count */,
						   ints[1] /* blklen */,
						   ints[2] /* stride */,
						   0 /* stride not bytes */,
						   types[0] /* oldtype */,
						   dlp_p, dlsz_p, dldepth_p,
						   flag);
	    break;
	case MPI_COMBINER_HVECTOR_INTEGER:
	case MPI_COMBINER_HVECTOR:
	    /* fortran hvector has integer stride in bytes */
	    if (combiner == MPI_COMBINER_HVECTOR_INTEGER) {
		stride = (MPI_Aint) ints[2];
	    }
	    else {
		stride = aints[0];
	    }

	    PREPEND_PREFIX(Dataloop_create_vector)(ints[0] /* count */,
						   ints[1] /* blklen */,
						   stride,
						   1 /* stride in bytes */,
						   types[0] /* oldtype */,
						   dlp_p, dlsz_p, dldepth_p,
						   flag);
	    break;
	case MPI_COMBINER_INDEXED_BLOCK:
	    PREPEND_PREFIX(Dataloop_create_blockindexed)(ints[0] /* count */,
							 ints[1] /* blklen */,
							 &ints[2] /* disps */,
							 0 /* disp not bytes */,
							 types[0] /* oldtype */,
							 dlp_p, dlsz_p,
							 dldepth_p,
							 flag);
	    break;
	case MPI_COMBINER_INDEXED:
	    PREPEND_PREFIX(Dataloop_create_indexed)(ints[0] /* count */,
						    &ints[1] /* blklens */,
						    &ints[ints[0]+1] /* disp */,
						    0 /* disp not in bytes */,
						    types[0] /* oldtype */,
						    dlp_p, dlsz_p, dldepth_p,
						    flag);
	    break;
	case MPI_COMBINER_HINDEXED_INTEGER:
	case MPI_COMBINER_HINDEXED:
	    if (combiner == MPI_COMBINER_HINDEXED_INTEGER) {
		disps = (MPI_Aint *) DLOOP_Malloc(ints[0] * sizeof(MPI_Aint));

		for (i=0; i < ints[0]; i++) {
		    disps[i] = (MPI_Aint) ints[ints[0] + 1 + i];
		}
	    }
	    else {
		disps = aints;
	    }

	    PREPEND_PREFIX(Dataloop_create_indexed)(ints[0] /* count */,
						    &ints[1] /* blklens */,
						    disps,
						    1 /* disp in bytes */,
						    types[0] /* oldtype */,
						    dlp_p, dlsz_p, dldepth_p,
						    flag);

	    if (combiner == MPI_COMBINER_HINDEXED_INTEGER) {
		DLOOP_Free(disps);
	    }

	    break;
	case MPI_COMBINER_STRUCT_INTEGER:
	case MPI_COMBINER_STRUCT:
	    for (i = 1; i < ints[0]; i++) {
		int type_combiner;
		MPI_Type_get_envelope(types[i], &dummy1, &dummy2, &dummy3,
				       &type_combiner);

		if (type_combiner != MPI_COMBINER_NAMED) {
		    DLOOP_Handle_get_loopptr_macro(types[i], old_dlp, flag);
		    if (old_dlp == NULL)
		    {
			PREPEND_PREFIX(Dataloop_create)(types[i],
							&old_dlp,
							&old_dlsz,
							&old_dldepth,
							flag);
			
			DLOOP_Handle_set_loopptr_macro(types[i], old_dlp,
						       flag);
			DLOOP_Handle_set_loopsize_macro(types[i], old_dlsz,
							flag);
			DLOOP_Handle_set_loopdepth_macro(types[i], old_dldepth,
							 flag);
		    }
		}
	    }
	    if (combiner == MPI_COMBINER_STRUCT_INTEGER) {
		disps = (MPI_Aint *) DLOOP_Malloc(ints[0] * sizeof(MPI_Aint));

		for (i=0; i < ints[0]; i++) {
		    disps[i] = (MPI_Aint) ints[ints[0] + 1 + i];
		}
	    }
	    else {
		disps = aints;
	    }

	    PREPEND_PREFIX(Dataloop_create_struct)(ints[0] /* count */,
						   &ints[1] /* blklens */,
						   disps,
						   types /* oldtype array */,
						   dlp_p, dlsz_p, dldepth_p,
						   flag);

	    if (combiner == MPI_COMBINER_STRUCT_INTEGER) {
		DLOOP_Free(disps);
	    }
	    break;
	case MPI_COMBINER_SUBARRAY:
	    ndims = ints[0];
	    PREPEND_PREFIX(Type_convert_subarray)(ndims,
						  &ints[1] /* sizes */,
						  &ints[1+ndims] /* subsizes */,
						  &ints[1+2*ndims] /* starts */,
						  ints[1+3*ndims] /* order */,
						  types[0],
						  &tmptype);

	    PREPEND_PREFIX(Dataloop_create)(tmptype,
					    dlp_p,
					    dlsz_p,
					    dldepth_p,
					    flag);
	    
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

	    PREPEND_PREFIX(Dataloop_create)(tmptype,
					    dlp_p,
					    dlsz_p,
					    dldepth_p,
					    flag);

	    MPI_Type_free(&tmptype);
	    break;
	case MPI_COMBINER_F90_REAL:
	case MPI_COMBINER_F90_COMPLEX:
	case MPI_COMBINER_F90_INTEGER:
	    /* TODO: WHAT DO I DO HERE? */
	default:
	    DLOOP_Assert(0);
	    break;
    }

 clean_exit:

    PREPEND_PREFIX(Type_release_contents)(type, &ints, &aints, &types);

    /* for now we just leave the intermediate dataloops in place.
     * could remove them to save space if we wanted.
     */

    return;
}

/*@
  DLOOP_Dataloop_create_named - create a dataloop for a "named" type
  if necessary.

  "named" types are ones for which MPI_Type_get_envelope() returns a
  combiner of MPI_COMBINER_NAMED. some types that fit this category,
  such as MPI_SHORT_INT, have multiple elements with potential gaps
  and padding. these types need dataloops for correct processing.
@*/
static void DLOOP_Dataloop_create_named(MPI_Datatype type,
					DLOOP_Dataloop **dlp_p,
					int *dlsz_p,
					int *dldepth_p,
					int flag)
{
    DLOOP_Dataloop *dlp;

    /* special case: pairtypes need dataloops too.
     *
     * note: not dealing with MPI_2INT because size == extent
     *       in all cases for that type.
     *
     * note: MPICH2 always precreates these, so we will never call
     *       Dataloop_create_pairtype() from here in the MPICH2
     *       case.
     */
    if (type == MPI_FLOAT_INT || type == MPI_DOUBLE_INT ||
	type == MPI_LONG_INT || type == MPI_SHORT_INT ||
	type == MPI_LONG_DOUBLE_INT)
    {
	DLOOP_Handle_get_loopptr_macro(type, dlp, flag);
	if (dlp != NULL) {
	    /* dataloop already created; just return it. */
	    *dlp_p = dlp;
	    DLOOP_Handle_get_loopsize_macro(type, *dlsz_p, flag);
	    DLOOP_Handle_get_loopdepth_macro(type, *dldepth_p, flag);
	}
	else {
	    PREPEND_PREFIX(Dataloop_create_pairtype)(type,
						     dlp_p,
						     dlsz_p,
						     dldepth_p,
						     flag);
	}
	return;
    }
    /* no other combiners need dataloops; exit. */
    else {
	*dlp_p = NULL;
	*dlsz_p = 0;
	*dldepth_p = 0;
	return;
    }
}
