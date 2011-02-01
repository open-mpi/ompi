/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2007 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* This file implements functions necessary to store dataloop representations
 * as an attribute on the MPI datatype. It also implements functionality to
 * store a size, extent, and count of contig blocks in the same way. The
 * size and extent are stored as MPI_Offset-sized values, so that overflow
 * isn't a factor (as it would be on BG* for example if we used ints or
 * MPI_Aints).
 *
 * Additionally this code puts an attribute on MPI_COMM_WORLD with a delete
 * callback to clean up keyvals and any allocated memory (although currently
 * memory cleanup relies on the delete functions on the types being called
 * as well).
 */

#include <stdio.h>
#include <stdlib.h>

#include "romio_dataloop.h"
#include "typesize_support.h"

typedef struct MPIO_Datatype_s {
    int             valid, refct;
    int             dloop_size, dloop_depth; /* size, depth of dloop struct */
    DLOOP_Offset    size, extent; /* size and extent of type */
    DLOOP_Offset    true_lb, true_extent;
    DLOOP_Dataloop *dloop;
    DLOOP_Count     contig_blks;
} MPIO_Datatype;

/* valid flags */
#define MPIO_DATATYPE_VALID_DLOOP_PTR    1
#define MPIO_DATATYPE_VALID_DLOOP_SIZE   2
#define MPIO_DATATYPE_VALID_DLOOP_DEPTH  4
#define MPIO_DATATYPE_VALID_TYPESZEXT    8
#define MPIO_DATATYPE_VALID_CONTIG_BLKS 16

#define MPIO_DATATYPE_VALID_DATALOOP (MPIO_DATATYPE_VALID_DLOOP_PTR | \
                                      MPIO_DATATYPE_VALID_DLOOP_SIZE | \
                                      MPIO_DATATYPE_VALID_DLOOP_DEPTH)

/* MPI attr keyvals used internally */
static int MPIO_Datatype_keyval = MPI_KEYVAL_INVALID;
static int MPIO_Datatype_finalize_keyval = MPI_KEYVAL_INVALID;

static MPIO_Datatype *MPIO_Datatype_allocate(MPI_Datatype type);
static void MPIO_Datatype_set_szext(MPI_Datatype type, MPIO_Datatype *dtp);
static int MPIO_Datatype_initialize(void);
static int MPIO_Datatype_finalize(MPI_Comm comm, int comm_keyval,
				  void *attrval, void *extrastate);
static int MPIO_Datatype_copy_attr_function(MPI_Datatype type, int type_keyval,
					    void *extra_state,
					    void *attribute_val_in,
					    void *attribute_val_out, int *flag);
static int MPIO_Datatype_delete_attr_function(MPI_Datatype type,
					      int type_keyval,
					      void *attribute_val,
					      void *extra_state);

/* MPIO_Datatype_init_dataloop
 *
 * Must be called before dataloop is accessed.
 *
 * Valid to call more than once on the same type.
 */
void MPIO_Datatype_init_dataloop(MPI_Datatype type)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    /* trivial types don't get dataloops */
    if (!(MPIO_Datatype_is_nontrivial(type))) return;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    if (!attrflag) {
	/* need to allocate structure and create dataloop representation */
	dtp = MPIO_Datatype_allocate(type);
    }
    if (!(dtp->valid | MPIO_DATATYPE_VALID_DLOOP_PTR)) {
	MPIO_Dataloop_create(type,
			     &dtp->dloop,
			     &dtp->dloop_size,
			     &dtp->dloop_depth,
			     0);
    }

    return;
}

void MPIO_Datatype_get_size(MPI_Datatype type, MPI_Offset *size_p)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);
    
    if (!attrflag) {
	dtp = MPIO_Datatype_allocate(type);
    }

    if (!(dtp->valid & MPIO_DATATYPE_VALID_TYPESZEXT)) {
	MPIO_Datatype_set_szext(type, dtp);
    }

    *size_p = dtp->size;
    return;
}

void MPIO_Datatype_get_extent(MPI_Datatype type, MPI_Offset *extent_p)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);
    
    if (!attrflag) {
	dtp = MPIO_Datatype_allocate(type);
    }

    if (!(dtp->valid & MPIO_DATATYPE_VALID_TYPESZEXT)) {
	MPIO_Datatype_set_szext(type, dtp);
    }

    *extent_p = dtp->extent;
    return;
}

/* MPIO_Datatype_get_block_info
 *
 * Parameters:
 * type     - MPI datatype
 * true_lb  - true_lb for type (offset to start of data)
 * count    - count of # of contiguous regions in the type
 * n_contig - flag, indicating if N of these types would also form a 
 *            single contiguous block
 */
void MPIO_Datatype_get_block_info(MPI_Datatype type,
				  MPI_Offset *true_lb_p,
				  MPI_Offset *count_p,
				  int *n_contig_p)
{
    int mpi_errno, attrflag;
    int nr_ints, nr_aints, nr_types, combiner;

    mpi_errno = MPI_Type_get_envelope(type, &nr_ints, &nr_aints,
				       &nr_types, &combiner);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    if (combiner == MPI_COMBINER_NAMED &&
	(type != MPI_FLOAT_INT &&
	 type != MPI_DOUBLE_INT &&
	 type != MPI_LONG_INT &&
	 type != MPI_SHORT_INT &&
	 type != MPI_LONG_DOUBLE_INT))
    {
	*true_lb_p  = 0;
	*count_p    = 1;
	*n_contig_p = 1;
    }
    else {
	MPIO_Datatype *dtp;
	MPIO_Segment  *segp;
	MPI_Offset     bytes;

	mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp,
				      &attrflag);
	DLOOP_Assert(mpi_errno == MPI_SUCCESS);
	if (!attrflag) {
	    /* need to allocate structure and create dataloop representation */
	    dtp = MPIO_Datatype_allocate(type);
	}
	if (!(dtp->valid | MPIO_DATATYPE_VALID_DLOOP_PTR)) {
	    MPIO_Dataloop_create(type,
				 &dtp->dloop,
				 &dtp->dloop_size,
				 &dtp->dloop_depth, 0);
	    DLOOP_Assert(dtp->dloop != NULL);
	}
	    
	DLOOP_Assert((dtp->valid | MPIO_DATATYPE_VALID_DLOOP_PTR) &&
		     (dtp->valid | MPIO_DATATYPE_VALID_DLOOP_SIZE) &&
		     (dtp->valid | MPIO_DATATYPE_VALID_DLOOP_DEPTH));

	segp = MPIO_Segment_alloc();
	DLOOP_Assert(segp != NULL);

	MPIO_Segment_init(NULL, 1, type, segp, 0);
	bytes = SEGMENT_IGNORE_LAST;

	MPIO_Segment_count_contig_blocks(segp, 0, &bytes, &dtp->contig_blks);
	MPIO_Segment_free(segp);
	dtp->valid |= MPIO_DATATYPE_VALID_CONTIG_BLKS;

	if (!(dtp->valid | MPIO_DATATYPE_VALID_TYPESZEXT)) {
	    MPIO_Datatype_set_szext(type, dtp);
	}
	*true_lb_p  = dtp->true_lb;
	*count_p    = dtp->contig_blks;
	*n_contig_p = (dtp->contig_blks == 1 &&
		       dtp->true_extent == dtp->extent) ? 1 : 0;
    }

    return;
}

void MPIO_Datatype_get_el_type(MPI_Datatype type,
			       MPI_Datatype *eltype_p,
			       int flag)
{
    int mpi_errno;
    int nr_ints, nr_aints, nr_types, combiner;

    mpi_errno = MPI_Type_get_envelope(type, &nr_ints, &nr_aints,
				       &nr_types, &combiner);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    if (combiner == MPI_COMBINER_NAMED) {
	if (type == MPI_FLOAT_INT ||
	    type == MPI_DOUBLE_INT ||
	    type == MPI_LONG_INT ||
	    type == MPI_SHORT_INT ||
	    type == MPI_LONG_DOUBLE_INT)
	{
	    *eltype_p = MPI_DATATYPE_NULL;
	}
	else if (type == MPI_2INT) {
	    *eltype_p = MPI_INT;
	}
	else {
	    /* all the other named types are their own element type */
	    *eltype_p = type;
	}
    }
    else {
	MPIO_Dataloop *dlp;
	MPIO_Datatype_get_loopptr(type, &dlp, flag);

	*eltype_p = dlp->el_type;
    }
    return;
}

/* dataloop-related functions used by dataloop code */
void MPIO_Datatype_get_loopptr(MPI_Datatype type,
			       MPIO_Dataloop **ptr_p,
			       int flag)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    if (!(dtp->valid & MPIO_DATATYPE_VALID_DLOOP_PTR))
	*ptr_p = NULL;
    else 
	*ptr_p = dtp->dloop;

    return;
}

void MPIO_Datatype_get_loopsize(MPI_Datatype type, int *size_p, int flag)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    if (!(dtp->valid & MPIO_DATATYPE_VALID_DLOOP_SIZE))
	*size_p = -1;
    else
	*size_p = dtp->dloop_size;

    return;
}

void MPIO_Datatype_get_loopdepth(MPI_Datatype type, int *depth_p, int flag)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    if (!(dtp->valid & MPIO_DATATYPE_VALID_DLOOP_DEPTH))
	*depth_p = -1;
    else
	*depth_p = dtp->dloop_depth;

    return;
}

void MPIO_Datatype_set_loopptr(MPI_Datatype type, MPIO_Dataloop *ptr, int flag)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);
    if (!attrflag) {
	dtp = MPIO_Datatype_allocate(type);
    }

    printf("set loopptr = %x\n", (int) ptr);

    dtp->dloop  = ptr;
    dtp->valid |= MPIO_DATATYPE_VALID_DLOOP_PTR;
    return;
}

void MPIO_Datatype_set_loopsize(MPI_Datatype type, int size, int flag)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);
    if (!attrflag) {
	dtp = MPIO_Datatype_allocate(type);
    }

    dtp->dloop_size  = size;
    dtp->valid      |= MPIO_DATATYPE_VALID_DLOOP_SIZE;
    return;
}

void MPIO_Datatype_set_loopdepth(MPI_Datatype type, int depth, int flag)
{
    int mpi_errno, attrflag;
    MPIO_Datatype *dtp;

    if (MPIO_Datatype_keyval != MPI_KEYVAL_INVALID) {
	MPIO_Datatype_initialize();
    }

    mpi_errno = MPI_Type_get_attr(type, MPIO_Datatype_keyval, &dtp, &attrflag);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);
    if (!attrflag) {
	dtp = MPIO_Datatype_allocate(type);
    }

    dtp->dloop_depth  = depth;
    dtp->valid       |= MPIO_DATATYPE_VALID_DLOOP_DEPTH;
    return;
}

int MPIO_Datatype_is_nontrivial(MPI_Datatype type)
{
    int nr_ints, nr_aints, nr_types, combiner;

    MPI_Type_get_envelope(type, &nr_ints, &nr_aints, &nr_types, &combiner);
    if (combiner != MPI_COMBINER_NAMED ||
	type == MPI_FLOAT_INT ||
	type == MPI_DOUBLE_INT ||
	type == MPI_LONG_INT ||
	type == MPI_SHORT_INT ||
	type == MPI_LONG_DOUBLE_INT) return 1;
    else return 0;
}

/* internal functions */

static int MPIO_Datatype_initialize(void)
{
    int mpi_errno;

    DLOOP_Assert(MPIO_Datatype_keyval == MPI_KEYVAL_INVALID);

    /* create keyval for dataloop storage */
    mpi_errno = MPI_Type_create_keyval(MPIO_Datatype_copy_attr_function,
				       MPIO_Datatype_delete_attr_function,
				       &MPIO_Datatype_keyval,
				       NULL);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    /* create keyval to hook to COMM_WORLD for finalize */
    mpi_errno = MPI_Comm_create_keyval(MPI_COMM_NULL_COPY_FN,
					MPIO_Datatype_finalize,
					&MPIO_Datatype_finalize_keyval,
					NULL);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    mpi_errno = MPI_Comm_set_attr(MPI_COMM_WORLD,
				   MPIO_Datatype_finalize_keyval,
				   NULL);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    printf("created keyval\n");

    return 0;
}

/* MPIO_Datatype_finalize()
 */
static int MPIO_Datatype_finalize(MPI_Comm comm,
				  int comm_keyval,
				  void *attrval,
				  void *extrastate)
{
    int mpi_errno;

    DLOOP_Assert(MPIO_Datatype_keyval != MPI_KEYVAL_INVALID);

    /* remove keyvals */
    mpi_errno = MPI_Type_free_keyval(&MPIO_Datatype_keyval);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    mpi_errno = MPI_Type_free_keyval(&MPIO_Datatype_finalize_keyval);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    printf("freed keyvals\n");
    
    return MPI_SUCCESS;
}

static MPIO_Datatype *MPIO_Datatype_allocate(MPI_Datatype type)
{
    int mpi_errno;
    MPIO_Datatype *dtp;

    dtp = (MPIO_Datatype *) malloc(sizeof(MPIO_Datatype));
    DLOOP_Assert(dtp != NULL);
    dtp->valid       = 0;
    dtp->refct       = 1;
    dtp->dloop       = NULL;
    dtp->dloop_size  = -1;
    dtp->dloop_depth = -1;
    
    mpi_errno = MPI_Type_set_attr(type, MPIO_Datatype_keyval, dtp);
    DLOOP_Assert(mpi_errno == MPI_SUCCESS);

    printf("allocated attr struct\n");

    return dtp;
}

/* MPIO_Datatype_set_szext()
 *
 * Calculates size, extent, true_lb, and true_extent of type, fills in values
 * in MPIO_Datatype structure, and sets valid flag.
 *
 * Note: This code currently checks for compatible variable sizes at
 *       runtime, while this check could instead be performed at configure
 *       time to save a few instructions. This seems like micro-optimization,
 *       so I skipped it for now. -- RobR, 03/22/2007
 */
static void MPIO_Datatype_set_szext(MPI_Datatype type, MPIO_Datatype *dtp)
{
    int mpi_errno;

    if (sizeof(int) >= sizeof(MPI_Offset) &&
	sizeof(MPI_Aint) >= sizeof(MPI_Offset))
    {
	int size;
	MPI_Aint lb, extent, true_lb, true_extent;
	
	mpi_errno = MPI_Type_size(type, &size);
	DLOOP_Assert(mpi_errno == MPI_SUCCESS);
	
	mpi_errno = MPI_Type_get_extent(type, &lb, &extent);
	DLOOP_Assert(mpi_errno == MPI_SUCCESS);
	
	mpi_errno = MPI_Type_get_true_extent(type, &true_lb, &true_extent); 

	dtp->size        = (MPI_Offset) size;
	dtp->extent      = (MPI_Offset) extent;
	dtp->true_lb     = (MPI_Offset) true_lb;
	dtp->true_extent = (MPI_Offset) true_extent;
    }
    else {
	MPIO_Type_footprint tfp;
	
	MPIO_Type_calc_footprint(type, &tfp);
	dtp->size        = tfp.size;
	dtp->extent      = tfp.extent;
	dtp->true_lb     = tfp.true_lb;
	dtp->true_extent = tfp.true_ub - tfp.true_lb;
    }

    dtp->valid |= MPIO_DATATYPE_VALID_TYPESZEXT;
    return;
}

static int MPIO_Datatype_copy_attr_function(MPI_Datatype type,
					    int type_keyval,
					    void *extra_state,
					    void *attribute_val_in,
					    void *attribute_val_out,
					    int *flag)
{
    MPIO_Datatype *dtp = (MPIO_Datatype *) attribute_val_in;

    printf("copy fn. called\n");

    DLOOP_Assert(dtp->refct);

    dtp->refct++;

    * (MPIO_Datatype **) attribute_val_out = dtp;
    *flag = 1;

    printf("inc'd refct.\n");

    return MPI_SUCCESS;
}

static int MPIO_Datatype_delete_attr_function(MPI_Datatype type,
					      int type_keyval,
					      void *attribute_val,
					      void *extra_state)
{
    MPIO_Datatype *dtp = (MPIO_Datatype *) attribute_val;

    printf("delete fn. called\n");

    DLOOP_Assert(dtp->refct);

    printf("dec'd refct\n");
    
    dtp->refct--;
    if (dtp->refct == 0) {
	free(dtp);
	printf("freed attr structure\n");
    }

    return MPI_SUCCESS;
}

/* 
 * Local variables:
 * c-indent-tabs-mode: nil
 * End:
 */
