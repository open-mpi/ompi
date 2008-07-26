/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "./dataloop.h"

#ifndef PREPEND_PREFIX
#error "You must explicitly include a header that sets the PREPEND_PREFIX and includes dataloop_parts.h"
#endif

static int DLOOP_Dataloop_create_struct_memory_error(void);
static int DLOOP_Dataloop_create_unique_type_struct(int count,
						    int *blklens,
						    MPI_Aint *disps,
						    DLOOP_Type *oldtypes,
						    int type_pos,
						    DLOOP_Dataloop **dlp_p,
						    int *dlsz_p,
						    int *dldepth_p,
						    int flag);
static int DLOOP_Dataloop_create_basic_all_bytes_struct(
	       int count,
	       int *blklens,
	       MPI_Aint *disps,
	       DLOOP_Type *oldtypes,
	       DLOOP_Dataloop **dlp_p,
	       int *dlsz_p,
	       int *dldepth_p,
	       int flag);
static int DLOOP_Dataloop_create_flattened_struct(int count,
						  int *blklens,
						  MPI_Aint *disps,
						  DLOOP_Type *oldtypes,
						  DLOOP_Dataloop **dlp_p,
						  int *dlsz_p,
						  int *dldepth_p,
						  int flag);

/*@
  Dataloop_create_struct - create the dataloop representation for a
  struct datatype

  Input Parameters:
+ count - number of blocks in vector
. blklens - number of elements in each block
. disps - offsets of blocks from start of type in bytes
- oldtypes - types (using handle) of datatypes on which vector is based

  Output Parameters:
+ dlp_p - pointer to address in which to place pointer to new dataloop
- dlsz_p - pointer to address in which to place size of new dataloop

  Return Value:
  0 on success, -1 on failure.

  Notes:
  This function relies on others, like Dataloop_create_indexed, to create
  types in some cases. This call (like all the rest) takes int blklens
  and MPI_Aint displacements, so it's possible to overflow when working
  with a particularly large struct type in some cases. This isn't detected
  or corrected in this code at this time.

@*/
int PREPEND_PREFIX(Dataloop_create_struct)(int count,
					   int *blklens,
					   MPI_Aint *disps,
					   DLOOP_Type *oldtypes,
					   DLOOP_Dataloop **dlp_p,
					   int *dlsz_p,
					   int *dldepth_p,
					   int flag)
{
    int err, i, nr_basics = 0, nr_derived = 0, type_pos = 0;

    DLOOP_Type first_basic = MPI_DATATYPE_NULL,
	first_derived = MPI_DATATYPE_NULL;

    /* variables used in general case only */
    int loop_idx, new_loop_sz, new_loop_depth;
    int old_loop_sz = 0, old_loop_depth = 0;

    DLOOP_Dataloop *new_dlp, *curpos;

    /* if count is zero, handle with contig code, call it a int */
    if (count == 0)
    {
	err = PREPEND_PREFIX(Dataloop_create_contiguous)(0,
							 MPI_INT,
							 dlp_p,
							 dlsz_p,
							 dldepth_p,
							 flag);
	return err;
    }

    /* browse the old types and characterize */
    for (i=0; i < count; i++)
    {
	/* ignore type elements with a zero blklen */
	if (blklens[i] == 0) continue;

	if (oldtypes[i] != MPI_LB && oldtypes[i] != MPI_UB)
	{
	    int is_builtin;

	    is_builtin =
		(DLOOP_Handle_hasloop_macro(oldtypes[i])) ? 0 : 1;

	    if (is_builtin)
	    {
		if (nr_basics == 0)
		{
		    first_basic = oldtypes[i];
		    type_pos = i;
		}
		else if (oldtypes[i] != first_basic)
		{
		    first_basic = MPI_DATATYPE_NULL;
		}
		nr_basics++;
	    }
	    else /* derived type */
	    {
		if (nr_derived == 0)
		{
		    first_derived = oldtypes[i];
		    type_pos = i;
		}
		else if (oldtypes[i] != first_derived)
		{
		    first_derived = MPI_DATATYPE_NULL;
		}
		nr_derived++;
	    }
	}
    }

    /* note on optimizations:
     *
     * because LB, UB, and extent calculations are handled as part of
     * the Datatype, we can safely ignore them in all our calculations
     * here.
     */

    /* optimization:
     *
     * if there were only MPI_LBs and MPI_UBs in the struct type,
     * treat it as a zero-element contiguous (just as count == 0).
     */
    if (nr_basics == 0 && nr_derived == 0)
    {
	err = PREPEND_PREFIX(Dataloop_create_contiguous)(0,
							 MPI_INT,
							 dlp_p,
							 dlsz_p,
							 dldepth_p,
							 flag);
	return err;
    }

    /* optimization:
     *
     * if there is only one unique instance of a type in the struct, treat it
     * as a blockindexed type.
     *
     * notes:
     *
     * if the displacement happens to be zero, the blockindexed code will
     * optimize this into a contig.
     */
    if (nr_basics + nr_derived == 1)
    {
	/* type_pos is index to only real type in array */
	err = PREPEND_PREFIX(Dataloop_create_blockindexed)
	    (1, /* count */
	     blklens[type_pos],
	     &disps[type_pos],
	     1, /* displacement in bytes */
	     oldtypes[type_pos],
	     dlp_p,
	     dlsz_p,
	     dldepth_p,
	     flag);

	return err;
    }

    /* optimization:
     *
     * if there only one unique type (more than one instance) in the
     * struct, treat it as an indexed type.
     *
     * notes:
     * 
     * this will apply to a single type with an LB/UB, as those
     * are handled elsewhere.
     *
     */
    if (((nr_derived == 0) && (first_basic != MPI_DATATYPE_NULL)) ||
	((nr_basics == 0) && (first_derived != MPI_DATATYPE_NULL)))
    {
	return DLOOP_Dataloop_create_unique_type_struct(count,
							blklens,
							disps,
							oldtypes,
							type_pos,
							dlp_p,
							dlsz_p,
							dldepth_p,
							flag);
    }

    /* optimization:
     *
     * if there are no derived types and caller indicated either a
     * homogeneous system or the "all bytes" conversion, convert
     * everything to bytes and use an indexed type.
     */
    if (nr_derived == 0 && ((flag == DLOOP_DATALOOP_HOMOGENEOUS) ||
			    (flag == DLOOP_DATALOOP_ALL_BYTES)))
    {
	return DLOOP_Dataloop_create_basic_all_bytes_struct(count,
							    blklens,
							    disps,
							    oldtypes,
							    dlp_p,
							    dlsz_p,
							    dldepth_p,
							    flag);
    }

    /* optimization:
     *
     * if caller asked for homogeneous or all bytes representation,
     * flatten the type and store it as an indexed type so that
     * there are no branches in the dataloop tree.
     */
    if ((flag == DLOOP_DATALOOP_HOMOGENEOUS) ||
	     (flag == DLOOP_DATALOOP_ALL_BYTES))
    {
	return DLOOP_Dataloop_create_flattened_struct(count,
						      blklens,
						      disps,
						      oldtypes,
						      dlp_p,
						      dlsz_p,
						      dldepth_p,
						      flag);
    }

    /* scan through types and gather derived type info */
    for (i=0; i < count; i++)
    {
	/* ignore type elements with a zero blklen */
	if (blklens[i] == 0) continue;

	if (DLOOP_Handle_hasloop_macro(oldtypes[i]))
	{
	    int tmp_loop_depth, tmp_loop_sz;

	    DLOOP_Handle_get_loopdepth_macro(oldtypes[i], tmp_loop_depth, flag);
	    DLOOP_Handle_get_loopsize_macro(oldtypes[i], tmp_loop_sz, flag);

	    if (tmp_loop_depth > old_loop_depth)
	    {
		old_loop_depth = tmp_loop_depth;
	    }
	    old_loop_sz += tmp_loop_sz;
	}
    }
    
    /* general case below: 2 or more distinct types that are either
     * basics or derived, and for which we want to preserve the types
     * themselves.
     */
    
    if (nr_basics > 0)
    {
	/* basics introduce an extra level of depth, so if our new depth
	 * must be at least 2 if there are basics.
	 */
	new_loop_depth = ((old_loop_depth+1) > 2) ? (old_loop_depth+1) : 2;
    }
    else
    {
	new_loop_depth = old_loop_depth + 1;
    }

    PREPEND_PREFIX(Dataloop_struct_alloc)((DLOOP_Count) nr_basics + nr_derived,
					  old_loop_sz,
					  nr_basics,
					  &curpos,
					  &new_dlp,
					  &new_loop_sz);
    /* --BEGIN ERROR HANDLING-- */
    if (!new_dlp)
    {
	return DLOOP_Dataloop_create_struct_memory_error();
    }
    /* --END ERROR HANDLING-- */


    new_dlp->kind = DLOOP_KIND_STRUCT;
    new_dlp->el_size = -1; /* not valid for struct */
    new_dlp->el_extent = -1; /* not valid for struct; see el_extent_array */
    new_dlp->el_type = MPI_DATATYPE_NULL; /* not valid for struct */

    new_dlp->loop_params.s_t.count = (DLOOP_Count) nr_basics + nr_derived;

    /* note: curpos points to first byte in "old dataloop" region of
     * newly allocated space.
     */

    for (i=0, loop_idx = 0; i < count; i++)
    {
	int is_builtin;

	/* ignore type elements with a zero blklen */
	if (blklens[i] == 0) continue;

	is_builtin = (DLOOP_Handle_hasloop_macro(oldtypes[i])) ? 0 : 1;

	if (is_builtin)
	{
	    DLOOP_Dataloop *dummy_dlp;
	    int dummy_sz, dummy_depth;

	    /* LBs and UBs already taken care of -- skip them */
	    if (oldtypes[i] == MPI_LB || oldtypes[i] == MPI_UB)
	    {
		continue;
	    }

	    /* build a contig dataloop for this basic and point to that
	     *
	     * optimization:
	     *
	     * push the count (blklen) from the struct down into the
	     * contig so we can process more at the leaf.
	     */
	    err = PREPEND_PREFIX(Dataloop_create_contiguous)(blklens[i],
							     oldtypes[i],
							     &dummy_dlp,
							     &dummy_sz,
							     &dummy_depth,
							     flag);
	    
	    /* --BEGIN ERROR HANDLING-- */
	    if (err) {
		/* TODO: FREE ALLOCATED RESOURCES */
		return -1;
	    }
	    /* --END ERROR HANDLING-- */

	    /* copy the new contig loop into place in the struct memory
	     * region
	     */
	    PREPEND_PREFIX(Dataloop_copy)(curpos, dummy_dlp, dummy_sz);
	    new_dlp->loop_params.s_t.dataloop_array[loop_idx] = curpos;
	    curpos = (DLOOP_Dataloop *) ((char *) curpos + dummy_sz);

	    /* we stored the block size in the contig -- use 1 here */
	    new_dlp->loop_params.s_t.blocksize_array[loop_idx] = 1;
	    new_dlp->loop_params.s_t.el_extent_array[loop_idx] =
		((DLOOP_Offset) blklens[i]) * dummy_dlp->el_extent;
	    PREPEND_PREFIX(Dataloop_free)(&dummy_dlp);
	}
	else
	{
	    DLOOP_Dataloop *old_loop_ptr;
	    int old_loop_sz;
	    DLOOP_Offset old_extent;

	    DLOOP_Handle_get_loopptr_macro(oldtypes[i], old_loop_ptr, flag);
	    DLOOP_Handle_get_loopsize_macro(oldtypes[i], old_loop_sz, flag);
	    DLOOP_Handle_get_extent_macro(oldtypes[i], old_extent);

	    PREPEND_PREFIX(Dataloop_copy)(curpos, old_loop_ptr, old_loop_sz);
	    new_dlp->loop_params.s_t.dataloop_array[loop_idx] = curpos;
	    curpos = (DLOOP_Dataloop *) ((char *) curpos + old_loop_sz);

	    new_dlp->loop_params.s_t.blocksize_array[loop_idx] =
		(DLOOP_Count) blklens[i];
	    new_dlp->loop_params.s_t.el_extent_array[loop_idx] =
		old_extent;
	}
	new_dlp->loop_params.s_t.offset_array[loop_idx] =
	    (DLOOP_Offset) disps[i];
	loop_idx++;
    }

    *dlp_p     = new_dlp;
    *dlsz_p    = new_loop_sz;
    *dldepth_p = new_loop_depth;

    return 0;
}

/* --BEGIN ERROR HANDLING-- */
static int DLOOP_Dataloop_create_struct_memory_error(void)
{
    return -1;
}
/* --END ERROR HANDLING-- */

static int DLOOP_Dataloop_create_unique_type_struct(int count,
						    int *blklens,
						    MPI_Aint *disps,
						    DLOOP_Type *oldtypes,
						    int type_pos,
						    DLOOP_Dataloop **dlp_p,
						    int *dlsz_p,
						    int *dldepth_p,
						    int flag)
{
    /* the same type used more than once in the array; type_pos
     * indexes to the first of these.
     */
    int i, err, *tmp_blklens, cur_pos = 0;
    DLOOP_Offset *tmp_disps;

    /* count is an upper bound on number of type instances */
    tmp_blklens = (int *) DLOOP_Malloc(count * sizeof(int));
    /* --BEGIN ERROR HANDLING-- */
    if (!tmp_blklens) {
	/* TODO: ??? */
	return DLOOP_Dataloop_create_struct_memory_error();
    }
    /* --END ERROR HANDLING-- */

    tmp_disps = (DLOOP_Offset *)
	DLOOP_Malloc(count * sizeof(DLOOP_Offset));
    /* --BEGIN ERROR HANDLING-- */
    if (!tmp_disps) {
	DLOOP_Free(tmp_blklens);
	/* TODO: ??? */
	return DLOOP_Dataloop_create_struct_memory_error();
    }
    /* --END ERROR HANDLING-- */

    for (i=type_pos; i < count; i++)
    {
	if (oldtypes[i] == oldtypes[type_pos] && blklens != 0)
	{
	    tmp_blklens[cur_pos] = blklens[i];
	    tmp_disps[cur_pos]   = disps[i];
	    cur_pos++;
	}
    }

    err = PREPEND_PREFIX(Dataloop_create_indexed)(cur_pos,
						  tmp_blklens,
						  tmp_disps,
						  1, /* disp in bytes */
						  oldtypes[type_pos],
						  dlp_p,
						  dlsz_p,
						  dldepth_p,
						  flag);

    DLOOP_Free(tmp_blklens);
    DLOOP_Free(tmp_disps);

    return err;

}

static int DLOOP_Dataloop_create_basic_all_bytes_struct(
	       int count,
	       int *blklens,
	       MPI_Aint *disps,
	       DLOOP_Type *oldtypes,
	       DLOOP_Dataloop **dlp_p,
	       int *dlsz_p,
	       int *dldepth_p,
	       int flag)
{
    int i, err, cur_pos = 0;
    int *tmp_blklens;
    MPI_Aint *tmp_disps;

    /* count is an upper bound on number of type instances */
    tmp_blklens = (int *) DLOOP_Malloc(count * sizeof(int));

    /* --BEGIN ERROR HANDLING-- */
    if (!tmp_blklens)
    {
	return DLOOP_Dataloop_create_struct_memory_error();
    }
    /* --END ERROR HANDLING-- */

    tmp_disps = (MPI_Aint *) DLOOP_Malloc(count * sizeof(MPI_Aint));

    /* --BEGIN ERROR HANDLING-- */
    if (!tmp_disps)
    {
	DLOOP_Free(tmp_blklens);
	return DLOOP_Dataloop_create_struct_memory_error();
    }
    /* --END ERROR HANDLING-- */

    for (i=0; i < count; i++)
    {
	if (oldtypes[i] != MPI_LB && oldtypes[i] != MPI_UB && blklens[i] != 0)
	{
	    DLOOP_Offset sz;

	    DLOOP_Handle_get_size_macro(oldtypes[i], sz);
	    tmp_blklens[cur_pos] = (int) sz * blklens[i];
	    tmp_disps[cur_pos]   = disps[i];
	    cur_pos++;
	}
    }
    err = PREPEND_PREFIX(Dataloop_create_indexed)(cur_pos,
						  tmp_blklens,
						  tmp_disps,
						  1, /* disp in bytes */
						  MPI_BYTE,
						  dlp_p,
						  dlsz_p,
						  dldepth_p,
						  flag);
    
    DLOOP_Free(tmp_blklens);
    DLOOP_Free(tmp_disps);

    return err;
}

static int DLOOP_Dataloop_create_flattened_struct(int count,
						  int *blklens,
						  MPI_Aint *disps,
						  DLOOP_Type *oldtypes,
						  DLOOP_Dataloop **dlp_p,
						  int *dlsz_p,
						  int *dldepth_p,
						  int flag)
{
    /* arbitrary types, convert to bytes and use indexed */
    int i, err, *tmp_blklens, nr_blks = 0;
    MPI_Aint *tmp_disps; /* since we're calling another fn that takes
			    this type as an input parameter */
    DLOOP_Offset bytes;
    DLOOP_Segment *segp;

    int first_ind, last_ind;

    segp = PREPEND_PREFIX(Segment_alloc)();
    /* --BEGIN ERROR HANDLING-- */
    if (!segp) {
	return DLOOP_Dataloop_create_struct_memory_error();
    }
    /* --END ERROR HANDLING-- */

    /* use segment code once to count contiguous regions */
    for (i=0; i < count; i++)
    {
	int is_basic;

	/* ignore type elements with a zero blklen */
	if (blklens[i] == 0) continue;

	is_basic = (DLOOP_Handle_hasloop_macro(oldtypes[i])) ? 0 : 1;

	if (is_basic && (oldtypes[i] != MPI_LB &&
			 oldtypes[i] != MPI_UB))
	{
	    nr_blks++;
	}
	else /* derived type; get a count of contig blocks */
	{
	    DLOOP_Count tmp_nr_blks;

	    PREPEND_PREFIX(Segment_init)(NULL,
					 (DLOOP_Count) blklens[i],
					 oldtypes[i],
					 segp,
					 flag);
	    bytes = SEGMENT_IGNORE_LAST;

	    PREPEND_PREFIX(Segment_count_contig_blocks)(segp,
							0,
							&bytes,
							&tmp_nr_blks);

	    nr_blks += tmp_nr_blks;
	}
    }

    nr_blks += 2; /* safety measure */

    tmp_blklens = (int *) DLOOP_Malloc(nr_blks * sizeof(int));
    /* --BEGIN ERROR HANDLING-- */
    if (!tmp_blklens) {
	return DLOOP_Dataloop_create_struct_memory_error();
    }
    /* --END ERROR HANDLING-- */


    tmp_disps = (MPI_Aint *) DLOOP_Malloc(nr_blks * sizeof(MPI_Aint));
    /* --BEGIN ERROR HANDLING-- */
    if (!tmp_disps) {
	DLOOP_Free(tmp_blklens);
	return DLOOP_Dataloop_create_struct_memory_error();
    }
    /* --END ERROR HANDLING-- */

    /* use segment code again to flatten the type */
    first_ind = 0;
    for (i=0; i < count; i++)
    {
	/* we're going to use the segment code to flatten the type.
	 * we put in our displacement as the buffer location, and use
	 * the blocklength as the count value to get N contiguous copies
	 * of the type.
	 *
	 * Note that we're going to get back values in bytes, so that will
	 * be our new element type.
	 */
	if (oldtypes[i] != MPI_UB && oldtypes[i] != MPI_LB && blklens[i] != 0)
	{
	    PREPEND_PREFIX(Segment_init)((char *) disps[i],
					 (DLOOP_Count) blklens[i],
					 oldtypes[i],
					 segp,
					 0 /* homogeneous */);
	    
	    last_ind = nr_blks - first_ind;
	    bytes = SEGMENT_IGNORE_LAST;
	    PREPEND_PREFIX(Segment_mpi_flatten)(segp,
						0,
						&bytes,
						&tmp_blklens[first_ind],
						&tmp_disps[first_ind],
						&last_ind);
	    first_ind += last_ind;
	}
    }
    nr_blks = first_ind;

#if 0
    if (MPIU_DBG_SELECTED(DATATYPE,VERBOSE)) {
	MPIU_DBG_OUT(DATATYPE,"--- start of flattened type ---");
        for (i=0; i < nr_blks; i++) {
	MPIU_DBG_OUT_FMT(DATATYPE,(MPIU_DBG_FDEST,
				   "a[%d] = (%d, %d)\n", i,
				   tmp_blklens[i], tmp_disps[i]));
	}
	MPIU_DBG_OUT(DATATYPE,"--- end of flattened type ---");
    }
#endif

    PREPEND_PREFIX(Segment_free)(segp);

    err = PREPEND_PREFIX(Dataloop_create_indexed)(nr_blks,
						  tmp_blklens,
						  tmp_disps,
						  1, /* disp in bytes */
						  MPI_BYTE,
						  dlp_p,
						  dlsz_p,
						  dldepth_p,
						  flag);
    
    DLOOP_Free(tmp_blklens);
    DLOOP_Free(tmp_disps);

    return err;
}
