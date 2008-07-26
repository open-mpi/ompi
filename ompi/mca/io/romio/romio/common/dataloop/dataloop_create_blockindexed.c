/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <stdio.h>

#include "./dataloop.h"

static DLOOP_Count DLOOP_Type_blockindexed_count_contig(DLOOP_Count count,
							DLOOP_Count blklen,
							void *disp_array,
							int dispinbytes,
							DLOOP_Offset old_extent);

static void DLOOP_Type_blockindexed_array_copy(DLOOP_Count count,
					       void *disp_array,
					       DLOOP_Offset *out_disp_array,
					       int dispinbytes,
					       DLOOP_Offset old_extent);

/*@
   Dataloop_create_blockindexed - create blockindexed dataloop

   Arguments:
+  int count
.  void *displacement_array (array of either MPI_Aints or ints)
.  int displacement_in_bytes (boolean)
.  MPI_Datatype old_type
.  DLOOP_Dataloop **output_dataloop_ptr
.  int output_dataloop_size
.  int output_dataloop_depth
-  int flag

.N Errors
.N Returns 0 on success, -1 on failure.
@*/
int PREPEND_PREFIX(Dataloop_create_blockindexed)(int icount,
						 int iblklen,
						 void *disp_array,
						 int dispinbytes,
						 DLOOP_Type oldtype,
						 DLOOP_Dataloop **dlp_p,
						 int *dlsz_p,
						 int *dldepth_p,
						 int flag)
{
    int err, is_builtin, is_vectorizable = 1;
    int i, new_loop_sz, old_loop_depth;

    DLOOP_Count contig_count, count, blklen;
    DLOOP_Offset old_extent, eff_disp0, eff_disp1, last_stride;
    DLOOP_Dataloop *new_dlp;

    count  = (DLOOP_Count) icount; /* avoid subsequent casting */
    blklen = (DLOOP_Count) iblklen;

    /* if count or blklen are zero, handle with contig code, call it a int */
    if (count == 0 || blklen == 0)
    {
	err = PREPEND_PREFIX(Dataloop_create_contiguous)(0,
							 MPI_INT,
							 dlp_p,
							 dlsz_p,
							 dldepth_p,
							 flag);
	return err;
    }

    is_builtin = (DLOOP_Handle_hasloop_macro(oldtype)) ? 0 : 1;

    if (is_builtin)
    {
	DLOOP_Handle_get_size_macro(oldtype, old_extent);
	old_loop_depth = 0;
    }
    else
    {
	DLOOP_Handle_get_extent_macro(oldtype, old_extent);
	DLOOP_Handle_get_loopdepth_macro(oldtype, old_loop_depth, flag);
    }

    contig_count = DLOOP_Type_blockindexed_count_contig(count,
							blklen,
							disp_array,
							dispinbytes,
							old_extent);

    /* optimization:
     *
     * if contig_count == 1 and block starts at displacement 0,
     * store it as a contiguous rather than a blockindexed dataloop.
     */
    if ((contig_count == 1) &&
	((!dispinbytes && ((int *) disp_array)[0] == 0) ||
	 (dispinbytes && ((MPI_Aint *) disp_array)[0] == 0)))
    {
	err = PREPEND_PREFIX(Dataloop_create_contiguous)(icount * iblklen,
							 oldtype,
							 dlp_p,
							 dlsz_p,
							 dldepth_p,
							 flag);
	return err;
    }

    /* optimization:
     *
     * if contig_count == 1 store it as a blockindexed with one
     * element rather than as a lot of individual blocks.
     */
    if (contig_count == 1)
    {
	/* adjust count and blklen and drop through */
	blklen *= count;
	count = 1;
	iblklen *= icount;
	icount = 1;
    }

    /* optimization:
     *
     * if displacements start at zero and result in a fixed stride,
     * store it as a vector rather than a blockindexed dataloop.
     */
    eff_disp0 = (dispinbytes) ? ((DLOOP_Offset) ((MPI_Aint *) disp_array)[0]) :
	(((DLOOP_Offset) ((int *) disp_array)[0]) * old_extent);

    if (count > 1 && eff_disp0 == (DLOOP_Offset) 0)
    {
	eff_disp1 = (dispinbytes) ?
	    ((DLOOP_Offset) ((MPI_Aint *) disp_array)[1]) :
	    (((DLOOP_Offset) ((int *) disp_array)[1]) * old_extent);
	last_stride = eff_disp1 - eff_disp0;

	for (i=2; i < count; i++) {
	    eff_disp0 = eff_disp1;
	    eff_disp1 = (dispinbytes) ?
		((DLOOP_Offset) ((MPI_Aint *) disp_array)[i]) :
		(((DLOOP_Offset) ((int *) disp_array)[i]) * old_extent);
	    if (eff_disp1 - eff_disp0 != last_stride) {
		is_vectorizable = 0;
		break;
	    }
	}
	if (is_vectorizable)
	{
	    err = PREPEND_PREFIX(Dataloop_create_vector)(count,
							 blklen,
							 last_stride,
							 1, /* strideinbytes */
							 oldtype,
							 dlp_p,
							 dlsz_p,
							 dldepth_p,
							 flag);
	    return err;
	}
    }

    /* TODO: optimization:
     *
     * if displacements result in a fixed stride, but first displacement
     * is not zero, store it as a blockindexed (blklen == 1) of a vector.
     */

    /* TODO: optimization:
     *
     * if a blockindexed of a contig, absorb the contig into the blocklen
     * parameter and keep the same overall depth
     */

    /* otherwise storing as a blockindexed dataloop */

    /* Q: HOW CAN WE TELL IF IT IS WORTH IT TO STORE AS AN
     * INDEXED WITH FEWER CONTIG BLOCKS (IF CONTIG_COUNT IS SMALL)?
     */

    if (is_builtin)
    {
	PREPEND_PREFIX(Dataloop_alloc)(DLOOP_KIND_BLOCKINDEXED,
				       count,
				       &new_dlp,
				       &new_loop_sz);
	/* --BEGIN ERROR HANDLING-- */
	if (!new_dlp) return -1;
	/* --END ERROR HANDLING-- */

	new_dlp->kind = DLOOP_KIND_BLOCKINDEXED | DLOOP_FINAL_MASK;

	if (flag == DLOOP_DATALOOP_ALL_BYTES)
	{
	    blklen            *= old_extent;
	    new_dlp->el_size   = 1;
	    new_dlp->el_extent = 1;
	    new_dlp->el_type   = MPI_BYTE;
	}
	else
	{
	    new_dlp->el_size   = old_extent;
	    new_dlp->el_extent = old_extent;
	    new_dlp->el_type   = oldtype;
	}
    }
    else
    {
	DLOOP_Dataloop *old_loop_ptr = NULL;
	int old_loop_sz = 0;

	DLOOP_Handle_get_loopptr_macro(oldtype, old_loop_ptr, flag);
	DLOOP_Handle_get_loopsize_macro(oldtype, old_loop_sz, flag);

	PREPEND_PREFIX(Dataloop_alloc_and_copy)(DLOOP_KIND_BLOCKINDEXED,
						count,
						old_loop_ptr,
						old_loop_sz,
						&new_dlp,
						&new_loop_sz);
	/* --BEGIN ERROR HANDLING-- */
	if (!new_dlp) return -1;
	/* --END ERROR HANDLING-- */

	new_dlp->kind = DLOOP_KIND_BLOCKINDEXED;

	DLOOP_Handle_get_size_macro(oldtype, new_dlp->el_size);
	DLOOP_Handle_get_extent_macro(oldtype, new_dlp->el_extent);
	DLOOP_Handle_get_basic_type_macro(oldtype, new_dlp->el_type);
    }

    new_dlp->loop_params.bi_t.count     = count;
    new_dlp->loop_params.bi_t.blocksize = blklen;

    /* copy in displacement parameters
     *
     * regardless of dispinbytes, we store displacements in bytes in loop.
     */
    DLOOP_Type_blockindexed_array_copy(count,
				       disp_array,
				       new_dlp->loop_params.bi_t.offset_array,
				       dispinbytes,
				       old_extent);

    *dlp_p     = new_dlp;
    *dlsz_p    = new_loop_sz;
    *dldepth_p = old_loop_depth + 1;

    return 0;
}

/* DLOOP_Type_blockindexed_array_copy
 *
 * Unlike the indexed version, this one does not compact adjacent
 * blocks, because that would really mess up the blockindexed type!
 */
static void DLOOP_Type_blockindexed_array_copy(DLOOP_Count count,
					       void *in_disp_array,
					       DLOOP_Offset *out_disp_array,
					       int dispinbytes,
					       DLOOP_Offset old_extent)
{
    int i;
    if (!dispinbytes)
    {
	for (i=0; i < count; i++)
	{
	    out_disp_array[i] =
		((DLOOP_Offset) ((int *) in_disp_array)[i]) * old_extent;
	}
    }
    else
    {
	for (i=0; i < count; i++)
	{
	    out_disp_array[i] =
		((DLOOP_Offset) ((MPI_Aint *) in_disp_array)[i]);
	}
    }
    return;
}

static DLOOP_Count DLOOP_Type_blockindexed_count_contig(DLOOP_Count count,
							DLOOP_Count blklen,
							void *disp_array,
							int dispinbytes,
							DLOOP_Offset old_extent)
{
    int i, contig_count = 1;

    if (!dispinbytes)
    {
	/* this is from the MPI type, is of type int */
	DLOOP_Offset cur_tdisp = (DLOOP_Offset) ((int *) disp_array)[0];

	for (i=1; i < count; i++)
	{
	    DLOOP_Offset next_tdisp = (DLOOP_Offset) ((int *) disp_array)[i];

	    if (cur_tdisp + blklen != next_tdisp)
	    {
		contig_count++;
	    }
	    cur_tdisp = next_tdisp;
	}
    }
    else
    {
	/* this is from the MPI type, is of type MPI_Aint */
	DLOOP_Offset cur_bdisp = (DLOOP_Offset) ((MPI_Aint *) disp_array)[0];

	for (i=1; i < count; i++)
	{
	    DLOOP_Offset next_bdisp =
		(DLOOP_Offset) ((MPI_Aint *) disp_array)[i];

	    if (cur_bdisp + (DLOOP_Offset) blklen * old_extent != next_bdisp)
	    {
		contig_count++;
	    }
	    cur_bdisp = next_bdisp;
	}
    }
    return contig_count;
}
