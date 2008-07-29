/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <stdlib.h>

#include "./dataloop.h"

static DLOOP_Count DLOOP_Type_indexed_count_contig(DLOOP_Count count,
						   int *blocklength_array,
						   void *displacement_array,
						   int dispinbytes,
						   DLOOP_Offset old_extent);

static void DLOOP_Type_indexed_array_copy(DLOOP_Count count,
					  DLOOP_Count contig_count,
					  int *input_blocklength_array,
					  void *input_displacement_array,
					  DLOOP_Count *output_blocklength_array,
					  DLOOP_Offset *out_disp_array,
					  int dispinbytes,
					  DLOOP_Offset old_extent);

/*@
   DLOOP_Dataloop_create_indexed

   Arguments:
+  int icount
.  int *iblocklength_array
.  void *displacement_array (either ints or MPI_Aints)
.  int dispinbytes
.  MPI_Datatype oldtype
.  DLOOP_Dataloop **dlp_p
.  int *dlsz_p
.  int *dldepth_p
-  int flag

.N Errors
.N Returns 0 on success, -1 on error.
@*/

int PREPEND_PREFIX(Dataloop_create_indexed)(int icount,
					    int *blocklength_array,
					    void *displacement_array,
					    int dispinbytes,
					    MPI_Datatype oldtype,
					    DLOOP_Dataloop **dlp_p,
					    int *dlsz_p,
					    int *dldepth_p,
					    int flag)
{
    int err, is_builtin;
    int i, new_loop_sz, old_loop_depth, blksz;

    DLOOP_Count old_type_count = 0, contig_count, count;
    DLOOP_Offset old_extent;
    struct DLOOP_Dataloop *new_dlp;

    count = (DLOOP_Count) icount; /* avoid subsequent casting */


    /* if count is zero, handle with contig code, call it an int */
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

    is_builtin = (DLOOP_Handle_hasloop_macro(oldtype)) ? 0 : 1;

    if (is_builtin)
    {
	DLOOP_Handle_get_extent_macro(oldtype, old_extent);
	old_loop_depth = 0;
    }
    else
    {
	DLOOP_Handle_get_extent_macro(oldtype, old_extent);
	DLOOP_Handle_get_loopdepth_macro(oldtype, old_loop_depth, flag);
    }

    for (i=0; i < count; i++)
    {
	old_type_count += (DLOOP_Count) blocklength_array[i];
    }

    contig_count = DLOOP_Type_indexed_count_contig(count,
						   blocklength_array,
						   displacement_array,
						   dispinbytes,
						   old_extent);

    /* if contig_count is zero (no data), handle with contig code */
    if (contig_count == 0)
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
     * if contig_count == 1 and block starts at displacement 0,
     * store it as a contiguous rather than an indexed dataloop.
     */
    if ((contig_count == 1) &&
	((!dispinbytes && ((int *) displacement_array)[0] == 0) ||
	 (dispinbytes && ((MPI_Aint *) displacement_array)[0] == 0)))
    {
	err = PREPEND_PREFIX(Dataloop_create_contiguous)((int) old_type_count,
							 oldtype,
							 dlp_p,
							 dlsz_p,
							 dldepth_p,
							 flag);
	return err;
    }

    /* optimization:
     *
     * if contig_count == 1 (and displacement != 0), store this as
     * a single element blockindexed rather than a lot of individual
     * blocks.
     */
    if (contig_count == 1)
    {
	err = PREPEND_PREFIX(Dataloop_create_blockindexed)(1,
							   (int) old_type_count,
							   displacement_array,
							   dispinbytes,
							   oldtype,
							   dlp_p,
							   dlsz_p,
							   dldepth_p,
							   flag);

	return err;
    }

    /* optimization:
     *
     * if block length is the same for all blocks, store it as a
     * blockindexed rather than an indexed dataloop.
     */
    blksz = blocklength_array[0];
    for (i=1; i < count; i++)
    {
	if (blocklength_array[i] != blksz)
	{
	    blksz--;
	    break;
	}
    }
    if (blksz == blocklength_array[0])
    {
	err = PREPEND_PREFIX(Dataloop_create_blockindexed)(icount,
							   blksz,
							   displacement_array,
							   dispinbytes,
							   oldtype,
							   dlp_p,
							   dlsz_p,
							   dldepth_p,
							   flag);

	return err;
    }

    /* note: blockindexed looks for the vector optimization */

    /* TODO: optimization:
     *
     * if an indexed of a contig, absorb the contig into the blocklen array
     * and keep the same overall depth
     */

    /* otherwise storing as an indexed dataloop */

    if (is_builtin)
    {
	PREPEND_PREFIX(Dataloop_alloc)(DLOOP_KIND_INDEXED,
				       count,
				       &new_dlp,
				       &new_loop_sz);
	/* --BEGIN ERROR HANDLING-- */
	if (!new_dlp) return -1;
	/* --END ERROR HANDLING-- */

	new_dlp->kind = DLOOP_KIND_INDEXED | DLOOP_FINAL_MASK;

	if (flag == DLOOP_DATALOOP_ALL_BYTES)
	{
	    /* blocklengths are modified below */
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

	PREPEND_PREFIX(Dataloop_alloc_and_copy)(DLOOP_KIND_INDEXED,
						contig_count,
						old_loop_ptr,
						old_loop_sz,
						&new_dlp,
						&new_loop_sz);
	/* --BEGIN ERROR HANDLING-- */
	if (!new_dlp) return -1;
	/* --END ERROR HANDLING-- */

	new_dlp->kind = DLOOP_KIND_INDEXED;

	DLOOP_Handle_get_size_macro(oldtype, new_dlp->el_size);
	DLOOP_Handle_get_extent_macro(oldtype, new_dlp->el_extent);
	DLOOP_Handle_get_basic_type_macro(oldtype, new_dlp->el_type);
    }

    new_dlp->loop_params.i_t.count        = contig_count;
    new_dlp->loop_params.i_t.total_blocks = old_type_count;

    /* copy in blocklength and displacement parameters (in that order)
     *
     * regardless of dispinbytes, we store displacements in bytes in loop.
     */
    DLOOP_Type_indexed_array_copy(count,
				  contig_count,
				  blocklength_array,
				  displacement_array,
				  new_dlp->loop_params.i_t.blocksize_array,
				  new_dlp->loop_params.i_t.offset_array,
				  dispinbytes,
				  old_extent);

    if (is_builtin && (flag == DLOOP_DATALOOP_ALL_BYTES))
    {
	DLOOP_Count *tmp_blklen_array =
	    new_dlp->loop_params.i_t.blocksize_array;

	for (i=0; i < contig_count; i++)
	{
	    /* increase block lengths so they are in bytes */
	    tmp_blklen_array[i] *= old_extent;
	}
    }

    *dlp_p     = new_dlp;
    *dlsz_p    = new_loop_sz;
    *dldepth_p = old_loop_depth + 1;

    return MPI_SUCCESS;
}

/* DLOOP_Type_indexed_array_copy()
 *
 * Copies arrays into place, combining adjacent contiguous regions and
 * dropping zero-length regions.
 *
 * Extent passed in is for the original type.
 *
 * Output displacements are always output in bytes, while block
 * lengths are always output in terms of the base type.
 */
static void DLOOP_Type_indexed_array_copy(DLOOP_Count count,
					  DLOOP_Count contig_count,
					  int *in_blklen_array,
					  void *in_disp_array,
					  DLOOP_Count *out_blklen_array,
					  DLOOP_Offset *out_disp_array,
					  int dispinbytes,
					  DLOOP_Offset old_extent)
{
    DLOOP_Count i, cur_idx = 0;

    out_blklen_array[0] = (DLOOP_Count) in_blklen_array[0];

    if (!dispinbytes)
    {
	out_disp_array[0] = (DLOOP_Offset)
	    ((int *) in_disp_array)[0] * old_extent;
	
	for (i = 1; i < count; i++)
	{
	    if (in_blklen_array[i] == 0)
	    {
		continue;
	    }
	    else if (out_disp_array[cur_idx] +
		     ((DLOOP_Offset) out_blklen_array[cur_idx]) * old_extent ==
		     ((DLOOP_Offset) ((int *) in_disp_array)[i]) * old_extent)
	    {
		/* adjacent to current block; add to block */
		out_blklen_array[cur_idx] += (DLOOP_Count) in_blklen_array[i];
	    }
	    else
	    {
		cur_idx++;
		DLOOP_Assert(cur_idx < contig_count);
		out_disp_array[cur_idx] =
		    ((DLOOP_Offset) ((int *) in_disp_array)[i]) * old_extent;
		out_blklen_array[cur_idx] = in_blklen_array[i];
	    }
	}
    }
    else /* input displacements already in bytes */
    {
	out_disp_array[0] = (DLOOP_Offset) ((MPI_Aint *) in_disp_array)[0];
	
	for (i = 1; i < count; i++)
	{
	    if (in_blklen_array[i] == 0)
	    {
		continue;
	    }
	    else if (out_disp_array[cur_idx] +
		     ((DLOOP_Offset) out_blklen_array[cur_idx]) * old_extent ==
		     ((DLOOP_Offset) ((MPI_Aint *) in_disp_array)[i]))
	    {
		/* adjacent to current block; add to block */
		out_blklen_array[cur_idx] += in_blklen_array[i];
	    }
	    else
	    {
		cur_idx++;
		DLOOP_Assert(cur_idx < contig_count);
		out_disp_array[cur_idx] =
		    (DLOOP_Offset) ((MPI_Aint *) in_disp_array)[i];
		out_blklen_array[cur_idx] = (DLOOP_Count) in_blklen_array[i];
	    }
	}
    }

    DLOOP_Assert(cur_idx == contig_count - 1);
    return;
}

/* DLOOP_Type_indexed_count_contig()
 *
 * Determines the actual number of contiguous blocks represented by the
 * blocklength/displacement arrays.  This might be less than count (as
 * few as 1).
 *
 * Extent passed in is for the original type.
 */
static DLOOP_Count DLOOP_Type_indexed_count_contig(DLOOP_Count count,
						   int *blocklength_array,
						   void *displacement_array,
						   int dispinbytes,
						   DLOOP_Offset old_extent)
{
    DLOOP_Count i, contig_count = 1;
    DLOOP_Count cur_blklen = (DLOOP_Count) blocklength_array[0];

    if (!dispinbytes)
    {
	DLOOP_Offset cur_tdisp =
	    (DLOOP_Offset) ((int *) displacement_array)[0];
	
	for (i = 1; i < count; i++)
	{
	    if (blocklength_array[i] == 0)
	    {
		continue;
	    }
	    else if (cur_tdisp + cur_blklen ==
		     (DLOOP_Offset) ((int *) displacement_array)[i])
	    {
		/* adjacent to current block; add to block */
		cur_blklen += (DLOOP_Count) blocklength_array[i];
	    }
	    else
	    {
		cur_tdisp  = (DLOOP_Offset) ((int *) displacement_array)[i];
		cur_blklen = (DLOOP_Count) blocklength_array[i];
		contig_count++;
	    }
	}
    }
    else
    {
	DLOOP_Offset cur_bdisp =
	    (DLOOP_Offset) ((MPI_Aint *) displacement_array)[0];
	
	for (i = 1; i < count; i++)
	{
	    if (blocklength_array[i] == 0)
	    {
		continue;
	    }
	    else if (cur_bdisp + cur_blklen * old_extent ==
		     (DLOOP_Offset) ((MPI_Aint *) displacement_array)[i])
	    {
		/* adjacent to current block; add to block */
		cur_blklen += (DLOOP_Count) blocklength_array[i];
	    }
	    else
	    {
		cur_bdisp  =
		    (DLOOP_Offset) ((MPI_Aint *) displacement_array)[i];
		cur_blklen = (DLOOP_Count) blocklength_array[i];
		contig_count++;
	    }
	}
    }
    return contig_count;
}
