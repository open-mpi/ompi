/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2007 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <stdio.h>
#include <stdlib.h>

#include "romio_dataloop.h"
#include "typesize_support.h"

/* Nomenclature:
 * ib_ - intermediate buffer
 * fv_ - file view
 * ub_ - user buffer
 * ds_ - data sieving
 */

/* MPIO_File_ib_params - used when finding "good" data in intermediate buffer
 *
 * direction   - either DLOOP_M2M_TO_USERBUF or DLOOP_M2M_FROM_USERBUF
 * iobuf       - location of IO buffer in memory
 * ib_file_off - location in file corresponding to first byte in buffer
 * ib_file_size     - size of valid data in IO buffer
 * fv_disp     - displacement of file view (not kept elsewhere)
 *
 * ub_segp     - segment corresponding to user buffer (with valid bufp)
 *
 * note: fv_disp is here mainly because the "bufp" value in the segment
 *       isn't necessarily large enough to hold an MPI_Offset.
 */
struct MPIO_File_ib_params {
    int           direction;
    char         *ib_ptr;
    MPI_Offset    ib_file_off, ib_file_size;
    MPI_Offset    fv_disp;

    DLOOP_Segment *ub_segp;
};

/* MPIO_File_ds_params
 *
 * Used to drive process of sifting data through intermediate buffer
 *
 * fv_disp        - file view displacement
 * ib_size        - size of intermediate (data sieving) buffer
 * ib_file_off    - starting location of next file access going to buffer
 * ib_file_size   - size of access going to buffer
 * ib_fv_off      - file view (stream) location corresponding buffer start
 * ib_fv_last     - stream location one past last one going to buffer
 *
 * ib - 
 * ib_segp -
 */
struct MPIO_File_ds_params {
    int           direction;
    MPI_Offset    fv_disp;
    MPI_Offset    ib_size;

    /* cur_file_size helps us track what I/O would be necessary to cover
     * the stream region we've processed so far.
     */
    MPI_Offset    ib_file_off, ib_file_size;
    MPI_Offset    ib_fv_off, ib_fv_last;

    struct MPIO_File_ib_params ib;
    DLOOP_Segment *ib_segp;
};

static int MPIO_Segment_contig_file_ds(MPI_Offset *blocks_p,
				       MPI_Datatype el_type,
				       MPI_Offset rel_off,
				       void *unused,
				       void *v_paramp);

static int MPIO_Segment_contig_fv2ib(DLOOP_Offset *blocks_p,
				     DLOOP_Type    el_type,
				     DLOOP_Offset  rel_off,
				     void         *unused,
				     void         *v_paramp);
static int MPIO_File_datasieve(struct MPIO_File_ds_params *dsp,
			       MPI_Offset *fv_lastp);


static int MPIO_Segment_contig_m2m_fake(DLOOP_Offset *blocks_p,
					DLOOP_Type el_type,
					DLOOP_Offset rel_off,
					void *unused,
					void *v_paramp);

int MPIO_File_ds_io(MPI_Aint    *ub_ptr,
		    int          ub_count,
		    MPI_Datatype ub_type,
		    MPI_Offset   fv_off,
		    MPI_Offset   fv_disp,
		    MPI_Datatype fv_type,
		    int          direction,
		    MPI_Offset   ib_size)
{
    /* "b" variables refer to memory buffer, "f" variables to file regions */
    int ub_ncontig, fv_ncontig;
    MPI_Offset ub_blkoff, ub_blkct, ub_typesize;
    MPI_Offset fv_blkoff, fv_blkct, fv_size, fv_count, fv_counttouch;

    MPI_Offset size;
    char *ib_ptr;
    struct MPIO_File_ds_params *dsp;
    struct MPIO_File_ib_params *ibp;
    DLOOP_Segment *ds_segp;

    MPIO_Datatype_get_size(fv_type, &fv_size);

    /* calculate size of access in bytes using buffer type */
    MPIO_Datatype_get_size(ub_type, &ub_typesize);
    size = ub_typesize * ub_count;

    /* fv_counttouch is count of file types touched by this particular
     * access. used to determine if we're really contiguous in file or not.
     */
    fv_counttouch = (((fv_off % fv_size) ? 1 : 0) +
		     (size - fv_size + (fv_off % fv_size)) / fv_size + 
		     (((size - fv_size + (fv_off % fv_size)) % fv_size) ? 1:0));

    MPIO_Datatype_get_block_info(ub_type, &ub_blkoff, &ub_blkct, &ub_ncontig);
    MPIO_Datatype_get_block_info(fv_type, &fv_blkoff, &fv_blkct, &fv_ncontig);

    /* set up IO buffer */
    DLOOP_Assert(ib_size > 0);
    if (size < ib_size) ib_size = size;
    ib_ptr = (char *) DLOOP_Malloc(ib_size);

    /* optimizations: use simplest possible types when contiguous */
    if (ub_ncontig || (ub_count == 1 && ub_blkct == 1)) {
	/* contiguous in memory -- use very simple contig instead */
	MPI_Datatype eltype;
	MPI_Offset   eltypesz;

	MPIO_Datatype_get_el_type(ub_type, &eltype, 0);
	if (eltype == MPI_DATATYPE_NULL) eltype = MPI_BYTE;

	MPIO_Datatype_get_size(eltype, &eltypesz);
	DLOOP_Assert((size / eltypesz) * eltypesz == size);

	/* it's possible that the original ub_type has a true_lb > 0.
	 * this would be returned in ub_blkoff. we account for this by
	 * incrementing the ub_ptr prior to processing.
	 *
	 * we will do the same thing for the file view below.
	 */
	ub_ptr   += ub_blkoff;
	ub_type   = eltype;
	ub_count  = size / eltypesz;
    }

    if (fv_ncontig || (fv_counttouch == 1 && fv_blkct == 1)) {
	/* contiguous in file -- use very simple contig instead */
	MPI_Datatype eltype;

	MPIO_Datatype_get_el_type(fv_type, &eltype, 0);
	if (eltype == MPI_DATATYPE_NULL) eltype = MPI_BYTE;

	MPIO_Datatype_get_size(eltype, &fv_size);

	fv_off  += fv_blkoff;
	fv_type  = eltype;
	/* fcount assigned below for all cases */
    }

    /* fv_count is the count necessary to encompass from start of file view
     * past end of access. needed for accurate dataloop processing.
     */
    fv_count = ((fv_off + size) / fv_size) +
	(((fv_off + size) % fv_size) ? 1 : 0);

    /* allocate and populate DS (top-level) param structures */
    dsp = (struct MPIO_File_ds_params *)
	DLOOP_Malloc(sizeof(struct MPIO_File_ds_params));
    DLOOP_Assert(dsp != NULL);

    dsp->direction    = direction;
    dsp->fv_disp      = fv_disp;
    dsp->ib_size      = ib_size;
    dsp->ib_file_off  = (MPI_Offset) -1;
    dsp->ib_file_size = 0;
    dsp->ib_fv_off    = (MPI_Offset) -1;
    dsp->ib_fv_last   = fv_off;

    dsp->ib_segp = MPIO_Segment_alloc();
    MPIO_Segment_init(NULL, /* bufp not used */
		      fv_count,
		      fv_type,
		      dsp->ib_segp,
		      0);

    /* allocate and populate IB (mid-level) param structures */
    ibp = (struct MPIO_File_ib_params *)
	DLOOP_Malloc(sizeof(struct MPIO_File_ib_params));
    DLOOP_Assert(ibp != NULL);

    ibp->ib_ptr       = ib_ptr;
    ibp->direction    = direction;
    ibp->ib_file_off  = (MPI_Offset) -1;
    ibp->ib_file_size = (MPI_Offset) -1;

    ibp->ub_segp = MPIO_Segment_alloc();
    MPIO_Segment_init(ub_ptr,
		      ub_count,
		      ub_type,
		      ibp->ub_segp,
		      0);

    /* allocate segment to drive the whole process */
    ds_segp = MPIO_Segment_alloc();
    MPIO_Segment_init(NULL, /* bufp not used */
		      fv_count,
		      fv_type,
		      ds_segp,
		      0);

    MPIO_Segment_manipulate(ds_segp,
			    fv_off,
			    &size,
			    MPIO_Segment_contig_file_ds,
			    NULL, NULL, NULL, NULL, 0);

    /* finish any remaining I/O */
    if (dsp->ib_file_off != 0) {
	MPIO_File_datasieve(dsp, &size);
    }

    /* free resources */
    DLOOP_Free(ib_ptr);
    MPIO_Segment_free(dsp->ib_segp);
    DLOOP_Free(dsp);
    MPIO_Segment_free(ibp->ub_segp);
    DLOOP_Free(ibp);
    MPIO_Segment_free(ds_segp);

    return 0;
}

/* MPIO_Segment_contig_file_ds
 *
 * Make progress on a data sieving I/O operation for a contiguous
 * region in the file view.
 */
static int MPIO_Segment_contig_file_ds(MPI_Offset *fv_blocks_p,
				       MPI_Datatype fv_eltype,
				       MPI_Offset rel_file_off,
				       void *unused,
				       void *v_paramp)
{
    MPI_Offset fv_blocks = *fv_blocks_p, fv_elsize;
    struct MPIO_File_ds_params *dsp = v_paramp;

    MPIO_Datatype_get_size(fv_eltype, &fv_elsize);

    while (fv_blocks > 0) {
	/* pastlastbyte is 1 more than the last byte location */
	MPI_Offset thisfirstbyte, blocksizebytes, pastlastbyte;
	MPI_Offset min_access_size, full_access_size;
	MPI_Offset fv_last;

	blocksizebytes = fv_blocks * fv_elsize;
	thisfirstbyte  = dsp->fv_disp + rel_file_off;
	pastlastbyte   = dsp->fv_disp + rel_file_off + blocksizebytes;

	/* start building a new access if one isn't in progress */
	if (dsp->ib_file_off == (MPI_Offset) -1) {
	    dsp->ib_file_off  = thisfirstbyte;
	    dsp->ib_file_size = 0;
	    dsp->ib_fv_off    = dsp->ib_fv_last;
	}

	/* monotonically nondecreasing file views guarantee that we don't
	 * move backward in the file as we move forward in the stream.
	 */
	DLOOP_Assert(thisfirstbyte >= dsp->ib_file_off);

	/* min_access_size = size needed to get even one element
	 * full_access_size = size needed to get all elements
	 */
	min_access_size  = (thisfirstbyte - dsp->ib_file_off) + fv_elsize;
	full_access_size = pastlastbyte - dsp->ib_file_off;

	if (dsp->ib_size > full_access_size) {
	    /* the entire region fits in the iobuf with space to spare.
	     * update current stream offset and file size but wait to
	     * perform I/O.
	     */
	    dsp->ib_file_size = pastlastbyte - dsp->ib_file_off;
	    dsp->ib_fv_last   = dsp->ib_fv_last + blocksizebytes;

	    /* update blocks to account for progress (I/O will be covered in a
	     * subsequent pass).
	     *
	     * note: we don't bother to update rel_file_off because we're done
	     * with this region.
	     */
	    fv_blocks = 0;

	    continue;
	}
	
	if (dsp->ib_size < min_access_size) {
	    /* iobuf will not fit any of this region; perform I/O 
	     * for previous regions and then deal with this one.
	     */
	}
	else if (dsp->ib_size <= full_access_size) {
	    /* this I/O can complete some or all of this region */
	    MPI_Offset remaining_iobuf, partial_elements;

	    remaining_iobuf = dsp->ib_size - (thisfirstbyte - dsp->ib_file_off);
	    partial_elements = (remaining_iobuf / fv_elsize);

	    /* update stored last file view offset (will verify later) */
	    dsp->ib_fv_last += partial_elements * fv_elsize;

	    /* update blocks and rel_off to account for upcoming progress */
	    fv_blocks       -= partial_elements;
	    rel_file_off    += partial_elements * fv_elsize;
	}
		
	fv_last = dsp->ib_fv_last;

	/* perform read */
	MPIO_File_datasieve(dsp, &fv_last);
    }

    return 0;
}

/* MPIO_File_datasieve
 *
 */
static int MPIO_File_datasieve(struct MPIO_File_ds_params *dsp,
			       MPI_Offset *fv_lastp)
{
    DLOOP_Assert(*fv_lastp == dsp->ib_fv_last);

    /* perform read */
    printf("READ: buf = %x, off = %d, size = %d\n",
	   (unsigned int) dsp->ib.ib_ptr,
	   (int) dsp->ib_file_off,
	   (int) dsp->ib_file_size);

    /* copy data between ib and ub */
    MPIO_Segment_manipulate(dsp->ib_segp,
			    dsp->ib_fv_off,
			    fv_lastp,
			    MPIO_Segment_contig_fv2ib,
			    NULL, NULL, NULL, NULL,
			    &dsp->ib);

    DLOOP_Assert(*fv_lastp == dsp->ib_fv_last);

    if (dsp->direction == DLOOP_M2M_FROM_USERBUF) {
	/* perform write */
	printf("WRITE: buf = %x, off = %d, size = %d\n",
	       (unsigned int) dsp->ib.ib_ptr,
	       (int) dsp->ib_file_off,
	       (int) dsp->ib_file_size);
    }
    
    /* mark iobuf values as unused */
    dsp->ib_file_off  = (MPI_Offset) -1;
    dsp->ib_file_size = 0;
    dsp->ib_fv_off    = (MPI_Offset) -1;
    /* dsp->ib_fv_last remains pointing to next byte to access */

    return 0;
}

/* MPIO_Segment_contig_fv2ib
 *
 * Map contiguous region in file view to a region in the intermediate
 * buffer, then use m2m functions (pack/unpack) to transfer into
 * user buffer.
 *
 * Parameters:
 * fv_blocks_p  - number of eltypes in contiguous fv region
 * fv_eltype    - element type for region
 * rel_file_off - offset into actual file, relative to fv_disp (displacement)
 * v_paramp     - pointer to a MPIO_File_ib_params structure
 *
 * Variables:
 * ib_off       - offset into intermed. buffer for start of "good" data
 */
static int MPIO_Segment_contig_fv2ib(DLOOP_Offset *fv_blocks_p,
				     DLOOP_Type    fv_eltype,
				     DLOOP_Offset  rel_file_off,
				     void         *unused,
				     void         *v_paramp)
{
    MPI_Offset fv_elsize, file_off, ub_first, ub_last;
    MPI_Aint ib_off;
    struct MPIO_File_ib_params *ibp = v_paramp;
    struct MPIO_m2m_params m2m_params;

    MPIO_Datatype_get_size(fv_eltype, &fv_elsize);

    file_off = ibp->fv_disp + rel_file_off;
    ib_off   = (MPI_Aint) (file_off - ibp->ib_file_off);

    /* we never attempt to process the filetype before the IO buffer or
     * beyond the IO buffer.
     */
    DLOOP_Assert(ib_off >= 0);
    DLOOP_Assert((ib_off + (*fv_blocks_p * fv_elsize)) <= ibp->ib_file_size);

    /* fill in parameters for m2m functions */
    m2m_params.direction = ibp->direction;
    m2m_params.streambuf = ibp->ib_ptr + ib_off;
    m2m_params.userbuf   = ibp->ub_segp->ptr;

    /* we always move through the memory type in order, so the last stored
     * stream location (stream_off) is where we want to start next.
     */
    ub_first             = ibp->ub_segp->stream_off;
    ub_last              = *fv_blocks_p * fv_elsize;

    /* move to/from user buffer (using m2m fns in segment_ops.c) */
    MPIO_Segment_manipulate(ibp->ub_segp, ub_first, &ub_last,
#if 0
			    MPIO_Segment_contig_m2m,
			    MPIO_Segment_vector_m2m,
			    MPIO_Segment_blkidx_m2m,
			    MPIO_Segment_index_m2m,
#else
			    MPIO_Segment_contig_m2m_fake,
			    NULL, NULL, NULL,
#endif
			    NULL,
			    &m2m_params);

    /* data should always be successfully processed by m2m code */
    DLOOP_Assert(ub_last == *fv_blocks_p * fv_elsize);

    return 0;
}

static int MPIO_Segment_contig_m2m_fake(DLOOP_Offset *blocks_p,
					DLOOP_Type el_type,
					DLOOP_Offset rel_off,
					void *unused,
					void *v_paramp)
{
    DLOOP_Offset el_size;
    DLOOP_Offset size;
    struct PREPEND_PREFIX(m2m_params) *paramp = v_paramp;

    DLOOP_Handle_get_size_macro(el_type, el_size);
    size = *blocks_p * el_size;

    printf("\t[%s: ub_start=%x (%x+%x), ib_start=%x, sz=%d (%d*%d)]\n",
	   (paramp->direction == DLOOP_M2M_TO_USERBUF) ? "unpack" : "pack",
	   (unsigned int) (paramp->userbuf + rel_off),
	   (unsigned) paramp->userbuf, (unsigned) rel_off,
	   (unsigned) paramp->streambuf,
	   (int) size,
	   (int) *blocks_p, (int) el_size);

#if 0
    if (paramp->direction == DLOOP_M2M_TO_USERBUF) {
	memcpy((char *) (paramp->userbuf + rel_off), paramp->streambuf, size);
    }
    else {
	memcpy(paramp->streambuf, (char *) (paramp->userbuf + rel_off), size);
    }
#endif
    paramp->streambuf += size;
    return 0;
}

/* 
 * Local variables:
 * c-indent-tabs-mode: nil
 * End:
 */
