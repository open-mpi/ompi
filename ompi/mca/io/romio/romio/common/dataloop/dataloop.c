/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "./dataloop.h"

#undef DEBUG_DLOOP_SIZE
#undef DLOOP_DEBUG_MEMORY

/* Dataloops
 *
 * The functions here are used for the creation, copying, update, and display
 * of DLOOP_Dataloop structures and trees of these structures.
 *
 * Currently we store trees of dataloops in contiguous regions of memory.  They
 * are stored in such a way that subtrees are also stored contiguously.  This
 * makes it somewhat easier to copy these subtrees around.  Keep this in mind
 * when looking at the functions below.
 *
 * The structures used in this file are defined in mpid_datatype.h.  There is
 * no separate mpid_dataloop.h at this time.
 *
 * OPTIMIZATIONS:
 *
 * There are spots in the code with OPT tags that indicate where we could 
 * optimize particular calculations or avoid certain checks.
 *
 * NOTES:
 *
 * Don't have locks in place at this time!
 */

/* Some functions in this file are responsible for allocation of space for
 * dataloops.  These structures include the dataloop structure itself
 * followed by a sequence of variable-sized arrays, depending on the loop
 * kind.  For example, a dataloop of kind DLOOP_KIND_INDEXED has a
 * dataloop structure followed by an array of block sizes and then an array
 * of offsets.
 *
 * For efficiency and ease of cleanup (preserving a single free at
 * deallocation), we want to allocate this memory as a single large chunk.
 * However, we must perform some alignment of the components of this chunk
 * in order to obtain correct and efficient operation across all platforms.
 */


/*@
  Dataloop_free - deallocate the resources used to store a dataloop

  Input Parameters:
. dataloop - pointer to dataloop structure
@*/
void PREPEND_PREFIX(Dataloop_free)(DLOOP_Dataloop **dataloop)
{

    if (*dataloop == NULL) return;

#ifdef DLOOP_DEBUG_MEMORY
    DLOOP_dbg_printf("DLOOP_Dataloop_free: freeing loop @ %x.\n",
		     (int) *dataloop);
#endif

    memset(*dataloop, 0, sizeof(DLOOP_Dataloop_common));
    DLOOP_Free(*dataloop);
    *dataloop = NULL;
    return;
}
/*@
  Dataloop_copy - Copy an arbitrary dataloop structure, updating
  pointers as necessary

  Input Parameters:
+ dest   - pointer to destination region
. src    - pointer to original dataloop structure
- size   - size of dataloop structure

  This routine parses the dataloop structure as it goes in order to
  determine what exactly it needs to update.

  Notes:
  It assumes that the source dataloop was allocated in our usual way;
  this means that the entire dataloop is in a contiguous region and that
  the root of the tree is first in the array.

  This has some implications:
+ we can use a contiguous copy mechanism to copy the majority of the
  structure
- all pointers in the region are relative to the start of the data region
  the first dataloop in the array is the root of the tree
@*/
void PREPEND_PREFIX(Dataloop_copy)(void *dest,
				   void *src,
				   int size)
{
    DLOOP_Offset ptrdiff;

#ifdef DLOOP_DEBUG_MEMORY
    DLOOP_dbg_printf("DLOOP_Dataloop_copy: copying from %x to %x (%d bytes).\n",
		     (int) src, (int) dest, size);
#endif

    /* copy region first */
    memcpy(dest, src, size);

    /* Calculate difference in starting locations. DLOOP_Dataloop_update()
     * then traverses the new structure and updates internal pointers by
     * adding this difference to them. This way we can just copy the
     * structure, including pointers, in one big block.
     */
    ptrdiff = (char *) dest - (char *) src;

    /* traverse structure updating pointers */
    PREPEND_PREFIX(Dataloop_update)(dest, ptrdiff);

    return;
}


/*@
  Dataloop_update - update pointers after a copy operation

  Input Parameters:
+ dataloop - pointer to loop to update
- ptrdiff - value indicating offset between old and new pointer values

  This function is used to recursively update all the pointers in a
  dataloop tree.
@*/
void PREPEND_PREFIX(Dataloop_update)(DLOOP_Dataloop *dataloop,
				     DLOOP_Offset ptrdiff)
{
    /* OPT: only declare these variables down in the Struct case */
    int i;
    DLOOP_Dataloop **looparray;

    switch(dataloop->kind & DLOOP_KIND_MASK) {
	case DLOOP_KIND_CONTIG:
	case DLOOP_KIND_VECTOR:
	    /*
	     * All these really ugly assignments are really of the form:
	     *
	     * ((char *) dataloop->loop_params.c_t.loop) += ptrdiff;
	     *
	     * However, some compilers spit out warnings about casting on the
	     * LHS, so we get this much nastier form instead (using common
	     * struct for contig and vector): 
	     */
	    dataloop->loop_params.cm_t.dataloop = (DLOOP_Dataloop *) 
		((char *) dataloop->loop_params.cm_t.dataloop + ptrdiff);

	    if (!(dataloop->kind & DLOOP_FINAL_MASK))
		PREPEND_PREFIX(Dataloop_update)(dataloop->loop_params.cm_t.dataloop, ptrdiff);
	    break;

	case DLOOP_KIND_BLOCKINDEXED:
	    dataloop->loop_params.bi_t.offset_array = (DLOOP_Offset *)
		((char *) dataloop->loop_params.bi_t.offset_array + ptrdiff);
	    dataloop->loop_params.bi_t.dataloop = (DLOOP_Dataloop *)
		((char *) dataloop->loop_params.bi_t.dataloop + ptrdiff);

	    if (!(dataloop->kind & DLOOP_FINAL_MASK))
		PREPEND_PREFIX(Dataloop_update)(dataloop->loop_params.bi_t.dataloop, ptrdiff);
	    break;

	case DLOOP_KIND_INDEXED:
	    dataloop->loop_params.i_t.blocksize_array = (DLOOP_Count *)
		((char *) dataloop->loop_params.i_t.blocksize_array + ptrdiff);
	    dataloop->loop_params.i_t.offset_array = (DLOOP_Offset *)
		((char *) dataloop->loop_params.i_t.offset_array + ptrdiff);
	    dataloop->loop_params.i_t.dataloop = (DLOOP_Dataloop *)
		((char *) dataloop->loop_params.i_t.dataloop + ptrdiff);

	    if (!(dataloop->kind & DLOOP_FINAL_MASK))
		PREPEND_PREFIX(Dataloop_update)(dataloop->loop_params.i_t.dataloop, ptrdiff);
	    break;

	case DLOOP_KIND_STRUCT:
	    dataloop->loop_params.s_t.blocksize_array = (DLOOP_Count *)
		((char *) dataloop->loop_params.s_t.blocksize_array + ptrdiff);
	    dataloop->loop_params.s_t.offset_array = (DLOOP_Offset *)
		((char *) dataloop->loop_params.s_t.offset_array + ptrdiff);
	    dataloop->loop_params.s_t.dataloop_array = (DLOOP_Dataloop **)
		((char *) dataloop->loop_params.s_t.dataloop_array + ptrdiff);

	    /* fix the N dataloop pointers too */
	    looparray = dataloop->loop_params.s_t.dataloop_array;
	    for (i=0; i < dataloop->loop_params.s_t.count; i++) {
		looparray[i] = (DLOOP_Dataloop *)
		    ((char *) looparray[i] + ptrdiff);
	    }

	    if (dataloop->kind & DLOOP_FINAL_MASK) break;

	    for (i=0; i < dataloop->loop_params.s_t.count; i++) {
		PREPEND_PREFIX(Dataloop_update)(looparray[i], ptrdiff);
	    }
	    break;
	default:
	    /* --BEGIN ERROR HANDLING-- */
	    DLOOP_Assert(0);
	    break;
	    /* --END ERROR HANDLING-- */
    }
    return;
}

/*@
  Dataloop_alloc - allocate the resources used to store a dataloop with
                   no old loops associated with it.

  Input Parameters:
+ kind          - kind of dataloop to allocate
. count         - number of elements in dataloop (kind dependent)
. new_loop_p    - address at which to store new dataloop pointer
- new_loop_sz_p - pointer to integer in which to store new loop size

  Notes:
  The count parameter passed into this function will often be different
  from the count passed in at the MPI layer due to optimizations.
@*/
void PREPEND_PREFIX(Dataloop_alloc)(int kind,
				    DLOOP_Count count,
				    DLOOP_Dataloop **new_loop_p,
				    int *new_loop_sz_p)
{
    PREPEND_PREFIX(Dataloop_alloc_and_copy)(kind,
					    count,
					    NULL,
					    0,
					    new_loop_p,
					    new_loop_sz_p);
    return;
}

/*@
  Dataloop_alloc_and_copy - allocate the resources used to store a
                            dataloop and copy in old dataloop as
			    appropriate

  Input Parameters:
+ kind          - kind of dataloop to allocate
. count         - number of elements in dataloop (kind dependent)
. old_loop      - pointer to old dataloop (or NULL for none)
. old_loop_sz   - size of old dataloop (should be zero if old_loop is NULL)
. new_loop_p    - address at which to store new dataloop pointer
- new_loop_sz_p - pointer to integer in which to store new loop size

  Notes:
  The count parameter passed into this function will often be different
  from the count passed in at the MPI layer.
@*/
void PREPEND_PREFIX(Dataloop_alloc_and_copy)(int kind,
					     DLOOP_Count count,
					     DLOOP_Dataloop *old_loop,
					     int old_loop_sz,
					     DLOOP_Dataloop **new_loop_p,
					     int *new_loop_sz_p)
{
    int new_loop_sz = 0;
    int align_sz = 8; /* default aligns everything to 8-byte boundaries */
    int epsilon;
    int loop_sz = sizeof(DLOOP_Dataloop);
    int off_sz = 0, blk_sz = 0, ptr_sz = 0, extent_sz = 0;

    char *pos;
    DLOOP_Dataloop *new_loop;

#ifdef HAVE_MAX_STRUCT_ALIGNMENT
    if (align_sz > HAVE_MAX_STRUCT_ALIGNMENT) {
	align_sz = HAVE_MAX_STRUCT_ALIGNMENT;
    }
#endif

    if (old_loop != NULL) {
	DLOOP_Assert((old_loop_sz % align_sz) == 0);
    }

    /* calculate the space that we actually need for everything */
    switch (kind) {
	case DLOOP_KIND_STRUCT:
	    /* need space for dataloop pointers and extents */
	    ptr_sz = count * sizeof(DLOOP_Dataloop *);
	    extent_sz = count * sizeof(DLOOP_Offset);
	case DLOOP_KIND_INDEXED:
	    /* need space for block sizes */
	    blk_sz = count * sizeof(DLOOP_Count);
	case DLOOP_KIND_BLOCKINDEXED:
	    /* need space for block offsets */
	    off_sz = count * sizeof(DLOOP_Offset);
	case DLOOP_KIND_CONTIG:
	case DLOOP_KIND_VECTOR:
	    break;
	default:
	    DLOOP_Assert(0);
    }

    /* pad everything that we're going to allocate */
    epsilon = loop_sz % align_sz;
    if (epsilon) loop_sz += align_sz - epsilon;

    epsilon = off_sz % align_sz;
    if (epsilon) off_sz += align_sz - epsilon;

    epsilon = blk_sz % align_sz;
    if (epsilon) blk_sz += align_sz - epsilon;

    epsilon = ptr_sz % align_sz;
    if (epsilon) ptr_sz += align_sz - epsilon;

    epsilon = extent_sz % align_sz;
    if (epsilon) extent_sz += align_sz - epsilon;

    new_loop_sz += loop_sz + off_sz + blk_sz + ptr_sz +
	extent_sz + old_loop_sz;

    /* allocate space */
    new_loop = (DLOOP_Dataloop *) DLOOP_Malloc(new_loop_sz);
    if (new_loop == NULL) {
	*new_loop_p = NULL;
	return;
    }

#ifdef DLOOP_DEBUG_MEMORY
    DLOOP_dbg_printf("DLOOP_Dataloop_alloc_and_copy: new loop @ %x (tot sz = %d, loop = %d, off = %d, blk = %d, ptr = %d, extent = %d, old = %d)\n",
		     (int) new_loop,
		     new_loop_sz,
		     loop_sz,
		     off_sz,
		     blk_sz,
		     ptr_sz,
		     extent_sz,
		     old_loop_sz);
#endif

    /* set all the pointers in the new dataloop structure */
    switch (kind) {
	case DLOOP_KIND_STRUCT:
	    /* order is:
	     * - pointers
	     * - blocks
	     * - offsets
	     * - extents
	     */
	    new_loop->loop_params.s_t.dataloop_array =
		(DLOOP_Dataloop **) (((char *) new_loop) + loop_sz);
	    new_loop->loop_params.s_t.blocksize_array =
		(DLOOP_Count *) (((char *) new_loop) + loop_sz + ptr_sz);
	    new_loop->loop_params.s_t.offset_array =
		(DLOOP_Offset *) (((char *) new_loop) + loop_sz +
				  ptr_sz + blk_sz);
	    new_loop->loop_params.s_t.el_extent_array =
		(DLOOP_Offset *) (((char *) new_loop) + loop_sz +
				  ptr_sz + blk_sz + off_sz);
	    break;
	case DLOOP_KIND_INDEXED:
	    /* order is:
	     * - blocks
	     * - offsets
	     */
	    new_loop->loop_params.i_t.blocksize_array =
		(DLOOP_Count *) (((char *) new_loop) + loop_sz);
	    new_loop->loop_params.i_t.offset_array =
		(DLOOP_Offset *) (((char *) new_loop) + loop_sz + blk_sz);
	    if (old_loop == NULL) {
		new_loop->loop_params.i_t.dataloop = NULL;
	    }
	    else {
		new_loop->loop_params.i_t.dataloop =
		    (DLOOP_Dataloop *) (((char *) new_loop) +
					(new_loop_sz - old_loop_sz));
	    }
	    break;
	case DLOOP_KIND_BLOCKINDEXED:
	    new_loop->loop_params.bi_t.offset_array =
		(DLOOP_Offset *) (((char *) new_loop) + loop_sz);
	    if (old_loop == NULL) {
		new_loop->loop_params.bi_t.dataloop = NULL;
	    }
	    else {
		new_loop->loop_params.bi_t.dataloop =
		    (DLOOP_Dataloop *) (((char *) new_loop) +
					(new_loop_sz - old_loop_sz));
	    }
	    break;
	case DLOOP_KIND_CONTIG:
	    if (old_loop == NULL) {
		new_loop->loop_params.c_t.dataloop = NULL;
	    }
	    else {
		new_loop->loop_params.c_t.dataloop =
		    (DLOOP_Dataloop *) (((char *) new_loop) +
					(new_loop_sz - old_loop_sz));
	    }
	    break;
	case DLOOP_KIND_VECTOR:
	    if (old_loop == NULL) {
		new_loop->loop_params.v_t.dataloop = NULL;
	    }
	    else {
		new_loop->loop_params.v_t.dataloop =
		    (DLOOP_Dataloop *) (((char *) new_loop) +
					(new_loop_sz - old_loop_sz));
	    }
	    break;
	default:
	    DLOOP_Assert(0);
    }

    pos = ((char *) new_loop) + (new_loop_sz - old_loop_sz);
    if (old_loop != NULL) {
	PREPEND_PREFIX(Dataloop_copy)(pos, old_loop, old_loop_sz);
    }

    *new_loop_p    = new_loop;
    *new_loop_sz_p = new_loop_sz;
    return;
}

/*@
  Dataloop_struct_alloc - allocate the resources used to store a dataloop and
                          copy in old dataloop as appropriate.  this version
                          is specifically for use when a struct dataloop is
                          being created; the space to hold old dataloops in
                          this case must be described back to the
                          implementation in order for efficient copying.

  Input Parameters:
+ count         - number of elements in dataloop (kind dependent)
. old_loop_sz   - size of old dataloop (should be zero if old_loop is NULL)
. basic_ct      - number of basic types for which new dataloops are needed
. old_loop_p    - address at which to store pointer to old loops
. new_loop_p    - address at which to store new struct dataloop pointer
- new_loop_sz_p - address at which to store new loop size

  Notes:
  The count parameter passed into this function will often be different
  from the count passed in at the MPI layer due to optimizations.

  The caller is responsible for filling in the region pointed to by
  old_loop_p (count elements).
@*/
void PREPEND_PREFIX(Dataloop_struct_alloc)(DLOOP_Count count,
					   int old_loop_sz,
					   int basic_ct,
					   DLOOP_Dataloop **old_loop_p,
					   DLOOP_Dataloop **new_loop_p,
					   int *new_loop_sz_p)
{
    int new_loop_sz = 0;
    int align_sz = 8; /* default aligns everything to 8-byte boundaries */
    int epsilon;
    int loop_sz = sizeof(DLOOP_Dataloop);
    int off_sz, blk_sz, ptr_sz, extent_sz, basic_sz;

    DLOOP_Dataloop *new_loop;

#ifdef HAVE_MAX_STRUCT_ALIGNMENT
    if (align_sz > HAVE_MAX_STRUCT_ALIGNMENT) {
	align_sz = HAVE_MAX_STRUCT_ALIGNMENT;
    }
#endif

    /* calculate the space that we actually need for everything */
    ptr_sz    = count * sizeof(DLOOP_Dataloop *);
    extent_sz = count * sizeof(DLOOP_Offset);
    blk_sz    = count * sizeof(DLOOP_Count);
    off_sz    = count * sizeof(DLOOP_Offset);
    basic_sz  = sizeof(DLOOP_Dataloop);

    /* pad everything that we're going to allocate */
    epsilon = loop_sz % align_sz;
    if (epsilon) loop_sz += align_sz - epsilon;

    epsilon = off_sz % align_sz;
    if (epsilon) off_sz += align_sz - epsilon;

    epsilon = blk_sz % align_sz;
    if (epsilon) blk_sz += align_sz - epsilon;

    epsilon = ptr_sz % align_sz;
    if (epsilon) ptr_sz += align_sz - epsilon;

    epsilon = extent_sz % align_sz;
    if (epsilon) extent_sz += align_sz - epsilon;

    epsilon = basic_sz % align_sz;
    if (epsilon) basic_sz += align_sz - epsilon;

    /* note: we pad *each* basic type dataloop, because the
     * code used to create them assumes that we're going to
     * do that.
     */

    new_loop_sz += loop_sz + off_sz + blk_sz + ptr_sz +
	extent_sz + (basic_ct * basic_sz) + old_loop_sz;

    /* allocate space */
    new_loop = (DLOOP_Dataloop *) DLOOP_Malloc(new_loop_sz);
    if (new_loop == NULL) {
	*new_loop_p = NULL;
	return;
    }

#ifdef DLOOP_DEBUG_MEMORY
    DLOOP_dbg_printf("DLOOP_Dataloop_struct_alloc: new loop @ %x (tot sz = %d, loop = %d, off = %d, blk = %d, ptr = %d, extent = %d, basics = %d, old = %d)\n",
		     (int) new_loop,
		     new_loop_sz,
		     loop_sz,
		     off_sz,
		     blk_sz,
		     ptr_sz,
		     extent_sz,
		     basic_sz,
		     old_loop_sz);
#endif

    /* set all the pointers in the new dataloop structure */
    new_loop->loop_params.s_t.dataloop_array = (DLOOP_Dataloop **)
	(((char *) new_loop) + loop_sz);
    new_loop->loop_params.s_t.blocksize_array =	(DLOOP_Count *)
	(((char *) new_loop) + loop_sz + ptr_sz);
    new_loop->loop_params.s_t.offset_array = (DLOOP_Offset *)
	(((char *) new_loop) + loop_sz + ptr_sz + blk_sz);
    new_loop->loop_params.s_t.el_extent_array =	(DLOOP_Offset *)
	(((char *) new_loop) + loop_sz + ptr_sz + blk_sz + off_sz);

    *old_loop_p = (DLOOP_Dataloop *)
	(((char *) new_loop) + loop_sz + ptr_sz + blk_sz + off_sz + extent_sz);
    *new_loop_p = new_loop;
    *new_loop_sz_p = new_loop_sz;

    return;
}

/*@
  Dataloop_dup - make a copy of a dataloop

  Returns 0 on success, -1 on failure.
@*/
void PREPEND_PREFIX(Dataloop_dup)(DLOOP_Dataloop *old_loop,
				  int old_loop_sz,
				  DLOOP_Dataloop **new_loop_p)
{
    DLOOP_Dataloop *new_loop;

    DLOOP_Assert(old_loop != NULL);
    DLOOP_Assert(old_loop_sz > 0);
    
    new_loop = (DLOOP_Dataloop *) DLOOP_Malloc(old_loop_sz);
    if (new_loop == NULL) {
	*new_loop_p = NULL;
	return;
    }

    PREPEND_PREFIX(Dataloop_copy)(new_loop, old_loop, old_loop_sz);
    *new_loop_p = new_loop;
    return;
}

/*@
  Dataloop_stream_size - return the size of the data described by the dataloop

  Input Parameters:
+ dl_p   - pointer to dataloop for which we will return the size
- sizefn - function for determining size of types in the corresponding stream
           (passing NULL will instead result in el_size values being used)

@*/
DLOOP_Offset
PREPEND_PREFIX(Dataloop_stream_size)(struct DLOOP_Dataloop *dl_p,
				     DLOOP_Offset (*sizefn)(DLOOP_Type el_type))
{
    DLOOP_Offset ret;
    DLOOP_Offset tmp_sz, tmp_ct = 1;
 
    for (;;)
    {
        if ((dl_p->kind & DLOOP_KIND_MASK) == DLOOP_KIND_STRUCT)
        {
            int i;
            
            tmp_sz = 0;
            for (i = 0; i < dl_p->loop_params.s_t.count; i++)
            {
                ret += dl_p->loop_params.s_t.blocksize_array[i] *
                    PREPEND_PREFIX(Dataloop_stream_size)(dl_p->loop_params.s_t.dataloop_array[i], sizefn);
            }
            return tmp_sz * tmp_ct;
        }

        switch (dl_p->kind & DLOOP_KIND_MASK) {
        case DLOOP_KIND_CONTIG:
            tmp_ct *= dl_p->loop_params.c_t.count;
#ifdef DLOOP_DEBUG_SIZE
            DLOOP_dbg_printf("stream_size: contig: ct = %d; new tot_ct = %d\n",
                             (int) dl_p->loop_params.c_t.count, (int) tmp_ct);
#endif
            break;
        case DLOOP_KIND_VECTOR:
            tmp_ct *= dl_p->loop_params.v_t.count * dl_p->loop_params.v_t.blocksize;
#ifdef DLOOP_DEBUG_SIZE
            DLOOP_dbg_printf("stream_size: vector: ct = %d; blk = %d; new tot_ct = %d\n",
                             (int) dl_p->loop_params.v_t.count,
                             (int) dl_p->loop_params.v_t.blocksize,
                             (int) tmp_ct);
#endif
            break;
        case DLOOP_KIND_BLOCKINDEXED:
            tmp_ct *= dl_p->loop_params.bi_t.count * dl_p->loop_params.bi_t.blocksize;
#ifdef DLOOP_DEBUG_SIZE
            DLOOP_dbg_printf("stream_size: blkindexed: blks = %d; new tot_ct = %d\n",
                             (int) dl_p->loop_params.bi_t.count *
                             (int) dl_p->loop_params.bi_t.blocksize,
                             (int) tmp_ct);
#endif
            break;
        case DLOOP_KIND_INDEXED:
            tmp_ct *= dl_p->loop_params.i_t.total_blocks;
#ifdef DLOOP_DEBUG_SIZE
            DLOOP_dbg_printf("stream_size: contig: blks = %d; new tot_ct = %d\n",
                             (int) dl_p->loop_params.i_t.total_blocks,
                             (int) tmp_ct);
#endif
            break;
        default:
            /* --BEGIN ERROR HANDLING-- */
            DLOOP_Assert(0);
            break;
            /* --END ERROR HANDLING-- */
        }

        if (dl_p->kind & DLOOP_FINAL_MASK) break;
        else {
            DLOOP_Assert(dl_p->loop_params.cm_t.dataloop != NULL);
            dl_p = dl_p->loop_params.cm_t.dataloop;
        }
    }

    /* call fn for size using bottom type, or use size if fnptr is NULL */
    tmp_sz = ((sizefn) ? sizefn(dl_p->el_type) : dl_p->el_size);

    return tmp_sz * tmp_ct;
}

/* --BEGIN ERROR HANDLING-- */
/*@
  Dataloop_print - dump a dataloop tree to stdout for debugging
  purposes

  Input Parameters:
+ dataloop - root of tree to dump
- depth - starting depth; used to help keep up with where we are in the tree
@*/
void PREPEND_PREFIX(Dataloop_print)(struct DLOOP_Dataloop *dataloop,
				    int depth)
{
    int i;

    if (dataloop == NULL)
    {
        DLOOP_dbg_printf("dataloop is NULL (probably basic type)\n");
        return;
    }

    DLOOP_dbg_printf("loc=%p, treedepth=%d, kind=%d, el_extent=%ld\n",
		     dataloop, (int) depth, (int) dataloop->kind, (long) dataloop->el_extent);
    switch(dataloop->kind & DLOOP_KIND_MASK) {
	case DLOOP_KIND_CONTIG:
	    DLOOP_dbg_printf("\tCONTIG: count=%d, datatype=%p\n", 
			     (int) dataloop->loop_params.c_t.count, 
			     dataloop->loop_params.c_t.dataloop);
	    if (!(dataloop->kind & DLOOP_FINAL_MASK))
		PREPEND_PREFIX(Dataloop_print)(dataloop->loop_params.c_t.dataloop, depth+1);
	    break;
	case DLOOP_KIND_VECTOR:
	    DLOOP_dbg_printf("\tVECTOR: count=%d, blksz=%d, stride=%d, datatype=%p\n",
			     (int) dataloop->loop_params.v_t.count, 
			     (int) dataloop->loop_params.v_t.blocksize, 
			     (int) dataloop->loop_params.v_t.stride,
			     dataloop->loop_params.v_t.dataloop);
	    if (!(dataloop->kind & DLOOP_FINAL_MASK))
		PREPEND_PREFIX(Dataloop_print)(dataloop->loop_params.v_t.dataloop, depth+1);
	    break;
	case DLOOP_KIND_BLOCKINDEXED:
	    DLOOP_dbg_printf("\tBLOCKINDEXED: count=%d, blksz=%d, datatype=%p\n",
			     (int) dataloop->loop_params.bi_t.count, 
			     (int) dataloop->loop_params.bi_t.blocksize, 
			     dataloop->loop_params.bi_t.dataloop);
	    /* print out offsets later */
	    if (!(dataloop->kind & DLOOP_FINAL_MASK))
		PREPEND_PREFIX(Dataloop_print)(dataloop->loop_params.bi_t.dataloop, depth+1);
	    break;
	case DLOOP_KIND_INDEXED:
	    DLOOP_dbg_printf("\tINDEXED: count=%d, datatype=%p\n",
			     (int) dataloop->loop_params.i_t.count,
			     dataloop->loop_params.i_t.dataloop);
	    /* print out blocksizes and offsets later */
	    if (!(dataloop->kind & DLOOP_FINAL_MASK))
		PREPEND_PREFIX(Dataloop_print)(dataloop->loop_params.i_t.dataloop, depth+1);
	    break;
	case DLOOP_KIND_STRUCT:
	    DLOOP_dbg_printf("\tSTRUCT: count=%d\n", (int) dataloop->loop_params.s_t.count);
	    DLOOP_dbg_printf("\tblocksizes:\n");
	    for (i=0; i < dataloop->loop_params.s_t.count; i++)
		DLOOP_dbg_printf("\t\t%d\n", (int) dataloop->loop_params.s_t.blocksize_array[i]);
	    DLOOP_dbg_printf("\toffsets:\n");
	    for (i=0; i < dataloop->loop_params.s_t.count; i++)
		DLOOP_dbg_printf("\t\t%d\n", (int) dataloop->loop_params.s_t.offset_array[i]);
	    DLOOP_dbg_printf("\tdatatypes:\n");
	    for (i=0; i < dataloop->loop_params.s_t.count; i++)
		DLOOP_dbg_printf("\t\t%p\n", dataloop->loop_params.s_t.dataloop_array[i]);
	    if (dataloop->kind & DLOOP_FINAL_MASK) break;

	    for (i=0; i < dataloop->loop_params.s_t.count; i++) {
		PREPEND_PREFIX(Dataloop_print)(dataloop->loop_params.s_t.dataloop_array[i],depth+1);
	    }
	    break;
	default:
	    DLOOP_Assert(0);
	    break;
    }
    return;
}
/* --END ERROR HANDLING-- */
