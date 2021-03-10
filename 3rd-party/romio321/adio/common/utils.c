/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *   Copyright (C) 2014 UChicgo/Argonne, LLC.
 *   See COPYRIGHT notice in top-level directory.
 */

#include <adio.h>
#include <limits.h>


/* utility function for creating large contiguous types: algorithim from BigMPI
 * https://github.com/jeffhammond/BigMPI */

static int type_create_contiguous_x(MPI_Count count,
	MPI_Datatype oldtype, MPI_Datatype *newtype)
{
    /* to make 'count' fit MPI-3 type processing routines (which take integer
     * counts), we construct a type consisting of N INT_MAX chunks followed by
     * a remainder.  e.g for a count of 4000000000 bytes you would end up with
     * one 2147483647-byte chunk followed immediately by a 1852516353-byte
     * chunk */
    MPI_Datatype chunks, remainder;
    MPI_Aint lb, extent, disps[2];
    int blocklens[2];
    MPI_Datatype types[2];

    /* truly stupendously large counts will overflow an integer with this math,
     * but that is a problem for a few decades from now.  Sorry, few decades
     * from now! */
    ADIOI_Assert(count/INT_MAX == (int)(count/INT_MAX));
    int c = (int)(count/INT_MAX); /* OK to cast until 'count' is 256 bits */
    int r = count%INT_MAX;

    MPI_Type_vector(c, INT_MAX, INT_MAX, oldtype, &chunks);
    MPI_Type_contiguous(r, oldtype, &remainder);

    MPI_Type_get_extent(oldtype, &lb, &extent);

    blocklens[0] = 1;      blocklens[1] = 1;
    disps[0]     = 0;      disps[1]     = c*extent*INT_MAX;
    types[0]     = chunks; types[1]     = remainder;

    MPI_Type_create_struct(2, blocklens, disps, types, newtype);

    MPI_Type_free(&chunks);
    MPI_Type_free(&remainder);

    return MPI_SUCCESS;
}
/* like MPI_Type_create_hindexed, except array_of_lengths can be a larger datatype.
 *
 * Hindexed provides 'count' pairs of (displacement, length), but what if
 * length is longer than an integer?  We will create 'count' types, using
 * contig if length is small enough, or something more complex if not */

int ADIOI_Type_create_hindexed_x(int count,
		const MPI_Count array_of_blocklengths[],
		const MPI_Aint array_of_displacements[],
		MPI_Datatype oldtype,
		MPI_Datatype *newtype)
{
    int i, ret;
    MPI_Datatype *types;
    int *blocklens;
    int is_big=0;

    types = ADIOI_Malloc(count*sizeof(MPI_Datatype));
    blocklens = ADIOI_Malloc(count*sizeof(int));

    /* squashing two loops into one.
     * - Look in the array_of_blocklengths for any large values
     * - convert MPI_Count items (if they are not too big) into int-sized items
     * after this loop we will know if we can use MPI_type_hindexed or if we
     * need a more complicated BigMPI-style struct-of-chunks.
     *
     * Why not use the struct-of-chunks in all cases?  HDF5 reported a bug,
     * which I have not yet precicesly nailed down, but appears to have
     * something to do with struct-of-chunks when the chunks are small */

    for(i=0; i<count; i++) {
	if (array_of_blocklengths[i] > INT_MAX) {
	    blocklens[i] = 1;
	    is_big=1;
	    type_create_contiguous_x(array_of_blocklengths[i], oldtype,  &(types[i]));
	} else {
	    /* OK to cast: checked for "bigness" above */
	    blocklens[i] = (int)array_of_blocklengths[i];
	    MPI_Type_contiguous(blocklens[i], oldtype, &(types[i]));
	}
    }

    if (is_big) {
	ret = MPI_Type_create_struct(count, blocklens, array_of_displacements,
		types, newtype);
    } else {
	ret = MPI_Type_create_hindexed(count, blocklens,
		array_of_displacements, oldtype, newtype);
    }
    for (i=0; i< count; i++)
	MPI_Type_free(&(types[i]));
    ADIOI_Free(types);
    ADIOI_Free(blocklens);

    return ret;
}

/* some systems do not have pread/pwrite, or requrie XOPEN_SOURCE set higher
 * than we would like.  see #1973 */
#if (HAVE_DECL_PWRITE == 0)

#include <sys/types.h>
#include <unistd.h>

ssize_t pread(int fd, void *buf, size_t count, off_t offset);
ssize_t pwrite(int fd, const void *buf, size_t count, off_t offset);

ssize_t pread(int fd, void *buf, size_t count, off_t offset) {
    off_t lseek_ret;
    off_t old_offset;
    ssize_t read_ret;

    old_offset = lseek(fd, 0, SEEK_CUR);
    lseek_ret = lseek(fd, offset, SEEK_SET);
    if (lseek_ret == -1)
	return lseek_ret;
    read_ret = read(fd, buf, count);
    if (read_ret < 0)
	return read_ret;
    /* man page says "file offset is not changed" */
    if ( (lseek_ret = lseek(fd, old_offset, SEEK_SET)) < 0)
	return lseek_ret;

    return read_ret;
}

ssize_t pwrite(int fd, const void *buf, size_t count, off_t offset) {
    off_t lseek_ret;
    off_t old_offset;
    ssize_t write_ret;

    old_offset = lseek(fd, 0, SEEK_CUR);
    lseek_ret = lseek(fd, offset, SEEK_SET);
    if (lseek_ret == -1)
	return lseek_ret;
    write_ret = write(fd, buf, count);
    if (write_ret < 0)
	return write_ret;
    /* man page says "file offset is not changed" */
    if ( (lseek_ret = lseek(fd, old_offset, SEEK_SET)) < 0)
	return lseek_ret;

    return write_ret;
}
#endif

/*
 * vim: ts=8 sts=4 sw=4 noexpandtab
 */
