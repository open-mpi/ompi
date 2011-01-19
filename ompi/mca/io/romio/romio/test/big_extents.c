/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*  
 *  (C) 2007 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* a test to exercise very large extents: on most platforms with 32 bit
 * integers, we'd expect these tests to give unexpected values.  On platforms
 * with 64 bit integers, these tests will be fine.  On BlueGene we're not sure
 * yet :> 
 */ 


#include <mpi.h>
#include <stdint.h>
#include <math.h>
#include <stdio.h>

#define CHECK(fn) {int errcode; errcode = (fn); if (errcode != MPI_SUCCESS) handle_error(errcode, NULL); } 


static void handle_error(int errcode, char *str) 
{
	char msg[MPI_MAX_ERROR_STRING];
	int resultlen;
	MPI_Error_string(errcode, msg, &resultlen);
	fprintf(stderr, "%s: %s\n", str, msg);
	MPI_Abort(MPI_COMM_WORLD, 1);
}

static void typestats(MPI_Datatype type) 
{
    MPI_Aint lb, extent;
    int size;

    MPI_Type_get_extent(type, &lb, &extent);
    MPI_Type_size(type, &size);

    printf("dtype %d: lb = %ld extent = %ld size = %d...", 
	    type, (long)lb, (long)extent, size);

}

static int verify_type(char *filename, MPI_Datatype type, 
	int64_t expected_extent, int do_coll)
{
    int rank, canary, tsize;
    int compare=-1;
    int errs=0, toterrs=0;
    MPI_Status status;
    MPI_File fh;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    CHECK( MPI_File_open(MPI_COMM_WORLD, filename, 
		MPI_MODE_CREATE|MPI_MODE_RDWR, MPI_INFO_NULL, &fh));
    CHECK( MPI_File_set_view(fh, rank*sizeof(int), 
	    MPI_BYTE, type, "native", MPI_INFO_NULL)); 

    MPI_Type_size(type, &tsize);

    canary=rank+1000000;

    /* skip over first instance of type */
    if (do_coll) {
	CHECK( MPI_File_write_at_all(fh, tsize, &canary, 1, MPI_INT, &status));
    } else {
	CHECK( MPI_File_write_at(fh, tsize, &canary, 1, MPI_INT, &status));
    }

    CHECK( MPI_File_set_view(fh, 0, MPI_INT, MPI_INT, "native", 
		MPI_INFO_NULL)); 

    if (do_coll) {
	CHECK( MPI_File_read_at_all(fh, expected_extent/sizeof(int)+rank, 
		&compare, 1, MPI_INT, &status));
    } else {
	CHECK( MPI_File_read_at(fh, expected_extent/sizeof(int)+rank, 
		&compare, 1, MPI_INT, &status));
    }

    if (compare != canary)
	errs=1;
    MPI_Allreduce(&errs, &toterrs, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    MPI_File_close(&fh);

    if (toterrs) {
	printf("%d: got %d expected %d\n", rank, compare, canary);
	/* keep file if there's an error */
    } else {
	if (rank == 0) MPI_File_delete(filename, MPI_INFO_NULL);
    }
	
    return (toterrs);

}

static int testtype(char *filename, MPI_Datatype type, int64_t expected_extent)
{
    int rank, ret, errs=0;
    int collective=1, nocollective=0;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (!rank) typestats(type);

    ret = verify_type(filename, type, expected_extent, nocollective); 
    if (ret) {
	errs++;
	fprintf(stderr, "type %d failed indep\n", type);
    } else 
	if (!rank) printf("indep: OK ");

    ret = verify_type(filename, type, expected_extent, collective); 
    if (ret) {
	errs++;
	fprintf(stderr, "type %d failed collective\n", type);
    } else
	if (!rank) printf("coll: OK\n");

    return errs;
}

int main(int argc, char **argv)
{
    int count=2;
    int blocks[2];
    int disps[2];

    int ndims=2;
    int sizes[2];
    int subs[2];
    int starts[2];

    MPI_Datatype baseindex, indexed1G, indexed3G, indexed6G; 
    MPI_Datatype subarray1G, subarray3G, subarray6G;
    int ret, rank;

    MPI_Init(&argc, &argv);

    if (argc != 2) {
	fprintf(stderr, "usage: %s <filename>\n", argv[0]);
	MPI_Abort(MPI_COMM_WORLD, 1);
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* base type: 1MB indexed type of ints*/
    count = 2;
    blocks[0] = 1;
    disps[0] = 0;
    blocks[1] = 1;
    disps[1] = 1024*256-1;

    MPI_Type_indexed(count, blocks, disps, MPI_INT, &baseindex);
    /* simple case: 1GB extent */
    MPI_Type_contiguous(1024, baseindex, &indexed1G);
    MPI_Type_commit(&indexed1G);

    /* a little trickier: 3Gb extent */
    MPI_Type_contiguous(3072, baseindex, &indexed3G);
    MPI_Type_commit(&indexed3G);

    /* and finally 6GB extent */
    MPI_Type_contiguous(6144, baseindex, &indexed6G);
    MPI_Type_commit(&indexed6G);

    /* TODO: 
     * - add a darray test
     * - add a test with crazy extents */
    sizes[0] = 1024*16;
    sizes[1] = 1024*16;
    subs[0] = subs[1] = 256;
    starts[0] = starts[1] = 0;

    MPI_Type_create_subarray(ndims, sizes, subs, starts, 
	    MPI_ORDER_C, MPI_INT, &subarray1G);
    MPI_Type_commit(&subarray1G);

    sizes[1] = 1024*16*3;
    MPI_Type_create_subarray(ndims, sizes, subs, starts, 
	    MPI_ORDER_C, MPI_INT, &subarray3G);
    MPI_Type_commit(&subarray3G);

    sizes[1] = 1024*16*6;
    MPI_Type_create_subarray(ndims, sizes, subs, starts, 
	    MPI_ORDER_C, MPI_INT, &subarray6G);
    MPI_Type_commit(&subarray6G);

    /* assume command line arguments make it out to all processes */
    ret = testtype(argv[1], indexed1G, (int64_t)1024*1024*1024);

    ret = testtype(argv[1], indexed3G, (int64_t)1024*1024*1024*3);

    ret = testtype(argv[1], indexed6G, (int64_t)1024*1024*1024*6);

    ret = testtype(argv[1], subarray1G, (int64_t)1024*1024*1024);

    ret = testtype(argv[1], subarray3G, (int64_t)1024*1024*1024*3);

    ret = testtype(argv[1], subarray6G, (int64_t)1024*1024*1024*6);

    if(!ret && !rank) fprintf(stderr, "  No Errors\n");
    
    MPI_Finalize();
    return (-ret);

}
/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
