/* -*- Mode: C; c-basic-offset:4 ; -*- */
#include "mpi.h"
#include "mpio.h"  /* not necessary with MPICH 1.1.1 or HPMPI 1.4 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* tests noncontiguous reads/writes using independent I/O */

#define SIZE 5000

int main(int argc, char **argv)
{
    int *buf, i, mynod, nprocs, len, b[3];
    MPI_Aint d[3];
    MPI_File fh;
    MPI_Status status;
    char *filename;
    MPI_Datatype typevec, newtype, t[3];
    MPI_Info info;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &mynod);

    if (nprocs != 2) {
        fprintf(stderr, "Run this program on two processes\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

/* process 0 takes the file name as a command-line argument and 
   broadcasts it to other processes (length first, then string) */
    if (!mynod) {
	i = 1;
	while ((i < argc) && strcmp("-fname", *argv)) {
	    i++;
	    argv++;
	}
	if (i >= argc) {
	    fprintf(stderr, "\n*#  Usage: noncontig -fname filename\n\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
	argv++;
	len = strlen(*argv);
	filename = (char *) malloc(len+1);
	strcpy(filename, *argv);
	MPI_Bcast(&len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(filename, len+1, MPI_CHAR, 0, MPI_COMM_WORLD);
    }
    else {
	MPI_Bcast(&len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	filename = (char *) malloc(len+1);
	MPI_Bcast(filename, len+1, MPI_CHAR, 0, MPI_COMM_WORLD);
    }

    buf = (int *) malloc(SIZE*sizeof(int));

    MPI_Type_vector(SIZE/2, 1, 2, MPI_INT, &typevec);

    /* create a struct type with explicitly set LB and UB; displacements
     * of typevec are such that the types for the two processes won't
     * overlap.
     */
    b[0] = b[1] = b[2] = 1;
    d[0] = 0;
    d[1] = mynod*sizeof(int);
    d[2] = SIZE*sizeof(int);
    t[0] = MPI_LB;
    t[1] = typevec;
    t[2] = MPI_UB;

    /* keep the struct, ditch the vector */
    MPI_Type_struct(3, b, d, t, &newtype);
    MPI_Type_commit(&newtype);
    MPI_Type_free(&typevec);

    MPI_Info_create(&info);
    /* I am setting these info values for testing purposes only. It is
       better to use the default values in practice. */
    MPI_Info_set(info, "ind_rd_buffer_size", "1209");
    MPI_Info_set(info, "ind_wr_buffer_size", "1107");

    if (!mynod) {
	fprintf(stderr, "\ntesting noncontiguous in memory, noncontiguous in file using independent I/O\n");
	MPI_File_delete(filename, MPI_INFO_NULL);
    }
    MPI_Barrier(MPI_COMM_WORLD);

    MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR,
                  info, &fh);

    /* set the file view for each process -- now writes go into the non-
     * overlapping but interleaved region defined by the struct type up above
     */
    MPI_File_set_view(fh, 0, MPI_INT, newtype, "native", info);

    /* fill our buffer with a pattern and write, using our type again */
    for (i=0; i<SIZE; i++) buf[i] = i + mynod*SIZE;
    MPI_File_write(fh, buf, 1, newtype, &status);

    MPI_Barrier(MPI_COMM_WORLD);

    /* fill the entire buffer with -1's.  read back with type.
     * note that the result of this read should be that every other value
     * in the buffer is still -1, as defined by our type.
     */
    for (i=0; i<SIZE; i++) buf[i] = -1;
    MPI_File_read_at(fh, 0, buf, 1, newtype, &status);

    /* check that all the values read are correct and also that we didn't
     * overwrite any of the -1 values that we shouldn't have.
     */
    for (i=0; i<SIZE; i++) {
	if (!mynod) {
	    if ((i%2) && (buf[i] != -1))
		fprintf(stderr, "Process %d: buf %d is %d, should be -1\n", mynod, i, buf[i]);
	    if (!(i%2) && (buf[i] != i))
		fprintf(stderr, "Process %d: buf %d is %d, should be %d\n", mynod, i, buf[i], i);
	}
	else {
	    if ((i%2) && (buf[i] != i + mynod*SIZE))
		fprintf(stderr, "Process %d: buf %d is %d, should be %d\n", mynod, i, buf[i], i + mynod*SIZE);
	    if (!(i%2) && (buf[i] != -1))
		fprintf(stderr, "Process %d: buf %d is %d, should be -1\n", mynod, i, buf[i]);
	}
    }

    MPI_File_close(&fh);

    MPI_Barrier(MPI_COMM_WORLD);

    if (!mynod) {
	fprintf(stderr, "\ntesting noncontiguous in memory, contiguous in file using independent I/O\n");
	MPI_File_delete(filename, MPI_INFO_NULL);
    }
    MPI_Barrier(MPI_COMM_WORLD);

    MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR,
                  info, &fh);

    /* in this case we write to either the first half or the second half
     * of the file space, so the regions are not interleaved.  this is done
     * by leaving the file view at its default.
     */
    for (i=0; i<SIZE; i++) buf[i] = i + mynod*SIZE;
    MPI_File_write_at(fh, mynod*(SIZE/2)*sizeof(int), buf, 1, newtype, &status);

    MPI_Barrier(MPI_COMM_WORLD);

    /* same as before; fill buffer with -1's and then read; every other
     * value should still be -1 after the read
     */
    for (i=0; i<SIZE; i++) buf[i] = -1;
    MPI_File_read_at(fh, mynod*(SIZE/2)*sizeof(int), buf, 1, newtype, &status);

    /* verify that the buffer looks like it should */
    for (i=0; i<SIZE; i++) {
	if (!mynod) {
	    if ((i%2) && (buf[i] != -1))
		fprintf(stderr, "Process %d: buf %d is %d, should be -1\n", mynod, i, buf[i]);
	    if (!(i%2) && (buf[i] != i))
		fprintf(stderr, "Process %d: buf %d is %d, should be %d\n", mynod, i, buf[i], i);
	}
	else {
	    if ((i%2) && (buf[i] != i + mynod*SIZE))
		fprintf(stderr, "Process %d: buf %d is %d, should be %d\n", mynod, i, buf[i], i + mynod*SIZE);
	    if (!(i%2) && (buf[i] != -1))
		fprintf(stderr, "Process %d: buf %d is %d, should be -1\n", mynod, i, buf[i]);
	}
    }

    MPI_File_close(&fh);

    MPI_Barrier(MPI_COMM_WORLD);

    if (!mynod) {
	fprintf(stderr, "\ntesting contiguous in memory, noncontiguous in file using independent I/O\n");
	MPI_File_delete(filename, MPI_INFO_NULL);
    }
    MPI_Barrier(MPI_COMM_WORLD);

    MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR, 
                  info, &fh);

    /* set the file view so that we have interleaved access again */
    MPI_File_set_view(fh, 0, MPI_INT, newtype, "native", info);

    /* this time write a contiguous buffer */
    for (i=0; i<SIZE; i++) buf[i] = i + mynod*SIZE;
    MPI_File_write(fh, buf, SIZE, MPI_INT, &status);

    MPI_Barrier(MPI_COMM_WORLD);

    /* fill buffer with -1's; this time they will all be overwritten */
    for (i=0; i<SIZE; i++) buf[i] = -1;
    MPI_File_read_at(fh, 0, buf, SIZE, MPI_INT, &status);

    for (i=0; i<SIZE; i++) {
	if (!mynod) {
	    if (buf[i] != i)
		fprintf(stderr, "Process %d: buf %d is %d, should be %d\n", mynod, i, buf[i], i);
	}
	else {
	    if (buf[i] != i + mynod*SIZE)
		fprintf(stderr, "Process %d: buf %d is %d, should be %d\n", mynod, i, buf[i], i + mynod*SIZE);
	}
    }

    MPI_File_close(&fh);

    MPI_Type_free(&newtype);
    MPI_Info_free(&info);
    free(buf);
    free(filename);
    MPI_Finalize();
    return 0;
}
