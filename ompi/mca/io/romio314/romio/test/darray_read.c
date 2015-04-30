
#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define NSIDE 5
#define NBLOCK 3
#define NPROC 2

#define CHECK(fn) {int errcode; errcode = (fn); if (errcode != MPI_SUCCESS) handle_error(errcode, #fn);}

static void handle_error(int errcode, const char *str)
{
	char msg[MPI_MAX_ERROR_STRING];
	int resultlen;
	MPI_Error_string(errcode, msg, &resultlen);
	fprintf(stderr, "%s: %s\n", str, msg);
	MPI_Abort(MPI_COMM_WORLD, 1);
}


int main(int argc, char *argv[]) 
{ 
  int i, j, nerrors=0, total_errors=0; 

  int rank, size;
  int bpos;

  MPI_Datatype darray;
  MPI_Status status;
  MPI_File mpi_fh;

  /* Define array distribution
      A 2x2 block size works with ROMIO, a 3x3 block size breaks it. */
  int distrib[2] = { MPI_DISTRIBUTE_CYCLIC, MPI_DISTRIBUTE_CYCLIC };
  int bsize[2] = { NBLOCK, NBLOCK };
  int gsize[2] = { NSIDE, NSIDE };
  int psize[2] = { NPROC, NPROC };

  double data[NSIDE*NSIDE];
  double *ldata, *pdata;

  int tsize, nelem;

  MPI_File dfile;
 
  MPI_Init(&argc, &argv);

  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* Set up type */
  CHECK(MPI_Type_create_darray(size, rank, 2, gsize, distrib,
			 bsize, psize, MPI_ORDER_FORTRAN, MPI_DOUBLE, &darray));
  CHECK(MPI_Type_commit(&darray));
  CHECK(MPI_Type_size(darray, &tsize));
  nelem = tsize / sizeof(double);

  for(i = 0; i < (NSIDE*NSIDE); i++) data[i] = i;

  if (rank == 0) {
    CHECK(MPI_File_open(MPI_COMM_SELF, argv[1],
		MPI_MODE_CREATE|MPI_MODE_WRONLY, MPI_INFO_NULL, &dfile));
    CHECK(MPI_File_write(dfile, data, NSIDE*NSIDE, MPI_DOUBLE, &status));
    CHECK(MPI_File_close(&dfile));
  }
  MPI_Barrier(MPI_COMM_WORLD);

  /* Allocate buffer */
  ldata = (double *)malloc(tsize);
  pdata = (double *)malloc(tsize);

  /* Use Pack to pull out array */
  bpos = 0;
  CHECK(MPI_Pack(data, 1, darray, pdata, tsize, &bpos, MPI_COMM_WORLD));

  MPI_Barrier(MPI_COMM_WORLD);

  /* Read in array from file.  */
  CHECK(MPI_File_open(MPI_COMM_WORLD, argv[1], MPI_MODE_RDONLY, MPI_INFO_NULL, &mpi_fh));
  CHECK(MPI_File_set_view(mpi_fh, 0, MPI_DOUBLE, darray, "native", MPI_INFO_NULL));
  CHECK(MPI_File_read_all(mpi_fh, ldata, nelem, MPI_DOUBLE, &status));
  CHECK(MPI_File_close(&mpi_fh));

  for(i = 0; i < size; i++) {
#ifdef VERBOSE
    MPI_Barrier(MPI_COMM_WORLD);
    if(rank == i) {
      printf("=== Rank %i === (%i elements) \nPacked: ", rank, nelem);
      for(j = 0; j < nelem; j++) {
        printf("%4.1f ", pdata[j]);
        fflush(stdout);
      }
      printf("\nRead:   ");
      for(j = 0; j < nelem; j++) {
        printf("%4.1f ", ldata[j]);
        fflush(stdout);
      }
      printf("\n\n");
      fflush(stdout);
    }
#endif
    if(rank == i) {
	for (j=0; j< nelem; j++) {
	    if (pdata[j] != ldata[j]) {
		fprintf(stderr, "rank %d at index %d: packbuf %4.1f filebuf %4.1f\n",
			rank, j, pdata[j], ldata[j]);
		nerrors++;
	    }
	}
    }
  }
  MPI_Allreduce(&nerrors, &total_errors, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  if (rank == 0 && total_errors == 0)
      printf(" No Errors\n");

  free(ldata);
  free(pdata);
  MPI_Type_free(&darray);
  MPI_Finalize();

  exit(total_errors);

} 
