#include "mpi.h"
#include "mpio.h"  /* not necessary with MPICH 1.1.1 or HPMPI 1.4 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* tests various miscellaneous functions. */

int main(int argc, char **argv)
{
    int buf[1024], amode, flag, mynod, len, i;
    MPI_File fh;
    MPI_Status status;
    MPI_Datatype newtype;
    MPI_Offset disp, offset;
    MPI_Group group;
    MPI_Datatype etype, filetype;
    char datarep[25], *filename;

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &mynod);

/* process 0 takes the file name as a command-line argument and 
   broadcasts it to other processes */
    if (!mynod) {
	i = 1;
	while ((i < argc) && strcmp("-fname", *argv)) {
	    i++;
	    argv++;
	}
	if (i >= argc) {
	    fprintf(stderr, "\n*#  Usage: misc -fname filename\n\n");
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


    MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR,
                  MPI_INFO_NULL, &fh);

    MPI_File_write(fh, buf, 1024, MPI_INT, &status);

    MPI_File_sync(fh);

    MPI_File_get_amode(fh, &amode);
    if (!mynod) fprintf(stderr, "testing MPI_File_get_amode\n");
    if (amode != (MPI_MODE_CREATE | MPI_MODE_RDWR))
	fprintf(stderr, "amode is %d, should be %d\n\n", amode, MPI_MODE_CREATE |
                      MPI_MODE_RDWR);

    MPI_File_get_atomicity(fh, &flag);
    if (flag) fprintf(stderr, "atomicity is %d, should be 0\n", flag);
    if (!mynod) fprintf(stderr, "setting atomic mode\n");
    MPI_File_set_atomicity(fh, 1);
    MPI_File_get_atomicity(fh, &flag);
    if (!flag) fprintf(stderr, "atomicity is %d, should be 1\n", flag);
    MPI_File_set_atomicity(fh, 0);
    if (!mynod) fprintf(stderr, "reverting back to nonatomic mode\n");

    MPI_Type_vector(10, 10, 20, MPI_INT, &newtype);
    MPI_Type_commit(&newtype);

    MPI_File_set_view(fh, 1000, MPI_INT, newtype, "native", MPI_INFO_NULL);
    if (!mynod) fprintf(stderr, "testing MPI_File_get_view\n");
    MPI_File_get_view(fh, &disp, &etype, &filetype, datarep);
    if ((disp != 1000) || strcmp(datarep, "native"))
	fprintf(stderr, "disp = @LL@, datarep = %s, should be 1000, native\n\n", disp, datarep);

    if (!mynod) fprintf(stderr, "testing MPI_File_get_byte_offset\n");
    MPI_File_get_byte_offset(fh, 10, &disp);
    if (disp != (1000+20*sizeof(int))) fprintf(stderr, "byte offset = @LL@, should be %d\n\n", disp, (int) (1000+20*sizeof(int)));

    MPI_File_get_group(fh, &group);

    if (!mynod) fprintf(stderr, "testing MPI_File_set_size\n");
    MPI_File_set_size(fh, 1000+15*sizeof(int));
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_File_sync(fh);
    MPI_File_get_size(fh, &disp);
    if (disp != 1000+15*sizeof(int)) fprintf(stderr, "file size = @LL@, should be %d\n\n", disp, (int) (1000+15*sizeof(int)));
 
    if (!mynod) fprintf(stderr, "seeking to eof and testing MPI_File_get_position\n");
    MPI_File_seek(fh, 0, MPI_SEEK_END);
    MPI_File_get_position(fh, &disp);
    if (disp != 10) fprintf(stderr, "file pointer posn = @LL@, should be 10\n\n", disp);

    if (!mynod) fprintf(stderr, "testing MPI_File_get_byte_offset\n");
    MPI_File_get_byte_offset(fh, disp, &offset);
    if (offset != (1000+20*sizeof(int))) fprintf(stderr, "byte offset = @LL@, should be %d\n\n", offset, (int) (1000+20*sizeof(int)));
    MPI_Barrier(MPI_COMM_WORLD);

    if (!mynod) fprintf(stderr, "testing MPI_File_seek with MPI_SEEK_CUR\n");
    MPI_File_seek(fh, -10, MPI_SEEK_CUR);
    MPI_File_get_position(fh, &disp);
    MPI_File_get_byte_offset(fh, disp, &offset);
    if (offset != 1000)
	fprintf(stderr, "file pointer posn in bytes = @LL@, should be 1000\n\n", offset);

    if (!mynod) fprintf(stderr, "preallocating disk space up to 8192 bytes\n");
    MPI_File_preallocate(fh, 8192);

    if (!mynod) fprintf(stderr, "closing the file and deleting it\n");
    MPI_File_close(&fh);
    
    MPI_Barrier(MPI_COMM_WORLD);
    if (!mynod) MPI_File_delete(filename, MPI_INFO_NULL);

    MPI_Type_free(&newtype);
    MPI_Type_free(&filetype);
    MPI_Group_free(&group);
    free(filename);
    MPI_Finalize(); 
    return 0;
}
