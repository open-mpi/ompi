/* Wei-keng Liao (wkliao@ece.northwestern.edu) September 8, 2008 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#define YLEN 5
#define XLEN 10
#define SUB_XLEN 3

/* rjl: I was just too lazy to compute this at run-time */
char compare_buf[XLEN*4][YLEN*4] = {
	{'0','1','2',0,0,'3','4','5',0,0,'D','E','F',0,0,'G','H','I'},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{'6','7','8',0,0,'9',':',';',0,0,'J','K','L',0,0,'M','N','O'},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{'X','Y','Z',0,0,'[','\\',']',0,0,'l','m','n',0,0,'o','p','q'},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{'^','_','`',0,0,'a','b','c',0,0,'r','s','t',0,0,'u','v','w'},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{'0','1','2',0,0,'3','4','5',0,0,'D','E','F',0,0,'G','H','I'},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{'6','7','8',0,0,'9',':',';',0,0,'J','K','L',0,0,'M','N','O'},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{'X','Y','Z',0,0,'[','\\',']',0,0,'l','m','n',0,0,'o','p','q'},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{'^','_','`',0,0,'a','b','c',0,0,'r','s','t',0,0,'u','v','w'},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
};


/* set this if you want a dump of the global array 
#define VERBOSE 1
*/

/*----< main() >------------------------------------------------------------*/
int main(int argc, char **argv) {
    int          i, j, err, rank, np, num_io;
    char        *buf, *filename;
    int          rank_dim[2], array_of_sizes[2];
    int          array_of_subsizes[2];
    int          count, *blocklengths, global_array_size, ftype_size;
    MPI_Aint    *displacements;
    MPI_File     fh;
    MPI_Datatype ftype;
    MPI_Status   status;
    MPI_Offset   offset=0;
    int          nr_errors=0;
#ifdef VERBOSE
    int k;
#endif

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &np);

    if (np != 4) {
        if (!rank) printf("Please run with 4 processes. Exiting ...\n\n");
        MPI_Finalize();
        return 1;
    }

    filename = argv[1];

    num_io = 2;

    /*-----------------------------------------------------------------------*/
    /* process rank in each dimension */
    rank_dim[0] = rank /  2;
    rank_dim[1] = rank %  2;

    /* global 2D array size */
    array_of_sizes[0] = YLEN * 2;
    array_of_sizes[1] = XLEN * 2;

    global_array_size = array_of_sizes[0] * array_of_sizes[1];

    array_of_subsizes[0] = YLEN / 2;
    array_of_subsizes[1] = XLEN * SUB_XLEN / 5;

    offset = rank_dim[0] * YLEN * array_of_sizes[1] +
             rank_dim[1] * XLEN;

    /* define data type for file view */
    count = array_of_subsizes[0] * 2;  /* 2 is the no. blocks along X */
    blocklengths  = (int*)     malloc(count*sizeof(int));
    displacements = (MPI_Aint*)malloc(count*sizeof(MPI_Aint));
    for (i=0; i<count; i++)
        blocklengths[i] = array_of_subsizes[1] / 2;
    for (i=0; i<array_of_subsizes[0]; i++)
        for (j=0; j<2; j++)
            displacements[i*2+j] = offset + i*2*array_of_sizes[1] + j * XLEN/2;
    MPI_Type_create_hindexed(count, blocklengths, displacements, MPI_CHAR, &ftype);
    MPI_Type_commit(&ftype);
    MPI_Type_size(ftype, &ftype_size);

/* subarray's layout in the global array

   P0's 's layout                               P1's layout
   [ 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9] | [ 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9]
[ 0] 0 1 2     3 4 5                          |                       D E F     G H I    
[ 1]                                          |                                          
[ 2] 6 7 8     9 : ;                          |                       J K L     M N O    
[ 3]                                          |                                          
[ 4]                                          |                                          
[ 5]                                          |                                          
[ 6]                                          |                                          
[ 7]                                          |                                          
[ 8]                                          |                                          
[ 9]                                          |                                          

   P2's 's layout                               P3's layout
   [ 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9] | [ 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9]
[ 0]                                          |                                          
[ 1]                                          |                                          
[ 2]                                          |                                          
[ 3]                                          |                                          
[ 4]                                          |                                          
[ 5] X Y Z     [ \ ]                          |                       l m n     o p q    
[ 6]                                          |                                          
[ 7] ^ _ `     a b c                          |                       r s t     u v w    
[ 8]                                          |                                          
[ 9]                                          |                                          
*/

    /* initialize the write buffer */
    buf = (char*) malloc(array_of_subsizes[0]*array_of_subsizes[1]);
    for (i=0; i<array_of_subsizes[0]*array_of_subsizes[1]; i++)
        buf[i] = '0' + rank*20  + i%79;

    /* zero file contents ---------------------------------------------------*/
    if (rank == 0) {
        char *wr_buf = (char*) calloc(num_io*global_array_size,1);
        MPI_File_open(MPI_COMM_SELF, filename,
                      MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
        MPI_File_write(fh, wr_buf, num_io*global_array_size, MPI_CHAR, &status);
        MPI_File_close(&fh);
        free(wr_buf);
    }
    /* open the file --------------------------------------------------------*/
    err = MPI_File_open(MPI_COMM_WORLD, filename,
                        MPI_MODE_CREATE | MPI_MODE_WRONLY, MPI_INFO_NULL, &fh);
    if (err != MPI_SUCCESS) {
        printf("Error: MPI_File_open() filename %s\n",filename);
	MPI_Abort(MPI_COMM_WORLD, -1);
	exit(1);
    }

    /* MPI collective write */
    for (i=0; i<num_io; i++) {
        offset = i * global_array_size;
        /* set the file view */
        MPI_File_set_view(fh, offset, MPI_BYTE, ftype, "native", MPI_INFO_NULL);
        MPI_File_write_all(fh, buf, ftype_size, MPI_CHAR, &status);
    }
    MPI_File_close(&fh);

    /* read and print file contents -----------------------------------------*/
    if (rank == 0) {
        char *ptr;
        char *rd_buf = (char*) calloc(num_io*global_array_size,1);
        MPI_File_open(MPI_COMM_SELF, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, &fh);
        MPI_File_read(fh, rd_buf, num_io*global_array_size, MPI_CHAR, &status);
        MPI_File_close(&fh);

#ifdef VERBOSE
        printf("-------------------------------------------------------\n");
        printf("   [");
        for (i=0; i<2; i++) {
            for (j=0; j<XLEN; j++) printf(" %d",j);
            printf(" ");
        }
        printf("]\n\n");
	

        ptr = rd_buf;
        for (k=0; k<num_io; k++) {
            for (i=0; i<2*YLEN; i++) {
                printf("[%2d]",k*2*YLEN+i);
                for (j=0; j<2*XLEN; j++) {
                    if (j>0 && j%XLEN==0) printf(" ");
                    if (*ptr != 0)
                        printf(" %c",*ptr);
                    else
                        printf("  ");
                    ptr++;
                }
                printf("\n");
            }
            printf("\n");
        }
#endif
	ptr = rd_buf;
        for(i=0; i<2*YLEN*num_io; i++) {
	    for(j=0; j<2*XLEN; j++) {
		if( *ptr != compare_buf[i][j]) {
			fprintf(stderr, "expected %d got %d at [%d][%d]\n", 
					*ptr, compare_buf[i][j], i, j);
			nr_errors++;
		}
		ptr++;
	    }
	}
        free(rd_buf);

        if (nr_errors == 0) 
	    fprintf(stdout, " No Errors\n");
        else
	    fprintf(stderr, "Found %d errors\n", nr_errors);
    }

    free(blocklengths);
    free(displacements);
    free(buf);
    MPI_Type_free(&ftype);
    MPI_Finalize();
    return 0;
}

/* command-line outputs are: (the global array is written twice)

% mpiexec -n 4 wkl_subarray
-------------------------------------------------------
   [ 0 1 2 3 4 5 6 7 8 9  0 1 2 3 4 5 6 7 8 9 ]

[ 0] 0 1 2     3 4 5      D E F     G H I    
[ 1]                                         
[ 2] 6 7 8     9 : ;      J K L     M N O    
[ 3]                                         
[ 4]                                         
[ 5] X Y Z     [ \ ]      l m n     o p q    
[ 6]                                         
[ 7] ^ _ `     a b c      r s t     u v w    
[ 8]                                         
[ 9]                                         

[10] 0 1 2     3 4 5      D E F     G H I    
[11]                                         
[12] 6 7 8     9 : ;      J K L     M N O    
[13]                                         
[14]                                         
[15] X Y Z     [ \ ]      l m n     o p q    
[16]                                         
[17] ^ _ `     a b c      r s t     u v w    
[18]                                         
[19]                                         

*/

