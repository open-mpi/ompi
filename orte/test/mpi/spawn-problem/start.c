#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <mpi.h>

int main (int argc, char **argv){
	char buff[30];
	MPI_Status st;
	MPI_Comm comm;
	int errcodes[1];

	MPI_Init(&argc, &argv);
	int level = 0;
	printf("level %d\n", level);
	
	sprintf(argv[0], "%d", level+1);
	MPI_Comm_spawn("ch_rec", argv, 1, MPI_INFO_NULL, 0, MPI_COMM_SELF, 
									&comm, errcodes);
	sprintf(buff,"level %d (pid:%d)", level, getpid());
	MPI_Send(&buff, sizeof(char)*30, MPI_CHAR, 0, 100, comm);
	
	MPI_Recv(&buff, sizeof(char)*30, MPI_CHAR, MPI_ANY_SOURCE, 
						MPI_ANY_TAG, comm, &st);
	printf("Child sent: %s\n", buff);

	MPI_Finalize();
	return 0;
}
