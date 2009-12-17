#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <mpi.h>

int main (int argc, char **argv){
	char buff[30];
	MPI_Status st;
	MPI_Comm comm[2], parent;
	MPI_Request req[2];
	int errcodes[1];
	int level;
	int x = 6, i, j;

	MPI_Init(&argc, &argv);
	MPI_Comm_get_parent(&parent);
	argv++;
	level = atoi(argv[0]);
	printf("level = %d\n",level);

	MPI_Recv(&buff, sizeof(char)*30, MPI_CHAR, MPI_ANY_SOURCE, 
						MPI_ANY_TAG, parent, &st);
  printf("Parent sent: %s\n", buff);

	if(level < x){
		sprintf(argv[0], "%d", level+1);
		MPI_Comm_spawn("ch_rec", argv, 1, MPI_INFO_NULL, 0, MPI_COMM_SELF, 
										&comm[0], errcodes);
		sprintf(buff,"level %d (pid:%d)", level, getpid());
		MPI_Send(&buff, sizeof(char)*30, MPI_CHAR, 0, 100, comm[0]);
		MPI_Irecv(&buff, sizeof(char)*30, MPI_CHAR, MPI_ANY_SOURCE, 
							MPI_ANY_TAG, comm[0], &req[0]);
	
		//sleep(2);
		sprintf(argv[0], "%d", (level+1));
		MPI_Comm_spawn("ch_rec", argv, 1, MPI_INFO_NULL, 0, MPI_COMM_SELF, 
										&comm[1], errcodes);
		sprintf(buff,"level %d (pid:%d)", level, getpid());
		MPI_Send(&buff, sizeof(char)*30, MPI_CHAR, 0, 100, comm[1]);
		MPI_Irecv(&buff, sizeof(char)*30, MPI_CHAR, MPI_ANY_SOURCE, 
						  MPI_ANY_TAG, comm[1], &req[1]);
	
		for(i=0; i<2; i++){
			MPI_Waitany(2, req, &j, MPI_STATUS_IGNORE);
			printf("Child %d sent: %s\n", j, buff);
		}
	}
	sprintf(buff,"level %d (pid:%d)", level, getpid());
	MPI_Send(&buff, sizeof(char)*30, MPI_CHAR, 0, 100, parent);
	MPI_Finalize();
	return 0;
}
