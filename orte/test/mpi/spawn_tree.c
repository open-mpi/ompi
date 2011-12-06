#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <mpi.h>

int main(int argc, char ** argv){

    int i;
    int rank, size, child_rank;
    char nomehost[20];
    MPI_Comm parent, intercomm1, intercomm2;
    int erro;
    int level, curr_level;


    if (argc < 2) {
        fprintf(stderr, "Usage: spawn_tree <#levels>\n");
        exit(1);
    }
    level = atoi(argv[1]);

    MPI_Init(&argc, &argv);


    MPI_Comm_get_parent(&parent);

    if(parent == MPI_COMM_NULL){
        rank=0;
    }
    else{
        MPI_Recv(&rank, 1, MPI_INT, 0, 0, parent, MPI_STATUS_IGNORE);
    }

    curr_level = (int) log2(rank+1);

    printf(" --> rank: %d and curr_level: %d\n", rank, curr_level);

    // Node propagation
    if(curr_level < level){
        // 2^(curr_level+1) - 1 + 2*(rank - 2^curr_level - 1) = 2*rank + 1
        child_rank = 2*rank + 1;
        printf("(%d) Before create rank %d\n", rank, child_rank);
        MPI_Comm_spawn(argv[0], &argv[1], 1, MPI_INFO_NULL, 0,
                       MPI_COMM_SELF, &intercomm1, &erro);
        printf("(%d) After create rank %d\n", rank, child_rank);
           
        MPI_Send(&child_rank, 1, MPI_INT, 0, 0, intercomm1);
           
        //sleep(1);
           
        child_rank = child_rank + 1;
        printf("(%d) Before create rank %d\n", rank, child_rank);
        MPI_Comm_spawn(argv[0], &argv[1], 1, MPI_INFO_NULL, 0,
                       MPI_COMM_SELF, &intercomm2, &erro);
        printf("(%d) After create rank %d\n", rank, child_rank);

        MPI_Send(&child_rank, 1, MPI_INT, 0, 0, intercomm2);

    }

    gethostname(nomehost, 20);
    printf("(%d) in %s\n", rank, nomehost);

    MPI_Finalize();
    return(0);

}
