/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
pml monitoring tester.

Designed by George Bosilca <bosilca@icl.utk.edu> and  Emmanuel Jeannot <emmanuel.jeannot@inria.fr>
Contact the authors for questions.

To be run as:

mpirun -np 4 --mca pml_monitoring_enable 2 ./monitoring_test
pm
Then, the output should be:

flushing to ./prof/phase_1_2.prof
flushing to ./prof/phase_1_0.prof
flushing to ./prof/phase_1_3.prof
flushing to ./prof/phase_2_1.prof
flushing to ./prof/phase_2_3.prof
flushing to ./prof/phase_2_0.prof
flushing to ./prof/phase_2_2.prof
I	0	1	108 bytes	27 msgs sent
E	0	1	1012 bytes	30 msgs sent
E	0	2	23052 bytes	61 msgs sent
I	1	2	104 bytes	26 msgs sent
I	1	3	208 bytes	52 msgs sent
E	1	0	860 bytes	24 msgs sent
E	1	3	2552 bytes	56 msgs sent
I	2	3	104 bytes	26 msgs sent
E	2	0	22804 bytes	49 msgs sent
E	2	3	860 bytes	24 msgs sent
I	3	0	104 bytes	26 msgs sent
I	3	1	204 bytes	51 msgs sent
E	3	1	2304 bytes	44 msgs sent
E	3	2	860 bytes	24 msgs sent

or as

mpirun -np 4 --mca pml_monitoring_enable 1 ./monitoring_test

for an output as:

flushing to ./prof/phase_1_1.prof
flushing to ./prof/phase_1_0.prof
flushing to ./prof/phase_1_2.prof
flushing to ./prof/phase_1_3.prof
flushing to ./prof/phase_2_1.prof
flushing to ./prof/phase_2_3.prof
flushing to ./prof/phase_2_2.prof
flushing to ./prof/phase_2_0.prof
I	0	1	1120 bytes	57 msgs sent
I	0	2	23052 bytes	61 msgs sent
I	1	0	860 bytes	24 msgs sent
I	1	2	104 bytes	26 msgs sent
I	1	3	2760 bytes	108 msgs sent
I	2	0	22804 bytes	49 msgs sent
I	2	3	964 bytes	50 msgs sent
I	3	0	104 bytes	26 msgs sent
I	3	1	2508 bytes	95 msgs sent
I	3	2	860 bytes	24 msgs sent
*/



#include <stdio.h>
#include "mpi.h"

/* opal mca header taken from opal/mca/base/mca_base_var.h
   Required to flush monitoring phases
*/
int mca_base_var_find_by_name (const char *full_name, int *vari);
int mca_base_var_get_value (int vari, const void *value,
			    void *source, /* should be mca_base_var_source_t *source,
					     but we do not need it
					     and we do not know what is mca_base_var_source_t */
			    const char **source_file);


int main(argc, argv)
     int argc;
     char **argv;
{
    int rank, size, n, to, from, tagno;
    MPI_Status status;
    MPI_Comm newcomm;
    MPI_Request request;
    char filename[1024];


    /* first phase : make a token circulated in MPI_COMM_WORLD */
    n = -1;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    to = (rank + 1) % size;
    from = (rank - 1) % size;
    tagno = 201;
    if (rank == 0){
        n=25;
        MPI_Isend(&n,1,MPI_INT,to,tagno,MPI_COMM_WORLD,&request);
    }
    while (1){
        MPI_Irecv(&n,1,MPI_INT,from,tagno,MPI_COMM_WORLD, &request);
        MPI_Wait(&request,&status);
        if (rank == 0) {n--;tagno++;}
        MPI_Isend(&n,1,MPI_INT,to,tagno,MPI_COMM_WORLD, &request);
        if (rank != 0) {n--;tagno++;}
        if (n<0){
            break;
        }
    }


    /* flush the monitoring of the first phase */
    int fctidx;
    void* fct;
    int (*flush_monitoring)(char*) = NULL;
    /*
       Get the function pointer of the flushing function of the monitoring
       This uses  Opal low level interface
    */
    mca_base_var_find_by_name( "pml_monitoring_flush", &fctidx);
    if(fctidx){
        mca_base_var_get_value(fctidx, &fct, NULL, NULL);
        flush_monitoring = *(unsigned long*)fct;
    }
    /* Build one file per processes
       Evevry thing that has been monitored by each
       process since the last flush will be output in filename*/

    /*
       Requires directory prof to be created.
       Filename format should display the phase number
       and the process rank for ease of parsing with
       aggregate_profile.pl script
     */
    sprintf(filename,"./prof/phase_1_%d.prof",rank);
    if(flush_monitoring){
        int r = flush_monitoring(filename);
	if(r == -1){
	  fprintf(stderr, "Process %d cannot save monitoring in %s\n", rank, filename);
	}
    }

    /*
       Second phase. Work with different communicators.
       even ranls will circulate a token
       while odd ranks wil perform a all_to_all
    */
    MPI_Comm_split(MPI_COMM_WORLD,rank%2,rank,&newcomm);

    /* the filename for flushing monitoring now uses 2 as phase number! */
    sprintf(filename,"./prof/phase_2_%d.prof",rank);


    if(rank%2){ /*even ranks (in COMM_WORD) circulate a token*/
        int old_rank=rank;
        MPI_Comm_rank(newcomm,&rank);
        MPI_Comm_size(newcomm,&size);
        if( size > 1 ) {
            to = (rank + 1) % size;;
            from = (rank - 1) % size ;
            tagno = 201;
            if (rank == 0){
                n=50;
                MPI_Send(&n,1,MPI_INT,to,tagno,newcomm);
            }
            while (1){
                MPI_Recv(&n,1,MPI_INT,from,tagno,newcomm, &status);
                if (rank == 0) {n--;tagno++;}
                MPI_Send(&n,1,MPI_INT,to,tagno,newcomm);
                if (rank != 0) {n--;tagno++;}
                if (n<0){
		  if(flush_monitoring){
		    int r = flush_monitoring(filename);
		    if(r == -1){
		      fprintf(stderr, "Process %d cannot save monitoring in %s\n", old_rank, filename);
		    }
		  }
		  break;
                }
            }
        }
    }else{ /*odd ranks (in COMM_WORD) will perform a all_to_all and a barrier*/
        int send_buff[10240];
        int recv_buff[10240];
        MPI_Comm_rank(newcomm,&rank);
        MPI_Comm_size(newcomm,&size);
        MPI_Alltoall(send_buff,10240/size, MPI_INT,recv_buff,10240/size,MPI_INT,newcomm);
        MPI_Comm_split(newcomm,rank%2,rank,&newcomm);
        MPI_Barrier(newcomm);
        if(flush_monitoring){
	  int r = flush_monitoring(filename);
	  if(r == -1){
	    fprintf(stderr, "Process %d cannot save monitoring in %s\n", rank, filename);
	  }
	}
    }

    /* Now, in MPI_Finalize(), the pml_monitoring library outputs, in STDERR, the aggregated recorded monitoring of all the phases*/
    MPI_Finalize();
    return 0;
}
