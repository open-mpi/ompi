/*******************************************************************************************
 *                                                                                         *
 * This program is designed to stress MPI and the network underneath it.                   *
 * It repeatedly does                                                                      *
 *                                                                                         *           
 *    1) a bunch of collective operations (reduce, gather etc)                             *
 *    2) a bunch of sends of randomly chosen length data arrays to                         *
 *       a) nearby pes                                                                     *
 *       b) randomly chosen pes throughout the whole set of available pes                  *
 *                                                                                         *
 * How this works:                                                                         *
 *                                                                                         *
 *   We first define our 'nearby' pes, as those with pe number within +-maxneigh_near      *
 *   of our own pe number. Then a random number (max of maxneigh_rand) 'randomly chosen'   *
 *   pes are selected from the set of all pes. Each of these selection processes           *
 *   proceeds independently on all pes, so at this stage one neighbor pe might not be      *
 *   a 'mutual neighbor' pe. At the same time as the pe selection happens, we define       *
 *   the quantity of data that we plan to send to our neighbor pe.                         *
 *                                                                                         *
 *   After we have all of this information, we do some reductions across all processors    *
 *   to determine max/min/average number of neighbor pes that any given pe has.            *
 *   Mostly this is just an exercise of the network infrastructure, but is nice in that    *
 *   it also prints out a bit of information.                                              *
 *                                                                                         *
 *   Next, we need to do discovery of all pairs of mpi pes, in order to know who should    *
 *   send how much data to whom, so that relevant send/receive calls can be made.          *
 *   This means doing an Allgather of the neighbors that each pe thinks it has, and a      *
 *   search of those neighbors by all pes, to determine if it is one of them. If so,       *
 *   we save the identity of the pe and the length of the array it expects to send to us,  *
 *   both so that we can set up a receive call, and so that we can send the same amount of *
 *   data back. Each pair of processors sends and recieves an equal volume of data between *
 *   them.                                                                                 *
 *   Note reciprocal data sends are not yet implemented.                                   *
 *                                                                                         *
 *   Finally, we do all the send and recieve operations between all pes, checking the      *
 *   integrity of the data recieved.                                                       *
 *                                                                                         *
 *   Then we start over and do it all again, with a different set of randomly selected     *
 *   neighbors pes and data lengths.                                                       *
 *                                                                                         *
 *                                                                                         *
 * How to run this test:                                                                   *
 *                                                                                         *
 * mpirun -n 64 netstress                                                              *
 *                                                                                         *
 *  64 pes reproduces immediately. 32 reproduces immediately. 16 can sometimes             *
 *  run many iterations but seems to reproduce after a minute or two.                      *
 *                                                                                         *
 *******************************************************************************************/
#include "mpi.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAXPE 4095
#define MAXPE2  16769025
#define MAXLENGTH 500000
#define MAXNEIGH_RAND 40
#define MAXNEIGH_NEAR 10
#define MAXNEIGH (MAXNEIGH_RAND + MAXNEIGH_NEAR)
#define MAXBIG 4146

/* global variables */
char string[120];
int mype, numpe;
int tag;
MPI_Request send_req[MAXPE], recv_req[MAXPE];
MPI_Status send_status[MAXPE], recv_status[MAXPE];
int myrecvneigh[MAXLENGTH], mysendneigh[MAXLENGTH];
int nrecvneigh, nsendneigh;
int len_recv[MAXBIG], len_send[MAXBIG];
int intarr2recv[MAXLENGTH], intarr2send[MAXLENGTH];
double array2recv[MAXLENGTH], array2send[MAXLENGTH];

int nsendneigh_all[MAXPE], allsendneigh[MAXPE2],len_send_all[MAXPE2];

int nayoffset[MAXPE];


char charrseed[8];
int rseed[2];
double harvest;

int numneigh_rand;
int lenmin, lenmax;
int maxnay, minnay, totnay;
int nsendneigh, nrecvneigh;
int rank;
int mpierror;

static void getnaypes(void);
static void exchange_nayinfo(void);

int main(int argc, char **argv)
{
    int i, ii;
    /*
     * Initialize MPI and the basic state of the executable
     */
    MPI_Init(&argc, &argv);
    
    MPI_Comm_size(MPI_COMM_WORLD, &numpe);
    MPI_Comm_rank(MPI_COMM_WORLD, &mype);
    
    for (i=0; i < MAXLENGTH; i++) {
        array2send[i] = (double)mype;
        intarr2send[i] = mype;
    }    
    
    srandom(time(0));

    /*********************************************
     *                                           *
     * Start loop                                *
     *                                           *
     *********************************************/
    while (1) {
        
        if (mype == 0) {
            printf("starting another iteration\n");
        }
        
        for (i=0; i < MAXLENGTH; i++) {
            array2recv[i] = -1.0;
        }
        
        getnaypes();
        
        exchange_nayinfo();
        
        /*
         * Now start the process of sending and receiving data from our neighbors
         */
        
        for (i=0; i < nrecvneigh; i++) {
            tag = len_recv[i];
            MPI_Irecv(array2recv,len_recv[i],MPI_DOUBLE,myrecvneigh[i],tag,MPI_COMM_WORLD,&recv_req[i]);
        }
        
        for (i=0; i < nrecvneigh; i++) {
            ii=i+nrecvneigh;
            tag = MAXLENGTH + len_recv[i];
            MPI_Irecv(intarr2recv,len_recv[i],MPI_INT,myrecvneigh[i],tag,MPI_COMM_WORLD,&recv_req[ii]);
        }
        
        for (i=0; i < nsendneigh; i++) {
            tag = len_send[i];
            MPI_Isend(array2send,len_send[i],MPI_DOUBLE,mysendneigh[i],tag,MPI_COMM_WORLD,&send_req[i]);
        }
        
        for (i=0; i < nsendneigh; i++) {
            ii=i+nsendneigh;
            tag = MAXLENGTH + len_send[i];
            MPI_Isend(intarr2send,len_send[i],MPI_INT,mysendneigh[i],tag,MPI_COMM_WORLD,&send_req[ii]);
        }
        
        
        MPI_Waitall(2*nrecvneigh,recv_req, recv_status);
        
        MPI_Waitall(2*nsendneigh,send_req, send_status);
        
    }
    
    
    MPI_Finalize();
    
}

static void getnaypes(void)
{
    int n, minpe, maxrank;
    
    /*
     * Predetermine a number of 'local' neighbors with whom we will exchange data,
     * and define how much data we will send to them.
     */
    nsendneigh=0;
    if ((numpe-1) < (mype+MAXNEIGH_NEAR/2)) {
        minpe = numpe-1;
    } else {
        minpe = mype+MAXNEIGH_NEAR/2;
    }
    if (0 < (rank-MAXNEIGH_NEAR/2)) {
        maxrank = rank-MAXNEIGH_NEAR/2;
    } else {
        maxrank = 0;
    }
    for (n=maxrank; n < minpe; n++) {
        nsendneigh=nsendneigh+1;
        mysendneigh[nsendneigh] = n;
        harvest = (double)random() / (double)RAND_MAX;
        len_send[nsendneigh] = (int)((double)MAXLENGTH*harvest) ;
    }
    
    /*
        * Predetermine a number of 'remote' neighbors with whom we will exchange data,
        * and define how much data we will send to them. Note that this could in
        * principle include some of our local neighbors too.
        */
    harvest = (double)random() / (double)RAND_MAX;
    if (numpe < (int)((double)numpe*harvest)) {
        numneigh_rand = numpe;
    } else {
        numneigh_rand = (int)((double)numpe*harvest);
    }
        
    for (n=0;  n < numneigh_rand; n++) {
        nsendneigh=nsendneigh+1;
        harvest = (double)random() / (double)RAND_MAX;
        if ((numpe-1) < (int)((double)(numpe-1)*harvest)) {
            mysendneigh[nsendneigh] = numpe-1;
        } else {
            mysendneigh[nsendneigh] = (int)((double)(numpe-1)*harvest);
        }
        harvest = (double)random() / (double)RAND_MAX;
        len_send[nsendneigh] = (int)((double)MAXLENGTH*harvest);
    }

#if 0
            /* Do some print outs of the information we just defined. */
    lenmin = minval(len_send(1:nsendneigh));
    lenmax = maxval(len_send(1:nsendneigh));
#endif
    mpierror = MPI_Allreduce(mysendneigh,myrecvneigh,nrecvneigh,MPI_INT,MPI_MAX,MPI_COMM_WORLD);
            
    if(mpierror != MPI_SUCCESS) {
        printf("My pe is: %d and I just failed getnaypes MPI_Allreduce #1 with code: %d\n", mype, mpierror);
               MPI_Abort(MPI_COMM_WORLD,1);
    }
                
    mpierror = MPI_Allreduce(mysendneigh,myrecvneigh,nrecvneigh,MPI_INT,MPI_MIN,MPI_COMM_WORLD);
                
    if(mpierror != MPI_SUCCESS) {
        printf("My pe is: %d and I just failed getnaypes MPI_Allreduce #2 with code: %d\n", mype, mpierror);
        MPI_Abort(MPI_COMM_WORLD,1);
    }
                    
    mpierror = MPI_Allreduce(mysendneigh,myrecvneigh,nrecvneigh,MPI_INT,MPI_SUM,MPI_COMM_WORLD);
                    
    if (mpierror != MPI_SUCCESS) {
        printf("My pe is: %d and I just failed getnaypes MPI_Allreduce #3 with code: %d\n", mype, mpierror);
        MPI_Abort(MPI_COMM_WORLD,1);
    }
                        
    if (mype == 0) {
        printf("\nThe pe with the least send-to neighbors has: %d neighbor pes\n", minnay);
        printf("The pe with the most  send-to neighbors has: %d neighbor pes\n", maxnay);
        printf("There are a total of %d neighbor pes spread across %d MPI ranks\n",totnay, numpe);
        printf("So the average number of send-to neighbor pes is %f\n",(double)(totnay)/(double)(numpe));
    }
                            
}

static void exchange_nayinfo(void)
{
    int i, j, n, nstart;
    
    /*
     * Send around data on who everyone thinks is its neighbor and how much
     * data it should expect to exchange with that neighbor:
     */
    
    if (mype == 0) {
        printf("Now we will determine how many recvfrom pes each pe has, and how much\ndata it should expect to receive from those pes\n");
    }
        
        /*
         * Gather how many neighbors everyone thinks they have to all pes, 
         * so that we can determine where the offset is into each processor's 
         * neighbor list, which we will send next.
         *
         */
    mpierror = MPI_Allgather(&nsendneigh,1,MPI_INT,nsendneigh_all,1,MPI_INT,MPI_COMM_WORLD);
        
    if (mpierror != MPI_SUCCESS) {
        printf("My pe is: %d and I just failed exchange_nayinfo MPI_Allgather #1 with code: %d\n",mype, mpierror);
        MPI_Abort(MPI_COMM_WORLD,1);
    }
        
    nayoffset[0]=0;
    for (i=1; i < numpe; i++) {
        nayoffset[i]=nayoffset[i-1] + nsendneigh_all[i-1];
    }

    /*
        * Now we distribute all of the actual neighbor identities for each
        * processor, to each processor
        */
    mpierror = MPI_Allgatherv(mysendneigh,nsendneigh,MPI_INT,allsendneigh,nsendneigh_all,nayoffset,MPI_INT,MPI_COMM_WORLD);
        
    if (mpierror != MPI_SUCCESS) {
        printf("My pe is: %d and I just failed exchange_nayinfo MPI_Allgatherv #1 with code: %d\n",mype, mpierror);
        MPI_Abort(MPI_COMM_WORLD,1);
    }
        
        /*
         * Finally, send around the amount of data we expect to come from each neighbor pe
        */
    mpierror = MPI_Allgatherv(len_send,nsendneigh,MPI_INT,len_send_all,nsendneigh_all,nayoffset,MPI_INT,MPI_COMM_WORLD);
        
    if (mpierror != MPI_SUCCESS) {
        printf("My pe is: %d and I just failed exchange_nayinfo MPI_Allgatherv #2 with code: %d\n",mype, mpierror);
        MPI_Abort(MPI_COMM_WORLD,1);
    }
        
        
    nstart = 1;
    nrecvneigh=0;
    for (i=0; i < numpe; i++) {
        for (j=nstart; j < nstart+nsendneigh_all[i]-1; j++) {
            if (allsendneigh[j] == mype) {
                nrecvneigh=nrecvneigh+1;
                myrecvneigh[nrecvneigh]=i;
                len_recv[nrecvneigh]=len_send_all[j];
            }
        }
        nstart=nstart + nsendneigh_all[i];
    }
}
