/*
 * Test the connectivity between all processes
 */

import mpi.*;
import java.nio.IntBuffer;

class Connectivity {
    public static void main(String args[]) throws MPIException {
         MPI.Init(args);
 
         /*
          * MPI.COMM_WORLD is the communicator provided when MPI is
          * initialized. It contains all the processes that are created
          * upon program execution.
          */
         int myRank = MPI.COMM_WORLD.getRank();
         int numProcesses = MPI.COMM_WORLD.getSize();
         boolean verbose = false;
         String processorName = MPI.getProcessorName();
 
         for (String arg : args) {
             if (arg.equals("-v") || arg.equals("--verbose")) {
                 verbose = true;
                 break;
             }
         }
 
         for (int i = 0; i < numProcesses; i++) {
             /* Find current process */
             if (myRank == i) {
                 /* send to and receive from all higher ranked processes */
                 for (int j = i + 1; j < numProcesses; j++) {
                     if (verbose)
                         System.out.printf("Checking connection between rank %d on %s and rank %d\n", i, processorName,
                                j);
 
                     /*
                      * rank is the Buffer passed into sendRecv to send to rank j.
                      * rank is populated with myRank, which is the data to send off
                      * peer is the Buffer received from rank j to current rank
                      */
                     IntBuffer rank = MPI.newIntBuffer(1);
                     IntBuffer peer = MPI.newIntBuffer(1);
                     rank.put(0, myRank);
 
                     /*
                      * To avoid deadlocks, use combined sendRecv operation.
                      * This performs a send and recv as a combined atomic operation
                      * and allow MPI to efficiently handle the requests internally.
                      */
                     MPI.COMM_WORLD.sendRecv(rank, 1, MPI.INT, j, myRank, peer, 1, MPI.INT, j, j);
                 }
             } else if (myRank > i) {
                 IntBuffer rank = MPI.newIntBuffer(1);
                 IntBuffer peer = MPI.newIntBuffer(1);
                 rank.put(0, myRank);
 
                 /* receive from and reply to rank i */
                 MPI.COMM_WORLD.sendRecv(rank, 1, MPI.INT, i, myRank, peer, 1, MPI.INT, i, i);
             }
         }
 
         /* Wait for all processes to reach barrier before proceeding */
         MPI.COMM_WORLD.barrier();
 
         /*
          * Once all ranks have reached the barrier,
          * have only one process print out the confirmation message.
          * In this case, we are having the "master" process print the message.
          */
         if (myRank == 0) {
             System.out.printf("Connectivity test on %d processes PASSED.\n", numProcesses);
         }
 
         MPI.Finalize();
     }
}