/*file .c : spawned  the file Exe*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "mpi.h"
#include <pthread.h>
#include <signal.h> 
#include <sys/time.h>
#include <errno.h>
#define     EXE_TEST             "./loop_child"



int main( int argc, char **argv ) {

    long *lpBufferMpi;
    MPI_Comm lIntercom;
    int lErrcode;
    MPI_Comm lCommunicateur;
    int lRangMain,lRangExe,lMessageEnvoi,lIter,lTailleBuffer;
    int *lpMessageEnvoi=&lMessageEnvoi;
    MPI_Status lStatus;             /*status de reception*/

     lIter=0;


    /* MPI environnement */    

    printf("main*******************************\n");
    printf("main : Lancement MPI*\n");

    MPI_Init( &argc, &argv);
    lpBufferMpi = calloc( 10000, sizeof(long));
    MPI_Buffer_attach( (void*)lpBufferMpi, 10000 * sizeof(long) );

    while (lIter<1000){
        lIter ++;
        lIntercom=(MPI_Comm)-1 ;

        MPI_Comm_spawn( EXE_TEST, NULL, 1, MPI_INFO_NULL,
                      0, MPI_COMM_WORLD, &lIntercom, &lErrcode );
        printf( "%i main***MPI_Comm_spawn return : %d\n",lIter, lErrcode );

        if(lIntercom == (MPI_Comm)-1 ){
            printf("%i Intercom null\n",lIter);
            return 0;
        }
        MPI_Intercomm_merge(lIntercom, 0,&lCommunicateur );
        MPI_Comm_rank( lCommunicateur, &lRangMain);
        lRangExe=1-lRangMain;

        printf("%i main***Rang main : %i   Rang exe : %i \n",lIter,(int)lRangMain,(int)lRangExe);
//        sleep(2);

    }


    /* Arret de l'environnement MPI */
    lTailleBuffer=10000* sizeof(long);
    MPI_Buffer_detach( (void*)lpBufferMpi, &lTailleBuffer );
    MPI_Comm_free( &lCommunicateur );
    MPI_Finalize( );
    free( lpBufferMpi );

    printf( "Main = End .\n" );
    return 0;

}
