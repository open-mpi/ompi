#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>     /* pour sleep() */
#include <pthread.h>
#include <semaphore.h>
#include "mpi.h"

int main( int argc, char **argv ) {
/*1)pour communiaction MPI*/
    MPI_Comm lCommunicateur;        /*communicateur du process*/
    MPI_Comm CommParent;            /*Communiacteur parent àépér*/
    int lRank;                      /*rang du communicateur du process*/
    int lRangMain;            /*rang du séenceur si lancén mode normal*/
    int lTailleCommunicateur;       /*taille du communicateur;*/
    long *lpBufferMpi;              /*buffer pour message*/
    int lBufferSize;                /*taille du buffer*/
   
 
    lCommunicateur   = (MPI_Comm)-1;
    int erreur = MPI_Init( &argc, &argv );   
    
    if (erreur!=0){
        printf("erreur\n");
        free( lpBufferMpi );  
        return -1;
    }
   
   /*2) Attachement àn buffer pour le message*/
    lBufferSize=10000 * sizeof(long);
    lpBufferMpi = calloc( 10000, sizeof(long));
    erreur = MPI_Buffer_attach( (void*)lpBufferMpi, lBufferSize );
    
    if (erreur!=0){
        printf("erreur\n");
        free( lpBufferMpi );  
        return -1;
    }
    
    printf( "Exe : Lance \n" );
    MPI_Comm_get_parent(&CommParent);   
    MPI_Intercomm_merge( CommParent, 1, &lCommunicateur );
    MPI_Comm_rank( lCommunicateur, &lRank );
    MPI_Comm_size( lCommunicateur, &lTailleCommunicateur );
    lRangMain   =1-lRank;
    printf( "Exe: lRankExe  = %d   lRankMain  = %d\n", lRank , lRangMain);
   
    sleep(1);
    MPI_Buffer_detach( (void*)lpBufferMpi, &lBufferSize );
    MPI_Comm_free( &lCommunicateur );
    MPI_Finalize( );
    free( lpBufferMpi );
    printf( "Exe: Fin.\n\n\n" );
    return 0;
}
