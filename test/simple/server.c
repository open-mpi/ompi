#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "mpi.h"

#define MAX_DATA 100

int main( int argc, char **argv )
{
 MPI_Comm client;
 MPI_Status status;
 char port_name[MPI_MAX_PORT_NAME];
 double buf[MAX_DATA];
 int size, again;

 MPI_Init( &argc, &argv );
 MPI_Comm_size(MPI_COMM_WORLD, &size);
 if (size != 1) {
    fprintf(stderr, "Server too big - need only 1 rank\n");
    exit(1);
 }
 MPI_Open_port(MPI_INFO_NULL, port_name);
 printf("server available at %s\n",port_name);

 while (1)
   {
     MPI_Comm_accept( port_name, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &client );
     again = 1;

     while (again)
       {
         fprintf(stderr, "Server loop %d\n", again);
         MPI_Recv( buf, MAX_DATA, MPI_DOUBLE, MPI_ANY_SOURCE,
MPI_ANY_TAG, client, &status );

         switch (status.MPI_TAG)
           {
           case 0:
             fprintf(stderr, "Server recvd terminate cmd\n");
             MPI_Comm_disconnect( &client );
             MPI_Close_port(port_name);
             MPI_Finalize();
             return 0;
           case 2: /* do something */
             fprintf( stderr, "Do something ...\n" );
             break;
           default:
             /* Unexpected message type */
             MPI_Abort( MPI_COMM_WORLD, 1 );
           }
           ++again;
       }
   }
}

