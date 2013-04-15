#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

#define MAX_DATA 100

int main( int argc, char **argv )
{
 MPI_Comm server;
 double buf[MAX_DATA];
 char port_name[MPI_MAX_PORT_NAME];
 int done = 0, tag, n, CNT=0;

 MPI_Init( &argc, &argv );
 strcpy(port_name, argv[1] );  /* assume server's name is cmd-line arg */

 MPI_Comm_connect( port_name, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &server );

 n = MAX_DATA;

 while (!done)
   {
     tag = 2; /* Action to perform */
     if ( CNT == 5 ) { tag = 0; done = 1; }
     fprintf(stderr, "Client sending message %d\n", CNT);
     MPI_Send( buf, n, MPI_DOUBLE, 0, tag, server );
     CNT++;
     /* etc */
   }

 MPI_Comm_disconnect( &server );
 MPI_Finalize();

 return 0;
}

