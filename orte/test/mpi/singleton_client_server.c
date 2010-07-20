
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <mpi.h>

/*

   LOGIC:

     - the 'server' opens a port and write the info to a file
     - the 'clients' open the file and connect to the port
     - after each accept, the server and client do a merge to
       convert the intercomm to an intracomm

   DETAIL STEPS:

     - server open port
     - server does accept
     - client #1 does connect
     - server and client #1 do merge
     - server does accept
     - client #2 does connect
     - server, client #1 and client #2 do merge
     - server does accept
     - client #3 does connect
     - server, client #1, client #2 and client #3 do merge

*/

#define TAG 0

#define CHK(code) do                            \
  {                                             \
    int retval = code ;                         \
    if (retval != MPI_SUCCESS)                  \
    {                                           \
      fprintf(stderr, "Error: " #code "\n") ;   \
      exit(1) ;                                 \
    }                                           \
  } while(0)

int main(int argc, char *argv[])
{
  char hostname[255] ;
  char buff[255] ;

  int role ;
  int num_clients ;
  int size, rank ;

  FILE *fp ;
  char server_port_name[MPI_MAX_PORT_NAME] ;

  MPI_Comm intercomm, intracomm ;
  MPI_Status status ;
  int msg_count ;
  int i ;

  /* sanity check the args */
  if(argc != 3)
  {
    fprintf(stderr, "usage %s <num clients> <1:server | 0:client>\n", argv[0]) ;
    exit(1) ;
  }

  num_clients = atoi(argv[1]) ;
  role = atoi(argv[2]) ;

  if (num_clients <= 0 || (role != 0 && role != 1))
  {
    fprintf(stderr, "usage %s <num clients> <1:server | 0:client>\n", argv[0]) ;
    exit(1) ;
  }

  /* initialize MPI  */
  CHK(MPI_Init(&argc, &argv)) ;

  /* get the node name */
  {
    int retval = gethostname(hostname, 255) ;
    if(retval == -1)
    {
      fprintf(stderr, "gethostname failed: %s\n", strerror(errno)) ;
      exit(1) ;
    }
  }

  /* server */
  if(role == 1)
  {
    printf("SERVER: on node '%s'\n", hostname) ;

    /* open port to establish connections */
    CHK(MPI_Open_port(MPI_INFO_NULL, server_port_name)) ;

    printf("SERVER: opened port=%s\n", server_port_name) ;

    /* store the port name */
    fp = fopen("server_port_name.txt", "w") ;
    if(fp == NULL)
    {
      fprintf(stderr, "fopen failed: %s\n", strerror(errno)) ;
      exit(1) ;
    }
    fprintf(fp, "%s", server_port_name) ;
    fclose(fp) ;

    /* the server accepts connections from all the clients */
    for(i = 0 ; i < num_clients ; i++ )
    {
      /* accept connections at this port */
      CHK(MPI_Comm_accept(server_port_name, MPI_INFO_NULL, 0,
                          i == 0 ? MPI_COMM_WORLD : intracomm,
                          &intercomm)) ;

      printf("SERVER: accepted connection from client %d\n", i+1) ;

      /* merge, to form one intra communicator */
      CHK(MPI_Intercomm_merge(intercomm, 0, &intracomm)) ;

      printf("SERVER: merged with client %d\n", i+1) ;

      CHK(MPI_Comm_size(intracomm, &size)) ;
      CHK(MPI_Comm_rank(intracomm, &rank)) ;

      printf("SERVER: after merging with client %d: size=%d rank=%d\n", i+1, size, rank) ;
    }
  } /* end server */

  /* client */
  if(role == 0)
  {
    printf("CLIENT: on node '%s'\n", hostname) ;

    fp = fopen("server_port_name.txt", "r") ;
    if(fp == NULL)
    {
      fprintf(stderr, "fopen failed: %s\n", strerror(errno)) ;
      exit(1) ;
    }
    fscanf(fp, "%s", server_port_name) ;
    fclose(fp) ;

    printf("CLIENT: attempting to connect to server on port=%s\n", server_port_name) ;

    /* connect to the server */
    CHK(MPI_Comm_connect (server_port_name, MPI_INFO_NULL, 0, MPI_COMM_WORLD, &intercomm)) ;

    printf("CLIENT: connected to server on port\n") ;

    /* merge the server and client to one intra communicator */
    CHK(MPI_Intercomm_merge(intercomm, 1, &intracomm)) ;

    printf("CLIENT: merged with existing intracomm\n") ;

    CHK(MPI_Comm_size(intracomm, &size)) ;
    CHK(MPI_Comm_rank(intracomm, &rank)) ;

    printf("CLIENT: after merging, new comm: size=%d rank=%d\n", size, rank) ;

    for (i = rank ; i < num_clients ; i++)
    {
      /* client performs a collective accept */
      CHK(MPI_Comm_accept(server_port_name, MPI_INFO_NULL, 0, intracomm, &intercomm)) ;

      printf("CLIENT: connected to server on port\n") ;

      /* merge the two intra comms back to one communicator */
      CHK(MPI_Intercomm_merge(intercomm, 0, &intracomm)) ;

      printf("CLIENT: merged with existing members\n") ;

      CHK(MPI_Comm_size(intracomm, &size)) ;
      CHK(MPI_Comm_rank(intracomm, &rank)) ;

      printf("CLIENT: new size after merging with existing members: size=%d rank=%d\n", size, rank) ;
    }

  } /* end client */

  CHK(MPI_Comm_size(intracomm, &size)) ;
  CHK(MPI_Comm_rank(intracomm, &rank)) ;

  printf("After fusion: size=%d rank=%d\n", size, rank) ;

  if(rank == 0)
  {
    msg_count = num_clients ;

    while(msg_count)
    {
      CHK(MPI_Recv(buff, 255, MPI_CHAR, MPI_ANY_SOURCE,
                   MPI_ANY_TAG, intracomm, &status)) ;

      printf("Received hello msg from '%s'\n", buff) ;
      msg_count-- ;
    }
  }
  else
  {
    /* all ranks > 0 */

    CHK(MPI_Send(hostname, strlen(hostname) + 1, MPI_CHAR, 0, TAG, intracomm)) ;
  }

  CHK(MPI_Finalize()) ;

  fprintf(stderr, "Rank %d is exiting\n", rank);
  return 0 ;
}
