/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <libgen.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <resolv.h>
#include <netdb.h>
#define TIMEOUT 500
#define SERV_PORT 6780


int setupContactPoint(int *port)
{
   int listenfd, rc;
   struct sockaddr_in servaddr;

   listenfd = socket(AF_INET, SOCK_STREAM, 0);
   bzero(&servaddr, sizeof(servaddr));
   servaddr.sin_family      = AF_INET;
   servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
   (*port) = SERV_PORT;
   servaddr.sin_port = htons((*port));
   while((rc=bind(listenfd, (struct sockaddr *) &servaddr, sizeof(servaddr)))!=0) {
       servaddr.sin_port = htons(++(*port));
   }
   listen(listenfd, 5);
   return listenfd;
}

int main(int argc,char **argv)
{
   int port,listenfd;
   int number_of_server_child=0;
   int n_child;
   int ret;

   if(argc!=2) {
      printf("%s <#of server's child process>\n",argv[0]);
      exit(1);
   }

   listenfd=setupContactPoint(&port);
   n_child=atoi(argv[1]);
   printf("Port is %d\n",port);

   rioc_init(listenfd, 0);
   ret=rioc_poll(TIMEOUT,&number_of_server_child);
   while(ret!=0 || number_of_server_child != n_child) {
      ret=rioc_poll(TIMEOUT,&number_of_server_child);
   }
}
