#include <stdio.h>
#include "libsio.h"

#define TIMEOUT 500

/* 
This file is an example for application that use libsio
It simply create numbers of processes and sent their output to 
speicify host and port
*/

struct rio {
   int pid;
   int pp[2]; /* stdout */
   int pp2[2]; /* stderr */
} *rios;

int main(int argc,char **argv)
{

   int nchild, ret;
   int entry=0;
   char cioapp_host[256];
   char buf[256];
   int cioapp_port;
   int startup_silent=0; /* 1=silent, 0= verbose */
   int pid,k;

   if(argc!=4) {
      printf("%s <cioapp_hostname> <cioapp_port> <#of child processes>\n",argv[0]);
      exit(1);
   }

   strcpy(cioapp_host,argv[1]);
   cioapp_port=atoi(argv[2]);
   nchild=atoi(argv[3]);
   if(nchild <1) {
      printf("#of child must be more than 1\n");  
      exit(1);
   }

   rios_init();
   rios=(struct rio *)malloc(sizeof(struct rio)*nchild);

   for(entry=0;entry<nchild;entry++) {
      if (pipe(rios[entry].pp) == -1) perror("pipe");
      if (pipe(rios[entry].pp2) == -1) perror("pipe");

      sprintf(buf,"%d:stdout",entry);
      ret=rios_register_pipe2_host_port(buf,rios[entry].pp[0],cioapp_host,cioapp_port,startup_silent);

      if(ret!=RIO_SUCCESS) printf("Cannot connect to %s:%d ret %d\n",cioapp_host,cioapp_port,ret);

      sprintf(buf,"%d:stderr",entry);
      ret=rios_register_dup(buf,rios[entry].pp[0],rios[entry].pp2[0],startup_silent);
      if(ret!=RIO_SUCCESS) printf("Cannot duplicate destination for stderr (ret=%d)\n",ret);

      pid = fork();

      if (pid == 0) {
         dup2(rios[entry].pp[1],1);
         dup2(rios[entry].pp2[1],2);
         for(k=0;k<5;k++) {
            printf("Hi world, my name is foo %d/%d [bar code is %d (pid %d)]\n",k+1,5,entry,getpid());
            sleep(1);
         }
         exit(1);
      }  
      rios[entry].pid=pid;
      close(rios[entry].pp[1]); 
      close(rios[entry].pp2[1]); 
      rios[entry].pp[1] = -1;
      rios[entry].pp2[1] = -1;
   }

   /* Now, parent poll for child stdout/err */  
   while(1) {
      rios_poll(TIMEOUT);
   }
   return 0;
}
