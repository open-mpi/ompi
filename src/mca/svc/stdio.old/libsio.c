/*
 * $HEADER$
 */

/*
	HARNESS G_HCORE
	HARNESS FT_MPI
	HARNESS FTMPI_NOTIFIER 

	Innovative Computer Laboratory,
	University of Tennessee,
	Knoxville, TN, USA.

	harness@cs.utk.edu

 --------------------------------------------------------------------------

 Authors:	
			THara Angskun <angskun@cs.utk.edu>

 --------------------------------------------------------------------------

                              NOTICE

 Permission to use, copy, modify, and distribute this software and
 its documentation for any purpose and without fee is hereby granted
 provided that the above copyright notice appear in all copies and
 that both the copyright notice and this permission notice appear in
 supporting documentation.

 Neither the University of Tennessee nor the Authors make any
 representations about the suitability of this software for any
 purpose.  This software is provided ``as is'' without express or
 implied warranty.

 HARNESS, HARNESS G_HCORE and  FT_MPI was funded in part by the 
 U.S. Department of Energy.

*/

#include "ompi_config.h"
#include "libsio.h"
#include "debug.h"

static struct pair rios_pair[RIO_MAXFD];
static int s_num_pair;
static struct pollfd* ppoll = NULL;
static int ppoll_space = 0;

/* rios_init - initialize data structure
*/
int rios_init(void)
{
   int i;

   for(i=0;i<RIO_MAXFD;i++) {
      rios_pair[i].name=NULL;
      rios_pair[i].sockfd=-1;
      rios_pair[i].mypair=-1;
      rios_pair[i].silent=0;
   }
   s_num_pair = 0;

   return RIO_SUCCESS;
}

/* rios_stop - clean up data structure
*/
int rios_stop(void)
{
   int i;
   for(i=0;i<RIO_MAXFD;i++) {
      if(s_num_pair==0) break;
      if(rios_pair[i].sockfd!=-1) {
         close(rios_pair[i].sockfd);
	 close(i);
         rios_pair[i].sockfd=-1;
         rios_pair[i].silent=0;
	 if(rios_pair[i].name!=NULL) _FREE(rios_pair[i].name);
	 rios_pair[i].name=NULL;
	 if(rios_pair[i].mypair!=-1) {
	     close(rios_pair[i].mypair);
             rios_pair[rios_pair[i].mypair].sockfd=-1;
             rios_pair[rios_pair[i].mypair].silent=0;
	     if(rios_pair[rios_pair[i].mypair].name!=NULL)
               _FREE(rios_pair[rios_pair[i].mypair].name);
	     rios_pair[rios_pair[i].mypair].name=NULL;
	     s_num_pair--;
	 }
	 s_num_pair--;
      }
   }
   if( ppoll != NULL ) _FREE(ppoll);
   ppoll_space = 0;
   return RIO_SUCCESS;
}

/*  rios_unregister - unregister
@param src_fd source file descriptor
*/
int rios_unregister(int src_fd)
{
   if(src_fd <0 || src_fd >= RIO_MAXFD || rios_pair[src_fd].sockfd==-1 ) {
       return RIO_EINVLFD;
   }
   close(src_fd);
   if(rios_pair[src_fd].mypair==-1) {
      writemsgconn(rios_pair[src_fd].sockfd,"",0);   /* tell listener to close connection */
      close(rios_pair[src_fd].sockfd);
   } else {
      rios_pair[rios_pair[src_fd].mypair].mypair=-1;
      rios_pair[src_fd].mypair=-1;
   }
   rios_pair[src_fd].sockfd=-1;
   rios_pair[src_fd].silent=0; /* reset it to normal */
   if(rios_pair[src_fd].name!=NULL) _FREE(rios_pair[src_fd].name);
   rios_pair[src_fd].name=NULL;
   s_num_pair--;
   return RIO_SUCCESS;
}

/* rios_register_dup
@param template_fd
@param my_fd
@param slient slient flags (-1 is yell, 0 is normal, 1 is shut up)
*/
int rios_register_dup(char *name,int template_fd,int my_fd, int silent)
{
   if(template_fd <0 || template_fd >= RIO_MAXFD || rios_pair[template_fd].sockfd == -1 || my_fd < 0 || my_fd >= RIO_MAXFD) {
       return RIO_EINVLFD;
   }
   rios_pair[my_fd].sockfd=rios_pair[template_fd].sockfd; 
   rios_pair[my_fd].mypair=template_fd;
   rios_pair[template_fd].mypair=my_fd;
   if(name!=NULL) {
       rios_pair[my_fd].name=_MALLOC(sizeof(char)*(strlen(name)+1));
       strcpy(rios_pair[my_fd].name,name);
   }
   rios_pair[my_fd].silent=silent;
   s_num_pair++;
   return RIO_SUCCESS;
}

/*  rios_register_pipe2_socket - Register I/O
@param src_fd source file descriptor
@param dest_fd destination file descriptor
@param slient slient flags (-1 is yell, 0 is normal, 1 is shut up)
*/
int rios_register_pipe2_socket(char *name,int src_fd,int dest_fd, int silent)
{
   if(src_fd <0 || src_fd >= RIO_MAXFD || dest_fd < 0 || dest_fd >= RIO_MAXFD) {
       return RIO_EINVLFD;
   }
   rios_pair[src_fd].sockfd=dest_fd; 
   if(name!=NULL) {
       rios_pair[src_fd].name=_MALLOC(sizeof(char)*(strlen(name)+1));
       strcpy(rios_pair[src_fd].name,name);
   }
   rios_pair[src_fd].silent=silent; 
   s_num_pair++;
   return RIO_SUCCESS;
}

/*  rios_register_pipe2_host_port - Register I/O
@param src_fd source file descriptor
@param host hostname
@param port port
@param slient slient flags (-1 is yell, 0 is normal, 1 is shut up)
*/
int rios_register_pipe2_host_port(char *name,int src_fd,char *host,int port, int silent)
{
   int dest_fd;
   dest_fd = getconn (host, &port, 10);     /* search a range of 10 */
   if(dest_fd<0) {
       return RIO_EINVLFD;
   }
   return rios_register_pipe2_socket(name,src_fd,dest_fd,silent);
}

/*  rios_register_pipe2_addr_port - Register I/O
@param src_fd source file descriptor
@param addr address
@param port port
*/
int rios_register_pipe2_addr_port(char *name,int src_fd,long addr,int port, int silent)
{
   int dest_fd;
   dest_fd = getconn_addr (addr, &port, 10);     /* search a range of 10 */
   if(dest_fd<0) {
       return RIO_EINVLFD;
   }
   return rios_register_pipe2_socket(name,src_fd,dest_fd,silent);
}

/* rios_pollinit - init poll call every time if number of pair has been changed
@param poll_fd poll file descriptor list.
@param free_me free pollfd
*/
int rios_pollinit(struct pollfd **poll_fd,int free_me)
{
   int i,j;

   *poll_fd = NULL;
   if(s_num_pair <= 0 ) return RIO_EINVLPAIR;

   if( ppoll_space < s_num_pair ) {
       if( ppoll != NULL ) _FREE( ppoll );
       ppoll_space = s_num_pair + (s_num_pair >> 1);
       ppoll = (struct pollfd*)_MALLOC( sizeof(struct pollfd) * ppoll_space );
   }
   j=0;
   for(i=0;i<RIO_MAXFD;i++) {
      if(rios_pair[i].sockfd!=-1) {
         ppoll[j].fd=i;       
	 ppoll[j].events = POLLIN;
	 ppoll[j].revents = 0;
	 j++;
      }
      if(j>=s_num_pair) break;
   }
   *poll_fd = ppoll;
   return RIO_SUCCESS;
}

/* rios_poll - startup I/O redirection (NOTE: This function return only no valid pair of src/dest left or timeout) 
@param timeout timeout (-1 for infinity)
@return number of connection left before timeout
*/ 
int rios_poll(int timeout)
{

   int i,count;
   struct pollfd *poll_fd;
   struct timeval begin, current;
   char buf[RIO_MAXBUF];
   char tmpbuf[RIO_MAXBUF+500];
   int max_pair;
   int ret;
   long diff;

   gettimeofday(&begin,NULL);
   poll_fd = NULL;
   ret=rios_pollinit(&poll_fd,0);
   if(ret<0) {
       return ret;
   }

   while(s_num_pair>0) {
      gettimeofday(&current,NULL);
      diff=(1000000*(current.tv_sec-begin.tv_sec)) + (current.tv_usec-begin.tv_usec);
      diff=diff/1000; /* millisecond */
      if(diff > timeout) {
         return s_num_pair;
      }

      if((ret=poll(poll_fd, s_num_pair, timeout)) <0) {
         return RIO_EPOLL;
      }
      if(ret==0) {  /* time out */
         return s_num_pair;
      }
      max_pair=s_num_pair; /* we need max_pair because s_num_pair is dynamic value */ 
      for(i=0;i<max_pair;i++) {
         if(poll_fd[i].revents!=0) {
             if( poll_fd[i].revents & POLLERR ) {
                 rios_unregister( poll_fd[i].fd );
             } else {
                 count=read(poll_fd[i].fd,buf,RIO_MAXBUF);
                 if( count <= 0 ) {
                     rios_unregister(poll_fd[i].fd);
                 } else {
                     buf[count]='\0';
                     if (rios_pair[poll_fd[i].fd].silent==1) {  /* shut up */
                         sprintf(tmpbuf,"%s",buf);
                     } else { /* shut down, ... I mean normal  */
                         sprintf(tmpbuf,"[%s] %s",rios_pair[poll_fd[i].fd].name,buf);
                     }
                     ret=writemsgconn(rios_pair[poll_fd[i].fd].sockfd,tmpbuf,strlen(tmpbuf));   
                     if(ret!=strlen(tmpbuf)) {
                         rios_unregister(poll_fd[i].fd);
                     }
                 }
             }
	 }
      } 
      if(s_num_pair!=max_pair) { /* someone close connection */
         ret=rios_pollinit(&poll_fd,1);
         if(ret<0) {
             return ret;
         }
      }
   }
   return RIO_SUCCESS;
}
