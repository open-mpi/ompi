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
#include "libcio.h"
#include "debug.h"

static int rioc_pair[RIO_MAXFD];
static int c_num_pair;
static int main_fd;

/* rioc_init - initialize data structure
@param fd file descriptor
@block block flag (1== block,0==nonblock)
*/
int rioc_init(int fd,int block)
{
   int i,s;
   for(i=0;i<RIO_MAXFD;i++) {
      rioc_pair[i]=-1;
   }
   c_num_pair = 0;
   main_fd = fd;
   rioc_register_socket(fd);
   if(block) {
      s = allowconn (main_fd, 0, NULL);
      rioc_register_socket(s);
   }
   return RIO_SUCCESS;
}

/* rioc_stop - clean up data structure
*/
int rioc_stop(void)
{
   int i;
   for(i=0;i<RIO_MAXFD;i++) {
      if(c_num_pair==0) break;
      if(rioc_pair[i]!=-1) {
	 closeconn(i);
         rioc_pair[i]=-1;
	 c_num_pair--;
      }
   }
   return RIO_SUCCESS;
}

/*  rioc_register_pipe2_socket - Register I/O
@param src_fd source file descriptor
@param dest_fd destination file descriptor
*/
int rioc_register_socket(int fd)
{
   if(fd==main_fd) {
       rioc_pair[fd]=0;
   } else {
       rioc_pair[fd]=1; 
   }
   c_num_pair++;
   return RIO_SUCCESS;
}

/* rioc_pollinit - init poll call every time if number of pair has been changed
@param poll_fd poll file descriptor list.
*/
int rioc_pollinit(struct pollfd **poll_fd)
{
   int i,j;

#ifndef WIN32
   if((*poll_fd)!=NULL) {
       _FREE(*poll_fd);
   }
   j=0;
   (*poll_fd)=(struct pollfd *)_MALLOC(sizeof(struct pollfd)*c_num_pair);
   for(i=0;i<RIO_MAXFD;i++) {
      if(rioc_pair[i]!=-1) {
         (*poll_fd)[j].fd=i;       
	 (*poll_fd)[j].events = POLLIN;
	 (*poll_fd)[j].revents = 0;
	 j++;
      }
     if(j>=c_num_pair) break; 
   }
#endif
   return RIO_SUCCESS;
}

/* rioc_poll - startup I/O redirection (NOTE: This function return only no valid pair of src/dest left or timeout) 
@param timeout timeout (-1 for infinity)
@return number of connection left before timeout
*/ 
int rioc_poll(int timeout, int *ncon)
{

   int i,count;
   struct pollfd *poll_fd;
   struct timeval begin, current;
   char buf[RIO_MAXBUF];
   int max_pair;
   int ret;
   int s;
   long diff;
   int update;
   int active;

#ifndef WIN32
   gettimeofday(&begin,NULL);
   poll_fd = NULL;
   rioc_pollinit(&poll_fd);
   update=0;

   do {
      gettimeofday(&current,NULL);
      diff=(1000000*(current.tv_sec-begin.tv_sec)) + (current.tv_usec-begin.tv_usec);
      diff=diff/1000; /* millisecond */
      if(timeout>=0) {
         if(diff > timeout) {
            if((poll_fd)!=NULL) {
                _FREE(poll_fd);
            }
	    if(c_num_pair!=0) return c_num_pair-1;
               return c_num_pair;
         }
      }

      if((ret=poll(poll_fd, c_num_pair, timeout)) <0) {
         if((poll_fd)!=NULL) {
             _FREE(poll_fd);
         }
         return RIO_EPOLL;
      }
      if(ret==0) {  /* time out */
         if((poll_fd)!=NULL) {
             _FREE(poll_fd);
         }
	 if(c_num_pair!=0) return c_num_pair-1;
         return c_num_pair;
      }
      max_pair=c_num_pair; /* we need max_pair because c_num_pair is dynamic value */ 
      active=0;
      for(i=0;i<max_pair;i++) {
         if(poll_fd[i].revents!=0) {
             if(poll_fd[i].fd==main_fd) {
                 rioc_pair[main_fd]++;
                 (*ncon)=rioc_pair[main_fd];
                 s = allowconn (main_fd, 0, NULL);
                 rioc_register_socket(s);
		 update=1;
	     } else {
                 count=readmsgconn(poll_fd[i].fd,buf,RIO_MAXBUF);
	         if(count<=0) {
                     close(poll_fd[i].fd);
		     rioc_pair[poll_fd[i].fd]=-1;
		     c_num_pair--;
		     update=1;
	         } else {
                     buf[count]='\0';
                     printf("%s",buf);   
		     buf[0]='\0';
	         }
	     }
	     active++;
	 }
	 if(active==ret) break;
      } 
      /* Don't check if(c_num_pair!=max_pair). because they may add/delete at the same time  */
      if(update==1) {
         rioc_pollinit(&poll_fd);
	 update=0;
      }
   } while(c_num_pair>1);
		 
/*   close(main_fd); */

   if((poll_fd)!=NULL) {
       _FREE(poll_fd);
   }

   if(c_num_pair!=0) return c_num_pair-1;
#endif
   return c_num_pair;
}

/* rioc_getoutput
@param mainfd main output file descriptor to accept output
@param expected_con expected number of connection
*/
int rioc_getoutput(int mainfd,int expected_con)
{
    fd_set rfds, rfds_bak;
    int maxfd, minfd, i, ret;
    int active_con;  /* number of active connection */
    char buf[RIO_MAXBUF];
    int fd;
    int updated=0;
    int curcon=0;
    int respawn=0;

#ifndef WIN32
    FD_ZERO(&rfds_bak);
    FD_SET(mainfd,&rfds_bak);
    maxfd=mainfd+1;
    minfd=mainfd;
    updated=1;

    while(expected_con > 0) {

        if(updated>0) {
	   FD_ZERO(&rfds);
	   maxfd=0;
	   minfd=0;
	   for(fd=0;fd<FD_SETSIZE;fd++) {
              if(FD_ISSET(fd,&rfds_bak)) {
                 FD_SET(fd,&rfds);
		 if(fd>=maxfd) maxfd=fd+1;
		 if(fd<minfd) minfd=fd;
	      }
	   }
	   updated=0;
	}

        active_con = select(maxfd, &rfds, NULL, NULL, NULL);
	if(active_con <= 0) {
            return RIO_EPOLL;
        }	
	i=minfd;
	while(active_con > 0) {
           if(FD_ISSET(i,&rfds)) {
               if(i==mainfd) {
                  ret = allowconn (i, 0, NULL);
		  if(ret>0) {
		     FD_SET(ret,&rfds_bak);
		     updated=1;
		     curcon++;
		     if(curcon>expected_con) {
                         respawn=1;
		     }
		     if(respawn==1) {
                        expected_con++;
		     }
		  }
	       } else {
                  ret=readmsgconn(i,buf,RIO_MAXBUF);
	          if(ret<=0) {
                     close(i);
		     expected_con--;
		     FD_CLR(i,&rfds_bak);
		     updated=1;
	         } else {
                     buf[ret]='\0';
                     printf("%s",buf);   
		     buf[0]='\0';
		     fflush(stdout);
	         }
	       }
	       active_con--;
	   }
           i++;     
	}
    }
#endif
    return RIO_SUCCESS;
}
