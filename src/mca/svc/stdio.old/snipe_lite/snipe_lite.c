/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
	HARNESS G_HCORE

	Innovative Computer Laboratory,
	University of Tennessee,
	Knoxville, TN, USA.

	harness@cs.utk.edu

 --------------------------------------------------------------------------

 Authors:	Graham Fagg <fagg@cs.utk.edu>

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

/*
	SNIPE_LITE was part of the SNIPE experimental metacomputing system 

	the comms library was a non threaded 'get the message there' TCP
	library that is a little more carefull than the cs340 socketfun stuff 

	Incept for this code was in 1998.

*/

#include "ompi_config.h"
#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <fcntl.h>

#include <sys/poll.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/ioctl.h>

#include <netinet/in.h>
#include <netinet/tcp.h>
#include <net/if.h>

#include <arpa/inet.h>

#include "syslog.h"

#ifdef __sun
#include <sys/sockio.h>
#endif

#include "snipe_lite.h"
#include "syslog.h"

#define RETRIES 2

extern int errno;

static char* default_name = NULL;
static int host_addr = 0;

syslog_t* slsyslog = NULL;

/* now that we have included everything under the Sun... */
/* and I won't mention Sun TLI problems here */

/* moved to snipe_lite.h, since these routines are also
   acces from snipe2.c */
/* void nodelay();*/	/* hack routine to improve performance.. maybe */
/* void setnonblocking();*/ /* sets the given socket non-blocking */
/* void setblocking(); *//* sets the given socket blocking again */
/* int setsendrecvbufs();*/  /* Sets send/recv buffer sizes */


int setportconn (int *s,int port,int range)	
/*                   returns final port value or error */
/* int *s;					 socket server uses.. this is created here */
/* int port; 				 initial port we make service on. */
/* int range;				 range of ports */
{
    struct sockaddr_in  sa;
    int ts;
    int i;
    int rc = 0;
    int p;

    p = port;
    ts = socket (AF_INET, SOCK_STREAM, 0);   /* yuck */
#ifdef VERBOSE
    printf("Socket [%d]\n", ts);
#endif /* VERBOSE */
    sa.sin_family       = AF_INET;
    sa.sin_addr.s_addr  = INADDR_ANY;

    for( i = 0; i <= range; i++ ) {
	sa.sin_port = htons (p); /* convert just to be safe */
	rc = bind( ts, (struct sockaddr *)&sa, sizeof (sa) );
#ifdef VERBOSE
	printf ("Bind on socket [%d] port [%d] = [%d]\n", ts, p, rc);
	if (rc) perror("Bind()");
#endif /* VERBOSE */

	if (!rc) { /* i.e. a valid port was found */
#ifdef VERBOSE
	  printf("Attempting listen after bind on socket [%d]\n", ts);
#endif /* VERBOSE */
	  setsendrecvbufs( ts, 128 );  /* TODO check if it's really useful */
	  rc = listen (ts, LISTENBACKLOG);	/* handle 5 possible connection at once */
#ifdef VERBOSE
	  printf("Listen on socket [%d] = [%d]\n", rc);
	  if (rc) perror ("Listen()");
#endif /* VERBOSE */
	  if (rc) break; /* i.e. error */
	  /* else */
	  *s = ts;	/* return socket value as well */
	  return (p);
	}
	
        /* 	 if ((rc!=EADDRINUSE)&&(rc!=EADDRNOTAVAIL)&&(rc!=EACCES)) { */
        /* for now we ignore this... yes we do */
        /*         } */
	
	/* port was unavailable so */
	p++; /* look at the next one */
    }
    
    /* ok.. we didnot find one */
    close (ts);
    *s = 0;	/* return no socket */
    return (rc);	/* which is negative and the last bind return code */
}

int allowconn (int s,int fe,int * spid)
/* int s;        socket we listen on */
/* int fe;       Fork Enabled? 0=no 1=yes */
/*               If enabled then forks new process on accept */
/* int *spid;    new process id handling the returned socked */
/*               used by multitasking servers */
{
   int s2;	/* new socket */
   /* struct sockaddr *new_s_addr_ptr;  */
   struct sockaddr new_s_addr; 
   /* socklen_t */ int new_s_addr_len;
    
   new_s_addr_len = sizeof (new_s_addr);
#ifdef VERBOSE
   printf("Allowing connections on socket [%d]\n", s);fflush(stdout);
#endif /* VERBOSE */
   /* s2 = accept (s, new_s_addr_ptr, &new_s_addr_len); */
    
   /* make sure the socket is a blocking socket i.e. hasn't been probe'd */
   setblocking (s);
 accept_again:
   s2 = accept (s, &new_s_addr, &new_s_addr_len);
   if (s2<0) {
      if( errno == EINTR ) {
         /* interrupted by a signal */
         goto accept_again;
      }
#ifdef VERBOSE
      printf("Accept on socket [%d] failed as [%d]\n", s, s2);
      perror ("Accept()");
#endif /* VERBOSE */
      return (s2);
   }
#ifdef VERBOSE
   printf("Accepted connection on socket [%d] as new socket [%d]\n", s, s2);
#endif /* VERBOSE */
    
   nodelay(s2);
   return (s2);
}

/* Probe conn is a non blocking version of allowconn() */
int probeconn (int s,int fe,int * spid)
/* int s;      socket we listen on */
/* int fe;     Fork Enabled? 0=no 1=yes */
/*             If enabled then forks new process on accept */
/* int *spid;  new process id handling the returned socked */
/*             used by multitasking servers */
{
   int s2;	/* new socket */
   /* struct sockaddr *new_s_addr_ptr;  */
   struct sockaddr new_s_addr; 
   /* socklen_t */ int new_s_addr_len; 
   int rc;

   new_s_addr_len = sizeof (new_s_addr);
#ifdef VERBOSE
   printf("Allowing connections on socket [%d]\n", s);
#endif /* VERBOSE */
   /* s2 = accept (s, new_s_addr_ptr, &new_s_addr_len); */
   /* make the socket non blocking for the probe to work */
   /* setnonblocking (s); */

   /* now just check using poll which should be faster */
   rc = pollconn (s, 0, 0);

   if (!rc) return (0);
   if (rc>0) {
      s2 = accept (s, &new_s_addr, &new_s_addr_len);
#ifdef VERBOSE
      printf("Accept returned [%d][0x%x]\n", s2, s2);
#endif /* VERBOSE */

      if (s2<0) {
         /* 	printf("Accept on socket [%d] failed as [%d]\n", s, s2); */
         /* 	printf("Errno is %d\n", errno); */
         /* 	printf("EWOULDBLOCK = %d\n", (int) EWOULDBLOCK); */
         /* as this is non-blocking printing the below is wrong as its not an */
         /* error, but an unavilable connection */
         /*  	perror ("Accept()"); */
         /* 	close (s2);  */
         /* not that it was open in the first place */
         /* be nice and make the return value 0 ? */
         /* 	s2 = 0; */
         /* 	return (s2); */
         /* 	setnonblocking (s);	 */
         /* return it back to blocking */
         return (0);
      }
#ifdef VERBOSE
      printf("Accepted connection on socket [%d] as new socket [%d]\n", s, s2);
#endif /* VERBOSE */

      /* make the new socket blocking by default */
      setblocking (s2);

      /* set tcpnodelay as well */
      nodelay(s2);

      return (s2);
   }

   if (rc<0) return (rc); /* return error code if poll had a problem */
   return 0;
}

int getconn (char * host,int * port,int search)
/* char *host;    full name of host running service */
/* int  *port;    port where service is running */
/*                note this is also a return value */
/* int  search;   range allowed for port search */
{
   struct sockaddr_in  sa_in;
   int target_port;
   struct hostent *hp;
   int ts;
   int rc = 0;

   target_port = *port;					 /* get start point */

   hp = gethostbyname (host);
   if(hp==NULL) {   /* cannot resolve hostname */
      return -1;
   }

#ifdef IMA_SGI6
   memcpy ( &sa_in.sin_addr, hp->h_addr, hp->h_length);
#else
   {
      u_long  target_s_addr;
      bcopy(hp->h_addr, (char *)&target_s_addr, hp->h_length);
      sa_in.sin_addr.s_addr  = target_s_addr; 
   }
#endif

   sa_in.sin_family  = AF_INET;

   while (search>=0) {
      ts = socket (AF_INET, SOCK_STREAM, 0);   /* yuck */
      /* try a new socket each time? */
      sa_in.sin_port = htons (target_port); /* convert just to be safe */
      /* hope I can keep reusing this struct */

      rc = connect (ts, (struct sockaddr *) &sa_in, sizeof(sa_in));
      if( rc == 0 ) { /* success */
         *port = target_port;	/* tell them the port used */
         nodelay(ts);
         return (ts);            /* give them the socket */
      } else { /* failure */
#if !defined(NDEBUG)
         fprintf( stderr, "Connect failed with host %s port %d (%s)\n",
                  host, target_port, strerror(errno) );
#endif  /* NDEBUG */
         target_port++;	/* look at the next port */
         search--;		/* one less attempt left */
         /* I belive ts is now defunted.. so I close it?? */
         close (ts);
      }
   } /* while */

   /* well we are here so fidles.. */
   return (rc);

}


int getconn_addr (unsigned long addr,int * port,int search)
/* unsigned long addr;   sa_addr of host running service */
/* int  *port;           port where service is running */
/*                       note this is also a return value */
/* int  search;             range allowed for port search */
{
   struct sockaddr_in  sa_in;
   int target_port;
   int ts;
   int rc = 0;

   sa_in.sin_addr.s_addr  = addr;
   sa_in.sin_family       = AF_INET;

   target_port = *port;  /* get start point */

   while( search >= 0 ) {
      ts = socket (AF_INET, SOCK_STREAM, 0);   /* yuck */
      /* try a new socket each time? */
      sa_in.sin_port         = htons (target_port); /* convert just to be safe */
      /* hope I can keep reusing this struct */

      rc = connect (ts, (struct sockaddr *) &sa_in, sizeof(sa_in));

      if (!rc) { /* success */
         *port = target_port;	/* tell them the port used */
         nodelay(ts);
         return (ts);            /* give them the socket */
      }
      /* failure */
      WARNING( LOG_CONN, "Connection failed to node %s on port %d\n",
               inet_ntoa(sa_in.sin_addr), target_port );
      target_port++;	/* look at the next port */
      search--;		/* one less attempt left */
      /* I belive ts is now defunted.. so I close it?? */
      close (ts);
   } /* while */

   /* well we are here so fidles.. */
   return (rc);
}

int closeconn (int s)
/* int	s;  socket to close, also notifies the other party */

{
   /* too tired */
   return close (s);
}


int pollconn (int s,int dir,int tout)	/* checks for in/out events on a socket */
/* int s; */
/* int dir;	 direction 0 = read, 1 = write, -1 for error detect */
/* int tout;  */
{
   struct pollfd pfd;
   /* int tout; */
   int rc;
  
   if (tout<0) tout = 10;	/* default to (10ms) if a bad arg */
  
   /* OK, pack the poll data struct */
   pfd.fd = s;
   /* select events depending on situation */
   pfd.events = POLLIN;
   if (dir>0) pfd.events |= POLLOUT;				/* look to write */
  
   pfd.revents = 0;
  
   /* ok, kick it in the arse and let it go */
   rc = poll (&pfd, 1, tout);
   if( rc >= 0 ) {
      /* zero means nothing relevant on the socket, and >0 one event ready */
      return rc;
   }
   /* now we get a negative return code from poll */
   if( errno == EINTR ) return 0; /* dont be messy, it's not really an error */
   DUMP_SOCKET_ERROR( s, errno );
   return -1;
}



int writeconn (int s,char * data,int len)/* sends stream data. I.e. no header/seq stuff */	
/*                return value = len or error */
/* int	s;        socket conn is on */
/* char *data;    raw data to send */
/* int len;       length of data to send */	
{
   int i;
   int tosend, gone, fluffed;              /* loop variables */


   tosend = len;
   gone = 0;       /* just in case */
   fluffed = 0;        /* just in case */

   for (i=0;tosend>0;) {
#ifdef DB9
      printf("Write loop for socket [%d]. Sent [%d] / [%d] or [%d] left.\n",
             s, i, len, tosend);
#endif /* DB9 */

      gone = write (s, &data[i], tosend);

#ifdef DB9
      printf("wrote [%d] byte on socket [%d].\n", gone, s);
#endif /* DB9 */
      if (gone<=0) {
         fluffed++;
#ifdef VERBOSE
         if (gone<0) perror ("Write()");
#endif /* VERBOSE */
         if (fluffed==RETRIES) {
            DUMP_SOCKET_ERROR(s, errno);
            fprintf(stderr,"%s:%d Problem, connection on socket [%d] has failed.\nThis connect has been closed.\n", __FILE__, __LINE__, s);
            fflush(stderr);
            close (s);
            return (i); /* how much we sent in the end... */
            /* upto the user app to realise the error. */
         } /* if fluffed */
      }
      else { /* i.e. we sent some */
         i+= gone;   /* total sent and index ptr update */
         tosend -= gone;
         fluffed = 0;    /* fluffed reset counter */
      } /* if gone */
   } /* for look on tosend */


   /* ok all done... now return amount wrote */
   return (i);


}

int readconn (int s,char * data,int len)/* reads stream data. I.e. no header/seq stuff */	
/*                  return is read length or error */
/* int	s;          socket conn is on */
/* char *data;      raw data buffer */
/* int len;         length of data tobe read */
{
   int i;
   unsigned int toget;  			/* note type */
   int got, fluffed;               /* loop variables */

   toget = (unsigned) len;
   fluffed = 0;

#ifdef DB9
   printf("read on [%d] to get msg length [%d]\n", s, toget);
#endif

   for (i=0;toget>0;) {
#ifdef DB9
      printf("Reading on [%d] amount [%d]\n", s, toget);
#endif /* DB9 */

      got = read (s, &data[i], toget);

#ifdef DB9
      printf("reading on [%d] got [%d] bytes.\n", s, got);
#endif /* DB9 */

      if (got<=0) {
         fluffed++;
         if (fluffed==RETRIES) {
#if !defined(NDEBUG)
            fprintf(stderr,"%s:%d Problem, connection on socket [%d] has failed.\nThis connect has been closed.\n", __FILE__, __LINE__, s);
#endif  /* NDEBUG */
            close (s);
            return (i); /* how much we got in the end... */
                        /* upto the user app to realise the error. */
         } /* if fluffed */
      }
      else { /* i.e. we got */
         i+= got;    /* total got and index ptr update */
         toget -= got;
         fluffed = 0;    /* fluffed reset counter */
      } /* if got */
   } /* for look on toget */

   /* ok all done... now return amount read */
   return (i);



}


int writemsgconn (int s,char * data,int len)/* sends message data. I.e. header & seq */	
/*               return value = len or error */
/* int	s;       socket conn is on */
/* char *data;   raw data to send */
/* int len;      length of data to send */	
{
   int i;
   int n;
   unsigned int nblen;			/* networkbyte message length */
   int tosend, gone, fluffed;				/* loop variables */

   nblen = (unsigned int) htonl (len);	/* message length for the header */
#ifdef DB9
   printf("Attempting message write on [%d] for msg of len [%d]\n", s, len);
#endif

   /* write outgoing message length */
   n = write (s, &nblen, sizeof(unsigned int));	/* ek */

   if(n!=sizeof(int)) { return (-100); } /* ok thats a bad way to do it! */
#ifdef DB9
   printf("wrote on socket [%d] message hdr.\n", s);
#endif



   tosend = len;
   gone = 0; 		/* just in case */
   fluffed = 0;		/* just in case */

   for (i=0;tosend>0;) {
#ifdef DB9	
      printf("Write loop for socket [%d]. Sent [%d] / [%d] or [%d] left.\n", 
             s, i, len, tosend);
#endif /* DB9 */

      gone = write (s, &data[i], tosend);

#ifdef DB9	
      printf("wrote [%d] byte on socket [%d].\n", gone, s);
#endif /* DB9 */

      if (gone<=0) {
         fluffed++;
#ifdef VERBOSE
         if (gone<0) perror ("Write()");
#endif /*  VERBOSE */
         if (fluffed==RETRIES) {
            fprintf(stderr,"%s:%d Problem, connection on socket [%d] has failed.\nThis connect has been closed.\n", __FILE__, __LINE__, s);
            fprintf( stderr, "write %d from %d bytes\n", i, len );
            close (s);
            return (i);	/* how much we sent in the end... */
            /* upto the user app to realise the error. */
         } /* if fluffed */
      }
      else { /* i.e. we sent some */
         i+= gone;	/* total sent and index ptr update */
         tosend -= gone;
         fluffed = 0;	/* fluffed reset counter */
      } /* if gone */
   } /* for look on tosend */


   /* ok all done... now return amount wrote */
   return (i);

}


int readmsgconn (int s,char * data,int mlen)/* reads message data. I.e. header & seq */	
/*                return value = len or error */
/* int	s;        socket conn is on */
/* char *data;    raw data buffer */
/* int mlen;      max length of data buffer */	
{
   int readed = 0;
   int n;
   char junk[512];              /* used as temporary trash */
   unsigned int ilen, nbilen;	/* incomming message length */
   unsigned int	tojunk, toget;	/* note type */
   int got, fluffed;            /* loop variables */

   fluffed = 0;
   readed = 0;

   /* read incomming message length */
   n = read (s, &nbilen, sizeof(unsigned int));	/* ek */
   if( n != sizeof(int) ) { return (-100); } /* ok thats a bad way to do it! */

   ilen = (unsigned int) ntohl (nbilen);	/* convert from network byte format */
   if( ilen > mlen ) {  /* buffer too small.. will have to truncate. */
      /* Note we don't read ahead or anything fancy */
      tojunk = ilen - mlen;
      toget  = mlen;
   } else {
      tojunk = 0;
      toget = ilen;
   }
#ifdef DB9
   printf("read on [%d] total msg length [%d] overrun [%d]\n", s, toget, tojunk);
#endif

   while( toget > 0 ) {
      got = read (s, &data[readed], toget);
      if( got < 0 ) {
         if( (errno == EAGAIN) || (errno == EINTR) ) continue;
         if( (++fluffed) == RETRIES ) {
            fprintf(stderr,"%s:%d Problem, connection on socket [%d] has failed.\nThis connect has been closed.\n", __FILE__, __LINE__, s);
            goto report_error;
         } /* if fluffed */
      }
      readed += got;	/* total got and index ptr update */
      toget -= got;
      fluffed = 0;	/* fluffed reset counter */
   } /* for look on toget */

   /* now for the truncate bit. */
   while( tojunk > 0 ) {
      got = ( tojunk < 512 ? tojunk : 512 );
      got = read( s, junk, got );
      if( got < 0 ) {  /* error on read check for normall interrupt */
         if( (errno == EINTR) || (errno == EAGAIN) ) continue;
         fprintf(stderr,"%s:%d Problem, connection on socket [%d] has failed.\nThis connect has been closed.\n", __FILE__, __LINE__, s);
         goto report_error;
      }
      tojunk -= got;
   }
   /* ok all done... now return amount read */
   return readed;
 report_error:
   /* upto the user app to realise the error. */
   close( s );
   return readed;
}

/* this routine sets the socket to no delay */
void nodelay(int s)
{
#ifdef NODELAYALLOWED
   int one=1;	/* spotted by Rainer Keller in Stuttgart */
   struct protoent *p;
   p = getprotobyname("tcp");
   /* if( p && setsockopt(s, p->p_proto, TCP_NODELAY, &one, sizeof(one)) < 0) */
   if( p && setsockopt(s, p->p_proto, TCP_NODELAY, (char*)&one, sizeof(one)) < 0)
      perror("setsockopt: nodelay");
#else
   /* printf("TCPNODELAY ignored\n"); */
#endif /* NODELAYALLOWED */
}

void setnonblocking(int s ) /* sets the given socket non-blocking */
{
   int i;
   i = fcntl (s, F_GETFL, 0);
#ifdef VERBOSE
   printf("FL on [%d] = %d\n", s, i);
#endif /* VERBOSE */
   i = i | O_NONBLOCK; /* mark it as non blocking */
#ifdef VERBOSE
   printf ("O_NONBLOCK = %d\n", O_NONBLOCK);
#endif /* VERBOSE */
   (void)fcntl (s, F_SETFL, i);
#ifdef VERBOSE
   printf("New FL [%d] on [%d] returns [%d]\n", i, s, j);
#endif /* VERBOSE */
}

void setblocking(int s ) /* sets the given socket blocking again */
{
   int i;
   i = fcntl (s, F_GETFL, 0);
#ifdef VERBOSE
   printf("FL on [%d] = %d\n", s, i);
#endif /* VERBOSE */
   i = i & (!O_NONBLOCK); /* mark it as non non-blocking, i.e. blocking */
   (void)fcntl (s, F_SETFL, i);
#ifdef VERBOSE
   printf("New FL [%d] on [%d] returns [%d]\n", i, s, j);
#endif /* VERBOSE */
}

int	setsendrecvbufs (int s,int bufsize ) /* Sets send/recv buffer sizes */
		/* returns 0 for ok and 1 for unix not accepting and -1 for error */
		/* if a problem occurs, it sets buffers back to what they were */
		/* Note: Bufsize is in kilobytes */
{
   unsigned int sb_org, rb_org;
   unsigned int sb_new, rb_new;
   unsigned int sb_chk, rb_chk;
   /* socklen_t */ int optsize;
   int rc;

   optsize = sizeof (int); /* I hope... */

   sb_new = bufsize * 1024;	/* into bytes */
   rb_new = bufsize * 1024;	/* into bytes */

   if (sb_new<=0) {
      fprintf(stderr,
              "SNIPE_LITE:Conn:attempt to set send/recv bufs to %d ignored\n",
              sb_new);
      return (-1);
   }
	
   /* get original values first */

   rc = getsockopt(s, SOL_SOCKET, SO_SNDBUF, (char*)&sb_org, &optsize);		
   rc = getsockopt(s, SOL_SOCKET, SO_RCVBUF, (char*)&rb_org, &optsize);  

   /* Now to attempt to set the new ones */

   rc = setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char*)&sb_new, optsize);
   rc = setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char*)&rb_new, optsize);

   /* Now to verify the socket options */

   rc = getsockopt(s, SOL_SOCKET, SO_SNDBUF, (char*)&sb_chk, &optsize);		
   rc = getsockopt(s, SOL_SOCKET, SO_RCVBUF, (char*)&rb_chk, &optsize);  

   /* ok junk debugging */
#ifdef VERBOSE
   printf("[sockopt] socket %d send org %d new %d vrfy %d\t\trecv org %d, new %d vrfy %d\n",
          s, sb_org, sb_new, sb_chk, rb_org, rb_new, rb_chk);
#endif

   /* Now to do what we do */

   /* if both are ok... worked */
   if ((sb_chk==sb_new)&&(rb_chk==rb_new)) return (0);

   /* if **either** match the older values */
   if ((sb_chk==sb_org)||(rb_chk==rb_org)) return (1);

   /* else we have to attempt a reset */
   rc = setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char*)&sb_org, optsize);
   rc = setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char*)&rb_org, optsize);

   /* At this point we could check again, but there is little point as */
   /* if this hasn't changed it back we are in trouble anyway */
   return (1);
}

double sec_time() 
/* returns the time in seconds as a double */
{
   struct timeval tp;
   struct timezone tzp;
   double sec=0.0;
   double psec=0.0;
  
   gettimeofday (&tp, &tzp);
   sec = (double)tp.tv_sec;
   psec = ((double)tp.tv_usec)/1000000.0;
  
   return (sec+psec);
}

/* Return:
 * 0 if there is no loopback
 * 1 if there is a loopback
 * 2 if we found a legal IP adress (except loopback).
 */
int find_ip_on_all_interfaces( struct sockaddr_in *pAddr )
{
 /******************************************************************/
/*
 * Get the interface device list, walk through it and deduce those
 * interfaces which can broadcast.
 */
   int i, sd, numdevs, have_loopback = 0;
   struct ifconf ifc_conf;
   char ifc_conf_buf[BUFSIZ];  /* 1024/32 == space for 32 interfaces */
   struct ifreq *devptr;
   int ifc_conf_buf_size;
   
   /*
    * Open a socket, any type will do so we choose UDP, and ask it with
    * an ioctl call what devices are behind it.
    */
   if ((sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
      /* HINT(LOG_CONN,"Error: Unable to create socket\n"); */
      goto complete_and_return;
   }
   
   /*
    * Fill the buffer with our static buffer, probably big enough, and get
    * the interface configuration.
    */
   ifc_conf_buf_size = sizeof ifc_conf_buf;
   ifc_conf.ifc_len = ifc_conf_buf_size;
   ifc_conf.ifc_buf = ifc_conf_buf;
   if (ioctl(sd, SIOCGIFCONF, &ifc_conf) < 0) {
      /* HINT(LOG_CONN,"Error: Unable to get network interface conf\n"); */
      close(sd);
      goto complete_and_return;
   }
   if ((sizeof ifc_conf_buf - ifc_conf.ifc_len) <= sizeof(struct ifreq))
      /* HINT(LOG_CONN, "Info: More interfaces then we anticipated.\n"); */
      
      /*
       * Excess space should be larger than one ifreq or we need more.  If
       * the buffer was not big enough then we need to malloc a larger space
       * and try again.  There is no number of retries.  We either get them
       * all or we run out of memory.
       */
      while ((sizeof(ifc_conf_buf) - ifc_conf.ifc_len) <= sizeof(struct ifreq)) {
         if (ifc_conf_buf_size != sizeof(ifc_conf_buf))
            free(ifc_conf.ifc_buf);     /* We allocated it last time around. */
         ifc_conf_buf_size *= 2;
         ifc_conf.ifc_len = ifc_conf_buf_size;
         if ((ifc_conf.ifc_buf = malloc(ifc_conf_buf_size)) == 0) {
            /* HINT(LOG_CONN,"Error: Out of memory allocating interfaces.\n"); */
            close(sd);
	    goto complete_and_return;
         }
#ifdef _AIX
         if( ioctl(sd, CSIOCGIFCONF, &ifc_conf) < 0 )
#else
         if( ioctl(sd, SIOCGIFCONF, &ifc_conf) < 0)
#endif  /* __aix__ */
            {
               /* HINT(LOG_CONN,"Error: Unable to get network interface conf\n"); */
               close(sd);
               goto complete_and_return;
            }
      }
   
   /*
    * An array of devices were returned.  Which ones are up right now and
    * have broadcast capability?
    */
   numdevs = ifc_conf.ifc_len / sizeof(struct ifreq);
   for (i = 0; i < numdevs; i++) {
      /* devptr points into an array of ifreq structs. */
      devptr = &ifc_conf.ifc_req[i];

#ifdef _AIX
        if( (devptr->ifr_addr.sa_family != AF_INET) && (devptr->ifr_addr.sa_family != AF_INET6) && (devptr->ifr_addr.sa_family != AF_LINK) )
#else
        if( (devptr->ifr_addr.sa_family != AF_INET) )
#endif
           {
              /*    HINT(LOG_CONN, "not AF_INET device %s (%d)\n", devptr->ifr_name, devptr->ifr_addr.sa_family ); */
              continue;
           }
        
        if (ioctl(sd, SIOCGIFFLAGS, devptr) < 0) {
           /* HINT(LOG_CONN, "Error: Unable to get device interface flags.\n"); */
           goto complete_and_return;
        }
        
        if ((devptr->ifr_flags & IFF_LOOPBACK) != 0) {
           have_loopback = 1;
           continue;
	}
        
        if ((devptr->ifr_flags & IFF_UP) == 0)
           continue;
        
        if ((devptr->ifr_flags & IFF_BROADCAST) == 0)
           continue;
        
        *pAddr = *(struct sockaddr_in *) &(devptr->ifr_addr); 
        {
           struct sockaddr_in x, *y;
           y=(struct sockaddr_in *)&devptr->ifr_addr;
           x=(*y);
        }
        
        return 2;
   }
   
 complete_and_return:
   /* HINT(LOG_CONN, "nothing here %d devices \n", numdevs ); */
   close(sd);
   return have_loopback;
}

int get_ip_address(char *szHostName)
{
   struct in_addr loopback, out;
   struct hostent *pHost;
   int i;

   /* removed by the compiler if --enable-debug was not provided to the configure script */
   loopback.s_addr = htonl(INADDR_LOOPBACK);
   if ((pHost = gethostbyname(szHostName)) != NULL) {
      for (i = 0; pHost->h_addr_list[i] != NULL; i++) {
         /* find the first one != local addresses (127.x.x.x) */
         memcpy(&(out.s_addr), pHost->h_addr_list[i], pHost->h_length);
         if (out.s_addr != loopback.s_addr) {
            return out.s_addr;
         }
      }
   } 
   return 0;
}

int get_my_addr( char** pdefault_name )
{
   char szHostName[512];
   int rc;
   struct sockaddr_in ip;
   int len;

   if( default_name != NULL ) {
      if( pdefault_name != NULL ) {
         *pdefault_name = _MALLOC( strlen(default_name) + 1 );
         sprintf( *pdefault_name, "%s", default_name );
      }
      return host_addr;
   }
   if( (pdefault_name != NULL) && ((*pdefault_name) != NULL) ) {
      host_addr = get_ip_address( *pdefault_name );
      if( host_addr != 0 ) {
         if( default_name != NULL ) _FREE( default_name );
         default_name = _MALLOC( strlen( *pdefault_name ) + 1 );
         sprintf( default_name, "%s", *pdefault_name );
         return host_addr;
      }
   }
   if (gethostname(szHostName, 512) == 0) {
      /* HINT(LOG_CONN,"hostname is %s\n",szHostName); */
      host_addr = get_ip_address(szHostName);
      if(host_addr != 0) goto update_name;
        
      /* try FQDN */
      len = strlen(szHostName);
      szHostName[len] = '.';
      if (getdomainname(szHostName + len + 1, 512 - len) != 0) {
         /* HINT(LOG_CONN,"Unable to get the domain: %s\n",strerror(errno)); */
         goto complex_way;
      }
      /* HINT(LOG_CONN, "Complete host name is %s\n", szHostName ); */
      host_addr = get_ip_address(szHostName);
      if( host_addr != 0)
         goto update_name;
   }

 complex_way:
   /* Let's try the complex way */
   rc = find_ip_on_all_interfaces(&ip);
   switch(rc) {
   case 2: 
      sprintf( szHostName, "%s", inet_ntoa( ip.sin_addr ) );
      host_addr = ip.sin_addr.s_addr;
      break;
   case 1:
      sprintf( szHostName, "%s", "127.0.0.1" );  /* hey why not !! */
      (void)inet_aton( "127.0.0.1", (struct in_addr*)&host_addr );
      break;
   default:
      return -1;
   }
 update_name:
   if( pdefault_name != NULL ) {
      if( (*pdefault_name) != NULL ) free( (*pdefault_name) );
      *pdefault_name = _MALLOC( strlen(szHostName) + 1 );
      sprintf( *pdefault_name, "%s", szHostName );
   }
   default_name = _MALLOC( strlen(szHostName) + 1 );
   sprintf( default_name, "%s", szHostName );
   return host_addr;
}

int free_default_name(void)
{
   if(default_name) _FREE(default_name);
   return 0;
}

/**
 * getHostnameByAddr - getHostname from IP(v4)
 * @param addr address
 * @return hostname
 * */
char * getHostnameByAddr(int addr)
{
   struct hostent *hp;
   struct in_addr saddr;
   saddr.s_addr=addr;
   hp = gethostbyaddr((char *) &addr, sizeof(addr), AF_INET);
   if (hp==NULL) {
      return inet_ntoa(saddr);
   } else {
#ifdef ENABLE_ALIASES    /* Enable alias hostname */
      if(hp->h_aliases[0]!=NULL) {
         return hp->h_aliases[0];
      }
#endif
      if(hp->h_name !=NULL) {
         return hp->h_name;
      } else {
         return inet_ntoa(saddr);
      }
   }
}
