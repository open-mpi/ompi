
/*
	HARNESS G_HCORE

	Innovative Computer Laboratory,
	University of Tennessee,
	Knoxville, TN, USA.

	harness@cs.utk.edu

 --------------------------------------------------------------------------

 Authors:	
			Graham E Fagg <fagg@cs.utk.edu>

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

#include "msgbuf.h"
#include "msg.h"
#ifdef WIN32
#include "wincomm.h"
#else
#include "snipe_lite.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>

/* Note this is a simple (bad) description of a basic header */
/*
	0-3		PKMESSAGEVER	used to make sure its a packed message.. so if its not, then drop it.... if we can.

	4-7 	PACKEDMESSAGELEN	length of packed data in octets	(can be zero but not negative)
	8-11	SENDERS ID			arbitary id that they specify
	12-15	NUMBER OF TAGS		if no tags then set to zero
	16-((4*ntag)-1)		individual tags ...
	16+(4*ntag)-		Start of packed data.

*/

/* Note to avoid using writeV etc we get a message buffer of our own JUST for packing stuff in and out of */
/* we don't use the msgbuf routines as that effects cache coherency */

char hdrbuf [4096];


/* this routine sends a message down a socket as a complete message */
/* this includes a simple header (sender, tag, length) */
/* if the free flag is set then the message buffer is freed */
/* if free is set then even if its not send it is still freed always */

/* return value is either the total length sent or an error */

/* if the complete amount is not sent then the buffer is not freed */

int send_pkmesg (int send_s,int  myid,int  ntag,int * tagp,int  buf_id,int  free_buf)
/* int send_s;		 socket to send_s */
/* int myid;		 ID I place in message header */
/* int ntag;		 Number of Tags in header */
/* int *tagp;		 pointer to Tags to put in header */
/* int buf_id;		 message buffer I am sending or if you want to send an EMPTY message send EMPTYMSGBUF */
/* int free_buf;	 non zero if you want to free the buffer up after a good send */
{
  int len = 0;
  char * buf_ptr;
  int ret = -1;
  int *data;
  int valid;
  int i;


	if (buf_id != EMPTYMSGBUF) valid = check_buf (buf_id);
	else valid = 1;

	if (!valid) return (BADBUFFER);

	if (buf_id != EMPTYMSGBUF) 
		get_msg_buf_info(buf_id,&buf_ptr,&len);	/* get buffer info */
	else {
		buf_ptr = NULL;							/* if empty message */
		len = 0;
	}
		
	data = (int*) hdrbuf;
	
	*data++ = htonl (PKMESSAGEVER);
	*data++ = htonl (len);
	*data++ = htonl (myid);
	*data++ = htonl (ntag);

	for (i=0;i<ntag;i++) {			/* pack the tags */
		*data++ = htonl (tagp[i]);
	}

#ifdef VERBOSE
  PRINT("TRYING TO SEND %d %d - size %d of buf %d\n",myid,ntag,len, buf_id);
#endif

  while(ret < 1){
    ret = writeconn(send_s,(char*) hdrbuf,sizeof(int)*(4+ntag));
    if(ret <= 0){
	  if ((buf_id != EMPTYMSGBUF)&&(free_buf)) free_msg_buf(buf_id);
      return(ret);
    }

    if(len > 0){
    ret = writeconn(send_s,buf_ptr,len);
      if(ret <= 0){
	    if ((buf_id != EMPTYMSGBUF)&&(free_buf)) free_msg_buf(buf_id);
        return(ret);
      } 
	  else 
		if ((buf_id != EMPTYMSGBUF)&&(free_buf)) free_msg_buf(buf_id);
    }
  }  
  return(ret);
}


/* recv a packed message and return the buffer of the message / size etc */
/* This route will allocate a buffer if it is needed (i.e. if it has a */
/* payload). */

/* If there isn't enought space for all the tags it drops tags */
/* ntags can be NULL or 0 */
/* if there is a protocol error i.e. protocol versions don't match it bombs */

int recv_pkmesg (int recv_s,int * from,int * ntag,int * tagp,int * buf_id)
{
  int len;
  char * buf_ptr = NULL;
  int *hdata;
  int *hdata2;
  int size;
  int ret;
  int client;
  int i, j;
  int tmp;
  int rtags;	/* how many tags to receive even if it drops some */
  int tbid;		/* temp buf id */

#ifdef VERBOSE
  PRINT("Atempting to receive from %d via socket %d\n",recv_s, from);
#endif 


	/* download the data into the header buffer */
	hdata = (int*) hdrbuf;
	hdata2 = (int*) hdrbuf;
  
	ret = readconn(recv_s,(char*)hdata,sizeof(int)*4);
	if(!ret){
		return(ret);
	}

	tmp = ntohl(hdata[0]);
	if (tmp != PKMESSAGEVER) {	/* Header check */
	#ifdef VERBOSE
		printf("Recv_pkmesg has received a message with an incorrect hdr [0x%x]\n", hdata[0]);
	#endif
		return (MSG_BAD_HEADER);
	}

	size = ntohl(hdata[1]);
	client = ntohl(hdata[2]);
  	rtags = ntohl(hdata[3]);

#ifdef VERBOSE
	printf("header 0x%x size %d from %d ntags %d\n",
			tmp, size, client, rtags);
#endif

	/* now we have a copy of the header we can overwrite the headerbuffer */

	/* download the message tags into the header buffer */
	if (rtags>0) {
		hdata2 = (int*) hdrbuf;
		ret = readconn(recv_s,(char*)hdata2,sizeof(int)*rtags);
		if(!ret){
			return(ret);
		}
	}

	/* ok copy over the ones we have been given space for */
	/* check to see if we are even excepting tags first ! */
	if (ntag) {
		j = *ntag;
		if (j>rtags) j=rtags;

		for(i=0;i<j;i++) {
			*tagp = ntohl(*hdata2);
			tagp++;
			hdata2++;
		}

				/* NOTE we only do the conversion on the ones we keep. Saves a few usecs? */
	
		/* return the number of tags copied down */
		*ntag = j;
		}
	else 
		j = 0;	/* no tags copied */

	/* copy across the return info */

	/* senders id */
	if (from) *from = client;

	/* if no data (payload) to copy in, return outa here */ 
	if(size == 0){
		if (buf_id) *buf_id = EMPTYMSGBUF;
		return(0);
	}

	/* get a buffer for the incoming data */
	/* the buffer must be resizable */
	/* i.e. we need a buffer, resize one if needed */
	/* Also we reset the unpack len (toend) value automatically on it */

	tbid = get_msg_buf_of_size(size, 1, 1);

	get_msg_buf_info(tbid, &buf_ptr, &len);		/* get the buffers ptr */

	ret = readconn(recv_s, buf_ptr, size);		/* read socket into buffer */

	/* now we need to return the buffer id */
	/* if its been given a NULL pointer we blow it away */

	if (!buf_id) { /* a null return address! */
	#ifdef VERBOSE
		printf("Recv_pkmesg has a valid buffer but a NULL return address!\n");
	#endif 
		free_msg_buf (tbid);	/* blow it away */
	}
	else
		*buf_id = tbid;			/* return it */

	return(ret);				/* return read data length */
} 

