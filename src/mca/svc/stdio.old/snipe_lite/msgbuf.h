/*
 * $HEADER$
 */

/*
	HARNESS G_HCORE
	HARNESS FT_MPI

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

#ifndef _GHCORE_MSGBUF_H
#define _GHCORE_MSGBUF_H 1



/*
	message packing/unpacking routines

*/


/* Uncomment the following line to enable msgbuf V2.0 */
#define MSG_BUF_V2




#ifdef MSG_BUF_V2

#define _msg_resize_buf     _hn_msg_resize_buf
#define init_msg_bufs       hn_init_msg_bufs
#define dump_msg_bufs        hn_dump_msg_bufs
#define check_buf           hn_check_buf
#define get_msg_buf_info    hn_get_msg_buf_info 
#define get_msg_buf         hn_get_msg_buf
#define set_unpksize        hn_set_unpksize 
#define get_msg_buf_of_size hn_get_msg_buf_of_size
#define free_msg_buf        hn_free_msg_buf
#define end_msg_buf         hn_end_msg_buf

#define pk_int8             hn_pk_int8
#define pk_int16            hn_pk_int16 
#define pk_int32            hn_pk_int32 
#define pk_raw32            hn_pk_raw32 
#define pk_int64            hn_pk_int64
#define pk_int128           hn_pk_int128
#define pk_real32           hn_pk_real32
#define pk_real64           hn_pk_real64
#define pk_byte             hn_pk_byte
#define pk_string           hn_pk_string

#define upk_int8            hn_upk_int8 
#define upk_int16           hn_upk_int16 
#define upk_int32           hn_upk_int32 
#define upk_raw32           hn_upk_raw32 
#define upk_int64           hn_upk_int64
#define upk_int128          hn_upk_int128
#define upk_real32          hn_upk_real32
#define upk_real64          hn_upk_real64
#define upk_byte            hn_upk_byte
#define upk_string          hn_upk_string


#endif










int init_msg_bufs ();		/* must call first */
int	get_msg_buf_info (int buf,char ** base,int * len);	/* get info on buf */
int	get_msg_buf (int resizable );			/* get a free buf */
int	get_msg_buf_of_size (unsigned long reqsize,int resize,int setunpksize);	/* get a free buf that is a particular size */
int free_msg_buf (int buf);		/* free a buf */
int check_buf (int buf);			/* valid buf ? */
int dump_msg_bufs ();		/* dump msg buf information */
int end_msg_buf ();			/* free ALL bufs */

/* pack and unpack binary data */
int		pk_byte (int buf,void * ptr,long n);
int		upk_byte (int buf,void * ptr,long n);
int     pk_raw32 (int buf,void * ptr,long n);
int     upk_raw32 (int buf,void * ptr,long n);

/* pack basic integer types */

int     pk_int8 (int buf,void * ptr,long n);
int     pk_int16 (int buf,void * ptr,long n);
int     pk_int32 (int buf,void * ptr,long n);
int     pk_int64 (int buf,void * ptr,long n);
int     pk_int128 (int buf,void * ptr,long n);

/* pack real numbers */
int     pk_real32 (int buf,void * ptr,long n);
int     pk_real64 (int buf,void * ptr,long n);

/* unpack basic integer types */

int     upk_int8 (int buf,void * ptr,long n);
int     upk_int16 (int buf,void * ptr,long n);
int     upk_int32 (int buf,void * ptr,long n);
int     upk_int64 (int buf,void * ptr,long n);
int     upk_int128 (int buf,void * ptr,long n);

/* unpack real numbers */
int     upk_real32 (int buf,void * ptr,long n);
int     upk_real64 (int buf,void * ptr,long n);

/* handles strings */
/* note this takes NULL terminated strings and returns null terminated strings */

int     pk_string (int buf,char *strptr);
int     upk_string (int buf,void * strptr,long maxlen);

/* error codes (i.e. rather than just -1) */
#define BADBUFFER	-2010				/* bad buffer id */
#define OUTOFBUFFERS	-2011			/* no free msg buffers free */
#define OUTOFMEMORY	-2012				/* cannot allocate any more memory for message buffers */
#define BADLUCK	-13					/* no comment */
#define BADPARM	-2014					/* other parameters are bad/invalid pointers */
#define BADDATA	-2015					/* to/from data addr is bad (null etc) */


#endif /* _GHCORE_MSGBUF_H */
