
/*
	HARNESS G_HCORE
	HARNESS FT_MPI

	Innovative Computer Laboratory,
	University of Tennessee,
	Knoxville, TN, USA.

	harness@cs.utk.edu

 --------------------------------------------------------------------------

 Authors:	
			Thara Angskun <angskun@cs.utk.edu>

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


/* msg buffer handling routines */

/*
	message packing/unpacking routines

*/



#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "msgbuf.h"
#include "debug.h"

/****************************/
/* message buffer version 2 */
/****************************/

/* Message buffer V2 should called with prefix "hn" */
/* to avoid conflict with other program such as intel testsuite */


/*** WARNING!!! :  (for backward compatability) */

/* some function return value >=0 is success, negative number is error  */
/* some function return value >=1 is success, 0 and negative number is error  */
/* Please see comment above each function */

/* OK, Let's me put it this way....  Double standard ? ;-) */

/* The following function, negative number is error */
/* - _hn_msg_resize_buf   */
/* - hn_dump_msg_buf      */
/* - hn_set_unpksize      */
/* - hn_get_msg_buf_of_size  */
/* - hn_free_msg_buf  */
/* - hn_end_msg_buf  */

/* The following function, zero and negative number is error */
/* - hn_init_msg_buf      */
/* - hn_check_buf */
/* - hn_get_msg_buf_info */
/* - hn_pk_* */
/* - hn_upk_* */

typedef struct {
   void* base_ptr;	/* start of my memory */
   void* data_ptr;	/* location of where next data will go */
   void* from_ptr;	/* location of where to get the next data from */
   long	size;		/* size of buffer */
   long	len;		/* total amount already packed */
   long	space;		/* size - len */
   long	toend;		/* how many bytes till the end when unpacking :) */
} msg_cb_t;

int num_entry=0;                  /* current number of entry */
int lowest_free_entry=0;          /* lowest entry free slot */
int msg_convert_type;           /* message conversion type */
int msg_buf_init_called = 0;    /* do we call init? */
msg_cb_t *msg_buf;




/**
_hn_msg_resize_buf - resize buffer (Internal)
@param buf buffer ID
@param incsize increase size
@retval <0 error
@param >=0 bufID
*/
int	_hn_msg_resize_buf (int buf,long incsize )
{

char *p;
char *q;
char *oldp;
long oldsize;
long oldlen;
long oldspace;
long difffrom;	/* if this has been read from at all, whats its offset */
long reqsize;

/*        printf("[%s:%d] Incsize is %ld\n",__FILE__,__LINE__,incsize); fflush(stdout); */

	/* remember what we are doing */
	oldp = (char*)msg_buf[buf].base_ptr;
	oldsize = msg_buf[buf].size;
	oldlen = msg_buf[buf].len;
	oldspace = msg_buf[buf].space;
	difffrom  = ((char*)msg_buf[buf].from_ptr - (char*)msg_buf[buf].base_ptr);


	reqsize = oldsize + incsize;

        p = _REALLOC(oldp,reqsize); 
        /* realloc is faster than malloc and loop pointer in case that we don't need to reallocate new space */
	/* Time on pack 10000 integer, realloc = 3052 micro sec, malloc + loop = 1915754 micro sec */

 #ifdef OLDMALLOC	 

    p = (char*) _MALLOC (reqsize);
    if (!p) {
        /* ops, we have a problem houston */
        return (-1);
        }

    /* else we have the memory, so lets do it */

	/* first copy the data over */

	q = p;	/* make a copy of the new start pointer */

	memcpy( q, oldp, oldlen );
   if(msg_buf[buf].base_ptr) _FREE (msg_buf[buf].base_ptr); /* free the old memory first */

 #endif 

    /* note, q = location of next free location in the new object */
    q = p + oldlen;

    msg_buf[buf].base_ptr = p;
    msg_buf[buf].data_ptr = q;
    msg_buf[buf].from_ptr = (char*)msg_buf[buf].base_ptr + difffrom;
    msg_buf[buf].size = reqsize;
    msg_buf[buf].len = oldlen;
    msg_buf[buf].space = oldspace + incsize;

    return (buf);
}

/**
hn_init_msg_bufs - Initialize message buffer 
*/
int hn_init_msg_bufs(void)
{
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif

    if (msg_buf_init_called) return 1;	
    else msg_buf_init_called = 1;

#if defined (IMA_SUN4) || defined (IMA_SUN4SOL2) || defined (IMA_JAVA) || defined (IMA_AIX4) || defined (IMA_RS6K) || defined (IMA_SGI6) || defined (IMA_SP2)
    msg_convert_type = 0;
#else
    msg_convert_type	= 1;
#endif
    return 1;
}

/**
hn_dump_msg_bufs - Display message buffer 
*/
int hn_dump_msg_bufs () 
{
    int i;
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif

    puts("----------------------------------------");
    puts("Internal Message Buffer Information Dump");
    puts("----------------------------------------");

    if(num_entry > 0) {
        printf("  ID\tBase\tSize\tLength\tSpace\tToEnd\n");
        for(i=0;i<num_entry;i++) {
            printf("  %2d\t",i);
            printf("%lx\t",(unsigned long int)msg_buf[i].base_ptr);
            printf("%ld\t",msg_buf[i].size);
            printf("%ld\t",msg_buf[i].len);
            printf("%ld\t",msg_buf[i].space);
            printf("%ld\n",msg_buf[i].toend);
        }
    }

    puts("");
    puts("----------------------------------------");
    printf("num_entry [%d] Lowest free entry [%d]\n", num_entry,lowest_free_entry);
    puts("----------------------------------------");
    return 0;
}

/**
hn_check_buf_basic - check basic buffer validity
@param buf buffer ID
@retval 0 Invalid buffer
@retval 1 Valid buffer
*/
int hn_check_buf_basic (int buf)
{
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s (buf %d)\n",__FILE__,__LINE__,__FUNCTION__,buf);
#endif

    if (buf<0) {
#ifdef VERBOSE
        printf("[%s:%d] buffer ID %d less than 0\n",__FILE__,__LINE__,buf);
	fflush(stdout);
#endif
        return (0);
    }
    if (buf>=num_entry) {
#ifdef VERBOSE
        printf("[%s:%d] buffer ID more than or equal number of entry\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }
    return (1);
}


/**
hn_check_buf - check buffer validity
@param buf buffer ID
@retval 0 Invalid buffer
@retval 1 Valid buffer
*/
int hn_check_buf (int buf)
{
    if (hn_check_buf_basic(buf) <=0) return 0;

    if (msg_buf[buf].size <= 0) {
#ifdef VERBOSE
        printf("[%s:%d] This buffer doesn't use anymore (buf %d,size %d)\n",__FILE__,__LINE__,buf,msg_buf[buf].size);
	fflush(stdout);
#endif
	return (0);
    }
    return (1);
}

/**
hn_get_msg_buf_info - get message buffer information 
@param buf buffer ID
@param base base memory address [OUTPUT]
@param len buffer length [OUTPUT]
@retval 1 success
@retval 0 error 
*/
int hn_get_msg_buf_info (int buf,char ** base,int * len)
{
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif

    if (!base) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid base parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }
    if (!len) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid len parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }
    *base = (char*) msg_buf[buf].base_ptr;
    *len  = msg_buf[buf].len;
    return (1);
}

/**
hn_set_unpksize - set unpack size
@param buf buffer ID
@param reqsize request size
@retval 0 success
@retval -1 error
*/
int hn_set_unpksize (int buf,int reqsize) 
{
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return -1;
    }
    if (reqsize < 0) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid reqsize parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return -1;
    }
    msg_buf[buf].toend = reqsize;
    return 0;
}

/**
hn_get_msg_buf_of_size - get message buffer of specify size
@param reqsize request size
@param resize [OBSOLETE - for backward compatibility only!!] 
@param setunpacksize - set unpack size (toend) to the request size 
@retval >=0  buffer ID 
@retval <0 error 
*/
int hn_get_msg_buf_of_size (unsigned long reqsize,int resize,int setunpksize)
{
    int bufid;
    int j;
    char * p;
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif

    if (reqsize <= 0) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid reqsize parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return -1;
    }

    if(lowest_free_entry==num_entry) {
        bufid=num_entry;	
        num_entry++;
	lowest_free_entry++;
	if(bufid==0) { 
            msg_buf=(msg_cb_t *)_MALLOC(sizeof(msg_cb_t));
	} else {
            msg_buf=(msg_cb_t *)_REALLOC(msg_buf,sizeof(msg_cb_t)*num_entry);
	}
    } else {
        bufid=lowest_free_entry;
    }

    p = (char *)_MALLOC(reqsize);
    if (!p) {
#ifdef VERBOSE
        printf("[%s:%d] Cannot allocate memory !!\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (-1);
    }

    /*
    if(msg_buf[bufid].base_ptr) _FREE (msg_buf[bufid].base_ptr);	
    */

    msg_buf[bufid].base_ptr = p;
    msg_buf[bufid].data_ptr = p;
    msg_buf[bufid].from_ptr = p;
    msg_buf[bufid].size = reqsize;
    msg_buf[bufid].len = 0;
    msg_buf[bufid].space = reqsize;

    for(j=lowest_free_entry;j<num_entry;j++) {
        if(msg_buf[j].size==0) {
           break;
	}
    }
    lowest_free_entry = j;

    if (setunpksize) msg_buf[bufid].toend = reqsize;
    else msg_buf[bufid].toend = 0;
#ifdef VERBOSE
        printf("[%s:%d] We get buf ID %d \n",__FILE__,__LINE__,bufid);
	fflush(stdout);
#endif
    return (bufid);
}

/**
hn_get_msg_buf - get message buffer
@param resize [OBSOLETE - for backward compatibility only!!] 
@retval >=0  buffer ID 
@retval <0 error 
*/
int hn_get_msg_buf (int resizable )
{
    /*  just return 1 byte buffer for now */ 
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    return hn_get_msg_buf_of_size(10,resizable,0);
}


/**
hn_free_msg_buf - Free message buffer
@param buf buffer ID
@param <0 error
@param >=0 success
*/
int hn_free_msg_buf (int buf)
{

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf_basic(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return -1;
    }
    if(buf < lowest_free_entry) lowest_free_entry = buf;

    if(msg_buf[buf].base_ptr!=NULL) {
       _FREE(msg_buf[buf].base_ptr);
       msg_buf[buf].base_ptr=NULL;
    }

    msg_buf[buf].data_ptr = NULL;
    msg_buf[buf].from_ptr = NULL;
    msg_buf[buf].len = 0;
    msg_buf[buf].size = 0;
    msg_buf[buf].space = 0;
    msg_buf[buf].toend = 0;

    return (buf);
}

/**
hn_end_msg_buf - End message buffer
@param buf buffer ID
@param <0 error
@param >=0 success
*/
int hn_end_msg_buf (void)
{
    int i;
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    for (i=0;i<num_entry;i++) {
	hn_free_msg_buf (i);
    }
    if( msg_buf != NULL ) _FREE(msg_buf); 
    msg_buf = NULL;
    return 0;
}


/*********************** PACKED STUFF ***********************/


/**
hn_pk_int8 - pack 8 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_int8 (int buf,void * ptr,long n) 
{
    int i;
    char *p;
    char *q;
    int s;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 1*n;

    if (msg_buf[buf].space < s) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, s);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }

    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    /* ok have space.. so do it */
    /* No msg_convert_type check for bytes */
	
    for(i=0;i<s;i++) *p++=*q++;	/* note we step through here 's' times not 'n' times */	

    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;
    return (s);
}



/**
hn_pk_int16 - pack 16 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_int16 (int buf,void * ptr,long n)
{
int i;
char *p;
char *q;
int s;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 2*n;

    if (msg_buf[buf].space < s) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, s);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }

    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    /* ok have space.. so do it */
    if (msg_convert_type) {
	for (i=0;i<n;i++) { /* Note we step through here 'n' times not 's' times */
	    *p++ = *(q+1);
	    *p++ = *q;
	    q+=2;
	}
    } else {
	for(i=0;i<s;i++) *p++=*q++;	/* note we step through here 's' times not 'n' times */
    }

    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;
    return (s);
}


/**
hn_pk_int32 - pack 32 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_int32 (int buf,void * ptr,long n)
{
    int i;
    char *p;
    char *q;
    int s;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 4*n;

    if (msg_buf[buf].space < s) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, s);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }
    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    /* ok have space.. so do it */
    if (msg_convert_type) {
	for (i=0;i<n;i++) { /* Note we step through here 'n' times not 's' times */
           *p++ = *(q+3);
	   *p++ = *(q+2);
	   *p++ = *(q+1);
	   *p++ = *q;
	   q+=4;
	}
    } else {
	for(i=0;i<s;i++) *p++=*q++;	/* note we step through here 's' times not 'n' times */
    }

    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;
    return (s);
}


/**
hn_pk_raw32 - No data conversion version of pk_int32
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_raw32 (int buf,void * ptr,long n)
{
int i;
char *p;
char *q;
int s;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 4*n;

    if (msg_buf[buf].space < s) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, s);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }

    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    for(i=0;i<s;i++) *p++=*q++;	

    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;
    return (s);
}


/**
hn_pk_int64 - pack 64 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_int64 (int buf,void * ptr,long n) 
{
     int i;
     char *p;
     char *q;
     int s;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 8*n;

    if (msg_buf[buf].space < s) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, s);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }
   
    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    if (msg_convert_type) {
        for (i=0;i<n;i++) {
            *p++ = *(q+7);
            *p++ = *(q+6);
            *p++ = *(q+5);
            *p++ = *(q+4);
            *p++ = *(q+3);
            *p++ = *(q+2);
            *p++ = *(q+1);
            *p++ = *q;
            q+=8;
        }
    } else {
        for(i=0;i<s;i++){
            *p++=*q++;
        }
    }  
   
    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;
    return (s);
}


/**
hn_pk_int128 - pack 128 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_int128 (int buf,void * ptr,long n) 
{
    int i;
    char *p;
    char *q;
    int s;
   
#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 16*n;

    if (msg_buf[buf].space < s) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, s);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }

    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    if (msg_convert_type) {
        for (i=0;i<n;i++) {
            *p++ = *(q+15);
            *p++ = *(q+14);
            *p++ = *(q+13);
            *p++ = *(q+12);
            *p++ = *(q+11);
            *p++ = *(q+10);
            *p++ = *(q+9);
            *p++ = *(q+8);
            *p++ = *(q+7);
            *p++ = *(q+6);
            *p++ = *(q+5);
            *p++ = *(q+4);
            *p++ = *(q+3);
            *p++ = *(q+2);
            *p++ = *(q+1);
            *p++ = *q;
            q+=16;
        }
    }  else {
        for(i=0;i<s;i++){
            *p++=*q++;
        }
    }  

    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;
    return (s);
}


/**
hn_pk_real32 - pack 32 bits real
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_real32 (int buf,void * ptr,long n)
{
    int i;
    char *p;
    char *q;
    int s;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 4*n;

    if (msg_buf[buf].space < s) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, s);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }
   
    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    if (msg_convert_type) {
        for (i=0;i<n;i++) {
            *p++ = *(q+3);
            *p++ = *(q+2);
            *p++ = *(q+1);
            *p++ = *q;
            q+=4;
        }
    } else {
        for(i=0;i<s;i++){
           *p++=*q++;
        }
    }  
   
    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;
    return (s);
}

/**
hn_pk_real64 - pack 64 bits real
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_real64 (int buf,void * ptr,long n)
{
    int i;
    char *p;
    char *q;
    int s;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 8*n;

    if (msg_buf[buf].space < s) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, s);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }
   
    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    if (msg_convert_type) {
        for (i=0;i<n;i++) {
            *p++ = *(q+7);
            *p++ = *(q+6);
            *p++ = *(q+5);
            *p++ = *(q+4);
            *p++ = *(q+3);
            *p++ = *(q+2);
            *p++ = *(q+1);
            *p++ = *q;
            q+=8;
	}
    } else {
        for(i=0;i<s;i++){
           *p++=*q++;
        }
    }  
   
    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;
    return (s);
}

/**
hn_pk_byte - Yes, it's pack byte. nothing else
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_pk_byte(int buf,void * ptr,long n)
{
    int i;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    if (msg_buf[buf].space < n) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, n);	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }
  
    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) ptr;

    for(i=0;i<n;i++){
        *p++=*q++;
    }
  
    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += n;
    msg_buf[buf].space -= n;
    msg_buf[buf].toend += n;

    return (n);
}

/**
hn_pk_string - pack string
@param buf buffer ID
@param strptr string pointer
@param 0 error
@param >0 success
*/
int hn_pk_string (int buf,char *strptr) 
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    if(!strptr) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid strptr parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0); 
    }

    /* What's happen if the string is not NULL terminated?. Yep.. call 911 */

    s = strlen (strptr);
    if(s<=0) {
#ifdef VERBOSE
            printf("[%s:%d] The string is not NULL terminated or NULL string\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); 
    }

#ifdef VERBOSE9
    printf("[%s:%d] If you see this number -> [%d] \"strange\". Your string is not NULL terminated. \n",__FILE__,__LINE__,s);
    fflush(stdout);
#endif

    /* plus 4 for the lenght of string */

    if (msg_buf[buf].space < (s+4)) {       /* not enough space */
	i = _hn_msg_resize_buf (buf, (s+4));	/* request a resize on it */
	if (i<0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to resize\n",__FILE__,__LINE__);
	    fflush(stdout);
#endif
	return (0); /* i.e. it failed to resize, so no data packed */
	}
    }

    if(hn_pk_int32 (buf, &s, 1) <=0) {
#ifdef VERBOSE
            printf("[%s:%d] Failed to pack lenght of string [%d]\n",__FILE__,__LINE__,s);
	    fflush(stdout);
#endif
	return (0); 
    }

    p = (char *) msg_buf[buf].data_ptr;
    q = (char *) strptr;

    /* memcopy oneday */
    for(i=0;i<s;i++) *p++ = *q++;

    /* note we don't update the buf by s+4 as the +4 has been done by the pkint strlen */
    msg_buf[buf].data_ptr = (void *) p;
    msg_buf[buf].len += s;
    msg_buf[buf].space -= s;
    msg_buf[buf].toend += s;

    /* but we did pack (s+4) bytes */
    return (s+4);
}

/*********************** UN-PACKED STUFF ***********************/


/**
hn_upk_int8 - unpack 8 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_int8 (int buf,void * ptr,long n) 
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 1*n;

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0); 
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    /* ok unpack it.. make it so */
    if (msg_convert_type) {
	for (i=0;i<n;i++) {	/* le duff device el GEF style */
						/* note we step through here 'n' times only */
            *p++ = *(q+3);
	    *p++ = *(q+2);
	    *p++ = *(q+1);
	    *p++ = *q;
	    q+=4;
	}
    } else {
	for(i=0;i<s;i++) *p++=*q++;	/* note we step through here 's' times not 'n' times */
    }

    /* update buf info */
    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= s;

    return (s);
}

/**
hn_upk_int16 - unpack 16 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_int16 (int buf,void * ptr,long n) 
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 2*n;

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    /* ok unpack it.. make it so */
    if (msg_convert_type) {
	for (i=0;i<n;i++) {	/* le duff device el GEF style */
	                        /* note we step through here 'n' times only */
		*p++ = *(q+1);
		*p++ = *q;
		q+=2;
	}
    } else {
	for(i=0;i<s;i++) *p++=*q++;	/* note we step through here 's' times not 'n' times */
    }

    /* update buf info */
    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= s;

    return (s);
}

/**
hn_upk_int32 - unpack 32 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_int32 (int buf,void * ptr,long n) 
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 4*n;

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    /* ok unpack it.. make it so */
    if (msg_convert_type) {
	for (i=0;i<n;i++) {	/* le duff device el GEF style */
						/* note we step through here 'n' times only */
		*p++ = *(q+3);
		*p++ = *(q+2);
		*p++ = *(q+1);
		*p++ = *q;
		q+=4;
	}
    } else {
	for(i=0;i<s;i++) *p++=*q++;	/* note we step through here 's' times not 'n' times */
    }

    /* update buf info */
    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= s;

    return (s);
}

/**
hn_upk_raw32 - No data conversion version of upk_int32
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_raw32 (int buf,void * ptr,long n) 
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 4*n;

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    /* ok unpack it.. make it so */
    /* note we step through here 's' times not 'n' times */
    for(i=0;i<s;i++) *p++=*q++;	

    /* update buf info */
    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= s;

    return (s);
}

/**
hn_upk_int64 - unpack 64 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_int64 (int buf,void * ptr,long n) 
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 8*n;

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    /* ok unpack it.. make it so */
    if (msg_convert_type) {
	for (i=0;i<n;i++) {	/* le duff device el GEF style */
						/* note we step through here 'n' times only */
      	*p++ = *(q+7);
      	*p++ = *(q+6);
      	*p++ = *(q+5);
      	*p++ = *(q+4);
		*p++ = *(q+3);
		*p++ = *(q+2);
		*p++ = *(q+1);
		*p++ = *q;
		q+=8;
	}
    } else {
	for(i=0;i<s;i++) *p++=*q++;	/* note we step through here 's' times not 'n' times */
    }

    /* update buf info */
    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= s;
    return (s);
}

/**
hn_upk_int128 - unpack 128 bits integer
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_int128 (int buf,void * ptr,long n) 
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 16*n;

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    /* ok unpack it.. make it so */
    if (msg_convert_type) {
	for (i=0;i<n;i++) {	/* le duff device el GEF style */
						/* note we step through here 'n' times only */
      	*p++ = *(q+15);
      	*p++ = *(q+14);
      	*p++ = *(q+13);
      	*p++ = *(q+12);
      	*p++ = *(q+11);
      	*p++ = *(q+10);
      	*p++ = *(q+9);
      	*p++ = *(q+8);
      	*p++ = *(q+7);
      	*p++ = *(q+6);
      	*p++ = *(q+5);
      	*p++ = *(q+4);
	*p++ = *(q+3);
	*p++ = *(q+2);
	*p++ = *(q+1);
	*p++ = *q;
	q+=16;
	}
    } else {
	for(i=0;i<s;i++) *p++=*q++;	/* note we step through here 's' times not 'n' times */
    }

    /* update buf info */
    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

   msg_buf[buf].from_ptr = p;
   msg_buf[buf].toend -= s;

   return (s);
}

/**
hn_upk_real32 - unpack 32 bits real
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_real32 (int buf,void * ptr,long n)
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 4*n;

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    if (msg_convert_type) {
        for (i=0;i<n;i++) { 
            *p++ = *(q+3);
            *p++ = *(q+2);
            *p++ = *(q+1);
            *p++ = *q;
            q+=4;
        }
    } else {
        for(i=0;i<s;i++) {
            *p++=*q++;
        }
    }

    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= s;
    return (s);
}

/**
hn_upk_real64 - unpack 64 bits real
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_real64 (int buf,void * ptr,long n)
{
    int i;
    int s;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    s = 8*n;

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    if (msg_convert_type) {
        for (i=0;i<n;i++) { 
            *p++ = *(q+7);
            *p++ = *(q+6);
            *p++ = *(q+5);
            *p++ = *(q+4);
            *p++ = *(q+3);
            *p++ = *(q+2);
            *p++ = *(q+1);
            *p++ = *q;
            q+=8;
        }
    } else {
        for(i=0;i<s;i++) {
           *p++=*q++;
        }
    }

    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= s;
    return (s);
}


/**
hn_upk_byte - unpack byte
@param buf buffer ID
@param ptr pointer
@param n number of integer
@param 0 error
@param >0 success
*/
int hn_upk_byte(int buf,void * ptr,long n)
{
    int i;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    if (msg_buf[buf].toend < n) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    p = (char *) ptr;
    q = (char *) msg_buf[buf].from_ptr;

    for(i=0;i<n;i++) {
        *p++=*q++;
    }

    p = (char *) msg_buf[buf].from_ptr;
    p+= n;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= n;
    return (n);
}


/**
hn_upk_byte - unpack string
@param buf buffer ID
@param strptr string pointer
@param maxlen maximun length of buffer
@param 0 error
@param >0 success
*/
int hn_upk_string (int buf,void * strptr,long maxlen) 
{
    int i;
    int s;
    int t;
    char *p;
    char *q;

#ifdef VERBOSE
    printf("[%s:%d] CALLING %s\n",__FILE__,__LINE__,__FUNCTION__);
#endif
    if (!hn_check_buf(buf)) {
#ifdef VERBOSE
        printf("[%s:%d] Invalid buf parameter\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
        return (0);
    }

    if(hn_upk_int32(buf, &s, 1) <=0 ) {
#ifdef VERBOSE
        printf("[%s:%d] Cannot unpack length of the string\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    if (msg_buf[buf].toend < s) {       /* not enough space */
#ifdef VERBOSE
        printf("[%s:%d] The space is not enough, Die another day.\n",__FILE__,__LINE__);
	fflush(stdout);
#endif
	return (0);
    }

    if ((s+1)>maxlen)	{	/* the +1 and -1 are for the NULL characters we will terminate the string with */
	t = maxlen-1;	
    } else {
	/* s stays the same */
	t = s;	/* no truncated message to chop off later */
    }

    p = (char *) strptr;
    q = (char *) msg_buf[buf].from_ptr;

    /* memcopy oneday */
    for(i=0;i<t;i++) *p++ = *q++;

    /* now terminate the string with a null character */
    *p = '\0';

    p = (char *) msg_buf[buf].from_ptr;
    p+= s;

    msg_buf[buf].from_ptr = p;
    msg_buf[buf].toend -= s;

    return (t);
}

