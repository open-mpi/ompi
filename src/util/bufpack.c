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

/** @file
 *
 * pack/unpack buffer management functions
 *
 */
#include "ompi_config.h"
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif



#include "include/constants.h"
#include "class/ompi_object.h"
#include "mca/ns/ns.h"

#include "util/bufpack.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
typedef struct ompi_buffer_internal_t {
	 /* first member must be the objects parent */
	ompi_object_t parent;
         /* now for the real elements of the type */

        void*   base_ptr;  /* start of my memory */
        void*   data_ptr;  /* location of where next data will go */
        void*   from_ptr;  /* location of where to get the next data from */

        /* counters */

        size_t    size;      /* total size of this buffer */
        size_t    len;       /* total amount already packed */
        size_t    space;     /* how much space we have left */
                             /* yep, size=len+space */

        size_t    toend;     /* how many bytes till the end when unpacking :) */
                             /* yep, toend is the opposite of len */


	size_t    cnt;     /* temp cnt of buffer usage (debugging) */
} ompi_buffer_internal_t;

/* formalise the declaration */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION (ompi_buffer_internal_t);


/* some internal functions */
/* which must be declared before creating an instance of the obj class */ 
static void ompi_buffer_construct (ompi_buffer_internal_t* buffer);
static void ompi_buffer_destruct (ompi_buffer_internal_t* buffer);

OBJ_CLASS_INSTANCE(ompi_buffer_internal_t, ompi_object_t, 
		ompi_buffer_construct, ompi_buffer_destruct);


static size_t ompi_buffer_cnts = 0;


static void ompi_buffer_construct (ompi_buffer_internal_t* buffer)
{
	buffer->base_ptr = buffer->data_ptr = buffer->from_ptr = NULL;
	buffer->size = buffer->len = buffer-> space = buffer-> toend = 0;
	buffer->cnt = 0;
}

static void ompi_buffer_destruct (ompi_buffer_internal_t* buffer)
{
	/* paranoid check */
	if (buffer->base_ptr) free (buffer->base_ptr);

	/* just clean up */
	buffer->base_ptr = buffer->data_ptr = buffer->from_ptr = NULL;
	buffer->size = buffer->len = buffer-> space = buffer-> toend = 0;
}

/**
 * This function creates a managed buffer
 * users then pack this buffer as many times as op_size
 * as the buffer is managed, we grow it as op_size
 *
 * @param pointer to new buffer handle
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 * 
 */

    int ompi_buffer_init (ompi_buffer_t *buffer, size_t reqinitsize)
{
ompi_buffer_internal_t* bptr;
size_t defaultinitsize = getpagesize();	/* should check the mca params here */
size_t isize = 0;

	/* check that we can return a buffer atall.. */
	if (!buffer) { return (OMPI_ERROR); }

        /* check the requested initial size */
        if (reqinitsize<0) { return (OMPI_ERROR); }

	/* create new buffer object */
	bptr = (ompi_buffer_internal_t *) OBJ_NEW (ompi_buffer_internal_t);

	if (!bptr) { return (OMPI_ERROR); }

	ompi_buffer_cnts++;


	/* we have a buffer now, so lets populate it */

	/* allocate initial buffer space */
	if (!reqinitsize) { isize = defaultinitsize; }
	else { isize = reqinitsize; }

	/* question, should we round upto a page? */

	bptr->base_ptr = (void*) malloc (isize); 

	bptr->data_ptr = bptr->base_ptr; /* set the start of the buffer */
	bptr->from_ptr = bptr->base_ptr; /* set the unpack start at start */

	/* set counts for size and space */
	bptr->size = bptr->space = isize;
	bptr->cnt = ompi_buffer_cnts;

	/* ok, all is well, return the buffer back to the user */
	*buffer = bptr;

	return (OMPI_SUCCESS);	
}


/**
 * This function creates a buffer using USER allocated memory
 *
 * users can then pack MORE into this buffer if needed 
 * as the buffer is managed, we grow it as needed
 *
 * the user should not free the memory handed to the buffer
 * this will be done by buffer_free
 *
 * This routine is really only used by the OOB
 *
 * @param pointer to new buffer handle (OUT)
 * @param pointer to USER allocated memory (IN)
 * @param to initial USER memory allocated length (IN)
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 * 
 */

    int ompi_buffer_init_preallocated (ompi_buffer_t *buffer, void *usermemory,
size_t usermemorylen)
{
ompi_buffer_internal_t* bptr;

	/* check that we can return a buffer atall.. */
	if (!buffer) { return (OMPI_ERROR); }

	/* check that we have a valid user memory buffer atall.. */
	if (!usermemory) { return (OMPI_ERROR); }

    /* check the requested initial size */
     if (usermemorylen<0) { return (OMPI_ERROR); }

	/* create new buffer object */
	bptr = (ompi_buffer_internal_t *) OBJ_NEW (ompi_buffer_internal_t);

	if (!bptr) { return (OMPI_ERROR); }

	ompi_buffer_cnts++;


	/* we have a buffer now, so lets populate it */

	/* allocate initial buffer space */

	bptr->base_ptr = usermemory; /* set the start of the buffer */

	/* set data pointer to END of the buffer */
	bptr->data_ptr = ((char*)bptr->base_ptr) + usermemorylen; 

	bptr->from_ptr = bptr->base_ptr; /* set the unpack start at start */

	/* set counts for size and space */
	bptr->size = usermemorylen;
	bptr->len  = usermemorylen;		/* users buffer is expected 2 be full */
	bptr->space = 0;                /* ditto */
	bptr->toend = usermemorylen;    /* ditto */
	bptr->cnt = ompi_buffer_cnts;

	/* ok, all is well, return the buffer back to the user */
	*buffer = bptr;

	return (OMPI_SUCCESS);	
}



/** 
 * This function gets the size of packed data in an ompi_buffer
 * 
 * @param buffer handle
 * @param pointer to buffer size
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 *
 */

    int ompi_buffer_size (ompi_buffer_t buffer, size_t *size)
{
	/* check that we can return a size atall.. */
	if (!size) { return (OMPI_ERROR); }
	/* check that buffer is not null */
	if (!buffer) { return (OMPI_ERROR); }

	/* deref and pass back */
	*size = buffer->len;

	return (OMPI_SUCCESS);
}

/** 
 * This function gets the base/data/from ptrs of data in an ompi_buffer
 * 
 * @param buffer handle
 * @param pointer to buffer start (base)
 * @param pointer to next data storage in buffer (data)
 * @param pointer to start of next buffer read (from)
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 *
 */

    int ompi_buffer_get_ptrs (ompi_buffer_t buffer, 
		void**  baseptr, void**  dataptr, void**  fromptr)
{
	/* check that buffer is not null */
	if (!buffer) { return (OMPI_ERROR); }

	/* deref and pass back */
	if (baseptr) { *baseptr = buffer->base_ptr; }
	if (dataptr) { *dataptr = buffer->data_ptr; }
	if (fromptr) { *fromptr = buffer->from_ptr; }

	return (OMPI_SUCCESS);
}


int ompi_buffer_get(ompi_buffer_t buffer, void**  baseptr, int *size)
{
	/* check that buffer is not null */
	if (!buffer) { return (OMPI_ERROR); }

	/* deref and pass back */
	if (baseptr) { *baseptr = buffer->base_ptr; }
	if (size) { *size = buffer->len; }

	return (OMPI_SUCCESS);
}

/**
 * This function frees a given buffer
 * If the buffer has data still, it is lost
 *
 * @param buffer handle
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 *
 */

    int ompi_buffer_free (ompi_buffer_t buffer)
{
ompi_buffer_internal_t *bptr;

	/* check that buffer is not null */
	if (!buffer) { return (OMPI_ERROR); }

	bptr = buffer;

	OBJ_RELEASE (bptr);

	return (OMPI_SUCCESS);
}

/**
 * Internal function that resizes (expands) an inuse buffer...
 * 
 * Takes the size increase, adds to current size rounds up to pagesize
 * 
 */
static int ompi_buffer_extend (ompi_buffer_internal_t *bptr, size_t increase)
{
/* no buffer checking, we should know what we are doing in here */

size_t newsize; 
size_t pages;
void*  newbaseptr;
ssize_t mdiff;		/* difference in memory */
size_t  sdiff;          /* difference (increase) in space */

/* calculate size of increase by pushing up page count */
pages = ((increase+bptr->size) / (size_t) getpagesize())+1;

newsize = (pages*(size_t)getpagesize());

sdiff = newsize - bptr->size; /* actual increase in space */
/* have to use relative change as no absolute without */
/* doing pointer maths for some counts such as space */

newbaseptr = realloc (bptr->base_ptr, newsize);

if (!newbaseptr) { return (OMPI_ERROR); }

/* ok, we have new memory */

/* update all the pointers in the buffer DT */
/* first calc change in memory location */
mdiff = ((char*)newbaseptr) - ((char*)bptr->base_ptr);

bptr->base_ptr = newbaseptr;
bptr->data_ptr = ((char*)bptr->data_ptr) + mdiff;
bptr->from_ptr = ((char*)bptr->from_ptr) + mdiff;

/* now update all pointers */
bptr->size = newsize;
bptr->space += sdiff; 

return (OMPI_SUCCESS);
}


/**
 * This function packs the passed data according to the type enum.
 *
 * @param buffer the destination for the packed data
 * @param src the source of the data
 * @param n the number of elements in the src
 * @param type the type of data 
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 */
    int ompi_pack(ompi_buffer_t buffer, void * src, size_t n, ompi_pack_type_t type)
{
    int rc;
    size_t i, pack_type_size;
    void *dest;
    ompi_buffer_internal_t* bptr;
    size_t op_size=0;
    uint8_t  * d8;
    uint16_t * d16;
    uint32_t * d32;
    uint16_t * s16;
    uint32_t * s32;

    /* first find the destination location in the buffer */
    if (!buffer) { return (OMPI_ERROR); }

    bptr = (ompi_buffer_internal_t *) buffer; /* get buffer */


    dest = bptr->data_ptr;	/* get location in buffer */

    /* calculate pack_type size */
    pack_type_size = sizeof(ompi_pack_type_t);
    
    /* calculate op_size data size */
    switch(type) {
        case OMPI_BYTE:
        case OMPI_INT8:
        case OMPI_NODE_STATE:
        case OMPI_PROCESS_STATUS:
        case OMPI_EXIT_CODE:
	    op_size = n;
	    break;
        case OMPI_STRING:
	    op_size = n;
	    break;
	case OMPI_PACKED:
	    {
	    void *base, *data, *from;

           	 rc = ompi_buffer_size ((ompi_buffer_t)src, &op_size);
                 rc = ompi_buffer_get_ptrs ((ompi_buffer_t)src, 
			&base, &data, &from);
                 /* check if packing from empty buffer.. */
                 if (NULL==from) { return OMPI_ERROR; }
                 else 
                    src = (ompi_buffer_t) from;
            }
            if (OMPI_ERROR==rc) { return OMPI_ERROR; }
            break;
        case OMPI_INT16:
	       op_size = n*sizeof(uint16_t);
	       break;
        case OMPI_INT32:
	       op_size = n*sizeof(uint32_t);
	       break;
        case OMPI_JOBID:
	       op_size = n*sizeof(mca_ns_base_jobid_t);
	       break;
        case OMPI_CELLID:
            op_size = n*sizeof(mca_ns_base_cellid_t);
            break;
        case OMPI_NAME:
            op_size = n*sizeof(ompi_process_name_t);
            break;
        default:
            return OMPI_ERROR;
    }

    /* add in storage needed for type information */
    op_size += pack_type_size;
    
    if (op_size > bptr->space) { /* need to expand the buffer */
    	rc =  ompi_buffer_extend (bptr, (op_size - bptr->space));
	if (OMPI_ERROR==rc) { return (rc); }

        /* after resizing, if it worked we would need to update the dest */
	/* as it could have moved. (Learned this bug from Edgar :) */
    	dest = bptr->data_ptr;	/* get location in buffer */
    } 
    
    /* store the pack data type */
    switch(pack_type_size) {
        case 1:
            d8 = (uint8_t *) dest;
            *d8 = type;
            d8++;
            dest = (void *) d8;
            break;
        case 2:
            d16 = (uint16_t *) dest;
            *d16 = htons(type);
            d16 += 2;
            dest = (void *) d16;
            break;
        case 4:
            d32 = (uint32_t *) dest;
            *d32 = htonl(type);
            d32 += 4;
            dest = (void *) d32;
            break;
        default:
            return OMPI_ERROR;
    }
    
    /* pack the data */
    switch(type) {
        case OMPI_BYTE:
        case OMPI_INT8:
        case OMPI_NODE_STATE:
        case OMPI_PROCESS_STATUS:
        case OMPI_EXIT_CODE:
            memcpy(dest, src, n);
            break;
            
        case OMPI_PACKED:
            memcpy(dest, src, op_size);
            break;
            
        case OMPI_INT16:
            d16 = (uint16_t *) dest;
            s16 = (uint16_t *) src;
            for (i=0;i<n;i++) {
                /* convert the host order to network order */
                d16[i] = htons(s16[i]);
            }
            break;
            
        case OMPI_INT32:
    	    d32 = (uint32_t *) dest;
            s32 = (uint32_t *) src;
            for (i=0;i<n;i++) {
                /* convert the host order to network order */
                d32[i] = htonl(s32[i]);
            }
            break;
            
        case OMPI_STRING:
            strncpy((char*) dest, (char*) src, n);
            *((char *) dest + n - 1) = '\0';
            break;
            
        case OMPI_JOBID:
	       mca_ns_base_pack_jobid(dest, src, n);
	       break;
        
        case OMPI_CELLID:
            mca_ns_base_pack_cellid(dest, src, n);
            break;
            
        case OMPI_NAME:
	       mca_ns_base_pack_name(dest, src, n);
            break;
            
        default:
            return OMPI_ERROR;
    }

    /* ok, we managed to pack some more stuff, so update all ptrs/cnts */
    bptr->data_ptr = ((char*)bptr->data_ptr) + op_size;

    bptr->len += op_size;
    bptr->toend += op_size;

    bptr->space -= op_size;

    return OMPI_SUCCESS;
}

/**
 * This function unpacks the passed data according to the type enum.
 *
 * @param buffer the source of the packed data
 * @param dest the destination for the unpacked data
 * @param n the number of elements in the src
 * @param type the type of the data to unpack
 * 
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 */ 
int
ompi_unpack(ompi_buffer_t buffer, void * dest, size_t n, ompi_pack_type_t type)
{
    size_t i, pack_type_size;
    void *src;
    ompi_buffer_internal_t* bptr;
    size_t op_size=0;
    ompi_pack_type_t packed_type;
    uint8_t  * s8;
    uint16_t * d16;
    uint32_t * d32;
    uint16_t * s16;
    uint32_t * s32;

    /* first find the source location in the buffer */
    if (!buffer) { return (OMPI_ERROR); }

    bptr = (ompi_buffer_internal_t *) buffer; /* get buffer */

    src = bptr->from_ptr;	/* get location in buffer */

    /* calculate the pack type size */
    pack_type_size = sizeof(ompi_pack_type_t);
    
    /* calculate op_size data size */
    switch(type) {
        case OMPI_BYTE:
        case OMPI_STRING:
        case OMPI_INT8:
        case OMPI_NODE_STATE:
        case OMPI_PROCESS_STATUS:
        case OMPI_EXIT_CODE:
	    op_size = n;
	    break;
	case OMPI_PACKED:
	    /* once a buffer is packed, the DS becomes 'flat'.. */
            /* so you cannot do a recursive unpack.. sorry. GEF */
            return OMPI_ERROR;
        case OMPI_INT16:
	    op_size = n*sizeof(uint16_t);
	    break;
        case OMPI_INT32:
	    op_size = n*sizeof(uint32_t);
	    break;
        case OMPI_JOBID:
	       op_size = n*sizeof(mca_ns_base_jobid_t);
	       break;
        case OMPI_CELLID:
            op_size = n*sizeof(mca_ns_base_cellid_t);
            break;
        case OMPI_NAME:
            op_size = n*sizeof(ompi_process_name_t);
            break;
        default:
            return OMPI_ERROR;
    }

    /* add in the pack_type storage size */
    op_size += pack_type_size;
    
    /* now we need to do a TRUNCATION buffer check... */
    /* as this is used by user level OMPI users this should be nicer */
    /* i.e. attempt to unpack as much as possible */
    /* but for now if attempt to unpack something invalid it will error. GEF */

    if (op_size > bptr->toend) { /* truncation of requested data  */
	/* todo nicer best effort unpack */
	return (OMPI_ERROR); /* for tonight */
    }

    /* retrieve the pack data type */
    switch(pack_type_size) {
        case 1:
            s8 = (uint8_t *) src;
            packed_type = *s8;
            s8++;
            src = (void *) s8;
            break;
        case 2:
            s16 = (uint16_t *) src;
            packed_type = ntohs(*s16);
            s16 += 2;
            src = (void *) s16;
            break;
        case 4:
            s32 = (uint32_t *) src;
            packed_type = ntohl(*s32);
            s32 += 4;
            src = (void *) s32;
            break;
        default:
            return OMPI_ERROR;
    }

    switch(type) {
        case OMPI_BYTE:
            if (OMPI_BYTE != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            memcpy(dest, src, n);
            break;
            
        case OMPI_INT8:
            if (OMPI_INT8 != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            memcpy(dest, src, n);
            break;
            
        case OMPI_NODE_STATE:
            if (OMPI_NODE_STATE != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            memcpy(dest, src, n);
            break;
            
        case OMPI_PROCESS_STATUS:
            if (OMPI_PROCESS_STATUS != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            memcpy(dest, src, n);
            break;
            
        case OMPI_EXIT_CODE:
            if (OMPI_EXIT_CODE != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            memcpy(dest, src, n);
	       break;
        
        case OMPI_PACKED:
            return OMPI_ERROR;

        case OMPI_INT16:
            if (OMPI_INT16 != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            d16 = (uint16_t *) dest;
            s16 = (uint16_t *) src;
            for (i=0;i<n;i++) {
                /* convert the network order to host order */
                d16[i] = ntohs(s16[i]);
            }
            break;
            
        case OMPI_INT32:
            if (OMPI_INT32 != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            d32 = (uint32_t *) dest;
            s32 = (uint32_t *) src;
            for (i=0;i<n;i++) {
                /* convert the network order to host order */
                d32[i] = ntohl(s32[i]);
            }
            break;
            
        case OMPI_STRING:
            if (OMPI_STRING != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            strncpy((char*) dest, (char*) src, n);
            *((char *) dest + n - 1) = '\0';
            break;
            
        case OMPI_JOBID:
            if (OMPI_JOBID != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
	       mca_ns_base_unpack_jobid(dest, src, n);
	       break;
        
        case OMPI_CELLID:
            if (OMPI_CELLID != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
            mca_ns_base_unpack_cellid(dest, src, n);
            break;
            
        case OMPI_NAME:
            if (OMPI_NAME != packed_type) {
                return OMPI_PACK_MISMATCH;
            }
	        mca_ns_base_unpack_name(dest, src, n);
            break;
            
        default:
            return OMPI_ERROR;
    }

    /* ok, we managed to unpack some stuff, so update all ptrs/cnts */
    bptr->from_ptr = ((char*)bptr->from_ptr) + op_size;

    bptr->toend -= op_size; /* closer to the end */
    bptr->len   -= op_size; /* and less data left */

    return OMPI_SUCCESS;
}



/* 
 * fuctions to handle strings, which use the length arguments differently to normal pack routines
 *
 * @param buffer the destination for the packed data
 * @param str pointer to start of NULL terminated string
 * 
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 *
 */

 int ompi_pack_string (ompi_buffer_t buffer, char *str)
 {
 uint32_t op_size=0;	
 int rc;

 if (!str) {
 	return OMPI_ERROR;
 }

 if (!buffer) { return (OMPI_ERROR); }

 op_size = (uint32_t) strlen (str);

 /* a packed string consists of a packed length, and the non terminated string */
 rc = ompi_pack(buffer, (void*) &op_size, 1, OMPI_INT32);

 if (OMPI_ERROR==rc) { return (rc); }

 if (op_size>0) {
 	rc = ompi_pack(buffer, (void*) str, op_size, OMPI_BYTE);
 }

 return (rc);
 }

/**
 * This function unpacks a string from the buffer. This routine ALLOCATES memory
 * for this string. Allocating means users DO NOT need to define max string lengths for any 
 * strings they pass (allowing the use of unrestricted naming in the GPR f.e.)
 * if this string is zero length we return a NULL pointer
 *
 * @param buffer the source of the packed string data
 * @param pointer to a character pointer of the unpacked string or NULL for zero length
 * string
 * @param type the type of the data to unpack
 * 
 * @retval number of characters unpacked (INCLUDING the NULL character)
 *         If this value is '0' this indicates an empty string was passed.
 * @retval OMPI_ERROR
 *
 */

 int ompi_unpack_string(ompi_buffer_t buffer, char ** str)
 {
 char *inptr;
 uint32_t inlen=0;
 uint32_t outlen=0;	/* always inlen+1 I hope */
 int rc;

 if (!str) {
 	return OMPI_ERROR;
 }

 if (!buffer) { return (OMPI_ERROR); }

 /* first unpack the length of the packed string */

 rc = ompi_unpack (buffer, (void*) &inlen, 1, OMPI_INT32);

 if (OMPI_ERROR==rc) { return (rc); }

 if (!inlen) { /* if we have a zero length string... set str pointer to NULL and return 0 length */
 	*str = NULL;
	return (0);
 }
 else {
 	outlen = inlen +1;
 }
 
 /* no zero length string, so allocate memory for it */
 inptr = (char*) calloc (outlen, 1);

 if (!inptr) { return (OMPI_ERROR); }

 /* have memory, unpack as byte, null terminate and then return */
 rc = ompi_unpack (buffer, (void*) inptr, inlen, OMPI_BYTE);

 if (OMPI_ERROR==rc) { return (rc); }

 inptr[inlen] = '\0'; /* NULL terminate */

 *str = (char*) inptr;	/* copy the string over */

 return (outlen);
 }


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

