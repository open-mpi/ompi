/*
 * $HEADER$
 */

/** @file
 *
 * pack/unpack buffer management functions
 *
 */
#include <string.h>
#include <stdlib.h>
#include <netinet/in.h>


#include "ompi_config.h"

#include "include/constants.h"
#include "class/ompi_object.h"

#include "pack.h"

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
OBJ_CLASS_DECLARATION (ompi_buffer_internal_t);


/* some internal functions */
/* which must be declared before creating an instance of the obj class */ 
static int ompi_buffer_get_ptrs (ompi_buffer_t buffer, void**  baseptr, void**  dataptr, void**  fromptr);
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

    int ompi_buffer_init (ompi_buffer_t *buffer)
{
ompi_buffer_internal_t* bptr;
size_t isize=4096;	/* we should check the mca params here */

	/* check that we can return a buffer atall.. */
	if (!buffer) return (OMPI_ERROR);

	/* create new buffer object */
	bptr = (ompi_buffer_internal_t *) OBJ_NEW (ompi_buffer_internal_t);

	if (!bptr) return (OMPI_ERROR);

	ompi_buffer_cnts++;

	/* we have a buffer now, so lets populate it */
	bptr->base_ptr = (void*) malloc (isize); /* BAD fixed initial size */

	bptr->data_ptr = bptr->base_ptr; /* set the start of the buffer */
	/* leave from_ptr NULL so we catch an unpack before pack! */

	/* set counts for size and space */
	bptr->size = bptr->space = isize;
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
	if (!size) return (OMPI_ERROR);
	/* check that buffer is not null */
	if (!buffer) return (OMPI_ERROR);

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
	if (!buffer) return (OMPI_ERROR);

	/* deref and pass back */
	if (baseptr) *baseptr = buffer->base_ptr;
	if (dataptr) *dataptr = buffer->data_ptr;
	if (fromptr) *fromptr = buffer->from_ptr;

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
	if (!buffer) return (OMPI_ERROR);

	bptr = buffer;

	OBJ_RELEASE (bptr);

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
    int i, rc;
    void *dest;
    ompi_buffer_internal_t* bptr;
    size_t op_size=0;
    uint16_t * d16;
    uint32_t * d32;
    uint16_t * s16;
    uint32_t * s32;

    /* first find the destination location in the buffer */
    if (!buffer) return (OMPI_ERROR);

    bptr = (ompi_buffer_internal_t *) buffer; /* get buffer */


    dest = bptr->data_ptr;	/* get location in buffer */

    /* calculate op_size data size */
    switch(type) {
        case OMPI_BYTE:
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
                 if (NULL==from) return OMPI_ERROR;
                 else 
                    src = (ompi_buffer_t) from;
            }
            if (OMPI_ERROR==rc) return OMPI_ERROR;
            break;
        case OMPI_INT16:
	    op_size = n*2;
	    break;
        case OMPI_INT32:
	    op_size = n*4;
	    break;
        default:
            return OMPI_ERROR;
    }

    if (op_size > bptr->space) { /* need to expand the buffer */
	/* todo resize buffer */
        /* after resizing, if it worked we would need to update the dest */
	/* as it could have moved. (Learned this bug from Edgar :) */
    	dest = bptr->data_ptr;	/* get location in buffer */

	return (OMPI_ERROR); /* for tonight */
    } 
    
    switch(type) {
        case OMPI_BYTE:
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
            strncpy(dest, src, n);
            *((char *) dest + n - 1) = '\0';
            break;
        default:
            return OMPI_ERROR;
    }

    /* ok, we managed to pack some more stuff, so update all ptrs/cnts */
    bptr->data_ptr = ((char*)bptr->data_ptr) + op_size;

    bptr->len += op_size;
    bptr->toend += op_size;

    bptr->space -= op_size;

    /* if first pack also set the from_ptr as it is now valid */
    if (!bptr->from_ptr) bptr->from_ptr = bptr->base_ptr;

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
    int ompi_unpack(ompi_buffer_t buffer, void * dest, size_t n, ompi_pack_type_t type)
{
    int i;
    void *src;
    ompi_buffer_internal_t* bptr;
    size_t op_size=0;
    uint16_t * d16;
    uint32_t * d32;
    uint16_t * s16;
    uint32_t * s32;

    /* first find the source location in the buffer */
    if (!buffer) return (OMPI_ERROR);

    bptr = (ompi_buffer_internal_t *) buffer; /* get buffer */

    src = bptr->from_ptr;	/* get location in buffer */

    /* calculate op_size data size */
    switch(type) {
        case OMPI_BYTE:
        case OMPI_STRING:
	    op_size = n;
	    break;
	case OMPI_PACKED:
	    /* once a buffer is packed, the DS becomes 'flat'.. */
            /* so you cannot do a recursive unpack.. sorry. GEF */
            return OMPI_ERROR;
        case OMPI_INT16:
	    op_size = n*2;
	    break;
        case OMPI_INT32:
	    op_size = n*4;
	    break;
        default:
            return OMPI_ERROR;
    }

    /* now we need to do a TRUNCATION buffer check... */
    /* as this is used by user level OMPI users this should be nicer */
    /* i.e. attempt to unpack as much as possible */
    /* but for now if attempt to unpack something invalid it will error. GEF */

    if (op_size > bptr->toend) { /* truncation of requested data  */
	/* todo nicer best effort unpack */
	return (OMPI_ERROR); /* for tonight */
    }


    switch(type) {
        case OMPI_BYTE:
            memcpy(dest, src, n);
        case OMPI_PACKED:
            return OMPI_ERROR;
        case OMPI_INT16:
            d16 = (uint16_t *) dest;
            s16 = (uint16_t *) src;
            for (i=0;i<n;i++) {
                /* convert the network order to host order */
                d16[i] = ntohs(s16[i]);
            }
            break;
        case OMPI_INT32:
            d32 = (uint32_t *) dest;
            s32 = (uint32_t *) src;
            for (i=0;i<n;i++) {
                /* convert the network order to host order */
                d32[i] = ntohl(s32[i]);
            }
            break;
        case OMPI_STRING:
            strncpy(dest, src, n);
            *((char *) dest + n - 1) = '\0';
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

