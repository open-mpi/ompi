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
/** @file:
 *
 * pack/unpack buffer management functions
 *
 */

/* Notes on these functions 
 * Yes they are used by the NS and GRP etc etc but they are part of the 
 * public interface to these and can be used for more than just those
 * RTE services
 *
 * aka there are here until the NS/GPR etc etc is completed
 * GEF
 */

#ifndef _OMPI_PACK_H_
#define _OMPI_PACK_H_
#include "ompi_config.h"

#include "mca/ns/base/base.h"
/*
 * Other constants
 */

/**
 * Supported datatypes for conversion operations.
 * NOTE, these have (or should) a one to one match to ompi_pack_type_t
 *
 */

typedef enum {
    OMPI_BYTE, /**< a byte of data */
    OMPI_INT8,  /**< an 8-bit integer */
    OMPI_INT16, /**< a 16 bit integer */
    OMPI_INT32, /**< a 32 bit integer */
    OMPI_STRING, /**< a NULL terminated string */
    OMPI_NAME,  /**< an ompi_process_name_t */
    OMPI_JOBID,  /**< a jobid */
    OMPI_PACKED /**< already packed data. */
} ompi_pack_type_t;

typedef struct ompi_buffer_internal_t* ompi_buffer_t;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/**
 * This function creates a managed buffer
 * users then pack this buffer as many times as needed
 * as the buffer is managed, we grow it as needed
 * if the initial requested buffer size is '0' then the system allocates
 * what it wants. If a size is given then it attempts to get that space
 * (useful for when receiving a known in advance data size into the buffer)
 * (hint, as in the oob wrappers)
 *
 * @param pointer to new buffer handle (OUT)
 * @param to initial buffer size request (IN)
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 * 
 */

OMPI_DECLSPEC int ompi_buffer_init (ompi_buffer_t *buffer, size_t reqinitsize);


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

OMPI_DECLSPEC int ompi_buffer_init_preallocated (ompi_buffer_t *buffer, void *usermemory, size_t usermemorylen);

/** 
 * This function gets the size of packed data in a ompi_buffer
 * 
 * @param buffer handle
 * @param pointer to buffer size
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 *
 */

OMPI_DECLSPEC int ompi_buffer_size (ompi_buffer_t buffer, size_t *size);

/** 
 * This function gets the base/data/from ptrs of data in an ompi_buffer
 * 
 * @param buffer handle (IN)
 * @param pointer to buffer start (base) (OUT)
 * @param pointer to next data storage in buffer (data) (OUT)
 * @param pointer to start of next buffer read (from) (OUT)
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 *
 */

OMPI_DECLSPEC int ompi_buffer_get_ptrs (ompi_buffer_t buffer,
                void**  baseptr, void**  dataptr, void**  fromptr);


/**
 * This function returns the base pointer and size of the 
 * packed region - suitable for use by oob_send/gpr_put/etc.
 * Note the memory is still owned by the buffer.
 *
 */

OMPI_DECLSPEC int ompi_buffer_get(ompi_buffer_t buffer, void** base, int* size);

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

OMPI_DECLSPEC int ompi_buffer_free (ompi_buffer_t buffer);

        

/*
 * functions for pack and unpack routines
 */
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
OMPI_DECLSPEC int ompi_pack(ompi_buffer_t buffer, void * src, size_t n, ompi_pack_type_t type);

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
OMPI_DECLSPEC int ompi_unpack(ompi_buffer_t buffer, void * dest, size_t n, ompi_pack_type_t type);


/* 
 * fuctions to handle strings, which use the length arguments different
 *
 * @param buffer the destination for the packed data
 * @param str pointer to start of NULL terminated string
 * 
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 *
 */

OMPI_DECLSPEC int ompi_pack_string (ompi_buffer_t buffer, char *str);

/**
 * This function unpacks a string from the buffer. This routine ALLOCATES memory
 * for this string. Allocating means users DO NOT need to define max string lengths for any 
 * strings they pass (allowing the use of unrestricted naming in the GPR f.e.)
 * if this string is zero length we return a NULL pointer
 *
 * @param buffer the source of the packed string data
 * @param pointer to a character pointer of the unpacked string or NULL for zero length string
 * @param type the type of the data to unpack
 * 
 * @retval number of characters unpacked (INCLUDING the NULL character)
 *         If this value is '0' this indicates an empty string was passed.
 * @retval OMPI_ERROR
 *
 */
OMPI_DECLSPEC int ompi_unpack_string(ompi_buffer_t buffer, char ** str);




#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

