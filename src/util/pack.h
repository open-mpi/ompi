/*
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
    OMPI_INT16, /**< a 16 bit integer */
    OMPI_INT32, /**< a 32 bit integer */
    OMPI_STRING, /**< a NULL terminated string */
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
 *
 * @param pointer to new buffer handle
 * 
 * @retval OMPI_SUCCESS 
 * @retval OMPI_ERROR
 * 
 */

    int ompi_buffer_init (ompi_buffer_t *buffer);

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

    int ompi_buffer_size (ompi_buffer_t buffer, size_t *size);

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

    int ompi_buffer_free (ompi_buffer_t buffer);

        

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
    int ompi_pack(ompi_buffer_t buffer, void * src, size_t n, ompi_pack_type_t type);

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
    int ompi_unpack(ompi_buffer_t buffer, void * dest, size_t n, ompi_pack_type_t type);



#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

