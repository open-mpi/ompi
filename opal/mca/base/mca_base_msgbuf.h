/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Run-time MCA message buffer interface for packing/unpacking messages
 * these routines are used by both the OOB and the registry.
 */

#ifndef OMPI_MCA_BASE_MSGBUF_H
#define OMPI_MCA_BASE_MSGBUF_H


/**************************************************************************/

/*
 * packed message buffer interface, moved to mca_base so that all MCA modules can 
 * access it
 */

/* exposed interface to the packed message buffer system */
typedef enum { 
	MCA_BASE_MSGBUF_BYTE, 
	MCA_BASE_MSGBUF_INT32, MCA_BASE_MSGBUF_INT64, MCA_BASE_MSGBUF_INT128,
	MCA_BASE_MSGBUF_REAL32, MCA_BASE_MSGBUF_REAL64,
	MCA_BASE_MSGBUF_PACKED,
	MCA_BASE_MSGBUF_BYTE_BY_REF
} mca_base_msgbuf_data_t;

/*
 * The data types are simple and are required for each pack/unpack
 * The user is expected to unpack the data in the same order as they packed.
 * The system may or maynot type the message.
 *
 * Special types
 * MCA_BASE_MSGBUF_BYTE does not get converted.
 * MCA_BASE_MSGBUF_PACKED packs a packed buffer into another buffer.
 * MCA_BASE_MSGBUF_PACKED should be unpacked into a new buffer (returned in 'ptr').
 * MCA_BASE_MSGBUF_BYTE_BY_REF only has real meaning during packing. The system would
 *            avoid copying this memory if possible. 
 * The receiver would need to unpack by using either MCA_BASE_MSGBUF_BYTE/_BY_REF.
 * 
 */

/*
 * the definition of the msgbuf is opaque completely
 */

typedef struct mca_base_msgbuffer_s* mca_base_msgbuf_t;

/* 
 * get/create a free buffer 
 * 
 * @param reqsize requested size for the buffer
 *
 * @retval mca_base_msgbuf_t handle to a message buffer
 * 
 * if reqsize = MCA_BASE_MSGBUF_GETBUF, then the system gives an 
 * unlimited buffer. 
 *
 * Giving a req size just makes it more memory efficient. */
OPAL_DECLSPEC mca_base_msgbuf_t mca_base_msgbuf_new (size_t reqsize);

/* make a copy of an existing buffer 
 * 
 * @param mca_base_msgbuf_t handle to an existing message buffer
 * @param return a new buffer handle via a pointer argument
 * 
 * @retval sucess or failue int
 *
 * this is usefull for the registry and is needed as unpack is 
 * destructive */
OPAL_DECLSPEC int  mca_base_msgbuf_copy (mca_base_msgbuf_t* copybufid, mca_base_msgbuf_t orgbufid);

/* set a buffer to a block of memory so that you do not pack/memory copy
 *
 * @param ptr Pointer to users memory to be send
 * @param datasize Length of the memory in bytes that needs to be sent
 * 
 * @retval mca_base_msgbuf_t message buffer that points to this userdata
 *
 * As base_pack send/recv handles buffer you might not want 
 * to pack a buffer but do a send from memory directly 
 * a free on this special buffer just frees its structure not the memory 
 */
OPAL_DECLSPEC mca_base_msgbuf_t mca_base_msgbuf_construct (void* ptr, size_t datasize);

/* explicit free of a buffer when not auto freeing them 
 * 
 * @param mca_base_msgbuf_t buffer handle that you request to be freed
 *
 * @retval success or error code
 *
 * This routine resets the handle the user passing inso that they can only 
 * free it once
 */
OPAL_DECLSPEC int mca_base_msgbuf_free (mca_base_msgbuf_t* bufid);

/* pack and non-string typed data 
 *
 * @param mca_base_msgbuf_t bufid buffer handle where data is packed
 * @param void* ptr Pointer to users memory to pack from
 * @param size_t num_items Number of items to pack into buffer bufid
 * @param mca_base_msgbuf_data_t datatype Type of item being packed
 * 
 * @retval if zero or greater the items packed, if negative error code
 *
 * If the buffer fills up, it will automatically resize unless allocated
 * with fixed buffer size.
 */
OPAL_DECLSPEC int  mca_base_msgbuf_pack (mca_base_msgbuf_t bufid, void* ptr, size_t num_items, mca_base_msgbuf_data_t datatype);


/* unpack non-string typed data 
 *
 * @param mca_base_msgbuf_t bufid buffer handle where data is packed
 * @param void* ptr Pointer to users memory to unpack into
 * @param size_t num_items Number of items to unpack from the buffer bufid
 * @param mca_base_msgbuf_data_t datatype Type of item being unpacked
 * 
 * @retval if zero or greater the items unpacked, if negative error code
 *
 * Once the buffer empties, the buffer is freed. 
 * If the remain data in the buffer is not large enought for the unpack 
 * request, the routine will unpack what it can and then return an error.
 * The user is responsible for unpacking a message correctly.
 */
OPAL_DECLSPEC int  mca_base_msgbuf_unpack (mca_base_msgbuf_t bufid, void* ptr, size_t num_items, mca_base_msgbuf_data_t datatype);

/* pack a NULL terminated string 
 *
 * @param mca_base_msgbuf_t bufid buffer handle where data is packed
 * @param strptr Pointer to NULL terminated string to pack
 * 
 * @retval if zero or greater the items packed, if negative error code
 *
 * If the buffer fills up, it will automatically resize unless allocated
 * with a fixed buffer size.
 */
OPAL_DECLSPEC int  mca_base_msgbuf_pack_string (mca_base_msgbuf_t bufid, char* strptr);

/* unpack a NULL terminated string 
 *
 * @param mca_base_msgbuf_t bufid buffer handle where data is already packed
 * @param char* strptr Pointer to memory to unpack string into
 * @param maxlen maximum size of the memory available to unpack into
 * 
 * @retval if zero or greater the items (un)packed, if negative error code
 *
 * If the memory given is smaller than the string (strlen(str)) + 1 
 * then the routine truncates the string but always NULL terminates it.
 *
 */
OPAL_DECLSPEC int  mca_base_msgbuf_unpack_string (mca_base_msgbuf_t bufid, char* strptr, size_t maxlen);


/* constants */
#define MCA_BASE_MSGBUF_NULL_BUFID -1	/* default buffer ID returned when freeing */
#define MCA_BASE_MSGBUF_GETBUF -2000      /* default system give me a buffer const */

/* error codes */
#define MCA_BASE_MSGBUF_SUCCESS	0;

/* buffer error codes */
#define MCA_BASE_MSGBUF_BADBUFFER    -2010 /* bad buffer id */
#define MCA_BASE_MSGBUF_OUTOFBUFFERS -2011 /* no free msg buffers free */
#define MCA_BASE_MSGBUF_OUTOFMEMORY  -2012 /* cannot allocate any more memory for message buffers */
#define MCA_BASE_MSGBUF_BADPARM      -2014 /* other parameters are bad/invalid pointers */
#define MCA_BASE_MSGBUF_BADDATA	 -2015 /* to/from data addr is bad (null etc) */

#endif /* OMPI_MCA_BASE_MSGBUF_H */
