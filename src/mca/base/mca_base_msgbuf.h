/*
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


/* get/create a free buffer */
/* if reqsize = MCA_BASE_MSGBUF_GETBUF, then the system gives an unlimited buffer. */
/* Giving a req size just makes it more memory efficient. */
mca_base_msgbuf_t mca_base_msgbuf_new (size_t reqsize);

/* make a copy of an existing buffer */
/* this is usefull for the registry and is needed as unpack is */
/* destructive */
mca_base_msgbuf_t mca_base_msgbuf_copy (mca_base_msgbuf_t* copybufid, mca_base_msgbuf_t orgbufid);

/* set a buffer. As base_pack send/recv handles buffer you might not want */
/* to pack a buffer but do a send from memory directly */
/* a free on this special buffer just frees its structure not the memory */
mca_base_msgbuf_t mca_base_msgbuf_construct (void* ptr, size_t datasize);

/* explicit free of a buffer when not auto freeing them */
int mca_base_msgbuf_free (mca_base_msgbuf_t bufid);

/* pack and unpack non-string typed data */
int  mca_base_msgbuf_pack (mca_base_msgbuf_t bufid, void* ptr, size_t num_items, mca_base_msgbuf_data_t datatype);
int  mca_base_msgbuf_unpack (mca_base_msgbuf_t bufid, void* ptr, size_t num_items, mca_base_msgbuf_data_t datatype);

/* handles strings */
/* note this takes NULL terminated strings and returns null terminated strings */
int  mca_base_msgbuf_pack_string (mca_base_msgbuf_t bufid, char* strptr);
int  mca_base_msgbuf_unpack_string (mca_base_msgbuf_t bufid, char* strptr, size_t maxlen);


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
