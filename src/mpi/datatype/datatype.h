/*
 * $HEADER$
 */

/** @file
 *
 * lam_datatype_t interface for LAM internal data type representation
 *
 * lam_datatype_t is a class which represents contiguous or
 * non-contiguous datat together with constituent type-related
 * information.  It is the LAM's-eye view of MPI_Datatype.
 */

#ifndef LAM_DATATYPE_H_INCLUDED
#define LAM_DATATYPE_H_INCLUDED 1

#include <sys/types.h>
#include <sys/uio.h>
#include <string.h>
#include <stdlib.h>

#include "lam_config.h"
#include "lam/constants.h"
#include "lam/stdint.h"
#include "lam/lfc/lam_object.h"
#include "lam/types.h"

#include "mpi.h"

/* macros *************************************************************/

/**
 * Test 32-bit alignment of an address
 *
 * @param address   An address
 * @return          true if the address is 32-bit aligned
 */
#define LAM_IS_32BIT_ALIGNED(addr) \
    (((uint32_t) addr & (uint32_t) 3) == (uint32_t) 0 ? true : false)

/**
 * Test 64-bit alignment of an address
 *
 * @param address   An address
 * @return          true if the address is 32-bit aligned
 */
#define LAM_IS_64BIT_ALIGNED(addr) \
    (((uint64_t) addr & (uint64_t) 7) == (uint64_t) 0 ? true : false)


/* typedefs ***********************************************************/

typedef struct lam_checksum_t lam_checksum_t;
typedef struct lam_datatype_t lam_datatype_t;
typedef struct lam_datavec_element_t lam_datavec_element_t;
typedef struct lam_datavec_t lam_datavec_t;
typedef struct lam_dataxdr_t lam_dataxdr_t;
typedef struct lam_pack_state_t lam_pack_state_t;
typedef struct lam_memcpy_state_t lam_memcpy_state_t;

/* Function prototype for a generalized memcpy() */
typedef void *(lam_memcpy_fn_t) (void *restrict dst,
				 const void *restrict src,
				 size_t size,
                                 lam_memcpy_state_t *check);

/* enums **************************************************************/

/**
 * Datatype state flags
 */
enum lam_datatype_state_t {
    LAM_DATATYPE_STATE_COMMITTED = 1 << 0,
    LAM_DATATYPE_STATE_CONTIGUOUS = 1 << 1,
    LAM_DATATYPE_STATE_FORTRAN = 1 << 2,
    LAM_DATATYPE_STATE_OPTIMIZED = 1 << 3,
    LAM_DATATYPE_STATE_DONT_OPTIMIZE = 1 << 4,
    LAM_DATATYPE_STATE_XDR = 1 << 5,
    /* etc. */
};


/**
 * Enumeration of datatype creation functions
 */
enum lam_datatype_kind_t {

    LAM_DATATYPE_KIND_BASIC = 0,

    LAM_DATATYPE_KIND_CONTIG,
    LAM_DATATYPE_KIND_DUP,
    LAM_DATATYPE_KIND_HINDEXED,
    LAM_DATATYPE_KIND_HVECTOR,
    LAM_DATATYPE_KIND_INDEXED,
    LAM_DATATYPE_KIND_LB,
    LAM_DATATYPE_KIND_PACKED,
    LAM_DATATYPE_KIND_STRUCT,
    LAM_DATATYPE_KIND_UB,
    LAM_DATATYPE_KIND_VECTOR,

    LAM_DATATYPE_KIND_CONTIG_FORTRAN,
    LAM_DATATYPE_KIND_HINDEXED_FORTRAN,
    LAM_DATATYPE_KIND_HVECTOR_FORTRAN,
    LAM_DATATYPE_KIND_INDEXED_FORTRAN,
    LAM_DATATYPE_KIND_STRUCT_FORTRAN,
    LAM_DATATYPE_KIND_VECTOR_FORTRAN
};


/**
 * lam_checksum_kind_t - checksum types
 */
enum lam_checksum_kind_t {
    LAM_CHECKSUM_KIND_NONE = 0,
    LAM_CHECKSUM_KIND_CRC32,
    LAM_CHECKSUM_KIND_SUM32,
    LAM_CHECKSUM_KIND_SUM64
};


typedef enum lam_datatype_state_t lam_datatype_state_t;
typedef enum lam_datatype_kind_t lam_datatype_kind_t;
typedef enum lam_checksum_kind_t lam_checksum_kind_t;

/* structs ************************************************************/

/**
 * State of incremental memcpy with checksum or CRC
 */
struct lam_memcpy_state_t {
    size_t size;	   /**< total size in bytes of the object
                            * being checksummed / CRCed */
    size_t partial_size;   /**< size of non- uint32_t to be carried
                            * over to next call */
    uint32_t partial_int;  /**< value of non- uint32_t to be carried
                            * over to next call */
    uint32_t sum;	   /**< current value of the CRC or
                            * checksum */
    bool first_call;	   /**< is this the first call for this
                            * checksum/CRC? */
};


/**
 * Internal representation of MPI datatype
 */
struct lam_datatype_t {

    lam_object_t d_super;	/**< object super class */
    char d_name[MPI_MAX_OBJECT_NAME]; /**< object name */
    int d_flags;		  /**< bit flags */

    /* cached information */

    ssize_t d_lower_bound;
    size_t d_extent;
    size_t d_packed_size;	/**< size in bytes, ignoring gaps */
    int d_nbasic;		 /**< number of basic elements */

    /* optimized representation */

    size_t d_datavec_size;	  /**< size of optimized representation */
    lam_datavec_t *d_datavec;	  /**< optimized representation (may be null) */

    /* XDR representation */

    size_t d_dataxdr_size;	  /**< size of XDR representation */
    lam_dataxdr_t *d_dataxdr;	  /**< XDR representation (may be null) */

    /* full representation (c.f. MPI_Type_create_struct) */

    struct {
	lam_datatype_kind_t c_kind;	/**< creation function */
	int c_count;		  /**< number of blocks */
	int *c_blocklengths;	  /**< number of elements in each block */
	MPI_Aint *c_offset;	  /**< stride/displacement as appropriate */
	lam_datatype_t **c_types; /**< array of types (array) */
    } d_creator;
};


/**
 * An optimized representation of noncontiguous data used by packing
 * routines
 */
struct lam_datavec_t {
    size_t dv_nrepeat;
    ssize_t dv_repeat_offset;
    size_t dv_nelement;
    lam_datavec_element_t *dv_element;
};


/**
 * An element of a data type in optimized form
 */
struct lam_datavec_element_t {
    size_t dve_size;		    /**< size in bytes of element */
    ssize_t dve_offset;		    /**< offset from start of data type */
    ssize_t dve_seq_offset;	    /**< offset from start of packed data type */
};


/**
 * XDR representation of a datatype
 */
struct lam_dataxdr_element_t {
    /* to be done */
    void *x_xdrs;		  /**< XDR stream */
};


/* interface **********************************************************/

/**
 * Checksum (the contents of) an array of data types
 *
 * @param addr          Data type array
 * @param count         Size of array
 * @param datatype      Datatype descriptor
 * @param checksum      Checksum
 * @return              0 on success, -1 on error
 */
int lam_datatype_checksum(const void *addr,
			  size_t count,
			  lam_datatype_t *datatype,
			  lam_checksum_t *checksum);

/**
 * Copy (the contents of) an array of data types
 *
 * @param dst           Output data type array
 * @param src           Input data type array
 * @param count         Size of array
 * @param datatype      Datatype descriptor
 * @param check         Pointer to checksum or CRC
 * @return              0 on success, -1 on error
 */
int lam_datatype_copy(void *dst,
		      const void *src,
		      size_t count,
		      lam_datatype_t *datatype,
		      lam_memcpy_fn_t *memcpy_fn,
		      lam_memcpy_state_t *check);

/**
 * Copy (the contents of) an array of data types, and convert to
 * another datatype
 *
 * @param dst           Output data type array
 * @param dst_count     Size of output array
 * @param dst_datatype  Output datatype descriptor
 * @param src           Input data type array
 * @param src_count     Size of input array
 * @param src_datatype  Input datatype descriptor
 * @param checksum      Checksum
 * @return              0 on success, -1 on error
 */
int lam_datatype_convert(void *dst,
			 lam_datatype_t *dst_datatype,
			 size_t dst_count,
			 const void *src,
			 lam_datatype_t *src_datatype,
			 size_t src_count,
			 lam_memcpy_fn_t *memcpy_fn,
			 lam_memcpy_state_t *check);


/**
 * Pack state
 *
 * Structure to store the state of an incremental pack/unpack of a
 * datatype.
 */
struct lam_pack_state_t {
    size_t current_offset_packed;  /**< current offset into packed buffer */
    size_t current_type;	   /**< current index of datatype */
    size_t current_repeat;	   /**< current index of datavec repeat */
    size_t current_element;	   /**< current index of datavec element */
    size_t current_offset_datavec; /**< current offset into datavec element */
};


/**
 * Incrementally pack an array of datatypes into a buffer
 *
 * @param state         current state of the incremental pack/unpack
 * @param buf           buffer to pack into/unpack from
 * @param bufsize       size of buffer
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param datatype      type descriptor
 * @param memcpy_fn     pointer to memcpy function
 * @param check         pointer to checksum
 * @return              0 if complete, non-zero otherwise
 *
 * Incrementally copy data type arrays to/from a packed buffer by
 * iterating over the type and type_map until we finish or run out of
 * room.
 *
 * The state (all members) should be initialized to 0 before the first
 * call.
 */
int lam_datatype_pack(lam_pack_state_t *state,
		      void *buf,
		      size_t bufsize,
		      const void *typebuf,
		      size_t ntype,
		      lam_datatype_t *datatype,
		      lam_memcpy_fn_t *memcpy_fn,
		      lam_memcpy_state_t *check);


/**
 * Incrementally unpack a buffer to an array of datatypes
 *
 * @param state         current state of the incremental pack/unpack
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param buf           buffer to pack into/unpack from
 * @param bufsize       size of buffer
 * @param datatype      type descriptor
 * @param memcpy_fn     pointer to memcpy function
 * @param check         pointer to checksum
 * @return              0 complete, non-zero otherwise
 *
 * Incrementally copy data type arrays to/from a packed buffer by
 * iterating over the type and type_map until we finish or run out of
 * room.
 *
 * The state (all members) should be initialized to 0 before the first
 * call.
 */
int lam_datatype_unpack(lam_pack_state_t *state,
			void *typebuf,
			size_t ntype,
			const void *buf,
			size_t bufsize,
			lam_datatype_t *datatype,
			lam_memcpy_fn_t *memcpy_fn,
			lam_memcpy_state_t *check);

/**
 * Incrementally generate an iovec for gathering from an array of
 * datatypes
 *
 * @param state         current state of the incremental pack/unpack
 * @param base_addr     base address for iovec offsets
 * @param vec           iovec buffer
 * @param vec_count     maximum length of iovec buffer
 * @param max_bytes     maximum bytes addressed by iovec
 * @param buf           buffer to pack into/unpack from
 * @param bufsize       size of buffer
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param type          type descriptor
 * @return              0 if complete, non-zero otherwise
 *
 * Incrementally traverse an array of datatypes and generate an iovec
 * of at most length vec_count and addressing at most max_bytes.  This
 * can be used to do a (partial) RDMA gather of the datatype array.
 *
 * The state (all members) should be initialized to 0 before the first
 * call.
 */
int lam_datatype_gather_iovec(lam_pack_state_t *state,
			      void *base_addr,
			      struct iovec *vec,
			      size_t vec_count,
			      size_t max_bytes,
			      const void *typebuf,
			      size_t ntype,
			      lam_datatype_t *datatype,
			      lam_checksum_t *checksum);

/**
 * Incrementally generate an iovec for scattering from a packed array
 * of datatypes
 *
 * @param state         current state of the incremental pack/unpack
 * @param base_addr     base address for iovec offsets
 * @param vec           iovec buffer
 * @param vec_count     maximum length of iovec buffer
 * @param max_bytes     maximum bytes addressed by iovec
 * @param buf           packed buffer
 * @param bufsize       size of buffer
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param type          type descriptor
 * @return              0 if complete, non-zero otherwise
 *
 * Incrementally copy data type arrays to/from a packed buffer.  by
 * iterating over the type and type_map until we finish or run out of
 * room.
 *
 * Incrementally traverse a packed array of datatypes and generate an
 * iovec of at most length vec_count and addressing at most max_bytes.
 * This can be used to do a (partial) RDMA scatter of the datatype
 * array.
 *
 * The state (all members) should be initialized to 0 before the first
 * call.
 */
int lam_datatype_scatter_iovec(lam_pack_state_t *state,
			       void *base_addr,
			       struct iovec *vec,
			       size_t vec_count,
			       size_t max_bytes,
			       const void *buf,
			       size_t bufsize,
			       lam_datatype_t *datatype,
			       lam_memcpy_fn_t *memcpy_fn,
			       lam_memcpy_state_t *check);


/*
 * incremental memcpy with checksum / CRC functions
 */


/**
 * initialize the state for an incremental memcpy with checksum / CRC
 *
 * @param state     pointer to state object for the current sequence of copies
 * @param sum_size  the length of the entire buffer to be checksummed
 */
static inline void
lam_memcpy_init(lam_memcpy_state_t *state, size_t sum_size)
{
    state->size = sum_size;
    state->first_call = true;
}


/**
 * Copy data from one buffer to another
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param check    unused
 * @return         the original value of dst
 */
static inline void *lam_memcpy(void *dst, const void *src, size_t size,
			       void *check)
{
    return memcpy(dst, src, size);
}

/**
 * An alternative version of memcpy that may out-perform the system
 * version on some (silly) systems.
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    unused
 * @return         the original value of dst
 */
void *lam_memcpy_alt(void *dst, const void *src, size_t size,
                     lam_memcpy_state_t *state);


/**
 * Generate a 32-bit CRC for a buffer
 *
 * @param buffer      Data buffer
 * @param size        Size of buffer
 * @param initial_crc Initial value of the CRC register
 * @return            The CRC
 *
 * Generate a 32-bit for a data buffer starting from a given CRC
 * value.
 */
uint32_t lam_crc32(const void *restrict buffer, size_t size,
		   uint32_t initial_crc);


/**
 * Generate a 32-bit checksum for a buffer
 *
 * @param buffer      Data buffer
 * @param size        Size of buffer
 * @return            The CRC
 *
 * Generate a 32-bit for a data buffer starting from a given CRC
 * value.
 */
uint32_t lam_sum32(const void *restrict buffer, size_t size);


/**
 * Copy data from one buffer to another and calculate a 32-bit CRC
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    pointer to a memcpy with checksum/CRC state structure
 * @return         the original value of dst
 *
 * This handles cumulative CRCs for for arbitrary lengths and address
 * alignments as best as it can. The initial contents of state->sum is
 * used as the starting value of the CRC.  The final CRC is placed
 * back in state->sum.
 */
void *lam_memcpy_crc32(void *restrict dst,
                       const void *restrict src,
                       size_t size,
                       lam_memcpy_state_t *check);


/**
 * Copy data from one buffer to another and calculate a 32-bit checksum
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    pointer to a memcpy with checksum/CRC state structure
 * @return         the original value of dst
 *
 * This handles cumulative checksumming for arbitrary lengths and
 * address alignments as best as it can; the contents of
 * lastPartialLong and lastPartialLength are updated to reflected the
 * last partial word's value and length (in bytes) -- this should
 * allow proper handling of checksumming contiguous or noncontiguous
 * buffers via multiple calls of bcopy_csum() - Mitch
 */
void *lam_memcpy_sum32(void *restrict dst,
                       const void *restrict src,
                       size_t size,
                       lam_memcpy_state_t *check);


/**
 * Copy data from one buffer to another and calculate a 32-bit checksum
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    pointer to a memcpy with checksum/CRC state structure
 * @return         the original value of dst
 */
void *lam_memcpy_sum64(void *restrict dst,
                       const void *restrict src,
                       size_t size,
                       lam_memcpy_state_t *check);


/**
 * Create a LAM/MPI datatype
 *
 * @param combiner   integer identifying the kind of MPI create function
 * @param ninteger   number of integers passed to the create function
 * @param integer    array of integers passed to the create function
 * @param naddress   number of addresses passed to the create function
 * @param address    array of addresses passed to the create function
 * @param ntype      number of data types passed to the create function
 * @param type       array of data types passed to the create function
 * @param newtype    pointer to address of new type
 * @return           LAM_SUCCESS on successful creation, LAM_ERROR otherwise
 *
 * This is the central location for creation of data types in LAM/MPI.
 * All MPI_Type_create functions rely upon this to do the actual type
 * creation.
 */
int lam_datatype_create(int combiner,
                        int nintegers,
                        int integers[],
                        int naddresses,
                        ssize_t addresses[],
                        int ntypes,
                        lam_datatype_t *types[],
                        lam_datatype_t **newtype);


/**
 * Delete a LAM/MPI datatype (actually, just mark it for deletion)
 *
 * @param type       datatype
 * @return           LAM_SUCCESS on success, LAM_ERROR otherwise
 *
 * This is the central location for creation of data types in LAM/MPI.
 * All MPI_Type_create functions rely upon this to do the actual type
 * creation.
 */
int lam_datatype_delete(lam_datatype_t *type);



#endif				/* LAM_DATATYPE_H_INCLUDED */
