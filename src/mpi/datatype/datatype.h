/*
 * $HEADER$
 */

 /** @file
  *
  * Data stuctures and functions related to LAM datatypes.
  */

/*
 * LAM internal data type representation
 */

#ifndef LAM_DATATYPE_H_INCLUDED
#define LAM_DATATYPE_H_INCLUDED 1

#include <sys/types.h>
#include <sys/uio.h>
#include <string.h>
#include <stdlib.h>

#include "lam_config.h"
#include "lam/constants.h"
#include "lam/lfc/object.h"
#include "lam/types.h"

#include "mpi.h"

/* typedefs ***********************************************************/

typedef struct lam_checksum_t lam_checksum_t;
typedef struct lam_datatype_t lam_datatype_t;
typedef struct lam_datavec_element_t lam_datavec_element_t;
typedef struct lam_datavec_t lam_datavec_t;
typedef struct lam_dataxdr_t lam_dataxdr_t;
typedef struct lam_pack_state_t lam_pack_state_t;


/* enums **************************************************************/

/**
 * Datatype state flags
 */
enum lam_datatype_state_t {
    LAM_DATATYPE_STATE_COMMITTED      = 1 << 0,
    LAM_DATATYPE_STATE_CONTIGUOUS     = 1 << 1,
    LAM_DATATYPE_STATE_FORTRAN        = 1 << 2,
    LAM_DATATYPE_STATE_OPTIMIZED      = 1 << 3,
    LAM_DATATYPE_STATE_DONT_OPTIMIZE  = 1 << 4,
    LAM_DATATYPE_STATE_XDR            = 1 << 5,
    /* etc. */
};
typedef enum lam_datatype_state_t lam_datatype_state_t;


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
typedef enum lam_datatype_kind_t lam_datatype_kind_t;


/**
 * lam_checksum_kind_t - checksum types
 */
enum lam_checksum_kind_t {
    LAM_CHECKSUM_KIND_NONE = 0,
    LAM_CHECKSUM_KIND_CRC32,
    LAM_CHECKSUM_KIND_SUM32,
    LAM_CHECKSUM_KIND_SUM64
};
typedef enum lam_checksum_kind_t lam_checksum_kind_t;


/* structs ************************************************************/

/**
 * State of incremental memcpy with checksum or CRC
 */
typedef struct lam_memcpy_state_t {
    size_t size;           /**< total size in bytes of the object
                            * being checksummed / CRCed */
    size_t partial_size;   /**< size of non- uint32_t to be carried
                            * over to next call */
    uint32_t partial_int;  /**< value of non- uint32_t to be carried
                            * over to next call */ 
    uint32_t sum;          /**< current value of the CRC or
                            * checksum */
    bool first_call;       /**< is this the first call for this
                            * checksum/CRC? */
} lam_memcpy_state_t;


/**
 * Internal representation of MPI datatype
 */
struct lam_datatype_t {

    lam_object_t d_super;       /**< object super class */
    char d_name[MPI_MAX_OBJECT_NAME]; /**< object name */
    int d_flags;                  /**< bit flags */

    /* cached information */

    ssize_t d_lower_bound;
    size_t d_extent;
    size_t d_packed_size;       /**< size in bytes, ignoring gaps */
    int d_nbasic;                /**< number of basic elements */

    /* optimized representation */

    size_t d_datavec_size;        /**< size of optimized representation */
    lam_datavec_t *d_datavec;     /**< optimized representation (may be null) */

    /* XDR representation */

    size_t d_dataxdr_size;        /**< size of XDR representation */
    lam_dataxdr_t *d_dataxdr;     /**< XDR representation (may be null) */

    /* full representation (c.f. MPI_Type_create_struct) */

    struct {
        lam_datatype_kind_t c_kind;     /**< creation function */
        int c_count;              /**< number of blocks */
        int *c_blocklengths;      /**< number of elements in each block */
        MPI_Aint *c_offset;       /**< stride/displacement as appropriate */
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
    size_t dve_size;                /**< size in bytes of element */
    ssize_t dve_offset;             /**< offset from start of data type */
    ssize_t dve_seq_offset;         /**< offset from start of packed data type */
};


/**
 * XDR representation of a datatype
 */
struct lam_dataxdr_element_t {
    /* to be done */
    void *x_xdrs;                 /**< XDR stream */
};


/**
 * Function protoype to do a memcpy with checksum
 */
typedef void *(*lam_memcpy_fn_t)(void *dst, const void *src, size_t size, void *csum);


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
 * @param csum          Pointer to checksum or CRC
 * @return              0 on success, -1 on error
 */
int lam_datatype_copy(void *dst,
                      const void *src,
                      size_t count,
                      lam_datatype_t *datatype,
                      lam_memcpy_fn_t *memcpy_fn,
                      void *csum);

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
                         lam_checksum_t *checksum);

/**
 * Pack state
 *
 * Structure to store the state of an incremental pack/unpack of a
 * datatype.
 */
struct lam_pack_state_t {
    size_t current_offset_packed;  /**< current offset into packed buffer */
    size_t current_type;           /**< current index of datatype */
    size_t current_repeat;         /**< current index of datavec repeat */
    size_t current_element;        /**< current index of datavec element */
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
 * @param csum          pointer to checksum
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
                      void *csum);


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
 * @param csum          pointer to checksum
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
                        void *csum);

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
                               void *csum);


/*
 * incremental memcpy with checksum / CRC functions
 */


/**
 * initialize the state for an incremental memcpy with checksum / CRC
 *
 * @param state     pointer to state object for the current sequence of copies
 * @param sum_size  the length of the entire buffer to be checksummed
 */
static inline void lam_memcpy_init(lam_memcpy_state_t *state, size_t sum_size)
{
    state->size = sum_size;
    state->first_call = true;
}


/**
 * Copy data from one buffer to another and calculate a 32-bit checksum
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    pointer to a memcpy with checksum/CRC state structure (ignored)
 * @return         the original value of dst
 */
static inline void *lam_memcpy(void *dst, const void *src, size_t size,
                               lam_memcpy_state_t *state)
{
    return memcpy(dst, src, size);
}

#if 0
uint32_t lam_crc32(const void *restrict buffer, size_t size, uint32_t initial_crc);
uint32_t lam_sum32(const void *restrict buffer, size_t size, uint32_t initial_crc);
void *lam_memcpy_sum32(void *dst, const void *src, size_t size,
                       lam_memcpy_state_t *state);
void *lam_memcpy_crc32(void *dst, const void *src, size_t size,
                       lam_memcpy_state_t *state);
#endif

/**
 * Copy data from one buffer to another and calculate a 32-bit checksum
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    pointer to a memcpy with checksum/CRC state structure
 * @return         the original value of dst
 */


/**
 * Copy data from one buffer to another and calculate a 32-bit checksum
 *
 * @param dst      pointer to the destination buffer
 * @param src      pointer to the source buffer
 * @param size     size of the buffer
 * @param state    pointer to a memcpy with checksum/CRC state structure
 * @return         the original value of dst
 */


#endif                          /* LAM_DATATYPE_H_INCLUDED */
