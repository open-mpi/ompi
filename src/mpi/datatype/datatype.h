/*
 * $HEADER$
 */

/** @file */

/*
 * LAM internal data type representation
 */

#ifndef LAM_DATATYPE_H_INCLUDED
#define LAM_DATATYPE_H_INCLUDED 1

/* typedefs ***********************************************************/

typedef enum lam_checksum_kind_t lam_checksum_kind_t;
typedef enum lam_checksum_kind_t lam_checksum_kind_t;
typedef enum lam_datatype_creator_t lam_dtype_creator_t;
typedef enum lam_datatype_state_t lam_datatype_state_t;
typedef struct lam_checksum_t lam_checksum_t;
typedef struct lam_datatype_t lam_datatype_t;
typedef struct lam_datatype_t lam_datatype_t;
typedef struct lam_datavec_element_t lam_datavec_element_t;
typedef struct lam_datavec_t lam_datavec_t;
typedef struct lam_dataxdr_t lam_dataxdr_t;


/* enums **************************************************************/

/**
 * Datatype state flags
 */
enum lam_datatype_state_t {
    LAM_DATATYPE_STATE_COMMITTED = 1 << 0,
    LAM_DATATYPE_STATE_FORTRAN = 1 << 1,
    LAM_DATATYPE_STATE_OPTIMIZED = 1 << 2,
    LAM_DATATYPE_STATE_DONT_OPTIMIZE = 1 << 3,
    LAM_DATATYPE_STATE_XDR = 1 << 4,
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


/* structs ************************************************************/

/**
 * Abstraction of checksum for data
 */
struct lam_checksum_t {
    lam_checksum_kind_t kind;
    union {
        uint64_t sum64;
        uint32_t sum32;
        uint32_t crc32;
    } sum;
};


/**
 * Internal representation of MPI datatype
 */
struct lam_datatype_t {

    lam_object_t super;         /**< object super class */
    char name[MPI_MAX_OBJECT_NAME]; /**< object name */
    int flags;                  /**< bit flags */

    /* cached information */

    ssize_t lower_bound;
    size_t extent;
    size_t packed_size;         /**< size in bytes, ignoring gaps */
    int nbasic;                 /**< number of basic elements */

    /* optimized representation */

    size_t datavec_size;        /**< size of optimized representation */
    lam_datavec_t *datavec;     /**< optimized representation (may be null) */

    /* XDR representation */

    size_t dataxdr_size;        /**< size of XDR representation */
    lam_dataxdr_t *dataxdr;     /**< XDR representation (may be null) */

    /* full representation (c.f. MPI_Type_create_struct) */

    struct {
        lam_datatype_kind_t kind;     /**< creation function */
        int count;              /**< number of blocks */
        int *blocklengths;      /**< number of elements in each block */
        MPI_Aint *offset;       /**< stride/displacement as appropriate */
        lam_datatype_t **types; /**< array of types (array) */
    } creator;
};


/**
 * An optimized representation of noncontiguous data used by packing
 * routines
 */
struct lam_datavec_t {
    size_t nrepeat;
    ssize_t repeat_offset;
    size_t nelement;
    lam_datavec_element_t *element;
};


/**
 * An element of a data type in optimized form
 */
struct lam_datavec_element_t {
    size_t size;                /**< size in bytes of element */
    ssize_t offset;             /**< offset from start of data type */
    ssize_t seq_offset;         /**< offset from start of packed data type */
};


/**
 * XDR representation of a datatype
 */
struct lam_dataxdr_element_t {
    /* to be done */
    void *xdrs;                 /**< XDR stream */
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
 * @param dest          Output data type array
 * @param src           Input data type array
 * @param count         Size of array
 * @param datatype      Datatype descriptor
 * @param checksum      Checksum
 * @return              0 on success, -1 on error
 */
int lam_datatype_copy(void *dest,
                      const void *src,
                      size_t count,
                      lam_datatype_t *datatype,
                      lam_checksum_t *checksum);

/**
 * Pack state
 *
 * Structure to store the state of an incremental pack/unpack of a
 * datatype.
 */
struct lam_pack_state_t {
    size_t current_offset;         /**< current offset into packed buffer */
    size_t current_type;           /**< current index of datatype */
    size_t current_repeat;         /**< current index of datavec repeat */
    size_t current_element;        /**< current index of datavec element */
    size_t current_offset;         /**< current offset into datavec element */
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
 * @param checksum      checksum descriptor
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
                      lam_checksum_t *checksum);


/**
 * Incrementally unpack a buffer to an array of datatypes
 *
 * @param state         current state of the incremental pack/unpack
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param buf           buffer to pack into/unpack from
 * @param bufsize       size of buffer
 * @param datatype      type descriptor
 * @param checksum      checksum descriptor
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
                        lam_checksum_t *checksum);


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
                               lam_checksum_t *checksum);

#endif                          /* LAM_DATATYPE_H_INCLUDED */
