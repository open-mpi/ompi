/*
 * $HEADER$
 */

/** @file */

#ifdef DATATYPES_ARE_READY

#include "lam_config.h"
#include "lam/datatype.h"


lam_class_info_t lam_datatype_cls = {
    "lam_datatype_t",
    &lam_dbl_item_cls,
    (class_init_t) lam_p2p_cdi_init,
    (class_destroy_t) lam_p2p_cdi_destroy
};


static int lam_datatype_init = 0;
lam_dbl_list_t lam_p2p_cdis;


void lam_datatype_t(lam_p2p_cdi_t* cdi)
{
    if(fetchNset(&lam_p2p_cdis_init,1) == 0) {
        lam_dbl_init(&lam_p2p_cdis);
    }
    lam_dbl_item_init(&cdi->cdi_base);
    cdi->cdi_name = 0;
    cdi->cdi_id = lam_dbl_get_size(&lam_p2p_cdis) + 1;
    cdi->cdi_frag_first_size = 0;
    cdi->cdi_frag_min_size = 0;
    cdi->cdi_frag_max_size = 0;
    cdi->cdi_endpoint_latency = 0;
    cdi->cdi_endpoint_bandwidth = 0;
    cdi->cdi_endpoint_count = 0;
    lam_dbl_init(&cdi->cdi_incomplete_sends);
    lam_dbl_append(&lam_p2p_cdis, &cdi->cdi_base);
}


void lam_p2p_cdi_destroy(lam_p2p_cdi_t* cdi)
{
    lam_dbl_remove(&lam_p2p_cdis, &cdi->cdi_base);
    lam_dbl_destroy(&cdi->cdi_incomplete_sends);
    lam_dbl_item_destroy(&cdi->cdi_base);
}


/*
 * This random stuff checked in while I think about things ...
 */

/**
 * type_pack -- Incrementally copy data type arrays to/from a packed buffer
 *
 * @param pack          direction of copy: PACK or UNPACK
 * @param buf           buffer to pack into/unpack from
 * @param bufsize       size of buffer
 * @param offset        pointer to current byte offset into the buffer
 * @param typebuf       array of types
 * @param ntype         size of type array
 * @param type          type descriptor
 * @param type_index    pointer to index of current type
 * @param map_index     pointer to index of current type map
 * @param map_offset    pointer to byte offset into current type map
 * @return              0 if pack/unpack is complete, non-zero otherwise
 *
 * Incrementally copy data type arrays to/from a packed buffer.  by
 * iterating over the type and type_map until we finish or run out of
 * room.
 *
 * The contents of type_index, map_index and map_offset should be
 * initialized to 0 before the first call.
 */

enum {
    TYPE_PACK_PACK = 0,
    TYPE_PACK_UNPACK,
    TYPE_PACK_COMPLETE = 0,
    TYPE_PACK_INCOMPLETE_VECTOR,
    TYPE_PACK_INCOMPLETE_TYPE,
    TYPE_PACK_INCOMPLETE_TYPE_MAP
};

enum lam_packer_direction_t {
    LAM_PACKER_PACK = 0,
    LAM_PACKER_UNPACK,
};

enum lam_packer_status_t {
    TYPE_PACK_COMPLETE = 0,
    TYPE_PACK_INCOMPLETE_VECTOR,
    TYPE_PACK_INCOMPLETE_TYPE,
    TYPE_PACK_INCOMPLETE_TYPE_MAP
};

struct lam_packer_state_t {
    int do_checksum;
    int do_crc;
    size_t current_datatype;
    size_t current_repeat;
    size_t current_element;
    size_t *current_offset;
    lam_checksum_t checksum;
    lam_crc_t crc;
};

/**
 * lam_datatype_copy - Copy (the contents of) an array of data types
 *
 * @param dest          output data type array
 * @param src           input data type array
 * @param count         size of array
 * @param type          type descriptor
 */
lam_packer_status_t lam_packer(lam_packer_direction_t direction,
                               void *buf,
                               size_t bufsize,
                               size_t *offset,
                               void *typebuf,
                               size_t ntype,
                               lam_datatype_t *datatype,
                               lam_pack_state_t *pack_state,
                               lam_checksum_t *checksum)
{

}



/**
 * lam_datatype_copy - Copy (the contents of) an array of data types
 *
 * @param dest          output data type array
 * @param src           input data type array
 * @param count         size of array
 * @param type          type descriptor
 */
void lam_datatype_copy(void *dest,
                       const void *src,
                       size_t count,
                       lam_datatype_t *datatype,
                       lam_checksum_t *checksum)
{
    if (datatype == NULL) {
        memmove(dest, src, count);
    } else if (datatype->layout == CONTIGUOUS) {
        memmove(dest, src, count * datatype->extent);
    } else {
        unsigned char *p = ((unsigned char *) dest);
        unsigned char *q = ((unsigned char *) src);
        int map;

        while (count--) {
            for (map = 0; map < datatype->num_pairs; map++) {
                memmove(p + datatype->type_map[map].offset,
                        q + datatype->type_map[map].offset,
                        datatype->type_map[map].size);
            }
            p += datatype->extent;
            q += datatype->extent;
        }
    }
}

#endif
