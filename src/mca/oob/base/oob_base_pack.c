/*
 * $HEADER$
 */

/** @file **/
/**
 * The pack and unpack routines for the oob
 */
#include "mca/oob/base/base.h"
#include <string.h>
#include <netinet/in.h>

/*
 * This function packs the passed data according to the type enum.
 *
 * @param dest the destination for the packed data
 * @param src the source of the data
 * @param n the number of elements in the src
 * @param type the type of data
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 */
int mca_oob_base_pack(void * dest, void * src, size_t n, mca_oob_base_types_t type)
{
    int i;
    uint16_t * d16 = (uint16_t *) dest;
    uint16_t * s16 = (uint16_t *) src;
    uint32_t * d32 = (uint32_t *) dest;
    uint32_t * s32 = (uint32_t *) src;

    switch(type) {
        case MCA_OOB_BASE_BYTE:
        case MCA_OOB_BASE_PACKED:
        case MCA_OOB_BASE_BYTE_BY_REF:
            memcpy(dest, src, n);
            break;
        case MCA_OOB_BASE_INT16:
            for (i=0;i<n;i++) {
                /* convert the host order to network order */
                d16[i] = htons(s16[i]);
            }
            break;
        case MCA_OOB_BASE_INT32:
            for (i=0;i<n;i++) {
                /* convert the host order to network order */
                d32[i] = htonl(s32[i]);
            }
            break;
        default:
            return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

/**
 * This function unpacks the passed data according to the type enum.
 *
 * @param dest the destination for the unpacked data
 * @param src the source of the packed data
 * @param n the number of elements in the src
 * @param type the type of the data to unpack
 *
 * @retval OMPI_SUCCESS
 * @retval OMPI_ERROR
 */
int mca_oob_base_unpack(void * dest, void * src, size_t n, mca_oob_base_types_t type)
{
    int i;
    uint16_t * d16 = (uint16_t *) dest;
    uint16_t * s16 = (uint16_t *) src;
    uint32_t * d32 = (uint32_t *) dest;
    uint32_t * s32 = (uint32_t *) src;

    switch(type) {
        case MCA_OOB_BASE_BYTE:
        case MCA_OOB_BASE_PACKED:
        case MCA_OOB_BASE_BYTE_BY_REF:
            memcpy(dest, src, n);
            break;
        case MCA_OOB_BASE_INT16:
            for (i=0;i<n;i++) {
                /* convert the network order to host order */
                d16[i] = ntohs(s16[i]);
            }
            break;
        case MCA_OOB_BASE_INT32:
            for (i=0;i<n;i++) {
                /* convert the network order to host order */
                d32[i] = ntohl(s32[i]);
            }
            break;
        default:
            return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

/*
 * This function packs null terminated strings
 *
 * @param dest the destination for the packed information
 * @param src the source of the information
 * @param maxlen the maximum length available in dest. the string will
 *               be truncated and a NULL added if it is longer then maxlen.
 *
 * @retval OMPI_SUCCESS
 */
int mca_oob_base_pack_string(void * dest, void * src, size_t maxlen)
{
    strncpy(dest, src, maxlen);
    /* add a null terminator at the end of dest since strncpy doesn't do it for us
     * if the src is longer than then dest  */
    *((char *) dest + maxlen - 1) = '\0';
    return OMPI_SUCCESS;
}

/*
 * This function unpacks strings
 *
 * @param dest the destination for the packed information
 * @param src the source of the information
 * @param maxlen the maximum length available in dest. the string will
 *               be truncated and a NULL added if it is longer then maxlen.
 *
 * @retval OMPI_SUCCESS
 */
int mca_oob_base_unpack_string(void * dest, void * src, size_t maxlen)
{
    strncpy(dest, src, maxlen);
    /* add a null terminator at the end of dest since strncpy doesn't do it for us
     * if the src is longer than then dest  */
    *((char *) dest + maxlen - 1) = '\0';
    return OMPI_SUCCESS;
}

