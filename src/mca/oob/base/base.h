/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef MCA_OOB_BASE_H_
#define MCA_OOB_BASE_H_

#include "ompi_config.h"

#include "include/types.h"
#include "mca/mca.h"
#include "mca/oob/oob.h"

/**
 * the different types one can use to describe the data
 */
typedef enum { 
    MCA_OOB_BASE_BYTE, /**< a byte of data */
    MCA_OOB_BASE_INT16, /**< a 16 bit integer */
    MCA_OOB_BASE_INT32, /**< a 32 bit integer */
    MCA_OOB_BASE_PACKED,/**< already packed data. */
    MCA_OOB_BASE_BYTE_BY_REF /**< indicates to try to use the data directly without copying */
} mca_oob_base_types_t;

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_oob_base_open(void);
    int mca_oob_base_select(bool *allow_multi_user_threads,
                            bool *have_hidden_threads);
    int mca_oob_base_close(void);

    bool mca_oob_base_is_checkpointable(void);

    int mca_oob_base_checkpoint(void);
    int mca_oob_base_continue(void);
    int mca_oob_base_restart(void);

/*
 * functions for pack and unpack routines
 */
/**
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
    int mca_oob_base_pack(void * dest, void * src, size_t n, mca_oob_base_types_t type);

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
    int mca_oob_base_unpack(void * dest, void * src, size_t n, mca_oob_base_types_t type);

/**
 * This function packs null terminated strings
 *
 * @param dest the destination for the packed information
 * @param src the source of the information
 * @param maxlen the maximum length available in dest. the string will
 *               be truncated and a NULL added if it is longer then maxlen.
 * 
 * @retval OMPI_SUCCESS
 */
    int mca_oob_base_pack_string(void * dest, void * src, size_t maxlen);
/**
 * This function unpacks strings
 *
 * @param dest the destination for the packed information
 * @param src the source of the information
 * @param maxlen the maximum length available in dest. the string will
 *               be truncated and a NULL added if it is longer then maxlen.
 *  
 * @retval OMPI_SUCCESS
 */
    int mca_oob_base_unpack_string(void * dest, void * src, size_t maxlen);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Global struct holding the selected module's function pointers
 */
extern int mca_oob_base_output;
extern ompi_list_t mca_oob_base_modules_available;
extern mca_oob_base_module_t mca_oob_base_selected_module;
extern mca_oob_t mca_oob;

#endif
