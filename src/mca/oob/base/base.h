/*
 * $HEADER$
 */
/** @file:
 *
 * Contains packing functions provided by the oob
 */

#ifndef MCA_OOB_BASE_H_
#define MCA_OOB_BASE_H_

#include "ompi_config.h"

#include "include/types.h"
#include "mca/mca.h"
#include "mca/oob/oob.h"

/*
 * This is the first module on the list. This is here temporarily
 * to make things work
 */
mca_oob_t mca_oob;

/**
 * the module data structure
 */
struct mca_oob_base_module_t {
  ompi_list_item_t super;
  mca_oob_base_component_t *oob_component;
  mca_oob_t *oob_module;
};
typedef struct mca_oob_base_module_t mca_oob_base_module_t;

/**
 * declare the module structure as a class
 */
OBJ_CLASS_DECLARATION(mca_oob_base_module_t);


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    int mca_oob_base_open(void);
    int mca_oob_base_init(bool *allow_multi_user_threads, bool *have_hidden_threads);
    int mca_oob_base_close(void);

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
    int mca_oob_base_pack(void * dest, void * src, size_t n, mca_oob_base_type_t type);

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
    int mca_oob_base_unpack(void * dest, void * src, size_t n, mca_oob_base_type_t type);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Global struct holding the selected module's function pointers
 */
extern int mca_oob_base_output;
extern ompi_list_t mca_oob_base_components;
extern ompi_list_t mca_oob_base_modules;

#endif
