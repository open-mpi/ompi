/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/list.h"
#include "mpi/info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_create = PMPI_Info_create
#endif

/**
 * Create a new info object 
 *
 * @param info Pointer to the MPI_Info handle
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_ARG
 * @retval MPI_ERR_EXHAUSTED
 *
 * When an MPI_Info object is not being used, it should be freed using
 * MPI_Info_free
 */
int MPI_Info_create(MPI_Info *info) {
    /**
     * Anju:
     * These are miscelleneous notes. They will be removed later.
     *
     * MPI_Info is a pointer to the struct lam_info_t. This structure 
     * contains a pointer to the lam_list_t structure. lam_list_t 
     * structure is a generic list management structure which is a
     * container that can hold lam_list_item_t objects. Hence any 
     * object that needs to be stored as a list has to have lam_list_item_t
     * as its first element.
     * 
     * Steps to initialize the list item:
     * info -> pointer to MPI_Info
     * MPI_Info -> pointer to lam_info_t
     * lam_info_t -> has pointer to lam_list_t item.
     *
     * First, we initialize by allocating memory for lam_info_t
     * structure. Then, we have to initialize the list itself. Way to 
     * initialize the list is 
     * 
     * (*info) = (lam_info_t) LAM_MALLOC (sizeof(struct laminfo_t))
     * (*info)->list_pointer = (lam_list_t) LAM_MALLOC (sizeof(struct
     *                                                  lam_list_t))
     * lam_list_init((*info)->list_pointer)
     *
     */
    return MPI_SUCCESS;
}
