/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_PTPCOLL_UTILS_H
#define MCA_BCOL_PTPCOLL_UTILS_H

#include "ompi_config.h"

#include "ompi/mca/rte/rte.h"

BEGIN_C_DECLS

/*
 *  Return closet power of K, for the number
 */
int pow_k_calc(int k, int number, int *out_number);

/*
 * Communicator rank to group index conversion function for K-nomial tree.
 */
int get_group_index_for_k_nomial(int my_group_index, int comm_source, int radix, int group_size, int *group_array);

/* the same like above, just more information on return */
int get_group_index_and_distance_for_k_nomial(int my_group_index, int comm_source, int radix, 
        int group_size, int *group_array, int *pow_distance);

int get_group_index_and_distance_for_binomial(int my_group_index, int comm_source,
        int group_size, int *group_array, int *pow_distance);
/*
 * Error and debug Macros/Functions
 */
static inline int mca_bcol_ptpcoll_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}

#define PTPCOLL_ERROR(args)                                         \
    do {                                                            \
        mca_bcol_ptpcoll_err("[%s]%s[%s:%d:%s] PTPCOLL ",           \
                ompi_process_info.nodename,                         \
                OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),                 \
                __FILE__, __LINE__, __func__);                      \
        mca_bcol_ptpcoll_err args;                                  \
        mca_bcol_ptpcoll_err("\n");                                 \
    } while(0)

#if OPAL_ENABLE_DEBUG
#define PTPCOLL_VERBOSE(level, args)                                \
    do {                                                            \
        if (mca_bcol_ptpcoll_component.verbose >= level) {          \
            mca_bcol_ptpcoll_err("[%s]%s[%s:%d:%s] PTPCOLL ",       \
                    ompi_process_info.nodename,                     \
                    OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),             \
                    __FILE__, __LINE__, __func__);                  \
            mca_bcol_ptpcoll_err args;                              \
            mca_bcol_ptpcoll_err("\n");                             \
        }                                                           \
    } while(0)
#else
#define PTPCOLL_VERBOSE(level, args)
#endif

END_C_DECLS

#endif
