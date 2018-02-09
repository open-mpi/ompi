/*
 * Copyright (c) 2016-2018 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_MONITORING_COMM_H
#define MCA_OSC_MONITORING_COMM_H

#include <ompi/request/request.h>
#include <ompi/datatype/ompi_datatype.h>
#include <ompi/win/win.h>

#define OSC_MONITORING_GENERATE_TEMPLATE_COMM(template)                 \
                                                                        \
    static int ompi_osc_monitoring_## template ##_put (const void *origin_addr, \
                                                       int origin_count, \
                                                       ompi_datatype_t *origin_datatype, \
                                                       int target_rank, \
                                                       ptrdiff_t target_disp, \
                                                       int target_count, \
                                                       ompi_datatype_t *target_datatype, \
                                                       ompi_win_t *win) \
    {                                                                   \
        int world_rank;                                                 \
        /**                                                             \
         * If this fails the destination is not part of my MPI_COM_WORLD \
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank \
         */                                                             \
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(target_rank, win->w_group, &world_rank)) { \
            size_t type_size, data_size;                                \
            ompi_datatype_type_size(origin_datatype, &type_size);       \
            data_size = origin_count*type_size;                         \
            mca_common_monitoring_record_osc(world_rank, data_size, SEND); \
            OPAL_MONITORING_PRINT_INFO("MPI_Put to %d intercepted", world_rank); \
        }                                                               \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_put(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_rput (const void *origin_addr, \
                                                        int origin_count, \
                                                        ompi_datatype_t *origin_datatype, \
                                                        int target_rank, \
                                                        ptrdiff_t target_disp, \
                                                        int target_count, \
                                                        ompi_datatype_t *target_datatype, \
                                                        ompi_win_t *win, \
                                                        ompi_request_t **request) \
    {                                                                   \
        int world_rank;                                                 \
        /**                                                             \
         * If this fails the destination is not part of my MPI_COM_WORLD \
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank \
         */                                                             \
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(target_rank, win->w_group, &world_rank)) { \
            size_t type_size, data_size;                                \
            ompi_datatype_type_size(origin_datatype, &type_size);       \
            data_size = origin_count*type_size;                         \
            mca_common_monitoring_record_osc(world_rank, data_size, SEND); \
            OPAL_MONITORING_PRINT_INFO("MPI_Rput to %d intercepted", world_rank); \
        }                                                               \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_rput(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, request); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_get (void *origin_addr, int origin_count, \
                                                       ompi_datatype_t *origin_datatype, \
                                                       int source_rank, \
                                                       ptrdiff_t source_disp, \
                                                       int source_count, \
                                                       ompi_datatype_t *source_datatype, \
                                                       ompi_win_t *win) \
    {                                                                   \
        int world_rank;                                                 \
        /**                                                             \
         * If this fails the destination is not part of my MPI_COM_WORLD \
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank \
         */                                                             \
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(source_rank, win->w_group, &world_rank)) { \
            size_t type_size, data_size;                                \
            ompi_datatype_type_size(origin_datatype, &type_size);       \
            data_size = origin_count*type_size;                         \
            mca_common_monitoring_record_osc(world_rank, 0, SEND);      \
            mca_common_monitoring_record_osc(world_rank, data_size, RECV); \
            OPAL_MONITORING_PRINT_INFO("MPI_Get to %d intercepted", world_rank); \
        }                                                               \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_get(origin_addr, origin_count, origin_datatype, source_rank, source_disp, source_count, source_datatype, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_rget (void *origin_addr, int origin_count, \
                                                        ompi_datatype_t *origin_datatype, \
                                                        int source_rank, \
                                                        ptrdiff_t source_disp, \
                                                        int source_count, \
                                                        ompi_datatype_t *source_datatype, \
                                                        ompi_win_t *win, \
                                                        ompi_request_t **request) \
    {                                                                   \
        int world_rank;                                                 \
        /**                                                             \
         * If this fails the destination is not part of my MPI_COM_WORLD \
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank \
         */                                                             \
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(source_rank, win->w_group, &world_rank)) { \
            size_t type_size, data_size;                                \
            ompi_datatype_type_size(origin_datatype, &type_size);       \
            data_size = origin_count*type_size;                         \
            mca_common_monitoring_record_osc(world_rank, 0, SEND);      \
            mca_common_monitoring_record_osc(world_rank, data_size, RECV); \
            OPAL_MONITORING_PRINT_INFO("MPI_Rget to %d intercepted", world_rank); \
        }                                                               \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_rget(origin_addr, origin_count, origin_datatype, source_rank, source_disp, source_count, source_datatype, win, request); \
    }

#endif /* MCA_OSC_MONITORING_COMM_H */

