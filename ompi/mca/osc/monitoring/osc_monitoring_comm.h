/*
 * Copyright (c) 2016 Inria.  All rights reserved.
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
                                                       OPAL_PTRDIFF_TYPE target_disp, \
                                                       int target_count, \
                                                       ompi_datatype_t *target_datatype, \
                                                       ompi_win_t *win) \
    {                                                                   \
        int world_rank;                                                 \
        /**                                                             \
         * If this fails the destination is not part of my MPI_COM_WORLD \
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank \
         */                                                             \
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(target_rank, ompi_osc_monitoring_## template ##_get_comm(win), &world_rank)) { \
            size_t type_size, data_size;                                \
            ompi_datatype_type_size(origin_datatype, &type_size);       \
            data_size = origin_count*type_size;                         \
            mca_common_monitoring_send_data(world_rank, data_size, 1);  \
            OPAL_MONITORING_VERBOSE(10, "MPI_Put to %d intercepted", world_rank); \
        }                                                               \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_put(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_rput (const void *origin_addr, \
                                                        int origin_count, \
                                                        ompi_datatype_t *origin_datatype, \
                                                        int target_rank, \
                                                        OPAL_PTRDIFF_TYPE target_disp, \
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
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(target_rank, ompi_osc_monitoring_## template ##_get_comm(win), &world_rank)) { \
            size_t type_size, data_size;                                \
            ompi_datatype_type_size(origin_datatype, &type_size);       \
            data_size = origin_count*type_size;                         \
            mca_common_monitoring_send_data(world_rank, data_size, 1);  \
            OPAL_MONITORING_VERBOSE(10, "MPI_Rput to %d intercepted", world_rank); \
        }                                                               \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_rput(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, win, request); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_get (void *origin_addr, int origin_count, \
                                                       ompi_datatype_t *origin_datatype, \
                                                       int source_rank, \
                                                       OPAL_PTRDIFF_TYPE source_disp, \
                                                       int source_count, \
                                                       ompi_datatype_t *source_datatype, \
                                                       ompi_win_t *win) \
    {                                                                   \
        int world_rank;                                                 \
        /**                                                             \
         * If this fails the destination is not part of my MPI_COM_WORLD \
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank \
         */                                                             \
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(source_rank, ompi_osc_monitoring_## template ##_get_comm(win), &world_rank)) { \
            size_t type_size, data_size;                                \
            ompi_datatype_type_size(origin_datatype, &type_size);       \
            data_size = origin_count*type_size;                         \
            mca_common_monitoring_send_data(world_rank, 0, -1);         \
            mca_common_monitoring_recv_data(world_rank, data_size, 1);  \
            OPAL_MONITORING_VERBOSE(10, "MPI_Get to %d intercepted", world_rank); \
        }                                                               \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_get(origin_addr, origin_count, origin_datatype, source_rank, source_disp, source_count, source_datatype, win); \
    }                                                                   \
                                                                        \
    static int ompi_osc_monitoring_## template ##_rget (void *origin_addr, int origin_count, \
                                                        ompi_datatype_t *origin_datatype, \
                                                        int source_rank, \
                                                        OPAL_PTRDIFF_TYPE source_disp, \
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
        if(OPAL_SUCCESS == mca_common_monitoring_get_world_rank(source_rank, ompi_osc_monitoring_## template ##_get_comm(win), &world_rank)) { \
            size_t type_size, data_size;                                \
            ompi_datatype_type_size(origin_datatype, &type_size);       \
            data_size = origin_count*type_size;                         \
            mca_common_monitoring_send_data(world_rank, 0, -1);         \
            mca_common_monitoring_recv_data(world_rank, data_size, 1);  \
            OPAL_MONITORING_VERBOSE(10, "MPI_Rget to %d intercepted", world_rank); \
        }                                                               \
        return OMPI_OSC_MONITORING_MODULE_VARIABLE(template).osc_rget(origin_addr, origin_count, origin_datatype, source_rank, source_disp, source_count, source_datatype, win, request); \
    }

#endif /* MCA_OSC_MONITORING_COMM_H */

