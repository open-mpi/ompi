/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 *
 * Copyright (c) 2008      Voltaire. All rights reserved
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/util/argv.h"
#include "opal/constants.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/util/output.h"

static int opal_paffinity_base_socket_to_cpu_set(char **socket_list, int socket_cnt, long rank)
{
    int i;
    char **range;
    int range_cnt;
    int lower_range, upper_range;
    int processor_id, num_processors, socket=-1, core=-1;
    int max_processor_id;
    int rc;
    opal_paffinity_base_cpu_set_t cpumask;

    if (OPAL_SUCCESS != (rc = opal_paffinity_base_get_processor_info(&num_processors, &max_processor_id))) {
        return OPAL_ERROR;
    }
    OPAL_PAFFINITY_CPU_ZERO(cpumask);
    for (i=0; i<socket_cnt; i++) {
        if (0 == strcmp("*", socket_list[i])) {
            for ( processor_id=0; processor_id<=max_processor_id; processor_id++) {
                OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                if (OPAL_SUCCESS != ( rc = opal_paffinity_base_set(cpumask))) {
                    return OPAL_ERROR;
                }
                opal_paffinity_base_map_to_socket_core(processor_id, &socket, &core);
                opal_output_verbose(5, opal_paffinity_base_output,
                        "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d )",rank, processor_id, socket, core);
            }
            continue;
        }
        range = opal_argv_split(socket_list[i],'-');
        range_cnt = opal_argv_count(range);
        switch (range_cnt) {
            case 1:
                processor_id = atoi(range[0]);
                if (processor_id > max_processor_id) {
                    opal_output(0, "ERROR !!! processor_id (%d) > max_processor_id(%d), modify rankfile and run again\n",processor_id, max_processor_id);
                    return OPAL_ERROR;
                }
                OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                if (OPAL_SUCCESS != ( rc = opal_paffinity_base_set(cpumask))) {
                    return OPAL_ERROR;
                }
                opal_paffinity_base_map_to_socket_core(processor_id, &socket, &core);
                opal_output_verbose(5, opal_paffinity_base_output,
                                    "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d )",
                                    rank, processor_id, socket, core);
                break;
            case 2:
                lower_range = atoi(range[0]);
                upper_range = atoi(range[1]);
                if (max_processor_id < upper_range || lower_range >= upper_range ) {
                    opal_output(0,"Error !!! Check your boundaries %d < %d(max_cpu) < %d, modify rankfile and run again\n",
                                lower_range, max_processor_id, upper_range);
                    return OPAL_ERROR;
                }
                for (processor_id=lower_range; processor_id<=upper_range; processor_id++) {
                    OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                    if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                        return OPAL_ERROR;
                    }
                    opal_paffinity_base_map_to_socket_core(processor_id, &socket, &core);
                    opal_output_verbose(5, opal_paffinity_base_output,
                                        "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d )",
                                        rank, processor_id, socket, core);
                }
                break;
            default:
                opal_argv_free(range);
                return OPAL_ERROR;
        }
        opal_argv_free(range);
    }
    return OPAL_SUCCESS;
}

static int opal_paffinity_base_socket_core_to_cpu_set(char **socket_core_list, int socket_core_list_cnt, long rank)
{
        int rc, i;
        char **socket_core;
        int socket_core_cnt;
        char **range;
        int range_cnt;
        int lower_range, upper_range;
        int socket, core, processor_id ;
        int max_socket_num, max_core_num;
        int num_sockets, num_cores;
        opal_paffinity_base_cpu_set_t cpumask;
        
        socket_core = opal_argv_split (socket_core_list[0], ':');
        socket_core_cnt = opal_argv_count(socket_core);
        OPAL_PAFFINITY_CPU_ZERO(cpumask);
        socket = atoi(socket_core[0]);
        
        if ( OPAL_SUCCESS != ( rc = opal_paffinity_base_get_socket_info(&num_sockets, &max_socket_num))) {
            return OPAL_ERROR;
        }
        
        if ( socket > max_socket_num) {
            opal_output(0,"ERROR !!! socket(%d) > max_socket_num(%d), modify rankfile and run again",
                        socket, max_socket_num);
            return OPAL_ERROR;
        }
        if ( OPAL_SUCCESS != ( rc = opal_paffinity_base_get_core_info(socket, &num_cores, &max_core_num))) {
            opal_output(0,"Error !!! Invalid socket number (%d) in rankfile, modify rankfile and run again\n",
                           socket);
            return OPAL_ERROR;
        }
        
        if (0 == strcmp("*",socket_core[1])) {
            for (core = 0; core <= max_core_num; core++) {
                if ( OPAL_SUCCESS != (rc = opal_paffinity_base_map_to_processor_id (socket, core, &processor_id))) {
                    return OPAL_ERROR;
                }
                OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                    return OPAL_ERROR;
                }
                opal_output_verbose(5, opal_paffinity_base_output,
                                    "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d)",
                                    rank, processor_id, socket, core);
            }
        } else {
            range = opal_argv_split(socket_core[1], '-');
            range_cnt = opal_argv_count(range);
            switch (range_cnt) {
                case 1:
                    core = atoi(range[0]);
                    if ( core > max_core_num  ) {
                        opal_output(0,"Error!!! core(%d)>max_core(%d) on socket %d, modify rankfile and run again\n",
                                core, max_core_num, socket);
                        return OPAL_ERROR;
                    }
                    if ( OPAL_SUCCESS != (rc = opal_paffinity_base_map_to_processor_id (socket, core, &processor_id))) {
                        return OPAL_ERROR;
                    }
                    OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                    if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                        return OPAL_ERROR;
                    }
                    opal_output_verbose(5, opal_paffinity_base_output,
                                        "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d)",
                                        rank, processor_id, socket, core);
                    break;
                case 2:
                    lower_range = atoi(range[0]);
                    upper_range = atoi(range[1]);
                    if ( 0 > lower_range || max_core_num < upper_range || lower_range >= upper_range ) {
                        opal_output(0,"Error !!! Check your boundaries %d < %d(max_core) < %d ,modify rankfile and run again\n",
                                lower_range, max_core_num, upper_range);
                        return OPAL_ERROR;
                    }
                    for (core=lower_range; core<=upper_range; core++) {
                        if ( OPAL_SUCCESS != (rc = opal_paffinity_base_map_to_processor_id (socket, core, &processor_id))) {
                            return OPAL_ERROR;
                        }
                        OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                        if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                            return OPAL_ERROR;
                        }
                        opal_output_verbose(5, opal_paffinity_base_output,
                                            "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d)",
                                            rank, processor_id, socket, core);
                    }
                    break;
                default:
                    opal_argv_free(range);
                    opal_argv_free(socket_core);
                    return OPAL_ERROR;
            }
            opal_argv_free(range);
            opal_argv_free(socket_core);
        }
        for (i=1; i<socket_core_list_cnt; i++) {
            socket_core = opal_argv_split (socket_core_list[i], ':');
            socket_core_cnt = opal_argv_count(socket_core);
            switch (socket_core_cnt) {
                case 1:
                    range = opal_argv_split(socket_core[0], '-');
                    range_cnt = opal_argv_count(range);
                    switch (range_cnt) {
                        case 1:
                            core = atoi(range[0]);
                            /* use PLPA to construct the child->cpu_set */
                            if ( core > max_core_num ) {
                                opal_output(0,"Error !!! core(%d) > max_core(%d), modify rankfile and run again\n",
                                            core, max_core_num);
                                return OPAL_ERROR;
                            }
                            if ( OPAL_SUCCESS != (rc = opal_paffinity_base_map_to_processor_id (socket, core, &processor_id))) {
                                opal_output(0,"Error !!! Invalid socket:core pair (%d:%d), modify rankfile and run again\n",
                                            socket, core);
                                return OPAL_ERROR;
                            }
                            OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                            if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                                return OPAL_ERROR;
                            }
                            opal_output_verbose(5, opal_paffinity_base_output,
                                                "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d)",
                                                rank, processor_id, socket, core);
                            break;
                        case 2:
                            lower_range = atoi(range[0]);
                            upper_range = atoi(range[1]);
                            if ( 0 > lower_range || max_core_num < upper_range || lower_range >= upper_range) {
                                opal_output(0,"Error !!! Check your boundaries %d < %d(max_core) < %d, modify rankfile and run again\n",
                                        lower_range, max_core_num, upper_range);
                                return OPAL_ERROR;
                            }
                            for (core=lower_range; core<=upper_range; core++) {
                                if ( OPAL_SUCCESS != (rc = opal_paffinity_base_map_to_processor_id (socket, core, &processor_id))) {
                                    return OPAL_ERROR;
                                }
                                OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                                if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                                    return OPAL_ERROR;
                                }
                                opal_output_verbose(5, opal_paffinity_base_output,
                                                    "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d)",
                                                    rank, processor_id, socket, core);
                            }
                            break;
                        default:
                            opal_argv_free(range);
                            opal_argv_free(socket_core);
                            return OPAL_ERROR;
                    }
                    opal_argv_free(range);
                    break;
                case 2:
                    socket = atoi(socket_core[0]);
                    if (0 == strcmp("*",socket_core[1])) {
                        for (core=0; core<=max_core_num; core++) {
                            if ( OPAL_SUCCESS != (rc = opal_paffinity_base_map_to_processor_id ( socket, core, &processor_id))) {
                                return OPAL_ERROR;
                            }
                            OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                            if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                                return OPAL_ERROR;
                            }
                            opal_output_verbose(5, opal_paffinity_base_output,
                                                "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d)",
                                                rank, processor_id, socket, core);
                        }
                    } else {
                        range = opal_argv_split(socket_core[1], '-');
                        range_cnt = opal_argv_count(range);
                        socket = atoi(socket_core[0]);
                        switch (range_cnt) {
                            case 1:
                                core = atoi(range[0]);
                                if ( core > max_core_num ) {
                                    opal_output(0,"Error !!! max_core(%d) < core(%d), modify rankfile and run again\n", 
                                                core, max_core_num);
                                    return OPAL_ERROR;
                                }
                                if ( OPAL_SUCCESS != (rc = opal_paffinity_base_map_to_processor_id (socket, core, &processor_id))) {
                                    return OPAL_ERROR;
                                }
                                OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                                if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                                    return OPAL_ERROR;
                                }
                                opal_output_verbose(5, opal_paffinity_base_output,
                                                    "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d)",
                                                    rank, processor_id, socket, core);
                                break;
                            case 2:
                                lower_range = atoi(range[0]);
                                upper_range = atoi(range[1]);
                                if ( 0 > lower_range || max_core_num < upper_range || lower_range > upper_range) {
                                    opal_output(0,"Error !!! Check your boundaries %d < %d(max_core) < %d, modify rankfile and run again\n",
                                            lower_range, max_core_num, upper_range);
                                    return OPAL_ERROR;
                                }
                                for ( core = lower_range; core <= upper_range; core++) {
                                    if ( OPAL_SUCCESS != (rc = opal_paffinity_base_map_to_processor_id (socket, core, &processor_id))) {
                                        return OPAL_ERROR;
                                    }
                                    OPAL_PAFFINITY_CPU_SET(processor_id, cpumask);
                                    if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                                        return OPAL_ERROR;
                                    }
                                    opal_output_verbose(5, opal_paffinity_base_output,
                                                        "paffinity slot assignment: rank %ld runs on cpu #%d ( %d : %d)",
                                                        rank, processor_id, socket, core); 
                                }
                                break;
                            default:
                                opal_argv_free(range);
                                opal_argv_free(socket_core);
                                return OPAL_ERROR;
                        }
                        opal_argv_free(range);
                    }
                    break;
                default:
                    opal_argv_free(socket_core);
                    return OPAL_ERROR;
            }
            opal_argv_free(socket_core);
        }
        return OPAL_SUCCESS;
}

int opal_paffinity_base_slot_list_set(long rank)
{
    char *slot_str = NULL;
    char **item;
    char **socket_core;
    int item_cnt, socket_core_cnt, rc;

    rc = mca_base_param_find("opal", NULL, "paffinity_slot_list");
    if (rc >= 0) {
        if (OPAL_SUCCESS == mca_base_param_lookup_string(rc, &slot_str)) {
            if (NULL == slot_str) {
                return OPAL_SUCCESS;
            }
        }
    }else{
        return OPAL_SUCCESS;
    }

        opal_output_verbose(5, opal_paffinity_base_output, "paffinity slot assignment: slot_list == %s", slot_str);
        
        item = opal_argv_split (slot_str, ',');
        item_cnt = opal_argv_count (item);
        socket_core = opal_argv_split (item[0], ':');
        socket_core_cnt = opal_argv_count(socket_core);
        opal_argv_free(socket_core);
        switch (socket_core_cnt) {
            case 1:
                if (OPAL_SUCCESS != (rc = opal_paffinity_base_socket_to_cpu_set(item, item_cnt, rank))) {
                    opal_argv_free(item);
                    return OPAL_ERROR;
                }
                break;
            case 2:
                if (OPAL_SUCCESS != (rc = opal_paffinity_base_socket_core_to_cpu_set(item, item_cnt, rank))) {
                    opal_argv_free(item);
                    return OPAL_ERROR;
                }
                break;
            default:
                opal_argv_free(item);
                return OPAL_ERROR;
        }
        opal_argv_free(item);
        return OPAL_SUCCESS;
}
