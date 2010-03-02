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

static bool diag_requested;

static int opal_paffinity_base_socket_to_cpu_set(char **socket_list, int socket_cnt, long rank, bool logical_map)
{
    int i;
    char **range;
    int range_cnt;
    int lower_range, upper_range;
    int processor_id, num_processors;
    int rc;
    int phys_processor;
    opal_paffinity_base_cpu_set_t cpumask;

    /* get the number of LOGICAL processors on this node */
    if (OPAL_SUCCESS != (rc = opal_paffinity_base_get_processor_info(&num_processors))) {
        return OPAL_ERROR;
    }
    OPAL_PAFFINITY_CPU_ZERO(cpumask);
    for (i=0; i<socket_cnt; i++) {
        if (0 == strcmp("*", socket_list[i])) {
            /* bind to all available logical processors - first set the bits in the cpu mask */
            for ( processor_id=0; processor_id<=num_processors; processor_id++) {
                if (0 > (phys_processor = opal_paffinity_base_get_physical_processor_id(processor_id))) {
                    opal_output(0, "Rank %ld: PAFFINITY cannot get physical processor id for logical processor %ld",
                                rank, (long)processor_id);
                    return OPAL_ERROR;
                }
                OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                /* output diagnostic if requested */
                if (diag_requested) {
                    opal_output(0, "paffinity slot assignment: rank %ld runs on physical cpu #%d (#%d)",
                                rank, phys_processor, processor_id);
                }
            }
            /* tell paffinity to bind us */
            if (OPAL_SUCCESS != ( rc = opal_paffinity_base_set(cpumask))) {
                return rc;
            }
           continue;
        } /* end if * */
        
        range = opal_argv_split(socket_list[i],'-');
        range_cnt = opal_argv_count(range);
        switch (range_cnt) {
            case 1:  /* no - was present, so just one processor given */
                processor_id = atoi(range[0]);
                if (logical_map) {
                    /* need to convert this to physical processor id */
                    if (0 > (phys_processor = opal_paffinity_base_get_physical_processor_id(processor_id))) {
                        opal_output(0, "Rank %ld: PAFFINITY cannot get physical processor id for logical processor %ld",
                                    rank, (long)processor_id);
                        return OPAL_ERROR;
                    }                    
                } else {
                    phys_processor = processor_id;
                }
                /* set the bit for this physical processor */
                OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                /* tell paffinity to bind us */
                if (OPAL_SUCCESS != ( rc = opal_paffinity_base_set(cpumask))) {
                    return rc;
                }
                /* output diagnostic if requested */
                if (diag_requested) {
                    opal_output(0, "paffinity slot assignment: rank %ld runs on cpu #%d (#%d)",
                                rank, phys_processor, processor_id);
                }
                break;
                
            case 2:  /* range of processor id's was given */
                lower_range = atoi(range[0]);
                upper_range = atoi(range[1]);
                if (num_processors < (upper_range - lower_range) ||
                    upper_range <= lower_range) {
                    opal_output(0,"Rank %ld: PAFFINITY Error !!! Check your boundaries lower %d upper %d #processors %d",
                                rank, lower_range, upper_range, num_processors);
                    return OPAL_ERROR;
                }

                for (processor_id=lower_range; processor_id<=upper_range; processor_id++) {
                    if (logical_map) {
                        /* need to convert this to physical processor id */
                        if (0 > (phys_processor = opal_paffinity_base_get_physical_processor_id(processor_id))) {
                            opal_output(0, "Rank %ld: PAFFINITY cannot get physical processor id for logical processor %d",
                                        rank, processor_id);
                            return OPAL_ERROR;
                        }                    
                    } else {
                        phys_processor = processor_id;
                    }
                    /* set the bit for this physical processor */
                    OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                    /* output diagnostic if requested */
                    if (diag_requested) {
                        opal_output(0, "paffinity slot assignment: rank %ld runs on cpu #%d (#%d)",
                                    rank, phys_processor, processor_id);
                    }
                }
                /* tell paffinity to bind us */
                if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                    return rc;
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

static int opal_paffinity_base_socket_core_to_cpu_set(char **socket_core_list, int socket_core_list_cnt, long rank, bool logical_map)
{
    int rc, i;
    char **socket_core;
    int socket_core_cnt;
    char **range;
    int range_cnt;
    int lower_range, upper_range;
    int socket, core;
    int num_sockets, num_cores;
    int phys_socket, phys_core, phys_processor;
    opal_paffinity_base_cpu_set_t cpumask;
    
    socket_core = opal_argv_split (socket_core_list[0], ':');
    socket_core_cnt = opal_argv_count(socket_core);
    OPAL_PAFFINITY_CPU_ZERO(cpumask);
    socket = atoi(socket_core[0]);
    core = atoi(socket_core[1]);
    
    /* get the number of LOGICAL sockets on this node */
    if ( OPAL_SUCCESS != ( rc = opal_paffinity_base_get_socket_info(&num_sockets))) {
        return rc;
    }
    
    if (logical_map) {
        /* need to convert provided socket to a PHYSICAL socket id */
        phys_socket = opal_paffinity_base_get_physical_socket_id(socket);
        if (0 > phys_socket) {
            opal_output(0, "Rank %ld: PAFFINITY cannot get physical socket id for logical socket %ld",
                        rank, (long)socket);
            return OPAL_ERROR;
        }
    } else {
        phys_socket = socket;
    }
    phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core);

    /* get the LOGICAL core info for this socket */
    if ( OPAL_SUCCESS != ( rc = opal_paffinity_base_get_core_info(phys_socket, &num_cores))) {
        opal_output(0,"Rank %ld: PAFFINITY Error !!! Could not get core info for physical socket number %d (%d)",
                    rank, phys_socket, socket);
        return rc;
    }
    
    if (0 == strcmp("*",socket_core[1])) {
        /* bind to all available LOGICAL cores */
        for (core = 0; core < num_cores; core++) {
            /* convert to PHYSICAL core id */
            if (0 > (phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core))) {
                opal_output(0, "Rank %ld: PAFFINITY cannot get physical core id for logical core %ld in physical socket %ld (%ld)",
                            rank, (long)core, (long)phys_socket, (long)socket);
                return OPAL_ERROR;
            }
            /* get the PHYSICAL processor id for the PHYSICAL socket/core */
            if ( OPAL_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id (phys_socket, phys_core, &phys_processor))) {
                return rc;
            }
            /* set the bit for this processor */
            OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
        }
        /* tell paffinity to bind us */
        if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
            return rc;
        }
        /* output diagnostic if requested */
        if (diag_requested) {
            opal_output(0, "paffinity slot assignment: rank %ld runs on physical processor #%d ( %d : %d)",
                        rank, phys_processor, socket, core);
        }
    } else {
        range = opal_argv_split(socket_core[1], '-');
        range_cnt = opal_argv_count(range);
        switch (range_cnt) {
            case 1:  /* only one core specified */
                core = atoi(range[0]);
                if (logical_map) {
                    /* convert to physical core */
                    if (0 > (phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core))) {
                        opal_output(0, "Rank %ld: PAFFINITY cannot get physical core id for logical core %ld in physical socket %ld (%ld)",
                                    rank, (long)core, (long)phys_socket, (long)socket);
                        return OPAL_ERROR;
                    }
                } else {
                    phys_core = core;
                }
                /* get the PHYSICAL processor id for this PHYSICAL socket/core */
                if ( OPAL_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id (phys_socket, phys_core, &phys_processor))) {
                    return rc;
                }
                /* set the bit for this processor */
                OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                /* tell paffinity to bind us */
                if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                    return rc;
                }
                /* output diagnostic if requested */
                if (diag_requested) {
                    opal_output(0, "paffinity slot assignment: rank %ld runs on physical cpu #%d ( %d[%d] : %d[%d])",
                                rank, phys_processor, phys_socket, socket, phys_core, core);
                }
                break;
                
            case 2:  /* range of core id's was given */
                lower_range = atoi(range[0]);
                upper_range = atoi(range[1]);
                if ( 0 > lower_range || num_cores < (upper_range - lower_range) || lower_range >= upper_range ) {
                    opal_output(0,"Rank %ld: PAFFINITY Error !!! Check your boundaries lower %d upper %d num_cores %d",
                                rank, lower_range, upper_range, num_cores);
                    return OPAL_ERROR;
                }
                for (core=lower_range; core<=upper_range; core++) {
                    if (logical_map) {
                        /* convert to physical core */
                        if (0 > (phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core))) {
                            opal_output(0, "Rank %ld: PAFFINITY cannot get physical core id for logical core %ld in physical socket %ld (%ld)",
                                        rank, (long)core, (long)phys_socket, (long)socket);
                            return OPAL_ERROR;
                        }
                    } else {
                        phys_core = core;
                    }
                    /* get the PHYSICAL processor id for this PHYSICAL socket/core */
                    if ( OPAL_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id (phys_socket, phys_core, &phys_processor))) {
                        return rc;
                    }
                    /* set the bit for this processor */
                    OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                    /* output diagnostic if requested */
                    if (diag_requested) {
                        opal_output(0,"paffinity slot assignment: rank %ld runs on cpu #%d ( %d[%d] : %d[%d])",
                                    rank, phys_processor, phys_socket, socket, phys_core, core);                        
                    }
                }
                /* tell paffinity to bind us */
                if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                    return rc;
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
                /* no colon => these cores are on the same socket as the last one specified,
                 * so we map them on that same physical socket
                 */
                range = opal_argv_split(socket_core[0], '-');
                range_cnt = opal_argv_count(range);
                switch (range_cnt) {
                    case 1:  /* only one core provided */
                        core = atoi(range[0]);
                        if (logical_map) {
                            /* convert to physical core */
                            if (0 > (phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core))) {
                                opal_output(0, "Rank %ld: PAFFINITY cannot get physical core id for logical core %ld in physical socket %ld (%ld)",
                                            rank, (long)core, (long)phys_socket, (long)socket);
                                return OPAL_ERROR;
                            }
                        } else {
                            phys_core = core;
                        }
                        /* get the PHYSICAL processor id for this PHYSICAL socket/core */
                        if ( OPAL_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id (phys_socket, phys_core, &phys_processor))) {
                            return rc;
                        }
                        /* set the bit for this processor */
                        OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                        /* tell paffinity to bind us */
                        if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                            return rc;
                        }
                        /* output diagnostic if requested */
                        if (diag_requested) {
                            opal_output(0, "paffinity slot assignment: rank %ld runs on physical cpu #%d ( %d[%d] : %d[%d])",
                                        rank, phys_processor, phys_socket, socket, phys_core, core);
                        }
                        break;
                        
                    case 2:    /* range of core id's was given */
                        lower_range = atoi(range[0]);
                        upper_range = atoi(range[1]);
                        if ( 0 > lower_range || num_cores < (upper_range - lower_range) || lower_range >= upper_range ) {
                            opal_output(0,"Rank %ld: PAFFINITY Error !!! Check your boundaries lower %d upper %d num_cores %d",
                                        rank, lower_range, upper_range, num_cores);
                            return OPAL_ERROR;
                        }
                        for (core=lower_range; core<=upper_range; core++) {
                            if (logical_map) {
                                /* convert to physical core */
                                if (0 > (phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core))) {
                                    opal_output(0, "Rank %ld: PAFFINITY cannot get physical core id for logical core %ld in physical socket %ld (%ld)",
                                                rank, (long)core, (long)phys_socket, (long)socket);
                                    return OPAL_ERROR;
                                }
                            } else {
                                phys_core = core;
                            }
                            /* get the PHYSICAL processor id for this PHYSICAL socket/core */
                            if ( OPAL_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id (phys_socket, phys_core, &phys_processor))) {
                                return rc;
                            }
                            /* set the bit for this processor */
                            OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                            /* output diagnostic if requested */
                            if (diag_requested) {
                                opal_output(0, "paffinity slot assignment: rank %ld runs on physical cpu #%d ( %d[%d] : %d[%d])",
                                            rank, phys_processor, phys_socket, socket, phys_core, core);
                            }
                        }
                        /* tell paffinity to bind us */
                        if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                            return rc;
                        }
                        break;
                        
                    default:
                        opal_argv_free(range);
                        opal_argv_free(socket_core);
                        return OPAL_ERROR;
                }
                opal_argv_free(range);
                break;
                
            case 2:  /* colon was given => refers to a new socket! */
                socket = atoi(socket_core[0]);
                if (logical_map) {
                    /* need to convert provided socket to a PHYSICAL socket id */
                    phys_socket = opal_paffinity_base_get_physical_socket_id(socket);
                    if (0 > phys_socket) {
                        opal_output(0, "Rank %ld: PAFFINITY cannot get physical socket id for logical socket %ld",
                                    rank, (long)socket);
                        return OPAL_ERROR;
                    }
                } else {
                    phys_socket = socket;
                }
                
                /* get the LOGICAL core info for this socket */
                if ( OPAL_SUCCESS != ( rc = opal_paffinity_base_get_core_info(phys_socket, &num_cores))) {
                    opal_output(0,"Rank %ld: PAFFINITY Error !!! Could not get core info for physical socket number %d (%d)",
                                rank, phys_socket, socket);
                    return rc;
                }
                
                if (0 == strcmp("*",socket_core[1])) {
                    /* bind to all available LOGICAL cores */
                    for (core = 0; core < num_cores; core++) {
                        /* convert to PHYSICAL core id */
                        if (0 > (phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core))) {
                            opal_output(0, "Rank %ld: PAFFINITY cannot get physical core id for logical core %ld in physical socket %ld (%ld)",
                                        rank, (long)core, (long)phys_socket, (long)socket);
                            return OPAL_ERROR;
                        }
                        /* get the PHYSICAL processor id for the PHYSICAL socket/core */
                        if ( OPAL_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id (phys_socket, phys_core, &phys_processor))) {
                            return rc;
                        }
                        /* set the bit for this processor */
                        OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                    }
                    /* tell paffinity to bind us */
                    if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                        return rc;
                    }
                    /* output diagnostic if requested */
                    if (diag_requested) {
                        opal_output(0, "paffinity slot assignment: rank %ld runs on physical cpu #%d ( %d[%d] : %d[%d])",
                                    rank, phys_processor, phys_socket, socket, phys_core, core);
                    }
                } else {
                    range = opal_argv_split(socket_core[1], '-');
                    range_cnt = opal_argv_count(range);
                    socket = atoi(socket_core[0]);
                    switch (range_cnt) {
                        case 1:  /* only one core specified */
                            core = atoi(range[0]);
                            if (logical_map) {
                                /* convert to physical core */
                                if (0 > (phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core))) {
                                    opal_output(0, "Rank %ld: PAFFINITY cannot get physical core id for logical core %ld in physical socket %ld (%ld)",
                                                rank, (long)core, (long)phys_socket, (long)socket);
                                    return OPAL_ERROR;
                                }
                            } else {
                                phys_core = core;
                            }
                            /* get the PHYSICAL processor id for this PHYSICAL socket/core */
                            if ( OPAL_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id (phys_socket, phys_core, &phys_processor))) {
                                return rc;
                            }
                            /* set the bit for this processor */
                            OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                            /* tell paffinity to bind us */
                            if (OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                                return rc;
                            }
                            /* output diagnostic if requested */
                            if (diag_requested) {
                                opal_output(0, "paffinity slot assignment: rank %ld runs on physical cpu #%d ( %d[%d] : %d[%d])",
                                            rank, phys_processor, phys_socket, socket, phys_core, core);
                            }
                            break;
                            
                        case 2:  /* range of core id's was given */
                            lower_range = atoi(range[0]);
                            upper_range = atoi(range[1]);
                            if ( 0 > lower_range || num_cores < (upper_range - lower_range) || lower_range >= upper_range ) {
                                opal_output(0,"Rank %ld: PAFFINITY Error !!! Check your boundaries lower %d upper %d num_cores %d",
                                            rank, lower_range, upper_range, num_cores);
                                return OPAL_ERROR;
                            }
                            for (core=lower_range; core<=upper_range; core++) {
                                if (logical_map) {
                                    /* convert to physical core */
                                    if (0 > (phys_core = opal_paffinity_base_get_physical_core_id(phys_socket, core))) {
                                        opal_output(0, "Rank %ld: PAFFINITY cannot get physical core id for logical core %ld in physical socket %ld (%ld)",
                                                    rank, (long)core, (long)phys_socket, (long)socket);
                                        return OPAL_ERROR;
                                    }
                                } else {
                                    phys_core = core;
                                }
                                /* get the PHYSICAL processor id for this PHYSICAL socket/core */
                                if ( OPAL_SUCCESS != (rc = opal_paffinity_base_get_map_to_processor_id (phys_socket, phys_core, &phys_processor))) {
                                    return rc;
                                }
                                /* set the bit for this processor */
                                OPAL_PAFFINITY_CPU_SET(phys_processor, cpumask);
                                /* output diagnostic if requested */
                                if (diag_requested) {
                                    opal_output(0, "paffinity slot assignment: rank %ld runs on physical cpu #%d ( %d[%d] : %d[%d])",
                                                rank, phys_processor, phys_socket, socket, phys_core, core);
                                }
                            }
                            /* tell paffinity to bind us */
                            if ( OPAL_SUCCESS != (rc = opal_paffinity_base_set(cpumask))) {
                                return rc;
                            }
                            
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

int opal_paffinity_base_slot_list_set(long rank, char *slot_str)
{
    char **item;
    char **socket_core;
    int item_cnt, socket_core_cnt, rc;
    bool logical_map;
    
    if (NULL == slot_str){
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* if the slot string is empty, that is an error */
    if (0 == strlen(slot_str)) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* check for diag request to avoid repeatedly doing so */
    if (4 < opal_output_get_verbosity(opal_paffinity_base_output)) {
        diag_requested = true;
    } else {
        diag_requested = false;
    }
    
    opal_output_verbose(5, opal_paffinity_base_output, "paffinity slot assignment: slot_list == %s", slot_str);
    
    if ('P' == slot_str[0] || 'p' == slot_str[0]) {
        /* user has specified physical mapping */
        logical_map = false;
        item = opal_argv_split (&slot_str[1], ',');
    } else {
        logical_map = true;  /* default to logical mapping */
        item = opal_argv_split (slot_str, ',');
    }
    
    item_cnt = opal_argv_count (item);
    socket_core = opal_argv_split (item[0], ':');
    socket_core_cnt = opal_argv_count(socket_core);
    opal_argv_free(socket_core);
    switch (socket_core_cnt) {
        case 1:  /* binding to cpu's */
            if (OPAL_SUCCESS != (rc = opal_paffinity_base_socket_to_cpu_set(item, item_cnt, rank, logical_map))) {
                opal_argv_free(item);
                return rc;
            }
            break;
        case 2: /* binding to socket/core specification */
            if (OPAL_SUCCESS != (rc = opal_paffinity_base_socket_core_to_cpu_set(item, item_cnt, rank, logical_map))) {
                opal_argv_free(item);
                return rc;
            }
            break;
        default:
            opal_argv_free(item);
            return OPAL_ERROR;
    }
    opal_argv_free(item);
    return OPAL_SUCCESS;
}
