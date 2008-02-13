/* -*- c -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PLPA_BOTTOM_H
#define PLPA_BOTTOM_H

/* Absolutely must not include <sched.h> here or it will generate
   conflicts. */

/* For memset() */
#include <string.h>
/* For pid_t and size_t */
#include <sys/types.h>

/***************************************************************************/

/* Internal macro to munge names */

/* Preprocessors are fun -- the double indirection is necessary.
   Extra bonus points if you can figure out why! :p */
#define PLPA_MUNGE_NAME(a, b) PLPA_MUNGE_NAME2(a, b)
#define PLPA_MUNGE_NAME2(a, b) a ## b
#define PLPA_NAME(name) PLPA_MUNGE_NAME(PLPA_SYM_PREFIX, name)
#define PLPA_NAME_CAPS(name) PLPA_MUNGE_NAME(PLPA_SYM_PREFIX_CAPS, name)

/***************************************************************************/

/* Values that can be returned from plpa_api_probe() */
typedef enum {
    /* Sentinel value */
    PLPA_NAME_CAPS(PROBE_UNSET),
    /* sched_setaffinity syscall available */
    PLPA_NAME_CAPS(PROBE_OK),
    /* syscall unavailable/unimplemented */
    PLPA_NAME_CAPS(PROBE_NOT_SUPPORTED),
    /* we experienced some strange failure that the user should report */
    PLPA_NAME_CAPS(PROBE_UNKNOWN)
} PLPA_NAME(api_type_t);

/* Values that can be used in plpa_max_id() */
typedef enum {
    /* Linux virtual processor */
    PLPA_NAME_CAPS(ID_TYPE_PROCESSOR),
    /* CPU sockets */
    PLPA_NAME_CAPS(ID_TYPE_SOCKET),
    /* CPU cores */
    PLPA_NAME_CAPS(ID_TYPE_CORE),
    /* Sentinel value */
    PLPA_NAME_CAPS(ID_TYPE_MAX)
} PLPA_NAME(plpa_id_type_t);

/* PLPA bitmask type */
typedef unsigned long int PLPA_NAME(bitmask_t);
#define PLPA_BITMASK_T_NUM_BITS (sizeof(PLPA_NAME(bitmask_t)) * 8)
#define PLPA_BITMASK_CPU_MAX 1024
#define PLPA_BITMASK_NUM_ELEMENTS (PLPA_BITMASK_CPU_MAX / PLPA_BITMASK_T_NUM_BITS)
typedef struct { PLPA_NAME(bitmask_t) bitmask[PLPA_BITMASK_NUM_ELEMENTS]; } PLPA_NAME(cpu_set_t);

/***************************************************************************/

/* Internal macro for identifying the byte in a bitmask array */
#define PLPA_CPU_BYTE(num) ((num) / PLPA_BITMASK_T_NUM_BITS)

/* Internal macro for identifying the bit in a bitmask array */
#define PLPA_CPU_BIT(num) ((num) % PLPA_BITMASK_T_NUM_BITS)

/***************************************************************************/

/* Public macro to zero out a PLPA cpu set */
#define PLPA_CPU_ZERO(cpuset) \
    memset((cpuset), 0, sizeof(PLPA_NAME(cpu_set_t)))

/* Public macro to set a bit in a PLPA cpu set */
#define PLPA_CPU_SET(num, cpuset) \
    (cpuset)->bitmask[PLPA_CPU_BYTE(num)] |= ((PLPA_NAME(bitmask_t))1 << PLPA_CPU_BIT(num))

/* Public macro to clear a bit in a PLPA cpu set */
#define PLPA_CPU_CLR(num, cpuset) \
    (cpuset)->bitmask[PLPA_CPU_BYTE(num)] &= ~((PLPA_NAME(bitmask_t))1 << PLPA_CPU_BIT(num))

/* Public macro to test if a bit is set in a PLPA cpu set */
#define PLPA_CPU_ISSET(num, cpuset) \
    (0 != (((cpuset)->bitmask[PLPA_CPU_BYTE(num)]) & ((PLPA_NAME(bitmask_t))1 << PLPA_CPU_BIT(num))))

/***************************************************************************/

/* Setup PLPA internals */
int PLPA_NAME(init)(void);

/* Check what API is on this machine */
int PLPA_NAME(api_probe)(PLPA_NAME(api_type_t) *api_type);

/* Set processor affinity */
int PLPA_NAME(sched_setaffinity)(pid_t pid, size_t cpusetsize,
                                 const PLPA_NAME(cpu_set_t) *cpuset);

/* Get processor affinity */
int PLPA_NAME(sched_getaffinity)(pid_t pid, size_t cpusetsize,
                                 PLPA_NAME(cpu_set_t) *cpuset);

/* Return whether topology information is available (i.e.,
   plpa_map_to_*, plpa_max_*) */
int PLPA_NAME(have_topology_information)(int *supported);

/* Map (socket,core) tuple to virtual processor ID */
int PLPA_NAME(map_to_processor_id)(int socket, int core, int *processor_id);

/* Map processor_id to (socket,core) tuple */
int PLPA_NAME(map_to_socket_core)(int processor_id, int *socket, int *core);

/* Return the max processor ID */
int PLPA_NAME(max_processor_id)(int *max_processor_id);

/* Return the max socket number */
int PLPA_NAME(max_socket)(int *max_socket);

/* Return the max core number for a given socket */
int PLPA_NAME(max_core)(int socket, int *max_core);

/* Shut down PLPA */
int PLPA_NAME(finalize)(void);

#endif /* PLPA_BOTTOM_H */

