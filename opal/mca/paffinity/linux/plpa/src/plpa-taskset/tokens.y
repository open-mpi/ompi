%{

/* Copyright 2007-2009 Cisco Systems, Inc.  All rights reserved. */

#include "plpa_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "plpa.h"
#include "plpa-taskset.h"


/*
 * Could this be done more efficiently?  Absolutely.
 *
 * But this is neat, elegant, and easy to understand / maintain.
 * Performance is not an issue here.
 */

/*
 * Typedefs
 */
typedef enum {
    ALL,
    PROCESSOR
} id_type_t;

/*
 * Global functions
 */
int token_parse(PLPA_NAME(cpu_set_t) *cpu_set);
void yyerror(char const *s);

/*
 * Local functions
 */
static void set_union(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *a, 
                      PLPA_NAME(cpu_set_t) *b);
static void set_copy(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *in);
static void cpu_set(PLPA_NAME(cpu_set_t) *out, int pos);
static void cpu_set_all(PLPA_NAME(cpu_set_t) *out, id_type_t type);
static void cpu_set_even(PLPA_NAME(cpu_set_t) *out, id_type_t type);
static void cpu_set_odd(PLPA_NAME(cpu_set_t) *out, id_type_t type);
static void cpu_set_range(PLPA_NAME(cpu_set_t) *out, int min, int max);
static void cpu_compliment(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *in);
static void sc_merge(PLPA_NAME(cpu_set_t) *out, 
                     PLPA_NAME(cpu_set_t) *cores, int cores_are_valid,
                     PLPA_NAME(cpu_set_t) *sockets, int sockets_are_valid);

/*
 * Local variables
 */
static int socket_list[PLPA_BITMASK_CPU_MAX];
static PLPA_NAME(cpu_set_t) *return_value;
%}

%union {
    PLPA_NAME(cpu_set_t) cpu_set;
    int number;
}

%token <cpu_set> TOKENS_STRING_ALL
%token <cpu_set> TOKENS_STRING_EVEN
%token <cpu_set> TOKENS_STRING_ODD
%token <cpu_set> TOKENS_STRING_NOT
%token <number> TOKENS_NUMBER

%type <cpu_set> start
%type <cpu_set> cpu_list
%type <cpu_set> cpu_numbers
%type <cpu_set> cpu_strings
%type <cpu_set> sc_list
%type <cpu_set> sc_expr
%type <cpu_set> sc_item
%type <cpu_set> sc_item_list
%type <cpu_set> sc_strings
     
%% /* Grammar rules and actions follow.  */

start:  cpu_list
            { set_copy(return_value, &$1); }
        | sc_list
            { set_copy(return_value, &$1); }

cpu_list: cpu_strings
            { set_copy(&$$, &$1); }
        | cpu_numbers
            { set_copy(&$$, &$1); }

cpu_numbers: TOKENS_NUMBER
            { cpu_set(&$$, $1); }
        | TOKENS_NUMBER '-' TOKENS_NUMBER
            { cpu_set_range(&$$, $1, $3); }
        | cpu_list ',' TOKENS_NUMBER
            { PLPA_NAME(cpu_set_t) temp; cpu_set(&temp, $3); set_union(&$$, &$1, &temp); }
        | cpu_list ',' TOKENS_NUMBER '-' TOKENS_NUMBER
            { PLPA_NAME(cpu_set_t) temp; cpu_set_range(&temp, $3, $5); set_union(&$$, &$1, &temp); }

cpu_strings: TOKENS_STRING_ALL
            { cpu_set_all(&$$, PROCESSOR); }
        | TOKENS_STRING_EVEN
            { cpu_set_even(&$$, PROCESSOR); }
        | TOKENS_STRING_ODD
            { cpu_set_odd(&$$, PROCESSOR); }

sc_list: sc_expr
            { set_copy(&$$, &$1); }
        | sc_list ',' sc_expr
            { set_union(&$$, &$1, &$3); }

sc_expr: sc_item '@' sc_item
            { sc_merge(&$$, &$1, 1, &$3, 1); }
        | sc_item '@' sc_strings
            { sc_merge(&$$, &$1, 1, &$3, 0); }
        | sc_strings '@' sc_item
            { sc_merge(&$$, &$1, 0, &$3, 1); }
        | sc_strings '@' sc_strings
            { sc_merge(&$$, &$1, 0, &$3, 0); }

sc_item: TOKENS_NUMBER
            { cpu_set(&$$, $1); }
        | TOKENS_STRING_NOT TOKENS_NUMBER
            { PLPA_NAME(cpu_set_t) temp; cpu_set(&temp, $2); cpu_compliment(&$$, &temp); }
        | TOKENS_STRING_NOT '{' sc_item_list '}'
            { cpu_compliment(&$$, &$3); }
        | '{' sc_item_list '}'
            { set_copy(&$$, &$2); }

sc_item_list: TOKENS_NUMBER
            { cpu_set(&$$, $1); }
        | TOKENS_NUMBER '-' TOKENS_NUMBER
            { cpu_set_range(&$$, $1, $3); }
        | sc_item_list ',' TOKENS_NUMBER
            { PLPA_NAME(cpu_set_t) temp; cpu_set(&temp, $3); set_union(&$$, &$1, &temp); }
        | sc_item_list ',' TOKENS_NUMBER '-' TOKENS_NUMBER
            { PLPA_NAME(cpu_set_t) temp; cpu_set_range(&temp, $3, $5); set_union(&$$, &$1, &temp); }

sc_strings: TOKENS_STRING_ALL
            { cpu_set_all(&$$, ALL); }
        | TOKENS_STRING_EVEN
            { cpu_set_even(&$$, ALL); }
        | TOKENS_STRING_ODD
            { cpu_set_odd(&$$, ALL); }

%%

int token_parse(PLPA_NAME(cpu_set_t) *cpu_set)
{
    int ret;

    PLPA_CPU_ZERO(cpu_set);
    return_value = cpu_set;
    ret = yyparse();
    if (0 != ret) {
        return ret;
    }
    return 0;
}

void yyerror (char const *s)
{
    fprintf(stderr, "ERROR: %s\n", s);
}

static void set_union(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *a, 
                      PLPA_NAME(cpu_set_t) *b)
{
    int i;
    PLPA_CPU_ZERO(out);
    for (i = 0; i < PLPA_BITMASK_CPU_MAX; ++i) {
        if (PLPA_CPU_ISSET(i, a) || PLPA_CPU_ISSET(i, b)) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void set_copy(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *in)
{
    int i;
    for (i = 0; i < PLPA_BITMASK_CPU_MAX; ++i) {
        if (PLPA_CPU_ISSET(i, in)) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void cpu_set(PLPA_NAME(cpu_set_t) *out, int pos)
{
    PLPA_CPU_ZERO(out);
    if (pos < PLPA_BITMASK_CPU_MAX) {
        PLPA_CPU_SET(pos, out);
    }
}

static void cpu_set_all(PLPA_NAME(cpu_set_t) *out, id_type_t type)
{
    int i, max_num, max_id, exists, online;
    PLPA_CPU_ZERO(out);

    /* Only set processor ID's that exist and are online */
    if (ALL == type) {
        max_id = PLPA_BITMASK_CPU_MAX;
    } else if (PROCESSOR == type) {
        PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_ONLINE),
                                      &max_num, &max_id);
    }

    for (i = 0; i <= max_id; ++i) {
        if (0 == PLPA_NAME(get_processor_flags)(i, &exists, &online) &&
            exists && online) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void cpu_set_even(PLPA_NAME(cpu_set_t) *out, id_type_t type)
{
    int i, max_num, max_id, exists, online;
    PLPA_CPU_ZERO(out);

    /* Only set processor ID's that exist and are online */
    if (ALL == type) {
        max_id = PLPA_BITMASK_CPU_MAX;
    } else if (PROCESSOR == type) {
        PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_ONLINE),
                                      &max_num, &max_id);
    }

    for (i = 0; i <= max_id; i += 2) {
        if (0 == PLPA_NAME(get_processor_flags)(i, &exists, &online) &&
            exists && online) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void cpu_set_odd(PLPA_NAME(cpu_set_t) *out, id_type_t type)
{
    int i, max_num, max_id, exists, online;
    PLPA_CPU_ZERO(out);

    /* Only set processor ID's that exist */
    if (ALL == type) {
        max_id = PLPA_BITMASK_CPU_MAX;
    } else if (PROCESSOR == type) {
        PLPA_NAME(get_processor_data)(PLPA_NAME_CAPS(COUNT_ONLINE),
                                      &max_num, &max_id);
    }

    for (i = 1; i <= max_id; i += 2) {
        if (0 == PLPA_NAME(get_processor_flags)(i, &exists, &online) &&
            exists && online) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void cpu_set_range(PLPA_NAME(cpu_set_t) *out, int min, int max)
{
    int i;
    PLPA_CPU_ZERO(out);
    for (i = min; i <= max && i < PLPA_BITMASK_CPU_MAX; ++i) {
        PLPA_CPU_SET(i, out);
    }
}

static void cpu_compliment(PLPA_NAME(cpu_set_t) *out, PLPA_NAME(cpu_set_t) *in)
{
    int i;
    PLPA_CPU_ZERO(out);
    for (i = 0; i < PLPA_BITMASK_CPU_MAX; ++i) {
        if (!PLPA_CPU_ISSET(i, in)) {
            PLPA_CPU_SET(i, out);
        }
    }
}

static void sc_merge(PLPA_NAME(cpu_set_t) *out, 
                     PLPA_NAME(cpu_set_t) *cores, int cores_are_valid,
                     PLPA_NAME(cpu_set_t) *sockets, int sockets_are_valid)
{
    int ret, i, core, socket, id, have_topo;
    int num_sockets, max_socket_id;
    int num_cores, max_core_id;

    /* This is just about the only function that's tricky.  Take a
       bitmask representing all the cores and a bitmask representing
       all the sockets and merge them into a single cpu_set_t
       representing real CPU id's using the plpa_map_to_processor_id()
       function.  But error out if this system doesn't support the
       topology information (because we won't be able to do the
       mapping). */

    PLPA_CPU_ZERO(out);
    if (0 != PLPA_NAME(have_topology_information)(&have_topo) ||
        0 == have_topo) {
        fprintf(stderr, "ERROR: This system does not support topology information\n");
        exit(1);
    }

    if (0 != PLPA_NAME(get_socket_info)(&num_sockets, &max_socket_id)) {
        fprintf(stderr, "ERROR: Unable to retrieve socket information\n");
        exit(1);
    }

    /* Even though I officially don't care about performance here,
       intentionally putting in a loop that is
       O(PLPA_BITMASK_CPU_MAX^2) gives me pause.  :-) So scan through
       the sockets array once and generate a list of the set bits in a
       much shorter array. */

    for (i = socket = 0; socket < PLPA_BITMASK_CPU_MAX; ++socket) {
        if (sockets_are_valid && socket > max_socket_id &&
            PLPA_CPU_ISSET(socket, sockets)) {
            fprintf(stderr, "ERROR: Invalid socket ID specified (%d; max socket ID is %d)\n",
                    socket, max_socket_id);
            exit(1);
        } else if (sockets_are_valid && 
                   ENOENT == PLPA_NAME(get_core_info)(socket, &num_cores,
                                                      &max_core_id) &&
                   PLPA_CPU_ISSET(socket, sockets)) {
            fprintf(stderr, "ERROR: Invalid socket ID specified (%d does not exist)\n",
                    socket);
            exit(1);
        } else if (PLPA_CPU_ISSET(socket, sockets)) {
            socket_list[i++] = socket;
        }
    }

    /* Bozo case: if there were no sockets set, we're done */

    if (0 == i) {
        return;
    }

    /* Otherwise, do the loop to create the mapping of sockets and
       cores, seting the final bitmask.  Yes, this is a double loop,
       but hopefully it's much smaller than
       PLPA_BITMASK_CPU_MAX^2. */

    for (core = 0; core < PLPA_BITMASK_CPU_MAX; ++core) {
        if (PLPA_CPU_ISSET(core, cores)) {
            for (socket = 0; socket < i; ++socket) {
                ret = PLPA_NAME(map_to_processor_id)(socket_list[socket],
                                                     core, &id);
                if (ENOENT == ret) {
                    if (cores_are_valid) {
                        fprintf(stderr, "ERROR: Invalid core@socket tuple (%d@%d does not exist)\n",
                                core, socket_list[socket]);
                        exit(1);
                    } else {
                        continue;
                    }
                } else if (0 != ret) {
                    fprintf(stderr, "ERROR: Failed to map %d@%d to processor ID\n",
                            core, socket);
                    exit(1);
                }
                PLPA_CPU_SET(id, out);
            }
        }
    }
}
