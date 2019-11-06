/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "ompi/constants.h"
#include "coll_tuned.h"

/* need to include our own topo prototypes so we can malloc data on the comm correctly */
#include "ompi/mca/coll/base/coll_base_topo.h"

/* also need the dynamic rule structures */
#include "coll_tuned_dynamic_rules.h"

#include <stdlib.h>
#include <stdio.h>

#include "ompi/mca/coll/base/coll_base_util.h"


ompi_coll_alg_rule_t* ompi_coll_tuned_mk_alg_rules (int n_alg)
{
    int i;
    ompi_coll_alg_rule_t* alg_rules;

    alg_rules = (ompi_coll_alg_rule_t *) calloc (n_alg, sizeof (ompi_coll_alg_rule_t));
    if (!alg_rules) return (alg_rules);

    /* set all we can at this point */
    for (i=0;i<n_alg;i++) {
        alg_rules[i].alg_rule_id = i;
    }
    return (alg_rules);
}

ompi_coll_nodes_rule_t* ompi_coll_tuned_mk_nodes_rules (int n_nodes_rules, int alg_rule_id)
{
    int i;
    ompi_coll_nodes_rule_t * nodes_rules;

    nodes_rules = (ompi_coll_nodes_rule_t *) calloc (n_nodes_rules, sizeof (ompi_coll_nodes_rule_t));
    if (!nodes_rules) return (nodes_rules);

    for (i=0;i<n_nodes_rules;i++) {
        nodes_rules[i].nodes_number = 0;   /* unknown */
        nodes_rules[i].alg_rule_id = alg_rule_id;
        nodes_rules[i].node_rule_id = i;
        nodes_rules[i].com_rules = (ompi_coll_com_rule_t *) NULL;
    }
    return (nodes_rules);
}

ompi_coll_com_rule_t* ompi_coll_tuned_mk_com_rules (int n_com_rules, int alg_rule_id, int node_rule_id)
{
    int i;
    ompi_coll_com_rule_t * com_rules;

    com_rules = (ompi_coll_com_rule_t *) calloc (n_com_rules, sizeof (ompi_coll_com_rule_t));
    if (!com_rules) return (com_rules);

    for (i=0;i<n_com_rules;i++) {
        com_rules[i].mpi_comsize = 0;   /* unknown */
        com_rules[i].alg_rule_id = alg_rule_id;
        com_rules[i].node_rule_id = node_rule_id;
        com_rules[i].com_rule_id = i;
        com_rules[i].n_msg_sizes = 0;   /* unknown */
        com_rules[i].msg_rules = (ompi_coll_msg_rule_t *) NULL;
    }
    return (com_rules);
}


ompi_coll_msg_rule_t* ompi_coll_tuned_mk_msg_rules (int n_msg_rules, int alg_rule_id, int node_rule_id, int com_rule_id, int mpi_comsize)
{
    int i;
    ompi_coll_msg_rule_t *msg_rules;

    msg_rules = (ompi_coll_msg_rule_t *) calloc (n_msg_rules, sizeof (ompi_coll_msg_rule_t));
    if (!msg_rules) return (msg_rules);

    for( i = 0; i < n_msg_rules; i++ ) {
        msg_rules[i].mpi_comsize = mpi_comsize;
        msg_rules[i].alg_rule_id = alg_rule_id;
        msg_rules[i].node_rule_id = node_rule_id;
        msg_rules[i].com_rule_id = com_rule_id;
        msg_rules[i].msg_rule_id = i;
        msg_rules[i].msg_size = 0;               /* unknown */
        msg_rules[i].result_alg = 0;             /* unknown */
        msg_rules[i].result_topo_faninout = 0;   /* unknown */
        msg_rules[i].result_segsize = 0;         /* unknown */
        msg_rules[i].result_max_requests = 0;    /* unknown & default */
    }
    return (msg_rules);
}


/*
 * Debug / IO routines
 *
 */
int ompi_coll_tuned_dump_msg_rule (ompi_coll_msg_rule_t* msg_p)
{
    if (!msg_p) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"Message rule was a NULL ptr?!\n");
        return (-1);
    }

    opal_output_verbose(50,ompi_coll_tuned_stream,"alg_id %3d\tnode_id %3d\tcom_id %3d\tcom_size %3d\tmsg_id %3d\t", msg_p->alg_rule_id,
                 msg_p->node_rule_id, msg_p->com_rule_id, msg_p->mpi_comsize, msg_p->msg_rule_id);

    opal_output_verbose(50,ompi_coll_tuned_stream,"msg_size %10lu -> algorithm %2d\ttopo in/out %2d\tsegsize %5ld\tmax_requests %4d\n",
                 msg_p->msg_size, msg_p->result_alg, msg_p->result_topo_faninout, msg_p->result_segsize,
                 msg_p->result_max_requests);

    return (0);
}


int ompi_coll_tuned_dump_com_rule (ompi_coll_com_rule_t* com_p)
{
    int i;

    if (!com_p) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"Com rule was a NULL ptr?!\n");
        return (-1);
    }

    opal_output_verbose(50,ompi_coll_tuned_stream, "alg_id %3d\tnode_id %3d\tcom_id %3d\tcom_size %3d\t", com_p->alg_rule_id, com_p->node_rule_id, com_p->com_rule_id, com_p->mpi_comsize);

    if (!com_p->n_msg_sizes) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"no msgsizes defined\n");
        return (0);
    }
    int rc=0;
    opal_output_verbose(50,ompi_coll_tuned_stream,"number of message sizes %3d\n", com_p->n_msg_sizes);

    for (i=0;i<com_p->n_msg_sizes;i++) {
        rc += ompi_coll_tuned_dump_msg_rule (&(com_p->msg_rules[i]));
    }

    return rc;
}


int ompi_coll_tuned_dump_nodes_rule (ompi_coll_nodes_rule_t* nodes_p)
{
    int i;

    if (!nodes_p) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"Node rule was a NULL ptr?!\n");
        return (-1);
    }

    opal_output_verbose(50,ompi_coll_tuned_stream,"alg_id %3d\tnode_id %3d", nodes_p->alg_rule_id, nodes_p->node_rule_id);

    if (!nodes_p->n_com_sizes) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"no coms defined\n");
        return (0);
    }
    int rc=0;
    opal_output_verbose(50,ompi_coll_tuned_stream,"number of com sizes %3d\n", nodes_p->n_com_sizes);

    for (i=0;i<nodes_p->n_com_sizes;i++) {
        rc += ompi_coll_tuned_dump_com_rule (&(nodes_p->com_rules[i]));
    }

    return rc;
}


int ompi_coll_tuned_dump_alg_rule (ompi_coll_alg_rule_t* alg_p)
{
    int i;

    if (!alg_p) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"Algorithm rule was a NULL ptr?!\n");
        return (-1);
    }

    opal_output_verbose(50,ompi_coll_tuned_stream,"alg_id %3d\t", alg_p->alg_rule_id);

    if (!alg_p->n_nodes_sizes) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"no nodes defined\n");
        return (0);
    }
    int rc=0;
    opal_output_verbose(50,ompi_coll_tuned_stream,"number of nodes sizes %3d\n", alg_p->n_nodes_sizes);

    for (i=0;i<alg_p->n_nodes_sizes;i++) {
        rc += ompi_coll_tuned_dump_nodes_rule (&(alg_p->nodes_rules[i]));
    }

    return rc;
}


int ompi_coll_tuned_dump_all_rules (ompi_coll_alg_rule_t* alg_p, int n_rules)
{
    int i;

    if (!alg_p) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"Algorithm rule was a NULL ptr?!\n");
        return (-1);
    }
    int rc=0;

    opal_output_verbose(50,ompi_coll_tuned_stream,"Number of algorithm rules %3d\n", n_rules);

    for (i=0;i<n_rules;i++) {
        rc += ompi_coll_tuned_dump_alg_rule (&(alg_p[i]));
    }

    return rc;
}


/*
 * Memory free routines
 *
 */
int ompi_coll_tuned_free_msg_rules_in_com_rule (ompi_coll_com_rule_t* com_p)
{
    int rc=0;

    if (!com_p) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"attempt to free NULL com_rule ptr\n");
        return (-1);
    }

    if (com_p->n_msg_sizes) {
        ompi_coll_msg_rule_t* msg_p;
        msg_p = com_p->msg_rules;

        if (!msg_p) {
            opal_output_verbose(1,ompi_coll_tuned_stream,"attempt to free NULL msg_rules when msg count was %d\n", com_p->n_msg_sizes);
            rc = -1; /* some error */
        }
        else {
            /* ok, memory exists for the msg rules so free that first */
            free (com_p->msg_rules);
            com_p->msg_rules = (ompi_coll_msg_rule_t*) NULL;
            com_p->n_msg_sizes = 0;
        }

    } /* if we have msg rules to free as well */

    return (rc);
}


int ompi_coll_tuned_free_coms_rules_in_node_rule (ompi_coll_nodes_rule_t* nodes_p)
{
    int rc=0;

    if (!nodes_p) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"attempt to free NULL node_rule ptr\n");
        return (-1);
    }

    if (nodes_p->n_com_sizes) {
        ompi_coll_com_rule_t* com_p;
        com_p = nodes_p->com_rules;

        if (!com_p) {
            opal_output_verbose(1,ompi_coll_tuned_stream,"attempt to free NULL com_rules when com count was %d\n", nodes_p->n_com_sizes);
        } else {
            int i;
            /* ok, memory exists for the com rules so free their message rules first */
            for( i = 0; i < nodes_p->n_com_sizes; i++ ) {
                com_p = &(nodes_p->com_rules[i]);
                ompi_coll_tuned_free_msg_rules_in_com_rule (com_p);
            }
            /* we are now free to free the com rules themselives */
            free (nodes_p->com_rules);
            nodes_p->com_rules = (ompi_coll_com_rule_t*) NULL;
            nodes_p->n_com_sizes = 0;
        }

    } /* if we have msg rules to free as well */

    return (rc);
}

int ompi_coll_tuned_free_nodes_rules_in_alg_rule (ompi_coll_alg_rule_t* alg_p)
{
    int rc=0;

    if (!alg_p) {
        opal_output_verbose(1,ompi_coll_tuned_stream,"attempt to free NULL alg_rule ptr\n");
        return (-1);
    }

    if (alg_p->n_nodes_sizes) {
        ompi_coll_nodes_rule_t* nodes_p;
        nodes_p = alg_p->nodes_rules;

        if (!nodes_p) {
            opal_output_verbose(1,ompi_coll_tuned_stream,"attempt to free NULL nodes_rules when nodes count was %d\n", alg_p->n_nodes_sizes);
        } else {
            int i;
            /* ok, memory exists for the nodes rules so free their message rules first */
            for( i = 0; i < alg_p->n_nodes_sizes; i++ ) {
                nodes_p = &(alg_p->nodes_rules[i]);
                ompi_coll_tuned_free_coms_rules_in_node_rule (nodes_p);
            }
            /* we are now free to free the nodes rules themselives */
            free (alg_p->nodes_rules);
            alg_p->nodes_rules = (ompi_coll_nodes_rule_t*) NULL;
            alg_p->n_nodes_sizes = 0;
        }

    } /* if we have nodes rules to free as well */

    return (rc);
}

int ompi_coll_tuned_free_all_rules (ompi_coll_alg_rule_t* alg_p, int n_algs)
{
    int i;
    int rc = 0;

    for( i = 0; i < n_algs; i++ ) {
        rc += ompi_coll_tuned_free_nodes_rules_in_alg_rule (&(alg_p[i]));
    }

    free (alg_p);

    return (rc);
}

/*
 * query functions
 * i.e. the functions that get me the algorithm, topo fanin/out and segment size fast
 * and also get the rules that are needed by each communicator as needed
 *
 */

/*
 * This function is used to get the pointer to the nearest (less than or equal)
 * com rule for this MPI collective (alg_id) for a given
 * MPI communicator size. The complete rule base must be presented.
 *
 * If no rule exits returns NULL, else the com rule ptr
 * (which can be used in the coll_tuned_get_target_method_params() call)
 *
 */
ompi_coll_com_rule_t* ompi_coll_tuned_get_com_rule_ptr (ompi_coll_alg_rule_t* rules, int alg_id, int nodes_nb, int mpi_comsize)
{
    ompi_coll_alg_rule_t*   alg_p = (ompi_coll_alg_rule_t*) NULL;
    ompi_coll_nodes_rule_t* nodes_p = (ompi_coll_nodes_rule_t*) NULL;
    ompi_coll_nodes_rule_t* best_nodes_p = (ompi_coll_nodes_rule_t*) NULL;
    ompi_coll_com_rule_t*   com_p = (ompi_coll_com_rule_t*) NULL;
    ompi_coll_com_rule_t*   best_com_p = (ompi_coll_com_rule_t*) NULL;
    int i;

    if (!rules) {                    /* no rule base no resulting com rule */
        return ((ompi_coll_com_rule_t*)NULL);
    }

    alg_p = &(rules[alg_id]); /* get the algorithm rule pointer */

    if (!alg_p->n_nodes_sizes) {   /* check for count of communicator nodes number */
        return ((ompi_coll_com_rule_t*)NULL);    /* no nodes sizes so no rule */
    }

    best_nodes_p = NULL;
    nodes_p = alg_p->nodes_rules;
    i = 0;

    while( i < alg_p->n_nodes_sizes ) {
        if (nodes_p->nodes_number > nodes_nb) {
            break;
        }
        best_nodes_p = nodes_p;
        /* go to the next entry */
        nodes_p++;
        i++;
    }
    if(NULL == best_nodes_p) { /* Check if there is a valid node configuration */
        return ((ompi_coll_com_rule_t*)NULL);    /* no config so no rule */
    }
    opal_output_verbose(50,ompi_coll_tuned_stream,"Selected the following node rule id %d nodes_nb %d (%d)\n", best_nodes_p->node_rule_id, best_nodes_p->nodes_number, nodes_nb);

    if (!best_nodes_p->n_com_sizes) {   /* check for count of communicator sizes */
        return ((ompi_coll_com_rule_t*)NULL);    /* no com sizes so no rule */
    }

    /* ok have some com sizes, now to find the one closest to my mpi_comsize */

    /* make a copy of the first com rule */
    best_com_p = NULL;
    com_p = best_nodes_p->com_rules;
    i = 0;

    while( i < best_nodes_p->n_com_sizes ) {
        if (com_p->mpi_comsize > mpi_comsize) {
            break;
        }
        best_com_p = com_p;
        /* go to the next entry */
        com_p++;
        i++;
    }
    if(NULL == best_com_p) { /* Check if there is a valid node configuration */
        return ((ompi_coll_com_rule_t*)NULL);    /* no config so no rule */
    }

    opal_output_verbose(50,ompi_coll_tuned_stream,"Selected the following com rule id %d\n", best_com_p->com_rule_id);
    ompi_coll_tuned_dump_com_rule (best_com_p);

    return (best_com_p);
}

/*
 * This function takes a com_rule ptr (from the communicators coll tuned data structure)
 * (Which is chosen for a particular MPI collective)
 * and a (total_)msg_size and it returns (0) and a algorithm to use and a recommended topo faninout and segment size
 * all based on the user supplied rules
 *
 * Just like the above functions it uses a less than or equal msg size
 * (hense config file must have a default defined for '0' if we reach this point)
 * else if no rules match we return '0' + '0,0' or used fixed decision table with no topo chand and no segmentation
 * of users data.. shame.
 *
 * On error return 0 so we default to fixed rules anyway :)
 *
 */

int ompi_coll_tuned_get_target_method_params (ompi_coll_com_rule_t* base_com_rule, size_t mpi_msgsize, int *result_topo_faninout,
                                              int* result_segsize, int* max_requests)
{
    ompi_coll_msg_rule_t*  msg_p = (ompi_coll_msg_rule_t*) NULL;
    ompi_coll_msg_rule_t*  best_msg_p = (ompi_coll_msg_rule_t*) NULL;
    int i;

    /* No rule or zero rules */
    if( (NULL == base_com_rule) || (0 == base_com_rule->n_msg_sizes)) {
        return (0);
    }

    /* ok have some msg sizes, now to find the one closest to my mpi_msgsize */

    /* make a copy of the first msg rule */
    best_msg_p = msg_p = base_com_rule->msg_rules;
    i = 0;

    while (i<base_com_rule->n_msg_sizes) {
        if (msg_p->msg_size <= mpi_msgsize) {
            best_msg_p = msg_p;
        }
        else {
            break;
        }
        /* go to the next entry */
        msg_p++;
        i++;
    }

    opal_output_verbose(50,ompi_coll_tuned_stream,"Selected the following msg rule id %d\n", best_msg_p->msg_rule_id);
    ompi_coll_tuned_dump_msg_rule (best_msg_p);

    /* return the segment size */
    *result_topo_faninout = best_msg_p->result_topo_faninout;

    /* return the segment size */
    *result_segsize = best_msg_p->result_segsize;

    /* return the maximum requests */
    *max_requests = best_msg_p->result_max_requests;

    /* return the algorithm/method to use */
    return (best_msg_p->result_alg);
}
