/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2016 INRIA.  All rights reserved.
 * Copyright (c) 2012-2017 Bordeaux Polytechnic Institute
 * Copyright (c) 2015-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/constants.h"
#include "opal/mca/hwloc/hwloc-internal.h"

#include "ompi/mca/topo/treematch/topo_treematch.h"
#include "ompi/mca/topo/treematch/treematch/treematch.h"
#include "ompi/mca/topo/treematch/treematch/tm_mapping.h"
#include "ompi/mca/topo/base/base.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"

#include "ompi/mca/pml/pml.h"

#include "opal/mca/pmix/pmix.h"

#define ERR_EXIT(ERR)                           \
    do {                                        \
        free (nodes_roots);                     \
        free (tracker);                         \
        free (colors);                          \
        free(local_pattern);                    \
        return (ERR); }                         \
    while(0);

#define FALLBACK()                  \
    do { free(nodes_roots);         \
        free(lindex_to_grank);      \
        if( NULL != set) hwloc_bitmap_free(set);     \
        goto fallback; }            \
    while(0);

#define MY_STRING_SIZE 64
/*#define __DEBUG__ 1  */

/**
 * This function is a allreduce between all processes to detect for oversubscription.
 * On each node, the local_procs will be a different array, that contains only the
 * local processes. Thus, that process will compute the node oversubscription and will
 * bring this value to the operation, while every other process on the node will
 * contribute 0.
 * Doing an AllReduce might be an overkill for this situation, but it should remain
 * more scalable than a star reduction (between the roots of each node (nodes_roots),
 * followed by a bcast to all processes.
 */
static int check_oversubscribing(int rank,
                                 int num_nodes,
                                 int num_objs_in_node,
                                 int num_procs_in_node,
                                 int *nodes_roots,
                                 int *local_procs,
                                 ompi_communicator_t *comm_old)
{
    int oversubscribed = 0, local_oversub = 0, err;

    /* Only a single process per node, the local root, compute the oversubscription condition */
    if (rank == local_procs[0])
        if(num_objs_in_node < num_procs_in_node)
            local_oversub = 1;


    if (OMPI_SUCCESS != (err = comm_old->c_coll->coll_allreduce(&local_oversub, &oversubscribed, 1, MPI_INT,
                                                                MPI_SUM, comm_old, comm_old->c_coll->coll_allreduce_module)))
        return err;

    return oversubscribed;
}

#ifdef __DEBUG__
static void dump_int_array( char* prolog, char* line_prolog, int* array, size_t length )
{
    size_t i;

    fprintf(stdout,"%s : ", prolog);
    for(i = 0; i < length ; i++)
        fprintf(stdout,"%s [%lu:%i] ", line_prolog, i, array[i]);
    fprintf(stdout,"\n");
}
static void dump_double_array( char* prolog, char* line_prolog, double* array, size_t length )
{
    size_t i;

    fprintf(stdout,"%s : ", prolog);
    for(i = 0; i < length ; i++)
        fprintf(stdout,"%s [%lu:%lf] ", line_prolog, i, array[i]);
    fprintf(stdout,"\n");
}
#endif

int mca_topo_treematch_dist_graph_create(mca_topo_base_module_t* topo_module,
                                         ompi_communicator_t *comm_old,
                                         int n, const int nodes[],
                                         const int degrees[], const int targets[],
                                         const int weights[],
                                         struct opal_info_t *info, int reorder,
                                         ompi_communicator_t **newcomm)
{
    int err;

    if (OMPI_SUCCESS != (err = mca_topo_base_dist_graph_distribute(topo_module, comm_old, n, nodes,
                                                                   degrees, targets, weights,
                                                                   &(topo_module->mtc.dist_graph))))
        return err;

    if(!reorder) {  /* No reorder. Create a new communicator, then   */
                    /* jump out to attach the dist_graph and return */
    fallback:

        if( OMPI_SUCCESS == (err = ompi_comm_create(comm_old,
                                                    comm_old->c_local_group,
                                                    newcomm))){
            /* Attach the dist_graph to the newly created communicator */
            (*newcomm)->c_flags        |= OMPI_COMM_DIST_GRAPH;
            (*newcomm)->c_topo          = topo_module;
            (*newcomm)->c_topo->reorder = reorder;
        }
        return err;
    }  /* reorder == yes */

    mca_topo_base_comm_dist_graph_2_2_0_t *topo = NULL;
    ompi_proc_t *proc = NULL;
    MPI_Request  *reqs = NULL;
    hwloc_cpuset_t set;
    hwloc_obj_t object,root_obj;
    hwloc_obj_t *tracker = NULL;
    double *local_pattern = NULL;
    int *vpids, *colors = NULL;
    int *lindex_to_grank = NULL;
    int *nodes_roots = NULL;
    int *localrank_to_objnum  = NULL;
    int depth, effective_depth, obj_rank = -1;
    int num_objs_in_node = 0;
    int num_pus_in_node = 0;
    int numlevels = 0;
    int num_nodes = 0;
    int num_procs_in_node = 0;
    int rank, size;
    int *k = NULL;
    int newrank = -1;
    int hwloc_err;
    int oversubscribing_objs = 0, oversubscribed_pus = 0;
    int i, j, idx;
    uint32_t val, *pval;

    topo = topo_module->mtc.dist_graph;
    rank = ompi_comm_rank(comm_old);
    size = ompi_comm_size(comm_old);

#ifdef __DEBUG__
    fprintf(stdout,"Process rank is : %i\n",rank);
#endif
    /**
     * In order to decrease the number of loops let's use a trick:
     * build the lindex_to_grank in the vpids array, and only allocate
     * it upon completion of the most costly loop.
     */
    vpids = (int *)malloc(size * sizeof(int));
    colors = (int *)malloc(size * sizeof(int));
    for(i = 0 ; i < size ; i++) {
        proc = ompi_group_peer_lookup(comm_old->c_local_group, i);
        if (( i == rank ) ||
            (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)))
            vpids[num_procs_in_node++] = i;

        pval = &val;
        OPAL_MODEX_RECV_VALUE(err, OPAL_PMIX_NODEID, &(proc->super.proc_name), &pval, OPAL_UINT32);
        if( OPAL_SUCCESS != err ) {
            opal_output(0, "Unable to extract peer %s nodeid from the modex.\n",
                        OMPI_NAME_PRINT(&(proc->super)));
            colors[i] = -1;
            continue;
        }
        colors[i] = (int)val;
    }
    lindex_to_grank = (int *)malloc(num_procs_in_node * sizeof(int));
    memcpy(lindex_to_grank, vpids, num_procs_in_node * sizeof(int));
    memcpy(vpids, colors, size * sizeof(int));

#ifdef __DEBUG__
    fprintf(stdout,"Process rank (2) is : %i \n",rank);
    if ( 0 == rank ) {
        dump_int_array("lindex_to_grank : ", "", lindex_to_grank, num_procs_in_node);
        dump_int_array("Vpids : ", "", colors, size);
    }
#endif
    /* clean-up dupes in the array */
    for(i = 0; i < size ; i++) {
        if ( -1 == vpids[i] ) continue;
        num_nodes++;  /* compute number of nodes */
        for(j = i+1; j < size; j++)
            if( vpids[i] == vpids[j] )
                vpids[j] = -1;
    }
    if( 0 == num_nodes ) {
        /* No useful info has been retrieved from the runtime. Fallback
         * and create a duplicate of the original communicator */
        free(vpids);
        free(colors);
        err = OMPI_SUCCESS;  /* return with success */
        goto fallback;
    }
    /* compute local roots ranks in comm_old */
    /* Only the global root needs to do this */
    if(0 == rank) {
        nodes_roots = (int *)calloc(num_nodes, sizeof(int));
        for(i = idx = 0; i < size; i++)
            if( vpids[i] != -1 )
                nodes_roots[idx++] = i;
#ifdef __DEBUG__
        fprintf(stdout,"num nodes is %i\n",num_nodes);
        dump_int_array("Root nodes are :\n", "root ", nodes_roots, num_nodes);
#endif
    }
    free(vpids);

    /* Then, we need to know if the processes are bound */
    /* We make the hypothesis that all processes are in  */
    /* the same state : all bound or none bound */
    if (OPAL_SUCCESS != opal_hwloc_base_get_topology()) {
        goto fallback;
    }
    root_obj = hwloc_get_root_obj(opal_hwloc_topology);
    if (NULL == root_obj) goto fallback;

    /* if cpubind returns an error, it will be full anyway */
    set = hwloc_bitmap_alloc_full();
    hwloc_get_cpubind(opal_hwloc_topology, set, 0);
    num_pus_in_node = hwloc_get_nbobjs_by_type(opal_hwloc_topology, HWLOC_OBJ_PU);

    /**
     * In all situations (including heterogeneous environments) all processes must execute
     * all the calls that involve collective communications, so we have to lay the logic
     * accordingly.
     */
    
    if(hwloc_bitmap_isincluded(root_obj->cpuset,set)){ /* processes are not bound on the machine */
#ifdef __DEBUG__
        if (0 == rank)
            fprintf(stdout,">>>>>>>>>>>>> Process Not bound <<<<<<<<<<<<<<<\n");
#endif /* __DEBUG__ */

        /* we try to bind to cores or above objects if enough are present */
        /* Not sure that cores are present in ALL nodes */
        depth = hwloc_get_type_or_above_depth(opal_hwloc_topology,HWLOC_OBJ_CORE);
        num_objs_in_node = hwloc_get_nbobjs_by_depth(opal_hwloc_topology,depth);

        /* Check for oversubscribing */
        oversubscribing_objs = check_oversubscribing(rank,num_nodes,
                                                     num_objs_in_node,num_procs_in_node,
                                                     nodes_roots,lindex_to_grank,comm_old);
    } else {    /* the processes are already bound */
        object = hwloc_get_obj_covering_cpuset(opal_hwloc_topology,set);
        obj_rank = object->logical_index;
        effective_depth = object->depth;
        num_objs_in_node = hwloc_get_nbobjs_by_depth(opal_hwloc_topology, effective_depth);

        /* Check for oversubscribing */
        oversubscribing_objs = check_oversubscribing(rank,num_nodes,
                                                     num_objs_in_node,num_procs_in_node,
                                                     nodes_roots,lindex_to_grank,comm_old);
    }

    if(oversubscribing_objs) {
        if(hwloc_bitmap_isincluded(root_obj->cpuset,set)){ /* processes are not bound on the machine */
#ifdef __DEBUG__
            fprintf(stdout,"Oversubscribing OBJ/CORES resources => Trying to use PUs \n");
#endif
            oversubscribed_pus = check_oversubscribing(rank,num_nodes,
                                                       num_pus_in_node,num_procs_in_node,
                                                       nodes_roots,lindex_to_grank,comm_old);
        } else {
            /* Bound processes will participate with the same data as before */
            oversubscribed_pus = check_oversubscribing(rank,num_nodes,
                                                       num_objs_in_node,num_procs_in_node,
                                                       nodes_roots,lindex_to_grank,comm_old);
        }
      
        if (!oversubscribed_pus) {
            /* Update the data used to compute the correct binding */
            if(hwloc_bitmap_isincluded(root_obj->cpuset,set)){ /* processes are not bound on the machine */
                obj_rank = ompi_process_info.my_local_rank%num_pus_in_node;
                effective_depth = hwloc_topology_get_depth(opal_hwloc_topology) - 1;
                num_objs_in_node = num_pus_in_node;
#ifdef __DEBUG__
                fprintf(stdout,"Process not bound : binding on PU#%i \n",obj_rank);
#endif
            }
        }
    }

    if( !oversubscribing_objs && !oversubscribed_pus ) {
        if( hwloc_bitmap_isincluded(root_obj->cpuset,set) ) { /* processes are not bound on the machine */
            obj_rank = ompi_process_info.my_local_rank%num_objs_in_node;
            effective_depth = depth;
            object = hwloc_get_obj_by_depth(opal_hwloc_topology,effective_depth,obj_rank);
            if( NULL == object) FALLBACK();

            hwloc_bitmap_copy(set,object->cpuset);
            hwloc_bitmap_singlify(set); /* we don't want the process to move */
            hwloc_err = hwloc_set_cpubind(opal_hwloc_topology,set,0);
            if( -1 == hwloc_err) FALLBACK();
#ifdef __DEBUG__
            fprintf(stdout,"Process not bound : binding on OBJ#%i \n",obj_rank);
#endif
        } else {
#ifdef __DEBUG__
            fprintf(stdout,"Process %i bound  on OBJ #%i \n",rank,obj_rank);
            fprintf(stdout,"=====> Num obj in node : %i | num pus in node : %i\n",num_objs_in_node,num_pus_in_node);
#endif
        }
    } else {
#ifdef __DEBUG__
        fprintf(stdout,"Oversubscribing PUs resources => Rank Reordering Impossible \n");
#endif
        FALLBACK();
    }

    reqs = (MPI_Request *)calloc(num_procs_in_node-1,sizeof(MPI_Request));
    if( rank == lindex_to_grank[0] ) {  /* local leader clean the hierarchy */
        int array_size = effective_depth + 1;
        int *myhierarchy = (int *)calloc(array_size, sizeof(int));

        numlevels = 1;
        myhierarchy[0] = hwloc_get_nbobjs_by_depth(opal_hwloc_topology, 0);
        for (i = 1; i < array_size ; i++) {
            myhierarchy[i] = hwloc_get_nbobjs_by_depth(opal_hwloc_topology, i);
#ifdef __DEBUG__
            fprintf(stdout,"hierarchy[%i] = %i\n", i, myhierarchy[i]);
#endif
            if ((myhierarchy[i] != 0) && (myhierarchy[i] != myhierarchy[i-1]))
                numlevels++;
        }

        tracker = (hwloc_obj_t *)calloc(numlevels, sizeof(hwloc_obj_t));
        for(idx = 0, i = 1; i < array_size; i++) {
            if(myhierarchy[i] != myhierarchy[i-1])
                tracker[idx++] = hwloc_get_obj_by_depth(opal_hwloc_topology, i-1, 0);
        }
        tracker[idx] = hwloc_get_obj_by_depth(opal_hwloc_topology, effective_depth, 0);
        free(myhierarchy);

#ifdef __DEBUG__
        fprintf(stdout,">>>>>>>>>>>>>>>>>>>>> Effective depth is : %i (total depth %i)| num_levels %i\n",
                effective_depth,hwloc_topology_get_depth(opal_hwloc_topology), numlevels);
        for(i = 0 ; i < numlevels ; i++)
            fprintf(stdout,"tracker[%i] : arity %i | depth %i\n", i, tracker[i]->arity, tracker[i]->depth);
#endif
        /* get the obj number */
        localrank_to_objnum = (int *)calloc(num_procs_in_node, sizeof(int));
        localrank_to_objnum[0] = obj_rank;

        for(i = 1;  i < num_procs_in_node; i++) {
            if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(&localrank_to_objnum[i], 1, MPI_INT,
                                                           lindex_to_grank[i], 111, comm_old, &reqs[i-1]))))
                return err;
        }
        if (OMPI_SUCCESS != ( err = ompi_request_wait_all(num_procs_in_node-1,
                                                          reqs,MPI_STATUSES_IGNORE)))
            return err;
    } else {
        /* sending my core number to my local master on the node */
        if (OMPI_SUCCESS != (err = MCA_PML_CALL(send(&obj_rank, 1, MPI_INT, lindex_to_grank[0],
                                                     111, MCA_PML_BASE_SEND_STANDARD, comm_old))))
            return err;
    }
    free(reqs);

    /* Centralized Reordering */
    if (0 == mca_topo_treematch_component.reorder_mode) {
        int *k = NULL;
        int *obj_mapping = NULL;
        int num_objs_total = 0;

        /* Gather comm pattern
         * If weights have been provided take them in account. Otherwise rely
         * solely on HWLOC information.
         */
        if( 0 == rank ) {

#ifdef __DEBUG__
            fprintf(stderr,"========== Centralized Reordering ========= \n");
#endif
            local_pattern = (double *)calloc(size*size,sizeof(double));
        } else {
            local_pattern = (double *)calloc(size,sizeof(double));
        }
        if( true == topo->weighted ) {
            for(i = 0; i < topo->indegree ; i++)
                local_pattern[topo->in[i]] += topo->inw[i];
            for(i = 0; i < topo->outdegree ; i++)
                local_pattern[topo->out[i]] += topo->outw[i];
        }
        err = comm_old->c_coll->coll_gather( (0 == rank ? MPI_IN_PLACE : local_pattern), size, MPI_DOUBLE,
                                             local_pattern, size, MPI_DOUBLE,  /* ignored on non-root */
                                             0, comm_old, comm_old->c_coll->coll_gather_module);
        if (OMPI_SUCCESS != err) {
            return err;
        }

        if( rank == lindex_to_grank[0] ) {
            tm_topology_t *tm_topology = NULL;
            tm_topology_t *tm_opt_topology = NULL;
            int *obj_to_rank_in_comm = NULL;
            int *hierarchies = NULL;
            int  hierarchy[TM_MAX_LEVELS+1];
            int  min;

            /* create a table that derives the rank in comm_old from the object number */
            obj_to_rank_in_comm = (int *)malloc(num_objs_in_node*sizeof(int));
            for(i = 0 ; i < num_objs_in_node ; i++) {
                obj_to_rank_in_comm[i] = -1;
                object = hwloc_get_obj_by_depth(opal_hwloc_topology, effective_depth, i);
                for( j = 0; j < num_procs_in_node ; j++ )
                    if(localrank_to_objnum[j] == (int)(object->logical_index)) {
                        obj_to_rank_in_comm[i] = lindex_to_grank[j];
                        break;
                    }
            }

            /* the global master gathers info from local_masters */
            if ( 0 == rank ) {
                if ( num_nodes > 1 ) {
                    int *objs_per_node = NULL, displ;

                    objs_per_node = (int *)calloc(num_nodes, sizeof(int));
                    reqs = (MPI_Request *)calloc(num_nodes-1, sizeof(MPI_Request));
                    objs_per_node[0] = num_objs_in_node;
                    for(i = 1; i < num_nodes ; i++)
                        if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(objs_per_node + i, 1, MPI_INT,
                                                                       nodes_roots[i], 111, comm_old, &reqs[i-1]))))
                            ERR_EXIT(err);

                    if (OMPI_SUCCESS != ( err = ompi_request_wait_all(num_nodes - 1,
                                                                      reqs, MPI_STATUSES_IGNORE)))
                        ERR_EXIT(err);

                    for(i = 0; i < num_nodes; i++)
                        num_objs_total += objs_per_node[i];
                    obj_mapping = (int *)calloc(num_objs_total,sizeof(int));

                    memcpy(obj_mapping, obj_to_rank_in_comm, objs_per_node[0]*sizeof(int));
                    displ = objs_per_node[0];
                    for(i = 1; i < num_nodes ; i++) {
                        if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(obj_mapping + displ, objs_per_node[i], MPI_INT,
                                                                       nodes_roots[i], 111, comm_old, &reqs[i-1]))))
                            ERR_EXIT(err);
                        displ += objs_per_node[i];
                    }
                    if (OMPI_SUCCESS != ( err = ompi_request_wait_all(num_nodes - 1,
                                                                      reqs, MPI_STATUSES_IGNORE)))
                        ERR_EXIT(err);
                    free(objs_per_node);
                } else {
                    /* if num_nodes == 1, then it's easy to get the obj mapping */
                    num_objs_total = num_objs_in_node;
                    obj_mapping = (int *)calloc(num_objs_total, sizeof(int));
                    memcpy(obj_mapping, obj_to_rank_in_comm, num_objs_total*sizeof(int));
                }
#ifdef __DEBUG__
                dump_int_array( "Obj mapping : ", "", obj_mapping, num_objs_total );
#endif
            } else {
                if ( num_nodes > 1 ) {
                    if (OMPI_SUCCESS != (err = MCA_PML_CALL(send(&num_objs_in_node, 1, MPI_INT,
                                                                 0, 111, MCA_PML_BASE_SEND_STANDARD, comm_old))))
                        ERR_EXIT(err);
                    if (OMPI_SUCCESS != (err = MCA_PML_CALL(send(obj_to_rank_in_comm, num_objs_in_node, MPI_INT,
                                                                 0, 111, MCA_PML_BASE_SEND_STANDARD, comm_old))))
                        ERR_EXIT(err);
                }
            }
            free(obj_to_rank_in_comm);

            hierarchy[0] = numlevels;
            assert(numlevels < TM_MAX_LEVELS);

            for(i = 0 ; i < hierarchy[0]; i++)
                hierarchy[i+1] = tracker[i]->arity;
            for(; i < (TM_MAX_LEVELS+1); i++)  /* fill up everything else with -1 */
                hierarchy[i] = -1;

            if( 0 == rank ) {
                hierarchies = (int *)malloc(num_nodes*(TM_MAX_LEVELS+1)*sizeof(int));
                memcpy(hierarchies, hierarchy, (TM_MAX_LEVELS+1)*sizeof(int));
            }

            /* gather hierarchies iff more than 1 node! */
            if ( num_nodes > 1 ) {
                if( rank != 0 ) {
                    if (OMPI_SUCCESS != (err = MCA_PML_CALL(send(hierarchy,(TM_MAX_LEVELS+1), MPI_INT, 0,
                                                                 111, MCA_PML_BASE_SEND_STANDARD, comm_old))))
                        ERR_EXIT(err);
                } else {
                    for(i = 1; i < num_nodes ; i++)
                        if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(hierarchies+i*(TM_MAX_LEVELS+1), (TM_MAX_LEVELS+1), MPI_INT,
                                                                       nodes_roots[i], 111, comm_old, &reqs[i-1])))){
                            free(hierarchies);
                            ERR_EXIT(err);
                        }
                    if (OMPI_SUCCESS != ( err = ompi_request_wait_all(num_nodes - 1,
                                                                      reqs,MPI_STATUSES_IGNORE))) {
                        free(hierarchies);
                        ERR_EXIT(err);
                    }
                    free(reqs);
                }
            }

            if ( 0 == rank ) {
                tm_tree_t *comm_tree = NULL;
		        tm_solution_t *sol = NULL;
		        tm_affinity_mat_t *aff_mat = NULL;
                double **comm_pattern = NULL;

#ifdef __DEBUG__
                dump_int_array("hierarchies : ", "", hierarchies, num_nodes*(TM_MAX_LEVELS+1));
#endif
                tm_topology = (tm_topology_t *)malloc(sizeof(tm_topology_t));
                tm_topology->nb_levels = hierarchies[0];

                /* extract min depth */
                for(i = 1 ; i < num_nodes ; i++)
                    if (hierarchies[i*(TM_MAX_LEVELS+1)] < tm_topology->nb_levels)
                        tm_topology->nb_levels = hierarchies[i*(TM_MAX_LEVELS+1)];

                /* Crush levels in hierarchies too long (ie > tm_topology->nb_levels)*/
                for(i = 0; i < num_nodes ; i++) {
                    int *base_ptr = hierarchies + i*(TM_MAX_LEVELS+1);
                    int  suppl = *base_ptr - tm_topology->nb_levels;
                    for(j = 1 ; j <= suppl ; j++)
                        *(base_ptr + tm_topology->nb_levels) *= *(base_ptr + tm_topology->nb_levels + j);
                }
                if( num_nodes > 1) {
                    /* We aggregate all topos => +1 level!*/
                    tm_topology->nb_levels += 1;
                    tm_topology->arity = (int *)calloc(tm_topology->nb_levels, sizeof(int));
                    tm_topology->arity[0] = num_nodes;
                    for(i = 1; i < tm_topology->nb_levels; i++) {  /* compute the minimum for each level */
                        min = hierarchies[i];
                        for(j = 1; j < num_nodes ; j++)
                            if( hierarchies[j*(TM_MAX_LEVELS+1) + i] < min)
                                min = hierarchies[j*(TM_MAX_LEVELS+1) + i];
                        tm_topology->arity[i] = min;
                    }
                } else {
                    tm_topology->arity = (int *)calloc(tm_topology->nb_levels, sizeof(int));
                    for(i = 0; i < tm_topology->nb_levels; i++)
                        tm_topology->arity[i] = hierarchies[i+1];
                }
                free(hierarchies);
#ifdef __DEBUG__
                for(i = 0; i < tm_topology->nb_levels; i++)
                    fprintf(stdout,"topo_arity[%i] = %i\n", i, tm_topology->arity[i]);
#endif
                /* compute the number of processing elements */
                tm_topology->nb_nodes = (size_t *)calloc(tm_topology->nb_levels, sizeof(size_t));
                tm_topology->nb_nodes[0] = 1;
                for(i = 1 ; i < tm_topology->nb_levels; i++)
                    tm_topology->nb_nodes[i] = tm_topology->nb_nodes[i-1] * tm_topology->arity[i-1];

                /* Build process id tab */
                tm_topology->node_id  = (int **)calloc(tm_topology->nb_levels, sizeof(int*));
		        tm_topology->node_rank = (int **)malloc(sizeof(int *) * tm_topology->nb_levels);
                for(i = 0; i < tm_topology->nb_levels; i++) {
                    tm_topology->node_id[i] = (int *)calloc(tm_topology->nb_nodes[i], sizeof(int));
		            tm_topology->node_rank[i] = (int * )calloc(tm_topology->nb_nodes[i], sizeof(int));
		            /*note : we make the hypothesis that logical indexes in hwloc range from
                      0 to N, are contiguous and crescent.  */

		            for( j = 0 ; j < tm_topology->nb_nodes[i] ; j++ ){
		                tm_topology->node_id[i][j] = j;
		                tm_topology->node_rank[i][j] = j;
		      
		                /* Should use object->logical_index */
		                /* obj = hwloc_get_obj_by_depth(topo,i,j%num_objs_in_node);
                           id = obj->logical_index + (num_objs_in_node)*(j/num_obj_in_node)*/
		                /*
		                  int id = core_numbering[j%nb_core_per_nodes] + (nb_core_per_nodes)*(j/nb_core_per_nodes);
	                      topology->node_id[i][j] = id;
                                               topology->node_rank[i][id] = j;
                        */
                    }
                }
                /* unused for now*/
                tm_topology->cost = (double*)calloc(tm_topology->nb_levels,sizeof(double));

                tm_topology->nb_proc_units = num_objs_total;
		
                tm_topology->nb_constraints = 0;
                for(i = 0; i < tm_topology->nb_proc_units ; i++)
                    if (obj_mapping[i] != -1)
                        tm_topology->nb_constraints++;
                tm_topology->constraints = (int *)calloc(tm_topology->nb_constraints,sizeof(int));		
                for(idx = 0, i = 0; i < tm_topology->nb_proc_units ; i++)
                    if (obj_mapping[i] != -1)
                        tm_topology->constraints[idx++] = obj_mapping[i];		

                tm_topology->oversub_fact = 1;

#ifdef __DEBUG__
                assert(num_objs_total == tm_topology->nb_nodes[tm_topology->nb_levels-1]);
		
                for(i = 0; i < tm_topology->nb_levels ; i++) {
                    fprintf(stdout,"tm topo node_id for level [%i] : ",i);
                    dump_int_array("", "", obj_mapping, tm_topology->nb_nodes[i]);
                }
                tm_display_topology(tm_topology);
#endif

                comm_pattern = (double **)malloc(size*sizeof(double *));
                for(i = 0 ; i < size ; i++)
                    comm_pattern[i] = local_pattern + i * size;
                /* matrix needs to be symmetric */
                for( i = 0; i < size ; i++ )
                    for( j = i; j < size ; j++ ) {
                        comm_pattern[i][j] = (comm_pattern[i][j] + comm_pattern[j][i]) / 2;
                        comm_pattern[j][i] = comm_pattern[i][j];
                    }
#ifdef __DEBUG__
                fprintf(stdout,"==== COMM PATTERN ====\n");
                for( i = 0 ; i < size ; i++) {
                    dump_double_array("", "", comm_pattern[i], size);
                }
#endif
                tm_optimize_topology(&tm_topology);
                aff_mat = tm_build_affinity_mat(comm_pattern,size);		  
                comm_tree = tm_build_tree_from_topology(tm_topology,aff_mat, NULL, NULL);
                sol = tm_compute_mapping(tm_topology, comm_tree);

                k = (int *)calloc(sol->k_length, sizeof(int));
                for(idx = 0 ; idx < sol->k_length ; idx++)
                    k[idx] = sol->k[idx][0];

#ifdef __DEBUG__
                fprintf(stdout,"====> nb levels : %i\n",tm_topology->nb_levels);
                dump_int_array("Rank permutation sigma/k : ", "", k, num_objs_total);
		        assert(size == sol->sigma_length);
                dump_int_array("Matching : ", "",sol->sigma, sol->sigma_length);
#endif
                free(obj_mapping);
                free(comm_pattern);
                free(aff_mat->sum_row);
                free(aff_mat);		
                tm_free_solution(sol);
                tm_free_tree(comm_tree);
                tm_free_topology(tm_topology);
            }
        }

        /* Todo : Bcast + group creation */
        /* scatter the ranks */
        if (OMPI_SUCCESS != (err = comm_old->c_coll->coll_scatter(k, 1, MPI_INT,
                                                                  &newrank, 1, MPI_INT,
                                                                  0, comm_old, comm_old->c_coll->coll_scatter_module)))
            ERR_EXIT(err);

        if ( 0 == rank )
            free(k);

        /* this needs to be optimized but will do for now */
        if (OMPI_SUCCESS != (err = ompi_comm_split(comm_old, 0, newrank, newcomm, false)))
            ERR_EXIT(err);
        /* end of TODO */

        /* Attach the dist_graph to the newly created communicator */
        (*newcomm)->c_flags        |= OMPI_COMM_DIST_GRAPH;
        (*newcomm)->c_topo          = topo_module;
        (*newcomm)->c_topo->reorder = reorder;

    } else { /* partially distributed reordering */
        ompi_communicator_t *localcomm = NULL;
        int *matching = (int *)calloc(num_procs_in_node,sizeof(int));
        int *lrank_to_grank = (int *)calloc(num_procs_in_node,sizeof(int));
        int *grank_to_lrank = (int *)calloc(size,sizeof(int));

        if (OMPI_SUCCESS != (err = ompi_comm_split(comm_old, colors[rank], rank,
                                                   &localcomm, false)))
            return err;

        if (OMPI_SUCCESS != (err = localcomm->c_coll->coll_allgather(&rank, 1, MPI_INT,
                                                                     lrank_to_grank, 1, MPI_INT,
                                                                     localcomm, localcomm->c_coll->coll_allgather_module)))
            return err;

        for(i = 0 ; i < size ; grank_to_lrank[i++] = -1);
        for(i = 0 ; i < num_procs_in_node ; i++)
            grank_to_lrank[lrank_to_grank[i]] = i;

        /* Discover the local patterns */
        if (rank == lindex_to_grank[0]) {
#ifdef __DEBUG__
            fprintf(stderr,"========== Partially Distributed Reordering ========= \n");
#endif
            local_pattern = (double *)calloc(num_procs_in_node * num_procs_in_node, sizeof(double));
        } else {
            local_pattern = (double *)calloc(num_procs_in_node, sizeof(double));
        }
        /* Extract the local communication pattern */
        if( true == topo->weighted ) {
            for(i = 0; i < topo->indegree; i++)
                if (grank_to_lrank[topo->in[i]] != -1)
                    local_pattern[grank_to_lrank[topo->in[i]]] += topo->inw[i];
            for(i = 0; i < topo->outdegree; i++)
                if (grank_to_lrank[topo->out[i]] != -1)
                    local_pattern[grank_to_lrank[topo->out[i]]] += topo->outw[i];
        }
        if (OMPI_SUCCESS != (err = localcomm->c_coll->coll_gather((rank == lindex_to_grank[0] ? MPI_IN_PLACE : local_pattern),
                                                                  num_procs_in_node, MPI_DOUBLE,
                                                                  local_pattern, num_procs_in_node, MPI_DOUBLE,
                                                                  0, localcomm, localcomm->c_coll->coll_gather_module)))
            ERR_EXIT(err);

        /* The root has now the entire information, so let's crunch it */
        if (rank == lindex_to_grank[0]) {
            tm_topology_t  *tm_topology = NULL;
            tm_tree_t *comm_tree = NULL;
            tm_solution_t *sol = NULL;
            tm_affinity_mat_t *aff_mat = NULL;
            double **comm_pattern = NULL;

            comm_pattern = (double **)malloc(num_procs_in_node*sizeof(double *));
            for( i = 0; i < num_procs_in_node; i++ ) {
                comm_pattern[i] = local_pattern + i * num_procs_in_node;
            }
            /* Matrix needs to be symmetric. Beware: as comm_patterns
             * refers to local_pattern we indirectly alter the content
             * of local_pattern */
            for( i = 0; i < num_procs_in_node ; i++ )
                for( j = i; j < num_procs_in_node ; j++ ) {
                    comm_pattern[i][j] = (comm_pattern[i][j] + comm_pattern[j][i]) / 2;
                    comm_pattern[j][i] = comm_pattern[i][j];
                }

#ifdef __DEBUG__
            fprintf(stdout,"========== COMM PATTERN ============= \n");
            for(i = 0 ; i < num_procs_in_node ; i++){
                fprintf(stdout," %i : ",i);
                dump_double_array("", "", comm_pattern[i], num_procs_in_node);
            }
            fprintf(stdout,"======================= \n");
#endif

            tm_topology  = (tm_topology_t *)malloc(sizeof(tm_topology_t));
            tm_topology->nb_levels = numlevels;
            tm_topology->arity     = (int *)calloc(tm_topology->nb_levels, sizeof(int));
            tm_topology->nb_nodes  = (size_t *)calloc(tm_topology->nb_levels, sizeof(size_t));
            tm_topology->node_id   = (int **)malloc(tm_topology->nb_levels*sizeof(int *));
            tm_topology->node_rank = (int **)malloc(tm_topology->nb_levels*sizeof(int *));
	    
            for(i = 0 ; i < tm_topology->nb_levels ; i++){
                int nb_objs = hwloc_get_nbobjs_by_depth(opal_hwloc_topology, tracker[i]->depth);
                tm_topology->nb_nodes[i] = nb_objs;
                tm_topology->arity[i]    = tracker[i]->arity;
                tm_topology->node_id[i]  = (int *)calloc(tm_topology->nb_nodes[i], sizeof(int));
                tm_topology->node_rank[i] = (int * )calloc(tm_topology->nb_nodes[i], sizeof(int)); 
                for(j = 0; j < tm_topology->nb_nodes[i] ; j++){
                    tm_topology->node_id[i][j] = j;
                    tm_topology->node_rank[i][j] = j;
                }
            }

            /* unused for now*/
            tm_topology->cost = (double*)calloc(tm_topology->nb_levels,sizeof(double));

            tm_topology->nb_proc_units = num_objs_in_node;
            //tm_topology->nb_proc_units = num_procs_in_node; 
            tm_topology->nb_constraints = 0;
            for(i = 0; i < num_procs_in_node ; i++)
                if (localrank_to_objnum[i] != -1)
                    tm_topology->nb_constraints++;
	    
            tm_topology->constraints = (int *)calloc(tm_topology->nb_constraints,sizeof(int));
            for(idx = 0,i = 0; i < num_procs_in_node ; i++)
                if (localrank_to_objnum[i] != -1)
                    tm_topology->constraints[idx++] = localrank_to_objnum[i];
	    
            tm_topology->oversub_fact = 1;
	    
#ifdef __DEBUG__
            assert(num_objs_in_node == tm_topology->nb_nodes[tm_topology->nb_levels-1]);
            fprintf(stdout,"Levels in topo : %i | num procs in node : %i\n",tm_topology->nb_levels,num_procs_in_node);
            for(i = 0; i < tm_topology->nb_levels ; i++){
                fprintf(stdout,"Nb objs for level %i : %i | arity %i\n ",i,tm_topology->nb_nodes[i],tm_topology->arity[i]);
                dump_int_array("", "Obj id ", tm_topology->node_id[i], tm_topology->nb_nodes[i]);
            }
            tm_display_topology(tm_topology);
#endif
            tm_optimize_topology(&tm_topology);
            aff_mat = tm_build_affinity_mat(comm_pattern,num_procs_in_node);
            comm_tree = tm_build_tree_from_topology(tm_topology,aff_mat, NULL, NULL);
            sol = tm_compute_mapping(tm_topology, comm_tree);

            k = (int *)calloc(sol->k_length, sizeof(int));
            for(idx = 0 ; idx < sol->k_length ; idx++)
                k[idx] = sol->k[idx][0];

#ifdef __DEBUG__
            fprintf(stdout,"====> nb levels : %i\n",tm_topology->nb_levels);
            dump_int_array("Rank permutation sigma/k : ", "", k, num_procs_in_node);
            assert(num_procs_in_node == sol->sigma_length);
            dump_int_array("Matching : ", "",sol->sigma, sol->sigma_length);
#endif
	    
            free(aff_mat->sum_row);
            free(aff_mat);
            free(comm_pattern);
            tm_free_solution(sol);
            tm_free_tree(comm_tree);
            tm_free_topology(tm_topology);
        }

        /* Todo : Bcast + group creation */
        /* scatter the ranks */
        if (OMPI_SUCCESS != (err = localcomm->c_coll->coll_bcast(matching, num_procs_in_node,
                                                                 MPI_INT,0,localcomm,
                                                                 localcomm->c_coll->coll_bcast_module)))
            ERR_EXIT(err);

        if ( 0 == rank )
            free(k);

        /* this needs to be optimized but will do for now */
        if (OMPI_SUCCESS != (err = ompi_comm_split(localcomm, 0, newrank, newcomm, false)))
            ERR_EXIT(err);
        /* end of TODO */

        /* Attach the dist_graph to the newly created communicator */
        (*newcomm)->c_flags        |= OMPI_COMM_DIST_GRAPH;
        (*newcomm)->c_topo          = topo_module;
        (*newcomm)->c_topo->reorder = reorder;
    	
        free(matching);
        free(grank_to_lrank);
        free(lrank_to_grank);
    } /* distributed reordering end */

    if(rank == lindex_to_grank[0])
        free(tracker);
    free(nodes_roots);
    free(lindex_to_grank);
    free(local_pattern);
    free(localrank_to_objnum);
    free(colors);
    hwloc_bitmap_free(set);
    return err;
}
