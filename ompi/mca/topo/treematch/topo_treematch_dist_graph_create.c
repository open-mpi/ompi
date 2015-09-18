/*
 * Copyright (c) 2011-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2015 INRIA.  All rights reserved.
 * Copyright (c) 2012-2015 Bordeaux Poytechnic Institute
 * Copyright (c) 2015      Intel, Inc. All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/constants.h"
#include "opal/mca/hwloc/hwloc.h"

#include "ompi/mca/topo/treematch/topo_treematch.h"
#include "ompi/mca/topo/treematch/treematch/tm_mapping.h"
#include "ompi/mca/topo/base/base.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"

#include "ompi/mca/pml/pml.h"

#include "opal/mca/pmix/pmix.h"

#define ERR_EXIT(ERR)                           \
    do { free(local_pattern);                   \
        return (ERR); }                         \
    while(0);

#define FALLBACK()                  \
    do { free(nodes_roots);			\
        free(local_procs);          \
        hwloc_bitmap_free(set);     \
        goto fallback; }            \
    while(0);

#define MY_STRING_SIZE 64
/*#define __DEBUG__ 1  */


static int check_oversubscribing(int rank,
                                 int num_nodes,
                                 int num_objs_in_node,
                                 int num_procs_in_node,
                                 int *nodes_roots,
                                 int *local_procs,
                                 ompi_communicator_t *comm_old)
{
    int oversubscribed = 0;
    int local_oversub = 0;
    int err;

    if (rank == local_procs[0])
        if(num_objs_in_node < num_procs_in_node)
            local_oversub =  1;

    if (rank == 0) {
        MPI_Request *reqs = (MPI_Request *)calloc(num_nodes-1, sizeof(MPI_Request));
        int *oversub = (int *)calloc(num_nodes, sizeof(int));
        int i;

        oversub[0] = local_oversub;
        for(i = 1;  i < num_nodes; i++)
            if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(&oversub[i], 1, MPI_INT,
                                                           nodes_roots[i], 111, comm_old, &reqs[i-1]))))
                return err;

        if (OMPI_SUCCESS != ( err = ompi_request_wait_all(num_nodes-1,
                                                          reqs, MPI_STATUSES_IGNORE)))
            return err;

        for(i = 0;  i < num_nodes; i++)
            oversubscribed += oversub[i];

        free(oversub);
        free(reqs);
    } else {
        if (rank == local_procs[0])
            if (OMPI_SUCCESS != (err = MCA_PML_CALL(send(&local_oversub, 1, MPI_INT, 0,
                                                         111, MCA_PML_BASE_SEND_STANDARD, comm_old))))
                return err;
    }

    if (OMPI_SUCCESS != (err = comm_old->c_coll.coll_bcast(&oversubscribed, 1,
                                                           MPI_INT, 0, comm_old,
                                                           comm_old->c_coll.coll_bcast_module)))
        return err;

    return oversubscribed;
}

int mca_topo_treematch_dist_graph_create(mca_topo_base_module_t* topo_module,
                                         ompi_communicator_t *comm_old,
                                         int n, const int nodes[],
                                         const int degrees[], const int targets[],
                                         const int weights[],
                                         struct ompi_info_t *info, int reorder,
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
    int *local_procs = NULL;
    int *nodes_roots = NULL;
    int *localrank_to_objnum  = NULL;
    int depth, effective_depth, obj_rank = -1;
    int num_objs_in_node = 0;
    int num_pus_in_node = 0;
    int numlevels = 0;
    int num_nodes = 0;
    int num_procs_in_node = 0;
    int rank, size;
    int hwloc_err;
    int oversubscribing_objs = 0;
    int i, j, idx;
    uint32_t val, *pval;

    topo = topo_module->mtc.dist_graph;
    rank = ompi_comm_rank(comm_old);
    size = ompi_comm_size(comm_old);

#ifdef __DEBUG__
    fprintf(stdout,"Process rank is : %i\n",rank);
#endif
    /* Determine the number of local procs */
    /* and the number of ext procs         */
    for(i = 0 ; i < size ; i++){
        proc = ompi_group_peer_lookup(comm_old->c_local_group, i);
        if (( i == rank ) ||
            (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)))
            num_procs_in_node++;
    }

    /* Get the ranks of the local procs in comm_old */
    local_procs = (int *)malloc(num_procs_in_node * sizeof(int));
    for(i = idx = 0 ; i < size ; i++){
        proc = ompi_group_peer_lookup(comm_old->c_local_group, i);
        if (( i == rank ) ||
            (OPAL_PROC_ON_LOCAL_NODE(proc->super.proc_flags)))
            local_procs[idx++] = i;
    }

    vpids = (int *)malloc(size * sizeof(int));
    colors = (int *)malloc(size * sizeof(int));
    for(i = 0; i < size ; i++) {
        proc = ompi_group_peer_lookup(comm_old->c_local_group, i);
        pval = &val;
        OPAL_MODEX_RECV_VALUE(err, OPAL_PMIX_NODEID, &(proc->super.proc_name), &pval, OPAL_UINT32);
        if( OPAL_SUCCESS != err ) {
            opal_output(0, "Unable to extract peer %s nodeid from the modex.\n",
                        OMPI_NAME_PRINT(&(proc->super)));
            vpids[i] = colors[i] = -1;
            continue;
        }
        vpids[i] = colors[i] = (int)val;
    }

#ifdef __DEBUG__
    fprintf(stdout,"Process rank (2) is : %i \n",rank);
    if ( 0 == rank ){
        fprintf(stdout,"local_procs : ");
        for(i = 0; i < num_procs_in_node ; i++)
            fprintf(stdout," [%i:%i] ",i,local_procs[i]);
        fprintf(stdout,"\n");

        fprintf(stdout,"Vpids : ");
        for(i = 0; i < size ; i++)
            fprintf(stdout," [%i:%i] ",i,vpids[i]);
        fprintf(stdout,"\n");
    }
#endif
    /* clean-up dupes in the array */
    for(i = 0; i < size ; i++)
        if ( -1 ==  vpids[i] )
            continue;
        else
            for(j = i+1 ; j < size ; j++)
                if( vpids[j] != -1 )
                    if( vpids[i] == vpids[j] )
                        vpids[j] = -1;
    /* compute number of nodes */
    for(i = 0; i < size ; i++)
        if( vpids[i] != -1 )
            num_nodes++;
    /* compute local roots ranks in comm_old */
    /* Only the global root needs to do this */
    if(0 == rank) {
        nodes_roots = (int *)calloc(num_nodes,sizeof(int));
        for(i = idx = 0; i < size ; i++)
            if( vpids[i] != -1 )
                nodes_roots[idx++] = i;
#ifdef __DEBUG__
        fprintf(stdout,"num nodes is %i\n",num_nodes);
        fprintf(stdout,"Root nodes are :\n");
        for(i = 0; i < num_nodes ; i++)
            fprintf(stdout," [root %i : %i] ",i,nodes_roots[i]);
        fprintf(stdout,"\n");
#endif
    }
    free(vpids);

    /* Then, we need to know if the processes are bound */
    /* We make the hypothesis that all processes are in  */
    /* the same state : all bound or none bound */
    assert(NULL != opal_hwloc_topology);
    root_obj = hwloc_get_root_obj(opal_hwloc_topology);
    if (NULL == root_obj) goto fallback;

    /* if cpubind returns an error, it will be full anyway */
    set = hwloc_bitmap_alloc_full();
    hwloc_get_cpubind(opal_hwloc_topology,set,0);
    num_pus_in_node = hwloc_get_nbobjs_by_type(opal_hwloc_topology, HWLOC_OBJ_PU);

    if(hwloc_bitmap_isincluded(root_obj->cpuset,set)){
        /* processes are not bound on the machine */
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
                                                     nodes_roots,local_procs,comm_old);
        if(oversubscribing_objs) {
#ifdef __DEBUG__
            fprintf(stdout,"Oversubscribing OBJ/CORES resources => Trying to use PUs \n");
#endif
            int oversubscribed_pus = check_oversubscribing(rank,num_nodes,
                                                           num_pus_in_node,num_procs_in_node,
                                                           nodes_roots,local_procs,comm_old);
            if (oversubscribed_pus){
#ifdef __DEBUG__
                fprintf(stdout,"Oversubscribing PUs resources => Rank Reordering Impossible \n");
#endif
                FALLBACK();
            } else {
                obj_rank = ompi_process_info.my_local_rank%num_pus_in_node;
                effective_depth = hwloc_topology_get_depth(opal_hwloc_topology) - 1;
                num_objs_in_node = num_pus_in_node;
#ifdef __DEBUG__
                fprintf(stdout,"Process not bound : binding on PU#%i \n",obj_rank);
#endif
            }
        } else {
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
        }
    } else {    /* the processes are already bound */
        object = hwloc_get_obj_covering_cpuset(opal_hwloc_topology,set);
        obj_rank = object->logical_index;
        effective_depth = object->depth;
        num_objs_in_node = hwloc_get_nbobjs_by_depth(opal_hwloc_topology, effective_depth);

        /* Check for oversubscribing */
        oversubscribing_objs = check_oversubscribing(rank,num_nodes,
                                                     num_objs_in_node,num_procs_in_node,
                                                     nodes_roots,local_procs,comm_old);
        if(oversubscribing_objs) {
#ifdef __DEBUG__
            fprintf(stdout,"Oversubscribing OBJ/CORES resources =>  Rank Reordering Impossible\n");
#endif
            FALLBACK();
        }
#ifdef __DEBUG__
        fprintf(stdout,"Process %i bound  on OBJ #%i \n",rank,obj_rank);
        fprintf(stdout,"=====> Num obj in node : %i | num pus in node : %i\n",num_objs_in_node,num_pus_in_node);
#endif
    }

    reqs = (MPI_Request *)calloc(num_procs_in_node-1,sizeof(MPI_Request));
    if( rank == local_procs[0] ) {
        /* we need to find the right elements of the hierarchy */
        /* and remove the unneeded elements                    */
        /* Only local masters need to do this                  */
        int array_size = effective_depth + 1;
        int *myhierarchy = (int *)calloc(array_size,sizeof(int));

        for (i = 0; i < array_size ; i++) {
            myhierarchy[i] = hwloc_get_nbobjs_by_depth(opal_hwloc_topology,i);
#ifdef __DEBUG__
            fprintf(stdout,"hierarchy[%i] = %i\n",i,myhierarchy[i]);
#endif
        }
        numlevels = 1;
        for (i = 1; i < array_size; i++)
            if ((myhierarchy[i] != 0) && (myhierarchy[i] != myhierarchy[i-1]))
                numlevels++;

        tracker = (hwloc_obj_t *)calloc(numlevels,sizeof(hwloc_obj_t));
        idx = 0;
        i = 1;
        while (i < array_size){
            if(myhierarchy[i] != myhierarchy[i-1])
                tracker[idx++] = hwloc_get_obj_by_depth(opal_hwloc_topology,i-1,0);
            i++;
        }
        tracker[idx] = hwloc_get_obj_by_depth(opal_hwloc_topology,effective_depth,0);
        free(myhierarchy);

#ifdef __DEBUG__
        fprintf(stdout,">>>>>>>>>>>>>>>>>>>>> Effective depth is : %i (total depth %i)| num_levels %i\n",
                effective_depth,hwloc_topology_get_depth(opal_hwloc_topology),numlevels);
        for(i = 0 ; i < numlevels ; i++)
            fprintf(stdout,"tracker[%i] : arity %i | depth %i\n",i,tracker[i]->arity,tracker[i]->depth);
#endif
        /* get the obj number */
        localrank_to_objnum = (int *)calloc(num_procs_in_node,sizeof(int));
        localrank_to_objnum[0] = obj_rank;

        for(i = 1;  i < num_procs_in_node; i++) {
            if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(&localrank_to_objnum[i],1,MPI_INT,
                                                           local_procs[i],111, comm_old,&reqs[i-1]))))
                return err;
        }
        if (OMPI_SUCCESS != ( err = ompi_request_wait_all(num_procs_in_node-1,
                                                          reqs,MPI_STATUSES_IGNORE)))
            return err;
    } else {
        /* sending my core number to my local master on the node */
        if (OMPI_SUCCESS != (err = MCA_PML_CALL(send(&obj_rank, 1, MPI_INT, local_procs[0],
                                                     111, MCA_PML_BASE_SEND_STANDARD, comm_old))))
            return err;
    }
    free(reqs);

    /* Centralized Reordering */
    if (0 == mca_topo_treematch_component.reorder_mode) {
        int *k = NULL;
        int *obj_mapping = NULL;
        int newrank = -1;
        int num_objs_total = 0;

        /* Gather comm pattern
         * If weights have been provided take them in account. Otherwise rely
         * solely on HWLOC information.
         */
        if(0 == rank) {

            fprintf(stderr,"========== Centralized Reordering ========= \n");

            local_pattern = (double *)calloc(size*size,sizeof(double));
            if( true == topo->weighted ) {
                for(i = 0; i < topo->indegree ; i++)
                    local_pattern[topo->in[i]] += topo->inw[i];
                for(i = 0; i < topo->outdegree ; i++)
                    local_pattern[topo->out[i]] += topo->outw[i];
                if (OMPI_SUCCESS != (err = comm_old->c_coll.coll_gather(MPI_IN_PLACE, size, MPI_DOUBLE,
                                                                        local_pattern, size, MPI_DOUBLE,
                                                                        0, comm_old,
                                                                        comm_old->c_coll.coll_gather_module)))
                    return err;
            }
        } else {
            local_pattern = (double *)calloc(size,sizeof(double));
            if( true == topo->weighted ) {
                for(i = 0; i < topo->indegree ; i++)
                    local_pattern[topo->in[i]] += topo->inw[i];
                for(i = 0; i < topo->outdegree ; i++)
                    local_pattern[topo->out[i]] += topo->outw[i];
                if (OMPI_SUCCESS != (err = comm_old->c_coll.coll_gather(local_pattern, size, MPI_DOUBLE,
                                                                        NULL,0,0,
                                                                        0, comm_old,
                                                                        comm_old->c_coll.coll_gather_module)))
                    return err;
            }
        }

        if( rank == local_procs[0]) {
            tm_topology_t *tm_topology = NULL;
            tm_topology_t *tm_opt_topology = NULL;
            int *obj_to_rank_in_comm = NULL;
            int *hierarchies = NULL;
            int  hierarchy[MAX_LEVELS+1];
            int  min;

            /* create a table that derives the rank in comm_old from the object number */
            obj_to_rank_in_comm = (int *)malloc(num_objs_in_node*sizeof(int));
            for(i = 0 ; i < num_objs_in_node ; i++)
                obj_to_rank_in_comm[i] = -1;
            for(i = 0 ; i < num_objs_in_node ; i++) {
                object = hwloc_get_obj_by_depth(opal_hwloc_topology,effective_depth,i);
                for( j = 0; j < num_procs_in_node ; j++ )
                    if(localrank_to_objnum[j] == (int)(object->logical_index))
                        break;
                if(j == num_procs_in_node)
                    obj_to_rank_in_comm[i] = -1;
                else {
                    int k;
                    for(k = 0; k < size ; k++)
                        if (k == local_procs[j])
                            break;
                    obj_to_rank_in_comm[i] = k;
                }
            }

            /* the global master gathers info from local_masters */
            if ( 0 == rank ) {
                if ( num_nodes > 1 ) {
                    int *objs_per_node = NULL ;
                    int *displs = NULL;

                    objs_per_node = (int *)calloc(num_nodes,sizeof(int));
                    reqs = (MPI_Request *)calloc(num_nodes-1,sizeof(MPI_Request));
                    objs_per_node[0] = num_objs_in_node;
                    for(i = 1; i < num_nodes ; i++)
                        if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(objs_per_node + i, 1, MPI_INT,
                                                                       nodes_roots[i],111,comm_old,&reqs[i-1]))))
                            ERR_EXIT(err);

                    if (OMPI_SUCCESS != ( err = ompi_request_wait_all(num_nodes - 1,
                                                                      reqs,MPI_STATUSES_IGNORE)))
                        ERR_EXIT(err);

                    for(i = 0; i < num_nodes; i++)
                        num_objs_total += objs_per_node[i];
                    obj_mapping = (int *)calloc(num_objs_total,sizeof(int));
                    displs = (int *)calloc(num_objs_total,sizeof(int));
                    displs[0] = 0;
                    for(i = 1; i < num_nodes ; i++)
                        displs[i] = displs[i-1] + objs_per_node[i];

                    memset(reqs,0,(num_nodes-1)*sizeof(MPI_Request));
                    memcpy(obj_mapping,obj_to_rank_in_comm,objs_per_node[0]*sizeof(int));
                    for(i = 1; i < num_nodes ; i++)
                        if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(obj_mapping + displs[i], objs_per_node[i], MPI_INT,
                                                                       nodes_roots[i],111,comm_old,&reqs[i-1]))))
                            ERR_EXIT(err);
                    if (OMPI_SUCCESS != ( err = ompi_request_wait_all(num_nodes - 1,
                                                                      reqs,MPI_STATUSES_IGNORE)))
                        ERR_EXIT(err);
                    free(displs);
                    free(objs_per_node);
                } else {
                    /* if num_nodes == 1, then it's easy to get the obj mapping */
                    num_objs_total = num_objs_in_node;
                    obj_mapping = (int *)calloc(num_objs_total,sizeof(int));
                    memcpy(obj_mapping,obj_to_rank_in_comm,num_objs_total*sizeof(int));
                }

#ifdef __DEBUG__
                fprintf(stdout,"Obj mapping : ");
                for(i = 0 ; i < num_objs_total ; i++)
                    fprintf(stdout," [%i:%i] ",i,obj_mapping[i]);
                fprintf(stdout,"\n");
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

            for(i = 0 ; i < (MAX_LEVELS+1) ; i++)
                hierarchy[i] = -1;
            hierarchy[0] = numlevels;

            assert(numlevels < MAX_LEVELS);

            for(i = 0 ; i < hierarchy[0] ; i++)
                hierarchy[i+1] = tracker[i]->arity;

            if( 0 == rank ) {
                hierarchies = (int *)malloc(num_nodes*(MAX_LEVELS+1)*sizeof(int));
                for(i = 0 ; i < num_nodes*(MAX_LEVELS+1) ; i++)
                    hierarchies[i] = -1;
            }

            /* gather hierarchies iff more than 1 node! */
            if ( num_nodes > 1 ) {
                if(rank != 0) {
                    if (OMPI_SUCCESS != (err = MCA_PML_CALL(send(hierarchy,(MAX_LEVELS+1), MPI_INT, 0,
                                                                 111, MCA_PML_BASE_SEND_STANDARD, comm_old))))
                        ERR_EXIT(err);
                } else {
                    memset(reqs,0,(num_nodes-1)*sizeof(MPI_Request));
                    for(i = 1; i < num_nodes ; i++)
                        if (OMPI_SUCCESS != ( err = MCA_PML_CALL(irecv(hierarchies+i*(MAX_LEVELS+1),(MAX_LEVELS+1),MPI_INT,
                                                                       nodes_roots[i],111,comm_old,&reqs[i-1])))){
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
                tree_t *comm_tree = NULL;
                double **comm_pattern = NULL;
                int *matching = NULL;

                memcpy(hierarchies,hierarchy,(MAX_LEVELS+1)*sizeof(int));
#ifdef __DEBUG__
                fprintf(stdout,"hierarchies : ");
                for(i = 0 ; i < num_nodes*(MAX_LEVELS+1) ; i++)
                    fprintf(stdout," [%i] ",hierarchies[i]);
                fprintf(stdout,"\n");
#endif
                tm_topology = (tm_topology_t *)malloc(sizeof(tm_topology_t));
                tm_topology->nb_levels = hierarchies[0];

                /* extract min depth */
                for(i = 1 ; i < num_nodes ; i++)
                    if (hierarchies[i*(MAX_LEVELS+1)] < tm_topology->nb_levels)
                        tm_topology->nb_levels = hierarchies[i*(MAX_LEVELS+1)];
                /* Crush levels in hierarchies too long (ie > tm_topology->nb_levels)*/
                for(i = 0; i < num_nodes ; i++) {
                    int *base_ptr = hierarchies + i*(MAX_LEVELS+1) ;
                    int  suppl = *base_ptr - tm_topology->nb_levels;
                    for(j = 1 ; j <= suppl ; j++)
                        *(base_ptr + tm_topology->nb_levels) *= *(base_ptr + tm_topology->nb_levels + j);
                }
                if( num_nodes > 1){
                    /* We aggregate all topos => +1 level!*/
                    tm_topology->nb_levels += 1;
                    tm_topology->arity = (int *)calloc(tm_topology->nb_levels,sizeof(int));
                    tm_topology->arity[0] = num_nodes;
                    for(i = 0; i < (tm_topology->nb_levels - 1); i++) {
                        min = *(hierarchies + 1 + i);
                        for(j = 1; j < num_nodes ; j++)
                            if( hierarchies[j*(MAX_LEVELS+1) + 1 + i] < min)
                                min = hierarchies[j*(MAX_LEVELS+1) + 1 + i];
                        tm_topology->arity[i+1] = min;
                    }
                }else{
                    tm_topology->arity = (int *)calloc(tm_topology->nb_levels,sizeof(int));
                    for(i = 0; i < tm_topology->nb_levels; i++)
                        tm_topology->arity[i] = hierarchies[i+1];  /* fixme !!!*/
                }
                free(hierarchies);
#ifdef __DEBUG__
                for(i = 0; i < tm_topology->nb_levels; i++)
                    fprintf(stdout,"topo_arity[%i] = %i\n",i,tm_topology->arity[i]);
#endif
                /* compute the number of processing elements */
                tm_topology->nb_nodes = (int *)calloc(tm_topology->nb_levels,sizeof(int));
                tm_topology->nb_nodes[0] = 1;
                for(i = 1 ; i < tm_topology->nb_levels; i++)
                    tm_topology->nb_nodes[i] = tm_topology->nb_nodes[i-1]*tm_topology->arity[i-1];

                /* Build process id tab */
                tm_topology->node_id  = (int **)calloc(tm_topology->nb_levels,sizeof(int*));
                for(i = 0; i < tm_topology->nb_levels ; i++) {
                    tm_topology->node_id[i] = (int *)calloc(tm_topology->nb_nodes[i],sizeof(int));
                    for (j = 0; j < tm_topology->nb_nodes[i] ; j++)
                        tm_topology->node_id[i][j] = obj_mapping[j];
                }
#ifdef __DEBUG__
                for(i = 0; i < tm_topology->nb_levels ; i++) {
                    fprintf(stdout,"tm topo node_id for level [%i] : ",i);
                    for(j = 0 ; j < tm_topology->nb_nodes[i] ; j++)
                        fprintf(stdout," [%i:%i] ",j,obj_mapping[j]);
                    fprintf(stdout,"\n");
                }
                display_topology(tm_topology);
#endif

                comm_pattern = (double **)malloc(size*sizeof(double *));
                for(i = 0 ; i < size ; i++)
                    comm_pattern[i] = local_pattern + i*size;
                /* matrix needs to be symmetric */
                for( i = 0 ; i < size ; i++)
                    for(j = i ; j < size ; j++) {
                        comm_pattern[i][j] += comm_pattern[j][i];
                        comm_pattern[j][i]  = comm_pattern[i][j];
                    }
                for( i = 0 ; i < size ; i++)
                    for(j = 0 ; j < size ; j++)
                        comm_pattern[i][j] /= 2;
#ifdef __DEBUG__
                fprintf(stdout,"==== COMM PATTERN ====\n");
                for( i = 0 ; i < size ; i++){
                    for(j = 0 ; j < size ; j++)
                        fprintf(stdout," %f ",comm_pattern[i][j]);
                    fprintf(stdout,"\n");
                }
#endif
                k = (int *)calloc(num_objs_total,sizeof(int));
                matching = (int *)calloc(size,sizeof(int));

                tm_opt_topology = optimize_topology(tm_topology);
                comm_tree = build_tree_from_topology(tm_opt_topology,comm_pattern,size,NULL,NULL);
                map_topology_simple(tm_opt_topology,comm_tree,matching,size,k);
#ifdef __DEBUG__

                fprintf(stdout,"====> nb levels : %i\n",tm_topology->nb_levels);
                fprintf(stdout,"Rank permutation sigma/k : ");
                for(i = 0 ; i < num_objs_total ; i++)
                    fprintf(stdout," [%i:%i] ",i,k[i]);
                fprintf(stdout,"\n");

                fprintf(stdout,"Matching : ");
                for(i = 0 ; i < size ; i++)
                    fprintf(stdout," [%i:%i] ",i,matching[i]);
                fprintf(stdout,"\n");
#endif
                free(comm_pattern);
                free(comm_tree);
                free(matching);
                free(obj_mapping);
                for(i = 0 ; i < tm_topology->nb_levels ; i++)
                    free(tm_topology->node_id[i]);
                free(tm_topology->node_id);
                free(tm_topology->nb_nodes);
                free(tm_topology->arity);
                free(tm_topology);
                FREE_topology(tm_opt_topology);
            }
        }

        /* Todo : Bcast + group creation */
        /* scatter the ranks */
        if (OMPI_SUCCESS != (err = comm_old->c_coll.coll_scatter(k, 1, MPI_INT,
                                                                 &newrank, 1, MPI_INT,
                                                                 0, comm_old,comm_old->c_coll.coll_scatter_module)))
            ERR_EXIT(err);

        if ( 0 == rank )
            free(k);

        /* this needs to be optimized but will do for now */
        if (OMPI_SUCCESS != (err = ompi_comm_split(comm_old, 0, newrank,newcomm, false)))
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
        hwloc_obj_t object;
        opal_hwloc_locality_t locality;
        char set_as_string[64];
        opal_value_t kv;

        if (OMPI_SUCCESS != (err = ompi_comm_split(comm_old,colors[rank],ompi_process_info.my_local_rank,&localcomm, false)))
            return err;

        for(i = 0 ; i < num_procs_in_node ; i++)
            lrank_to_grank[i] = -1;
        lrank_to_grank[ompi_process_info.my_local_rank] = rank;

        for(i = 0 ; i < size ; i++)
            grank_to_lrank[i] = -1;

        if (OMPI_SUCCESS != (err = localcomm->c_coll.coll_allgather(&rank,1,MPI_INT,
                                                                    lrank_to_grank,1,MPI_INT,
                                                                    localcomm,
                                                                    localcomm->c_coll.coll_allgather_module)))
            return err;

        for(i = 0 ; i < num_procs_in_node ; i++)
            grank_to_lrank[lrank_to_grank[i]] = i;

        if (rank == local_procs[0]){
            tm_topology_t  *tm_topology = NULL;
            tm_topology_t  *tm_opt_topology = NULL;
            tree_t *comm_tree = NULL;
            double **comm_pattern = NULL;

#ifdef __DEBUG__
            fprintf(stderr,"========== Partially Distributed Reordering ========= \n");
#endif

            local_pattern = (double *)calloc(num_procs_in_node*num_procs_in_node,sizeof(double));
            for(i = 0 ; i < num_procs_in_node*num_procs_in_node ; i++)
                local_pattern[i] = 0.0;

            if( true == topo->weighted ) {
                for(i = 0; i < topo->indegree ; i++)
                    if (grank_to_lrank[topo->in[i]] != -1)
                        local_pattern[grank_to_lrank[topo->in[i]]] += topo->inw[i];
                for(i = 0; i < topo->outdegree ; i++)
                    if (grank_to_lrank[topo->out[i]] != -1)
                        local_pattern[grank_to_lrank[topo->out[i]]] += topo->outw[i];
                if (OMPI_SUCCESS != (err = localcomm->c_coll.coll_gather(MPI_IN_PLACE, num_procs_in_node, MPI_DOUBLE,
                                                                         local_pattern, num_procs_in_node, MPI_DOUBLE,
                                                                         0,localcomm,
                                                                         localcomm->c_coll.coll_gather_module)))
                    ERR_EXIT(err);
            }

            comm_pattern = (double **)malloc(num_procs_in_node*sizeof(double *));
            for(i = 0 ; i < num_procs_in_node ; i++){
                comm_pattern[i] = (double *)calloc(num_procs_in_node,sizeof(double));
                memcpy((void *)comm_pattern[i],(void *)(local_pattern + i*num_procs_in_node),num_procs_in_node*sizeof(double));
            }
            /* Matrix needs to be symmetric */
            for( i = 0 ; i < num_procs_in_node ; i++)
                for(j = i ; j < num_procs_in_node ; j++){
                    comm_pattern[i][j] += comm_pattern[j][i];
                    comm_pattern[j][i]  = comm_pattern[i][j];
                }
            for( i = 0 ; i < num_procs_in_node ; i++)
                for(j = 0 ; j < num_procs_in_node ; j++)
                    comm_pattern[i][j] /= 2;

#ifdef __DEBUG__
            fprintf(stdout,"========== COMM PATTERN ============= \n");
            for(i = 0 ; i < num_procs_in_node ; i++){
                fprintf(stdout," %i : ",i);
                for(j = 0; j < num_procs_in_node ; j++)
                    fprintf(stdout,"  %f  ",comm_pattern[i][j]);
                fprintf(stdout,"\n");
            }
            fprintf(stdout,"======================= \n");
#endif

            tm_topology  = (tm_topology_t *)malloc(sizeof(tm_topology_t));
            tm_topology->nb_levels = numlevels;
            tm_topology->arity     = (int *)calloc(tm_topology->nb_levels,sizeof(int));
            tm_topology->nb_nodes  = (int *)calloc(tm_topology->nb_levels,sizeof(int));
            tm_topology->node_id   = (int **)malloc(tm_topology->nb_levels*sizeof(int *));
            for(i = 0 ; i < tm_topology->nb_levels ; i++){
                int nb_objs = hwloc_get_nbobjs_by_depth(opal_hwloc_topology,tracker[i]->depth);
                tm_topology->nb_nodes[i] = nb_objs;
                tm_topology->node_id[i]  = (int*)malloc(sizeof(int)*nb_objs);
                tm_topology->arity[i]    = tracker[i]->arity;
                for(j = 0 ; j < nb_objs ; j++)
                    tm_topology->node_id[i][j] = -1;
                for(j = 0 ; j < nb_objs ; j++)
                    if ( j < num_procs_in_node )
                        tm_topology->node_id[i][j] = localrank_to_objnum[j];
            }

#ifdef __DEBUG__
            fprintf(stdout,"Levels in topo : %i | num procs in node : %i\n",tm_topology->nb_levels,num_procs_in_node);
            for(i = 0; i < tm_topology->nb_levels ; i++){
                fprintf(stdout,"Nb objs for level %i : %i | arity %i\n ",i,tm_topology->nb_nodes[i],tm_topology->arity[i]);
                for(j = 0; j < tm_topology->nb_nodes[i] ; j++)
                    fprintf(stdout,"Obj id : %i |",tm_topology->node_id[i][j]);
                fprintf(stdout,"\n");
            }
            display_topology(tm_topology);
#endif

            tm_opt_topology = optimize_topology(tm_topology);
            comm_tree = build_tree_from_topology(tm_opt_topology,comm_pattern,num_procs_in_node,NULL,NULL);
            map_topology_simple(tm_opt_topology,comm_tree,matching,num_procs_in_node,NULL);

#ifdef __DEBUG__

            fprintf(stdout,"Matching :");
            for(i = 0 ; i < num_procs_in_node ; i++)
                fprintf(stdout," %i ",matching[i]);
            fprintf(stdout,"\n");
#endif
            for(i = 0 ; i < num_procs_in_node ; i++)
                free(comm_pattern[i]);
            free(comm_pattern);
            for(i = 0; i < tm_topology->nb_levels ; i++)
                free(tm_topology->node_id[i]);
            free(tm_topology->node_id);
            free(tm_topology->nb_nodes);
            free(tm_topology->arity);
            free(tm_topology);
            FREE_topology(tm_opt_topology);
        } else {
            local_pattern = (double *)calloc(num_procs_in_node,sizeof(double));
            for(i = 0 ; i < num_procs_in_node ; i++)
                local_pattern[i] = 0.0;

            if( true == topo->weighted ) {
                for(i = 0; i < topo->indegree ; i++)
                    if (grank_to_lrank[topo->in[i]] != -1)
                        local_pattern[grank_to_lrank[topo->in[i]]] += topo->inw[i];
                for(i = 0; i < topo->outdegree ; i++)
                    if (grank_to_lrank[topo->out[i]] != -1)
                        local_pattern[grank_to_lrank[topo->out[i]]] += topo->outw[i];
                if (OMPI_SUCCESS != (err = localcomm->c_coll.coll_gather(local_pattern, num_procs_in_node, MPI_DOUBLE,
                                                                         NULL,0,0,
                                                                         0,localcomm,
                                                                         localcomm->c_coll.coll_gather_module)))
                    ERR_EXIT(err);
            }
        }

        if (OMPI_SUCCESS != (err = localcomm->c_coll.coll_bcast(matching, num_procs_in_node,
                                                                MPI_INT,0,localcomm,
                                                                localcomm->c_coll.coll_bcast_module)))
            ERR_EXIT(err);

        object = hwloc_get_obj_by_depth(opal_hwloc_topology,
                                        effective_depth,matching[ompi_process_info.my_local_rank]);
        if( NULL == object) goto fallback;
        hwloc_bitmap_copy(set,object->cpuset);
        hwloc_bitmap_singlify(set);
        hwloc_err = hwloc_set_cpubind(opal_hwloc_topology,set,0);
        if( -1 == hwloc_err) goto fallback;

        /* Report new binding to ORTE/OPAL */
        /*	hwloc_bitmap_list_asprintf(&orte_process_info.cpuset,set);   */
        err = hwloc_bitmap_snprintf (set_as_string,64,set);

#ifdef __DEBUG__
        fprintf(stdout,"Bitmap str size : %i\n",err);
#endif

        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(OPAL_PMIX_CPUSET);
        kv.type = OPAL_STRING;
        kv.data.string = strdup(set_as_string);

        (void)opal_pmix.store_local((opal_process_name_t*)ORTE_PROC_MY_NAME, &kv);
        OBJ_DESTRUCT(&kv);

        locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                         orte_process_info.cpuset,set_as_string);
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup(OPAL_PMIX_LOCALITY);
        kv.type = OPAL_UINT16;
        kv.data.uint16 = locality;
        (void)opal_pmix.store_local((opal_process_name_t*)ORTE_PROC_MY_NAME, &kv);
        OBJ_DESTRUCT(&kv);

        if( OMPI_SUCCESS != (err = ompi_comm_create(comm_old,
                                                    comm_old->c_local_group,
                                                    newcomm))) {
            ERR_EXIT(err);
        } else {
            /* Attach the dist_graph to the newly created communicator */
            (*newcomm)->c_flags        |= OMPI_COMM_DIST_GRAPH;
            (*newcomm)->c_topo          = topo_module;
            (*newcomm)->c_topo->reorder = reorder;
        }
        free(matching);
        free(grank_to_lrank);
        free(lrank_to_grank);
    } /* distributed reordering end */

    if(rank == local_procs[0])
        free(tracker);
    free(nodes_roots);
    free(local_procs);
    free(local_pattern);
    free(localrank_to_objnum);
    free(colors);
    hwloc_bitmap_free(set);
    return err;
}
