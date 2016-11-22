#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/util/output.h"
#include "opal/class/opal_graph.h"
#include "opal/mca/paffinity/base/base.h"
#include "ompi/constants.h"

#include "orte/mca/ess/ess.h"
#include "coll_ml_resource_affinity.h"

int get_dev_distance_for_all_procs(opal_carto_graph_t *graph, const char *device)
{
    opal_paffinity_base_cpu_set_t cpus;
    opal_carto_base_node_t *device_node;
    int min_distance = -1, i, num_processors;

    if(opal_paffinity_base_get_processor_info(&num_processors) != OMPI_SUCCESS) {
        num_processors = 100; /* Choose something big enough */
    }

    device_node = opal_carto_base_find_node(graph, device);

    /* no topology info for device found. Assume that it is close */
    if(NULL == device_node)
        return 0;

    OPAL_PAFFINITY_CPU_ZERO(cpus);
    opal_paffinity_base_get(&cpus);

    for (i = 0; i < num_processors; i++) {
        opal_carto_base_node_t *slot_node;
        int distance, socket, core;
        char *slot;

        if(!OPAL_PAFFINITY_CPU_ISSET(i, cpus))
            continue;

        opal_paffinity_base_get_map_to_socket_core(i, &socket, &core);
        asprintf(&slot, "socket%d", socket);

        slot_node = opal_carto_base_find_node(graph, slot);

        free(slot);

        if(NULL == slot_node)
            return 0;

        distance = opal_carto_base_spf(graph, slot_node, device_node);

        if(distance < 0)
            return 0;

        if(min_distance < 0 || min_distance > distance)
            min_distance = distance;
    }

    return min_distance;
}

int get_dev_distance_proc(opal_carto_graph_t *graph,
				const char *device,int rank, struct ompi_proc_t *proc){
    opal_paffinity_base_cpu_set_t cpus;
    opal_carto_base_node_t *device_node;
	opal_carto_base_node_t *slot_node;
    int distance, socket, core;
    char *slot;
	int process_id;
	int nrank;

	nrank = orte_ess.get_node_rank(&(proc->proc_name));

	opal_paffinity_base_get_physical_processor_id(nrank, &process_id);

	device_node = opal_carto_base_find_node(graph, device);

    /* no topology info for device found. Assume that it is close */
    if(NULL == device_node)
        return 0;

    OPAL_PAFFINITY_CPU_ZERO(cpus);
    opal_paffinity_base_get(&cpus);



    opal_paffinity_base_get_map_to_socket_core(process_id, &socket, &core);
    asprintf(&slot, "socket%d", socket);
	ML_VERBOSE(10,("The socket addres is %d",socket));

    slot_node = opal_carto_base_find_node(graph, slot);

    free(slot);

    if(NULL == slot_node)
            return -1;

    distance = opal_carto_base_spf(graph, slot_node, device_node);

    if(distance < 0)
	return -1;

    return distance;

}

int coll_ml_select_leader(mca_coll_ml_module_t *ml_module,
						  mca_sbgp_base_module_t *sbgp_module,
							int *rank_in_comm,
							struct ompi_proc_t ** procs,
							int nprocs){

	int rank, dist1, dist2,dist;
	int min_dist = 10000;
	int i,leader = 10000;
	struct ompi_proc_t *proc = NULL;

	for (i=0; i<nprocs; i++) {

		/* if local process */
		rank = rank_in_comm[sbgp_module->group_list[i]];
		proc = procs[sbgp_module->group_list[i]];
		dist1 = get_dev_distance_proc(ml_module->sm_graph,"mem0",rank,proc);
		dist2 = get_dev_distance_proc(ml_module->ib_graph,"mthca0",rank,proc);

		dist = dist1 + dist2;

		ML_VERBOSE(10,("The distance for proc %d dist1 %d, dist2 %d",i,dist1,dist2));
		if ((dist < min_dist) || ((dist == min_dist) && (i < leader))) {
			leader = i;
			min_dist = dist;
		}
	}

	return leader;
}


int coll_ml_construct_resource_graphs(mca_coll_ml_module_t *ml_module){

	opal_carto_base_get_host_graph(&ml_module->sm_graph,"Memory");
	opal_carto_base_get_host_graph(&ml_module->ib_graph,"Infiniband");

	/* debug
	opal_graph_print(ml_module->sm_graph);
	*/
	return 0;

}
