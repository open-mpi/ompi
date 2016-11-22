#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/util/output.h"
#include "opal/class/opal_graph.h"
#include "coll_ml.h"


/* Get the host graph for SM and Infiniband */
int discover_on_node_resources(const char device);
int get_dev_distance_for_all_procs(opal_carto_graph_t *graph,
							const char *device);
int get_dev_distance_proc(opal_carto_graph_t *graph,
				const char *device,int rank,struct ompi_proc_t *proc);
int coll_ml_select_leader(mca_coll_ml_module_t *ml_module,
						  mca_sbgp_base_module_t *sbgp_module,
							int *rank_in_comm,
							struct ompi_proc_t ** procs,
							int nprocs);
int coll_ml_construct_resource_graphs(mca_coll_ml_module_t *ml_module);
