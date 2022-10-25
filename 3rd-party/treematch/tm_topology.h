#include <hwloc.h>
#include "tm_tree.h"

tm_topology_t* get_local_topo_with_hwloc(void);
OMPI_HIDDEN int tm_int_cmp_inc(const void* x1,const void* x2);
OMPI_HIDDEN void tm_display_arity(tm_topology_t *topology);
OMPI_HIDDEN void tm_display_topology(tm_topology_t *topology);
OMPI_HIDDEN void tm_free_topology(tm_topology_t *topology);
OMPI_HIDDEN tm_topology_t *tm_load_topology(char *arch_filename, tm_file_type_t arch_file_type);
OMPI_HIDDEN void tm_optimize_topology(tm_topology_t **topology);
OMPI_HIDDEN int  tm_topology_add_binding_constraints(char *constraints_filename, tm_topology_t *topology);
void topology_arity(tm_topology_t *topology,int **arity,int *nb_levels);
void topology_constraints(tm_topology_t *topology,int **constraints,int *nb_constraints);
void topology_cost(tm_topology_t *topology,double **cost);
void topology_numbering(tm_topology_t *topology,int **numbering,int *nb_nodes);
OMPI_HIDDEN int tm_nb_processing_units(tm_topology_t *topology);

