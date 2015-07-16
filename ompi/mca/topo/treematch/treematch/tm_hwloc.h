#include <hwloc.h>
#include "tm_tree.h"

void hwloc_topology_tag(hwloc_topology_t topology);
tm_topology_t* hwloc_to_tm(char *filename,double **pcost);
tm_topology_t * tgt_to_tm(char *filename,double **pcost);
tm_topology_t* get_local_topo_with_hwloc(void);
