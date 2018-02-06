/* This example program shows how to retrieve Knights Landing
 * memory and cluster modes.
 * See "Custom string infos" in the documentation for details
 * about these attributes.
 *
 * Copyright © 2015-2016, 2015 Intel
 * Copyright © 2016 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <stdio.h>
#include <stdlib.h>

#include <hwloc.h>

int main(void)
{
	hwloc_topology_t topology;
	hwloc_obj_t root;
	const char *cluster_mode;
	const char *memory_mode;

	hwloc_topology_init(&topology);
	hwloc_topology_load(topology);

	root = hwloc_get_root_obj(topology);

	cluster_mode = hwloc_obj_get_info_by_name(root, "ClusterMode");
	memory_mode = hwloc_obj_get_info_by_name(root, "MemoryMode");

	printf ("ClusterMode is '%s' MemoryMode is '%s'\n",
		cluster_mode ? cluster_mode : "NULL",
		memory_mode ? memory_mode : "NULL");

	hwloc_topology_destroy(topology);
	return 0;
}
