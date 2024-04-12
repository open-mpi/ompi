#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <hwloc.h>
#include <hwloc/bitmap.h>

int main(int argc, char **argv)
{
    hwloc_topology_t topo;
    char *tmp;
    int ret;

    if (2 != argc) {
        fprintf(stderr, "Usage: %s input-xml-filename\n", argv[0]);
        return -1;
    }

    if (0 != hwloc_topology_init(&topo)) {
        return -1;
    }
    if (0 != hwloc_topology_set_xml(topo, argv[1])) {
        fprintf(stderr, "SETXML FAILED\n");
        return -1;
    }
    /* since we are loading this from an external source, we have to
     * explicitly set a flag so hwloc sets things up correctly
     */
    ret = hwloc_topology_set_io_types_filter(topo, HWLOC_TYPE_FILTER_KEEP_IMPORTANT);
    if (0 != ret) {
        fprintf(stderr, "IOTYPES FAILED\n");
        hwloc_topology_destroy(topo);
        return ret;
    }

    if (0 != hwloc_topology_set_flags(topo, HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM)) {
        fprintf(stderr, "SETFLAGS FAILED\n");
        hwloc_topology_destroy(topo);
        return -1;
    }
    /* now load the topology */
    if (0 != hwloc_topology_load(topo)) {
        fprintf(stderr, "LOAD FAILED\n");
        hwloc_topology_destroy(topo);
        return -1;
    }

    /* output it in v1 format */
    asprintf(&tmp, "%s.v1", argv[1]);
    if (0 != hwloc_topology_export_xml(topo, tmp, HWLOC_TOPOLOGY_EXPORT_XML_FLAG_V1)) {
        fprintf(stderr, "EXPORT FAILED\n");
        hwloc_topology_destroy(topo);
        return -1;
    }
    hwloc_topology_destroy(topo);
    return 0;
}
