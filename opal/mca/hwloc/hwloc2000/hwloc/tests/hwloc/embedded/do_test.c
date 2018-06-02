#include <hwloc.h>
#include <stdio.h>

/* The body of the test is in a separate .c file and a separate
   library, just to ensure that hwloc didn't force compilation with
   visibility flags enabled. */

int do_test(void)
{
    mytest_hwloc_topology_t topology;
    unsigned depth;
    hwloc_bitmap_t cpu_set;

    /* Just call a bunch of functions to see if we can link and run */

    printf("*** Test 1: bitmap alloc\n");
    cpu_set = mytest_hwloc_bitmap_alloc();
    if (NULL == cpu_set) return 1;
    printf("*** Test 2: topology init\n");
    if (0 != mytest_hwloc_topology_init(&topology)) return 1;
    printf("*** Test 3: topology load\n");
    if (0 != mytest_hwloc_topology_load(topology)) return 1;
    printf("*** Test 4: topology get depth\n");
    depth = mytest_hwloc_topology_get_depth(topology);
    if (depth > 10000) return 1;
    printf("    Max depth: %u\n", depth);
    printf("*** Test 5: topology destroy\n");
    mytest_hwloc_topology_destroy(topology);
    printf("*** Test 6: bitmap free\n");
    mytest_hwloc_bitmap_free(cpu_set);

    return 0;
}
