#include <stdio.h>

#include "ns/name_server.h"

int main(int argc, char **argv)
{
    int i;
    uint32_t *foo;
    ompi_process_name_t j;

foo = (uint32_t*) &j;

    for (i=0; i<30; i++) {
	j = ompi_process_name_new();
	fprintf(stderr, "name: %x %x\n", foo[0], foo[1]);
    }

    for (i=0; i<3; i++) {
	j = ompi_process_name_get_range(30000);
	fprintf(stderr, "range name: %x %x\n", foo[0], foo[1]);
    }

}
