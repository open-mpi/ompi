/*
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "mca/gpr/base/base.h"

void mca_gpr_base_print_dump(ompi_buffer_t buffer, int output_id)
{
    char *line;

    while (0 < ompi_unpack_string(buffer, &line)) {
	ompi_output(output_id, "%s", line);
	free(line);
    }
}
