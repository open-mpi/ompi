/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
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
