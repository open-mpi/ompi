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

int mca_gpr_base_pack_test_internals(ompi_buffer_t cmd, int level)
{
    mca_gpr_cmd_flag_t command;
    int32_t test_level;

    command = MCA_GPR_TEST_INTERNALS_CMD;
    test_level = (int32_t)level;

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &test_level, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
