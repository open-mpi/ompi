/*
 * $HEADER
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "util/common_cmd_line.h"


/*
 * The global variable that everyone accesses
 */
ompi_cmd_line_t *ompi_common_cmd_line = NULL;


/*
 * Setup the ompi_common_cmd_line variable
 */
int ompi_common_cmd_line_init(int argc, char **argv)
{
    if (NULL == ompi_common_cmd_line) {
        ompi_common_cmd_line = OBJ_NEW(ompi_cmd_line_t);

        /* Setup the options that are allowable */
        
        ompi_cmd_line_make_opt(ompi_common_cmd_line, 'v', "version", 0,
                               "Show version of Open MPI and this program");

        ompi_cmd_line_make_opt(ompi_common_cmd_line, 'h', "help", 0,
                               "Show help for this function");

        /* Parse the command line */

        if (OMPI_SUCCESS != 
            ompi_cmd_line_parse(ompi_common_cmd_line, true, argc, argv)) {
            return OMPI_ERROR;
        }
    }

    return OMPI_SUCCESS;
}


/*
 * Free resources associated with the ompi_common_cmd_line variable
 */
int ompi_common_cmd_line_finalize(void)
{
    if (NULL != ompi_common_cmd_line) {
        OBJ_RELEASE(ompi_common_cmd_line);
    }

    return OMPI_SUCCESS;
}
