/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orca_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orca/constants.h"
#include "orca/mca/stems/stems.h"
#include "orca/mca/stems/base/base.h"
#include "opal/util/output.h"

#include "orca/mca/stems/base/static-components.h"

/*
 * Globals
 */
int  orca_stems_base_output  = -1;
orca_stems_base_module_t orca_stems = {
    NULL, /* stems_init               */
    NULL, /* stems_finalize           */

    NULL, /* name_print */
    NULL, /* name_to_string */
    NULL, /* name_from_string */
    NULL, /* name_hash */
    NULL, /* name_compare */

    NULL, /* info_proc_is_bound */
    NULL, /* info_proc_get_applied_binding */

    NULL, /* info_create_session_dirs */
    NULL, /* info_in_parallel_debugger */
    NULL, /* info_standalone_operation */
    NULL, /* info_cr_continue_like_restart */
    NULL, /* info_job_identifier */

    NULL, /* oob_send_buffer */
    NULL, /* oob_send_buffer_nb */
    NULL, /* oob_recv_buffer */
    NULL, /* oob_recv_buffer_nb */
    NULL, /* oob_recv_cancel */
    NULL, /* oob_parse_uris */

    NULL, /* proc_get_locality */
    NULL, /* proc_get_hostname */
    NULL, /* proc_get_node_rank */

    NULL, /* error_log */
    NULL, /* error_set_callback */
    NULL, /* error_abort */
    NULL, /* error_abort_peers */

    NULL, /* coll_modex */
    NULL, /* coll_barrier */
    NULL, /* coll_set_attr */
    NULL, /* coll_get_attr */

    orca_stems_base_notifier_show_help, /* Need to define here for ompi_info - notifier_show_help */
    NULL, /* notifier_show_help_is_available */
    NULL  /* notifier_show_help_want_aggregate */
};
opal_list_t orca_stems_base_components_available;
orca_stems_base_component_t orca_stems_base_selected_component;

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
int orca_stems_base_open(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    int value;

    /* Debugging/Verbose output */
    mca_base_param_reg_int_name("stems",
                                "base_verbose",
                                "Verbosity level of the STEMS framework",
                                false, false,
                                0, &value);
    if(0 != value) {
        orca_stems_base_output = opal_output_open(NULL);
    } else {
        orca_stems_base_output = -1;
    }
    opal_output_set_verbosity(orca_stems_base_output, value);

    /* Open up all available components */
    if (OPAL_SUCCESS != (ret = mca_base_components_open("stems", 
                                                        orca_stems_base_output, 
                                                        mca_stems_base_static_components,
                                                        &orca_stems_base_components_available,
                                                        true)) ) {
        exit_status = OPAL_ERROR;
    }

    return exit_status;
}
