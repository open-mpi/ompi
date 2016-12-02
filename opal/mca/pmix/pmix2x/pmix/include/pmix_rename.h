/*
 * Copyright (c) 2016      Intel, Inc. All rights reserved
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_RENAME_H
#define PMIX_RENAME_H

#define PMIx_Init                      OPAL_MCA_PMIX3X_PMIx_Init
#define PMIx_Initialized               OPAL_MCA_PMIX3X_PMIx_Initialized
#define PMIx_Finalize                  OPAL_MCA_PMIX3X_PMIx_Finalize
#define PMIx_Abort                     OPAL_MCA_PMIX3X_PMIx_Abort
#define PMIx_Put                       OPAL_MCA_PMIX3X_PMIx_Put
#define PMIx_Commit                    OPAL_MCA_PMIX3X_PMIx_Commit
#define PMIx_Fence                     OPAL_MCA_PMIX3X_PMIx_Fence
#define PMIx_Fence_nb                  OPAL_MCA_PMIX3X_PMIx_Fence_nb
#define PMIx_Get                       OPAL_MCA_PMIX3X_PMIx_Get
#define PMIx_Get_nb                    OPAL_MCA_PMIX3X_PMIx_Get_nb
#define PMIx_Publish                   OPAL_MCA_PMIX3X_PMIx_Publish
#define PMIx_Publish_nb                OPAL_MCA_PMIX3X_PMIx_Publish_nb
#define PMIx_Lookup                    OPAL_MCA_PMIX3X_PMIx_Lookup
#define PMIx_Lookup_nb                 OPAL_MCA_PMIX3X_PMIx_Lookup_nb
#define PMIx_Unpublish                 OPAL_MCA_PMIX3X_PMIx_Unpublish
#define PMIx_Unpublish_nb              OPAL_MCA_PMIX3X_PMIx_Unpublish_nb
#define PMIx_Spawn                     OPAL_MCA_PMIX3X_PMIx_Spawn
#define PMIx_Spawn_nb                  OPAL_MCA_PMIX3X_PMIx_Spawn_nb
#define PMIx_Connect                   OPAL_MCA_PMIX3X_PMIx_Connect
#define PMIx_Connect_nb                OPAL_MCA_PMIX3X_PMIx_Connect_nb
#define PMIx_Disconnect                OPAL_MCA_PMIX3X_PMIx_Disconnect
#define PMIx_Disconnect_nb             OPAL_MCA_PMIX3X_PMIx_Disconnect_nb
#define PMIx_Resolve_peers             OPAL_MCA_PMIX3X_PMIx_Resolve_peers
#define PMIx_Resolve_nodes             OPAL_MCA_PMIX3X_PMIx_Resolve_nodes
#define PMIx_Query_info_nb             OPAL_MCA_PMIX3X_PMIx_Query_info_nb
#define PMIx_Log_nb                    OPAL_MCA_PMIX3X_PMIx_Log_nb

#define PMIx_server_init               OPAL_MCA_PMIX3X_PMIx_server_init
#define PMIx_server_finalize           OPAL_MCA_PMIX3X_PMIx_server_finalize
#define PMIx_generate_regex            OPAL_MCA_PMIX3X_PMIx_generate_regex
#define PMIx_generate_ppn              OPAL_MCA_PMIX3X_PMIx_generate_ppn
#define PMIx_server_register_nspace    OPAL_MCA_PMIX3X_PMIx_server_register_nspace
#define PMIx_server_deregister_nspace  OPAL_MCA_PMIX3X_PMIx_server_deregister_nspace
#define PMIx_server_register_client    OPAL_MCA_PMIX3X_PMIx_server_register_client
#define PMIx_server_deregister_client  OPAL_MCA_PMIX3X_PMIx_server_deregister_client
#define PMIx_server_setup_fork         OPAL_MCA_PMIX3X_PMIx_server_setup_fork
#define PMIx_server_dmodex_request     OPAL_MCA_PMIX3X_PMIx_server_dmodex_request

#define PMIx_tool_init                 OPAL_MCA_PMIX3X_PMIx_tool_init
#define PMIx_tool_finalize             OPAL_MCA_PMIX3X_PMIx_tool_finalize

#define PMIx_Register_event_handler    OPAL_MCA_PMIX3X_PMIx_Register_event_handler
#define PMIx_Deregister_event_handler  OPAL_MCA_PMIX3X_PMIx_Deregister_event_handler
#define PMIx_Notify_event              OPAL_MCA_PMIX3X_PMIx_Notify_event
#define PMIx_Error_string              OPAL_MCA_PMIX3X_PMIx_Error_string
#define PMIx_Proc_state_string         OPAL_MCA_PMIX3X_PMIx_Proc_state_string
#define PMIx_Persistence_string        OPAL_MCA_PMIX3X_PMIx_Persistence_string
#define PMIx_Data_range_string         OPAL_MCA_PMIX3X_PMIx_Data_range_string
#define PMIx_Info_directives_string    OPAL_MCA_PMIX3X_PMIx_Info_directives_string
#define PMIx_Data_type_string          OPAL_MCA_PMIX3X_PMIx_Data_type_string
#define PMIx_Get_version               OPAL_MCA_PMIX3X_PMIx_Get_version
#define PMIx_Store_internal            OPAL_MCA_PMIX3X_PMIx_Store_internal

#define pmix_value_load                OPAL_MCA_PMIX3X_pmix_value_load
#define pmix_value_xfer                OPAL_MCA_PMIX3X_pmix_value_xfer
#define pmix_globals                   OPAL_MCA_PMIX3X_pmix_globals
#define pmix_output                    OPAL_MCA_PMIX3X_pmix_output
#define pmix_output_verbose            OPAL_MCA_PMIX3X_pmix_output_verbose

#endif
