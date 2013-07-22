/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "opal/threads/tsd.h"
#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"

#include "rte_pmi.h"
#include "rte_pmi_internal.h"


int
ompi_rte_send_buffer(const ompi_process_name_t *peer,
                     struct opal_buffer_t *buffer,
                     ompi_rml_tag_t tag,
                     int flags)
{
    return OMPI_ERR_NOT_SUPPORTED;
}


int
ompi_rte_send_buffer_nb(const ompi_process_name_t *peer,
                        struct opal_buffer_t *buffer,
                        ompi_rml_tag_t tag,
                        int flags,
                        void (*cbfunc)(int, ompi_process_name_t*,
                                       opal_buffer_t*, ompi_rml_tag_t,
                                       void*),
                        void *cbdata)
{
    return OMPI_ERR_NOT_SUPPORTED;
}


int
ompi_rte_recv_buffer(const ompi_process_name_t *peer,
                     struct opal_buffer_t *buf,
                     ompi_rml_tag_t tag,
                     int flags)
{
    return OMPI_ERR_NOT_SUPPORTED;
}


int
ompi_rte_recv_buffer_nb(const ompi_process_name_t *peer,
                        ompi_rml_tag_t tag,
                        int flags,
                        void (*cbfunc)(int, ompi_process_name_t*,
                                       opal_buffer_t*, ompi_rml_tag_t,
                                       void*),
                        void *cbdata)
{
    return OMPI_ERR_NOT_SUPPORTED;
}


int
ompi_rte_recv_cancel(const ompi_process_name_t *peer,
                     ompi_rml_tag_t tag)
{
    return OMPI_ERR_NOT_SUPPORTED;
}


int
ompi_rte_parse_uris(const char* contact_info,
                    ompi_process_name_t *peer,
                    char ***uris)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

