/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_INFO_MEMKIND_H
#define OMPI_INFO_MEMKIND_H

#include "ompi_config.h"
#include "opal/util/info_subscriber.h"

BEGIN_C_DECLS

#define OMPI_MAX_NUM_MEMKIND_RESTRICTORS 3
struct ompi_memkind_t {
    char *im_name;
    int   im_num_restrictors;
    char *im_restrictors[OMPI_MAX_NUM_MEMKIND_RESTRICTORS];
};
typedef struct ompi_memkind_t ompi_memkind_t;

/*
** Given a string of user requested memory alloc kinds, create
** a string with the actually support memory kinds by the library.
**
** @param[IN]: requested_str     input string
** @param[OUT]: provided_str     result string
**
** @return:                      OMPI_SUCCESS or error on failure
*/
OMPI_DECLSPEC int ompi_info_memkind_process (const char* requested_str,
                                             char **provided_str);
/*
** Set the memory_alloc_kind info object on the child object, either
** by copying it from the parent object, or adjusting it based
** on the assert_memory_alloc_kind info object provided by the code
** during object creation
**
** @param[IN]:     parent        parent object (e.g. comm->super, file->super, etc.)
** @param [INOUT]: child         child object
** @param[IN]:     info          info object provided by code during object creation
**                               (e.g. MPI_Comm_dup_with_info, MPI_File_open, etc.)
**
** @return:                      OMPI_SUCCESS or error on failure
*/
OMPI_DECLSPEC int ompi_info_memkind_copy_or_set (opal_infosubscriber_t *parent,
                                                 opal_infosubscriber_t *child,
                                                 opal_info_t *info);

/*
** free the array of available memkinds when shutting down the info
** infrastructure.
*/
OMPI_DECLSPEC void ompi_info_memkind_free_available (void);

/*
** Callback function used when registering memkind info object
*/
OMPI_DECLSPEC const char *ompi_info_memkind_cb (opal_infosubscriber_t *obj, const char *key, const char *value);

END_C_DECLS

#endif /* OMPI_INFO_MEMKIND_H */

