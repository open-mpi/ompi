/*
 * Copyright (c) 2013-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer listed
 *   in this license in the documentation and/or other materials
 *   provided with the distribution.
 *
 * - Neither the name of the copyright holders nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * The copyright holders provide no reassurances that the source code
 * provided does not infringe any patent, copyright, or any other
 * intellectual property rights of third parties.  The copyright holders
 * disclaim any liability to any recipient for claims brought against
 * recipient by any third party for infringement of that parties
 * intellectual property rights.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $HEADER$
 *
 * PMIx interfaces for support of Workload Managers (Schedulers)
 */

#ifndef PMIx_SCHED_API_H
#define PMIx_SCHED_API_H

/* Structure and constant definitions */
#include <pmix_common.h>
#include <pmix_server.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/******     FABRIC-SCHEDULER INTERACTIONS     ******/

/* Define a pmix_fabric_t struct that host RMs can use to
 * interact with fabric-related interfaces */
typedef struct pmix_fabric_s {
    /* user-supplied name for this fabric */
    char *name;
    /* a PMIx-supplied index identifying this registration object */
    size_t index;
    /* communication cost array - the number of vertices
     * (nverts) equals the number of interfaces in the
     * fabric. This equates to the number of columns & rows
     * in the commcost array as the matrix is symmetric */
    uint16_t **commcost;
    uint32_t nverts;
    /* object pointer for use by the PMIx server library */
    void *module;
} pmix_fabric_t;

/* convenience macros to support pmix_fabric_t */
#define PMIX_FABRIC_CONSTRUCT(x) \
    memset(x, 0, sizeof(pmix_fabric_t))

/* Register for access to fabric-related information, including
 * communication cost matrix. This call must be made prior to
 * requesting information from a fabric.
 *
 * fabric - address of a pmix_fabric_t (backed by storage). User
 *          may populate the "name" field at will - PMIx does not
 *          utilize this field
 *
 * directives - an optional array of values indicating desired
 *              behaviors and/or fabric to be accessed. If NULL,
 *              then the highest priority available fabric will
 *              be used
 *
 * ndirs - number of elements in the directives array
 *
 * Return values include:
 *
 * PMIX_SUCCESS - indicates success
 */
PMIX_EXPORT pmix_status_t PMIx_server_register_fabric(pmix_fabric_t *fabric,
                                                      const pmix_info_t directives[],
                                                      size_t ndirs);

/* Deregister a fabric object, providing an opportunity for
 * the PMIx server library to cleanup any information
 * (e.g., cost matrix) associated with it
 *
 * fabric - pointer to the pmix_fabric_t struct provided
 *          to the registration function
 */
PMIX_EXPORT pmix_status_t PMIx_server_deregister_fabric(pmix_fabric_t *fabric);

/* Given a communication cost matrix index for a specified fabric,
 * return the corresponding vertex info and the name of the node upon
 * which it resides.
 *
 * fabric - pointer to the pmix_fabric_t struct provided to
 *          the registration function
 *
 * i - communication cost matrix index
 *
 * vertex - pointer to the pmix_value_t where the vertex info is to
 *          be returned (backed by storage)
 *
 * nodename - pointer to the location where the string nodename
 *            is to be returned. The caller is responsible for
 *            releasing the string when done
 *
 * Return values include:
 *
 * PMIX_SUCCESS - indicates return of a valid value
 * PMIX_ERR_BAD_PARAM - provided index is out of bounds
 */
PMIX_EXPORT pmix_status_t PMIx_server_get_vertex_info(pmix_fabric_t *fabric,
                                                      uint32_t i, pmix_value_t *vertex,
                                                      char **nodename);

/* Given vertex info and the name of the device upon which that
 * vertex resides, return the corresponding communication cost matrix
 * index
 *
 * fabric - pointer to the pmix_fabric_t struct provided to
 *          the registration function
 *
 * vertex - pointer to the vertex info whose index is being requested
 *
 * i - pointer to the location where the index is to be returned
 *
 * Return values include:
 *
 * PMIX_SUCCESS - indicates return of a valid value
 * PMIX_ERR_NOT_FOUND - provided vertex description is not found
 * PMIX_ERR_RESOURCE_BUSY - matrix is being updated
 * PMIX_ERR_FABRIC_UPDATED - fabric info has been updated since
 *                           last call involving this pmix_fabric_t
  */
PMIX_EXPORT pmix_status_t PMIx_server_get_index(pmix_fabric_t *fabric,
                                                pmix_value_t *vertex, uint32_t *i);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
