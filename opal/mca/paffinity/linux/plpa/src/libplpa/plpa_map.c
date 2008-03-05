/*
 * Copyright (c) 2007 Cisco Systems, Inc.  All rights reserved.
 *
 * Portions of this file originally contributed by Advanced Micro
 * Devices, Inc.  See notice below.
 */
/* ============================================================
 License Agreement

 Copyright (c) 2006, 2007 Advanced Micro Devices, Inc.
 All rights reserved.

 Redistribution and use in any form of this material and any product 
 thereof including software in source or binary forms, along with any 
 related documentation, with or without modification ("this material"), 
 is permitted provided that the following conditions are met:

 + Redistributions of source code of any software must retain the above
 copyright notice and all terms of this license as part of the code.

 + Redistributions in binary form of any software must reproduce the
 above copyright notice and all terms of this license in any related 
 documentation and/or other materials.

 + Neither the names nor trademarks of Advanced Micro Devices, Inc. or
 any copyright holders or contributors may be used to endorse or 
 promote products derived from this material without specific prior 
 written permission.

 + Notice about U.S. Government restricted rights: This material is
 provided with "RESTRICTED RIGHTS." Use, duplication or disclosure by 
 the U.S. Government is subject to the full extent of restrictions set 
 forth in FAR52.227 and DFARS252.227 et seq., or any successor or 
 applicable regulations. Use of this material by the U.S. Government 
 constitutes acknowledgment of the proprietary rights of Advanced Micro 
 Devices, Inc.
 and any copyright holders and contributors.

 + In no event shall anyone redistributing or accessing or using this
 material commence or participate in any arbitration or legal action 
 relating to this material against Advanced Micro Devices, Inc. or any 
 copyright holders or contributors. The foregoing shall survive any 
 expiration or termination of this license or any agreement or access 
 or use related to this material.

 + ANY BREACH OF ANY TERM OF THIS LICENSE SHALL RESULT IN THE IMMEDIATE
 REVOCATION OF ALL RIGHTS TO REDISTRIBUTE, ACCESS OR USE THIS MATERIAL.

 THIS MATERIAL IS PROVIDED BY ADVANCED MICRO DEVICES, INC. AND ANY 
 COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" IN ITS CURRENT CONDITION 
 AND WITHOUT ANY REPRESENTATIONS, GUARANTEE, OR WARRANTY OF ANY KIND OR 
 IN ANY WAY RELATED TO SUPPORT, INDEMNITY, ERROR FREE OR UNINTERRUPTED 
 OPERATION, OR THAT IT IS FREE FROM DEFECTS OR VIRUSES.  ALL 
 OBLIGATIONS ARE HEREBY DISCLAIMED - WHETHER EXPRESS, IMPLIED, OR 
 STATUTORY - INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF 
 TITLE, MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, ACCURACY, 
 COMPLETENESS, OPERABILITY, QUALITY OF SERVICE, OR NON-INFRINGEMENT. IN 
 NO EVENT SHALL ADVANCED MICRO DEVICES, INC. OR ANY COPYRIGHT HOLDERS 
 OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 SPECIAL, PUNITIVE, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF 
 USE, REVENUE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
 CAUSED OR BASED ON ANY THEORY OF LIABILITY ARISING IN ANY WAY RELATED 
 TO THIS MATERIAL, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 THE ENTIRE AND AGGREGATE LIABILITY OF ADVANCED MICRO DEVICES, INC. AND 
 ANY COPYRIGHT HOLDERS AND CONTRIBUTORS SHALL NOT EXCEED TEN DOLLARS 
 (US $10.00). ANYONE REDISTRIBUTING OR ACCESSING OR USING THIS MATERIAL 
 ACCEPTS THIS ALLOCATION OF RISK AND AGREES TO RELEASE ADVANCED MICRO 
 DEVICES, INC. AND ANY COPYRIGHT HOLDERS AND CONTRIBUTORS FROM ANY AND 
 ALL LIABILITIES, OBLIGATIONS, CLAIMS, OR DEMANDS IN EXCESS OF TEN 
 DOLLARS (US $10.00). THE FOREGOING ARE ESSENTIAL TERMS OF THIS LICENSE 
 AND, IF ANY OF THESE TERMS ARE CONSTRUED AS UNENFORCEABLE, FAIL IN 
 ESSENTIAL PURPOSE, OR BECOME VOID OR DETRIMENTAL TO ADVANCED MICRO 
 DEVICES, INC. OR ANY COPYRIGHT HOLDERS OR CONTRIBUTORS FOR ANY REASON, 
 THEN ALL RIGHTS TO REDISTRIBUTE, ACCESS OR USE THIS MATERIAL SHALL 
 TERMINATE IMMEDIATELY. MOREOVER, THE FOREGOING SHALL SURVIVE ANY 
 EXPIRATION OR TERMINATION OF THIS LICENSE OR ANY AGREEMENT OR ACCESS 
 OR USE RELATED TO THIS MATERIAL.

 NOTICE IS HEREBY PROVIDED, AND BY REDISTRIBUTING OR ACCESSING OR USING 
 THIS MATERIAL SUCH NOTICE IS ACKNOWLEDGED, THAT THIS MATERIAL MAY BE 
 SUBJECT TO RESTRICTIONS UNDER THE LAWS AND REGULATIONS OF THE UNITED 
 STATES OR OTHER COUNTRIES, WHICH INCLUDE BUT ARE NOT LIMITED TO, U.S.
 EXPORT CONTROL LAWS SUCH AS THE EXPORT ADMINISTRATION REGULATIONS AND 
 NATIONAL SECURITY CONTROLS AS DEFINED THEREUNDER, AS WELL AS STATE 
 DEPARTMENT CONTROLS UNDER THE U.S. MUNITIONS LIST. THIS MATERIAL MAY 
 NOT BE USED, RELEASED, TRANSFERRED, IMPORTED, EXPORTED AND/OR RE- 
 EXPORTED IN ANY MANNER PROHIBITED UNDER ANY APPLICABLE LAWS, INCLUDING 
 U.S. EXPORT CONTROL LAWS REGARDING SPECIFICALLY DESIGNATED PERSONS, 
 COUNTRIES AND NATIONALS OF COUNTRIES SUBJECT TO NATIONAL SECURITY 
 CONTROLS.
 MOREOVER,
 THE FOREGOING SHALL SURVIVE ANY EXPIRATION OR TERMINATION OF ANY 
 LICENSE OR AGREEMENT OR ACCESS OR USE RELATED TO THIS MATERIAL.

 This license forms the entire agreement regarding the subject matter 
 hereof and supersedes all proposals and prior discussions and writings 
 between the parties with respect thereto. This license does not affect 
 any ownership, rights, title, or interest in, or relating to, this 
 material. No terms of this license can be modified or waived, and no 
 breach of this license can be excused, unless done so in a writing 
 signed by all affected parties. Each term of this license is 
 separately enforceable. If any term of this license is determined to 
 be or becomes unenforceable or illegal, such term shall be reformed to 
 the minimum extent necessary in order for this license to remain in 
 effect in accordance with its terms as modified by such reformation.
 This license shall be governed by and construed in accordance with the 
 laws of the State of Texas without regard to rules on conflicts of law 
 of any state or jurisdiction or the United Nations Convention on the 
 International Sale of Goods. All disputes arising out of this license 
 shall be subject to the jurisdiction of the federal and state courts 
 in Austin, Texas, and all defenses are hereby waived concerning 
 personal jurisdiction and venue of these courts.
 ============================================================ */

#include "plpa.h"
#include "plpa_internal.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <limits.h>
#include <errno.h>
#include <stdlib.h>

typedef struct tuple_t_ {
    int processor_id, socket, core;
} tuple_t;

static int supported = 0;
static int max_processor = -1;
static int max_socket = -1;
static int *max_core = NULL;
static int max_core_overall = -1;
static tuple_t *map_processor_id_to_tuple = NULL;
static tuple_t ***map_tuple_to_processor_id = NULL;

static void clear_cache(void)
{
    if (NULL != max_core) {
        free(max_core);
        max_core = NULL;
    }
    if (NULL != map_processor_id_to_tuple) {
        free(map_processor_id_to_tuple);
        map_processor_id_to_tuple = NULL;
    }
    if (NULL != map_tuple_to_processor_id) {
        if (NULL != map_tuple_to_processor_id[0]) {
            free(map_tuple_to_processor_id[0]);
            map_tuple_to_processor_id = NULL;
        }
        free(map_tuple_to_processor_id);
        map_tuple_to_processor_id = NULL;
    }

    max_processor = -1;
    max_socket = -1;
    max_core_overall = -1;
}

static void load_cache(const char *sysfs_mount)
{
    int i, j, k, invalid_entry, fd;
    char path[PATH_MAX], buf[8];

    /* Check for the parent directory */
    sprintf(path, "%s/devices/system/cpu", sysfs_mount);
    if (access(path, R_OK|X_OK)) {
        return;
    }

    /* Go through and find the max processor ID */
    for (max_processor = 0; max_processor < PLPA_BITMASK_CPU_MAX; 
         ++max_processor) {
        sprintf(path, "%s/devices/system/cpu/cpu%d", sysfs_mount, 
                max_processor);
        if ( access(path, R_OK|X_OK) ) {
            break;
        }
    }
    --max_processor;

    /* Malloc space for the first map (processor ID -> tuple).
       Include enough space for one invalid entry. */
    map_processor_id_to_tuple = malloc(sizeof(tuple_t) * (max_processor + 2));
    if (NULL == map_processor_id_to_tuple) {
        return;
    }
    for (i = 0; i <= max_processor; ++i) {
        map_processor_id_to_tuple[i].processor_id = i;
        map_processor_id_to_tuple[i].socket = -1;
        map_processor_id_to_tuple[i].core = -1;
    }
    /* Set the invalid entry */
    invalid_entry = i;
    map_processor_id_to_tuple[invalid_entry].processor_id = -1;
    map_processor_id_to_tuple[invalid_entry].socket = -1;
    map_processor_id_to_tuple[invalid_entry].core = -1;

    /* Malloc space for the max number of cores on each socket */
    max_core = malloc(sizeof(int) * (max_processor + 1));
    if (NULL == max_core) {
        clear_cache();
        return;
    }
    for (i = 0; i <= max_processor; ++i) {
        max_core[i] = -1;
    }

    /* Build a cached map of (socket,core) tuples */
    for ( i = 0; i <= max_processor; i++ ) {
        sprintf(path, "%s/devices/system/cpu/cpu%d/topology/core_id", 
                sysfs_mount, i);
        fd = open(path, O_RDONLY);
        if ( fd < 0 ) {
            clear_cache();
            return;
        }
        if ( read(fd, buf, 7) <= 0 ) {
            clear_cache();
            return;
        }
        sscanf(buf, "%d", &(map_processor_id_to_tuple[i].core));
        close(fd);
        
        sprintf(path, "%s/devices/system/cpu/cpu%d/topology/physical_package_id",
                sysfs_mount, i);
        fd = open(path, O_RDONLY);
        if ( fd < 0 ) {
            clear_cache();
            return;
        }
        if ( read(fd, buf, 7) <= 0 ) {
            clear_cache();
            return;
        }
        sscanf(buf, "%d", &(map_processor_id_to_tuple[i].socket));
        close(fd);
        
        /* Compute some globals */
        if (map_processor_id_to_tuple[i].socket > max_socket) {
            max_socket = map_processor_id_to_tuple[i].socket;
        }
        if (map_processor_id_to_tuple[i].core > 
            max_core[map_processor_id_to_tuple[i].socket]) {
            max_core[map_processor_id_to_tuple[i].socket] = 
                map_processor_id_to_tuple[i].core;
        }
        if (max_core[map_processor_id_to_tuple[i].socket] > max_core_overall) {
            max_core_overall = max_core[map_processor_id_to_tuple[i].socket];
        }
    }

    /* Now go through and build the map in the other direction:
       (socket,core) => processor_id.  This map simply points to
       entries in the other map (i.e., it's by reference instead of by
       value). */
    map_tuple_to_processor_id = malloc(sizeof(tuple_t **) * (max_socket + 1));
    if (NULL == map_tuple_to_processor_id) {
        clear_cache();
        return;
    }
    map_tuple_to_processor_id[0] = malloc(sizeof(tuple_t *) * 
                                          ((max_socket + 1) * 
                                           (max_core_overall + 1)));
    if (NULL == map_tuple_to_processor_id[0]) {
        clear_cache();
        return;
    }
    /* Set pointers for 2nd dimension */
    for (i = 1; i <= max_socket; ++i) {
        map_tuple_to_processor_id[i] = 
            map_tuple_to_processor_id[i - 1] + max_core_overall;
    }
    /* Compute map */
    for (i = 0; i <= max_socket; ++i) {
        for (j = 0; j <= max_core_overall; ++j) {
            /* Default to the invalid entry in the other map, meaning
               that this (socket,core) combination doesn't exist
               (e.g., the core number does not exist in this socket,
               although it does exist in other sockets). */
            map_tuple_to_processor_id[i][j] = 
                &map_processor_id_to_tuple[invalid_entry];

            /* See if this (socket,core) tuple exists in the other
               map.  If so, set this entry to point to it (overriding
               the invalid entry default). */
            for (k = 0; k <= max_processor; ++k) {
                if (map_processor_id_to_tuple[k].socket == i &&
                    map_processor_id_to_tuple[k].core == j) {
                    map_tuple_to_processor_id[i][j] = 
                        &map_processor_id_to_tuple[k];
                    break;
                }
            }
        }
    }

    supported = 1;
}

/* Internal function to setup the mapping data.  Guaranteed to be
   calling during PLPA_NAME(init), so we don't have to worry about
   thread safety here. */
int PLPA_NAME(map_init)(void)
{
    const char *sysfs_mount = "/sys";
    char *temp;

    temp = getenv("PLPA_SYSFS_MOUNT");
    if (temp) {
        sysfs_mount = temp;
    }

    load_cache(sysfs_mount);
    return 0;
}

/* Internal function to cleanup allocated memory.  Only called by one
   thread (during PLPA_NAME(finalize), so don't need to worry about
   thread safety here. */
void PLPA_NAME(map_finalize)(void)
{
    clear_cache();
}

/* Return whether this kernel supports topology information or not */
int PLPA_NAME(have_topology_information)(int *supported_arg)
{
    int ret;

    /* Initialize if not already done so */
    if (!PLPA_NAME(initialized)) {
        if (0 != (ret = PLPA_NAME(init)())) {
            return ret;
        }
    }

    /* Check for bozo arguments */
    if (NULL == supported_arg) {
        return EINVAL;
    }

    *supported_arg = supported;
    return 0;
}

int PLPA_NAME(map_to_processor_id)(int socket, int core, int *processor_id)
{
    int ret;

    /* Initialize if not already done so */
    if (!PLPA_NAME(initialized)) {
        if (0 != (ret = PLPA_NAME(init)())) {
            return ret;
        }
    }

    /* Check for bozo arguments */
    if (NULL == processor_id) {
        return EINVAL;
    }

    /* If this system doesn't support mapping, sorry Charlie */
    if (!supported) {
        return ENOENT;
    }

    /* Check for some invalid entries */
    if (socket < 0 || socket > max_socket ||
        core < 0 || core > max_core_overall ||
        NULL == processor_id) {
        return EINVAL;
    }

    /* Ok, all should be good -- return the mapping */
    *processor_id = map_tuple_to_processor_id[socket][core]->processor_id;
    return 0;
}

int PLPA_NAME(map_to_socket_core)(int processor_id, int *socket, int *core)
{
    int ret;

    /* Initialize if not already done so */
    if (!PLPA_NAME(initialized)) {
        if (0 != (ret = PLPA_NAME(init)())) {
            return ret;
        }
    }

    /* Check for bozo arguments */
    if (NULL == socket || NULL == core) {
        return EINVAL;
    }

    /* If this system doesn't support mapping, sorry Charlie */
    if (!supported) {
        return ENOENT;
    }

    /* Check for some invalid entries */
    if (processor_id < 0 || processor_id > max_processor ||
        NULL == socket ||
        NULL == core) {
        return EINVAL;
    }

    /* Ok, all should be good -- return the mapping */
    *socket = map_processor_id_to_tuple[processor_id].socket;
    *core = map_processor_id_to_tuple[processor_id].core;
    return 0;
}

int PLPA_NAME(max_processor_id)(int *max_processor_id_arg)
{
    int ret;

    /* Initialize if not already done so */
    if (!PLPA_NAME(initialized)) {
        if (0 != (ret = PLPA_NAME(init)())) {
            return ret;
        }
    }

    /* Check for bozo arguments */
    if (NULL == max_processor_id_arg) {
        return EINVAL;
    }

    /* If this system doesn't support mapping, sorry Charlie */
    if (!supported) {
        return ENOENT;
    }

    /* All done */
    *max_processor_id_arg = max_processor;
    return 0;
}

/* Return the max socket number */
int PLPA_NAME(max_socket)(int *max_socket_arg)
{
    int ret;

    /* Initialize if not already done so */
    if (!PLPA_NAME(initialized)) {
        if (0 != (ret = PLPA_NAME(init)())) {
            return ret;
        }
    }

    /* Check for bozo arguments */
    if (NULL == max_socket_arg) {
        return EINVAL;
    }

    /* If this system doesn't support mapping, sorry Charlie */
    if (!supported) {
        return ENOENT;
    }

    /* All done */
    *max_socket_arg = max_socket;
    return 0;
}

/* Return the max core number for a given socket */
int PLPA_NAME(max_core)(int socket, int *max_core_arg)
{
    int ret;

    /* Initialize if not already done so */
    if (!PLPA_NAME(initialized)) {
        if (0 != (ret = PLPA_NAME(init)())) {
            return ret;
        }
    }

    /* Check for bozo arguments */
    if (NULL == max_core_arg) {
        return EINVAL;
    }

    /* If this system doesn't support mapping, sorry Charlie */
    if (!supported) {
        return ENOENT;
    }

    /* Check for some invalid entries */
    if (socket < 0 || socket > max_socket) {
        return EINVAL;
    }

    /* All done */
    *max_core_arg = max_core[socket];
    return 0;
}
