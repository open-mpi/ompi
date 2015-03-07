/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * LICENSE_END
 *
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <errno.h>

#include <infiniband/kern-abi.h>
#include <infiniband/verbs.h>

#include "usnic_direct.h"
#include "usd.h"

/*
 * Read an event from IB event fd
 */
int
usd_get_device_event(struct usd_device *dev,
                     struct usd_device_event *devent)
{
    struct ibv_kern_async_event ib_event;
    int n;

    n = read(dev->ud_attrs.uda_event_fd, &ib_event, sizeof(ib_event));
    if (n == 0)
        return -EAGAIN;
    else if (n < 0)
        return -errno;

    switch (ib_event.event_type) {
    case IBV_EVENT_PORT_ACTIVE:
        devent->ude_type = USD_EVENT_LINK_UP;
        break;
    case IBV_EVENT_PORT_ERR:
        devent->ude_type = USD_EVENT_LINK_DOWN;
        break;
    default:
        printf("Unexpected event type: %d\n", ib_event.event_type);
        return -EAGAIN;
        break;
    }

    return 0;
}
