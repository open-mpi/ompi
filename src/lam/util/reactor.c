/*
 * $HEADER$
 *
 * Copyright 2002-2003. The Regents of the University of
 * California. This material was produced under U.S. Government
 * contract W-7405-ENG-36 for Los Alamos National Laboratory, which is
 * operated by the University of California for the U.S. Department of
 * Energy. The Government is granted for itself and others acting on
 * its behalf a paid-up, nonexclusive, irrevocable worldwide license
 * in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years
 * after October 10,2002 subject to additional five-year worldwide
 * renewals, the Government is granted for itself and others acting on
 * its behalf a paid-up, nonexclusive, irrevocable worldwide license
 * in this material to reproduce, prepare derivative works, distribute
 * copies to the public, perform publicly and display publicly, and to
 * permit others to do so. NEITHER THE UNITED STATES NOR THE UNITED
 * STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF CALIFORNIA, NOR
 * ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR
 * ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT,
 * OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE
 * PRIVATELY OWNED RIGHTS.

 * Additionally, this program is free software; you can distribute it
 * and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or any later version.  Accordingly, this
 * program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include <sys/types.h>
#include <sys/select.h>
#include <sys/time.h>
#include <unistd.h>
#include <errno.h>

#include "lam/util/reactor.h"


const int LAM_NOTIFY_RECV = 1;
const int LAM_NOTIFY_SEND = 2;
const int LAM_NOTIFY_EXCEPT = 4;
const int LAM_NOTIFY_ALL = 7;

#define MAX_DESCRIPTOR_POOL_SIZE 256

                                                                            
lam_class_info_t lam_reactor_cls = {
    "lam_reactor_t", &object_cls, (class_init_t)lam_reactor_init,
    (class_destroy_t)lam_reactor_destroy
};
                                                                                                                
lam_class_info_t lam_reactor_descriptor_cls = {
    "lam_reactor_t", &lam_dbl_item_cls, (class_init_t)lam_reactor_descriptor_init,
    (class_destroy_t)lam_reactor_descriptor_destroy
};

                                                                                                                
void lam_reactor_descriptor_init(lam_reactor_descriptor_t* rd)
{
    lam_dbl_item_init(&rd->rd_base);
    rd->rd_base.super.obj_class = &lam_reactor_descriptor_cls;
}


void lam_reactor_descriptor_destroy(lam_reactor_descriptor_t* rd)
{
    lam_dbl_item_destroy(&rd->rd_base);
}


static inline lam_reactor_descriptor_t* lam_reactor_get_descriptor(lam_reactor_t* r, int sd)
{
    lam_reactor_descriptor_t *descriptor;
    if(lam_dbl_get_size(&r->r_free))
        descriptor = (lam_reactor_descriptor_t*)lam_dbl_remove_first_item(&r->r_free);
    else {
        descriptor = (lam_reactor_descriptor_t*)lam_malloc(sizeof(lam_reactor_descriptor_t));
        lam_reactor_descriptor_init(descriptor);
    }
    if(descriptor == 0) {
        lam_err(("lam_reactor_get_descriptor(): malloc(%d) failed.", sizeof(lam_reactor_descriptor_t)));
        return 0;
    }
    descriptor->rd = sd;
    descriptor->rd_flags = 0;
    descriptor->rd_recv = 0;
    descriptor->rd_send = 0;
    descriptor->rd_except = 0;
    return descriptor;
}


void lam_reactor_init(lam_reactor_t* r)
{ 
    lam_obj_init(&r->r_base);
    r->r_base.obj_class = &lam_reactor_cls;

    lam_mutex_init(&r->r_mutex);
    lam_dbl_init(&r->r_active);
    lam_dbl_init(&r->r_free);
    lam_dbl_init(&r->r_pending);
    lam_fh_init(&r->r_hash);
    lam_fh_init_with(&r->r_hash, 1024);

    r->r_max = -1;
    r->r_run = LAM_TRUE;
    r->r_changes = 0;

    LAM_FD_ZERO(&r->r_recv_set);
    LAM_FD_ZERO(&r->r_send_set);
    LAM_FD_ZERO(&r->r_except_set);
}


void lam_reactor_destroy(lam_reactor_t* r)
{
    lam_dbl_destroy(&r->r_active);
    lam_dbl_destroy(&r->r_free);
    lam_dbl_destroy(&r->r_pending);
    lam_fh_destroy(&r->r_hash);
    lam_obj_destroy(&r->r_base);
}


lam_bool_t lam_reactor_insert(lam_reactor_t* r, int sd, lam_reactor_listener_t* listener, int flags)
{
#ifndef NDEBUG
    if(sd < 0 || sd > LAM_FD_SETSIZE) {
        lam_err(("Reactor::insertListener(%d) invalid descriptor.\n", sd));
        return LAM_FALSE;
    }
#endif

    lam_mtx_lock(&r->r_mutex);
    lam_reactor_descriptor_t *descriptor = (lam_reactor_descriptor_t*)lam_dbl_remove_first(&r->r_free);
    if(descriptor == 0) {
        descriptor = lam_reactor_get_descriptor(r, sd);
        if(descriptor == 0) {
            lam_mtx_unlock(&r->r_mutex);
            return LAM_FALSE;
        }
        lam_dbl_append(&r->r_pending, &descriptor->rd_base);
        lam_fh_set_value_for_ikey(&r->r_hash,descriptor,sd);
    }

    descriptor->rd_flags |= flags;
    if(flags & LAM_NOTIFY_RECV) {
        descriptor->rd_recv = listener;
        LAM_FD_SET(sd, &r->r_recv_set);
    }
    if(flags & LAM_NOTIFY_SEND) {
        descriptor->rd_send = listener;
        LAM_FD_SET(sd, &r->r_send_set);
    }
    if(flags & LAM_NOTIFY_EXCEPT) {
        descriptor->rd_except = listener;
        LAM_FD_SET(sd, &r->r_except_set);
    }
    r->r_changes++;
    lam_mtx_unlock(&r->r_mutex);
    return LAM_TRUE;
}


lam_bool_t lam_reactor_remove(lam_reactor_t* r, int sd, lam_reactor_listener_t* rl, int flags)
{
#ifndef NDEBUG
    if(sd < 0 || sd > LAM_FD_SETSIZE) {
        lam_err(("lam_reactor_remove(%d) invalid descriptor.\n", sd));
        return LAM_FALSE;
    }
#endif

    lam_mtx_lock(&r->r_mutex);
    lam_reactor_descriptor_t* descriptor = (lam_reactor_descriptor_t*)lam_fh_get_value_for_ikey(&r->r_hash, sd);
    if(descriptor == 0) {
        lam_err(("lam_reactor_remove(%d): descriptor not registered.\n", sd));
        lam_mtx_unlock(&r->r_mutex);
        return LAM_FALSE;
    }
    descriptor->rd_flags &= ~flags;
    if(flags & LAM_NOTIFY_RECV) {
        descriptor->rd_recv = 0;
        LAM_FD_CLR(sd, &r->r_recv_set);
    }
    if(flags & LAM_NOTIFY_SEND) {
        descriptor->rd_send = 0;
        LAM_FD_CLR(sd, &r->r_send_set);
    }
    if(flags & LAM_NOTIFY_EXCEPT) {
        descriptor->rd_except = 0;
        LAM_FD_CLR(sd, &r->r_except_set);
    }
    r->r_changes++;
    lam_mtx_unlock(&r->r_mutex);
    return LAM_TRUE;
}


void lam_reactor_dispatch(lam_reactor_t* r, int cnt, lam_fd_set_t* rset, lam_fd_set_t* sset, lam_fd_set_t* eset)
{
    // walk through the active list w/out holding lock, as this thread
    // is the only one that modifies the active list. however, note
    // that the descriptor flags could have been cleared in a callback,
    // so check that the flag is still set before invoking the callbacks

    lam_reactor_descriptor_t *descriptor;
    for(descriptor =  (lam_reactor_descriptor_t*)lam_dbl_get_first(&r->r_active);
        descriptor != 0;
        descriptor =  (lam_reactor_descriptor_t*)lam_dbl_get_next(descriptor)) {
        int rd = descriptor->rd; 
        int flags = 0;
        if(LAM_FD_ISSET(rd, rset) && descriptor->rd_flags & LAM_NOTIFY_RECV) {
            descriptor->rd_recv->rl_recv_handler(descriptor->rd_recv, rd);
            flags |= LAM_NOTIFY_RECV;
        }
        if(LAM_FD_ISSET(rd, sset) && descriptor->rd_flags & LAM_NOTIFY_SEND) {
            descriptor->rd_send->rl_send_handler(descriptor->rd_send, rd);
            flags |= LAM_NOTIFY_SEND;
        }
        if(LAM_FD_ISSET(rd, eset) && descriptor->rd_flags & LAM_NOTIFY_EXCEPT) {
            descriptor->rd_except->rl_except_handler(descriptor->rd_except, rd);
            flags |= LAM_NOTIFY_EXCEPT;
        }
        if(flags) cnt--;
    }

    lam_mtx_lock(&r->r_mutex);
    if(r->r_changes == 0) {
        lam_mtx_unlock(&r->r_mutex);
        return;
    }

    // cleanup any pending deletes while holding the lock
    descriptor = (lam_reactor_descriptor_t*)lam_dbl_get_first(&r->r_active);
    while(descriptor != 0) {
        lam_reactor_descriptor_t* next = (lam_reactor_descriptor_t*)lam_dbl_get_next(&r->r_active);
        if(descriptor->rd_flags == 0) {
            lam_fh_remove_value_for_ikey(&r->r_hash, descriptor->rd);
            lam_dbl_list_remove(&r->r_active, descriptor);
            if(lam_dbl_get_size(&r->r_free) < MAX_DESCRIPTOR_POOL_SIZE) {
                lam_dbl_append(&r->r_free, &descriptor->rd_base);
            } else {
                lam_reactor_descriptor_destroy(descriptor);
                lam_free(descriptor);
            }
        } 
        descriptor = next;
    } 

    // add any other pending inserts/deletes
    while(lam_dbl_get_size(&r->r_pending)) {
        lam_reactor_descriptor_t* descriptor = (lam_reactor_descriptor_t*)lam_dbl_remove_first(&r->r_pending);
        if(descriptor->rd_flags == 0) {
            lam_fh_remove_value_for_ikey(&r->r_hash, descriptor->rd);
            if(lam_dbl_get_size(&r->r_free) < MAX_DESCRIPTOR_POOL_SIZE) {
                lam_dbl_append(&r->r_free, &descriptor->rd_base);
            } else {
                lam_reactor_descriptor_destroy(descriptor);
                lam_free(descriptor);
            }
        } else {
            lam_dbl_append(&r->r_active, &descriptor->rd_base);
            if(descriptor->rd > r->r_max)
                r->r_max = descriptor->rd;
        }
    }

    r->r_changes = 0;
    lam_mtx_unlock(&r->r_mutex);
}


void lam_reactor_poll(lam_reactor_t* r)
{
    struct timeval tm;
    tm.tv_sec = 0;
    tm.tv_usec = 0;
    lam_fd_set_t rset = r->r_recv_set;
    lam_fd_set_t sset = r->r_send_set;
    lam_fd_set_t eset = r->r_except_set;
    int rc = select(r->r_max+1, (fd_set*)&rset, (fd_set*)&sset, (fd_set*)&eset, &tm);
    if(rc < 0) {
#ifndef WIN32
        if(errno != EINTR)
#endif
           lam_exit((-1, "lam_reactor_poll: select() failed with errno=%d\n", errno));
        return;
    }
    lam_reactor_dispatch(r, rc, &rset, &sset, &eset);
}


void lam_reactor_run(lam_reactor_t* r)
{
    while(r->r_run == LAM_TRUE) {
        lam_fd_set_t rset = r->r_recv_set;
        lam_fd_set_t sset = r->r_send_set;
        lam_fd_set_t eset = r->r_except_set;
        int rc = select(r->r_max+1, (fd_set*)&rset, (fd_set*)&sset, (fd_set*)&eset, 0);
        if(rc < 0) {
#ifndef WIN32
            if(errno != EINTR)
#endif
                lam_exit((-1, "lam_reactor_run: select() failed with errno=%d\n", errno));
            continue;
        }
        lam_reactor_dispatch(r, rc, &rset, &sset, &eset);
    }
}



