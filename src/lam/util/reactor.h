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

#ifndef LAM_REACTOR
#define LAM_REACTOR

#include "include/lam_types.h"
#include "lam/lfc/list.h"
#include "lam/lfc/hash_table.h"
#include "lam/threads/mutex.h"

extern const int LAM_NOTIFY_ALL;
extern const int LAM_NOTIFY_RECV;
extern const int LAM_NOTIFY_SEND;
extern const int LAM_NOTIFY_EXCEPT;

extern lam_class_info_t lam_reactor_cls;


//
//  Utilizes select() to provide callbacks when an event (e.g. readable,writeable,exception)
//  occurs on a designated descriptor.  Objects interested in receiving callbacks must implement
//  the lam_reactor_listener_t interface.
//

typedef struct _lam_reactor_listener {
    void *rl_user_data;
    void (*rl_recv_handler)(struct _lam_reactor_listener*, int sd);
    void (*rl_send_handler)(struct _lam_reactor_listener*, int sd);
    void (*rl_except_handler)(struct _lam_reactor_listener*, int sd);
} lam_reactor_listener_t;


typedef struct _lam_reactor_descriptor {
    lam_dbl_item_t          rd_base;
    int                     rd;
    volatile int            rd_flags;
    lam_reactor_listener_t *rd_recv;
    lam_reactor_listener_t *rd_send;
    lam_reactor_listener_t *rd_except;
} lam_reactor_descriptor_t;


void lam_reactor_descriptor_init(lam_reactor_descriptor_t*);
void lam_reactor_descriptor_destroy(lam_reactor_descriptor_t*);


typedef struct _lam_reactor {
    lam_object_t       r_base;
    lam_mutex_t        r_mutex;
    lam_dbl_list_t     r_active;
    lam_dbl_list_t     r_free;
    lam_dbl_list_t     r_pending;
    lam_fast_hash_t    r_hash;
    int                r_max;
    lam_bool_t         r_run;
    int                r_changes;
    lam_fd_set_t       r_send_set;
    lam_fd_set_t       r_recv_set;
    lam_fd_set_t       r_except_set;
} lam_reactor_t;

 
void lam_reactor_init(lam_reactor_t*);
void lam_reactor_destroy(lam_reactor_t*);

lam_bool_t lam_reactor_insert(lam_reactor_t*, int sd, lam_reactor_listener_t*, int flags);
lam_bool_t lam_reactor_remove(lam_reactor_t*, int sd, lam_reactor_listener_t*, int flags);
void   lam_reactor_poll(lam_reactor_t*);
void   lam_reactor_run(lam_reactor_t*);

#endif /* LAM_REACTOR */

