/*
 * $HEADER$
 */

#ifndef LAM_REACTOR_H
#define LAM_REACTOR_H

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
    bool               r_run;
    int                r_changes;
    lam_fd_set_t       r_send_set;
    lam_fd_set_t       r_recv_set;
    lam_fd_set_t       r_except_set;
} lam_reactor_t;

 
void lam_reactor_init(lam_reactor_t*);
void lam_reactor_destroy(lam_reactor_t*);

bool lam_reactor_insert(lam_reactor_t*, int sd, lam_reactor_listener_t*, int flags);
bool lam_reactor_remove(lam_reactor_t*, int sd, lam_reactor_listener_t*, int flags);
void lam_reactor_poll(lam_reactor_t*);
void lam_reactor_run(lam_reactor_t*);

#endif /* LAM_REACTOR_H */

