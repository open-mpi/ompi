/*
 * $HEADER$
 */

#ifndef LAM_REACTOR_H
#define LAM_REACTOR_H

#include "lam/types.h"
#include "lam/lfc/list.h"
#include "lam/lfc/hash_table.h"
#include "lam/threads/mutex.h"

extern const int LAM_REACTOR_NOTIFY_ALL;
extern const int LAM_REACTOR_NOTIFY_RECV;
extern const int LAM_REACTOR_NOTIFY_SEND;
extern const int LAM_REACTOR_NOTIFY_EXCEPT;

extern lam_class_info_t lam_reactor_cls;


/*
 * Utilizes select() to provide callbacks when an event
 * (e.g. readable,writeable,exception) occurs on a designated
 * descriptor.  Objects interested in receiving callbacks must
 * implement the lam_reactor_listener_t interface.
 */

struct lam_reactor_listener_t;
typedef void (*lam_rl_recv_handler_fn_t)(void* user, int sd);
typedef void (*lam_rl_send_handler_fn_t)(void* user, int sd);
typedef void (*lam_rl_except_handler_fn_t)(void* user, int sd);

struct lam_reactor_listener_t {
    lam_rl_recv_handler_fn_t   rl_recv_handler;
    lam_rl_send_handler_fn_t   rl_send_handler;
    lam_rl_except_handler_fn_t rl_except_handler;
};
typedef struct lam_reactor_listener_t lam_reactor_listener_t;


struct lam_reactor_descriptor_t {
    lam_list_item_t         super;
    int                     rd;
    volatile int            rd_flags;
    lam_reactor_listener_t* rd_recv;
    void*                   rd_recv_user;
    lam_reactor_listener_t* rd_send;
    void*                   rd_send_user;
    lam_reactor_listener_t* rd_except;
    void*                   rd_except_user;
};
typedef struct lam_reactor_descriptor_t lam_reactor_descriptor_t;


void lam_reactor_descriptor_init(lam_reactor_descriptor_t*);
void lam_reactor_descriptor_destroy(lam_reactor_descriptor_t*);


struct lam_reactor_t {
    lam_object_t       super;
    lam_mutex_t        r_mutex;
    lam_list_t         r_active;
    lam_list_t         r_free;
    lam_list_t         r_pending;
    lam_fast_hash_t    r_hash;
    int                r_max;
    bool               r_run;
    int                r_changes;
    lam_fd_set_t       r_send_set;
    lam_fd_set_t       r_recv_set;
    lam_fd_set_t       r_except_set;
};
typedef struct lam_reactor_t lam_reactor_t;

 
void lam_reactor_init(lam_reactor_t*);
void lam_reactor_destroy(lam_reactor_t*);

int  lam_reactor_insert(lam_reactor_t*, int sd, lam_reactor_listener_t*, void* user, int flags);
int  lam_reactor_remove(lam_reactor_t*, int sd, int flags);
void lam_reactor_poll(lam_reactor_t*);
void lam_reactor_run(lam_reactor_t*);
void lam_reactor_dispatch(lam_reactor_t* r, int cnt, lam_fd_set_t* rset, lam_fd_set_t* sset, lam_fd_set_t* eset);

#endif /* LAM_REACTOR_H */

