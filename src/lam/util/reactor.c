/*
 * $HEADER$
 */

#include <sys/types.h>
#include <sys/select.h>
#include <sys/time.h>
#include <unistd.h>
#include <errno.h>

#include "lam_config.h"
#include "lam/mem/malloc.h"
#include "lam/util/reactor.h"
#include "lam/util/output.h"
#include "lam/runtime/runtime.h"


const int LAM_NOTIFY_RECV = 1;
const int LAM_NOTIFY_SEND = 2;
const int LAM_NOTIFY_EXCEPT = 4;
const int LAM_NOTIFY_ALL = 7;

#define MAX_DESCRIPTOR_POOL_SIZE 256

                                                                            
lam_class_info_t lam_reactor_cls = {
    "lam_reactor_t", &lam_object_cls, (class_init_t)lam_reactor_init,
    (class_destroy_t)lam_reactor_destroy
};
                                                                                                                
lam_class_info_t lam_reactor_descriptor_cls = {
    "lam_reactor_t", &lam_list_item_cls, (class_init_t)lam_reactor_descriptor_init,
    (class_destroy_t)lam_reactor_descriptor_destroy
};

                                                                                                                
void lam_reactor_descriptor_init(lam_reactor_descriptor_t* rd)
{
    lam_list_item_init(&rd->rd_base);
    rd->rd_base.super.obj_class = &lam_reactor_descriptor_cls;
}


void lam_reactor_descriptor_destroy(lam_reactor_descriptor_t* rd)
{
    lam_list_item_destroy(&rd->rd_base);
}


static inline lam_reactor_descriptor_t* lam_reactor_get_descriptor(lam_reactor_t* r, int sd)
{
    lam_reactor_descriptor_t *descriptor;
    if(lam_list_get_size(&r->r_free))
        descriptor = (lam_reactor_descriptor_t*)lam_list_remove_first(&r->r_free);
    else {
        descriptor = (lam_reactor_descriptor_t*)LAM_MALLOC(sizeof(lam_reactor_descriptor_t));
        lam_reactor_descriptor_init(descriptor);
    }
    if (NULL == descriptor) {
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
    lam_list_init(&r->r_active);
    lam_list_init(&r->r_free);
    lam_list_init(&r->r_pending);
    lam_fh_init(&r->r_hash);
    lam_fh_init_with(&r->r_hash, 1024);

    r->r_max = -1;
    r->r_run = true;
    r->r_changes = 0;

    LAM_FD_ZERO(&r->r_recv_set);
    LAM_FD_ZERO(&r->r_send_set);
    LAM_FD_ZERO(&r->r_except_set);
}


void lam_reactor_destroy(lam_reactor_t* r)
{
    lam_list_destroy(&r->r_active);
    lam_list_destroy(&r->r_free);
    lam_list_destroy(&r->r_pending);
    lam_fh_destroy(&r->r_hash);
    lam_obj_destroy(&r->r_base);
}


bool lam_reactor_insert(lam_reactor_t* r, int sd, lam_reactor_listener_t* listener, void* user, int flags)
{
#ifndef NDEBUG
    if(sd < 0 || sd > LAM_FD_SETSIZE) {
      lam_output(0, "lam_reactor_insert(%d) invalid descriptor", sd);
      return false;
    }
#endif

    lam_mutex_lock(&r->r_mutex);
    lam_reactor_descriptor_t *descriptor = (lam_reactor_descriptor_t*)lam_list_remove_first(&r->r_free);
    if(descriptor == 0) {
        descriptor = lam_reactor_get_descriptor(r, sd);
        if(descriptor == 0) {
            lam_mutex_unlock(&r->r_mutex);
            return false;
        }
        lam_list_append(&r->r_pending, &descriptor->rd_base);
        lam_fh_set_value_for_ikey(&r->r_hash,descriptor,sd);
    }

    descriptor->rd_flags |= flags;
    if(flags & LAM_NOTIFY_RECV) {
        descriptor->rd_recv = listener;
        descriptor->rd_recv_user = user;
        LAM_FD_SET(sd, &r->r_recv_set);
    }
    if(flags & LAM_NOTIFY_SEND) {
        descriptor->rd_send = listener;
        descriptor->rd_send_user = user;
        LAM_FD_SET(sd, &r->r_send_set);
    }
    if(flags & LAM_NOTIFY_EXCEPT) {
        descriptor->rd_except = listener;
        descriptor->rd_except_user = user;
        LAM_FD_SET(sd, &r->r_except_set);
    }
    r->r_changes++;
    lam_mutex_unlock(&r->r_mutex);
    return true;
}


bool lam_reactor_remove(lam_reactor_t* r, int sd, int flags)
{
#ifndef NDEBUG
    if(sd < 0 || sd > LAM_FD_SETSIZE) {
      lam_output(0, "lam_reactor_remove(%d) invalid descriptor", sd);
      return false;
    }
#endif

    lam_mutex_lock(&r->r_mutex);
    lam_reactor_descriptor_t* descriptor = (lam_reactor_descriptor_t*)lam_fh_get_value_for_ikey(&r->r_hash, sd);
    if (NULL == descriptor) {
      lam_output(0, "lam_reactor_remove(%d): descriptor not registered", sd);
      lam_mutex_unlock(&r->r_mutex);
      return false;
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
    lam_mutex_unlock(&r->r_mutex);
    return true;
}


void lam_reactor_dispatch(lam_reactor_t* r, int cnt, lam_fd_set_t* rset, lam_fd_set_t* sset, lam_fd_set_t* eset)
{
    /*
     * walk through the active list w/out holding lock, as this thread
     * is the only one that modifies the active list. however, note
     * that the descriptor flags could have been cleared in a callback,
     * so check that the flag is still set before invoking the callbacks
     */

    lam_reactor_descriptor_t *descriptor;
    for(descriptor =  (lam_reactor_descriptor_t*)lam_list_get_first(&r->r_active);
        descriptor != 0;
        descriptor =  (lam_reactor_descriptor_t*)lam_list_get_next(descriptor)) {
        int rd = descriptor->rd; 
        int flags = 0;
        if(LAM_FD_ISSET(rd, rset) && descriptor->rd_flags & LAM_NOTIFY_RECV) {
            descriptor->rd_recv->rl_recv_handler(rd, descriptor->rd_recv_user);
            flags |= LAM_NOTIFY_RECV;
        }
        if(LAM_FD_ISSET(rd, sset) && descriptor->rd_flags & LAM_NOTIFY_SEND) {
            descriptor->rd_send->rl_send_handler(rd, descriptor->rd_send_user);
            flags |= LAM_NOTIFY_SEND;
        }
        if(LAM_FD_ISSET(rd, eset) && descriptor->rd_flags & LAM_NOTIFY_EXCEPT) {
            descriptor->rd_except->rl_except_handler(rd, descriptor->rd_except_user);
            flags |= LAM_NOTIFY_EXCEPT;
        }
        if(flags) cnt--;
    }

    lam_mutex_lock(&r->r_mutex);
    if(r->r_changes == 0) {
        lam_mutex_unlock(&r->r_mutex);
        return;
    }

    /* cleanup any pending deletes while holding the lock */
    descriptor = (lam_reactor_descriptor_t*)lam_list_get_first(&r->r_active);
    while(descriptor != 0) {
        lam_reactor_descriptor_t* next = (lam_reactor_descriptor_t*)lam_list_get_next(&r->r_active);
        if(descriptor->rd_flags == 0) {
            lam_fh_remove_value_for_ikey(&r->r_hash, descriptor->rd);
            lam_list_remove(&r->r_active, (lam_list_item_t*)descriptor);
            if(lam_list_get_size(&r->r_free) < MAX_DESCRIPTOR_POOL_SIZE) {
                lam_list_append(&r->r_free, &descriptor->rd_base);
            } else {
                lam_reactor_descriptor_destroy(descriptor);
                LAM_FREE(descriptor);
            }
        } 
        descriptor = next;
    } 

    /* add any other pending inserts/deletes */
    while(lam_list_get_size(&r->r_pending)) {
        lam_reactor_descriptor_t* descriptor = (lam_reactor_descriptor_t*)lam_list_remove_first(&r->r_pending);
        if(descriptor->rd_flags == 0) {
            lam_fh_remove_value_for_ikey(&r->r_hash, descriptor->rd);
            if(lam_list_get_size(&r->r_free) < MAX_DESCRIPTOR_POOL_SIZE) {
                lam_list_append(&r->r_free, &descriptor->rd_base);
            } else {
                lam_reactor_descriptor_destroy(descriptor);
                LAM_FREE(descriptor);
            }
        } else {
            lam_list_append(&r->r_active, &descriptor->rd_base);
            if(descriptor->rd > r->r_max)
                r->r_max = descriptor->rd;
        }
    }

    r->r_changes = 0;
    lam_mutex_unlock(&r->r_mutex);
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
      if (EINTR != errno) {
#endif
        lam_abort(1, "lam_reactor_poll: select() failed with errno=%d", errno);
#ifndef WIN32
      }
#endif
      return;
    }
    lam_reactor_dispatch(r, rc, &rset, &sset, &eset);
}


void lam_reactor_run(lam_reactor_t* r)
{
    while(true == r->r_run) {
        lam_fd_set_t rset = r->r_recv_set;
        lam_fd_set_t sset = r->r_send_set;
        lam_fd_set_t eset = r->r_except_set;
        int rc = select(r->r_max+1, (fd_set*)&rset, (fd_set*)&sset, (fd_set*)&eset, 0);
        if(rc < 0) {
#ifndef WIN32
          if (EINTR != errno) {
#endif
            lam_abort(1, "lam_reactor_run: select() failed with errno=%d",
                      errno);
#ifndef WIN32
          }
#endif
            continue;
        }
        lam_reactor_dispatch(r, rc, &rset, &sset, &eset);
    }
}



