#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include <netinet/in.h>

struct mca_oob_base_cb_data_t {
    mca_oob_callback_fn_t user_callback;
    void * user_data;
    const mca_oob_base_type_t * types;
    const struct iovec * user_iovec;
};

/* this is the callback function we will register when we have to do any conversion */
static void mca_oob_base_recv_cb(int status, const ompi_process_name_t* peer,
                                 const struct iovec* msg,
                                 int count, int tag, void* cbdata);

static void mca_oob_base_recv_cb(int status, const ompi_process_name_t* peer,
                                 const struct iovec* msg,
                                 int count, int tag, void* cbdata)
{
    int i, num;
    struct mca_oob_base_cb_data_t * cb_struct = (struct mca_oob_base_cb_data_t *) cbdata;
    const struct iovec * user_iovec = cb_struct->user_iovec;
    mca_oob_base_type_t * types = cb_struct->types;
                            
    for(i = 0; i < count; i++) {
        if(types[i] == MCA_OOB_BASE_INT16) {
            /* figure out how many integers we have */
            num = msg[i].iov_len / 2;
            /* unpack the data */
            mca_oob_base_unpack(user_iovec[i].iov_base, msg[i].iov_base, num, MCA_OOB_BASE_INT16);
            /* free the old buffer */
            free(msg[i].iov_base);
        } else if(types[i] == MCA_OOB_BASE_INT32) {
            /* figure out how many integers we have */
            num = msg[i].iov_len / 4;
            /* unpack the data */
            mca_oob_base_unpack(user_iovec[i].iov_base, msg[i].iov_base, num, MCA_OOB_BASE_INT32);
            /* free the old buffer */
            free(msg[i].iov_base);
        }
    }
    /* free the iovecs we allocated */
    free((void *)msg);
    /* call the user callback function */
    cb_struct->user_callback(status, peer, user_iovec, count, tag, cb_struct->user_data);
    /* free the cb structure */
    free(cb_struct);
    return;
}

/*
 * Non-blocking version of mca_oob_recv_nb().
 *
 * @param peer (IN)    Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)  Callback function on recv completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error or number of bytes actually received.
 */
int mca_oob_recv_nb(ompi_process_name_t* peer, const struct iovec* msg, int count, int tag, int flags,
                    mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    return(mca_oob.oob_recv_nb(peer, msg, count, tag, flags, cbfunc, cbdata));
}

/*
 * Non-blocking version of mca_oob_recv_ntoh().
 *
 * @param peer (IN/OUT) Opaque name of peer process or MCA_OOB_BASE_ANY for wildcard receive.
 * @param msg (IN)      Array of iovecs describing user buffers and lengths.
 * @param types (IN)    Parallel array to iovecs describing data type of each iovec element.
 * @param count (IN)    Number of elements in iovec array.
 * @param flags (IN)    May be MCA_OOB_PEEK to return up to size bytes of msg w/out removing it from the queue,
 * @param cbfunc (IN)   Callback function on recv completion.
 * @param cbdata (IN)   User data that is passed to callback function.
 * @return              OMPI error code (<0) on error or number of bytes actually received.
 */

int mca_oob_recv_ntoh_nb(ompi_process_name_t* peer, const struct iovec* msg,
                         const mca_oob_base_type_t* types, int count, int tag, int flags,
                         mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    int rc, i = 0;
    struct iovec * orig;
    bool convert = false;
    struct mca_oob_base_cb_data_t * cb_struct;
    /* see if we actually have to convert anything */
    /* first check to see if we are already in network byte order */
    if(1 != htons(1)) {
        /* if we aren't, see if there is any data types that need to be converted */
        while(!convert && (i < count)) {
            if((types[i] == MCA_OOB_BASE_INT16) || (types[i] == MCA_OOB_BASE_INT32)) {
                convert = true;
            }
            i++;
        }
    }
    if(convert) {
        /* now if we need to convert anything we neeg to create a new iovec
         * to recieve into */
        orig = malloc(sizeof(struct iovec) * count);
        /* copy their iovecs */
        memcpy(orig, msg, sizeof(struct iovec) * count);
        cb_struct = malloc(sizeof(struct mca_oob_base_cb_data_t));
        cb_struct->user_data = cbdata;
        cb_struct->user_callback = cbfunc;
        cb_struct->user_iovec = msg;
        cb_struct->types = types;
        /* copy their array into ours */
        memcpy(orig, msg, sizeof(struct iovec) * count);
        /* now we need to go through the iovects, and any ints we need to
         * allocate our own space to recieve into, so we can convert into
         * their space later */
        for(i = 0; i < count; i++) {
            if((types[i] == MCA_OOB_BASE_INT16) || (types[i] == MCA_OOB_BASE_INT32)) {
                orig[i].iov_base = malloc(orig[i].iov_len);
            }
        }
        /* now the new buffers are ready. do the recieve */
        rc = mca_oob.oob_recv_nb(peer, orig, count, tag, flags, mca_oob_base_recv_cb, cb_struct);
    } else {
        rc = mca_oob.oob_recv_nb(peer, msg, count, tag, flags, cbfunc, cbdata);
    }
    return rc;
}

