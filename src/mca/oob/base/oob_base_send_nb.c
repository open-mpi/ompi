#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include <string.h>
#include <netinet/in.h>

struct mca_oob_base_cb_data_t {
    mca_oob_callback_fn_t user_callback;
    void * user_data;
    const mca_oob_base_type_t * types;
    const struct iovec * user_iovec;
};

/* this is the callback function we will register when we have to do any conversion */
static void mca_oob_base_send_cb(
    int status, 
    const ompi_process_name_t* peer,
    const struct iovec* msg,
    int count, 
    int tag, 
    void* cbdata)
{
    int i;
    struct mca_oob_base_cb_data_t * cb_struct = (struct mca_oob_base_cb_data_t *) cbdata;
    mca_oob_base_type_t * types = cb_struct->types;
    for(i = 0; i < count; i++) {
        if((types[i] == MCA_OOB_BASE_INT16) || (types[i] == MCA_OOB_BASE_INT32)) {
            free(msg[i].iov_base);
        }
    }
    free((void *)msg);
    /* call the user callback function */
    cb_struct->user_callback(status, peer, cb_struct->user_iovec, count, tag, cb_struct->user_data);
    free(cb_struct);
    return;
}

    

/*
 * Non-blocking version of mca_oob_send().
 *
 * @param peer (IN)    Opaque name of peer process.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   Currently unused.
 * @param cbfunc (IN)  Callback function on send completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error number of bytes actually sent.
 *
 */

int mca_oob_send_nb(const ompi_process_name_t* peer, const struct iovec* msg, int count, int tag,
                    int flags, mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    return(mca_oob.oob_send_nb(peer, msg, count, tag, flags, cbfunc, cbdata));
}

/*
 * Non-blocking version of mca_oob_send_hton().
 *
 * @param peer (IN)    Opaque name of peer process.
 * @param msg (IN)     Array of iovecs describing user buffers and lengths.
 * @param types (IN)   Parallel array to iovecs describing data type of each iovec element.
 * @param count (IN)   Number of elements in iovec array.
 * @param flags (IN)   Currently unused.
 * @param cbfunc (IN)  Callback function on send completion.
 * @param cbdata (IN)  User data that is passed to callback function.
 * @return             OMPI error code (<0) on error number of bytes actually sent.
 */

int mca_oob_send_hton_nb(const ompi_process_name_t* peer, const struct iovec* msg,
                         const mca_oob_base_type_t* types, int count, int tag, int flags,
                         mca_oob_callback_fn_t cbfunc, void* cbdata)
{
    int rc, i = 0;
    struct iovec * converted;
    struct mca_oob_base_cb_data_t * cb_struct;
    bool convert = false;
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
        converted = malloc(sizeof(struct iovec) * count);
        /* copy the passed iovect into the new one */
        memcpy(converted, msg, sizeof(struct iovec) * count);
        cb_struct = malloc(sizeof(struct mca_oob_base_cb_data_t));
        cb_struct->user_data = cbdata;
        cb_struct->user_callback = cbfunc;
        cb_struct->user_iovec = msg;
        cb_struct->types = types;
        for(i = 0; i < count; i++) {
            if(types[i] == MCA_OOB_BASE_INT16) {
                /* figure out how many integers we have */
                rc = msg[i].iov_len / 2;
                /* allocate a buffer for the converted data */
                converted[i].iov_base = malloc(msg[i].iov_len);
                /* pack the data */
                mca_oob_base_pack(converted[i].iov_base, msg[i].iov_base, rc, MCA_OOB_BASE_INT16);
            } else if(types[i] == MCA_OOB_BASE_INT32) {
                /* figure out how many integers we have */
                rc = msg[i].iov_len / 4;
                /* allocate a buffer for the converted data */
                converted[i].iov_base = malloc(msg[i].iov_len);
                /* pack the data */
                mca_oob_base_pack(converted[i].iov_base, msg[i].iov_base, rc, MCA_OOB_BASE_INT32);
            }
        }
        rc = mca_oob.oob_send_nb(peer, converted, count, tag, flags, mca_oob_base_send_cb, cb_struct);
    } else {
        rc = mca_oob.oob_send_nb(peer, msg, count, tag, flags, cbfunc, cbdata);
    }
    return rc;                       
}

