/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_PORTALS4_MESSAGE_H
#define MTL_PORTALS4_MESSAGE_H

struct ompi_mtl_portals4_message_t {
    opal_free_list_item_t super;
    ptl_event_t ev;
    void *buffer;
};
typedef struct ompi_mtl_portals4_message_t ompi_mtl_portals4_message_t;
OBJ_CLASS_DECLARATION(ompi_mtl_portals4_message_t);


static inline ompi_mtl_portals4_message_t*
ompi_mtl_portals4_message_alloc(const ptl_event_t *ev)
{
    opal_free_list_item_t *tmp;
    ompi_mtl_portals4_message_t* message;

    tmp = opal_free_list_get (&ompi_mtl_portals4.fl_message);
    if (NULL == tmp) return NULL;

    message = (ompi_mtl_portals4_message_t*) tmp;

    message->ev = *ev;

    if (0 == ev->mlength) {
        message->buffer = NULL;
    } else {
        /* once we've finished processing the event, an AUTO_FREE
           event might be next, rendering the data in ev.start
           invalid.  Copy it away... */
        memcpy(message->buffer, ev->start, ev->mlength);
        message->ev.start = message->buffer;
    }

    return message;
}

static inline void
ompi_mtl_portals4_message_free(ompi_mtl_portals4_message_t *message)
{
    opal_free_list_return (&ompi_mtl_portals4.fl_message,
                           &message->super);
}

#endif
