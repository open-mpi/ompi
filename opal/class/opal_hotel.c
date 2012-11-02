/*
 * Copyright (c) 2012      Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC. All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <stddef.h>

#include "opal/mca/event/event.h"
#include "opal/class/opal_hotel.h"


static void local_eviction_callback(int fd, short flags, void *arg)
{
    opal_hotel_room_eviction_callback_arg_t *eargs = 
        (opal_hotel_room_eviction_callback_arg_t*) arg;
    void *occupant = eargs->hotel->rooms[eargs->room_num].occupant;

    /* Remove the occupant from the room and invoke the user callback
       to tell them that they were evicted */
    opal_hotel_checkout(eargs->hotel, eargs->room_num);
    eargs->hotel->evict_callback_fn(eargs->hotel,
                                    eargs->room_num,
                                    occupant);
}


int opal_hotel_init(opal_hotel_t *h, int num_rooms,
                    uint32_t eviction_timeout,
                    int eviction_event_priority,
                    opal_hotel_eviction_callback_fn_t evict_callback_fn)
{
    int i;

    /* Bozo check */
    if (num_rooms <= 0 ||
        NULL == evict_callback_fn) {
        return OPAL_ERR_BAD_PARAM;
    }

    h->num_rooms = num_rooms;
    h->eviction_timeout.tv_usec = eviction_timeout % 1000000;
    h->eviction_timeout.tv_sec = eviction_timeout / 1000000;
    h->evict_callback_fn = evict_callback_fn;
    h->rooms = (opal_hotel_room_t*)malloc(num_rooms * sizeof(opal_hotel_room_t));
    if (NULL != evict_callback_fn) {
        h->eviction_args = 
            (opal_hotel_room_eviction_callback_arg_t*)malloc(num_rooms * sizeof(opal_hotel_room_eviction_callback_arg_t));
    }
    h->unoccupied_rooms = (int*) malloc(num_rooms * sizeof(int));
    h->last_unoccupied_room = num_rooms - 1;

    for (i = 0; i < num_rooms; ++i) {
        /* Mark this room as unoccupied */
        h->rooms[i].occupant = NULL;

        /* Setup this room in the unoccupied index array */
        h->unoccupied_rooms[i] = i;

        /* Setup the eviction callback args */
        h->eviction_args[i].hotel = h;
        h->eviction_args[i].room_num = i;

        /* Create this room's event (but don't add it) */
        opal_event_set(opal_event_base,
                       &(h->rooms[i].eviction_timer_event),
                       -1, 0, local_eviction_callback,
                       &(h->eviction_args[i]));

        /* Set the priority so it gets serviced properly */
        opal_event_set_priority(&(h->rooms[i].eviction_timer_event),
                                eviction_event_priority);
    }

    return OPAL_SUCCESS;
}

static void constructor(opal_hotel_t *h)
{
    h->num_rooms = 0;
    h->eviction_timeout.tv_sec = 0;
    h->eviction_timeout.tv_usec = 0;
    h->evict_callback_fn = NULL;
    h->rooms = NULL;
    h->eviction_args = NULL;
    h->unoccupied_rooms = NULL;
    h->last_unoccupied_room = -1;
}

static void destructor(opal_hotel_t *h)
{
    int i;

    /* Go through all occupied rooms and destroy their events */
    for (i = 0; i < h->num_rooms; ++i) {
        if (NULL != h->rooms[i].occupant) {
            opal_event_del(&(h->rooms[i].eviction_timer_event));
        }
    }

    if (NULL != h->rooms) {
        free(h->rooms);
    }
    if (NULL != h->eviction_args) {
        free(h->eviction_args);
    }
    if (NULL != h->unoccupied_rooms) {
        free(h->unoccupied_rooms);
    }
}

OBJ_CLASS_INSTANCE(opal_hotel_t,
                   opal_object_t,
                   constructor,
                   destructor);
