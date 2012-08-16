/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/mca/event/event.h"
#include "opal/class/opal_hotel.h"
#include "opal/runtime/opal.h"
#include "opal/runtime/opal_progress.h"

#define NUM_OCC 200
#define NUM_RMS 128
#define NUM_CYCLES 10

static int num_evicted = 0;

typedef struct {
    int id;
    int room;
} occupant_t;

occupant_t occupants[NUM_OCC];
occupant_t *checked_out[NUM_OCC];

static void evict_cbfunc(opal_hotel_t *hotel, 
                         int room_num,
                         void *occupant_arg)
{
    int *occupant = (int*) occupant_arg;
    fprintf(stderr, "Room %d / occupant %d evicted!\n", *occupant, room_num);
    ++num_evicted;
}

int main(int argc, char* argv[])
{
    int rc;
    opal_hotel_t hotel;
    int i, j, rm;
    int num_occupied;

    if (0 > (rc = opal_init(&argc, &argv))) {
        fprintf(stderr, "orte_hotel: couldn't init opal - error code %d\n", rc);
        return rc;
    }
    
    OBJ_CONSTRUCT(&hotel, opal_hotel_t);
    opal_hotel_init(&hotel, NUM_RMS, 3000000, OPAL_EV_SYS_HI_PRI, evict_cbfunc);

    /* prep the occupants */
    for (i=0; i < NUM_OCC; i++) {
        occupants[i].id = i;
        occupants[i].room = -1;
    }

    /* arbitrarily checkin/checkout some things */
    for (i=0; i < NUM_RMS; i++) {
        if (OPAL_SUCCESS != opal_hotel_checkin(&hotel, 
                                               (void*)(&occupants[i]), &rm)) {
            fprintf(stderr, "Hotel is fully occupied\n");
            continue;
        }
        occupants[i].room = rm;
        fprintf(stderr, "Occupant %d checked into room %d\n", 
                occupants[i].id, rm);
    }
    num_occupied = NUM_RMS;
    fprintf(stderr, "---------------------------------------\n");

    /* cycle thru adding and removing some */
    for (i=0; i < NUM_CYCLES; i++) {
        for (j=0; j < 30; j++) {
            fprintf(stderr, "Checking occupant %d out of room %d\n", 
                    occupants[i + j].id, occupants[i + j].room);
            opal_hotel_checkout(&hotel, occupants[i + j].room);
            --num_occupied;
        }
        for (j=0; j < 30; j++) {
            if (OPAL_SUCCESS != 
                opal_hotel_checkin(&hotel, (void*) &(occupants[i + j]), &rm)) {
                fprintf(stderr, "Hotel is fully occupied\n");
                continue;
            }
            occupants[i + j].room = rm;
            fprintf(stderr, "Occupant %d checked into room %d\n", 
                    occupants[i + j].id, rm);
            ++num_occupied;
        }
        fprintf(stderr, "---------------------------------------\n");
    }

    /* sit here and see if we get an eviction notice */
    fprintf(stderr, "Waiting for %d evictions...\n", num_occupied);
    while (num_evicted < num_occupied) {
        opal_progress();
    }
    fprintf(stderr, "All occupants evicted!\n");

    OBJ_DESTRUCT(&hotel);

    opal_finalize();
    return 0;
}
