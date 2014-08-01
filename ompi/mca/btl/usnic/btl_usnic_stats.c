/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <unistd.h>
#include <stdlib.h>

#include "opal/util/output.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_pvar.h"

#include "ompi/proc/proc.h"

#include "btl_usnic.h"
#include "btl_usnic_module.h"
#include "btl_usnic_stats.h"

/*
 * Local variables
 */
static mca_base_var_type_t pvar_type = MCA_BASE_VAR_TYPE_MAX;

static inline void usnic_stats_reset(ompi_btl_usnic_module_t *module)
{
    int i;

    module->stats.num_total_sends =
        module->stats.num_resends =
        module->stats.num_chunk_sends =
        module->stats.num_frag_sends =
        module->stats.num_ack_recvs =

        module->stats.num_total_recvs =
        module->stats.num_unk_recvs =
        module->stats.num_dup_recvs =
        module->stats.num_oow_low_recvs =
        module->stats.num_oow_high_recvs =
        module->stats.num_frag_recvs =
        module->stats.num_chunk_recvs =
        module->stats.num_badfrag_recvs =
        module->stats.num_ack_sends =
        module->stats.num_recv_reposts =
        module->stats.num_crc_errors =

        module->stats.num_old_dup_acks =
        module->stats.num_dup_acks =
        module->stats.num_fast_retrans =
        module->stats.num_timeout_retrans =

        module->stats.max_sent_window_size =
        module->stats.max_rcvd_window_size =

        module->stats.pml_module_sends =
        module->stats.pml_send_callbacks =

        0;

    for (i=0; i<USNIC_NUM_CHANNELS; ++i) {
        module->mod_channels[i].num_channel_sends = 0;
    }
}

/* Prints a few terse statistics lines via opal_output(0,...).  The first
 * line will be prefixed with the string "prefix".  If "reset_stats" is true
 * then the statistics will be reset after printing.
 *
 * NOTE: this routine ignores the setting of stats_enable, so it can be used
 * for debugging routines even when normal stats reporting is not enabled.
 */
void ompi_btl_usnic_print_stats(
    ompi_btl_usnic_module_t *module,
    const char *prefix,
    bool reset_stats)
{
    char tmp[128], str[2048];

    /* The usuals */
    snprintf(str, sizeof(str), "%s:MCW:%3u, ST(P+D)/F/C/R(T+F)/A:%8lu(%8u+%8u)/%8lu/%8lu/%4lu(%4lu+%4lu)/%8lu, RcvTot/Chk/F/C/L/H/D/BF/A:%8lu/%c%c/%8lu/%8lu/%4lu+%2lu/%4lu/%4lu/%6lu OA/DA %4lu/%4lu CRC:%4lu ",
             prefix,
             ompi_proc_local()->proc_name.vpid,

             module->stats.num_total_sends,
             module->mod_channels[USNIC_PRIORITY_CHANNEL].num_channel_sends,
             module->mod_channels[USNIC_DATA_CHANNEL].num_channel_sends,
             module->stats.num_frag_sends,
             module->stats.num_chunk_sends,
             module->stats.num_resends,
             module->stats.num_timeout_retrans,
             module->stats.num_fast_retrans,
             module->stats.num_ack_sends,

             module->stats.num_total_recvs,
             (module->stats.num_total_recvs -
              module->stats.num_recv_reposts) == 0 ? 'g' : 'B',
             (module->stats.num_total_recvs -
              module->stats.num_frag_recvs -
              module->stats.num_chunk_recvs -
              module->stats.num_badfrag_recvs -
              module->stats.num_oow_low_recvs -
              module->stats.num_oow_high_recvs -
              module->stats.num_dup_recvs -
              module->stats.num_ack_recvs -
              module->stats.num_unk_recvs) == 0 ? 'g' : 'B',
             module->stats.num_frag_recvs,
             module->stats.num_chunk_recvs,
             module->stats.num_oow_low_recvs,
             module->stats.num_oow_high_recvs,
             module->stats.num_dup_recvs,
             module->stats.num_badfrag_recvs,
             module->stats.num_ack_recvs,

             module->stats.num_old_dup_acks,
             module->stats.num_dup_acks,

             module->stats.num_crc_errors);

    /* If our PML calls were 0, then show send and receive window
       extents instead */
    if (module->stats.pml_module_sends +
        module->stats.pml_send_callbacks == 0) {
        int64_t send_unacked, su_min = WINDOW_SIZE * 2, su_max = 0;
        int64_t recv_depth, rd_min = WINDOW_SIZE * 2, rd_max = 0;
        ompi_btl_usnic_endpoint_t *endpoint;
        opal_list_item_t *item;

        rd_min = su_min = WINDOW_SIZE * 2;
        rd_max = su_max = 0;

        opal_mutex_lock(&module->all_endpoints_lock);
        item = opal_list_get_first(&module->all_endpoints);
        while (item != opal_list_get_end(&(module->all_endpoints))) {
            endpoint = container_of(item, mca_btl_base_endpoint_t,
                    endpoint_endpoint_li);
            item = opal_list_get_next(item);

            /* Number of un-acked sends (i.e., sends for which we're
               still waiting for ACK) */
            send_unacked =
                endpoint->endpoint_next_seq_to_send -
                endpoint->endpoint_ack_seq_rcvd - 1;
            if (send_unacked > su_max) su_max = send_unacked;
            if (send_unacked < su_min) su_min = send_unacked;

            /* Receive window depth (i.e., difference between highest
               seq received and the next message we haven't ACKed
               yet) */
            recv_depth =
                endpoint->endpoint_highest_seq_rcvd -
                endpoint->endpoint_next_contig_seq_to_recv;
            if (recv_depth > rd_max) rd_max = recv_depth;
            if (recv_depth < rd_min) rd_min = recv_depth;
        }
        opal_mutex_unlock(&module->all_endpoints_lock);
        snprintf(tmp, sizeof(tmp), "PML S:%1ld, Win!A/R:%4ld/%4ld %4ld/%4ld",
                 module->stats.pml_module_sends,
                 su_min, su_max,
                 rd_min, rd_max);
    } else {
        snprintf(tmp, sizeof(tmp), "PML S/CB/Diff:%4lu/%4lu=%4ld",
                module->stats.pml_module_sends,
                module->stats.pml_send_callbacks,
                module->stats.pml_module_sends -
                 module->stats.pml_send_callbacks);
    }

    strncat(str, tmp, sizeof(str) - strlen(str) - 1);
    opal_output(0, "%s", str);

    if (reset_stats) {
        usnic_stats_reset(module);
    }
}

/*
 * Callback routine for libevent
 */
static void usnic_stats_callback(int fd, short flags, void *arg)
{
    ompi_btl_usnic_module_t *module = (ompi_btl_usnic_module_t*) arg;
    char tmp[128];

    if (!mca_btl_usnic_component.stats_enabled) {
        return;
    }

    snprintf(tmp, sizeof(tmp), "%4lu", ++module->stats.report_num);

    ompi_btl_usnic_print_stats(module, tmp,
                               /*reset=*/mca_btl_usnic_component.stats_relative);

    /* In OMPI v1.6, we have to re-add this event (because there's an
       old libevent in OMPI v1.6) */
    opal_event_add(&(module->stats.timer_event),
                   &(module->stats.timeout));
}

/*
 * Initialize usnic module statistics
 */
int ompi_btl_usnic_stats_init(ompi_btl_usnic_module_t *module)
{
    if (mca_btl_usnic_component.stats_enabled) {
        usnic_stats_reset(module);

        module->stats.timeout.tv_sec = mca_btl_usnic_component.stats_frequency;
        module->stats.timeout.tv_usec = 0;

        opal_event_set(opal_event_base, &(module->stats.timer_event),
                       -1, EV_TIMEOUT | EV_PERSIST,
                       &usnic_stats_callback, module);
        opal_event_add(&(module->stats.timer_event),
                       &(module->stats.timeout));
    }

    return OMPI_SUCCESS;
}

/*
 * Finalize usnic module statistics
 */
int ompi_btl_usnic_stats_finalize(ompi_btl_usnic_module_t *module)
{
    /* Disable the stats callback event, and then call the stats
       callback manually to display the final stats */
    if (mca_btl_usnic_component.stats_enabled) {
        opal_event_del(&(module->stats.timer_event));
        ompi_btl_usnic_print_stats(module, "final", /*reset_stats=*/false);
    }

    return OMPI_SUCCESS;
}

/************************************************************************/

/*
 * Function called by the pvar base upon MPI_T_pvar_handle_alloc,
 * handle_start, and handle_stop.
 */
static int usnic_pvar_notify(struct mca_base_pvar_t *pvar,
                             mca_base_pvar_event_t event,
                             void *obj, int *count)
{
    if (MCA_BASE_PVAR_HANDLE_BIND == event) {
        *count = mca_btl_usnic_component.num_modules;
    }

    /* Don't care about the other events */

    return OMPI_SUCCESS;
}


/*
 * Function called by the pvar base when a user wants to read the
 * value of an MPI_T performance variable.
 */
static int usnic_pvar_read(const struct mca_base_pvar_t *pvar,
                           void *value, void *bound_obj)
{
    size_t i;
    size_t offset = (size_t) pvar->ctx;
    uint64_t *array = (uint64_t*) value;

    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        char *base = (char*) &(mca_btl_usnic_component.usnic_active_modules[i]->stats);
        array[i] = *((uint64_t*) (base + offset));
    }

    return OMPI_SUCCESS;
}


/*
 * Register an MPI_T performance variable of type CLASS_HIGHWATERMARK.
 */
static void register_pvar_highwater(char *name, char *desc, size_t offset)
{
    int rc __opal_attribute_unused__;

    rc = mca_base_component_pvar_register(&mca_btl_usnic_component.super.btl_version,
                                          name, desc,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_PVAR_CLASS_HIGHWATERMARK,
                                          pvar_type,
                                          NULL, /* enumeration */
                                          MCA_BASE_VAR_BIND_NO_OBJECT,
                                          (MCA_BASE_PVAR_FLAG_READONLY |
                                           MCA_BASE_PVAR_FLAG_CONTINUOUS),
                                          usnic_pvar_read,
                                          NULL, /* write function */
                                          usnic_pvar_notify,
                                          (void *) offset);
    assert(rc >= 0);
}


/*
 * Function called by the pvar base when a user wants to read the
 * devices enum value.  The array is a simple list of 0..num_modules,
 * which will map to the strings in the devices_enum
 * setup_mpit_pvar_type().
 */
static int usnic_pvar_enum_read(const struct mca_base_pvar_t *pvar,
                                void *value, void *bound_obj)
{
    size_t i;
    int *array = (int *) value;

    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        array[i] = i;
    }

    return OMPI_SUCCESS;
}


/*
 * Register an MPI_T performance variable of type CLASS_COUNTER.
 */
static void register_pvar_counter(char *name, char *desc, size_t offset)
{
    int rc __opal_attribute_unused__;

    rc = mca_base_component_pvar_register(&mca_btl_usnic_component.super.btl_version,
                                          name, desc,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_PVAR_CLASS_COUNTER,
                                          pvar_type,
                                          NULL, /* enumeration */
                                          MCA_BASE_VAR_BIND_NO_OBJECT,
                                          (MCA_BASE_PVAR_FLAG_READONLY |
                                           MCA_BASE_PVAR_FLAG_CONTINUOUS),
                                          usnic_pvar_read,
                                          NULL, /* write function */
                                          usnic_pvar_notify,
                                          (void *) offset);
    assert(rc >= 0);
}


/*
 * Find the MPI_T type corresponding to our uint64_t counters and
 * highwatermarks.
 */
static bool setup_mpit_pvar_type(void)
{
    /* Our stats variables are uint64_t's, so find a pvar type that is
       compatible */
    if (sizeof(uint64_t) == sizeof(unsigned int)) {
        pvar_type = MCA_BASE_VAR_TYPE_UNSIGNED_INT;
    } else if (sizeof(uint64_t) == sizeof(unsigned long)) {
        pvar_type = MCA_BASE_VAR_TYPE_UNSIGNED_LONG;
#ifdef HAVE_UNSIGNED_LONG_LONG
    } else if (sizeof(uint64_t) == sizeof(unsigned long long)) {
        pvar_type = MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG;
#endif
    }

    /* Let the caller know if we found a compatible type or not */
    if (MCA_BASE_VAR_TYPE_MAX == pvar_type) {
        return false;
    }
    return true;
}


/*
 * Setup the usnic_X device enumeration pvar
 */
static void setup_mpit_pvars_enum(void)
{
    size_t i;
    int rc __opal_attribute_unused__;
    mca_base_var_enum_value_t *devices;
    static mca_base_var_enum_t *devices_enum;
    struct ibv_device *device;
    ompi_btl_usnic_module_t *m;
    unsigned char *c;

    devices = calloc(mca_btl_usnic_component.num_modules + 1,
                     sizeof(*devices));
    assert(devices != NULL);

    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        char *str;

        m = mca_btl_usnic_component.usnic_active_modules[i];
        c = (unsigned char*) &m->if_ipv4_addr;

        device = m->device;
        devices[i].value = i;
        rc = asprintf(&str, "%s,%s,%hhu.%hhu.%hhu.%hhu/%" PRIu32,
                 ibv_get_device_name(device),
                 m->if_name,
                 c[0], c[1], c[2], c[3],
                 m->if_cidrmask);
        assert(rc > 0);
        devices[i].string = str;
    }
    devices[i].string = NULL;

    rc = mca_base_var_enum_create("btl_usnic", devices, &devices_enum);
    assert(OPAL_SUCCESS == rc);

    rc = mca_base_component_pvar_register(&mca_btl_usnic_component.super.btl_version,
                                          "devices",
                                          "Enumeration representing which slot in btl_usnic_* MPI_T pvar value arrays correspond to which usnic_X Linux device",
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_PVAR_CLASS_STATE,
                                          MCA_BASE_VAR_TYPE_INT,
                                          devices_enum,
                                          MCA_BASE_VAR_BIND_NO_OBJECT,
                                          (MCA_BASE_PVAR_FLAG_READONLY |
                                           MCA_BASE_PVAR_FLAG_CONTINUOUS),
                                          usnic_pvar_enum_read,
                                          NULL, /* write function */
                                          usnic_pvar_notify,
                                          NULL /* context */);
    assert(rc >= 0);

    /* Free the strings (mca_base_var_enum_create() strdup()'ed them
       into private storage, so we don't need them any more) */
    for (i = 0; i < mca_btl_usnic_component.num_modules; ++i) {
        free((char*) devices[i].string);
    }    

    /* The devices_enum has been RETAIN'ed by the pvar, so we can
       RELEASE it here, and the enum will be destroyed when the pvar
       is destroyed. */
    OBJ_RELEASE(devices_enum);
}


/*
 * Setup high watermark MPI_T performance variables
 */
static void setup_mpit_pvars_highwatermark(void)
{
#define REGISTERHW(field, desc) \
    register_pvar_highwater(#field, (desc), offsetof(ompi_btl_usnic_module_stats_t, field))

    REGISTERHW(max_sent_window_size,
               "Maximum number of entries in all send windows from this peer");
    REGISTERHW(max_rcvd_window_size,
               "Maximum number of entries in all receive windows to this peer");
}


/*
 * Setup counter MPI_T performance variables
 */
static void setup_mpit_pvars_counters(void)
{
#define REGISTERC(field, desc) \
    register_pvar_counter(#field, (desc), offsetof(ompi_btl_usnic_module_stats_t, field))

    REGISTERC(num_total_sends,
              "Total number of sends (MPI data, ACKs, retransmissions, etc.)");
    REGISTERC(num_resends,
              "Total number of all retransmissions");
    REGISTERC(num_timeout_retrans,
              "Number of times chunk retransmissions have occured because an ACK was not received within the timeout");
    REGISTERC(num_fast_retrans,
              "Number of times chunk retransmissions have occured because due to a repeated ACK");
    REGISTERC(num_chunk_sends,
              "Number of sends that were part of a larger MPI message fragment (i.e., the MPI message was so long that it had to be split into multiple MTU/network sends)");
    REGISTERC(num_frag_sends,
              "Number of sends where the entire MPI message fragment fit into a single MTU/network send");
    REGISTERC(num_ack_sends,
              "Number of ACKs sent (i.e., usNIC-BTL-to-usNIC-BTL control messages)");

    REGISTERC(num_total_recvs,
              "Total number of receives completed");
    REGISTERC(num_unk_recvs,
              "Number of receives with an unknown source or type, and therefore ignored by the usNIC BTL (this should never be >0)");
    REGISTERC(num_dup_recvs,
              "Number of duplicate receives");
    REGISTERC(num_oow_low_recvs,
              "Number of times a receive was out of the sliding window (on the low side)");
    REGISTERC(num_oow_high_recvs,
              "Number of times a receive was out of the sliding window (on the high side)");
    REGISTERC(num_frag_recvs,
              "Number of receives where the entire MPI message fragment fit into a single MTU/network send");
    REGISTERC(num_chunk_recvs,
              "Number of receives that were part of a larger MPI message fragment (i.e., this receive was reassembled into a larger MPI message fragment)");
    REGISTERC(num_badfrag_recvs,
              "Number of chunks received that had a bad fragment ID (this should never be >0)");

    REGISTERC(num_ack_recvs,
              "Total number of ACKs received");
    REGISTERC(num_old_dup_acks,
              "Number of old duplicate ACKs received (i.e., before the current expected ACK)");
    REGISTERC(num_dup_acks,
              "Number of duplicate ACKs received (i.e., the current expected ACK)");

    REGISTERC(num_recv_reposts,
              "Number of times buffers have been reposted for receives");
    REGISTERC(num_crc_errors,
              "Number of times receives were aborted because of a CRC error");

    REGISTERC(pml_module_sends,
              "Number of times the PML has called down to send a message");
    REGISTERC(pml_send_callbacks,
              "Number of times the usNIC BTL has called up to the PML to complete a send");
}


/*
 * Initialize MPI_T performance variables
 */
int ompi_btl_usnic_setup_mpit_pvars(void)
{
    /* If we cannot find a compatible pvar type, we're done (i.e.,
       don't register any pvars) */
    if (!setup_mpit_pvar_type()) {
        return OMPI_SUCCESS;
    }

    /* Setup the usnic_X device enumeration pvar */
    setup_mpit_pvars_enum();

    /* Register watermark pvars */
    setup_mpit_pvars_highwatermark();

    /* If our counter stats are relative, don't report them through
       MPI_T, because MPI_T expects counters to be monotonically
       rising. */
    if (!mca_btl_usnic_component.stats_relative) {
        setup_mpit_pvars_counters();
    }

    /* All done */
    return OMPI_SUCCESS;
}
