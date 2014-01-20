/*
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <stdio.h>
#include <math.h>
#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef ORTE_SIGAR_LINUX
#include <sigar.h>
#else
#include <libsigar-universal-macosx>
#endif

#include "opal_stdint.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_ring_buffer.h"
#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/mca/pstat/pstat.h"
#include "opal/mca/event/event.h"
#include "opal/mca/db/db.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_globals.h"
#include "orte/orted/orted.h"

#include "orte/mca/sensor/base/base.h"
#include "orte/mca/sensor/base/sensor_private.h"
#include "sensor_sigar.h"

/* declare the API functions */
static int init(void);
static void finalize(void);
static void start(orte_jobid_t job);
static void stop(orte_jobid_t job);
static void sigar_sample(void);
static void sigar_log(opal_buffer_t *buf);

/* instantiate the module */
orte_sensor_base_module_t orte_sensor_sigar_module = {
    init,
    finalize,
    start,
    stop,
    sigar_sample,
    sigar_log
};

/* define some local classes */
typedef struct {
    opal_list_item_t super;
    char *interface;
    uint64_t rx_packets;
    uint64_t rx_bytes;
    uint64_t tx_packets;
    uint64_t tx_bytes;
} sensor_sigar_interface_t;
static void sit_cons(sensor_sigar_interface_t *sit)
{
    sit->interface = NULL;
    sit->rx_packets = 0;
    sit->rx_bytes = 0;
    sit->tx_packets = 0;
    sit->tx_bytes = 0;
}
static void sit_dest(sensor_sigar_interface_t *sit)
{
    if (NULL != sit->interface) {
        free(sit->interface);
    }
}
OBJ_CLASS_INSTANCE(sensor_sigar_interface_t,
                   opal_list_item_t,
                   sit_cons, sit_dest);

typedef struct {
    opal_list_item_t super;
    char *mount_pt;
    uint64_t reads;
    uint64_t writes;
    uint64_t read_bytes;
    uint64_t write_bytes;
} sensor_sigar_disks_t;
static void dit_cons(sensor_sigar_disks_t *dit)
{
    dit->mount_pt = NULL;
    dit->reads = 0;
    dit->writes = 0;
    dit->read_bytes = 0;
    dit->write_bytes = 0;
}
static void dit_dest(sensor_sigar_disks_t *dit)
{
    if (NULL != dit->mount_pt) {
        free(dit->mount_pt);
    }
}
OBJ_CLASS_INSTANCE(sensor_sigar_disks_t,
                   opal_list_item_t,
                   dit_cons, dit_dest);

static sigar_t *sigar;
static opal_list_t fslist;
static opal_list_t netlist;
static time_t last_sample = 0;
static struct cpu_data_t {
    uint64_t user;
    uint64_t nice;
    uint64_t sys;
    uint64_t idle;
    uint64_t wait;
    uint64_t total;
} pcpu;
static struct swap_data_t {
    uint64_t page_in;
    uint64_t page_out;
} pswap;
static bool log_enabled = true;
static opal_buffer_t test_vector;

static uint64_t metric_diff_calc(sigar_uint64_t newval, uint64_t oldval,
                                 const char *name_for_log,
                                 const char* value_name_for_log);
static void generate_test_vector(opal_buffer_t *v);

static int init(void)
{
    sigar_file_system_list_t sigar_fslist;
    sigar_net_interface_list_t sigar_netlist;
    sensor_sigar_disks_t *dit;
    sensor_sigar_interface_t *sit;
    unsigned int i;

    if (mca_sensor_sigar_component.test) {
        /* generate test vector */
        OBJ_CONSTRUCT(&test_vector, opal_buffer_t);
        generate_test_vector(&test_vector);
        return ORTE_SUCCESS;
    }

    /* setup the globals */
    OBJ_CONSTRUCT(&fslist, opal_list_t);
    OBJ_CONSTRUCT(&netlist, opal_list_t);
    pcpu.user = 0;
    pcpu.nice = 0;
    pcpu.sys = 0;
    pcpu.idle = 0;
    pcpu.wait = 0;
    pcpu.total = 0;
    pswap.page_in = 0;
    pswap.page_out = 0;

    /* initialize sigar */
    if (0 != sigar_open(&sigar)) {
        opal_output(0, "%s: sigar_open failed on node %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    orte_process_info.nodename);
        return ORTE_ERROR;
    }

    /* load the disk list */
    if (0 != sigar_file_system_list_get(sigar, &sigar_fslist)) {
        return ORTE_ERROR;
    }
    for (i = 0; i < sigar_fslist.number; i++) {
        if (sigar_fslist.data[i].type == SIGAR_FSTYPE_LOCAL_DISK || sigar_fslist.data[i].type == SIGAR_FSTYPE_NETWORK) {
            dit = OBJ_NEW(sensor_sigar_disks_t);
            dit->mount_pt = strdup(sigar_fslist.data[i].dir_name);
            opal_list_append(&fslist, &dit->super);
        }
    }
    sigar_file_system_list_destroy(sigar, &sigar_fslist);

    /* load the list of network interfaces */
    if (0 != sigar_net_interface_list_get(sigar, &sigar_netlist)) {
        return ORTE_ERROR;
    }
    for (i=0; i < sigar_netlist.number; i++) {
        sit = OBJ_NEW(sensor_sigar_interface_t);
        sit->interface = strdup(sigar_netlist.data[i]);
        opal_list_append(&netlist, &sit->super);
    }
    sigar_net_interface_list_destroy(sigar, &sigar_netlist);

    return ORTE_SUCCESS;
}

static void finalize(void)
{
    opal_list_item_t *item;

    if (mca_sensor_sigar_component.test) {
        /* destruct test vector */
        OBJ_DESTRUCT(&test_vector);
        return;
    }

    if (NULL != sigar) {
        sigar_close(sigar);
    }
    while (NULL != (item = opal_list_remove_first(&fslist))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&fslist);
    while (NULL != (item = opal_list_remove_first(&netlist))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&netlist);

    return;
}

/*
 * Start monitoring of local processes
 */
static void start(orte_jobid_t jobid)
{
    return;
}


static void stop(orte_jobid_t jobid)
{
    return;
}

static void sigar_sample(void)
{
    sigar_mem_t mem;
    sigar_swap_t swap;
    sigar_cpu_t cpu;
    sigar_loadavg_t loadavg;
    sigar_disk_usage_t tdisk;
    sensor_sigar_disks_t *dit;
    sigar_file_system_usage_t fsusage;
    sensor_sigar_interface_t *sit;
    sigar_net_interface_stat_t tnet, ifc;
    uint64_t reads, writes, read_bytes, write_bytes;
    uint64_t rxpkts, txpkts, rxbytes, txbytes;
    uint64_t ui64;
    opal_buffer_t data, *bptr;
    int rc;
    time_t now;
    double cpu_diff, tdiff;
    float tmp;
    char *ctmp;
    char time_str[40];
    char *timestamp_str;

    if (mca_sensor_sigar_component.test) {
        /* just send the test vector */
        bptr = &test_vector;
        opal_dss.pack(orte_sensor_base.samples, &bptr, 1, OPAL_BUFFER);
        return;
    }

    /* prep the buffer to collect the data */
    OBJ_CONSTRUCT(&data, opal_buffer_t);
    /* pack our name */
    ctmp = strdup("sigar");
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &ctmp, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    free(ctmp);
    /* include our node name */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &orte_process_info.nodename, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }

    /* get the sample time */
    now = time(NULL);
    tdiff = difftime(now, last_sample);
    /* pass the time along as a simple string */
    strftime(time_str, sizeof(time_str), "%F %T%z", localtime(&now));
    asprintf(&timestamp_str, "%s", time_str);

    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &timestamp_str, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    free(timestamp_str);

    /* get the memory usage for this node */
    memset(&mem, 0, sizeof(mem));
    sigar_mem_get(sigar, &mem);
    opal_output_verbose(1, orte_sensor_base_framework.framework_output,
                        "mem total: %" PRIu64 " used: %" PRIu64 " actual used: %" PRIu64 " actual free: %" PRIu64 "",
                        mem.total, mem.used, mem.actual_used, mem.actual_free);
    /* add it to the data */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &mem.total, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &mem.used, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &mem.actual_used, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &mem.actual_free, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }

    /* get swap data */
    memset(&swap, 0, sizeof(swap));
    sigar_swap_get(sigar, &swap);
    opal_output_verbose(1, orte_sensor_base_framework.framework_output,
                        "swap total: %" PRIu64 " used: %" PRIu64 "page_in: %" PRIu64 " page_out: %" PRIu64 "\n",
                        swap.total, swap.used, swap.page_in, swap.page_out);
    /* compute the values we actually want and add them to the data */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &swap.total, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &swap.used, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    ui64 = swap.page_in - pswap.page_in;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &ui64, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    ui64 = swap.page_out - pswap.page_out;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &ui64, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }

    /* get the cpu usage */
    memset(&cpu, 0, sizeof(cpu));
    sigar_cpu_get(sigar, &cpu);
    opal_output_verbose(1, orte_sensor_base_framework.framework_output,
                        "cpu user: %" PRIu64 " sys: %" PRIu64 " idle: %" PRIu64 " wait: %" PRIu64 " nice: %" PRIu64 " total: %" PRIu64 "", 
			cpu.user, cpu.sys, cpu.idle, cpu.wait, cpu.nice, cpu.total);
    /* compute the values we actually want and add them to the data */
    cpu_diff = (double)(cpu.total - pcpu.total);
    tmp = (float)((cpu.user - pcpu.user) * 100.0 / cpu_diff) + (float)((cpu.nice - pcpu.nice) * 100.0 / cpu_diff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &tmp, 1, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    tmp = ((float) (cpu.sys - pcpu.sys) * 100.0 / cpu_diff) + ((float)((cpu.wait - pcpu.wait) * 100.0 / cpu_diff));
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &tmp, 1, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    tmp = (float) (cpu.idle - pcpu.idle) * 100.0 / cpu_diff;
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &tmp, 1, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    /* update the values */
    pcpu.user = cpu.user;
    pcpu.nice = cpu.nice;
    pcpu.sys = cpu.sys;
    pcpu.wait = cpu.wait;
    pcpu.idle = cpu.idle;
    pcpu.total = cpu.total;

    /* get load average data */
    memset(&loadavg, 0, sizeof(loadavg));
    sigar_loadavg_get(sigar, &loadavg);
    opal_output_verbose(1, orte_sensor_base_framework.framework_output,
                        "load_avg: %e %e %e",
                        loadavg.loadavg[0], loadavg.loadavg[1], loadavg.loadavg[2]);
    /* add them to the data */
    tmp = (float)loadavg.loadavg[0];
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &tmp, 1, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    tmp = (float)loadavg.loadavg[1];
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &tmp, 1, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    tmp = (float)loadavg.loadavg[2];
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &tmp, 1, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }

    /* get disk usage data */
    memset(&tdisk, 0, sizeof(tdisk));
    OPAL_LIST_FOREACH(dit, &fslist, sensor_sigar_disks_t) {
        if (0 != sigar_file_system_usage_get(sigar, dit->mount_pt, &fsusage)) {
            opal_output(0, "%s Failed to get usage data for filesystem %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), dit->mount_pt);
        } else {
            opal_output_verbose(1, orte_sensor_base_framework.framework_output,
                                "FileSystem: %s Reads: %" PRIu64 " Writes: %" PRIu64 " ReadBytes: %" PRIu64 " WriteBytes: %" PRIu64 "", 
                                dit->mount_pt, fsusage.disk.reads, fsusage.disk.writes, fsusage.disk.read_bytes, fsusage.disk.write_bytes);
            /* compute the number of reads since last reading */
            reads = metric_diff_calc(fsusage.disk.reads, dit->reads, dit->mount_pt, "disk reads");
            dit->reads = fsusage.disk.reads;  /* old = new */
            /* compute the number of writes since last reading */
            writes = metric_diff_calc(fsusage.disk.writes, dit->writes, dit->mount_pt, "disk writes");
            dit->writes = fsusage.disk.writes;  /* old = new */
            /* compute the number of read bytes since last reading */
            read_bytes = metric_diff_calc(fsusage.disk.read_bytes, dit->read_bytes, dit->mount_pt, "disk read bytes");
            dit->read_bytes = fsusage.disk.read_bytes;  /* old = new */
            /* compute the number of bytes written since last reading */
            write_bytes = metric_diff_calc(fsusage.disk.write_bytes, dit->write_bytes, dit->mount_pt, "disk write bytes");
            dit->write_bytes = fsusage.disk.write_bytes;  /* old = new */
            opal_output_verbose(4, orte_sensor_base_framework.framework_output,
                                "FileSystem: %s ReadsChange: %" PRIu64 " WritesChange: %" PRIu64 " ReadBytesChange: %" PRIu64 " WriteBytesChange: %" PRIu64 "", 
                                dit->mount_pt, reads, writes, read_bytes, write_bytes);
            /* accumulate the values */
            tdisk.reads += reads;
            tdisk.writes += writes;
            tdisk.read_bytes += read_bytes;
            tdisk.write_bytes += write_bytes;
        }
    }
    opal_output_verbose(4, orte_sensor_base_framework.framework_output,
                        "Totals: ReadsChange: %" PRIu64 " WritesChange: %" PRIu64 " ReadBytesChange: %" PRIu64 " WriteBytesChange: %" PRIu64 "", 
                        tdisk.reads, tdisk.writes, tdisk.read_bytes, tdisk.write_bytes);
    /* compute the values we actually want and add them to the data */
    reads = (uint64_t)ceil((double)tdisk.reads/tdiff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &reads, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    writes = (uint64_t)ceil((double)tdisk.writes/tdiff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &writes, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    read_bytes = (uint64_t)ceil((double)tdisk.read_bytes/tdiff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &read_bytes, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    write_bytes = (uint64_t)ceil((double)tdisk.write_bytes/tdiff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &write_bytes, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }

    /* get network usage data */
    memset(&tnet, 0, sizeof(tnet));
    OPAL_LIST_FOREACH(sit, &netlist, sensor_sigar_interface_t) {
        memset(&ifc, 0, sizeof(ifc));
        if (0 != sigar_net_interface_stat_get(sigar, sit->interface, &ifc)) {
            opal_output(0, "%s Failed to get usage data for interface %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), sit->interface);
        } else {
            opal_output_verbose(1, orte_sensor_base_framework.framework_output,
                                "Interface: %s RecvdPackets: %" PRIu64 " RecvdBytes: %" PRIu64 " TransPackets: %" PRIu64 " TransBytes: %" PRIu64 "", 
                                sit->interface, ifc.rx_packets, ifc.rx_bytes, ifc.tx_packets, ifc.tx_bytes);
            /* compute the number of recvd packets since last reading */
            rxpkts = metric_diff_calc(ifc.rx_packets, sit->rx_packets, sit->interface, "rx packets");
            sit->rx_packets = ifc.rx_packets;  /* old = new */
            /* compute the number of transmitted packets since last reading */
            txpkts = metric_diff_calc(ifc.tx_packets, sit->tx_packets, sit->interface, "tx packets");
            sit->tx_packets = ifc.tx_packets;  /* old = new */
            /* compute the number of recvd bytes since last reading */
            rxbytes = metric_diff_calc(ifc.rx_bytes, sit->rx_bytes, sit->interface, "rx bytes");
            sit->rx_bytes = ifc.rx_bytes;  /* old = new */
            /* compute the number of transmitted bytes since last reading */
            txbytes = metric_diff_calc(ifc.tx_bytes, sit->tx_bytes, sit->interface, "tx bytes");
            sit->tx_bytes = ifc.tx_bytes;  /* old = new */
            opal_output_verbose(4, orte_sensor_base_framework.framework_output,
                                "Interface: %s RxPkts: %" PRIu64 " TxPkts: %" PRIu64 " RxBytes: %" PRIu64 " TxBytes: %" PRIu64 "", 
                                sit->interface, rxpkts, txpkts, rxbytes, txbytes);
            /* accumulate the values */
            tnet.rx_packets += rxpkts;
            tnet.rx_bytes += rxbytes;
            tnet.tx_packets += txpkts;
            tnet.tx_bytes += txbytes;
        }
    }
    opal_output_verbose(4, orte_sensor_base_framework.framework_output,
                        "Totals: RxPkts: %" PRIu64 " TxPkts: %" PRIu64 " RxBytes: %" PRIu64 " TxBytes: %" PRIu64 "", 
                        tnet.rx_packets, tnet.tx_packets, tnet.rx_bytes, tnet.tx_bytes);
    /* compute the values we actually want and add them to the data */
    rxpkts = (uint64_t)ceil((double)tnet.rx_packets/tdiff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &rxpkts, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    txpkts = (uint64_t)ceil((double)tnet.tx_packets/tdiff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &txpkts, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    rxbytes = (uint64_t)ceil((double)tnet.rx_bytes/tdiff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &rxbytes, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }
    txbytes = (uint64_t)ceil((double)tnet.tx_bytes/tdiff);
    if (OPAL_SUCCESS != (rc = opal_dss.pack(&data, &txbytes, 1, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&data);
        return;
    }

    /* xfer the data for transmission - need at least one prior sample before doing so */
    if (0 < last_sample) {
        bptr = &data;
        if (OPAL_SUCCESS != (rc = opal_dss.pack(orte_sensor_base.samples, &bptr, 1, OPAL_BUFFER))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&data);
            return;
        }
    }
    OBJ_DESTRUCT(&data);

    last_sample = now;
}

static void sigar_log(opal_buffer_t *sample)
{
    char *hostname;
    char *sampletime;
    int rc;
    int32_t n;
    opal_value_t kv[24];
    uint64_t uint64;
    float fval;
    int i;

    if (!log_enabled) {
        return;
    }

    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &hostname, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    opal_output_verbose(3, orte_sensor_base_framework.framework_output,
                        "%s Received log from host %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == hostname) ? "NULL" : hostname);

    /* prep the xfr storage */
    for (i=0; i < 24; i++) {
        OBJ_CONSTRUCT(&kv[i], opal_value_t);
    }

    /* unpack the incoming data and xfer it for storage */
    i=0;

    /* sample time */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &sampletime, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("ctime");
    kv[i].type = OPAL_STRING;
    kv[i++].data.string = strdup(sampletime);
    free(sampletime);

    /* hostname */
    kv[i].key = strdup("hostname");
    kv[i].type = OPAL_STRING;
    kv[i++].data.string = strdup(hostname);

    /* total memory */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("mem_total");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* total used memory */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("mem_used");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* actual used memory */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("mem_actual_used");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* actual free memory */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("mem_actual_free");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* total swap memory */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("swap_total");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* swap used */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("swap_used");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* swap pages in */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("swap_page_in");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* swap pages out */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("swap_page_out");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* cpu user */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &fval, &n, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("cpu_user");
    kv[i].type = OPAL_FLOAT;
    kv[i++].data.fval = fval;

    /* cpu sys */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &fval, &n, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("cpu_sys");
    kv[i].type = OPAL_FLOAT;
    kv[i++].data.fval = fval;

    /* cpu idle */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &fval, &n, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("cpu_idle");
    kv[i].type = OPAL_FLOAT;
    kv[i++].data.fval = fval;

    /* la0 */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &fval, &n, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("load0");
    kv[i].type = OPAL_FLOAT;
    kv[i++].data.fval = fval;

    /* la5 */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &fval, &n, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("load1");
    kv[i].type = OPAL_FLOAT;
    kv[i++].data.fval = fval;

    /* la15 */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &fval, &n, OPAL_FLOAT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("load2");
    kv[i].type = OPAL_FLOAT;
    kv[i++].data.fval = fval;

    /* disk read ops rate */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("disk_ro_rate");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* disk write ops rate */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("disk_wo_rate");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* disk read bytes/sec */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("disk_rb_rate");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* disk write bytes/sec */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("disk_wb_rate");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* net recv packet rate */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("net_rp_rate");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* net tx packet rate */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("net_wp_rate");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* net recv bytes rate */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("net_rb_rate");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* net tx bytes rate */
    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(sample, &uint64, &n, OPAL_UINT64))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    kv[i].key = strdup("net_wb_rate");
    kv[i].type = OPAL_UINT64;
    kv[i++].data.uint64 = uint64;

    /* store it */
    if (ORTE_SUCCESS != (rc = opal_db.add_log("sigar", kv, 24))) {
        /* don't bark about it - just quietly disable the log */
        log_enabled = false;
    }

    /* cleanup the xfr storage */
    for (i=0; i < 24; i++) {
        OBJ_DESTRUCT(&kv[i]);
    }
    if (NULL != hostname) {
        free(hostname);
    }
}

/* Helper function to calculate the metric differences */
static uint64_t metric_diff_calc(sigar_uint64_t newval, uint64_t oldval,
                                 const char *name_for_log,
                                 const char *value_name_for_log)
{
    uint64_t diff;

    if (newval < oldval) {
        /* assume that the value was reset and we are starting over */
        opal_output_verbose(3, orte_sensor_base_framework.framework_output,
                            "%s metric_diff_calc: new value %" PRIu64 " is less than old value %" PRIu64
                            " for %s metric %s; assume the value was reset and set diff to new value.",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            newval, oldval, name_for_log, value_name_for_log);
        diff = newval;
    } else {
        diff = newval - oldval;
    }

    return diff;
}

static void generate_test_vector(opal_buffer_t *v)
{
    char *ctmp;
    uint64_t ui64;
    float ft;
    time_t now;

    ctmp = strdup("sigar");
    opal_dss.pack(v, &ctmp, 1, OPAL_STRING);
    free(ctmp);
    opal_dss.pack(v, &orte_process_info.nodename, 1, OPAL_STRING);
    /* get the time so it will be unique each time */
    now = time(NULL);
    /* pass the time along as a simple string */
    ctmp = ctime(&now);
    /* strip the trailing newline */
    ctmp[strlen(ctmp)-1] = '\0';
    opal_dss.pack(v, &ctmp, 1, OPAL_STRING);
    /* mem_total */
    ui64 = 1;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* mem_used */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* mem_actual_used */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* mem_actual_free */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* swap total */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* swap used */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* swap page in */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* swap page out */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* cpu user */
    ft = 1.0;
    opal_dss.pack(v, &ft, 1, OPAL_FLOAT);
    /* cpu sys */
    ft += 1.0;
    opal_dss.pack(v, &ft, 1, OPAL_FLOAT);
    /* cpu idle */
    ft += 1.0;
    opal_dss.pack(v, &ft, 1, OPAL_FLOAT);
    /* la */
    ft += 1.0;
    opal_dss.pack(v, &ft, 1, OPAL_FLOAT);
    /* la5 */
    ft += 1.0;
    opal_dss.pack(v, &ft, 1, OPAL_FLOAT);
    /* la15 */
    ft += 1.0;
    opal_dss.pack(v, &ft, 1, OPAL_FLOAT);
    /* reads */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* writes */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* read bytes */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* write bytes */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* rx packets */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* tx packets */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* rx bytes */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
    /* tx bytes */
    ui64++;
    opal_dss.pack(v, &ui64, 1, OPAL_UINT64);
}
