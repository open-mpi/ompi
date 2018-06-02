/*
 * Copyright © 2015-2018 Intel
 * Copyright © 2015-2018 Inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <private/autogen/config.h>
#include <stdio.h>
#include <stdint.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#define KERNEL_SMBIOS_SYSFS "/sys/firmware/dmi/entries"

#define KNL_SMBIOS_GROUP_STRING "Group: Knights Landing Information"
#define KNM_SMBIOS_GROUP_STRING "Group: Knights Mill Information"

/* Header is common part of all SMBIOS entries */
struct smbios_header
{
    uint8_t type;
    uint8_t length;
    uint16_t handle;
};

struct smbios_group
{
    uint8_t group_name;
};

/* This structrures are padded by compiler
 * So we hardcode size of the struct and use it
 * instead of sizeof() */
#define GROUP_ENTRY_SIZE 3
struct smbios_group_entry
{
    uint8_t type;
    uint16_t handle;
};

/* KNL header is similar as SMBIOS header
 * decided to add it for readability */
#define SMBIOS_KNL_HEADER_SIZE 7
struct knl_smbios_header
{
    uint8_t type;
    uint8_t length;
    uint16_t handle;
    uint16_t member_id;
    uint8_t member_name;
};

/* general info data */
#define SMBIOS_KNL_GENERAL_INFO_SIZE 5
struct knl_general_info
{
    uint8_t supp_cluster_mode;
    uint8_t cluster_mode;
    uint8_t supp_memory_mode;
    uint8_t memory_mode;
    uint8_t cache_info;
};

/* memory info */
#define SMBIOS_KNL_EDC_INFO_SIZE 9
struct knl_edc_info
{
    uint8_t mcdram_present;
    uint8_t mcdram_enabled;
    uint8_t allowed_channels;
    uint8_t reserved[4];
    uint8_t mcdram_info_size;
    uint8_t mcdram_info_count;
};

/* mcdram controller structure */
struct knl_mcdram_info {
    uint32_t status;
    uint8_t controller;
    uint8_t channel;
    uint16_t size64MB;
    uint8_t product_revision;
    uint8_t fwmajor_revision;
    uint8_t fwminor_revision;
    uint8_t fwpatch_revision;
};

/* internal data */
struct parser_data
{
    uint64_t mcdram_regular;
    uint64_t mcdram_cache;
    int cluster_mode;
    int memory_mode;
    int cache_info;
    int type_count;
    int knl_types[64];
};

enum cluster_mode
{
    QUADRANT = 1,
    HEMISPHERE = 2,
    SNC4 = 4,
    SNC2 = 8,
    ALL2ALL = 16
};

enum memory_mode
{
    CACHE = 1,
    FLAT = 2,
    HYBRID = 4
};

enum hybrid_cache
{
    H25 = 1,
    H50 = 2,
    H100 = 4 /* Incorrect but possible value */
};

static int get_file_buffer(const char *file, char *buffer, int size)
{
    FILE *f;

    if (!buffer) {
        fprintf(stderr, "Unable to allocate buffer\n");
        return 0;
    }

    f = fopen(file, "rb");
    if (!f) {
        fprintf(stderr, "Unable to open %s (%s)\n", file, strerror(errno));
        return 0;
    }

    printf("  File = %s, size = %d\n", file, size);

    size = fread(buffer, 1, size, f);
    if (size == 0) {
        fprintf(stderr, "Unable to read file\n");
        fclose(f);
        return 0;
    }

    fclose(f);
    return size;
}

static int check_entry(struct smbios_header *h, const char *end, const char *query)
{
    char *group_strings = (char*)h + h->length;
    do {
        int len = strlen(group_strings);
        /* SMBIOS string entries end with "\0\0"
         * if length is 0 break and return
         * */
        if (len == 0)
            break;
        if (!strncmp(group_strings, query, len))
            return 1;
        group_strings += len;
    } while(group_strings < end);

    return 0;
}

static int is_phi_group(struct smbios_header *h, const char *end)
{
    if (h->type != 14) {
        fprintf(stderr, "SMBIOS table is not group table\n");
        return -1;
    }

    return check_entry(h, end, KNL_SMBIOS_GROUP_STRING) ||
           check_entry(h, end, KNM_SMBIOS_GROUP_STRING);
}

#define KNL_MEMBER_ID_GENERAL 0x1
#define KNL_MEMBER_ID_EDC 0x2

#define PATH_SIZE 512
#define SMBIOS_FILE_BUF_SIZE 4096
#define KNL_MCDRAM_SIZE (16ULL*1024*1024*1024)

static int process_smbios_group(const char *input_fsroot, char *dir_name, struct parser_data *data)
{
    char path[PATH_SIZE];
    char file_buf[SMBIOS_FILE_BUF_SIZE];
    struct smbios_header *h;
    char *p;
    char *end;
    int size;
    int i;
    snprintf(path, PATH_SIZE-1, "%s/" KERNEL_SMBIOS_SYSFS "/%s/raw", input_fsroot, dir_name);
    path[PATH_SIZE-1] = 0;

    size = get_file_buffer(path, file_buf, SMBIOS_FILE_BUF_SIZE);
    if (!size) {
        fprintf(stderr, "Unable to read raw table file\n");
        return -1;
    }

    h = (struct smbios_header*)file_buf;
    end = file_buf+size;
    if (!is_phi_group(h, end)) {
        fprintf(stderr, "SMBIOS table does not contain KNL entries\n");
        return -1;
    }

    p = file_buf + sizeof(struct smbios_header) + sizeof(struct smbios_group);
    if ((char*)p >= end) {
        fprintf(stderr, "SMBIOS table does not have entries\n");
        return -1;
    }

    end = file_buf+h->length;

    i = 0;
    for (; p < end; i++, p+=3) {
        struct smbios_group_entry *e = (struct smbios_group_entry*)p;
        data->knl_types[i] = e->type;
        printf("  Found KNL type = %d\n", e->type);
    }

    data->type_count = i;
    return 0;
}

static int process_knl_entry(const char *input_fsroot, char *dir_name, struct parser_data *data)
{
    char path[PATH_SIZE];
    char file_buf[SMBIOS_FILE_BUF_SIZE];
    char *end;
    int size;
    struct knl_smbios_header *h;

    snprintf(path, PATH_SIZE-1, "%s/" KERNEL_SMBIOS_SYSFS "/%s/raw", input_fsroot, dir_name);
    path[PATH_SIZE-1] = 0;

    size = get_file_buffer(path, file_buf, SMBIOS_FILE_BUF_SIZE);
    if (!size) {
        fprintf(stderr, "Unable to read raw table file\n");
        return -1;
    }

    end = file_buf+size;
    h = (struct knl_smbios_header*)file_buf;
    if (h->member_id & KNL_MEMBER_ID_GENERAL) {
        struct knl_general_info *info =
            (struct knl_general_info*) (file_buf+SMBIOS_KNL_HEADER_SIZE);
        printf("  Getting general KNL info\n");
        data->cluster_mode = info->cluster_mode;
        data->memory_mode = info->memory_mode;
        data->cache_info = info->cache_info;
    } else if (h->member_id & KNL_MEMBER_ID_EDC) {
        struct knl_edc_info *info = (struct knl_edc_info*)(file_buf+SMBIOS_KNL_HEADER_SIZE);
        if (info->mcdram_present && info->mcdram_enabled) {
            struct knl_mcdram_info *mi = (struct knl_mcdram_info*)(info + 1);
            /* we use always smbios size not struct size
             * as it can change in future.*/
            int struct_size = info->mcdram_info_size;
            int i = 0;

            if (0 == struct_size) {
                printf("  MCDRAM info size is set to 0, falling back to known size\n");
                struct_size = sizeof(*mi);
            }
            printf("  Getting MCDRAM KNL info. Count=%d struct size=%d\n",
                   (int)info->mcdram_info_count, struct_size);
            for ( ; i < info->mcdram_info_count; i++) {
                if ((char*)mi >= end) {
                    fprintf(stderr, "SMBIOS KNL entry is too small\n");
                    return -1;
                }
                printf("  MCDRAM controller %d\n", mi->controller);
                if (mi->status & 0x1) {
                    printf("  Controller fused\n");
                } else {
                    data->mcdram_regular += mi->size64MB;
                    printf("  Size = %d MB\n", (int)mi->size64MB*64);
                }
                mi = (struct knl_mcdram_info*)(((char*)mi)+struct_size);
            }
            /* convert to bytes  */
            printf("  Total MCDRAM %llu MB\n", (long long unsigned int)data->mcdram_regular*64);
            data->mcdram_regular *= 64*1024*1024;
            /*
             * BIOS can expose some MCRAM controllers as fused
             * When this happens we hardcode MCDRAM size to 16 GB
             */
            if (data->mcdram_regular != KNL_MCDRAM_SIZE) {
                fprintf(stderr, "Not all MCDRAM is exposed in DMI. Please contact BIOS vendor\n");
                data->mcdram_regular = KNL_MCDRAM_SIZE;
            }

        } else {
            data->mcdram_regular = 0;
            data->mcdram_cache = 0;
        }

    } else {
        /* We skip unknown table */
        fprintf(stderr, "Ignoring unknown SMBIOS entry type=%x\n", h->member_id);
    }

    return 0;
}
static const char* get_memory_mode_str(int memory_mode, int hybrid_cache_size)
{
    switch (memory_mode) {
        case CACHE: return "Cache";
        case FLAT: return "Flat";
        case HYBRID:
            if (hybrid_cache_size == H25) {
                return "Hybrid25";
            } else if (hybrid_cache_size == H50) {
                return "Hybrid50";
            }
            return "Unknown";
        default:
            return "Unknown";
    }
}

static const char* get_cluster_mode_str(int cluster_mode)
{
    switch (cluster_mode) {
        case QUADRANT: return "Quadrant";
        case HEMISPHERE: return "Hemisphere";
        case ALL2ALL: return "All2All";
        case SNC2: return "SNC2";
        case SNC4: return "SNC4";
        default:
            return "Unknown";
    }
}

static int print_result(struct parser_data *data, const char *out_file)
{
    int node_count = 0;
    int fd;
    FILE *f;

    switch (data->cluster_mode) {
        case QUADRANT:
            node_count = 1;
            break;
        case HEMISPHERE:
            node_count = 1;
            break;
        case ALL2ALL:
            node_count = 1;
            break;
        case SNC2:
            node_count = 2;
            break;
        case SNC4:
            node_count = 4;
            break;
        default:
            fprintf(stderr, "Incorrect cluster mode %d\n", data->cluster_mode);
            return -1;
    }

    switch (data->memory_mode) {
        case CACHE:
            data->mcdram_cache = data->mcdram_regular;
            data->mcdram_regular = 0;
            break;
        case FLAT:
            data->mcdram_cache = 0;
            break;
        case HYBRID:
            if (data->cache_info == H25) {
                data->mcdram_cache = data->mcdram_regular/4;
            } else if (data->cache_info == H50) {
                data->mcdram_cache = data->mcdram_regular/2;
            } else if (data->cache_info == H100) {
                data->mcdram_cache = data->mcdram_regular;
            } else {
                fprintf(stderr, "SMBIOS reserved cache info value %d\n", data->cache_info);
                return -1;
            }
            data->mcdram_regular -= data->mcdram_cache;
            break;
        default:
            fprintf(stderr, "Incorrect memory mode %d\n", data->memory_mode);
            return -1;
    }

    printf("  Cluster Mode: %s Memory Mode: %s\n",
            get_cluster_mode_str(data->cluster_mode),
            get_memory_mode_str(data->memory_mode, data->cache_info));
    printf("  MCDRAM total = %llu bytes, cache = %llu bytes\n",
           (long long unsigned int)data->mcdram_regular,
           (long long unsigned int)data->mcdram_cache);
    data->mcdram_regular /= node_count;
    data->mcdram_cache /= node_count;
    printf("  MCDRAM total = %llu bytes, cache = %llu bytes per node\n",
           (long long unsigned int)data->mcdram_regular,
           (long long unsigned int)data->mcdram_cache);

    /* Now we can start printing stuff */
    /* use open+fdopen so that we can specify the file creation mode */
    fd = open(out_file, O_WRONLY|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH);
    if (fd < 0) {
        fprintf(stderr, "Unable to open file `%s' (%s).\n", out_file, strerror(errno));
        return -1;
    }
    f = fdopen(fd, "w");
    if (!f) {
        fprintf(stderr, "Unable to fdopen file `%s' (%s).\n", out_file, strerror(errno));
        close(fd);
        return -1;
    }

    fprintf(f, "version: 2\n");
    /* We cache is equal for node */
    fprintf(f, "cache_size: %llu\n",
                (long long unsigned int)data->mcdram_cache);
    fprintf(f, "associativity: 1\n");// direct-mapped cache
    fprintf(f, "inclusiveness: 1\n");// inclusive cache
    fprintf(f, "line_size: 64\n");
    fprintf(f, "cluster_mode: %s\n", get_cluster_mode_str(data->cluster_mode));
    fprintf(f, "memory_mode: %s\n", get_memory_mode_str(data->memory_mode, data->cache_info));
    fflush(f);
    fclose(f);
    close(fd);
    return 0;
}

/**
* Seeks SMBIOS sysfs for entry with type
*/
int hwloc_dump_hwdata_knl_smbios(const char *input_fsroot, const char *outfile);

int hwloc_dump_hwdata_knl_smbios(const char *input_fsroot, const char *outfile)
{
    DIR *d;
    int i;
    struct dirent *dir;
    struct parser_data data = { 0 };
    char path[PATH_SIZE];
    int err;

    printf("Dumping KNL SMBIOS Memory-Side Cache information:\n");

    snprintf(path, PATH_SIZE-1, "%s/" KERNEL_SMBIOS_SYSFS, input_fsroot);
    path[PATH_SIZE-1] = 0;

    d = opendir(path);
    if (!d) {
        fprintf(stderr, "Unable to open dmi-sysfs dir: %s", path);
        return -1;
    }

    /* process KNL entries
     * start with group (type 14, dash os to omit 140 types) then find SMBIOS types for
     * Knights Landing mcdram indofrmation
     */
    while ((dir = readdir(d))) {
        if (strncmp("14-", dir->d_name, 3) == 0) {
            err = process_smbios_group(input_fsroot, dir->d_name, &data);
            if (err < 0) {
                closedir(d);
                return err;
            }
        }
    }

    if (!data.type_count) {
      fprintf (stderr, "  Couldn't find any KNL information.\n");
      closedir(d);
      return -1;
    }

    /* We probably have KNL type identifiers here */
    for (i = 0; i < data.type_count; i++) {
        char tab[16] = {0};
        int l = snprintf(tab, sizeof(tab)-1, "%d-", data.knl_types[i]);
        printf ("  Seeking dir ̀`%s' %d\n", tab, l);
        rewinddir(d);
        while ((dir = readdir(d))) {
            if (strncmp(dir->d_name, tab, l) == 0) {
                err = process_knl_entry(input_fsroot, dir->d_name, &data);
                if (err < 0) {
                    closedir(d);
                    return err;
                }
            }
        }
    }

    closedir(d);

    return print_result(&data, outfile);
}
