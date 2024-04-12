/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/util/pmix_fd.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/odls/odls_types.h"

#include "src/mca/rtc/base/base.h"

void prte_rtc_base_assign(prte_job_t *jdata)
{
    prte_rtc_base_selected_module_t *active;

    PMIX_LIST_FOREACH(active, &prte_rtc_base.actives, prte_rtc_base_selected_module_t)
    {
        if (NULL != active->module->assign) {
            /* give this module a chance to operate on it */
            active->module->assign(jdata);
        }
    }
}

void prte_rtc_base_set(prte_odls_spawn_caddy_t *cd, int error_fd)
{
    prte_rtc_base_selected_module_t *active;

    PMIX_LIST_FOREACH(active, &prte_rtc_base.actives, prte_rtc_base_selected_module_t)
    {
        if (NULL != active->module->set) {
            /* give this module a chance to operate on it */
            active->module->set(cd, error_fd);
        }
    }
}

void prte_rtc_base_get_avail_vals(pmix_list_t *vals)
{
    prte_rtc_base_selected_module_t *active;

    PMIX_LIST_FOREACH(active, &prte_rtc_base.actives, prte_rtc_base_selected_module_t)
    {
        if (NULL != active->module->get_available_values) {
            /* give this module a chance to operate on it */
            active->module->get_available_values(vals);
        }
    }
}

static int write_help_msg(int fd, prte_odls_pipe_err_msg_t *msg, const char *file,
                          const char *topic, va_list ap)
{
    int ret;
    char *str;

    if (NULL == file || NULL == topic) {
        return PRTE_ERR_BAD_PARAM;
    }

    str = pmix_show_help_vstring(file, topic, true, ap);

    msg->file_str_len = (int) strlen(file);
    if (msg->file_str_len > PRTE_ODLS_MAX_FILE_LEN) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }
    msg->topic_str_len = (int) strlen(topic);
    if (msg->topic_str_len > PRTE_ODLS_MAX_TOPIC_LEN) {
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
        return PRTE_ERR_BAD_PARAM;
    }
    msg->msg_str_len = (int) strlen(str);

    /* Only keep writing if each write() succeeds */
    if (PRTE_SUCCESS != (ret = pmix_fd_write(fd, sizeof(*msg), msg))) {
        goto out;
    }
    if (msg->file_str_len > 0
        && PRTE_SUCCESS != (ret = pmix_fd_write(fd, msg->file_str_len, file))) {
        goto out;
    }
    if (msg->topic_str_len > 0
        && PRTE_SUCCESS != (ret = pmix_fd_write(fd, msg->topic_str_len, topic))) {
        goto out;
    }
    if (msg->msg_str_len > 0 && PRTE_SUCCESS != (ret = pmix_fd_write(fd, msg->msg_str_len, str))) {
        goto out;
    }

out:
    free(str);
    return ret;
}

int prte_rtc_base_send_warn_show_help(int fd, const char *file, const char *topic, ...)
{
    int ret;
    va_list ap;
    prte_odls_pipe_err_msg_t msg;

    msg.fatal = false;
    msg.exit_status = 0; /* ignored */

    /* Send it */
    va_start(ap, topic);
    ret = write_help_msg(fd, &msg, file, topic, ap);
    va_end(ap);

    return ret;
}

void prte_rtc_base_send_error_show_help(int fd, int exit_status, const char *file,
                                        const char *topic, ...)
{
    va_list ap;
    prte_odls_pipe_err_msg_t msg;

    msg.fatal = true;
    msg.exit_status = exit_status;

    /* Send it */
    va_start(ap, topic);
    write_help_msg(fd, &msg, file, topic, ap);
    va_end(ap);

    exit(exit_status);
}
