/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OSC_UBCL_SYNC_H
#define MCA_OSC_UBCL_SYNC_H

typedef enum ubcl_win_sync_type {
    UBCL_WIN_SYNC_NONE,
    UBCL_WIN_SYNC_LOCK,
    UBCL_WIN_SYNC_LOCK_NO_CHECK,
    UBCL_WIN_SYNC_LOCK_ALL,
    UBCL_WIN_SYNC_LOCK_ALL_NO_CHECK,
    UBCL_WIN_SYNC_PSCW,
    UBCL_WIN_SYNC_FENCE,
    UBCL_WIN_SYNC_FENCE_EPOCH
} ubcl_win_sync_type_t;

/* Component API */
int ompi_osc_ubcl_lock(int lock_type, int target, int assert, struct ompi_win_t *win);
int ompi_osc_ubcl_unlock(int target, struct ompi_win_t *win);
int ompi_osc_ubcl_lock_all(int assert, struct ompi_win_t *win);
int ompi_osc_ubcl_unlock_all(struct ompi_win_t *win);

int ompi_osc_ubcl_start(struct ompi_group_t *group, int assert, struct ompi_win_t *win);
int ompi_osc_ubcl_complete(struct ompi_win_t *win);
int ompi_osc_ubcl_post(struct ompi_group_t *group, int assert, struct ompi_win_t *win);
int ompi_osc_ubcl_wait(struct ompi_win_t *win);
int ompi_osc_ubcl_test(struct ompi_win_t *win, int *flag);

int ompi_osc_ubcl_fence(int assert, struct ompi_win_t *win);

int ompi_osc_ubcl_sync(struct ompi_win_t *win);

/* OSC/UBCL internals */
int ompi_osc_ubcl_check_access_epoch(int target_rank, struct ompi_win_t *win);

#endif /* MCA_OSC_UBCL_SYNC_H */
