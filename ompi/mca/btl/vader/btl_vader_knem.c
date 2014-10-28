/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_vader.h"

#if OMPI_BTL_VADER_HAVE_KNEM

#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include "opal/util/show_help.h"

int mca_btl_vader_knem_init (void)
{
    struct knem_cmd_info knem_info;
    int rc;

    /* Open the knem device.  Try to print a helpful message if we
       fail to open it. */
    mca_btl_vader.knem_fd = open("/dev/knem", O_RDWR);
    if (mca_btl_vader.knem_fd < 0) {
	if (EACCES == errno) {
	    struct stat sbuf;
	    if (0 != stat("/dev/knem", &sbuf)) {
		sbuf.st_mode = 0;
	    }
	    opal_show_help("help-btl-vader.txt", "knem permission denied",
			   true, ompi_process_info.nodename, sbuf.st_mode);
	} else {
	    opal_show_help("help-btl-vader.txt", "knem fail open",
			   true, ompi_process_info.nodename, errno,
			   strerror(errno));
	}

	return OMPI_ERR_NOT_AVAILABLE;
    }

    do {
	/* Check that the ABI if kernel module running is the same
	 * as what we were compiled against. */
	rc = ioctl(mca_btl_vader.knem_fd, KNEM_CMD_GET_INFO, &knem_info);
	if (rc < 0) {
	    opal_show_help("help-btl-vader.txt", "knem get ABI fail",
			   true, ompi_process_info.nodename, errno,
			   strerror(errno));
	    break;
	}

	if (KNEM_ABI_VERSION != knem_info.abi) {
	    opal_show_help("help-btl-vader.txt", "knem ABI mismatch",
			   true, ompi_process_info.nodename, KNEM_ABI_VERSION,
			   knem_info.abi);
	    break;
	}

	if (!(mca_btl_vader_component.knem_dma_min && (knem_info.features & KNEM_FEATURE_DMA))) {
	    /* disable DMA */
	    mca_btl_vader_component.knem_dma_min = UINT_MAX;
	}

	/* TODO: add async support */

	/* knem set up successfully */
	mca_btl_vader.super.btl_get = mca_btl_vader_get_knem;
	mca_btl_vader.super.btl_put = mca_btl_vader_put_knem;

	return OMPI_SUCCESS;
    } while (0);

    mca_btl_vader_knem_fini ();

    return OMPI_ERR_NOT_AVAILABLE;;
}

int mca_btl_vader_knem_fini (void)
{
    if (-1 != mca_btl_vader.knem_fd) {
	close (mca_btl_vader.knem_fd);
	mca_btl_vader.knem_fd = -1;
    }

    return OMPI_SUCCESS;
}

int mca_btl_vader_knem_progress (void)
{
    /* NTH: does nothing until async support is added */
    return OMPI_SUCCESS;
}

#endif
