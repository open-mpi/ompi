/*
 * Copyright (c) 2013 Intel Corporation. All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "psmx.h"

static void psmx_string_to_uuid(const char *s, psm_uuid_t uuid)
{
	int n;

	if (!s) {
		memset(uuid, 0, sizeof(psm_uuid_t));
		return;
	}

	n = sscanf(s,
		"%2hhx%2hhx%2hhx%2hhx-"
		"%2hhx%2hhx-%2hhx%2hhx-%2hhx%2hhx-"
		"%2hhx%2hhx%2hhx%2hhx%2hhx%2hhx",
		&uuid[0], &uuid[1], &uuid[2], &uuid[3],
		&uuid[4], &uuid[5], &uuid[6], &uuid[7], &uuid[8], &uuid[9],
		&uuid[10], &uuid[11], &uuid[12], &uuid[13], &uuid[14], &uuid[15]);

	if (n != 16) {
		fprintf(stderr, "%s: wrong uuid format: %s\n", __func__, s);
		fprintf(stderr, "%s: correct uuid format is: "
			"xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\n",
			__func__);
	}
}

void psmx_get_uuid(psm_uuid_t uuid)
{
	psmx_string_to_uuid(psmx_env.uuid, uuid);
}

int psmx_uuid_to_port(psm_uuid_t uuid)
{
	uint16_t port;
	uint16_t *u = (uint16_t *)uuid;

	port = u[0] + u[1] + u[2] + u[3] + u[4] + u[5] + u[6] + u[7];
	if (port < 4096)
		port += 4096;

	return (int)port;
}

static void psmx_name_server_cleanup(void *args)
{
	close((int)(uintptr_t)args);
}

/*************************************************************
 * A simple name resolution mechanism for client-server style
 * applications. The server side has to run first. The client
 * side then passes the server name as the first parameter
 * of fi_getinfo call and the resulting provider info should
 * have the transport address of the server in the dest_addr
 * field. Both side has to use the same UUID.
 *************************************************************/
void *psmx_name_server(void *args)
{
	struct psmx_fid_domain *domain;
	struct addrinfo hints = {
		.ai_flags = AI_PASSIVE,
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_STREAM
	};
	struct addrinfo *res, *p;
	char *service;
	int listenfd = -1, connfd;
	int port;
	int n;

	domain = args;
	port = domain->ns_port;

	pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);

	if (asprintf(&service, "%d", port) < 0)
		return NULL;

	n = getaddrinfo(NULL, service, &hints, &res);
	if (n < 0) {
		fprintf(stderr, "%s: port %d: %s\n", __func__, port, gai_strerror(n));
		free(service);
		return NULL;
	}

	for (p=res; p; p=p->ai_next) {
		listenfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
		if (listenfd >= 0) {
			n = 1;
			setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &n, sizeof(n));
			if (!bind(listenfd, p->ai_addr, p->ai_addrlen))
				break;
			close(listenfd);
			listenfd = -1;
		}
	}

	freeaddrinfo(res);
	free(service);

	if (listenfd < 0) {
		fprintf(stderr, "%s: couldn't listen to port %d\n", __func__, port);
		return NULL;
	}

	listen(listenfd, 256);

	pthread_cleanup_push(psmx_name_server_cleanup, (void *)(uintptr_t)listenfd);
	{
		pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
		pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

		while (1) {
			connfd = accept(listenfd, NULL, 0);
			if (connfd >= 0) {
				pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
				write(connfd, &domain->psm_epid, sizeof(psm_epid_t));
				close(connfd);
				pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
			}
		}
	}
	pthread_cleanup_pop(1);

	return NULL;
}

void *psmx_resolve_name(const char *servername, int port)
{
	struct addrinfo hints = {
		.ai_family   = AF_UNSPEC,
		.ai_socktype = SOCK_STREAM
	};
	struct addrinfo *res, *p;
	psm_uuid_t uuid;
	char *service;
	void *dest_addr;
	int sockfd = -1;
	int n;

	if (!port) {
		psmx_get_uuid(uuid);
		port = psmx_uuid_to_port(uuid);
	}

	if (asprintf(&service, "%d", port) < 0)
		return NULL;

	n = getaddrinfo(servername, service, &hints, &res);
	if (n < 0) {
		fprintf(stderr, "%s:(%s:%d):%s\n", __func__, servername, port, gai_strerror(n));
		free(service);
		return NULL;
	}

	for (p = res; p; p = p->ai_next) {
		sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
		if (sockfd >= 0) {
			if (!connect(sockfd, p->ai_addr, p->ai_addrlen))
				break;
			close(sockfd);
			sockfd = -1;
		}
	}

	freeaddrinfo(res);
	free(service);

	if (sockfd < 0) {
		fprintf(stderr, "%s: couldn't connect to %s:%d\n", __func__, servername, port);
		return NULL;
	}

	dest_addr = calloc(1,sizeof(*dest_addr));
	if (!dest_addr) {
		close(sockfd);
		return NULL;
	}

	if (read(sockfd, dest_addr, sizeof(psm_epid_t)) != sizeof(psm_epid_t)) {
		perror(__func__);
		free(dest_addr);
		close(sockfd);
		return NULL;
	}

	close(sockfd);

	return dest_addr;
}

static int psmx_errno_table[PSM_ERROR_LAST] = {
	0,		/* PSM_OK = 0 */
	0,		/* PSM_OK_NO_PROGRESS = 1 */
	-FI_EOTHER,
	-FI_EINVAL,	/* PSM_PARAM_ERR = 3 */
	-FI_ENOMEM, 	/* PSM_NO_MEMORY = 4 */
	-FI_EBADF,	/* PSM_INIT_NOT_INIT = 5 */
	-FI_EINVAL,	/* PSM_INIT_BAD_API_VERSION = 6 */
	-FI_ENOSYS,	/* PSM_NO_AFFINITY = 7 */
	-FI_EIO,	/* PSM_INTERNAL_ERR = 8 */
	-FI_EINVAL,	/* PSM_SHMEM_SEGMENT_ERR = 9 */
	-FI_EACCES,	/* PSM_OPT_READONLY = 10 */
	-FI_ETIMEDOUT,	/* PSM_TIMEOUT = 11 */
	-FI_EMFILE,	/* PSM_TOO_MANY_ENDPOINTS = 12 */
	-FI_ESHUTDOWN,	/* PSM_IS_FINALIZED = 13 */
	-FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER,
	-FI_ESHUTDOWN,	/* PSM_EP_WAS_CLOSED = 20 */
	-FI_ENODEV,	/* PSM_EP_NO_DEVICE = 21 */
	-FI_ENOENT,	/* PSM_EP_UNIT_NOT_FOUND = 22 */
	-FI_EIO,	/* PSM_EP_DEVICE_FAILURE = 23 */
	-FI_ETIMEDOUT, 	/* PSM_EP_CLOSE_TIMEOUT = 24 */
	-FI_ENOENT,	/* PSM_EP_NO_PORTS_AVAIL = 25 */
	-FI_ENETDOWN,	/* PSM_EP_NO_NETWORK = 26 */
	-FI_EINVAL,	/* PSM_EP_INVALID_UUID_KEY = 27 */
	-FI_ENOSPC,	/* PSM_EP_NO_RESOURCES = 28 */
	-FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER,
	-FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER,
	-FI_EBADF,	/* PSM_EPID_UNKNOWN = 40 */
	-FI_ENETUNREACH,/* PSM_EPID_UNREACHABLE = 41 */
	-FI_EOTHER,
	-FI_EINVAL,	/* PSM_EPID_INVALID_NODE = 43 */
	-FI_EINVAL,	/* PSM_EPID_INVALID_MTU =  44 */
	-FI_EINVAL,	/* PSM_EPID_INVALID_UUID_KEY = 45 */
	-FI_EINVAL,	/* PSM_EPID_INVALID_VERSION = 46 */
	-FI_EINVAL,	/* PSM_EPID_INVALID_CONNECT = 47 */
	-FI_EISCONN,	/* PSM_EPID_ALREADY_CONNECTED = 48 */
	-FI_EIO,	/* PSM_EPID_NETWORK_ERROR = 49 */
	-FI_EINVAL,	/* PSM_EPID_INVALID_PKEY = 50 */
	-FI_ENETUNREACH,/* PSM_EPID_PATH_RESOLUTION = 51 */
	-FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER,
	-FI_EOTHER, -FI_EOTHER,
	-FI_EAGAIN,	/* PSM_MQ_NO_COMPLETIONS = 60 */
	-FI_EMSGSIZE,	/* PSM_MQ_TRUNCATION = 61 */
	-FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER,
	-FI_EOTHER, -FI_EOTHER,
	-FI_EINVAL,	/* PSM_AM_INVALID_REPLY = 70 */
	-FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER, -FI_EOTHER,
	-FI_EOTHER, -FI_EOTHER, -FI_EOTHER
			/* PSM_ERROR_LAST = 80 */
};

int psmx_errno(int err)
{
	if (err >= 0 && err < PSM_ERROR_LAST)
		return psmx_errno_table[err];
	else
		return -FI_EOTHER;
}

/*
 * PSM context sharing requires some information from the MPI process manager.
 * Try to get the needed information from the environment.
 */
void psmx_query_mpi(void)
{
	char *s;
	char env[32];
	int local_size = -1;
	int local_rank = -1;

	/* Check Open MPI */
	if ((s = getenv("OMPI_COMM_WORLD_LOCAL_SIZE"))) {
		local_size = atoi(s);
		if ((s = getenv("OMPI_COMM_WORLD_LOCAL_RANK")))
			local_rank = atoi(s);
		snprintf(env, sizeof(env), "%d", local_size);
		setenv("MPI_LOCALNRANKS", env, 0);
		snprintf(env, sizeof(env), "%d", local_rank);
		setenv("MPI_LOCALRANKID", env, 0);
		return;
	}

	/* TODO: check other MPI */
}

void psmx_debug(char *fmt, ...)
{
	va_list ap;

	if (psmx_env.debug) {
		va_start(ap, fmt);
		vfprintf(stderr, fmt, ap);
		va_end(ap);
	}
}

