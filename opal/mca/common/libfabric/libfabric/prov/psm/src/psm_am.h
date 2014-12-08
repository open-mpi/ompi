/*
 * Copyright (c) 2006-2012. QLogic Corporation. All rights reserved.
 * Copyright (c) 2003-2006, PathScale, Inc. All rights reserved.
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

#ifndef PSM_AM_H
#define PSM_AM_H

#include <psm.h>

#ifdef __cplusplus
extern "C" {
#endif



/* Datatype for an index number representing an active message handler */
typedef uint32_t psm_handler_t;

/* Datatype for a token for an active message handler.*/
typedef void    *psm_am_token_t;

/* PSM AM flags
 * These flags may be combined using bitwise-or.
 */
#define PSM_AM_FLAG_NONE    0 /* This flag should be used when no other PSM AM flags are needed. */
#define PSM_AM_FLAG_ASYNC   1 /* This flag indicates no need to copy source data. */
#define PSM_AM_FLAG_NOREPLY 2 /* This flag indicates that the handler for this AM request is guaranteed not to generate a reply. */

/* The psm_amarg type represents the type of an AM argument. This is
 *  a 64-bit type and is broken down into four 16-bit fields, two 32-bit
 *  fields or one 64-bit field for the convenience of code using the PSM AM
 *  interface.
 */
typedef
struct psm_amarg { 
    union {
	struct {
	    uint16_t	u16w3;
	    uint16_t	u16w2;
	    uint16_t	u16w1;
	    uint16_t	u16w0;
	};
	struct {
	    uint32_t	u32w1;
	    uint32_t	u32w0;
	};
	uint64_t	u64w0;
	uint64_t	u64;
    };
}
psm_amarg_t;

/* The AM handler function type
 *  
 * psm_am_handler_fm_t is the datatype for an AM handler. PSM AM will call-back
 * into an AM handler using this function prototype. The parameters and result
 * of these handler functions are described here.
 *
 * [in] token This is an opaque token value passed into a handler.
 *                  A request handler may send at most one reply back to the original 
 *                  requestor, and must pass this value as the token parameter
 *                  to the psm_am_reply_short() function. A reply handler is also
 *                  passed a token value, but must not attempt to reply.
 * [in] epaddr The end-point address of the other party in this AM transaction.
 * [in] args A pointer to the arguments provided to this handler.
 * [in] nargs The number of arguments.
 * [in] src A pointer to the data payload provided to this handler.
 * [in] len The length of the data payload in bytes.
 *
 * [returns] 0 The handler should always return a result of 0.
 */
typedef
int (*psm_am_handler_fn_t)(psm_am_token_t token, psm_epaddr_t epaddr,
			   psm_amarg_t *args, int nargs, 
			   void *src, uint32_t len);

/* Type for a completion call-back handler.
 *  
 * A completion handler can be specified to give a call-back on the initiation
 * side that an AM request or reply has completed on the target side. The call-back
 * has a context pointer which is provided along with the call-back function
 * pointer when the initiator generates the request or reply. This approach will
 * typically give higher performance than using an AM request or reply to achieve
 * the same effect, though note that no additional information can be passed
 * from the target side back to the initiator side with the completion handler
 * approach.
 *
 * [in] context A context pointer.
 * [returns] void This handler has no return result.
 */
typedef
void (*psm_am_completion_fn_t)(void *context);

/* Register AM call-back handlers at the specified end-point.
 *
 * This function is used to register an array of handlers, and may be called
 * multiple times to register additonal handlers. The maximum number of handlers
 * that can be registered is limited to the max_handlers value returned by
 * psm_am_get_parameters(). Handlers are associated with a PSM end-point. The
 * handlers are allocated index numbers in the the handler table for that end-point.
 * The allocated index for the handler function in handlers[i] is returned in
 * handlers_idx[i] for i in (0, num_handlers]. These handler index values are
 * used in the psm_am_request_short() and psm_am_reply_short() functions.
 * 
 * [in] ep End-point value
 * [in] handlers Array of handler functions
 * [in] num_handlers Number of handlers (sizes the handlers and handlers_idx arrays)
 * [out] handlers_idx Used to return handler index mapping table
 * 
 * [returns] PSM_OK Indicates success
 * [returns] PSM_EP_NO_RESOURCES Insufficient slots in the AM handler table
 */
psm_error_t psm_am_register_handlers(psm_ep_t ep, 
				     const psm_am_handler_fn_t *handlers, 
				     int num_handlers, int *handlers_idx);

/* Generate an AM request.
 *
 * This function generates an AM request causing an AM handler function to be
 * called in the PSM process associated with the specified end-point address.
 * The number of arguments is limited to max_nargs and the payload length in bytes
 * to max_request_short returned by the psm_am_get_parameters() function.
 * If arguments are not required, set the number of arguments to 0 and the argument
 * pointer will not be dereferenced. If payload is not required, set the payload size
 * to 0 and the payload pointer will not be dereferenced.
 *
 * Optionally a completion function and completion context pointer can be provided,
 * and a local call-back will be made to that function passing in that context
 * pointer once remote execution of the handler has completed. If the completion
 * call-back is not required, the handler should be specified as NULL and the
 * pointer value will not be used.
 * 
 * The allowed flags are any combination of the following combined with bitwise-or:
 *   PSM_AM_FLAG_NONE    - No flags
 *   PSM_AM_FLAG_ASYNC   - Indicates no need to copy source data
 *   PSM_AM_FLAG_NOREPLY - The handler for this AM request is guaranteed not to generate a reply
 *
 * The PSM AM implementation will not dereference the args pointer after return from
 * this function. If PSM_AM_FLAG_ASYNC is not provided, the PSM AM implementation will
 * not dereference the src pointer after return from this function. This may require the
 * implementation to take a copy of the payload if the request cannot be issued immediately.
 * However, if PSM_AM_FLAG_ASYNC is provided then a copy will not be taken and the PSM AM
 * implementation retains ownership of the payload src memory until the request is locally
 * complete. Local completion can be determined using the completion handler call-back, or
 * through an AM handler associated with an AM reply.
 *
 * The PSM_AM_FLAG_NOREPLY flag indicates ahead of time to the AM handler that a reply will
 * not be generated. Use of this flag is optional, but it may enable a performance optimization
 * in this case by indicating that reply state is not required.
 *
 * [in] epaddr End-point address to run handler on
 * [in] handler Index of handler to run
 * [in] args Array of arguments to be provided to the handler
 * [in] nargs Number of arguments to be provided to the handler
 * [in] src Pointer to the payload to be delivered to the handler
 * [in] len Length of the payload in bytes
 * [in] flags These are PSM AM flags and may be combined together with bitwise-or
 * [in] completion_fn The completion function to called locally when remote handler is complete
 * [in] completion_ctxt User-provided context pointer to be passed to the completion handler
 * 
 * [returns] PSM_OK indicates success.
 */
psm_error_t
psm_am_request_short(psm_epaddr_t epaddr, psm_handler_t handler, 
		     psm_amarg_t *args, int nargs, void *src, size_t len,
		     int flags, psm_am_completion_fn_t completion_fn,
		     void *completion_ctxt);

/* Generate an AM reply.
 *
 * This function may only be called from an AM handler called due to an AM request.
 * If the AM request uses the PSM_AM_FLAG_NOREPLY flag, the AM handler must not
 * call this function. Otherwise, the AM request handler may call psm_am_reply_short() 
 * at most once, and must pass in the token value that it received in its own handler 
 * call-back. 
 *
 * This function generates an AM reply causing an AM handler function to be
 * called in the PSM process associated with the specified end-point address.
 * The number of arguments is limited to max_nargs and the payload length in bytes
 * to max_reply_short returned by the psm_am_get_parameters() function.
 * If arguments are not required, set the number of arguments to 0 and the argument
 * pointer will not be dereferenced. If payload is not required, set the payload size
 * to 0 and the payload pointer will not be dereferenced.
 *
 * Optionally a completion function and completion context pointer can be provided,
 * and a local call-back will be made to that function passing in that context
 * pointer once remote execution of the handler has completed. If the completion
 * call-back is not required, the handler should be specified as NULL and the
 * pointer value will not be used.
 * 
 * The allowed flags are any combination of the following combined with bitwise-or:
 *   PSM_AM_FLAG_NONE    - No flags
 *   PSM_AM_FLAG_ASYNC   - Indicates no need to copy source data
 *
 * The PSM AM implementation will not dereference the args pointer after return from
 * this function. If PSM_AM_FLAG_ASYNC is not provided, the PSM AM implementation will
 * not dereference the src pointer after return from this function. This may require the
 * implementation to take a copy of the payload if the reply cannot be issued immediately.
 * However, if PSM_AM_FLAG_ASYNC is provided then a copy will not be taken and the PSM AM
 * implementation retains ownership of the payload src memory until the reply is locally
 * complete. Local completion can be determined using the completion handler call-back.
 *
 * [in] token Token value provided to the AM handler that is generating the reply.
 * [in] handler Index of handler to run
 * [in] args Array of arguments to be provided to the handler
 * [in] nargs Number of arguments to be provided to the handler
 * [in] src Pointer to the payload to be delivered to the handler
 * [in] len Length of the payload in bytes
 * [in] flags These are PSM AM flags and may be combined together with bitwise-or
 * [in] completion_fn The completion function to called locally when remote handler is complete
 * [in] completion_ctxt User-provided context pointer to be passed to the completion handler
 * 
 * [returns] PSM_OK indicates success.
 */
psm_error_t
psm_am_reply_short(psm_am_token_t token, psm_handler_t handler, 
		   psm_amarg_t *args, int nargs, void *src, size_t len, 
		   int flags, psm_am_completion_fn_t completion_fn,
		   void *completion_ctxt);

/* AM parameters
 *
 * This structure is used to return PSM AM implementation-specific parameter
 * values back to the caller of the psm_am_get_parameters() function. This
 * API also specifies the minimum values for these parameters that an 
 * implementation must at least provide:
 *   max_handlers >= 64,
 *   max_nargs >= 2,
 *   max_request_short >= 256 and
 *   max_reply_short >= 256.
 */
struct psm_am_parameters {
    uint32_t    max_handlers;		/* Maximum number of handlers that can be registered. */
    uint32_t	max_nargs;		/* Maximum number of arguments to an AM handler. */
    uint32_t	max_request_short;	/* Maximum number of bytes in a request payload. */
    uint32_t	max_reply_short;	/* Maximum number of bytes in a reply payload. */
};

/* Get the AM parameter values
 *
 * This function retrieves the implementation-specific AM parameter values for 
 * the specified end-point.
 *
 * [in] ep The end-point value returned by psm_ep_open().
 * [out] parameters Pointer to the struct where the parameters will be returned.
 * [in] sizeof_parameters_in The size in bytes of the struct provided by the caller.
 * [out] sizeof_parameters_out The size in bytes of the struct returned by PSM.
 *
 * [returns] PSM_OK indicates success.
 */
psm_error_t
psm_am_get_parameters(psm_ep_t ep, struct psm_am_parameters *parameters,
                      size_t sizeof_parameters_in,
		      size_t *sizeof_parameters_out);


#ifdef __cplusplus
}				/* extern "C" */
#endif

#endif
