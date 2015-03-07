Design notes on usnic BTL

======================================
nomenclature

fragment - something the PML asks us to send or put, any size
segment - something we can put on the wire in a single packet
chunk - a piece of a fragment that fits into one segment

a segment can contain either an entire fragment or a chunk of a fragment

each segment and fragment has associated descriptor.

Each segment data structure has a block of registered memory associated with
it which matches MTU for that segment
ACK - acks get special small segments with only enough memory for an ACK
non-ACK segments always have a parent fragment

fragments are either large (> MTU) or small (<= MTU)
a small fragment has a segment descriptor embedded within it since it
always needs exactly one.

a large fragment has no permanently associated segments, but allocates them
as needed.

======================================
channels

a channel is a queue pair with an associated completion queue
each channel has its own MTU and r/w queue entry counts

There are 2 channels, command and data
command queue is generally for higher priority fragments
data queue is for standard data traffic
command queue should possibly be called "priority" queue

command queue is shorter and has a smaller MTU that the data queue
this makes the command queue a lot faster than the data queue, so we
hijack it for sending very small fragments (<= tiny_mtu, currently 768 bytes)

command queue is used for ACKs and tiny fragments
data queue is used for everything else

PML fragments marked priority should perhaps use command queue

======================================
sending

Normally, all send requests are simply enqueued and then actually posted
to the NIC by the routine opal_btl_usnic_module_progress_sends().
"fastpath" tiny sends are the exception.

Each module maintains a queue of endpoints that are ready to send.
An endpoint is ready to send if all of the following are met:
- the endpoint has fragments to send
- the endpoint has send credits
- the endpoint's send window is "open" (not full of un-ACKed segments)

Each module also maintains a list of segments that need to be retransmitted.
Note that the list of pending retrans is per-module, not per-endpoint.

send progression first posts any pending retransmissions, always using the
data channel.  (reason is that if we start getting heavy congestion and
there are lots of retransmits, it becomes more important than ever to
prioritize ACKs, clogging command channel with retrans data makes things worse,
not better)

Next, progression loops sending segments to the endpoint at the top of
the "endpoints_with_sends" queue.  When an endpoint exhausts its send
credits or fills its send window or runs out of segments to send, it removes
itself from the endpoint_with_sends list.  Any pending ACKs will be
picked up and piggy-backed on these sends.

Finally, any endpoints that still need ACKs whose timer has expired will
be sent explicit ACK packets.

[double-click fragment sending]
The middle part of the progression loop handles both small (single-segment)
and large (multi-segment) sends.

For small fragments, the verbs descriptor within the embedded segment is
updated with length, BTL header is updated, then we call
opal_btl_usnic_endpoint_send_segment() to send the segment.
After posting, we make a PML callback if needed.

For large fragments, a little more is needed.  segments froma large
fragment have a slightly larger BTL header which contains a fragment ID,
and offset, and a size.  The fragment ID is allocated when the first chunk
the fragment is sent.  A segment gets allocated, next blob of data is
copied into this segment, segment is posted.  If last chunk of fragment
sent, perform callback if needed, then remove fragment from endpoint
send queue.

[double-click opal_btl_usnic_endpoint_send_segment()]

This is common posting code for large or small segments.  It assigns a
sequence number to a segment, checks for an ACK to piggy-back,
posts the segment to the NIC, and then starts the retransmit timer
by checking the segment into hotel.  Send credits are consumed here.


======================================
send dataflow

PML control messages with no user data are sent via:
desc = usnic_alloc(size)
usnic_send(desc)

user messages less than eager limit and 1st part of larger
messages are sent via:
desc = usnic_prepare_src(convertor, size)
usnic_send(desc)

larger msgs
desc = usnic_prepare_src(convertor, size)
usnic_put(desc)


usnic_alloc() currently asserts the length is "small", allocates and
fills in a small fragment.  src pointer will point to start of
associated registered mem + sizeof BTL header, and PML will put its
data there.

usnic_prepare_src() allocated either a large or small fragment based on size
The fragment descriptor is filled in to have 2 SG entries, 1st pointing to
place where PML should construct its header.  If the data convertor says
data is contiguous, 2nd SG entry points to user buffer, else it is null and
sf_convertor is filled in with address of convertor.

usnic_send()
If the fragment being sent is small enough, has contiguous data, and
"very few" command queue send WQEs have been consumed, usnic_send() does
a fastpath send.  This means it posts the segment immediately to the NIC
with INLINE flag set.

If all of the conditions for fastpath send are not met, and this is a small
fragment, the user data is copied into the associated registered memory at this
time and the SG list in the descriptor is collapsed to one entry.

After the checks above are done, the fragment is enqueued to be sent
via opal_btl_usnic_endpoint_enqueue_frag()

usnic_put()
Do a fast version of what happens in prepare_src() (can take shortcuts
because we know it will always be a contiguous buffer / no convertor
needed).  PML gives us the destination address, which we save on the
fragment (which is the sentinel value that the underlying engine uses
to know that this is a PUT and not a SEND), and the fragment is
enqueued for processing.

opal_btl_usnic_endpoint_enqueue_frag()
This appends the fragment to the "to be sent" list of the endpoint and
conditionally adds the endpoint to the list of endpoints with data to send
via opal_btl_usnic_check_rts()

======================================
receive dataflow

BTL packets has one of 3 types in header: frag, chunk, or ack.

A frag packet is a full PML fragment.
A chunk packet is a piece of a fragment that needs to be reassembled.
An ack packet is header only with a sequence number being ACKed.

Both frag and chunk packets go through some of the same processing.
Both may carry piggy-backed ACKs which may need to be processed.
Both have sequence numbers which must be processed and may result in
dropping the packet and/or queueing an ACK to the sender.

frag packets may be either regular PML fragments or PUT segments.
If the "put_addr" field of the BTL header is set, this is a PUT and
the data is copied directly to the user buffer.  If this field is NULL,
the segment is passed up to the PML.  The PML is expected to do everything
it needs with this packet in the callback, including copying data out if
needed.  Once the callback is complete, the receive buffer is recycled.

chunk packets are parts of a larger fragment.  If an active fragment receive
for the matching fragment ID cannot be found, and new fragment info
descriptor is allocated.  If this is not a PUT (put_addr == NULL), we
malloc() data to reassemble the fragment into.  Each subsequent chunk
is copied either into this reassembly buffer or directly into user memory.
When the last chunk of a fragment arrives, a PML callback is made for non-PUTs,
then the fragment info descriptor is released.

======================================
fast receive optimization

In order to optimize latency of small packets, the component progress routine
implements a fast path for receives.  If the first completion is a receive on
the priority queue, then it is handled by a routine called
opal_btl_usnic_recv_fast() which does nothing but validates that the packet
is OK to be received (sequence number OK and not a DUP) and then delivers it
to the PML.  This packet is recorded in the channel structure, and all
bookeeping for the packet is deferred until the next time component_progress
is called again.

This fast path cannot be taken every time we pass through component_progress
because there will be other completions that need processing, and the receive
bookeeping for one fast receive must be complete before allowing another fast
receive to occur, as only one recv segment can be saved for deferred
processing at a time.  This is handled by maintaining a variable in
opal_btl_usnic_recv_fast() called fastpath_ok which is set to false every time
the fastpath is taken.  A call into the regular progress routine will set this
flag back to true.

======================================
reliability:

every packet has sequence #
each endpoint has a "send window" , currently 4096 entries.
once a segment is sent, it is saved in window array until ACK is received
ACKs acknowledge all packets <= specified sequence #
rcvr only ACKs a sequence # when all packets up to that sequence have arrived

each pkt has dflt retrans timer of 100ms
packet will be scheduled for retrans if timer expires

Once a segment is sent, it always has its retransmit timer started.
This is accomplished by opal_hotel_checkin()
Any time a segment is posted to the NIC for retransmit, it is checked out
of the hotel (timer stopped).
So, a send segment is always in one of 4 states:
- on free list, unallocated
- on endpoint to-send list in the case of segment associated with small fragment
- posted to NIC and in hotel awaiting ACK
- on module re-send list awaiting retransmission

rcvr:
- if a pkt with seq >= expected seq is received, schedule ack of largest
  in-order sequence received if not already scheduled.  dflt time is 50us
- if a packet with seq < expected seq arrives, we send an ACK immediately,
  as this indicates a lost ACK

sender:
duplicate ACK triggers immediate retrans if one is not pending for that segment

======================================
Reordering induced by two queues and piggy-backing:

ACKs can be reordered-
  not an issue at all, old ACKs are simply ignored

Sends can be reordered-
(small send can jump far ahead of large sends)
large send followed by lots of small sends could trigger many retrans
of the large sends.  smalls would have to be paced pretty precisely to
keep command queue empty enough and also beat out the large sends.
send credits limit how many larges can be queued on the sender, but there
could be many on the receiver


======================================
RDMA emulation

We emulate the RDMA PUT because it's more efficient than regular send:
it allows the receive to copy directly to the target buffer
(vs. making an intermediate copy out of the bounce buffer).

It would actually be better to morph this PUT into a GET -- GET would
be slightly more efficient.  In short, when the target requests the
actual RDMA data, with PUT, the request has to go up to the PML, which
will then invoke PUT on the source's BTL module.  With GET, the target
issues the GET, and the source BTL module can reply without needing to
go up the stack to the PML.

Once we start supporting RDMA in hardware:

- we need to provide module.btl_register_mem and
  module.btl_deregister_mem functions (see openib for an example)
- we need to put something meaningful in
  btl_usnic_frag.h:mca_btl_base_registration_handle_t.
- we need to set module.btl_registration_handle_size to sizeof(struct
  mca_btl_base_registration_handle_t).
- module.btl_put / module.btl_get will receive the
  mca_btl_base_registration_handle_t from the peer as a cookie.

Also, module.btl_put / module.btl_get do not need to make descriptors
(this was an optimization added in BTL 3.0).  They are now called with
enough information to do whatever they need to do.  module.btl_put
still makes a descriptor and submits it to the usnic sending engine so
as to utilize a common infrastructure for send and put.

But it doesn't necessarily have to be that way -- we could optimize
out the use of the descriptors.  Have not investigated how easy/hard
that would be.

======================================

November 2014 / SC 2014
Update February 2015

The usnic BTL code has been unified across master and the v1.8
branches.  That is, you can copy the code from
v1.8:ompi/mca/btl/usnic/* to master:opal/mca/btl/usnic*, and then only
have to make 3 changes in the resulting code in master:

1. Edit Makefile.am: s/ompi/opal/gi
2. Edit configure.m4: s/ompi/opal/gi
   --> EXCEPT for:
       - opal_common_libfabric_* (which will eventually be removed,
         when the embedded libfabric goes away)
       - OPAL_BTL_USNIC_FI_EXT_USNIC_H (which will eventually be
         removed, when the embedded libfabric goes away)
       - OPAL_VAR_SCOPE_*
3. Edit Makefile.am: change -DBTL_IN_OPAL=0 to -DBTL_IN_OPAL=1

*** Note: the BTL_IN_OPAL preprocessor macro is set in Makefile.am
    rather that in btl_usnic_compat.h to avoid all kinds of include
    file dependency issues (i.e., btl_usnic_compat.h would need to be
    included first, but it requires some data structures to be
    defined, which means it either can't be first or we have to
    declare various structs first... just put BTL_IN_OPAL in
    Makefile.am and be happy).

*** Note 2: CARE MUST BE TAKEN WHEN COPYING THE OTHER DIRECTION!  It
    is *not* as simple as simple s/opal/ompi/gi in configure.m4 and
    Makefile.am.  It certainly can be done, but there's a few strings
    that need to stay "opal" or "OPAL" (e.g., OPAL_HAVE_HWLOC).
    Hence, the string replace will likely need to be done via manual
    inspection.

Things still to do:

- VF/PF sanity checks in component.c:check_usnic_config() uses
  usnic-specific fi_provider info.  The exact mechanism might change
  as provider-specific info is still being discussed upstream.

- component.c:usnic_handle_cq_error is using a USD_* constant from
  usnic_direct.  Need to get that value through libfabric somehow.

======================================

libfabric abstractions:

fi_fabric: corresponds to a VIC PF
fi_domain: corresponds to a VIC VF
fi_endpoint: resources inside the VIC VF (basically a QP)
