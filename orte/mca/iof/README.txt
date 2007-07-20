Some notes from Jeff/Rolf while trying to figure out how IOF works...

1. E-mail from Rolf->Jeff with some pretty ASCII art
2. Notes from Jeff about problems we'll have when/if we ever try to
   use IOF more creatively.

===============================================================================

	From: 	  Rolf.Vandevaart@Sun.COM
	Subject: 	Picture of IOF side of things.
	Date: 	June 7, 2007 10:43:42 AM EDT
	To: 	  jsquyres@cisco.com


Not sure if this will come out for you, but here is an ASCII represantation of what
the HNP looks like after IOF has been wired up.



VIEW OF DATA STRUCTURES ON IOF SVC SIDE

KEY: ORTE_IOF_SOURCE=0
     ORTE_IOF_SINK=1

After the job starts up, this is how things look on the HNP side of things.

ENDPOINTS (orte_iof_base_endpoint_t)
mode   origin	 tag	fd	seq	ack	src_frags   sink_frags  notes
=============================================================================================      
1	0,0,0	 1      1         0       0             0            0  pull() call from rmgr<-
=============================================================================================  |
1	0,0,0	 2      2         0       0             0            0  pull() call from rmgr<-|---
=============================================================================================  |   |
0	0,0,0    0      0         0       0             0            0  push() call from rmgr  |   |
=============================================================================================  |   |
                                                                                               |   |
                                                                                               |   |
SUBSCRIBERS (orte_iof_svc_sub_t)                                                               |   |
   ORIGIN              TARGET		  	 (list)                                        |   |
name   mask  tag    name   mask  tag  endpoint  forward  has_been_acked last_ack_forwarded     |   |
======================================================================================         |   |
0,1,0     2    1    0,0,0     2    1    	      0                                        |   |
                                           ptr-------------------------------------------------|   |
======================================================================================             |
0,1,0     2    2    0,0,0     2    2                  0                                            |
                                           ptr-----------------------------------------------------|
======================================================================================
0,0,0    ff    0    0,1,0     2    0      NULL	      1 -----------------------------------
                                                                                           |
======================================================================================     |
                                                                                           |
                                                                                           |
PUBLISHED (orte_iof_svc_pub_t)                                                             |
name   proxy    mask  tag	endpoint                                                   |
=========================================================================================  |
0,1,0  0,0,1      ff    0       NULL                                    <------------------
=========================================================================================


FORWARD (orte_iof_svc_fwd_t)
This structure is just a connection from a subscriber to publisher.  I have
omitted it in the drawings.  However, it is worth pointing out the structure
as I am not clear on why we have the table.

struct orte_iof_svc_fwd_t {
    opal_list_item_t super;
    orte_iof_svc_pub_t* fwd_pub;
    opal_hash_table_t fwd_seq_hash;
};


Note: This first subscriber says that it will receive from any process 
in the job.  Note that the jobid=1 and the mask=2.  So, we expect this
to collect the stdout from any of the ranks.  Obviously the second 
subscriber says the same thing but for stderr.  The third subscriber
is for receving data from stdin and sending it out to rank 0 of
the job.  Notice the mask=ff which means compare jobid,vpid
when addressing where the data goes.

The first endpoint is created by a call to pull by the rmgr.  After
the endpoint is created, a subscription is created as well.  Then, the
subscription is tied to the endpoint.

For the stdin creation, we first create the subscription, and then the
endpoint.  In that way, the endpoint is not found and does not get
tied to the subscription.  Hmmm, this I do not really understand.



APPENDIX A
These are the defines that go with the mask.  
#define ORTE_NS_CMP_NONE       0x00
#define ORTE_NS_CMP_JOBID      0x02
#define ORTE_NS_CMP_VPID       0x04
#define ORTE_NS_CMP_ALL        0Xff


When we get a HDR_MSG, we call orte_iof_svc_proxy_msg() 

APPENDIX B
There are two dbx files that help get to where we want to get 
for seeing how things work.  
start.x : Run this first to get initial breakpoint.  Needs this
so we can set additional breakpoints.  This also has some very
helpful aliases for looking at the structures shown above.

follow.x : Run this second to set initial breakpoints and setup
some useful aliases.

===============================================================================

Random notes from Jeff:

- Many issues may not come up because we only have single subscribers;
  I'm sure new things will come up.  Examples:

  - What happens if all subscribers to a stream disconnect, and then a
    new subscriber connects?  I'm guessing the ACKs will be all
    screwed up and we'll never end up reading from that fd again
    (because it will likely be stalled because not enough acks have
    been received, and therefore it removed itself from the event
    engine).

  - If all subscribers disconnect from a stdin/SINK, chances are that
    we'll lose the last frag that was sent before the disconnect.
    I.e., if there was a frag in flight when the disconnect was
    received, that frag is effectively lost.  So if someone reconnects
    to the stdin stream later, it won't start reading exactly where
    the first subscriber left off.  We need to define what is
    *supposed* to happen here...

- odls default: make handling of vpid 0 uniform between setup and
  takedown -- some kind of global variable, perhaps?  (not multi-proc /
  thread safe)

- odls default: currently, we publish stdin (if relevant), stdout, and
  stderr (note that only the stdin publish message gets sent to svc;
  the publish for SOURCEs stdout/stderr is not actually sent to the
  svc because all SOURCE frags are sent to the svc automatically).
  But we only unpublish stdout.  I think we should either:
  - publish stdin, stdout, stderr, and unpublish stdin, stdout, stderr
    or
  - publish stdin, and unpublish stdin
  I.e., make the code symmetric.

  Note, however, that unpublish for STDOUT/STDERR are sent to the svc
  (whereas publish for STDOUT/STDERR are not).  So if we unpublish
  stdout/stderr, we'll be creating a storm to the svc upon shutdown
  (i.e,. scalability problems).  :-(

- for scalability, we want to be able to change the proxy to *not*
  unconfitionally send everything to svc.  But this has the problem
  that if we do this, then we have to send the publish request to the
  svc (which we don't today since everything just automatically goes
  to svc).  But then in the common case (where vpid!=0 has no
  stdout/stderr), we're flooding svc with N publish requests from all
  the vpids, simply creating a different scalability problem (during
  startup).

- random q: are the proxy publish requests not sent back to svc
  because it prevents a storm of publish requests during startup?
  I.e., this was intentional to give better scalability?  Could be;
  but it still seems weird...

  Perhaps a better scheme would be to have the IOF *assume* that the
  stdin/stdout/stderr are all published upon startup (or be told by a
  single control message; perhaps in the app context?) and further
  *assume* that they are all unpublished when the job completes.

  Putting this info in the app context (for example) might jive with a
  more capable orterun that allows flexible stdin/stdout/stderr
  mapping (think: mpirun --screen ...).  mpirun makes the decision
  about how to wire up stdin/stdout/stderr and includes it in the app
  context (or whatever).  This is given to the svc who then creates
  publications as relevant.  Upon job completion, all
  publications/subscriptions related to that job are destroyed.

