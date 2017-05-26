START_FILE

	.pred.safe_across_calls p1-p5,p16-p63
	.text
	.align 16
	.global pmix_atomic_mb#
	.proc pmix_atomic_mb#
pmix_atomic_mb:
	.prologue
	.body
	mf
	br.ret.sptk.many b0
	;;
	.endp pmix_atomic_mb#
	.align 16
	.global pmix_atomic_rmb#
	.proc pmix_atomic_rmb#
pmix_atomic_rmb:
	.prologue
	.body
	mf
	br.ret.sptk.many b0
	;;
	.endp pmix_atomic_rmb#
	.align 16
	.global pmix_atomic_wmb#
	.proc pmix_atomic_wmb#
pmix_atomic_wmb:
	.prologue
	.body
	mf
	br.ret.sptk.many b0
	;;
	.endp pmix_atomic_wmb#
	.align 16
	.global pmix_atomic_cmpset_acq_32#
	.proc pmix_atomic_cmpset_acq_32#
pmix_atomic_cmpset_acq_32:
	.prologue
	.body
	mov ar.ccv=r33;;
	cmpxchg4.acq r32=[r32],r34,ar.ccv
	;;
	cmp4.eq p6, p7 = r32, r33
	;;
	(p6) addl r8 = 1, r0
	(p7) mov r8 = r0
	br.ret.sptk.many b0
	;;
	.endp pmix_atomic_cmpset_acq_32#
	.align 16
	.global pmix_atomic_cmpset_rel_32#
	.proc pmix_atomic_cmpset_rel_32#
pmix_atomic_cmpset_rel_32:
	.prologue
	.body
	mov ar.ccv=r33;;
	cmpxchg4.rel r32=[r32],r34,ar.ccv
	;;
	cmp4.eq p6, p7 = r32, r33
	;;
	(p6) addl r8 = 1, r0
	(p7) mov r8 = r0
	br.ret.sptk.many b0
	;;
	.endp pmix_atomic_cmpset_rel_32#
	.align 16
	.global pmix_atomic_cmpset_acq_64#
	.proc pmix_atomic_cmpset_acq_64#
pmix_atomic_cmpset_acq_64:
	.prologue
	.body
	mov ar.ccv=r33;;
	cmpxchg8.acq r32=[r32],r34,ar.ccv
	;;
	cmp.eq p6, p7 = r33, r32
	;;
	(p6) addl r8 = 1, r0
	(p7) mov r8 = r0
	br.ret.sptk.many b0
	;;
	.endp pmix_atomic_cmpset_acq_64#
	.align 16
	.global pmix_atomic_cmpset_rel_64#
	.proc pmix_atomic_cmpset_rel_64#
pmix_atomic_cmpset_rel_64:
	.prologue
	.body
	mov ar.ccv=r33;;
	cmpxchg8.rel r32=[r32],r34,ar.ccv
	;;
	cmp.eq p6, p7 = r33, r32
	;;
	(p6) addl r8 = 1, r0
	(p7) mov r8 = r0
	br.ret.sptk.many b0
	;;
	.endp pmix_atomic_cmpset_rel_64#
        .align 16
        .global pmix_sys_timer_get_cycles#
        .proc pmix_sys_timer_get_cycles#
pmix_sys_timer_get_cycles:
        .prologue
        .body
        mov r8=ar.itc
        br.ret.sptk.many b0
        ;;
        .endp pmix_sys_timer_get_cycles#
	.ident	"GCC: (GNU) 3.2.3 20030502 (Red Hat Linux 3.2.3-49)"
