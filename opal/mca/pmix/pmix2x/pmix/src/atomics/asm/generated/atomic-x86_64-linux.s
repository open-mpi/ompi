	.text

	.globl pmix_atomic_mb
	.type pmix_atomic_mb, @function
pmix_atomic_mb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
	.size pmix_atomic_mb, .-pmix_atomic_mb


	.globl pmix_atomic_rmb
	.type pmix_atomic_rmb, @function
pmix_atomic_rmb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
	.size pmix_atomic_rmb, .-pmix_atomic_rmb


	.globl pmix_atomic_wmb
	.type pmix_atomic_wmb, @function
pmix_atomic_wmb:
	pushq	%rbp
	movq	%rsp, %rbp
	leave
	ret
	.size pmix_atomic_wmb, .-pmix_atomic_wmb


	.globl pmix_atomic_cmpset_32
	.type pmix_atomic_cmpset_32, @function
pmix_atomic_cmpset_32:
        movl    %esi, %eax
        lock; cmpxchgl %edx,(%rdi)
        sete     %dl
        movzbl  %dl, %eax
        ret
	.size pmix_atomic_cmpset_32, .-pmix_atomic_cmpset_32


	.globl pmix_atomic_cmpset_64
	.type pmix_atomic_cmpset_64, @function
pmix_atomic_cmpset_64:
        movq    %rsi, %rax
        lock; cmpxchgq %rdx,(%rdi)
        sete     %dl
        movzbl  %dl, %eax
        ret
	.size pmix_atomic_cmpset_64, .-pmix_atomic_cmpset_64


	.globl pmix_sys_timer_get_cycles
	.type pmix_sys_timer_get_cycles, @function
pmix_sys_timer_get_cycles:
        rdtsc
        salq    $32, %rdx
        mov     %eax, %eax
        orq     %rdx, %rax
        ret
	.size pmix_sys_timer_get_cycles, .-pmix_sys_timer_get_cycles

	.section	.note.GNU-stack,"",@progbits
