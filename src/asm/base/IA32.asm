START_FILE
	TEXT

START_FUNC(ompi_atomic_mb)
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
END_FUNC(ompi_atomic_mb)


START_FUNC(ompi_atomic_rmb)
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
END_FUNC(ompi_atomic_rmb)


START_FUNC(ompi_atomic_wmb)
	pushl	%ebp
	movl	%esp, %ebp
	leave
	ret
END_FUNC(ompi_atomic_wmb)


START_FUNC(ompi_atomic_cmpset_32)
	pushl   %ebp
	movl    %esp, %ebp
	movl    8(%ebp), %edx
	movl    16(%ebp), %ecx
	movl    12(%ebp), %eax
#APP
	lock cmpxchgl %ecx,(%edx)
	sete     %dl

#NO_APP
	movzbl  %dl, %eax
	leave
	ret
END_FUNC(ompi_atomic_cmpset_32)


START_FUNC(ompi_atomic_cmpset_64)
	pushl	%ebp
	movl	%esp, %ebp
	subl	$32, %esp
	movl	%ebx, -12(%ebp)
	movl	%esi, -8(%ebp)
	movl	%edi, -4(%ebp)
	movl	8(%ebp), %edi
	movl	12(%ebp), %eax
	movl	16(%ebp), %edx
	movl	%eax, -24(%ebp)
	movl	%edx, -20(%ebp)
	movl	20(%ebp), %eax
	movl	24(%ebp), %edx
	movl	%eax, -32(%ebp)
	movl	%edx, -28(%ebp)
	movl	-24(%ebp), %ebx
	movl	-20(%ebp), %edx
	movl	-32(%ebp), %esi
	movl	-28(%ebp), %ecx
	movl	%ebx, %eax
#APP
	push %ebx            
	movl %esi, %ebx        
	lock cmpxchg8b (%edi)
	sete %dl               
	pop %ebx             
	
#NO_APP
	movzbl	%dl, %eax
	movl	-12(%ebp), %ebx
	movl	-8(%ebp), %esi
	movl	-4(%ebp), %edi
	movl	%ebp, %esp
	popl	%ebp
	ret
END_FUNC(ompi_atomic_cmpset_64)


START_FUNC(ompi_atomic_add_32)
        pushl   %ebp
        movl    %esp, %ebp
        movl    8(%ebp), %eax
        movl    12(%ebp), %edx
#APP
        lock addl %edx,(%eax)
#NO_APP
        movl    (%eax), %eax
        leave
        ret
END_FUNC(ompi_atomic_add_32)


START_FUNC(ompi_atomic_sub_32)
        pushl   %ebp
        movl    %esp, %ebp
        movl    8(%ebp), %eax
        movl    12(%ebp), %edx
#APP
        lock subl %edx,(%eax)
#NO_APP
        movl    (%eax), %eax
        leave
        ret
END_FUNC(ompi_atomic_sub_32)
