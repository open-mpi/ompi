;;
;; $HEADER$
;;
	
#include <asm.h>

.text

.global spinlock

spinlock:
        push    %ebp
        mov     %esp,%ebp
        mov     0x8(%ebp),%eax
        cmpl    $0x1,(%eax)
        jl      2f
1:
        lock decl (%eax)
        jz      3f
2:
        cmpl    $0x1,(%eax)
        jl      2b
        jmp     1b
3:
        pop     %ebp
        ret

.global spintrylock

spintrylock:
        push    %ebp
        mov     %esp,%ebp
        sub     $0x4,%esp
        mov     0x8(%ebp),%edx
        mov     $0x0,%eax
        cmpl    $0x1,(%edx)
        jl      1f
        lock decl (%edx)
        js      1f
        mov     $0x1,%eax
        jmp     2f
1:
        mov     $0x0,%eax
2:
        mov     %eax,0xfffffffc(%ebp)
        leave
        ret

.global fetchNadd

fetchNadd:
        push    %ebp
        mov     %esp,%ebp
        sub     $0x4,%esp
        mov     0x8(%ebp),%edx
        mov     0xc(%ebp),%eax
        lock xadd %eax,(%edx)
        mov     %eax,0xfffffffc(%ebp)
        leave
        ret

.global fetchNset

fetchNset:
        push    %ebp
        mov     %esp,%ebp
        sub     $0x4,%esp
        mov     0x8(%ebp),%edx
        mov     0xc(%ebp),%eax
        lock xchg %eax,(%edx)
        mov     %eax,0xfffffffc(%ebp)
        leave
        ret


