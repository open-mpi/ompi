START_FILE
       TEXT

       ALIGN(4)
START_FUNC(opal_atomic_mb)
       mcr p15, 0, r0, c7, c10, 5
       bx      lr
END_FUNC(opal_atomic_mb)


START_FUNC(opal_atomic_rmb)
       mcr p15, 0, r0, c7, c10, 5
       bx      lr
END_FUNC(opal_atomic_rmb)


START_FUNC(opal_atomic_wmb)
       mcr p15, 0, r0, c7, c10, 5
       bx      lr
END_FUNC(opal_atomic_wmb)


START_FUNC(opal_atomic_cmpset_32)
       push    {r4, lr}
       mov     r3, r0
       mov     r0, r1
       mov     r1, r2
       mov     r2, r3
       ldr     r3, REFLSYM(1)
       blx     r3
       movcc   r0, #0
       movcs   r0, #1
       pop     {r4, lr}
       bx      lr
       .align  2
       LSYM(1)
       .word   0xffff0fc0
END_FUNC(opal_atomic_cmpset_32)


START_FUNC(opal_atomic_cmpset_acq_32)
       push    {r4, lr}
       mov     r3, r0
       mov     r0, r1
       mov     r1, r2
       mov     r2, r3
       ldr     r3, REFLSYM(2)
       blx     r3
       movcc   r0, #0
       movcs   r0, #1
       pop     {r4, lr}
       bx      lr
       .align  2
       LSYM(2)
       .word   0xffff0fc0
END_FUNC(opal_atomic_cmpset_acq_32)


START_FUNC(opal_atomic_cmpset_rel_32)
       push    {r4, lr}
       mov     r3, r0
       mov     r0, r1
       mov     r1, r2
       mov     r2, r3
       ldr     r3, REFLSYM(3)
       blx     r3
       movcc   r0, #0
       movcs   r0, #1
       pop     {r4, lr}
       bx      lr
       .align  2
       LSYM(3)
       .word   0xffff0fc0
END_FUNC(opal_atomic_cmpset_rel_32)

START_FUNC(opal_atomic_add_32)
        push    {r4, lr}
        mov     r4, r1
        mov     r2, r0
        LSYM(4)
        ldr     r0, [r2]
        ldr     r3, REFLSYM(5)
        add     r1, r0, r4
        blx     r3
        bcc     REFLSYM(4)
        pop     {r4, lr}
        bx      lr
        .align  2
        LSYM(5)
        .word   0xffff0fc0
END_FUNC(opal_atomic_add_32)


START_FUNC(opal_atomic_sub_32)
        push    {r4, lr}
        mov     r4, r1
        mov     r2, r0
        LSYM(6)
        ldr     r0, [r2]
        ldr     r3, REFLSYM(7)
        sub     r1, r0, r4
        blx     r3
        bcc     REFLSYM(6)
        pop     {r4, lr}
        bx      lr
        .align  2
        LSYM(7)
        .word   0xffff0fc0
END_FUNC(opal_atomic_sub_32)
