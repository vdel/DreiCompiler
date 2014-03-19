start:
  BEQ R0 init                  // saute au code d’initialisation

vmts:
class_Gc:
  DATA Gc_Init 
  DATA Gc_Alloc 
  DATA Gc_gc 
  DATA Gc_colorie_fils 
  DATA Gc_is_block 
  DATA Gc_merge_free_blocks 
  DATA Gc_Free 
  DATA Gc_search_freeblk 
  DATA Gc_blk_next 
  DATA Gc_setWhite 
  DATA Gc_setBlack 
  DATA Gc_isWhite 
  DATA Gc_isBlack 
  DATA Gc_setSize 
  DATA Gc_Size 
  DATA Gc_setUsed 
  DATA Gc_setFree 
  DATA Gc_isUsed 
  DATA Gc_isFree 
  DATA Gc_shiftl 
  DATA Gc_shiftr 
  DATA Gc_wrm_w 
  DATA Gc_rdm_w 
  DATA Gc_rdm_b 
  DATA Gc_Print_Tas 
  DATA Gc_Give_Tas 

methods:

Gc_Give_Tas:
  PSH R31 R30 4
  SYSCALL R29 0 13
  ADDI R1 R29 0
  POP R31 R30 4
  ADDI R30 R30 4               // on libère les arguments
  RET R31 
Gc_Print_Tas:
  PSH R31 R30 4
  SYSCALL R29 0 13
  SYSCALL R29 0 7
  ADDI R28 0 10
  SYSCALL R28 0 6
  POP R31 R30 4
  ADDI R30 R30 4               // on libère les arguments
  RET R31 
Gc_rdm_b:
  PSH R31 R30 4
  LDW R29 R30 4
  LDB R1 R29 0
  ANDI R1 R1 0xff
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_rdm_w:
  PSH R31 R30 4
  LDW R29 R30 4
  LDW R1 R29 0
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_wrm_w:
  PSH R31 R30 4
  LDW R29 R30 4
  LDW R28 R30 8
  STW R29 R28 0
  POP R31 R30 4
  ADDI R30 R30 12              // on libère les arguments
  RET R31 
Gc_shiftr:
  PSH R31 R30 4
  LDW R29 R30 8
  LDW R28 R30 4
  ASH R1 R29 R28
  POP R31 R30 4
  ADDI R30 R30 12              // on libère les arguments
  RET R31 
Gc_shiftl:
  PSH R31 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 80                 // adresse de shiftr dans R1
  LDW R2 R30 12                // x
  PSH R2 R30 4
  LDW R2 R30 12                // y
  SUB R2 R0 R2
  PSH R2 R30 4
  ORIU R31 R0 320
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R31 R30 4
  ADDI R30 R30 12              // on libère les arguments
  RET R31 
Gc_isFree:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 88                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 372
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  ADDI R1 R0 2
  LDW R2 R30 0                 // cont
  AND R1 R2 R1
  ADDI R2 R0 2
  CMP R1 R1 R2
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  ADDI R30 R30 4               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_isUsed:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 72                 // adresse de isFree dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 468
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_setFree:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 88                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 536
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 84                 // adresse de wrm_w dans R1
  LDW R2 R30 12                // b
  PSH R2 R30 4
  ADDI R2 R0 2
  LDW R3 R30 8                 // cont
  OR R2 R3 R2
  PSH R2 R30 4
  ORIU R31 R0 592
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R30 R30 4               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_setUsed:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 88                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 648
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 84                 // adresse de wrm_w dans R1
  LDW R2 R30 12                // b
  PSH R2 R30 4
  ADDI R2 R0 2
  ORIU R3 R0 0xFFFF
  ASHI R3 R3 16
  ORIU R3 R3 0xFFFF
  XOR R2 R2 R3
  LDW R3 R30 8                 // cont
  AND R2 R3 R2
  PSH R2 R30 4
  ORIU R31 R0 720
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R30 R30 4               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_Size:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 88                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 776
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 80                 // adresse de shiftr dans R1
  LDW R2 R30 16                // this
  PSH R1 R30 4
  PSH R2 R30 4
  LDW R1 R2 0                  // adresse de la vmt dans R1
  LDW R1 R1 76                 // adresse de shiftl dans R1
  LDW R2 R30 12                // cont
  PSH R2 R30 4
  ADDI R2 R0 2
  PSH R2 R30 4
  ORIU R31 R0 844
  RET R1 
  ADD R2 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  PSH R2 R30 4
  ADDI R2 R0 2
  PSH R2 R30 4
  ORIU R31 R0 872
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R30 R30 4               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_setSize:
  PSH R31 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 84                 // adresse de wrm_w dans R1
  LDW R2 R30 12                // b
  PSH R2 R30 4
  LDW R2 R30 12                // size
  PSH R2 R30 4
  ORIU R31 R0 936
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R31 R30 4
  ADDI R30 R30 12              // on libère les arguments
  RET R31 
Gc_isBlack:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 88                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 988
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  ADDI R1 R0 1
  LDW R2 R30 0                 // cont
  AND R1 R2 R1
  ADDI R2 R0 1
  CMP R1 R1 R2
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  ADDI R30 R30 4               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_isWhite:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 48                 // adresse de isBlack dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 1084
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_setBlack:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 88                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 1152
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 84                 // adresse de wrm_w dans R1
  LDW R2 R30 12                // b
  PSH R2 R30 4
  ADDI R2 R0 1
  LDW R3 R30 8                 // cont
  OR R2 R3 R2
  PSH R2 R30 4
  ORIU R31 R0 1208
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R30 R30 4               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_setWhite:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 88                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 1264
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 84                 // adresse de wrm_w dans R1
  LDW R2 R30 12                // b
  PSH R2 R30 4
  ADDI R2 R0 0
  LDW R3 R30 8                 // cont
  AND R2 R3 R2
  PSH R2 R30 4
  ORIU R31 R0 1320
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R30 R30 4               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_blk_next:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 56                 // adresse de Size dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 1376
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R2 R30 4                 // p
  ADD R1 R2 R1
  PSH R1 R30 4
  ADDI R1 R0 1
  SUB R1 R0 R1
  PSH R1 R30 4
  LDW R1 R30 4                 // b
  LDW R2 R30 16                // this
  LDW R2 R2 8                  // valeur de max_size dans R2
  LDW R3 R30 16                // this
  LDW R3 R3 4                  // valeur de memory dans R3
  ADD R2 R3 R2
  CMP R1 R1 R2
  BLT R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 else_label_0
  LDW R1 R30 4                 // b
  STW R1 R30 0
  JSR finelse_label_0 
else_label_0:
finelse_label_0:
  LDW R1 R30 0                 // res
  ADDI R30 R30 8               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_search_freeblk:
  PSH R31 R30 4
  ADDI R1 R0 1
  SUB R1 R0 R1
  PSH R1 R30 4
  LDW R1 R30 12                // this
  LDW R1 R1 4                  // valeur de memory dans R1
  PSH R1 R30 4
while_label_1:
  LDW R1 R30 4                 // res
  ADDI R2 R0 1
  SUB R2 R0 R2
  CMP R1 R1 R2
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  LDW R2 R30 0                 // p
  ADDI R3 R0 1
  SUB R3 R0 R3
  CMP R2 R2 R3
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  AND R1 R2 R1
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 finwhile_label_1
  LDW R1 R30 12                // s
  LDW R2 R30 16                // this
  PSH R1 R30 4
  PSH R2 R30 4
  LDW R1 R2 0                  // adresse de la vmt dans R1
  LDW R1 R1 56                 // adresse de Size dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 1640
  RET R1 
  ADD R2 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  CMP R1 R1 R2
  BLE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  LDW R2 R30 16                // this
  PSH R1 R30 4
  PSH R2 R30 4
  LDW R1 R2 0                  // adresse de la vmt dans R1
  LDW R1 R1 72                 // adresse de isFree dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 1704
  RET R1 
  ADD R2 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  AND R1 R2 R1
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_2
  LDW R1 R30 0                 // p
  STW R1 R30 4
  JSR finelse_label_2 
else_label_2:
finelse_label_2:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de blk_next dans R1
  LDW R2 R30 4                 // p
  PSH R2 R30 4
  ORIU R31 R0 1780
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 0
  JSR while_label_1 
finwhile_label_1:
  LDW R1 R30 4                 // res
  ADDI R30 R30 8               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_Free:
  PSH R31 R30 4
  LDW R1 R30 4                 // b
  LDW R2 R30 8                 // this
  LDW R2 R2 8                  // valeur de max_size dans R2
  LDW R3 R30 8                 // this
  LDW R3 R3 4                  // valeur de memory dans R3
  ADD R2 R3 R2
  CMP R1 R1 R2
  BLT R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  LDW R2 R30 4                 // b
  LDW R3 R30 8                 // this
  LDW R3 R3 4                  // valeur de memory dans R3
  CMP R2 R2 R3
  BGE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  AND R1 R2 R1
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_3
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 68                 // adresse de isUsed dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 1948
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 else_label_4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 64                 // adresse de setFree dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 1988
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  JSR finelse_label_4 
else_label_4:
finelse_label_4:
  JSR finelse_label_3 
else_label_3:
finelse_label_3:
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_merge_free_blocks:
  PSH R31 R30 4
  LDW R1 R30 4                 // this
  LDW R1 R1 4                  // valeur de memory dans R1
  PSH R1 R30 4
  ADDI R1 R0 0
  PSH R1 R30 4
  ADDI R1 R0 0
  PSH R1 R30 4
while_label_5:
  LDW R1 R30 8                 // p
  ADDI R2 R0 1
  SUB R2 R0 R2
  CMP R1 R1 R2
  BNE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 finwhile_label_5
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 72                 // adresse de isFree dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  ORIU R31 R0 2112
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 else_label_6
while_label_7:
  LDW R1 R30 0                 // ok
  ADDI R2 R0 1
  CMP R1 R1 R2
  BNE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 finwhile_label_7
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de blk_next dans R1
  LDW R2 R30 8                 // q
  PSH R2 R30 4
  ORIU R31 R0 2184
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 4
  LDW R1 R30 4                 // q
  ADDI R2 R0 1
  SUB R2 R0 R2
  CMP R1 R1 R2
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 else_label_8
  ADDI R1 R0 1
  STW R1 R30 0
  JSR finelse_label_8 
else_label_8:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 68                 // adresse de isUsed dans R1
  LDW R2 R30 8                 // q
  PSH R2 R30 4
  ORIU R31 R0 2272
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 else_label_9
  ADDI R1 R0 1
  STW R1 R30 0
  JSR finelse_label_9 
else_label_9:
finelse_label_9:
finelse_label_8:
  JSR while_label_7 
finwhile_label_7:
  ADDI R1 R0 0
  STW R1 R30 0
  LDW R1 R30 4                 // q
  LDW R2 R30 16                // this
  PSH R1 R30 4
  PSH R2 R30 4
  LDW R1 R2 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de blk_next dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  ORIU R31 R0 2344
  RET R1 
  ADD R2 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  CMP R1 R1 R2
  BNE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 else_label_10
  LDW R1 R30 4                 // q
  ADDI R2 R0 1
  SUB R2 R0 R2
  CMP R1 R1 R2
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 else_label_11
  LDW R1 R30 16                // this
  LDW R1 R1 8                  // valeur de max_size dans R1
  LDW R2 R30 16                // this
  LDW R2 R2 4                  // valeur de memory dans R2
  ADD R1 R2 R1
  STW R1 R30 4
  JSR finelse_label_11 
else_label_11:
finelse_label_11:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 52                 // adresse de setSize dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  LDW R2 R30 16                // p
  LDW R3 R30 12                // q
  SUB R2 R3 R2
  PSH R2 R30 4
  ORIU R31 R0 2488
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 64                 // adresse de setFree dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  ORIU R31 R0 2524
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  JSR finelse_label_10 
else_label_10:
finelse_label_10:
  JSR finelse_label_6 
else_label_6:
finelse_label_6:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de blk_next dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  ORIU R31 R0 2568
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 8
  JSR while_label_5 
finwhile_label_5:
  ADDI R30 R30 12              // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 4               // on libère les arguments
  RET R31 
Gc_is_block:
  PSH R31 R30 4
  ADDI R1 R0 0
  PSH R1 R30 4
  LDW R1 R30 12                // this
  LDW R1 R1 4                  // valeur de memory dans R1
  PSH R1 R30 4
  ADDI R1 R0 4
  LDW R2 R30 12                // b
  MOD R1 R2 R1
  ADDI R2 R0 0
  CMP R1 R1 R2
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  LDW R2 R30 12                // b
  LDW R3 R30 0                 // p
  CMP R2 R2 R3
  BGE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  AND R1 R2 R1
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_12
while_label_13:
  LDW R1 R30 4                 // res
  ADDI R2 R0 0
  CMP R1 R1 R2
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  LDW R2 R30 0                 // p
  ADDI R3 R0 1
  SUB R3 R0 R3
  CMP R2 R2 R3
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  AND R1 R2 R1
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 finwhile_label_13
  LDW R1 R30 0                 // p
  LDW R2 R30 12                // b
  CMP R1 R1 R2
  BEQ R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 else_label_14
  ADDI R1 R0 1
  STW R1 R30 4
  JSR finelse_label_14 
else_label_14:
finelse_label_14:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de blk_next dans R1
  LDW R2 R30 4                 // p
  PSH R2 R30 4
  ORIU R31 R0 2868
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 0
  JSR while_label_13 
finwhile_label_13:
  JSR finelse_label_12 
else_label_12:
finelse_label_12:
  LDW R1 R30 4                 // res
  ADDI R30 R30 8               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_colorie_fils:
  PSH R31 R30 4
  ADDI R1 R0 1
  PSH R1 R30 4
  ADDI R1 R0 0
  PSH R1 R30 4
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 16                 // adresse de is_block dans R1
  LDW R2 R30 16                // b
  PSH R2 R30 4
  ORIU R31 R0 2956
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 else_label_15
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 44                 // adresse de isWhite dans R1
  LDW R2 R30 16                // b
  PSH R2 R30 4
  ORIU R31 R0 2996
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 else_label_16
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 40                 // adresse de setBlack dans R1
  LDW R2 R30 16                // b
  PSH R2 R30 4
  ORIU R31 R0 3036
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R1 R0 4
  LDW R2 R30 16                // this
  PSH R1 R30 4
  PSH R2 R30 4
  LDW R1 R2 0                  // adresse de la vmt dans R1
  LDW R1 R1 56                 // adresse de Size dans R1
  LDW R2 R30 20                // b
  PSH R2 R30 4
  ORIU R31 R0 3080
  RET R1 
  ADD R2 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  SUB R1 R2 R1
  STW R1 R30 4
while_label_17:
  LDW R1 R30 4                 // i
  ADDI R2 R0 4
  CMP R1 R1 R2
  BGE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 finwhile_label_17
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 88                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // i
  LDW R3 R30 16                // b
  ADD R2 R3 R2
  PSH R2 R30 4
  ORIU R31 R0 3168
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 0
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 12                 // adresse de colorie_fils dans R1
  LDW R2 R30 4                 // p
  PSH R2 R30 4
  ORIU R31 R0 3208
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R1 R0 4
  LDW R2 R30 4                 // i
  SUB R1 R2 R1
  STW R1 R30 4
  JSR while_label_17 
finwhile_label_17:
  JSR finelse_label_16 
else_label_16:
finelse_label_16:
  JSR finelse_label_15 
else_label_15:
finelse_label_15:
  ADDI R30 R30 8               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_gc:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  LDW R1 R1 12                 // valeur de memto dans R1
  PSH R1 R30 4
  ADDI R1 R0 0
  PSH R1 R30 4
  LDW R1 R30 12                // b
  LDW R2 R30 16                // this
  LDW R2 R2 4                  // valeur de memory dans R2
  CMP R1 R1 R2
  BGE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 else_label_18
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 12                 // adresse de colorie_fils dans R1
  LDW R2 R30 16                // b
  PSH R2 R30 4
  ORIU R31 R0 3348
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  JSR finelse_label_18 
else_label_18:
finelse_label_18:
  LDW R1 R30 16                // this
  LDW R1 R1 4                  // valeur de memory dans R1
  STW R1 R30 0
while_label_19:
  LDW R1 R30 4                 // p
  ADDI R2 R0 1
  SUB R2 R0 R2
  CMP R1 R1 R2
  BNE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 finwhile_label_19
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 48                 // adresse de isBlack dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 3436
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 else_label_20
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 36                 // adresse de setWhite dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 3476
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  JSR finelse_label_20 
else_label_20:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 64                 // adresse de setFree dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 3516
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
finelse_label_20:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de blk_next dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 3552
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 4
  JSR while_label_19 
finwhile_label_19:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 20                 // adresse de merge_free_blocks dans R1
  ORIU R31 R0 3588
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R30 R30 8               // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_Alloc:
  PSH R31 R30 4
  ADDI R1 R0 4
  ADDI R2 R0 4
  LDW R3 R30 4                 // n
  MOD R2 R3 R2
  LDW R3 R30 4                 // n
  SUB R2 R3 R2
  ADD R1 R2 R1
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 28                 // adresse de search_freeblk dans R1
  LDW R2 R30 4                 // t
  PSH R2 R30 4
  ORIU R31 R0 3676
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  ADDI R1 R0 1
  SUB R1 R0 R1
  PSH R1 R30 4
  LDW R1 R30 20                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 56                 // adresse de Size dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 3728
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 8                 // p
  PSH R1 R30 4
  LDW R1 R30 4                 // t1
  LDW R2 R30 16                // t
  CMP R1 R1 R2
  BGT R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  LDW R2 R30 12                // p
  ADDI R3 R0 1
  SUB R3 R0 R3
  CMP R2 R2 R3
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  AND R1 R2 R1
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_21
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 52                 // adresse de setSize dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  LDW R2 R30 24                // t
  PSH R2 R30 4
  ORIU R31 R0 3868
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 60                 // adresse de setUsed dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  ORIU R31 R0 3904
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de blk_next dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  ORIU R31 R0 3940
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 8
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 52                 // adresse de setSize dans R1
  LDW R2 R30 12                // prochain
  PSH R2 R30 4
  LDW R2 R30 24                // t
  LDW R3 R30 12                // t1
  SUB R2 R3 R2
  PSH R2 R30 4
  ORIU R31 R0 3996
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 64                 // adresse de setFree dans R1
  LDW R2 R30 12                // prochain
  PSH R2 R30 4
  ORIU R31 R0 4032
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R1 R0 4
  LDW R2 R30 0                 // res
  ADD R1 R2 R1
  STW R1 R30 0
  JSR finelse_label_21 
else_label_21:
  LDW R1 R30 12                // p
  ADDI R2 R0 1
  SUB R2 R0 R2
  CMP R1 R1 R2
  BNE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 else_label_22
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 60                 // adresse de setUsed dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  ORIU R31 R0 4124
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 52                 // adresse de setSize dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  LDW R2 R30 24                // t
  PSH R2 R30 4
  ORIU R31 R0 4168
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  ADDI R1 R0 4
  LDW R2 R30 0                 // res
  ADD R1 R2 R1
  STW R1 R30 0
  JSR finelse_label_22 
else_label_22:
finelse_label_22:
finelse_label_21:
  LDW R1 R30 0                 // res
  ADDI R30 R30 20              // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
Gc_Init:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 52                 // adresse de setSize dans R1
  LDW R2 R30 12                // this
  LDW R2 R2 4                  // valeur de memory dans R2
  PSH R2 R30 4
  ADDI R2 R0 2
  LDW R3 R30 16                // this
  LDW R3 R3 8                  // valeur de max_size dans R3
  DIV R2 R3 R2
  PSH R2 R30 4
  ORIU R31 R0 4272
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 64                 // adresse de setFree dans R1
  LDW R2 R30 12                // this
  LDW R2 R2 4                  // valeur de memory dans R2
  PSH R2 R30 4
  ORIU R31 R0 4312
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 4                  // adresse de Alloc dans R1
  LDW R2 R30 8                 // s
  PSH R2 R30 4
  ORIU R31 R0 4348
  RET R1 
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
error_:
  ADDI R1 R0 97
  SYSCALL R1 0 6
  RET R0 
gc_init_:
  ADDI R3 R1 4                 // on met l'adresse du tas + 4
  ADDI R4 R0 5000              // on met la taille du tas
  ORIU R2 R0 4                 // adresse de la VMT
  STW R2 R3 0
  ADD R2 R0 R1                 // this.memory
  STW R2 R3 4
  STW R4 R3 8                  // this.max_size
  DIVI R4 R4 2                 // memto/memfrom
  ADD R4 R2 R4
  STW R4 R3 12                 // this.memto
  LDW R4 R3 0                  // adresse de la vmt dans R3
  LDW R4 R4 0                  // adresse de Init dans R3
  PSH R3 R30 4
  ADDI R2 R0 20                // taille, argument s
  PSH R2 R30 4
  ORIU R31 R0 4444
  RET R4                       // On appelle Init(s)
  BEQ R0 main
main:

  SYSCALL 0 0 15
  RET R0                       // quitte l’emulateur
init:
  SYSCALL R30 0 13             // initialise le pointeur de pile
  ORIU R1 R0 ((init >> 16) & 0xffff)
  LSHI R1 R1 16
  ORIU R1 R1 (init & 0xffff)   // le tas commence en init
  SUB R2 R30 R1                // taille memoire sans le code
  DIVIU R2 R2 (3*4)            // coupe en trois morceaux
  LSHI R2 R2 1                 // deux tiers pour le tas
  ORIU R3 R0 30                // registre de pile =30
  LSHI R3 R3 27
  OR R2 R2 R3
  BEQ R0 gc_init_              // initialisation du GC
