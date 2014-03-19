start:
  BEQ R0 init                  // saute au code d’initialisation

vmts:
class_Gc:
  DATA Gc_Init 
  DATA Gc_Alloc 
  DATA Gc_merge_free_blocks 
  DATA Gc_Free 
  DATA Gc_search_freeblk 
  DATA Gc_blk_next 
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
class_Ad:

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
  LDW R1 R1 52                 // adresse de shiftr dans R1
  LDW R2 R30 12                // x
  PSH R2 R30 4
  LDW R2 R30 12                // y
  SUB R2 R0 R2
  PSH R2 R30 4
  ORIU R31 R0 292              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R31 R30 4
  ADDI R30 R30 12              // on libère les arguments
  RET R31 
Gc_isFree:
  PSH R31 R30 4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 60                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 344              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 0                 // cont
  ANDI R1 R1 2
  CMPI R1 R1 2
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
  LDW R1 R1 44                 // adresse de isFree dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 432              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
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
  LDW R1 R1 60                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 500              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 56                 // adresse de wrm_w dans R1
  LDW R2 R30 12                // b
  PSH R2 R30 4
  LDW R2 R30 8                 // cont
  ORI R2 R2 2
  PSH R2 R30 4
  ORIU R31 R0 552              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
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
  LDW R1 R1 60                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 608              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 56                 // adresse de wrm_w dans R1
  LDW R2 R30 12                // b
  PSH R2 R30 4
  LDW R2 R30 8                 // cont
  ADDI R3 R0 -3
  AND R2 R2 R3
  PSH R2 R30 4
  ORIU R31 R0 664              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
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
  LDW R1 R1 60                 // adresse de rdm_w dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 720              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 52                 // adresse de shiftr dans R1
  LDW R2 R30 16                // this
  PSH R1 R30 4
  PSH R2 R30 4
  LDW R1 R2 0                  // adresse de la vmt dans R1
  LDW R1 R1 48                 // adresse de shiftl dans R1
  LDW R2 R30 12                // cont
  PSH R2 R30 4
  ADDI R2 R0 2
  PSH R2 R30 4
  ORIU R31 R0 788              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R2 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  PSH R2 R30 4
  ADDI R2 R0 2
  PSH R2 R30 4
  ORIU R31 R0 816              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
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
  LDW R1 R1 56                 // adresse de wrm_w dans R1
  LDW R2 R30 12                // b
  PSH R2 R30 4
  LDW R2 R30 12                // size
  PSH R2 R30 4
  ORIU R31 R0 880              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R31 R30 4
  ADDI R30 R30 12              // on libère les arguments
  RET R31 
Gc_blk_next:
  PSH R31 R30 4
  LDW R1 R30 4                 // p
  LDW R2 R30 8                 // this
  PSH R1 R30 4
  PSH R2 R30 4
  LDW R1 R2 0                  // adresse de la vmt dans R1
  LDW R1 R1 28                 // adresse de Size dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  ORIU R31 R0 940              // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R2 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  ADD R1 R1 R2
  PSH R1 R30 4
  ORIU R1 R0 ((-1>>16) & 0xFFFF)
  ASHI R1 R1 16
  ORIU R1 R1 (-1 & 0xFFFF)
  PSH R1 R30 4
  LDW R1 R30 4                 // b
  LDW R2 R30 16                // this
  LDW R2 R2 4                  // valeur de memory dans R2
  LDW R3 R30 16                // this
  LDW R3 R3 8                  // valeur de max_size dans R3
  ADD R2 R2 R3
  CMP R1 R1 R2
  BLT R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
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
  ORIU R1 R0 ((-1>>16) & 0xFFFF)
  ASHI R1 R1 16
  ORIU R1 R1 (-1 & 0xFFFF)
  PSH R1 R30 4
  LDW R1 R30 12                // this
  LDW R1 R1 4                  // valeur de memory dans R1
  PSH R1 R30 4
while_label_1:
  LDW R1 R30 0                 // p
  ORIU R2 R0 ((-1>>16) & 0xFFFF)
  ASHI R2 R2 16
  ORIU R2 R2 (-1 & 0xFFFF)
  CMP R1 R1 R2
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  LDW R2 R30 4                 // res
  ORIU R3 R0 ((-1>>16) & 0xFFFF)
  ASHI R3 R3 16
  ORIU R3 R3 (-1 & 0xFFFF)
  CMP R2 R2 R3
  BEQ R3 3
  ADDI R3 R0 0
  BSR 2 
  ADDI R3 R0 1
  AND R1 R1 R2
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 finwhile_label_1
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 44                 // adresse de isFree dans R1
  LDW R2 R30 4                 // p
  PSH R2 R30 4
  ORIU R31 R0 1212             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R2 R30 12                // s
  LDW R3 R30 16                // this
  PSH R2 R30 4
  PSH R1 R30 4
  PSH R3 R30 4
  LDW R1 R3 0                  // adresse de la vmt dans R1
  LDW R1 R1 28                 // adresse de Size dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  ORIU R31 R0 1260             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R3 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  POP R2 R30 4
  CMP R2 R2 R3
  BLE R3 3
  ADDI R3 R0 0
  BSR 2 
  ADDI R3 R0 1
  AND R1 R1 R2
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
  LDW R1 R1 20                 // adresse de blk_next dans R1
  LDW R2 R30 4                 // p
  PSH R2 R30 4
  ORIU R31 R0 1360             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
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
  LDW R2 R2 4                  // valeur de memory dans R2
  CMP R1 R1 R2
  BGE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  LDW R2 R30 4                 // b
  LDW R3 R30 8                 // this
  LDW R3 R3 4                  // valeur de memory dans R3
  LDW R4 R30 8                 // this
  LDW R4 R4 8                  // valeur de max_size dans R4
  ADD R3 R3 R4
  CMP R2 R2 R3
  BLT R3 3
  ADDI R3 R0 0
  BSR 2 
  ADDI R3 R0 1
  AND R1 R1 R2
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_3
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 40                 // adresse de isUsed dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 1528             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 else_label_4
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 36                 // adresse de setFree dans R1
  LDW R2 R30 8                 // b
  PSH R2 R30 4
  ORIU R31 R0 1568             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
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
  ORIU R2 R0 ((-1>>16) & 0xFFFF)
  ASHI R2 R2 16
  ORIU R2 R2 (-1 & 0xFFFF)
  CMP R1 R1 R2
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 finwhile_label_5
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 44                 // adresse de isFree dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  ORIU R31 R0 1696             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  BEQ R1 else_label_6
while_label_7:
  LDW R1 R30 0                 // ok
  CMPI R1 R1 1
  BNE R1 3
  ADDI R1 R0 0
  BSR 2 
  ADDI R1 R0 1
  BEQ R1 finwhile_label_7
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 20                 // adresse de blk_next dans R1
  LDW R2 R30 8                 // q
  PSH R2 R30 4
  ORIU R31 R0 1764             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 4
  LDW R1 R30 4                 // q
  ORIU R2 R0 ((-1>>16) & 0xFFFF)
  ASHI R2 R2 16
  ORIU R2 R2 (-1 & 0xFFFF)
  CMP R1 R1 R2
  BEQ R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_8
  ADDI R1 R0 1
  STW R1 R30 0
  JSR finelse_label_8 
else_label_8:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 40                 // adresse de isUsed dans R1
  LDW R2 R30 8                 // q
  PSH R2 R30 4
  ORIU R31 R0 1856             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
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
  LDW R1 R1 20                 // adresse de blk_next dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  ORIU R31 R0 1928             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R2 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R1 R30 4
  CMP R1 R1 R2
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_10
  LDW R1 R30 4                 // q
  ORIU R2 R0 ((-1>>16) & 0xFFFF)
  ASHI R2 R2 16
  ORIU R2 R2 (-1 & 0xFFFF)
  CMP R1 R1 R2
  BEQ R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_11
  LDW R1 R30 16                // this
  LDW R1 R1 4                  // valeur de memory dans R1
  LDW R2 R30 16                // this
  LDW R2 R2 8                  // valeur de max_size dans R2
  ADD R1 R1 R2
  STW R1 R30 4
  JSR finelse_label_11 
else_label_11:
finelse_label_11:
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 24                 // adresse de setSize dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  LDW R2 R30 12                // q
  LDW R3 R30 16                // p
  SUB R2 R2 R3
  PSH R2 R30 4
  ORIU R31 R0 2076             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 16                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 36                 // adresse de setFree dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  ORIU R31 R0 2112             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
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
  LDW R1 R1 20                 // adresse de blk_next dans R1
  LDW R2 R30 12                // p
  PSH R2 R30 4
  ORIU R31 R0 2156             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 8
  JSR while_label_5 
finwhile_label_5:
  ADDI R30 R30 12              // on libère les variables
  POP R31 R30 4
  ADDI R30 R30 4               // on libère les arguments
  RET R31 
Gc_Alloc:
  PSH R31 R30 4
  ADDI R1 R0 4
  LDW R2 R30 4                 // n
  MODI R2 R2 4
  SUB R1 R1 R2
  LDW R2 R30 4                 // n
  ADD R1 R1 R2
  PSH R1 R30 4
  LDW R1 R30 12                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 16                 // adresse de search_freeblk dans R1
  LDW R2 R30 4                 // t
  PSH R2 R30 4
  ORIU R31 R0 2248             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  ORIU R1 R0 ((-1>>16) & 0xFFFF)
  ASHI R1 R1 16
  ORIU R1 R1 (-1 & 0xFFFF)
  PSH R1 R30 4
  LDW R1 R30 20                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 28                 // adresse de Size dans R1
  LDW R2 R30 8                 // p
  PSH R2 R30 4
  ORIU R31 R0 2304             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  PSH R1 R30 4
  LDW R1 R30 8                 // p
  PSH R1 R30 4
  LDW R1 R30 12                // p
  ORIU R2 R0 ((-1>>16) & 0xFFFF)
  ASHI R2 R2 16
  ORIU R2 R2 (-1 & 0xFFFF)
  CMP R1 R1 R2
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  LDW R2 R30 4                 // t1
  LDW R3 R30 16                // t
  CMP R2 R2 R3
  BGT R3 3
  ADDI R3 R0 0
  BSR 2 
  ADDI R3 R0 1
  AND R1 R1 R2
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_12
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 24                 // adresse de setSize dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  LDW R2 R30 24                // t
  PSH R2 R30 4
  ORIU R31 R0 2448             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de setUsed dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  ORIU R31 R0 2484             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 20                 // adresse de blk_next dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  ORIU R31 R0 2520             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  STW R1 R30 8
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 24                 // adresse de setSize dans R1
  LDW R2 R30 12                // prochain
  PSH R2 R30 4
  LDW R2 R30 12                // t1
  LDW R3 R30 24                // t
  SUB R2 R2 R3
  PSH R2 R30 4
  ORIU R31 R0 2576             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 36                 // adresse de setFree dans R1
  LDW R2 R30 12                // prochain
  PSH R2 R30 4
  ORIU R31 R0 2612             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 0                 // res
  ADDI R1 R1 4
  STW R1 R30 0
  JSR finelse_label_12 
else_label_12:
  LDW R1 R30 12                // p
  ORIU R2 R0 ((-1>>16) & 0xFFFF)
  ASHI R2 R2 16
  ORIU R2 R2 (-1 & 0xFFFF)
  CMP R1 R1 R2
  BNE R2 3
  ADDI R2 R0 0
  BSR 2 
  ADDI R2 R0 1
  BEQ R1 else_label_13
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 32                 // adresse de setUsed dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  ORIU R31 R0 2704             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 28                // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 24                 // adresse de setSize dans R1
  LDW R2 R30 16                // p
  PSH R2 R30 4
  LDW R2 R30 24                // t
  PSH R2 R30 4
  ORIU R31 R0 2748             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 0                 // res
  ADDI R1 R1 4
  STW R1 R30 0
  JSR finelse_label_13 
else_label_13:
finelse_label_13:
finelse_label_12:
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
  LDW R1 R1 24                 // adresse de setSize dans R1
  LDW R2 R30 12                // this
  LDW R2 R2 4                  // valeur de memory dans R2
  PSH R2 R30 4
  LDW R2 R30 16                // this
  LDW R2 R2 8                  // valeur de max_size dans R2
  PSH R2 R30 4
  ORIU R31 R0 2840             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 36                 // adresse de setFree dans R1
  LDW R2 R30 12                // this
  LDW R2 R2 4                  // valeur de memory dans R2
  PSH R2 R30 4
  ORIU R31 R0 2880             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  LDW R1 R30 8                 // this
  PSH R1 R30 4
  LDW R1 R1 0                  // adresse de la vmt dans R1
  LDW R1 R1 4                  // adresse de Alloc dans R1
  LDW R2 R30 8                 // s
  PSH R2 R30 4
  ORIU R31 R0 2916             // on calcule l'adresse à laquelle on saute
  RET R1                       // on saute
  ADD R1 R0 R1                 // ON LOAD LES REg-iSters ! *
  POP R31 R30 4
  ADDI R30 R30 8               // on libère les arguments
  RET R31 
error_:
  ADDI R1 R0 97
  SYSCALL R1 0 6
  RET R0 
main:
  ADDI R1 R0 4                 // new
  SYSCALL R1 R1 12
  ORIU R2 R0 80
  STW R2 R1 0
  PSH R1 R30 4
  ADDI R1 R0 16                // create array
  SYSCALL R1 R1 12
  ORIU R2 R0 12
  STW R2 R1 0
  ADDI R2 R0 1
  STW R2 R1 4
  ADDI R2 R0 3
  STW R2 R1 8
  ADDI R2 R0 45
  STW R2 R1 12
  PSH R1 R30 4
  ADDI R1 R0 16                // create array
  SYSCALL R1 R1 12
  ORIU R2 R0 12
  STW R2 R1 0
  LDW R2 R30 0                 // s
  STW R2 R1 4
  LDW R2 R30 0                 // s
  STW R2 R1 8
  LDW R2 R30 0                 // s
  STW R2 R1 12
  PSH R1 R30 4
  ADDI R1 R0 1
  MULI R1 R1 4
  LDW R2 R30 4                 // s
  LDW R3 R2 0                  // taille du tableau
  SUB R3 R1 R3                 // on vérifie qu'on sort pas du tableau
  BGE R3 error_
  ADD R1 R1 R2                 // adresse voulue
  LDW R1 R1 4
  SYSCALL R1 0 7
  ADDI R30 R30 12              // on dépile les variables déclarées dans le bloc

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
  SYSCALL R1 R2 11             // initialise le GC de l'emulateur
  BEQ R0 main                  // saute a la fonction principale
