0: move R0, 147
1: move R1, 0
2: jump_eq R0, 0, 8
3: mod R2, R0, 2
4: push R2
5: div R0, R0, 2
6: add R1, R1, 1
7: jump 2
8: jump_eq R1, 0, 13
9: print [SP-1]
10: pop
11: sub R1, R1, 1
12: jump 8
13: println ()
14: halt
