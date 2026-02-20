0: malloc R0, 11
1: move R1, 0
2: mul [R0+R1], R1, R1
3: add R1, R1, 1
4: jump_l R1, 11, 2
5: move R2, 0
6: println ("Indice " + R2 + " : " + [R0+R2])
7: add R2, R2, 1
8: jump_l R2, 11, 6
9: halt
