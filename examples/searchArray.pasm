// Fonction main
  0: malloc R0, 5
  1: move [R0], 9
  2: move [R0+1], 2
  3: move [R0+2], 7
  4: move [R0+3], 4
  5: move [R0+4], 0
  6: push R0
  7: push 5        // tab.length
  8: push [BP+1]   // tab
  9: push 4        // elt
 10: call 100
 11: sub SP, SP, 3
 12: jump_eq R0, 0, 14
 13: println ("Trouvé !")
 14: sub SP, SP, 1
 15: halt
 
// Fonction find(int elt, int[] tab, int tab.length)
100: push 0
101: push 0
102: jump_ge [BP+1], [BP-3], 110
103: jump_eq [BP+2], 1, 110
104: move R0, [BP-2]    // tab dans R0
105: move R1, [BP+1]    // i dans R1
106: jump_neq [R0+R1], [BP-1], 108
107: move [BP+2], 1
108: add [BP+1], [BP+1], 1
109: jump 102
110: move R0, [BP+2]
111: sub SP, SP, 2
112: ret
