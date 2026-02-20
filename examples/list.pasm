// Fonction main :
  0: malloc R0, 2
  1: move [R0], 3
  2: push R0         // Cellule c1 dans [BP+1]
  3: malloc R0, 2
  4: move [R0], 4
  5: move [R0+1], [BP+1]
  6: push R0         // Cellule c2 dans [BP+2]
  7: malloc R0, 2
  8: move [R0], 2
  9: move [R0+1], [BP+2]
 10: push R0         // Cellule c3 dans [BP+3]
 11: malloc R0, 2
 12: move [R0], 6
 13: push R0         // Cellule c4 dans [BP+4]
 14: malloc R0, 1
 15: move [R0], [BP+3]
 16: push R0         // Liste l dans [BP+5]
 17: push 7
 18: push [BP+5]
 19: call 100        // Appel ajouter(l, 7)
 20: pop 2
 21: halt

// Fonction ajouter (Liste this, int x)
100: move R0, [BP-1]     // R0 = this
101: jump_neq [R0], 0, 106
102: malloc R1, 2        //
103: move [R1], [BP-2]   // R1 = new Cellule(x, null)
104: move [R0], R1       // this.tete = R1
105: jump 110
106: push [BP-2]  // push x
107: push [R0]    // push this.tete
108: call 200     // ajouter(this.tete, x)
109: pop 2
110: ret

// Fonction ajouter (Cellule this, int x)
200: push [BP-1]             // Variable tmp dans [BP+1], initialisée à this
201: move R0, [BP+1]         // R0 = tmp
202: jump_eq [R0+1], 0, 205  // while (R0.next != null)
203: move [BP+1], [R0+1]     //    tmp = tmp.next
204: jump 201                // Fin du while
205: move R0, [BP+1]         // R0 = tmp
206: malloc R1, 2
207: move [R1], [BP-2]       // R1 = new Cellule(x, null)
208: move [R0+1], R1         // tmp.next = R1
209: pop
210: ret
