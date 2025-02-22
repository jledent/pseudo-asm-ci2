// main : Calcul de Σ_{i=0}^25000 i
0: push 25000
1: call 100
2: sub SP, SP, 1
3: println ("Résultat = " + R0)
4: halt

// sum(n)
100: push 0
101: push [BP-1]
102: call 200
103: sub SP, SP, 2
104: ret

// sum(n, acc)
200: jump_neq [BP-1], 0, 203  // If n = 0
201: move R0, [BP-2]          // Return acc
202: ret
203: sub R1, [BP-1], 1        // R1 = n - 1
204: add R2, [BP-2], [BP-1]   // R2 = acc + n
// 205: sub SP, SP, 3         // Optimisation de l'appel récursif terminal
206: push R2
207: push R1
208: call 200                 // return sum(n-1, acc+n)
209: sub SP, SP, 2
210: ret