// Fonction main
0: push 30      // Variable n dans [BP+1]
1: push [BP+1]  // Préparation de l'appel fibo(n)
2: call 100     // Appel fibo(n)
3: pop R7       // Nettoyage de la pile après appel
4: println ("Fibo (" + [BP+1] + ") = " + R0)
5: halt

// Fonction fibo(n)
100: add R1, [BP-1], 1  // R1 = n+1
101: malloc R0, R1      // Alloue un tableau de taille n+1
102: push R0            // Préparation de l'appel fibo(n, tab)
103: push [BP-1]        //
104: call 200           // Appel fibo(n, tab)
105: sub SP, SP, 2      // Nettoyage de la pile après appel
106: ret                // Le résultat est déjà dans R0

// Fonction fibo(n, tab)
200: move R1, [BP-1]          // R1 = n
201: jump_ge [BP-1], 2, 204   // Si n < 2, on renvoie n
202: move R0, R1
203: ret
204: move R2, [BP-2]          // R2 = tab
205: jump_eq [R2+R1], 0, 208  // Si tab[n] != 0, on le renvoie
206: move R0, [R2+R1]
207: ret
208: push 0                   // Résultat final dans [BP+1]
209: sub R0, [BP-1], 1        // R0 = n-1
210: push [BP-2]
211: push R0
212: call 200                 // Appel fibo(n-1, tab)
213: sub SP, SP, 2
214: move [BP+1], R0   // res = fibo(n-1)
215: sub R0, [BP-1], 2        // R0 = n-2
216: push [BP-2]
217: push R0
218: call 200                 // Appel fibo(n-2, tab)
219: sub SP, SP, 2
220: add [BP+1], [BP+1], R0   // res = fibo(n-1) + fibo(n-2)
221: move R2, [BP-2]
222: move R1, [BP-1]
223: move [R2+R1], [BP+1]     // Écrit la valeur de fibo(n) dans tab
224: move R0, [BP+1]          // Valeur de retour dans R0
225: pop R7                   // pop la variable locale res
226: ret
