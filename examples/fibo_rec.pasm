// Fonction main
0: push 12      // Argument dans [BP+1]
1: push [BP+1]  // On push l'argument de fibo
2: call 100     // Appel fibo
3: pop          // Nettoyage de pile après appel
4: println ("Fibo (" + [BP+1] + ") = " + R0)
5: halt

// Fonction fibo(n)
100: jump_ge [BP-1], 2, 103  // Si n < 2, on renvoie n
101: move R0, [BP-1]         // Valeur de retour dans R0
102: ret
103: push 0                  // Résultat final dans res = [BP+1]
104: sub [BP-1], [BP-1], 1   // n -= 1
105: push [BP-1]             // Argument de fibo = n-1
106: call 100                // Appel fibo
107: pop                     // Nettoyage de pile
108: add [BP+1], [BP+1], R0  // res = fibo(n-1)
109: sub [BP-1], [BP-1], 1   // n -= 1
110: push [BP-1]             // Argument de fibo = n-2
111: call 100                // Appel fibo
112: pop                     // Nettoyage de pile
113: add [BP+1], [BP+1], R0  // res = fibo(n-1) + fibo(n-1)
114: move R0, [BP+1]         // Résultat placé dans R0
115: pop                     // On retire la variable res de la pile
116: ret
