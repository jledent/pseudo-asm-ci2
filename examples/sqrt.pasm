 0: move R0, 150
 1: move R1, 0        // Résultat
 2: move R2, 1        // Compteur nombres impairs
 3: move R3, 0        // Somme des nombres impairs
 4: add R3, R3, R2     
 5: jump_g R3, R0, 9  // Tant que (somme + compteur <= R0)
 6: add R2, R2, 2     //   compteur += 2
 7: add R1, R1, 1     //   resultat++
 8: jump 4            // Fin tant que
 9: print ("sqrt(" + R0 + ") = " + R1)
10: halt
