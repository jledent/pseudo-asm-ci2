  0: move R0, 12  
  1: move R1, 0         // R1 = Fib(0)
  2: move R2, 1         // R2 = Fib(1)
  3: move R3, 1         // Compteur d'itérations
  4: jump_eq R3, R0, 10 // Quand R3 = R0, on a fini
  5: add R4, R1, R2     // R4 = R1 + R2, prochain terme de la suite
  6: move R1, R2        // On decale les valeurs de R1 et R2
  7: move R2, R4        //
  8: add R3, R3, 1      // Decremente le nombre d'etapes restantes
  9: jump 4             // Iteration suivante
 10: println ("Fibonacci(" + R0 + ") = " + R2)
 11: halt
