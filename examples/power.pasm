  0: move R0, 3
  1: move R1, 7
  2: move R2, 1        // R2 : Resultat final
  3: jump_eq R1, 0, 7  // Tant que R1 != 0, faire :
  4: mul R2, R2, R0    //    R2 = R2 * R0
  5: sub R1, R1, 1     //    R1--
  6: jump 3            // Revenir au debut de la boucle
  7: print ("3 puissance 7 = " + R2)  // Resultat : 2187
  8: halt