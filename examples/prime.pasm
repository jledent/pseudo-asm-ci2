 0: move R0, 103
 2: move R1, 2         // R1 : diviseur a tester
 4: mod R2, R0, R1     // R2 = R0 % R1
 6: jump_eq R2, 0, 18  // Si R2 == 0 : on a trouve un diviseur
 8: add R1, R1, 1      // Sinon : on incremente R1
10: mul R3, R1, R1     // R3 = R1 * R1
12: jump_le R3, R0, 4  // Si R1 <= sqrt(R0), on recommence
14: println ("Premier")
16: halt
18: println ("Pas premier")
20: halt
