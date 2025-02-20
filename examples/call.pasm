// main
0: println ("Debut main")
1: call 100
2: println ("Fin main")
3: halt

// Fonction f
100: println ("Debut f")
101: call 200
102: println ("Milieu f")
103: call 200
104: println ("Fin f")
105: ret

// Fonction g
200: println ("Fonction g")
201: ret
