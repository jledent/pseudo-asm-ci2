  0: push 0
  1: call 100
  2: sub SP, SP, 1
  3: println ("0! = " + R0)
  4: push 5
  5: call 100
  6: sub SP, SP, 1
  7: println ("5! = " + R0)
  8: push 10
  9: call 100
 10: sub SP, SP, 1
 11: println ("10! = " + R0)
 12: halt
100: push 1
101: push 1
102: jump_g [BP+2], [BP-1], 106
103: mul [BP+1], [BP+1], [BP+2]
104: add [BP+2], [BP+2], 1
105: jump 102
106: move R0, [BP+1]
107: sub SP, SP, 2
108: ret
