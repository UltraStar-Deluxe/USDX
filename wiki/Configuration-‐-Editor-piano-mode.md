In the Editor, `F6` toggles a piano mode, where part of the keyboard can be used to enter pitches directly.
Pressing a key will immediately set the pitch for that note.
In piano mode, the `Play Audio+Clicks functionality` is on `Enter` instead of `P`.

### QWERTZ
By default it maps to QWERTZ.
See the QWERTY section and the QWERT part is identical.

### QWERTY
If you wish to use QWERTY, find and replace the relevant parts with:
```ini
[KeyBindings]
PianoKeysLow=92,97,122,115,120,100,99,118,103,98,104,110,109,107,44,108,46,59,47
PianoKeysHigh=49,113,50,119,51,101,114,53,116,54,121,117,56,105,57,111,48,112,91,61,93
```

It uses these keys:

```
+----+----+----+    +----+----+    +----+----+----+    +----+
| 1  | 2  | 3  |    | 5  | 6  |    | 8  | 9  | 0  |    | =  |
+-+----+----+----+----+----+----+----+----+----+----+----+----+
  | Q  | W  | E  | R  | T  | Y  | U  | I  | O  | P  | [  | ]  |
  +----+----+----+----+----+----+----+----+----+----+----+----+
    | A  | S  | D  |    | G  | H  |    | K  | L  | ;  |
    +-+----+----+----+----+----+----+----+----+----+--+
      | Z  | X  | C  | V  | B  | N  | M  | ,  | .  |
      +----+----+----+----+----+----+----+----+----+
```

And maps them to these notes:
```
+----+----+----+    +----+----+    +----+----+----+    +----+
| F# | G# | A# |    | C# | D# |    | F# | G# | A# |    | C# |
+-+----+----+----+----+----+----+----+----+----+----+----+----+
  | G2 | A2 | B  | C3 | D  | E  | F  | G  | A  | B  | C4 | D4 |
  +----+----+----+----+----+----+----+----+----+----+----+----+
    | F# | G# | A# |    | C# | D# |    | F# | G# | A# |
    +-+----+----+----+----+----+----+----+----+----+--+
      | G1 | A  | B  | C2 | D  | E  | F  | G2 | A2 |
      +----+----+----+----+----+----+----+----+----+
```

The slight overlap between the two rows is deliberate, to prevent having to jump rows too often.
