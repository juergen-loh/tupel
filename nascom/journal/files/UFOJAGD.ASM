* GERHARD BAIER
* ___ ERLANGEN
* ______-_______-___.__
*
* ------------------------------------
DELAY   EQU     0FFFFH
PRS     EQU     0EFH
GESCHW  EQU     0FH
IN      EQU     62DFH
RIN     EQU     0CFH
ROUT    EQU     0F7H
CS      EQU     0CH
UFOJGD  ORG     0C80H
        DB      PRS,CS
        DB      'START (G):'
        DB      00H
UFOJGD1 DB      RIN
        CMP     47H
        JR      NZ,UFOJGD1
        DB      PRS,CS,00H
        LD      HL,UFOTXT
        LD      DE,0BCAH
        LD      BC,0027H
        LDIR
UFOJGD2 LD      BC,0100H
        EXX
        LD      BC,013DH
        LD      IX,088AH
UFOJGD3 LD      IY,0BAAH
        LD      (IY-01H),03H
        LD      (IY+00H),5EH
        LD      (IY+01H),4CH
UFOJGD4 DW      DELAY
        DJNZ    UFOJGD5
GESCHA  LD      B,GESCHW
        LD      (IX-05H),20H
        LD      (IX-04H),17H
        LD      (IX-03H),05H
        LD      (IX-02H),05H
        LD      (IX-01H),05H
        LD      (IX+00H),3EH
        INC     IX
        DEC     C
        LD      A,00H
        CMP     C
        JR      NZ,UFOJGD5
        LD      C,3DH
        LD      A,32H
        LD      HL,0BD7H
        CMP     (HL)
        JMP     Z,UFOJGD1
        INC     HL
        CALL    ZAEHL
        LD      IX,088AH
UFOJGD5 EXX
        LD      A,00H
        CMP     C
        JR      NZ,UFOJGD6
        DW      IN
        JR      NC,UFOJGD9
        LD      C,0FH
UFOJGD6 DJNZ    UFOJGD9
        LD      (IY+00H),20H
        DEC     C
        LD      A,00H
        CMP     C
        JR      NZ,UFOJGD7
        INC     B
        LD      C,00H
        EXX
        JR      UFOJGD3
UFOJGD7 LD      B,06H
        PUSH    BC
        LD      B,40H
UFOJGD8 DEC     IY
        DJNZ    UFOJGD8
        POP     BC
        LD      A,(IY+00H)
        CMP     20H
        JR      NZ,$+0AH
        LD      (IY+00H),5EH
UFOJGD9 EXX
        JMP     UFOJGD4
        LD      HL,0BEF
        CALL    ZAEHL
        CALL    BLITZ
        LD      A,32H
        LD      HL,0BD7H
        CMP     (HL)
        JMP     Z,UFOJGD1
        INC     HL
        CALL    ZAEHL
        JMP     UFOJGD2
*
*
* UNTERPROGRAMME
*
CLSCR   LD      D,20H
CLSCR0  LD      HL,0BBAH
CLSCR1  LD      A,L
        CMP     0AH
        JR      NZ,CLSCR2
        LD      A,H
        CMP     08H
        RET     Z
CLSCR2  DEC     HL
        LD      (HL),D
        JR      CLSCR1
*
ZAEHL   LD      A,R
        AND     0AH
        OR      04H
        LD      (GESCHA+1),A
        LD      DE,3039H
        LD      A,(HL)
        INC     (HL)
        CMP     E
        RET     NZ
        LD      (HL),D
        DEC     HL
        INC     (HL)
        RET
*
BLITZ   LD      B,03H
BLITZ1  LD      D,2AH
        CALL    CLSCR0
        CALL    WAIT
        CALL    CLSCR
        CALL    WAIT
        DJNZ    BLITZ1
        RET
*
WAIT    PUSH    BC
        LD      B,06H
WAIT1   DW      DELAY
        DJNZ    WAIT1
        POP     BC
        RET
*
UFOTXT  DB      'UFO-JAGD     '
        DB      '01. UFO    '
        DB      'Abschuesse: 00 '
        END     UFOJGD
