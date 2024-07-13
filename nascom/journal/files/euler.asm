0005 ; E U L E R
0006 ;
0010 ;Berechnung der Eulerschen Zahl e auf
0020 ;bis zu 501 Stellen Genauigkeit
0030 ;(c) by Joerg Wittich, Donaueschingen
0040 ;
0050 ANZ    EQU 240 ;Anzahl der Glieder (max. 256)
0060 STELLE EQU 155 ;Nachkommastellen/3
0070 LNG    EQU 200 ;Laenge der Var.(>1.25*STELLE)
0080 ;
0090        ORG 1000H
0100        CALL CLEAR
0110        RST 28H
0120        DEFM "Calculating..."
0130        DEFB 0
0140        LD A,1
0150        LD (FAK),A ;FAK:=1
0160        LD (SUM),A ;SUM:=1
0170        LD B,ANZ ;FOR I:=1 TO ANZ DO
0180 LOOP   LD A,ANZ+1
0190        SUB B
0200        CALL DIV ;FAK:=FAK/I
0210        CALL ADD ;SUM:=SUM+FAK
0220        DJNZ LOOP
0230        CALL PRINT ;WRITE(SUM)
0240        SCAL 5BH
0250 ;
0260 CLEAR  LD HL,FAK ;Variablen loeschen
0270        LD DE,FAK+1
0280        LD BC,LNG+LNG-1
0290        LD (HL),0
0300        LDIR
0310        LD A,0CH ;Schirm loeschen
0320        RST 30H
0330        RET
0340 ;
0350 ADD    PUSH BC
0360        LD HL,SUM+LNG
0370        LD DE,FAK+LNG
0380        LD B,LNG
0390 LP     DEC HL
0400        DEC DE
0410        LD A,(DE)
0420        ADC A,(HL)
0430        LD (HL),A
0440        DJNZ LP
0450        POP BC
0460        RET
0470 ;
0480 MUL10  PUSH BC ;Multiplikation mit 10
0490        LD HL,PPUF+LNG
0500        LD BC,LNG
0510 M1     DEC HL
0520        LD E,(HL)
0530        PUSH HL
0540        LD H,0
0550        LD D,H
0560        LD L,E
0570        ADD HL,HL ;*2
0580        ADD HL,HL ;*4
0590        ADD HL,DE ;*5
0600        ADD HL,HL ;*10
0610        LD E,B
0620        ADD HL,DE ;+Uebertrag
0630        LD B,H
0640        LD E,L
0650        POP HL
0660        LD (HL),E
0670        DEC C
0680        JR NZ,M1
0690        POP BC
0700        RET
0710 ;
0720 PRINT  PUSH BC
0730        LD HL,080AH ;Printposition
0740        LD (0C29H),HL
0750        LD HL,SUM ;Kopie in Printpuffer
0760        LD DE,PPUF
0770        LD BC,LNG
0780        LDIR
0790        RST 28H
0800        DEFM "e="
0810        DEFB 0
0820        LD A,(HL)
0830        OR 30H
0840        RST 30H
0850        LD (HL),0
0860        LD A,".
0870        RST 30H
0880        LD BC,300H+STELLE
0890 PL     CALL MUL10
0900        LD A,(HL)
0910        OR 30H
0920        RST 30H
0930        LD (HL),0
0940        DJNZ PL
0950        LD A,",
0960        RST 30H
0970        LD B,3
0980        DEC C
0990        JR NZ,PL
1000        POP BC
1010        RET
1020 ;
1030 DIV    PUSH BC
1040        LD B,LNG
1050        LD C,A
1060        LD DE,FAK
1070        LD L,0
1080 DL     PUSH BC
1090        LD B,8
1100        PUSH DE
1110        LD A,(DE)
1120        LD H,L
1130        LD L,A
1140        XOR A
1150        LD E,A
1160        LD D,C
1170 D1     SRL D
1180        RR E
1190        SBC HL,DE
1200        JR NC,D2
1210        ADD HL,DE
1220 D2     CCF
1230        RLA
1240        DJNZ D1
1250        POP DE
1260        LD (DE),A
1270        INC DE
1280        POP BC
1290        DJNZ DL
1300        POP BC
1310        RET
1320 ;
1330 FAK    DEFS LNG
1340 SUM    DEFS LNG
1350 PPUF   DEFS LNG
