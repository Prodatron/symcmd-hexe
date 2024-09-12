;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@         H E X E   -   H e x   M o n i t o r   a n d   E d i t o r          @
;@                                                                            @
;@             (c) 2017-2021 by Prodatron / SymbiosiS (Jörn Mika)             @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

relocate_start

;==============================================================================
;### CODE-TEIL ################################################################
;==============================================================================

;### PROGRAMM-KOPF ############################################################

prgdatcod       equ 0           ;Länge Code-Teil (Pos+Len beliebig; inklusive Kopf!)
prgdatdat       equ 2           ;Länge Daten-Teil (innerhalb 16K Block)
prgdattra       equ 4           ;Länge Transfer-Teil (ab #C000)
prgdatorg       equ 6           ;Original-Origin
prgdatrel       equ 8           ;Anzahl Einträge Relocator-Tabelle
prgdatstk       equ 10          ;Länge Stack (Transfer-Teil beginnt immer mit Stack)
prgdatrs1       equ 12          ;*reserved* (3 bytes)
prgdatnam       equ 15          ;program name (24+1[0] chars)
prgdatflg       equ 40          ;flags (+1=16colour icon available)
prgdat16i       equ 41          ;file offset of 16colour icon
prgdatrs2       equ 43          ;*reserved* (5 bytes)
prgdatidn       equ 48          ;"SymExe10"
prgdatcex       equ 56          ;zusätzlicher Speicher für Code-Bereich
prgdatdex       equ 58          ;zusätzlicher Speicher für Data-Bereich
prgdattex       equ 60          ;zusätzlicher Speicher für Transfer-Bereich
prgdatres       equ 62          ;*reserviert* (28 bytes)
prgdatism       equ 90          ;Icon (klein)
prgdatibg       equ 109         ;Icon (gross)
prgdatlen       equ 256         ;Datensatzlänge

prgpstdat       equ 6           ;Adresse Daten-Teil
prgpsttra       equ 8           ;Adresse Transfer-Teil
prgpstspz       equ 10          ;zusätzliche Prozessnummern (4*1)
prgpstbnk       equ 14          ;Bank (1-8)
prgpstmem       equ 48          ;zusätzliche Memory-Bereiche (8*5)
prgpstnum       equ 88          ;Programm-Nummer
prgpstprz       equ 89          ;Prozess-Nummer

prgcodbeg   dw prgdatbeg-prgcodbeg  ;Länge Code-Teil
            dw prgtrnbeg-prgdatbeg  ;Länge Daten-Teil
            dw prgtrnend-prgtrnbeg  ;Länge Transfer-Teil
prgdatadr   dw #1000                ;Original-Origin                    POST Adresse Daten-Teil
prgtrnadr   dw relocate_count       ;Anzahl Einträge Relocator-Tabelle  POST Adresse Transfer-Teil
prgprztab   dw prgstk-prgtrnbeg     ;Länge Stack                        POST Tabelle Prozesse
            dw 0                    ;*reserved*
prgbnknum   db 0                    ;*reserved*                         POST Bank-Nummer
            db "HexE":ds 20:db 0 ;Name
            db 0                    ;flags (+1=16c icon)
            dw 0                    ;16c icon offset
            ds 5                    ;*reserved*
prgmemtab   db "SymExe10"           ;SymbOS-EXE-Kennung                 POST Tabelle Speicherbereiche
            dw 0                    ;zusätzlicher Code-Speicher
            dw 0                    ;zusätzlicher Data-Speicher
            dw 0                    ;zusätzlicher Transfer-Speicher
            ds 28                   ;*reserviert*
prgicnsml   db 2, 8, 8:ds  16
prgicnbig   db 6,24,24:ds 144


txttit  db 13,10,"HexE 1.0 - very simple Hex Monitor and Editor",13,10,13,10,0

txttyp  db "(R)ead/(W)rite: ",0
txtbnk  db "Bank (0-15):    ",0
txtadr  db "Address:        ",0
txtman  db "Type in space seperated hex (#, ##) or decimal values (###)",13,10
        db "and press enter (Example: F9 C3 123 250 0 7A)",13,10
        db "Quit with empty input",13,10,0
txtinp  db "0000  ",0
txterr  db "Incorrect number",13,10,0

;### PRGPRZ -> Programm-Prozess
sysprzn db 3    ;Nummer des System-Prozesses
shlprzn db 0    ;Nummer des Shell-Prozesses
shlxlen db 80   ;Breite der Shell
shlylen db 25   ;Höhe   der Shell

membuf  ds 512
memtxt  db "0000  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  XXXXXXXXXXXXXXXX",13,10,0

prgprz  call prgpar         ;angehangene Parameter und Shell-Prozess, -Höhe und -Breite holen
        push de
        call prgshl

        ld hl,txttit        ;title text
        ld d,0
        call strout
        jp c,prgend

        pop de
        call monpar
        ld a,(adrmod)
        dec a
        jr z,prgprz1
        call memlod
        call hexplt
        jr prgend
prgprz1 call memwrt

;### PRGEND -> Programm beenden
prgend  ld a,(prgprzn)          ;Shell mitteilen, das sich Prozess beendet
        db #dd:ld l,a
        ld a,(shlprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        ld (iy+0),MSC_SHL_EXIT
        ld (iy+1),0
        rst #10
prgend0 ld a,(prgprzn)          ;Programm beenden
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        ld iy,prgmsgb
        ld (iy+0),MSC_SYS_PRGEND
        ld a,(prgcodbeg+prgpstnum)
        ld (iy+1),a
        rst #10
prgend1 rst #30                 ;Warten, bis beendet
        jr prgend1

;### PRGSHL -> Holt Shell-Parameter
;### Eingabe    (prgflgtab)=Flag-Liste (Byte0/1=Zeiger, Byte2=Len), E=Anzahl Flags
;### Ausgabe    (shlprzn), (shlxlen), (shlylen) gefüllt
prgshl  ld a,e
        or a
        jp z,prgend             ;keine Flags vorhanden -> Ende
        ld ix,prgflgtab
        ld bc,3
prgshl1 ld l,(ix+0)
        ld h,(ix+1)
        ld a,(hl):cp "s":jr nz,prgshl2:inc hl
        ld a,(hl):cp "p":jr  z,prgshl3
prgshl2 add ix,bc
        dec e
        jr nz,prgshl1
        jp prgend               ;keine Shell-Daten gefunden -> Ende
prgshl3 inc hl
        call prgshl4
        ld (shlprzn),a
        call prgshl4
        ld (shlxlen),a
        call prgshl4
        ld (shlylen),a
        ret
;(HL)=2stellige Zahl -> A=Zahl, HL=HL+2
prgshl4 call prgshl5
        add a
        ld d,a
        add a:add a
        add d
        ld d,a
        call prgshl5
        add d
        ret
prgshl5 ld a,(hl)
        inc hl
        sub "0"
        cp 10
        ret c
        sub 7
        ret

;### PRGPAR -> Erstellt Parameter-Liste aus angehangenem String
;### Ausgabe    (prgpartab)=Parameter-Liste, (prgflgtab)=Flag-Liste (Byte0/1=Zeiger, Byte2=Len),
;###            D=Anzahl Parameter, E=Anzahl Flags, ZF=1 -> keine Parameter
            db 0
prgpartab   ds 8*3
prgflgtab   ds 8*3
prgparanz   dw 0

prgpar  ld hl,(prgcodbeg)   ;nach angehängter Grafik suchen
        ld de,prgcodbeg
        dec h
        add hl,de           ;HL=CodeEnde=Pfad
        ld ix,prgpartab     ;IX=Parameter Liste
        ld iy,prgflgtab     ;IX=Flag      Liste
        ld bc,8*256+8       ;B=8-Anzahl Parameter, C=8-Anzahl Flags
prgpar1 push bc
        call prgpar2
        pop bc
        jr z,prgpar7
        ld a,(hl)
        cp "%"
        jr z,prgpar6
        ld (ix+0),l         ;Parameter
        ld (ix+1),h
        push hl
        call prgpar4
        pop hl
        ld (ix+2),e
        ld de,3
        add ix,de
        dec b
        jr nz,prgpar1
        jr prgpar7
prgpar6 inc hl              ;Flag
        ld (iy+0),l
        ld (iy+1),h
        push hl
        call prgpar4
        pop hl
        ld (iy+2),e
        ld de,3
        add iy,de
        dec c
        jr nz,prgpar1
prgpar7 ld a,8
        sub c
        ld e,a
        ld a,8
        sub b
        ld d,a
        or e
        ld (prgparanz),de
        ret
;HL=Position im String -> zum nächsten String springen -> HL=Nächster String, ZF=1 Ende erreicht
prgpar2 ld a,(hl)
        inc hl
        or a
        ret z
        cp " "
        jr nz,prgpar2
        dec hl
        ld (hl),0
prgpar3 inc hl
        ld a,(hl)
        cp " "
        jr z,prgpar3
        or a
        ret
;HL=Position im String -> E=Länge bis Ende
prgpar4 ld e,0
prgpar5 ld a,(hl)
        or a
        ret z
        cp " "
        ret z
        inc e
        inc hl
        jr prgpar5

;### PRGERR -> Beendet Prozess, falls Fehler oder EOF
prgerr  jp nz,prgend    ;EOF
        jp c,prgend     ;Fehler
        ret

;### MONPAR -> Reads mode, bank and address
monpar  ld a,d
        cp 3
        jr c,monpar3
        ld iy,(3*0+prgpartab+0)
        call monpar1
        ld iy,(3*1+prgpartab+0)
monpar2 call clch16
        ld (adrmem+0),hl
        ret
monpar1 call clcr32
        db #dd:ld a,l
        ld (adrbnk),a
        ret

monpar3 ld hl,txttyp
        ld d,0
        call strout
        jp c,prgend
        ld d,0
        call strinp         ;ask for type
        call prgerr
        ld a,(msgstr)
        call clcucs
        cp "R"
        ld c,0
        jr z,monpar4
        cp "W"
        jr nz,monpar3
        ld c,1
monpar4 ld a,c
        ld (adrmod),a
        ld hl,txtbnk
        ld d,0
        call strout
        jp c,prgend
        ld d,0
        call strinp         ;ask for bank
        call prgerr
        ld iy,msgstr
        call monpar1
        ld hl,txtadr
        ld d,0
        call strout
        jp c,prgend
        ld d,0
        call strinp         ;ask for address
        call prgerr
        ld iy,msgstr
        jr monpar2

;### MEMLOD -> Loads 512 bytes
adrmod  db 0    ;0=read, 1=write
adrbnk  db 0
adrmem  ds 2

memlod  ld a,(prgbnknum)
        add a:add a:add a:add a
        ld hl,adrbnk
        add (hl)
        ld hl,(adrmem)
        ld de,membuf
        ld bc,512
        rst #20:dw #8130
        ret

;### MEMWRT -> writes bytes to memory
memwrtf db 0

memwrt  ld hl,txtman
        ld d,0
        call strout
        jp c,prgend
memwrt1 ld hl,(adrmem)
        ld de,txtinp
        push de
        ld a,h
        call hexplt6
        ld a,l
        call hexplt6
        pop hl
        ld d,0
        call strout
        jp c,prgend
        ld d,0
        call strinp
        ld ix,msgstr
        ld a,(ix+0)
        or a
        ret z
memwrt3 ld a,(ix+1)
        cp 32
        jr z,memwrt5
        or a
        jr z,memwrt5
        ld a,(ix+2)
        cp 32
        jr z,memwrt7
        or a
        jr z,memwrt7
        ld a,(ix+3)
        cp 32
        jr z,memwrtb
        or a
        jr z,memwrtb
memwrt4 ld hl,txterr
        ld d,0
        call strout
        jr memwrt1
memwrtb xor a               ;** 3digit dec
        ld l,(ix+0)
        call memwrt6
        jr c,memwrt4
        inc ix
        ld l,(ix+0)
        call memwrt6
        jr c,memwrt4
        inc ix
        ld l,(ix+0)
        call memwrt6
        jr c,memwrt4
memwrtc ld b,a              ;a=byte -> write
        inc ix:inc ix
        ld hl,(adrmem)
        ld a,(adrbnk)
        rst #20:dw jmp_bnkwbt
        ld (adrmem),hl
        ld a,(ix-1)
        or a
        jr nz,memwrt3
        jp memwrt1
memwrt7 xor a               ;** 2digit hex
        ld l,(ix+0)
        call memwrta
        jr c,memwrt4
        inc ix
        jr memwrt9
memwrt5 xor a               ;** 1digit hex
memwrt9 ld l,(ix+0)
        call memwrta
        jr c,memwrt4
        jr memwrtc

memwrta add a               ;a = a*16+hex(l)
        add a:add a:add a
        ld c,a
        ld a,l
        call clcucs
        sub "0"
        ret c
        cp 10
        jr c,memwrtd
        sub "A"-"0"
        ret c
        cp 6
        ccf
        ret c
        add 10
memwrtd add c
        ret

memwrt6 add a               ;a = a*10+dec(l)
        ld c,a
        add a
        add a
        add c
        ld c,a
        ld a,l
        sub "0"
        ret c
        cp 10
        ccf
        ret c
        add c
        ret

;### HEXPLT -> Dumps 512 bytes
hexplt  ld de,(adrmem)
        ld hl,membuf
        ld b,32
hexplt0 push bc
        ld a,b
        cp 16
        jr nz,hexplt1
        push hl
        push de
        ld d,0
        call chrinp
        call prgerr
        pop de
        pop hl
hexplt1 push de
        push hl
        ex de,hl
        ld de,memtxt
        ld a,h
        call hexplt6
        ld a,l
        call hexplt6
        inc de:inc de
        pop hl
        ld b,16
hexplt2 ld a,(hl)
        inc hl
        call hexplt6
        inc de
        djnz hexplt2
        ld bc,-16
        add hl,bc
        ld b,16
hexplt3 ld a,(hl)
        inc hl
        cp 32
        jr c,hexplt4
        cp 128
        jr c,hexplt5
hexplt4 ld a,127
hexplt5 inc de
        ld (de),a
        djnz hexplt3        ;hl=next membuf
        push hl
        ld hl,memtxt
        ld d,0
        call strout
        jp c,prgend
        pop de
        pop hl
        ld bc,16
        add hl,bc
        ex de,hl
        pop bc
        djnz hexplt0
        ret
hexplt6 ld c,a          ;a=number -> (DE)=hexdigits, DE=DE+2
        rlca:rlca:rlca:rlca
        call hexplt7
        ld a,c
hexplt7 and 15
        add "0"
        cp "9"+1
        jr c,hexplt8
        add "A"-"9"-1
hexplt8 ld (de),a
        inc de
        ret

;==============================================================================
;### SUB-ROUTINEN #############################################################
;==============================================================================

msgstr  ds 256

;### CHROUT -> Zeichen ausgeben
;### Eingabe    E=Zeichen, D=Kanal (0=Standard, 1=Bildschirm)
;### Ausgabe    CF=0 ok, CF=1 Fehler
chrout  ld a,(shlprzn)
        ld c,MSC_SHL_CHROUT
        call msgsnd
        ld b,MSR_SHL_CHROUT
        jr strinp1

;### STROUT -> String ausgeben
;### Eingabe    HL=String, D=Kanal (0=Standard, 1=Bildschirm)
;### Ausgabe    CF=0 ok, CF=1 Fehler
strout  push hl
        call clclen
        pop hl
        ld b,c
        ld a,(prgbnknum)
        ld e,a
        ld a,(shlprzn)
        ld c,MSC_SHL_STROUT
        call msgsnd
        ld b,MSR_SHL_STROUT
        jr strinp1

;### CHRINP -> Zeichen anfordern
;### Eingabe    D=Kanal (0=Standard, 1=Keyboard)
;### Ausgabe    ZF=0 EOF, CF=0 ok (A=Zeichen), CF=1 Fehler
chrinp  ld bc,MSR_SHL_CHRINP*256+MSC_SHL_CHRINP
        jr strinp0

;### STRINP -> String anfordern
;### Eingabe    D=Kanal (0=Standard, 1=Keyboard)
;### Ausgabe    ZF=0 EOF, CF=0 ok (msgstr=String), CF=1 Fehler
strinp  ld bc,MSR_SHL_STRINP*256+MSC_SHL_STRINP
        ld hl,msgstr
strinp0 ld a,(prgbnknum)
        ld e,a
        ld a,(shlprzn)
        push bc
        call msgsnd
        pop bc
strinp1 push bc
        call msgget
        pop bc
        jr nc,strinp1
        ld c,a
        ld a,(shlprzn)
        db #dd:cp h
        jr nz,strinp1
        ld a,c
        cp b
        jr nz,strinp1
        ld a,(iy+3)         ;Fehlerstatus
        ld c,(iy+1)         ;EOF (0=nein)
        cp 1
        ccf             ;CF=0 kein Fehler
        inc c
        dec c           ;ZF=0 EOF
        ret

;### MSGGET -> Message für Programm abholen
;### Ausgabe    CF=0 -> keine Message vorhanden, CF=1 -> IXH=Absender, (prgmsgb)=Message, A=(prgmsgb+0), IY=prgmsgb
;### Veraendert 
msgget  ld a,(prgprzn)
        db #dd:ld l,a           ;IXL=Rechner-Prozeß-Nummer
        db #dd:ld h,-1
        ld iy,prgmsgb           ;IY=Messagebuffer
        rst #08                 ;Message holen -> IXL=Status, IXH=Absender-Prozeß
        or a
        db #dd:dec l
        ret nz
        ld iy,prgmsgb
        ld a,(iy+0)
        or a
        jp z,prgend
        scf
        ret

;### MSGSND -> Message an Prozess senden
;### Eingabe    A=Prozess, C=Kommando, D=Kanal, E=Bank/Zeichen, HL=Adresse (, B=Länge)
msgsnd  db #dd:ld h,a
        ld a,(prgprzn)
        db #dd:ld l,a
        ld iy,prgmsgb
        ld (iy+0),c
        ld (iy+1),d
        ld (iy+2),e
        ld (iy+3),l
        ld (iy+4),h
        ld (iy+5),b
        rst #10
        ret

;### SYSCLL -> Betriebssystem-Funktion aufrufen
;### Eingabe    (SP)=Modul/Funktion, AF,BC,DE,HL,IX,IY=Register
;### Ausgabe    AF,BC,DE,HL,IX,IY=Register
sysclln db 0
syscll  ld (prgmsgb+04),bc      ;Register in Message-Buffer kopieren
        ld (prgmsgb+06),de
        ld (prgmsgb+08),hl
        ld (prgmsgb+10),ix
        ld (prgmsgb+12),iy
        push af
        pop hl
        ld (prgmsgb+02),hl
        pop hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        inc hl
        push hl
        ld (prgmsgb+00),de      ;Modul und Funktion in Message-Buffer kopieren
        ld a,e
        ld (sysclln),a
        ld iy,prgmsgb
        ld a,(prgprzn)          ;Desktop und System-Prozessnummer holen
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #10                 ;Message senden
syscll1 rst #30
        ld iy,prgmsgb
        ld a,(prgprzn)
        db #dd:ld l,a
        ld a,(sysprzn)
        db #dd:ld h,a
        rst #18                 ;auf Antwort warten
        db #dd:dec l
        jr nz,syscll1
        ld a,(prgmsgb)
        sub 128
        ld e,a
        ld a,(sysclln)
        cp e
        jr nz,syscll1
        ld hl,(prgmsgb+02)      ;Register aus Message-Buffer holen
        push hl
        pop af
        ld bc,(prgmsgb+04)
        ld de,(prgmsgb+06)
        ld hl,(prgmsgb+08)
        ld ix,(prgmsgb+10)
        ld iy,(prgmsgb+12)
        ret

;### CLCLEN -> Ermittelt Länge eines Strings
;### Eingabe    HL=String
;### Ausgabe    HL=Stringende (0), BC=Länge (maximal 255)
;### Verändert  -
clclen  push af
        xor a
        ld bc,255
        cpir
        ld a,254
        sub c
        ld c,a
        dec hl
        pop af
        ret

;### CLCH16 -> converts HEX string (teminated by 0) into 16Bit-number (unsigned)
;### Input      IY=string
;### Output     HL=number, CF=1 error/overflow
clch16  ld hl,0
clch161 ld a,(iy+0)
        inc iy
        or a
        ret z
        call clcucs
        sub "0"
        ret c
        cp 10
        jr c,clch162
        sub "A"-"0"
        ret c
        cp 6
        ccf
        ret c
        add 10
clch162 add hl,hl:ret c
        add hl,hl:ret c
        add hl,hl:ret c
        add hl,hl:ret c
        ld c,a
        ld b,0
        add hl,bc
        ret c
        jr clch161

;### CLCR32 -> Converts ASCII-String (teminated by 0) into 32Bit-number (unsigned)
;### Input      IY=string
;### Output     HL,IX=number, CF=1 overflow
clcr32  ld ix,0
        ld hl,0
clcr321 ld a,(iy+0)
        or a
        ret z
        add ix,ix:adc hl,hl:ret c
        db #dd:ld c,l
        db #dd:ld b,h
        ld e,l
        ld d,h
        add ix,ix:adc hl,hl:ret c
        add ix,ix:adc hl,hl:ret c
        add ix,bc:adc hl,de:ret c   ;HL,IX*=10
        sub "0"
        ld c,a
        ld b,0
        add ix,bc
        ld c,b
        adc hl,bc:ret c             ;HL,IX+=digit
        inc iy
        jr clcr321

;### CLCUCS -> Wandelt Klein- in Großbuchstaben um
;### Eingabe    A=Zeichen
;### Ausgabe    A=ucase(Zeichen)
;### Verändert  F
clcucs  cp "a"
        ret c
        cp "z"+1
        ret nc
        add "A"-"a"
        ret



;==============================================================================
;### DATEN-TEIL ###############################################################
;==============================================================================

prgdatbeg

;### Verschiedenes
db 0

;==============================================================================
;### TRANSFER-TEIL ############################################################
;==============================================================================

prgtrnbeg
;### PRGPRZS -> Stack für Programm-Prozess
        ds 128
prgstk  ds 6*2
        dw prgprz
prgprzn db 0
prgmsgb ds 14

prgtrnend

relocate_table
relocate_end
