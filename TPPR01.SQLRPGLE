000100210810     h copyright('© Banco Occidental de Descuento, B.U. 2013')
000200210810     h DFTACTGRP(*no) ACTGRP(*caller)
000300210810     H Option(*Nodebugio:*Srcstmt)
000400210810     HDEBUG(*YES)
000500210810     HDATFMT(*ISO)
000600210810     hDatEdit(*YMD)
000700210113     ?*?--------------------------------------------------------------------
000800201018      *? B.O.D. BANCO UNIVERSAL                                    *
000900201129      *? Vicepresidencia de Ingenieria y Transformación        *
001000210113     ?*?--------------------------------------------------------------------
001100210526      * Programa   : Recepcion de Recarga CC
001200210526      * Fecha        : 26 / 05 / 2021
001300210113      * Creado por : Ing. Jesús Romero.
001400201018     ?*?--------------------------------------------------------------------
001500210601     FMNTOR     O  A E           K DISK
001600210601     fCntrlBth  if   e           K DISK
001700210510     ?*?-------------------------------------------------------------------
001800210601     d                sds
001900210601     d NomPgm                  1     10
002000210601     d Job                   244    253
002100210601     d User_Id               254    263
002200210601     d*-----------------------------------------------------------------
002300210601     d Sel             S            300A   Inz(*Blanks)
002400210601     d FechaDia2       S              8a   Inz(*Blanks)
002500210601     ?*?-------------------------------------------------------------------
002600210601     d FechaDia        ds             8
002700210601     d anod                           4  0
002800210601     d mesd                           2  0
002900210601     d diad                           2  0
003000210601     d*-----------------------------------------------------------------
003100210601     D                 DS
003200210601     DWfechaii                        8S 0 Inz
003300210601     D WfeciY                         4S 0 Overlay(Wfechaii:1)
003400210601     D WfeciM                         2S 0 Overlay(Wfechaii:*next)
003500210601     D WfeciD                         2S 0 Overlay(Wfechaii:*next)
003600210601     d*-----------------------------------------------------------------
003700210810     D Dia_Jul         S              3S 0
003800210810     d*-----------------------------------------------------------------
003900210601     d NomArchivo      s             50a   Inz(*Blanks)
004000210805     d @tam            s              4s 0 Inz(*Zeros)
004100210601     d Tamano          s             10a   Inz(*Blanks)
004200210601     d @emp            s              4a   Inz(*Blanks)
004300210601     d @ban            s              6s 0 Inz(*Blanks)
004400210601     d @nom            s              4a   Inz(*Blanks)
004500210601     d Biblioteca      s             50a   Inz(*Blanks)
004600210601     d Nom400          s             50a   Inz(*Blanks)
004700210601     d Salida          s             50a   Inz(*Blanks)
004800210601     d Etiqueta        s             50a   Inz(*Blanks)
004900210601     d Programa        s             50a   Inz(*Blanks)
005000210601     d CondError       s             50a   Inz(*Blanks)
005100210617     d*-----------------------------------------------------------------
005200210617      *?Programa que hace reverso de en caso de Rechazo
005300210707     d TPRev           PR                  Extpgm('CM4950R')
005400210617     d*-----------------------------------------------------------------
005500210601      *?Programa que hace la transferencia
005600210601     D SSH003          PR                  ExtPgm('SSH003')
005700210601     DP_Biblioteca                   10
005800210601     DP_Nom400                       10
005900210601     DP_NomArchivo                   50
006000210601     DP_Salida                        1
006100210601     DP_Etiqueta                     30
006200210601     DP_Programa                     10
006300210601     DP_Longitud                     10
006400210601     DP_CondError                    10
006500210601     d*-----------------------------------------------------------------
006600210510     D @TPPVR          S             50A
006700210510     D @FILLER         S           1890A
006800211124     D @VR1            S             10A     Inz(*zeros)
006900211124     D @VR2            S             10A     Inz(*zeros)
007000211124     D @VR8            S             10A     Inz(*zeros)
007100211124     D @VR9            S             10A     Inz(*zeros)
007200211124     D @VRR            S             10A
007300210601     d rg              s              2  0 Inz(*zeros)
007400211026     d @FECGEN         s              8  0 Inz(*zeros)
007500211223     D @LIBRERIA       s             50a   Inz(*Blanks)
007600210601     D TOTAL           s             10
007700210601     D*?--------------------------------------------------------------------
007800210601     c*?Rutina Principal del Programa
007900210601     c*?--------------------------------------------------------------------
008000210601     c                   ExSr      RutIni
008100210601     c                   ExSr      CheqCC
008200210601     c                   ExSr      RutPro
008300210601     c                   ExSr      ParseCC
008400210601     c                   ExSr      VrCC
008500210601     c                   ExSr      ActCC
008600210601     c                   ExSr      Auditoria
008700210601     c                   Eval      *Inlr = '1'
008800210601     c*?--------------------------------------------------------------------
008900210601     c*?Sub Rutina Inicial
009000210601     c*?--------------------------------------------------------------------
009100210601     c     RutIni        BegSr
009200210601     c     User_Id       Chain     Cntrlbth
009300210601     c                   Movel     BthUbk        wbanco            2
009400210601     c                   Move      *Date         FechaDia
009500210601     c                   move      *Date         FechaDia2
009600210601     c                   move      *date         wfechaii
009700210601     c*
009800210601     c/Exec SQL          Set Option Commit = *None, NamIng = *Sys,
009900210601     c+                  CloSqlCsr = *EndMod
010000210601     c/End-Exec
010100210601     c*
010200210601     c                   EndSr
010300210601     c
010400210601     ?*?-------------------------------W1-----------------------------------
010500210601     c     RutPro        BegSr
010600210601      /FREE
010700210601
010800211223        Biblioteca = @LIBRERIA;
010900210806        Nom400     = 'TPPRW                       ';
011000210806        NomArchivo = 'RG915' + %char(dia_jul) +  %EDITC(RG:'X');
011100210806        Salida     = 'R';
011200211223        Etiqueta   = 'SSHTPP';
011300210806        Programa   = 'TPPR01  ';
011400210806        CondError  = '          ';
011500210601        @tam = 2000;
011600210601        Tamano = %char(@tam);
011700210806
011800210601        SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
011900210601               Programa: Tamano: CondError);
012000210601     c                   EndSr
012100210601     ?*?-------------------------------W1-----------------------------------
012200210601       BegSr ParseCC;
012300210526
012400210526       EXEC SQL
012500210526
012600210526        INSERT INTO TPRDAR01
012700210810         SELECT SUBSTR(CAMPO, 1, 2 ) AS TPHTRG,
012800210810                    SUBSTR(CAMPO, 3, 2) AS TPHRAR,
012900210810                    INT(REPLACE(CHAR(CURRENT DATE , ISO),'-','')) AS TPHFGA,
013000210810                    SUBSTR(CAMPO, 36, 2)  AS TPHNCD,
013100211108                    SUBSTR(CAMPO, 61, 50),
013200210810                    SUBSTR(CAMPO, 1742, 259) AS FILLER
013300210526                    FROM TPPRW
013400210526                    WHERE SUBSTR(CAMPO, 1, 2) = '01';
013500211108
013600211108       EXEC SQL
013700211124
013800211124        SELECT LOCATE_IN_STRING(TRIM(SUBSTR(TPHVDR,1,7)),'*') as VR1 ,
013900211124        LOCATE_IN_STRING(TRIM(SUBSTR(TPHVDR,8,1)),'*')   AS VR8,
014000211124        LOCATE_IN_STRING(TRIM(SUBSTR(TPHVDR,9,1)),'*')  AS VR9
014100211124        INTO :@VR1, :@VR8, :@VR9
014200211124        FROM TPRDAR01
014300211124        WHERE DATE(TIMESTAMP_FORMAT(CHAR(TPHFGA),'YYYYMMDD'))= CURRENT_DATE
014400211124        AND TPHNCD = :RG;
014500210601     ?*?-------------------------------W2-----------------------------------
014600211108
014700211124       IF @VR1 =  '0'  ;
014800211108
014900211108        EXEC SQL
015000210601
015100211108         INSERT INTO TPRDAR02
015200211108       SELECT SUBSTR(CAMPO, 1, 2) AS TPPTREG,
015300210810       SUBSTR(CAMPO, 25, 1) AS TPPTIPA,
015400210810       CHAR(INT(SUBSTR(CAMPO, 26, 12))) AS TPPDNIN,
015500210810       SUBSTR(CAMPO, 88, 19) AS TPPNPAN,
015600210810       CAST(SUBSTR(CAMPO, 107, 13) AS DEC(13, 2)) / 100 AS TPPMNTO,
015700210810       SUBSTR(CAMPO, 120, 3) AS TPPMCCY,
015800210810       SUBSTR(CAMPO, 123, 10) AS TPPCHKN,
015900210810       SUBSTR(CAMPO, 133, 8) AS TPPFEPR,
016000210810       SUBSTR(CAMPO, 141, 6) AS TPPHOPR,
016100210810       SUBSTR(CAMPO, 147, 1) AS TPPCODR,
016200210810       SUBSTR(CAMPO, 148, 50) AS TPPVDRC,
016300210810       SUBSTR(CAMPO, 88, 19) AS TPPNACC,
016400210810       CAST(SUBSTR(CAMPO, 107, 13) AS DEC(13, 2)) / 100 AS TPPMTEN,
016500210810       SUBSTR(CAMPO, 230, 3) AS TPPTTRA,
016600210810       SUBSTR(CAMPO, 239, 10) AS TPPUSI1,
016700210810       SUBSTR(CAMPO, 249, 10) AS TPPUSI2,
016800210810       SUBSTR(CAMPO, 259, 1742) AS FILLERC
016900210810       FROM TPPRW
017000210810        WHERE SUBSTR(CAMPO, 1, 2) = '02';
017100211108
017200211108       ELSE;
017300211108
017400211108       EXEC SQL
017500211108
017600211108        INSERT INTO TPRDAR02
017700211108        SELECT SUBSTR(CAMPO, 1, 2) AS TPPTREG,
017800211108       SUBSTR(CAMPO, 25, 1) AS TPPTIPA,
017900211108       CHAR(INT(SUBSTR(CAMPO, 26, 12))) AS TPPDNIN,
018000211108       SUBSTR(CAMPO, 88, 19) AS TPPNPAN,
018100211108       CAST(SUBSTR(CAMPO, 107, 13) AS DEC(13, 2)) / 100 AS TPPMNTO,
018200211108       SUBSTR(CAMPO, 120, 3) AS TPPMCCY,
018300211108       SUBSTR(CAMPO, 123, 10) AS TPPCHKN,
018400211108       SUBSTR(CAMPO, 133, 8) AS TPPFEPR,
018500211108       SUBSTR(CAMPO, 141, 6) AS TPPHOPR,
018600211108       'R' AS TPPCODR,
018700211123       SUBSTR(CAMPO, 148, 50) AS TPPVDRC,
018800211108       SUBSTR(CAMPO, 88, 19) AS TPPNACC,
018900211108       CAST(SUBSTR(CAMPO, 107, 13) AS DEC(13, 2)) / 100 AS TPPMTEN,
019000211108       SUBSTR(CAMPO, 230, 3) AS TPPTTRA,
019100211108       SUBSTR(CAMPO, 239, 10) AS TPPUSI1,
019200211108       SUBSTR(CAMPO, 249, 10) AS TPPUSI2,
019300211108       SUBSTR(CAMPO, 259, 1742) AS FILLERC
019400211108       FROM TPPRW
019500211108        WHERE SUBSTR(CAMPO, 1, 2) = '02';
019600211108
019700211108       ENDIF;
019800211108
019900210511           ENDSR;
020000210526
020100210601     ?*?-------------------------------VR-----------------------------------
020200210526       BegSr VrCC;
020300210601
020400210526       EXEC SQL
020500210810       SELECT LOCATE_IN_STRING(TPHVDR,'*') INTO:@VR1 FROM TPRDAR01
020600211108       WHERE DATE(TIMESTAMP_FORMAT(CHAR(TPHFGA),'YYYYMMDD'))= CURRENT_DATE
020700211108       AND TPHNCD=:RG;
020800211108
020900210526       EXEC SQL
021000210810       SELECT LOCATE_IN_STRING(TPPVDRC,'*')  INTO:@VR2 FROM TPRDAR02
021100211108       WHERE DATE(TIMESTAMP_FORMAT(CHAR(TPPFEPR),'YYYYMMDD'))= CURRENT_DATE;
021200211124
021300211123          ENDSR;
021400210511
021500210601     ?*?-----------------------------INZ-----------------------------------
021600210526       BegSr CheqCC;
021700210810
021800211119         EXEC SQL DELETE  FROM TPPRW;
021900211124         EXEC SQL DELETE  FROM TPRDAR01;
022000211223
022100211223         EXEC SQL SELECT TPMETDEC INTO :@LIBRERIA FROM TPMETBLS
022200211223         WHERE TPMETCOD= 'LIBRERIA';
022300210810
022400211119         EXEC SQL
022500211119
022600211119         SELECT   CHAR(
022700211119         SUBSTR(DIGITS(YEAR(DATE(TIMESTAMP_FORMAT
022800211119         (CHAR(TPFECGEN), 'YYYYMMDD')))), 04, 01) ||
022900211119         DIGITS(dec( JULIAN_DAY(DATE(TIMESTAMP_FORMAT
023000211119        (CHAR(TPFECGEN), 'YYYYMMDD'))) - JULIAN_DAY(CHAR
023100211119        (CHAR(YEAR(DATE(TIMESTAMP_FORMAT(CHAR(TPFECGEN),
023200211119         'YYYYMMDD')))), 04) || '-01-01') + 1, 03,00)), 4) AS
023300211119          JULIAN, TPFECGEN INTO :dia_jul , :@FECGEN FROM TPRDA01;
023400211025
023500210810           EXEC SQL
023600210810           SELECT COUNT(*) INTO :RG  FROM TPHERDA
023700210810           WHERE TPFECGEN = :@FECGEN AND TPTDA='RG';
023800210601
023900210526           ENDSR;
024000211119
024100210601     ?*-----------------------------RESP----------------------------------
024200220112
024300210601           BegSr ActCC;
024400210601
024500210601                EXEC SQL
024600210526            MERGE INTO TPRDA02 A USING TPRDAR02 B
024700210526                ON (A.TPPCEDREG = B.TPPDNIN
024800210526                AND A.TPPNPAN = B.TPPNPAN
024900210526                AND A.TPPIDREC = B.TPPCHKN)
025000220112              WHEN MATCHED THEN UPDATE SET A.TPPCODRESP = B.TPPCODR,
025100220112                    A.TPPVR = B.TPPVDRC, A.TPPFECPRO=B.TPPFEPR,
025200220112                    A.TPPHORPRO=B.TPPHOPR;
025300220112
025400211124           IF  @VR1 <> '0' OR  @VR2<> '0' ;
025500211124
025600211124            TPRev ();
025700211124
025800211124           ENDIF;
025900211124
026000210526           ENDSR;
026100210113
026200210601      /END-FREE
026300210601     c*?---------------------------------------------------------------
026400210601     c*?Graba Trazas de Auditoria
026500210601     c*?---------------------------------------------------------------
026600210601     c     Auditoria     Begsr
026700210601     c*?   Graba registro en Mntor
026800210601     c                   Clear                   rmntor
026900210601     c                   Eval      MntBnk = wbanco
027000211123     c                   eval      MntFil = 'TPRDA02'
027100210601     c                   eval      Mntusr = User_Id
027200210601     c                   Time                    MntSyt
027300210601     c                   eval      MntSyd = Uday
027400210601     c                   eval      MntSym = Umonth
027500210601     c                   eval      MntSYy = Uyear
027600210601     c                   eval      MntMod = 'CARGA'
027700210601     c                   eval      MntMnu = 'SCHEDUL'
027800210601     c                   eval      MntPrg = 'TPPR01'
027900210601     c                   eval      MntOpt = *Zeros
028000210601     c                   eval      MntPrc = 'TPPR01'
028100210601     c                   eval      MntCrt = Job
028200210601     c                   Write     Rmntor
028300210601     c                   Endsr
