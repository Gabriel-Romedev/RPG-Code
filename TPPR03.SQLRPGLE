000200210727     h DFTACTGRP(*no) ACTGRP(*caller)
000300210727     H Option(*Nodebugio:*Srcstmt)
000400210727     HDEBUG(*YES)
000500210727     HDATFMT(*ISO)
000600210806     hDatEdit(*YMD)
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
003700210601     D Dia_Jul         S              3S 0
003800211217     D Dia_JulD        S              3S 0
003900210601     d*-----------------------------------------------------------------
004000210601     d NomArchivo      s             50a   Inz(*Blanks)
004100210805     d @tam            s              4s 0 Inz(*Zeros)
004200210601     d Tamano          s             10a   Inz(*Blanks)
004300210601     d @emp            s              4a   Inz(*Blanks)
004400210601     d @ban            s              6s 0 Inz(*Blanks)
004500210601     d @nom            s              4a   Inz(*Blanks)
004600210601     d Biblioteca      s             50a   Inz(*Blanks)
004700210601     d Nom400          s             50a   Inz(*Blanks)
004800210601     d Salida          s             50a   Inz(*Blanks)
004900210601     d Etiqueta        s             50a   Inz(*Blanks)
005000210601     d Programa        s             50a   Inz(*Blanks)
005100210601     d CondError       s             50a   Inz(*Blanks)
005200210617     d*-----------------------------------------------------------------
005300210617      *?Programa que hace reverso de en caso de Rechazo
005400210707     d TPRev           PR                  Extpgm('XX')
005500210617     d*-----------------------------------------------------------------
005600210601      *?Programa que hace la transferencia
005700210601     D SSH003          PR                  ExtPgm('SSH003')
005800210601     DP_Biblioteca                   10
005900210601     DP_Nom400                       10
006000210601     DP_NomArchivo                   50
006100210601     DP_Salida                        1
006200210601     DP_Etiqueta                     30
006300210601     DP_Programa                     10
006400210601     DP_Longitud                     10
006500210601     DP_CondError                    10
006600210601     d*-----------------------------------------------------------------
006700210510     D @TPPVR          S             50A
006800210510     D @FILLER         S           1890A
006900211223     D @LIBRERIA       s             50a   Inz(*Blanks)
007000210529     D @VR1            S             10
007100210529     D @VR2            S             10
007200211123     d @FECGEN         s              8  0 Inz(*zeros)
007300211123     d @FECGEND        s              8  0 Inz(*zeros)
007400210809     d aj              s              2  0 Inz(*zeros)
007500210809     D TOTAJ           s             10  0
007600210601     D*?--------------------------------------------------------------------
007700210601     c*?Rutina Principal del Programa
007800210601     c*?--------------------------------------------------------------------
007900210601     c                   ExSr      RutIni
008000210601     c                   ExSr      CheqCC
008100210601     c                   ExSr      RutPro
008200210601     c                   ExSr      ParseCC
008300210601     c                   ExSr      VrCC
008400210601     c                   ExSr      ActCC
008500210601     c                   ExSr      Auditoria
008600210601     c                   Eval      *Inlr = '1'
008700210601     c*?--------------------------------------------------------------------
008800210601     c*?Sub Rutina Inicial
008900210601     c*?--------------------------------------------------------------------
009000210601     c     RutIni        BegSr
009100210601     c     User_Id       Chain     Cntrlbth
009200210601     c                   Movel     BthUbk        wbanco            2
009300210601     c                   Move      *Date         FechaDia
009400210601     c                   move      *Date         FechaDia2
009500210601     c                   move      *date         wfechaii
009600210601     c*
009700210601     c/Exec SQL          Set Option Commit = *None, NamIng = *Sys,
009800210601     c+                  CloSqlCsr = *EndMod
009900210601     c/End-Exec
010000210601     c*
010100210601     c                   EndSr
010200210601     c
010300210601     ?*?-------------------------------W1-----------------------------------
010400210601     c     RutPro        BegSr
010500210601      /FREE
010600210809         //----Recepcion de Ajustes
010700210809
010800211223        Biblioteca = @LIBRERIA;
010900210809        Nom400     = 'TPAJW                       ';
011000210809        NomArchivo = 'AJ915' + %char(dia_jul) +  %EDITC(AJ:'X');
011100210806        Salida     = 'R';
011200220113        Etiqueta   = 'SSHTPP';
011300220113        Programa   = 'TPPR03  ';
011400210806        CondError  = '          ';
011500210601        @tam = 2000;
011600210601        Tamano = %char(@tam);
011700210806
011800210601        SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
011900210601               Programa: Tamano: CondError);
012000220113
012100210809       EXEC SQL
012200210809       SELECT COUNT (*) INTO:TOTAJ FROM  TPAJW;
012300210809
012400210809
012500210601     c                   EndSr
012600210809     ?*?-------------------------------AJ-----------------------------------
012700210601       BegSr ParseCC;
012800210809
012900210809       IF TOTAJ > 0;
013000210526
013100210526       EXEC SQL
013200210526
013300210526        INSERT INTO TPRDAR01
013400210526         SELECT SUBSTR(CAMPO, 1, 2) AS TPHTRG,
013500210526                    SUBSTR(CAMPO, 3, 2) AS TPHRAR,
013600210526                    SUBSTR(CAMPO, 28, 8) AS TPHFGA,
013700210526                    SUBSTR(CAMPO, 36, 2) AS TPHNCD,
013800210526                    SUBSTR(CAMPO, 61, 50) AS TPHVDR,
013900210526                    SUBSTR(CAMPO, 1742, 259) AS FILLER
014000210809                    FROM TPAJW
014100210526                    WHERE SUBSTR(CAMPO, 1, 2) = '01';
014200210809
014300210809
014400210809       EXEC SQL
014500210809
014600210809        INSERT INTO TPRDAR02
014700210810        SELECT SUBSTR(CAMPO, 1, 2) AS TPPTREG,
014800210810       SUBSTR(CAMPO, 25, 1) AS TPPTIPA,
014900210810       SUBSTR(CAMPO, 26, 12) AS TPPDNIN,
015000210810       SUBSTR(CAMPO, 88, 19) AS TPPNPAN,
015100210810       CAST(SUBSTR(CAMPO, 107, 13) AS DEC(13, 2)) / 100 AS TPPMNTO,
015200210810       SUBSTR(CAMPO, 120, 3) AS TPPMCCY,
015300210810       SUBSTR(CAMPO, 123, 10) AS TPPCHKN,
015400210810       SUBSTR(CAMPO, 133, 8) AS TPPFEPR,
015500210810       SUBSTR(CAMPO, 141, 6) AS TPPHOPR,
015600220113       SUBSTR(CAMPO, 147, 1) AS TPPCODR,
015601220113       SUBSTR(CAMPO, 148, 50) AS TPPVDRC,
015800210810       SUBSTR(CAMPO, 88, 19) AS TPPNACC,
015900210810       CAST(SUBSTR(CAMPO, 107, 13) AS DEC(13, 2)) / 100 AS TPPMTEN,
016000210810       SUBSTR(CAMPO, 230, 3) AS TPPTTRA,
016100210810       SUBSTR(CAMPO, 239, 10) AS TPPUSI1,
016200210810       SUBSTR(CAMPO, 249, 10) AS TPPUSI2,
016300210810       SUBSTR(CAMPO, 259, 1742) AS FILLERC
016400210809            FROM TPAJW
016500210809            WHERE SUBSTR(CAMPO, 1, 2) = '02';
016600210809
016700220113       ENDIF;
016800210511           ENDSR;
016900210526
017000210601     ?*?-------------------------------VR-----------------------------------
017100210526       BegSr VrCC;
017200210601
017300210526       EXEC SQL
017400210529       SELECT LOCATE_IN_STRING(TPHVDR,'*') INTO:@VR1 FROM TPRDAR01;
017500210526       EXEC SQL
017600210529       SELECT LOCATE_IN_STRING(TPPVDRC,'*')  INTO:@VR2 FROM TPRDAR02;
017700210526
017800210806       IF @VR1 <> '0' OR @VR2<>'0';
017900220113          //TPRev ();
018000220113          //Return;
018100210601       ENDIF;
018200210601
018300210526       ENDSR;
018400210511
018500210601     ?*?-----------------------------INZ-----------------------------------
018600210526       BegSr CheqCC;
018700210809
018800211123         EXEC SQL
018900211123
019000211123         SELECT  CHAR(
019100211123         SUBSTR(DIGITS(YEAR(DATE(TIMESTAMP_FORMAT
019200211123         (CHAR(TPFECGEN), 'YYYYMMDD')))), 04, 01) ||
019300211123         DIGITS(dec( JULIAN_DAY(DATE(TIMESTAMP_FORMAT
019400211123         (CHAR(TPFECGEN), 'YYYYMMDD'))) - JULIAN_DAY(CHAR
019500211123         (CHAR(YEAR(DATE(TIMESTAMP_FORMAT(CHAR(TPFECGEN),
019600211123         'YYYYMMDD')))), 04) || '-01-01') + 1, 03,00)), 4) AS
019700211123          JULIAN, TPFECGEN INTO :dia_jul , :@FECGEN FROM TPAJ01;
019800211123
019900211123           EXEC SQL
020000211123           SELECT COUNT(*) INTO :AJ  FROM TPHERDA
020100211123           WHERE TPFECGEN = :@FECGEN AND TPTDA='AJ';
020200211223
020300211223           EXEC SQL SELECT TPMETDEC INTO :@LIBRERIA FROM TPMETBLS
020400211223           WHERE TPMETCOD= 'LIBRERIA';
020500220113
020600211223           EXEC SQL DELETE FROM TPAJW ;
020700210601
020800211123           ENDSR;
020900211123
021000210601     ?*-----------------------------RESP----------------------------------
021100210601           BegSr ActCC;
021200210601
021300210601                EXEC SQL
021400220112
021500210526            MERGE INTO TPRDA02 A USING TPRDAR02 B
021600210526                ON (A.TPPCEDREG = B.TPPDNIN
021700210526                AND A.TPPNPAN = B.TPPNPAN
021800210526                AND A.TPPIDREC = B.TPPCHKN)
021900220112                AND A.TPPTTRA <> ' '
022000210526              WHEN MATCHED THEN UPDATE SET A.TPPCODRESP = B.TPPCODR,
022100220112              A.TPPVR = B.TPPVDRC, A.TPPFECPRO=B.TPPFEPR,
022200220112              A.TPPHORPRO=B.TPPHOPR;
022300220112
022400210526           ENDSR;
022500210113
022600210601      /END-FREE
022700210601     c*?---------------------------------------------------------------
022800210601     c*?Graba Trazas de Auditoria
022900210601     c*?---------------------------------------------------------------
023000210601     c     Auditoria     Begsr
023100210601     c*?   Graba registro en Mntor
023200210601     c                   Clear                   rmntor
023300210601     c                   Eval      MntBnk = wbanco
023400211123     c                   eval      MntFil = 'TPRDA02'
023500210601     c                   eval      Mntusr = User_Id
023600210601     c                   Time                    MntSyt
023700210601     c                   eval      MntSyd = Uday
023800210601     c                   eval      MntSym = Umonth
023900210601     c                   eval      MntSYy = Uyear
024000210601     c                   eval      MntMod = 'CARGA'
024100210601     c                   eval      MntMnu = 'SCHEDUL'
024200211123     c                   eval      MntPrg = 'TPPR03'
024300210601     c                   eval      MntOpt = *Zeros
024400211123     c                   eval      MntPrc = 'TPPR03'
024500210601     c                   eval      MntCrt = Job
024600210601     c                   Write     Rmntor
024700210601     c                   Endsr
