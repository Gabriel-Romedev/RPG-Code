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
005400210707     d TPRev           PR                  Extpgm('CM4950R')
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
007400210809     d dv              s              2  0 Inz(*zeros)
007600220113     D TOTDV           s             10  0
007800210601     D*?--------------------------------------------------------------------
007900210601     c*?Rutina Principal del Programa
008000210601     c*?--------------------------------------------------------------------
008100210601     c                   ExSr      RutIni
008200210601     c                   ExSr      CheqCC
008300210601     c                   ExSr      RutPro
008400210601     c                   ExSr      ParseCC
008500210601     c                   ExSr      VrCC
008600210601     c                   ExSr      ActCC
008700210601     c                   ExSr      Auditoria
008800210601     c                   Eval      *Inlr = '1'
008900210601     c*?--------------------------------------------------------------------
009000210601     c*?Sub Rutina Inicial
009100210601     c*?--------------------------------------------------------------------
009200210601     c     RutIni        BegSr
009300210601     c     User_Id       Chain     Cntrlbth
009400210601     c                   Movel     BthUbk        wbanco            2
009500210601     c                   Move      *Date         FechaDia
009600210601     c                   move      *Date         FechaDia2
009700210601     c                   move      *date         wfechaii
009800210601     c*
009900210601     c/Exec SQL          Set Option Commit = *None, NamIng = *Sys,
010000210601     c+                  CloSqlCsr = *EndMod
010100210601     c/End-Exec
010200210601     c*
010300210601     c                   EndSr
010400210601     c
010500210601     ?*?-------------------------------W1-----------------------------------
010600210601     c     RutPro        BegSr
010700210601      /FREE
010800220113
012300210809         //----Recepcion de Devoluciones
012301220113
012302220113        Biblioteca = @LIBRERIA;
012303220113        Etiqueta   = 'SSHTPP';
012305220113        Programa   = 'TPPR05  ';
012306220113        Salida     = 'S';
012307220113
012400210809
012500210809        Nom400     = 'TPDVW                       ';
012600211123        NomArchivo = 'DV915' + %char(dia_juld) +  %EDITC(DV:'X');
012700220113        Programa   = 'TPPR05  ';
012800210809        CondError  = '          ';
012900210809        @tam = 2000;
013000210809        Tamano = %char(@tam);
013100210809
013200210809        SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
013300210809               Programa: Tamano: CondError);
013400210809
013500210809       EXEC SQL
013600210809       SELECT COUNT (*) INTO:TOTDV FROM  TPDVW;
014000210809
014100210601     c                   EndSr
014200210809     ?*?-------------------------------AJ-----------------------------------
014300210601       BegSr ParseCC;
014400210809
014500220113       IF TOTDV > 0;
018400210809
018500210809     ?*?-------------------------------DV-----------------------------------
018600210809       EXEC SQL
018700210809
018800210809        INSERT INTO TPRDAR01
018900210809         SELECT SUBSTR(CAMPO, 1, 2) AS TPHTRG,
019000210809                    SUBSTR(CAMPO, 3, 2) AS TPHRAR,
019100210809                    SUBSTR(CAMPO, 28, 8) AS TPHFGA,
019200210809                    SUBSTR(CAMPO, 36, 2) AS TPHNCD,
019300210809                    SUBSTR(CAMPO, 61, 50) AS TPHVDR,
019400210809                    SUBSTR(CAMPO, 1742, 259) AS FILLER
019500210809                    FROM TPDVW
019600210809                    WHERE SUBSTR(CAMPO, 1, 2) = '01';
019700210809
019800210809       EXEC SQL
019900210809
020000210810       INSERT INTO TPRDAR02
020100210810        SELECT SUBSTR(CAMPO, 1, 2) AS TPPTREG,
020200210810       SUBSTR(CAMPO, 25, 1) AS TPPTIPA,
020300210810       SUBSTR(CAMPO, 26, 12) AS TPPDNIN,
020400210810       SUBSTR(CAMPO, 88, 19) AS TPPNPAN,
020500210810       CAST(SUBSTR(CAMPO, 107, 13) AS DEC(13, 2)) / 100 AS TPPMNTO,
020600210810       SUBSTR(CAMPO, 120, 3) AS TPPMCCY,
020700210810       SUBSTR(CAMPO, 123, 10) AS TPPCHKN,
020800210810       SUBSTR(CAMPO, 133, 8) AS TPPFEPR,
020900210810       SUBSTR(CAMPO, 141, 6) AS TPPHOPR,
020901220113       SUBSTR(CAMPO, 147, 1) AS TPPCODR,
020902220113       SUBSTR(CAMPO, 148, 50) AS TPPVDRC,
021200210810       SUBSTR(CAMPO, 88, 19) AS TPPNACC,
021300210810       CAST(SUBSTR(CAMPO, 107, 13) AS DEC(13, 2)) / 100 AS TPPMTEN,
021400210810       SUBSTR(CAMPO, 230, 3) AS TPPTTRA,
021500210810       SUBSTR(CAMPO, 239, 10) AS TPPUSI1,
021600210810       SUBSTR(CAMPO, 249, 10) AS TPPUSI2,
021700210810       SUBSTR(CAMPO, 259, 1742) AS FILLERC
021800210810         FROM TPDVW
021900210809            WHERE SUBSTR(CAMPO, 1, 2) = '02';
022000210809
022100210809       ENDIF;
022200210511           ENDSR;
022300210526
022400210601     ?*?-------------------------------VR-----------------------------------
022500210526       BegSr VrCC;
022600210601
022700210526       EXEC SQL
022800210529       SELECT LOCATE_IN_STRING(TPHVDR,'*') INTO:@VR1 FROM TPRDAR01;
022900210526       EXEC SQL
023000210529       SELECT LOCATE_IN_STRING(TPPVDRC,'*')  INTO:@VR2 FROM TPRDAR02;
023100210526
023200210806       IF @VR1 <> '0' OR @VR2<>'0';
023300220113          //TPRev ();
023400220113          //Return;
023500210601       ENDIF;
023600210601
023700210526       ENDSR;
023800210511
023900210601     ?*?-----------------------------INZ-----------------------------------
024000210526       BegSr CheqCC;
024100220113
025800211123          EXEC SQL
025900211123          SELECT  CHAR(
026000211123          SUBSTR(DIGITS(YEAR(DATE(TIMESTAMP_FORMAT
026100211123          (CHAR(TPFECGEN), 'YYYYMMDD')))), 04, 01) ||
026200211123          DIGITS(dec( JULIAN_DAY(DATE(TIMESTAMP_FORMAT
026300211123          (CHAR(TPFECGEN), 'YYYYMMDD'))) - JULIAN_DAY(CHAR
026400211123          (CHAR(YEAR(DATE(TIMESTAMP_FORMAT(CHAR(TPFECGEN),
026500211123          'YYYYMMDD')))), 04) || '-01-01') + 1, 03,00)), 4) AS
026600211123           JULIAN, TPFECGEN INTO :dia_juld , :@FECGEND FROM TPDV01;
026700211123
026800211123           EXEC SQL
026900211123           SELECT COUNT(*) INTO :DV  FROM TPHERDA
027000211123           WHERE TPFECGEN = :@FECGEND AND TPTDA='DV';
027100211223
027200211223           EXEC SQL SELECT TPMETDEC INTO :@LIBRERIA FROM TPMETBLS
027300211223           WHERE TPMETCOD= 'LIBRERIA';
027400211223
027500211223           EXEC SQL DELETE FROM TPDVW ;
027600220113
027700210601
027800211123           ENDSR;
027900211123
028000210601     ?*-----------------------------RESP----------------------------------
028100210601           BegSr ActCC;
028200210601
028300210601                EXEC SQL
028400220112
028500210526            MERGE INTO TPRDA02 A USING TPRDAR02 B
028600210526                ON (A.TPPCEDREG = B.TPPDNIN
028700210526                AND A.TPPNPAN = B.TPPNPAN
028800210526                AND A.TPPIDREC = B.TPPCHKN)
028900220112                AND A.TPPTTRA <> ' '
029000210526              WHEN MATCHED THEN UPDATE SET A.TPPCODRESP = B.TPPCODR,
029100220112              A.TPPVR = B.TPPVDRC, A.TPPFECPRO=B.TPPFEPR,
029200220112              A.TPPHORPRO=B.TPPHOPR;
029300220112
029400210526           ENDSR;
029500210113
029600210601      /END-FREE
029700210601     c*?---------------------------------------------------------------
029800210601     c*?Graba Trazas de Auditoria
029900210601     c*?---------------------------------------------------------------
030000210601     c     Auditoria     Begsr
030100210601     c*?   Graba registro en Mntor
030200210601     c                   Clear                   rmntor
030300210601     c                   Eval      MntBnk = wbanco
030400211123     c                   eval      MntFil = 'TPRDA02'
030500210601     c                   eval      Mntusr = User_Id
030600210601     c                   Time                    MntSyt
030700210601     c                   eval      MntSyd = Uday
030800210601     c                   eval      MntSym = Umonth
030900210601     c                   eval      MntSYy = Uyear
031000210601     c                   eval      MntMod = 'CARGA'
031100210601     c                   eval      MntMnu = 'SCHEDUL'
031200211123     c                   eval      MntPrg = 'TPPR03'
031300210601     c                   eval      MntOpt = *Zeros
031400211123     c                   eval      MntPrc = 'TPPR03'
031500210601     c                   eval      MntCrt = Job
031600210601     c                   Write     Rmntor
031700210601     c                   Endsr
