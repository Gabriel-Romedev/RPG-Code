000200210727     h DFTACTGRP(*no) ACTGRP(*caller)
000300210727     H Option(*Nodebugio:*Srcstmt)
000400210727     HDEBUG(*YES)
000500210727     HDATFMT(*ISO)
000900210113     ?*?--------------------------------------------------------------------
001000210601      * Programa   : Envio de Ajustes y Devoluciones a CCC
001100210524      * Fecha        : 05 / 24 / 2021
001200210113      * Creado por : Ing. Jesús Romero.
001300210601     ?*?-------------------------------------------------------------------
001400210601     FMNTOR     O  A E           K DISK
001500210601     fCntrlBth  if   e           K DISK
001600210601     ?*?-------------------------------------------------------------------
001700210601     d                sds
001800210601     d NomPgm                  1     10
001900210601     d Job                   244    253
002000210601     d User_Id               254    263
002100210601     d*-----------------------------------------------------------------
002200210601     d Sel             S            300A   Inz(*Blanks)
002300210601     d FechaDia2       S              8a   Inz(*Blanks)
002400210601     ?*?-------------------------------------------------------------------
002500210601     d FechaDia        ds             8
002600210601     d anod                           4  0
002700210601     d mesd                           2  0
002800210601     d diad                           2  0
002900210601     d*-----------------------------------------------------------------
003000210601     D                 DS
003100210601     DWfechaii                        8S 0 Inz
003200210601     D WfeciY                         4S 0 Overlay(Wfechaii:1)
003300210601     D WfeciM                         2S 0 Overlay(Wfechaii:*next)
003400210601     D WfeciD                         2S 0 Overlay(Wfechaii:*next)
003500210601     d*-----------------------------------------------------------------
003600210601     D Dia_Jul         S              3S 0
003700210601     d*-----------------------------------------------------------------
003800210601     d NomArchivo      s             50a   Inz(*Blanks)
003900211217     d NomOld          s             50a   Inz(*Blanks)
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
005100210601     d*-----------------------------------------------------------------
005200210601      *?Programa que hace la transferencia
005300210601     D SSH003          PR                  ExtPgm('SSH003')
005400210601     DP_Biblioteca                   10
005500210601     DP_Nom400                       10
005600210601     DP_NomArchivo                   50
005700210601     DP_Salida                        1
005800210601     DP_Etiqueta                     30
005900210601     DP_Programa                     10
006000210601     DP_Longitud                     10
006100211123     d*-----------------------------------------------------------------
006200210601     DP_CondError                    10
006300211123     D ExceptionMsg    S              7A   Import('_EXCP_MSGID')
006400211123     d Comando         s             97
006500211123     DRunCmd           Pr            10i 0 ExtProc('system')
006600211123     D   Cmd                           *   Value options(*string)
006700211123     d*-----------------------------------------------------------------
006800211123     D RESPTEMP1       s             10A   iNZ('RESPTEMP1')
006900211123     D RESPTEMP2       s             10A   iNZ('RESPTEMP2')
007000211123     D Artxtw          s             16A
007100211123     D Dperror         s             50A
007200210601     d*-------------------------------------------------------
007300210601     D @IN_TPP         S              1S 0   Inz(*Zeros)
007400211217     D @INDV           S              1S 0   Inz(*Zeros)
007500210601     d dv              s              2  0 Inz(*zeros)
007600211123     d dv1             s              2  0 Inz(*zeros)
007700211123     D File_Old        s             10A
007800211123     D File_400        s             10A
007900211123     D File_400_1      s             10A
008000210601     D*?--------------------------------------------------------------------
008100210601     c*?Rutina Principal del Programa
008200210601     c*?--------------------------------------------------------------------
008300210601     c                   ExSr      RutIni
008400210601     c                   ExSr      CheqCC
008500210601     c                   ExSr      GenCC
008600211123     c                   Exsr      Elimina_file
008700211123     c                   Exsr      Crea_file
008800211123     c                   Exsr      Grabar_archRes
008900211123     c                   Exsr      Copia_archRes
009000210601     c                   ExSr      HistCC
009100210601     c                   ExSr      EnvCC
009200210601     c                   ExSr      Auditoria
009300210601     c                   Eval      *Inlr = '1'
009400210601     c*?--------------------------------------------------------------------
009500210601     c*?Sub Rutina Inicial
009600210601     c*?--------------------------------------------------------------------
009700210601     c     RutIni        BegSr
009800210601     c     User_Id       Chain     Cntrlbth
009900210601     c                   Movel     BthUbk        wbanco            2
010000210601     c                   Move      *Date         FechaDia
010100210601     c                   move      *Date         FechaDia2
010200210601     c                   move      *date         wfechaii
010300210601     c*
010400210601     c/Exec SQL          Set Option Commit = *None, NamIng = *Sys,
010500210601     c+                  CloSqlCsr = *EndMod
010600210601     c/End-Exec
010700210601     c*
010800210601     c                   EndSr
010900210601    C?*---------------------------------------------------------------
011000210511      /FREE
011100210601
011200210526           BegSr GenCC;
011300220113
011400210601
011500210601           EXEC SQL
011600210601          INSERT INTO TPDV01
011700210601                SELECT CHAR(01) AS TPTIPREG,
011800210601                       'DV' AS TPTDA,
011900210617                       915 AS TPCODBAN,
012000210617                       16 AS TPCODAS,
012100210803                      492 AS TPCODRE,
012200210617                       REPLACE(CHAR(CURRENT_DATE), '-', '') AS TPFECGEN,
012300210617                       1 AS TPNCOR,
012400210617                       COUNT(*) AS TPCANTAJ,
012500210617                      SUM(TPPMTO) AS TPTOTMOV,
012600210617                      '' AS TPPVR,
012700210617                      '' AS FILLER
012800210601              FROM TPRDA02 WHERE TPPCONTRA IN ('100','25','57','71')
012900220112               AND TPPCODRESP='A' ;
013000210601
013100210601               IF SQLCODE <>0  ;
013200210601               @IN_TPP= 1;
013300210601               ENDIF;
013400210601     ?*?--------------------------------W1---------------------------------
013500220113
013600210601           EXEC SQL
013700210601
013800210601           INSERT INTO TPDVW
013900210601
014000210617            SELECT '01' || TPTDA || TPCODBAN || LPAD((TPCODAS),
014100210617                        10,
014200210617                        '0') || LPAD((TPCODRE),
014300210617                        10,
014400210617                        '0') || TPFECGEN || LPAD((:DV),
014500210617                        2,
014600210617                        '0') || LPAD((TPCANTAJ),
014700210617                        10,
014800210617                        '0') || REPLACE(LPAD((TPTOTMOV),
014900210617                            14,
015000210617                            '0'), '.', '') || LPAD((TPPVR),
015100210617                        13,
015200210617                        '0') AS CAMPO
015300210617           FROM TPDV01;
015400210524               IF SQLCODE <> 0;
015500210524               @IN_TPP= 1;
015600210529               ENDIF;
015700210524
015800210524           IF  @IN_TPP <> 1;
015900210601     ?*?--------------------------------W2---------------------------------
016000210524          EXEC SQL
016100210524
016200220113
016300210601         INSERT INTO TPDVW
016400210601
016500210629             SELECT TPPREG||LPAD((TPPCODFP),2,'0')||LPAD((TPPCODES),10,'0') ||
016600210629              LPAD((TPPCODPR),10,'0')||TPPTIPER||LPAD(INT((TPPCEDREG)),12,'0')||
016700210629              LPAD((TPPBAPE),25,' ')||LPAD((TPPBNOM),25,' ')||
016800210629              RPAD((TPPNPAN),19,' ')||REPLACE (LPAD((TPPMTO),14,'0'),'.','')||
016900210629              LPAD((TPPCODMON),3,'0') ||LPAD((TPPIDREC),11,'0') || TPPFECPRO
017000210629              ||TPPHORPRO||TPPCODRESP|| LPAD((TPPVR),50,' ')  ||
017100210630               LPAD((TPPACC),19,' ')|| REPLACE (LPAD((TPPMTOE),13,'0'),'.','')||
017200210630               LPAD(INT((TPPTTRA)),3,'0')||LPAD(INT((TPPORITRA)),3,'0')||
017300210629               LPAD((TPPCONTRA),3,'0')||LPAD((TPPUS1),10,'0')||
017400210629               LPAD((TPPUS2),10,'0')||LPAD((FILLER),1742,' ') AS CAMPO
017500220112               FROM TPRDA02 WHERE TPPCODRESP='A' AND
017600210601                TPPCONTRA IN ('100','25','57','71');
017700210524
017800210529               IF SQLCODE<> 0;
017900210601               @IN_TPP= 2;
018000210617               EXSR CheqCC;
018100210617
018200210601               ELSEIF SQLCODE=0;
018300210601               @INDV=1;
018400210601               ENDIF;
018500210601               ENDIF;
018600210601
018700211123     ?*?-------------------------------OLD-----------------------------------
018800211217        IF @INDV =1;
018900211123        NomOld = 'DV915' + %char(dia_jul) + %EDITC(DV1:'X');
019000211123        NomArchivo = 'DV915' + %char(dia_jul) + %EDITC(DV:'X');
019100211123        ENDIF;
019200211123
019300211123        File_400 = %trim(NomArchivo);
019400211123        File_Old = %trim(NomOld);
019500211123        File_400_1 =%trim('MQCONTROL');
019600210526
019700210529          ENDSR;
019800210601     ?*?--------------------------------IBS---------------------------------
019900210529          BegSr CheqCC;
020000210617
020100210524
020200220113
020300210601           Exec SQL
020400210601           DELETE FROM TPDVW ;
020500220113
020600210601            Exec SQL
020700210601            DELETE FROM TPDV01;
020800210617
020900210617            IF @IN_TPP<> 0;
021000211123
021100210617            *Inlr = *ON;
021200211123            Return;
021300211123
021400210617            ENDIF;
021500210617
021600210617     ?*?--------------------------------IBS---------------------------------
021700210601
021800211123            EXEC SQL
021900211123            SELECT COUNT(*) INTO :DV1
022000211123            FROM TPHERDA
022100211123            WHERE DATE(TIMESTAMP_FORMAT(CHAR(TPFECGEN), 'YYYYMMDD'))
022200211123             = CURRENT_DATE AND TPTDA='DV';
022300211123
022400211123           IF DV1>= 1 ;
022500211123            DV  = DV1  + 1;
022600211123           ELSE ;
022700211123            DV = 1;
022800211123           ENDIF;
022900211123
023000211123     ?*?-------------------------------JUL-----------------------------------
023100211123          EXEC SQL
023200211123
023300211123        SELECT DISTINCT CHAR(
023400211123         SUBSTR(DIGITS(YEAR(CURRENT DATE)), 04, 01) ||
023500211123          DIGITS(dec(JULIAN_DAY(CURRENT DATE) -
023600211123           JULIAN_DAY(CHAR(CHAR(YEAR(CURRENT DATE)),
023700211123               04) || '-01-01') + 1, 03, 00)), 4)
023800211123               INTO :Dia_jul
023900211123        FROM SYSIBM.SYSDUMMY1;
024000211123
024100211123          ENDSR;
024200211123
024300211123       //------------------------------------------------------------//
024400211123       //?  Elimina Archivo                                          //
024500211123       //------------------------------------------------------------//
024600211123         Begsr Elimina_file;
024700211123
024800211123               Comando = ' DLTF FILE' + '(QTEMP/'+(File_Old) + ')';
024900211123               Exsr  Run_cmd;
025000211123               Comando = ' DLTF FILE' + '(QTEMP/'+(RESPTEMP1) + ')';
025100211123               Exsr  Run_cmd;
025200211123               Comando = ' DLTF FILE' + '(QTEMP/'+(File_400_1) + ')';
025300211123               Exsr  Run_cmd;
025400211123               Comando = ' DLTF FILE' + '(QTEMP/'+(RESPTEMP2) + ')';
025500211123               Exsr  Run_cmd;
025600211123         Endsr;
025700211123       //------------------------------------------------------------//
025800211123       //?  Crea Archivo                                             //
025900211123       //------------------------------------------------------------//
026000211123         Begsr Crea_file;
026100211123
026200211123               Comando = ' CRTPF FILE(QTEMP/'+(File_400)+ ')'+
026300211123                         ' RCDLEN(2000) ' +
026400211123                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
026500211123               Exsr  Run_cmd;
026600211123
026700211123
026800211123               Comando = ' CRTPF FILE(QTEMP/'+(RESPTEMP1)+ ')'+
026900211123                         ' RCDLEN(2000) ' +
027000211123                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
027100211123               Exsr  Run_cmd;
027200211123
027300211123                    Comando = ' CRTPF FILE(QTEMP/'+(File_400_1)+ ')'+
027400211123                         ' RCDLEN(1) ' +
027500211123                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
027600211123               Exsr  Run_cmd;
027700211123
027800211123               Comando = ' CRTPF FILE(QTEMP/'+(RESPTEMP2)+ ')'+
027900211123                         ' RCDLEN(1) ' +
028000211123                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
028100211123               Exsr  Run_cmd;
028200211123
028300211123         Endsr;
028400211123
028500211123      //------------------------------------------------------------//
028600211123       //?  Graba Archivo de Respuesta cuando la cantidad de         //
028700211123       //?  registros de entrada es  = cero.                         //
028800211123       //------------------------------------------------------------//
028900211123         Begsr Grabar_archRes;
029000211123
029100220113         IF @INDV=1;
029200211123
029300211123               Exec Sql
029400211123                  INSERT INTO QTEMP/RESPTEMP1
029500211123                    SELECT * FROM TPDVW;
029600211123
029700211123         ENDIF;
029800211123
029900211123            Exec SQL INSERT INTO QTEMP/RESPTEMP2 VALUES (' ');
030000211123
030100211123         Endsr;
030200211123
030300211123       //------------------------------------------------------------//
030400211123       //?  Copia hacia el archivo diario de TransferenciA      //
030500211123       //------------------------------------------------------------//
030600211123         Begsr Copia_archRes;
030700211123
030800211123           Comando = 'CPYF FROMFILE(QTEMP/' + (RESPTEMP1) + ')'+
030900211123                     ' TOFILE(QTEMP/' + (File_400) + ')'+
031000211123                     ' MBROPT(*REPLACE) '               +
031100211123                     ' FMTOPT(*NOCHK)';
031200211123           Exsr  Run_cmd;
031300211123
031400211123           Comando = 'CPYF FROMFILE(QTEMP/' + (RESPTEMP2) + ')'+
031500211123                     ' TOFILE(QTEMP/' + (File_400_1) + ')'+
031600211123                     ' MBROPT(*REPLACE) '               +
031700211123                     ' FMTOPT(*NOCHK)';
031800211123           Exsr  Run_cmd;
031900211123
032000211123
032100211123         Endsr;
032200211123
032300210617     ?*?--------------------------------IBS---------------------------------
032400211123
032500210601          BegSr EnvCC;
032600211223
032700220113         Etiqueta   = 'CREATM06';
032800211223         Biblioteca = 'QTEMP     ';
032900220113         Programa   = 'TPPR06  ';
033000211223         Salida     = 'S';
033100211223         @tam = 2000;
033200211223         Tamano = %char(@tam);
033300211223
033400210601
033500211223         IF @INDV= 1;
033600211223
033700210601         NomArchivo = 'DV915' + %char(dia_jul) + %char(DV);
033800211223         Nom400     = %trim(File_400);
033900210601         CondError  = '          ';
034000210601         SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
034100210601                Programa: Tamano: CondError);
034200210809
034300210601
034400210601         ENDIF;
034500210809
034600211123       //------------------------------------------------------------//
034700211123       //Envio MQCONTROL
034800211123       //------------------------------------------------------------//
034900211123
035000211123           Nom400     = %trim(File_400_1);
035100211123           NomArchivo = %trim(File_400_1);
035200211123           Salida     = 'S';
035300211123           Tamano = '1';
035400211123           CondError  = '          ';
035500211123
035600211123            SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
035700211123            Programa: Tamano: CondError);
035800210601
035900210601          ENDSR;
036000210526
036100210601     ?*?-----------------------HISTORICO---------------------------------*
036200210526
036300210526          BegSr HistCC;
036400210601
036500210601       EXEC SQL
036600210601
036700210601       INSERT INTO  TPHERDA  SELECT TPTIPREG, TPTDA, TPCODBAN,
036800210601       TPCODAS,TPCODRE,TPFECGEN,TPNCOR,
036900210601       TPCANTAJ,TPTOTMOV FROM TPDV01 ;
037000210526
037100210529          ENDSR;
037200211217       //------------------------------------------------------------//
037300211217       //?  Rutina de Ejecución de Comando.                          //
037400211217       //------------------------------------------------------------//
037500211217         Begsr Run_cmd;
037600211217
037700211217               Clear ExceptionMsg;
037800211217               Monitor;
037900211217               RunCmd(Comando);
038000211217               On-Error;
038100211217               EndMon;
038200211217           If (ExceptionMsg <> *Blanks);
038300211217             Dperror = 'Error en copiar';
038400211217           Endif;
038500211217         Endsr;
038600211217
038700210510      /END-FREE
038800210601     c*?---------------------------------------------------------------
038900210601     c*?Graba Trazas de Auditoria
039000210601     c*?---------------------------------------------------------------
039100210601     c     Auditoria     Begsr
039200210601     c*?   Graba registro en Mntor
039300210601     c                   Clear                   rmntor
039400210601     c                   Eval      MntBnk = wbanco
039500211123     c                   eval      MntFil = 'TPRDA02'
039600210601     c                   eval      Mntusr = User_Id
039700210601     c                   Time                    MntSyt
039800210601     c                   eval      MntSyd = Uday
039900210601     c                   eval      MntSym = Umonth
040000210601     c                   eval      MntSYy = Uyear
040100210601     c                   eval      MntMod = 'CARGA'
040200210601     c                   eval      MntMnu = 'SCHEDUL'
040300220113     c                   eval      MntPrg = 'TPPR06'
040400210601     c                   eval      MntOpt = *Zeros
040500220113     c                   eval      MntPrc = 'TPPR06'
040600210601     c                   eval      MntCrt = Job
040700210601     c                   Write     Rmntor
040800210601     c                   Endsr
