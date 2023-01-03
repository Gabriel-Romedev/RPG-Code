000100210727     h copyright('© Banco Occidental de Descuento, B.U. 2013')
000200210727     h DFTACTGRP(*no) ACTGRP(*caller)
000300210727     H Option(*Nodebugio:*Srcstmt)
000400210727     HDEBUG(*YES)
000500210727     HDATFMT(*ISO)
000600210113     ?*?--------------------------------------------------------------------
000700201018      *? B.O.D. BANCO UNIVERSAL                                    *
000800201129      *? Vicepresidencia de Ingenieria y Transformación        *
000900210113     ?*?--------------------------------------------------------------------
001000220113      * Programa   : Envio de Devoluciones a CCC
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
007400210601     D @INAJ           S              1S 0   Inz(*Zeros)
007800210601     d aj              s              2  0 Inz(*zeros)
007900211123     d aj1             s              2  0 Inz(*zeros)
008000211123     D File_Old        s             10A
008100211123     D File_400        s             10A
008200211123     D File_400_1      s             10A
008300210601     D*?--------------------------------------------------------------------
008400210601     c*?Rutina Principal del Programa
008500210601     c*?--------------------------------------------------------------------
008600210601     c                   ExSr      RutIni
008700210601     c                   ExSr      CheqCC
008800210601     c                   ExSr      GenCC
008900211123     c                   Exsr      Elimina_file
009000211123     c                   Exsr      Crea_file
009100211123     c                   Exsr      Grabar_archRes
009200211123     c                   Exsr      Copia_archRes
009300210601     c                   ExSr      HistCC
009400210601     c                   ExSr      EnvCC
009500210601     c                   ExSr      Auditoria
009600210601     c                   Eval      *Inlr = '1'
009700210601     c*?--------------------------------------------------------------------
009800210601     c*?Sub Rutina Inicial
009900210601     c*?--------------------------------------------------------------------
010000210601     c     RutIni        BegSr
010100210601     c     User_Id       Chain     Cntrlbth
010200210601     c                   Movel     BthUbk        wbanco            2
010300210601     c                   Move      *Date         FechaDia
010400210601     c                   move      *Date         FechaDia2
010500210601     c                   move      *date         wfechaii
010600210601     c*
010700210601     c/Exec SQL          Set Option Commit = *None, NamIng = *Sys,
010800210601     c+                  CloSqlCsr = *EndMod
010900210601     c/End-Exec
011000210601     c*
011100210601     c                   EndSr
011200210601    C?*---------------------------------------------------------------
011300210511      /FREE
011400210601
011500210526           BegSr GenCC;
011600210524
011700210601           EXEC SQL
011800210601            INSERT INTO TPAJ01
011900210601                SELECT CHAR(01) AS TPTIPREG,
012000210617                   'AJ' AS TPTDA,
012100210617                   915 AS TPCODBAN,
012200210617                   16 AS TPCODAS,
012300210803                  492 AS TPCODRE,
012400210617                   REPLACE(CHAR(CURRENT_DATE), '-', '') AS TPFECGEN,
012500210617                   1 AS TPNCOR,
012600210617                   COUNT(*) AS TPCANTAJ,
012700210617                   SUM(TPPMTO) AS TPTOTMOV,
012800210617                   '' AS TPPVR,
012900210617                   '' AS FILLER
013000210601                    FROM TPRDA02
013100220112                    WHERE TPPCODRESP = 'A'
013200210601                      AND TPPCONTRA IN ('114',
013300210601                      '116', '117', '119', '125', '126',
013400210601                      '127', '26', '38', '42', '53',
013500220113                      '64', '85', '87', '88', '90', '96', '97', '98');
013600220113
013700210601
013800210601               IF SQLCODE <>0  ;
013900210601               @IN_TPP= 1;
014000210601               ENDIF;
014100210601     ?*?--------------------------------W1---------------------------------
014200210511           EXEC SQL
014300210524
014400210601           INSERT INTO TPAJW
014500210524
014600210617            SELECT '01' || TPTDA || TPCODBAN || LPAD((TPCODAS),
014700210617                        10,
014800210617                        '0') || LPAD((TPCODRE),
014900210617                        10,
015000210617                        '0') || TPFECGEN || LPAD((:AJ),
015100210617                        2,
015200210617                        '0') || LPAD((TPCANTAJ),
015300210617                        10,
015400210617                        '0') || REPLACE(LPAD((TPTOTMOV),
015500210617                            14,
015600210617                            '0'), '.', '') || LPAD((TPPVR),
015700210617                        13,
015800210617                        '0') AS CAMPO
015900210617            FROM TPAJ01;
016000220113
016100210524
016200210524           IF  @IN_TPP <> 1;
016300210601     ?*?--------------------------------W2---------------------------------
016400210524          EXEC SQL
016500210524
016600210601         INSERT INTO TPAJW
016700210629             SELECT TPPREG||LPAD((TPPCODFP),2,'0')||LPAD((TPPCODES),10,'0') ||
016800210629              LPAD((TPPCODPR),10,'0')||TPPTIPER||LPAD(INT((TPPCEDREG)),12,'0')||
016900210629              LPAD((TPPBAPE),25,' ')||LPAD((TPPBNOM),25,' ')||
017000210630              RPAD((TPPNPAN),19,' ')||REPLACE (LPAD((TPPMTO),14,'0'),'.','')||
017100210629              LPAD((TPPCODMON),3,'0') ||LPAD((TPPIDREC),11,'0') || TPPFECPRO
017200210629              ||TPPHORPRO||TPPCODRESP|| LPAD((TPPVR),50,' ')  ||
017300210630               LPAD((TPPACC),19,' ')|| REPLACE (LPAD((TPPMTOE),13,'0'),'.','')||
017400210630               LPAD(INT((TPPTTRA)),3,'0')||LPAD(INT((TPPORITRA)),3,'0')||
017500210629               LPAD((TPPCONTRA),3,'0')||LPAD((TPPUS1),10,'0')||
017600210629               LPAD((TPPUS2),10,'0')||LPAD((FILLER),1742,' ') AS CAMPO
017700220112               FROM TPRDA02 WHERE TPPCODRESP='A' AND
017800210601                TPPCONTRA IN ('114', '116', '117', '119', '125', '126', '127',
017900210601                '26', '38', '42', '53', '64', '85', '87', '88',
018000210601                '90', '96', '97', '98');
018100210601               IF SQLCODE= 0;
018200210601               @INAJ=1;
018300220113               ENDIF;
018400220113
018500210529               IF SQLCODE<> 0;
018600210601               @IN_TPP= 2;
018700210617               EXSR CheqCC;
018800220113
018900210601               ENDIF;
019000220113               ENDIF;
019100210601
019200211123     ?*?-------------------------------OLD-----------------------------------
019300211123
019400211123        IF @INAJ =1;
019500211123        NomOld = 'AJ915' + %char(dia_jul) + %EDITC(AJ1:'X');
019600211123        NomArchivo = 'AJ915' + %char(dia_jul) + %EDITC(AJ:'X');
019700211123        ENDIF;
019800211123
019900211123        File_400 = %trim(NomArchivo);
020000211123        File_Old = %trim(NomOld);
020100211123        File_400_1 =%trim('MQCONTROL');
020200210526
020300210529          ENDSR;
020400210601     ?*?--------------------------------IBS---------------------------------
020500210529          BegSr CheqCC;
020600210617
020700210524
020800210601           Exec SQL
020900210601           DELETE FROM TPAJW;
021000220113
021100210526
021200210526            Exec SQL
021300210601            DELETE FROM TPAJ01;
021400220113
021500210617
021600210617            IF @IN_TPP<> 0;
021700211123
021800210617            *Inlr = *ON;
021900211123            Return;
022000211123
022100210617            ENDIF;
022200210617
022300210617     ?*?--------------------------------IBS---------------------------------
022400210526
022500211123            EXEC SQL
022600211123            SELECT COUNT(*) INTO :AJ1
022700211123            FROM TPHERDA
022800211123            WHERE DATE(TIMESTAMP_FORMAT(CHAR(TPFECGEN), 'YYYYMMDD'))
022900211123             = CURRENT_DATE AND TPTDA='AJ';
023000211123
023100211123           IF AJ1>= 1 ;
023200211123            AJ  = AJ1  + 1;
023300211123           ELSE ;
023400211123            AJ = 1;
023500220113           ENDIF;
023600211123
023700211123     ?*?-------------------------------JUL-----------------------------------
023800211123          EXEC SQL
023900211123
024000211123        SELECT DISTINCT CHAR(
024100211123         SUBSTR(DIGITS(YEAR(CURRENT DATE)), 04, 01) ||
024200211123          DIGITS(dec(JULIAN_DAY(CURRENT DATE) -
024300211123           JULIAN_DAY(CHAR(CHAR(YEAR(CURRENT DATE)),
024400211123               04) || '-01-01') + 1, 03, 00)), 4)
024500211123               INTO :Dia_jul
024600211123        FROM SYSIBM.SYSDUMMY1;
024700211123
024800211123          ENDSR;
024900211123
025000211123       //------------------------------------------------------------//
025100211123       //?  Elimina Archivo                                          //
025200211123       //------------------------------------------------------------//
025300211123         Begsr Elimina_file;
025400211123
025500211123               Comando = ' DLTF FILE' + '(QTEMP/'+(File_Old) + ')';
025600211123               Exsr  Run_cmd;
025700211123               Comando = ' DLTF FILE' + '(QTEMP/'+(RESPTEMP1) + ')';
025800211123               Exsr  Run_cmd;
025900211123               Comando = ' DLTF FILE' + '(QTEMP/'+(File_400_1) + ')';
026000211123               Exsr  Run_cmd;
026100211123               Comando = ' DLTF FILE' + '(QTEMP/'+(RESPTEMP2) + ')';
026200211123               Exsr  Run_cmd;
026300211123         Endsr;
026400211123       //------------------------------------------------------------//
026500211123       //?  Crea Archivo                                             //
026600211123       //------------------------------------------------------------//
026700211123         Begsr Crea_file;
026800211123
026900211123               Comando = ' CRTPF FILE(QTEMP/'+(File_400)+ ')'+
027000211123                         ' RCDLEN(2000) ' +
027100211123                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
027200211123               Exsr  Run_cmd;
027300211123
027400211123
027500211123               Comando = ' CRTPF FILE(QTEMP/'+(RESPTEMP1)+ ')'+
027600211123                         ' RCDLEN(2000) ' +
027700211123                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
027800211123               Exsr  Run_cmd;
027900211123
028000211123                    Comando = ' CRTPF FILE(QTEMP/'+(File_400_1)+ ')'+
028100211123                         ' RCDLEN(1) ' +
028200211123                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
028300211123               Exsr  Run_cmd;
028400211123
028500211123               Comando = ' CRTPF FILE(QTEMP/'+(RESPTEMP2)+ ')'+
028600211123                         ' RCDLEN(1) ' +
028700211123                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
028800211123               Exsr  Run_cmd;
028900211123
029000211123         Endsr;
029100211123
029200211123      //------------------------------------------------------------//
029300211123       //?  Graba Archivo de Respuesta cuando la cantidad de         //
029400211123       //?  registros de entrada es  = cero.                         //
029500211123       //------------------------------------------------------------//
029600211123         Begsr Grabar_archRes;
029700211123
029800211123         IF @INAJ= 1;
029900211123
030000211123               Exec Sql
030100211123                  INSERT INTO QTEMP/RESPTEMP1
030200220113                    SELECT * FROM TPAJW;
030300211123
030400211123         ENDIF;
030500211123
030600211123            Exec SQL INSERT INTO QTEMP/RESPTEMP2 VALUES (' ');
030700211123
030800211123         Endsr;
030900211123
031000211123       //------------------------------------------------------------//
031100211123       //?  Copia hacia el archivo diario de TransferenciA      //
031200211123       //------------------------------------------------------------//
031300211123         Begsr Copia_archRes;
031400211123
031500211123           Comando = 'CPYF FROMFILE(QTEMP/' + (RESPTEMP1) + ')'+
031600211123                     ' TOFILE(QTEMP/' + (File_400) + ')'+
031700211123                     ' MBROPT(*REPLACE) '               +
031800211123                     ' FMTOPT(*NOCHK)';
031900211123           Exsr  Run_cmd;
032000211123
032100211123           Comando = 'CPYF FROMFILE(QTEMP/' + (RESPTEMP2) + ')'+
032200211123                     ' TOFILE(QTEMP/' + (File_400_1) + ')'+
032300211123                     ' MBROPT(*REPLACE) '               +
032400211123                     ' FMTOPT(*NOCHK)';
032500211123           Exsr  Run_cmd;
032600211123
032700211123
032800211123         Endsr;
032900211123
033000210617     ?*?--------------------------------IBS---------------------------------
033100211123
033200210601          BegSr EnvCC;
033300211223
033400220113         Etiqueta   = 'SSHTPP';
033500211223         Biblioteca = 'QTEMP     ';
033600220113         Programa   = 'TPPR04  ';
033700211223         Salida     = 'S';
033800211223         @tam = 2000;
033900211223         Tamano = %char(@tam);
034000211223
034100211223          IF @INAJ= 1;
034200211223
034300211123         Nom400     = %trim(File_400);
034400220113         NomArchivo = 'AJ915' + %char(dia_jul) + %EDITC(AJ:'X');
034500210601         CondError  = '          ';
034600210805
034700210601         SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
034800210601                Programa: Tamano: CondError);
034900210601         ENDIF;
035000210601
035100220113
035200211123       //------------------------------------------------------------//
035300211123       //Envio MQCONTROL
035400211123       //------------------------------------------------------------//
035500211123
035600211123           Nom400     = %trim(File_400_1);
035700211123           NomArchivo = %trim(File_400_1);
035800211123           Salida     = 'S';
035900211123           Tamano = '1';
036000211123           CondError  = '          ';
036100211123
036200211123            SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
036300211123            Programa: Tamano: CondError);
036400210601
036500210601          ENDSR;
036600210526
036700210601     ?*?-----------------------HISTORICO---------------------------------*
036800210526
036900210526          BegSr HistCC;
037000220113
037100210601
037200210601       EXEC SQL
037300210601
037400210601       INSERT INTO  TPHERDA  SELECT TPTIPREG, TPTDA, TPCODBAN,
037500210601       TPCODAS,TPCODRE,TPFECGEN,TPNCOR,
037600220113       TPCANTAJ,TPTOTMOV FROM TPAJ01 ;
037700210526
037800210529          ENDSR;
037900211217       //------------------------------------------------------------//
038000211217       //?  Rutina de Ejecución de Comando.                          //
038100211217       //------------------------------------------------------------//
038200211217         Begsr Run_cmd;
038300211217
038400211217               Clear ExceptionMsg;
038500211217               Monitor;
038600211217               RunCmd(Comando);
038700211217               On-Error;
038800211217               EndMon;
038900211217           If (ExceptionMsg <> *Blanks);
039000211217             Dperror = 'Error en copiar';
039100211217           Endif;
039200211217         Endsr;
039300211217
039400210510      /END-FREE
039500210601     c*?---------------------------------------------------------------
039600210601     c*?Graba Trazas de Auditoria
039700210601     c*?---------------------------------------------------------------
039800210601     c     Auditoria     Begsr
039900210601     c*?   Graba registro en Mntor
040000210601     c                   Clear                   rmntor
040100210601     c                   Eval      MntBnk = wbanco
040200211123     c                   eval      MntFil = 'TPRDA02'
040300210601     c                   eval      Mntusr = User_Id
040400210601     c                   Time                    MntSyt
040500210601     c                   eval      MntSyd = Uday
040600210601     c                   eval      MntSym = Umonth
040700210601     c                   eval      MntSYy = Uyear
040800210601     c                   eval      MntMod = 'CARGA'
040900210601     c                   eval      MntMnu = 'SCHEDUL'
041000211123     c                   eval      MntPrg = 'TPPR04'
041100210601     c                   eval      MntOpt = *Zeros
041200211123     c                   eval      MntPrc = 'TPPR04'
041300210601     c                   eval      MntCrt = Job
041400210601     c                   Write     Rmntor
041500210601     c                   Endsr
