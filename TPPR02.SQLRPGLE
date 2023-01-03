000200210727     h DFTACTGRP(*no) ACTGRP(*caller)
000300210727     H Option(*Nodebugio:*Srcstmt)
000400210727     HDEBUG(*YES)
000500210727     HDATFMT(*ISO)
000600210806     hDatEdit(*YMD)
001000210113     ?*?--------------------------------------------------------------------
001100210524      * Programa   : Envio de Recargas a CCC
001200210524      * Fecha        : 05 / 24 / 2021
001300210113      * Creado por : Ing. Jesús Romero.
001400201018     ?*?--------------------------------------------------------------------
001500210601     FMNTOR     O  A E           K DISK
001600210601     fCntrlBth  if   e           K DISK
001700210601     ?*?-------------------------------------------------------------------
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
003700211123     D Dia_Jul         S              3S 0
003800210601     d*-----------------------------------------------------------------
003900210601     d NomArchivo      s             50a   Inz(*Blanks)
004000211117     d NomOld          s             50a   Inz(*Blanks)
004100210805     d @tam            s              4s 0 Inz(*Zeros)
004200210601     d Tamano          s             10a   Inz(*Blanks)
004300210601     d @emp            s              4a   Inz(*Blanks)
004400210601     d @ban            s              6s 0 Inz(*Blanks)
004500210601     d @nom            s              4a   Inz(*Blanks)
004600211116     d Biblioteca      s             50a   Inz(*Blanks)
004700211116     d Nom400          s             50a   Inz(*Blanks)
004800210601     d Salida          s             50a   Inz(*Blanks)
004900211116     d Etiqueta        s             50a   Inz(*Blanks)
005000211116     d Programa        s             50a   Inz(*Blanks)
005100211116     d CondError       s             50a   Inz(*Blanks)
005200210601     d*-----------------------------------------------------------------
005300210721      *?Programa que hace reverso de en caso de Rechazo
005400210721     d TPRev           PR                  Extpgm('CM4950R')
005500210721     d*-----------------------------------------------------------------
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
006600211115     d*
006700211115     D ExceptionMsg    S              7A   Import('_EXCP_MSGID')
006800211115     d Comando         s             97
006900211115     DRunCmd           Pr            10i 0 ExtProc('system')
007000211115     D   Cmd                           *   Value options(*string)
007100210601     d*-----------------------------------------------------------------
007200211115     D RESPTEMP1       s             10A   iNZ('RESPTEMP1')
007300211117     D RESPTEMP2       s             10A   iNZ('RESPTEMP2')
007400211115     D Artxtw          s             16A
007500211115     D Dperror         s             50A
007600211115     d*-----------------------------------------------------------------
007700210524     D @IN_TPP         S              1S 0   Inz(*Zeros)
007800210601     d rg              s              2  0 Inz(*zeros)
007900211117     d rg1             s              2  0 Inz(*zeros)
008000211117     D File_Old        s             10A
008100211115     D File_400        s             10A
008200211117     D File_400_1      s             10A
008300210601     D*?--------------------------------------------------------------------
008400210601     c*?Rutina Principal del Programa
008500210601     c*?--------------------------------------------------------------------
008600210601     c                   ExSr      RutIni
008700210601     c                   ExSr      CheqCC
008800210601     c                   ExSr      GenCC
008900211115     c                   Exsr      Elimina_file
009000211115     c                   Exsr      Crea_file
009100211115     c                   Exsr      Grabar_archRes
009200211115     c                   Exsr      Copia_archRes
009300210601     c                   ExSr      EnvCC
009400210601     c                   ExSr      HistCC
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
011200210601     c
011300210511      /FREE
011400211123
011500210601     ?*?--------------------------------SR---------------------------------
011600210526           BegSr GenCC;
011700210601
011800210511           EXEC SQL
011900210511
012000210511          INSERT INTO TPRDA01
012100210617            SELECT CHAR(01) AS TPTIPREG,
012200210617                   'RG' AS TPTDA,
012300210617                   915 AS TPCODBAN,
012400210617                   16 AS TPCODAS,
012500210803                  492 AS TPCODRE,
012600210617                   REPLACE(CHAR(CURRENT_DATE), '-', '') AS TPFECGEN,
012700210617                   1 AS TPNCOR,
012800210617                   COUNT(*) AS TPCANTAJ,
012900210617                   SUM(TPPMTO) AS TPTOTMOV,
013000210617                   '' AS TPPVR,
013100210617                   '' AS FILLER
013200210617                   FROM TPRDA02
013300210601               WHERE TPPCODRESP='' AND TPPORITRA=''  AND TPPVR='';
013400210524
013500210524               IF SQLCODE <>0  ;
013600210601               @IN_TPP= 1;
013700210721               ENDIF;
013800210601     ?*?--------------------------------W1---------------------------------
013900210511           EXEC SQL
014000210524
014100210524           INSERT INTO TPPRW
014200210524
014300210617            SELECT '01' || TPTDA || TPCODBAN || LPAD((TPCODAS),
014400210617                        10,
014500210617                        '0') || LPAD((TPCODRE),
014600210617                        10,
014700210617                        '0') || TPFECGEN || LPAD((:RG),
014800210617                        2,
014900210617                        '0') || LPAD((TPCANTAJ),
015000210617                        10,
015100210617                        '0') || REPLACE(LPAD((TPTOTMOV),
015200210617                            14,
015300210617                            '0'), '.', '') || LPAD((TPPVR),
015400210617                        13,
015500210617                        '0') AS CAMPO
015600210617                FROM TPRDA01 WHERE DATE
015700210617                (TIMESTAMP_FORMAT(CHAR(TPFECGEN), 'YYYYMMDD')) = current_date;
015800210524
015900210524               IF SQLCODE <> 0;
016000210524               @IN_TPP= 1;
016100210529               ENDIF;
016200210524
016300210524           IF  @IN_TPP <> 1;
016400210601     ?*?--------------------------------W2---------------------------------
016500210524          EXEC SQL
016600210524
016700210524         INSERT INTO TPPRW
016800210617             SELECT TPPREG||LPAD((TPPCODFP),2,'0')||LPAD((TPPCODES),10,'0') ||
016900210617             LPAD((TPPCODPR),10,'0')||TPPTIPER||LPAD(INT((TPPCEDREG)),12,'0')||
017000210617             LPAD((TPPBAPE),25,'')||LPAD((TPPBNOM),25,'')||
017100210617             RPAD((TPPNPAN),19,' ')||  REPLACE (LPAD((TPPMTO),14,'0'),'.','')||
017200210617              LPAD((TPPCODMON),3,'0') ||LPAD((TPPIDREC),10,' ')
017300210617               FROM TPRDA02 WHERE TPPCODRESP='' AND TPPORITRA='' AND TPPVR='';
017400210524
017500210529               IF SQLCODE<> 0;
017600210601               @IN_TPP= 2;
017700210601              ENDIF;
017800210601
017900210526              ENDIF;
018000210601
018100211026            EXSR ErrCC;
018200211026
018300210529          ENDSR;
018400210721
018500210721     ?*?-------------------------------CHQ-----------------------------------
018600210721
018700210529          BegSr CheqCC;
018800210721
018900210809            Exec SQL DELETE FROM TPPRW;
019000210526
019100210809            Exec SQL DELETE FROM TPRDA01;
019200210526
019300210526            EXEC SQL
019400211117            SELECT COUNT(*) INTO :RG1
019500210526            FROM TPHERDA
019600210526            WHERE DATE(TIMESTAMP_FORMAT(CHAR(TPFECGEN), 'YYYYMMDD'))
019700210601             = CURRENT_DATE AND TPTDA='RG';
019800211117
019900211117           IF RG1>= 1 ;
020000211117            RG  = RG1  + 1;
020100211117           ELSE ;
020200211117            RG = 1;
020300211117           ENDIF;
020400211123     ?*?-------------------------------JUL-----------------------------------
020500211123          EXEC SQL
020600211123
020700211123        SELECT DISTINCT CHAR(
020800211123         SUBSTR(DIGITS(YEAR(CURRENT DATE)), 04, 01) ||
020900211123          DIGITS(dec(JULIAN_DAY(CURRENT DATE) -
021000211123           JULIAN_DAY(CHAR(CHAR(YEAR(CURRENT DATE)),
021100211123               04) || '-01-01') + 1, 03, 00)), 4)
021200211123               INTO :Dia_jul
021300211123        FROM SYSIBM.SYSDUMMY1;
021400211123
021500211123     ?*?-------------------------------OLD-----------------------------------
021600211123
021700211117        NomOld = 'RG915' + %char(dia_jul) + %EDITC(RG1:'X');
021800211123        NomArchivo = 'RG915' + %char(dia_jul) + %EDITC(RG:'X');
021900211117
022000211115        File_400 = %trim(NomArchivo);
022100211117        File_Old = %trim(NomOld);
022200211117        File_400_1 =%trim('MQCONTROL');
022300211115
022400210601          ENDSR;
022500210721     ?*?-------------------------------ERR-----------------------------------
022600210721          BegSr ErrCC;
022700210721
022800210721            IF @IN_TPP<> 0;
022900210721
023000210721            Exec SQL
023100210721            INSERT INTO TPRDAR01
023200210721            SELECT TPTIPREG,TPTDA,
023300210721            TPFECGEN,TPNCOR,'*'
023400210721            AS TPPVER, FILLER  FROM TPRDA01;
023500210721
023600211026            TPRev ();
023700210721
023800210809            Exec SQL DELETE FROM TPPRW;
023900210721
024000210809            Exec SQL DELETE FROM TPRDA01;
024100211026
024200211026           *INLR = *ON;
024300211026            Return;
024400211026
024500210721            ENDIF;
024600210721
024700210721          ENDSR;
024800210601     ?*?-------------------------------MQ-----------------------------------
024900211115
025000211115       //------------------------------------------------------------//
025100211115       //?  Elimina Archivo                                          //
025200211115       //------------------------------------------------------------//
025300211115         Begsr Elimina_file;
025400211115
025500211117               Comando = ' DLTF FILE' + '(QTEMP/'+(File_Old) + ')';
025600211115               Exsr  Run_cmd;
025700211115               Comando = ' DLTF FILE' + '(QTEMP/'+(RESPTEMP1) + ')';
025800211115               Exsr  Run_cmd;
025900211117               Comando = ' DLTF FILE' + '(QTEMP/'+(File_400_1) + ')';
026000211117               Exsr  Run_cmd;
026100211117               Comando = ' DLTF FILE' + '(QTEMP/'+(RESPTEMP2) + ')';
026200211117               Exsr  Run_cmd;
026300211115         Endsr;
026400211115       //------------------------------------------------------------//
026500211115       //?  Crea Archivo                                             //
026600211115       //------------------------------------------------------------//
026700211115         Begsr Crea_file;
026800211115
026900211115               Comando = ' CRTPF FILE(QTEMP/'+(File_400)+ ')'+
027000211117                         ' RCDLEN(2000) ' +
027100211115                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
027200211115               Exsr  Run_cmd;
027300211115
027400211115
027500211115               Comando = ' CRTPF FILE(QTEMP/'+(RESPTEMP1)+ ')'+
027600211117                         ' RCDLEN(2000) ' +
027700211115                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
027800211115               Exsr  Run_cmd;
027900211117
028000211117                    Comando = ' CRTPF FILE(QTEMP/'+(File_400_1)+ ')'+
028100211117                         ' RCDLEN(1) ' +
028200211117                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
028300211117               Exsr  Run_cmd;
028400211117
028500211117               Comando = ' CRTPF FILE(QTEMP/'+(RESPTEMP2)+ ')'+
028600211117                         ' RCDLEN(1) ' +
028700211117                         ' MAXMBRS(*NOMAX) SIZE(*NOMAX) AUT(*ALL)';
028800211117               Exsr  Run_cmd;
028900211115
029000211115         Endsr;
029100211115
029200211115      //------------------------------------------------------------//
029300211115       //?  Graba Archivo de Respuesta cuando la cantidad de         //
029400211115       //?  registros de entrada es  = cero.                         //
029500211115       //------------------------------------------------------------//
029600211115         Begsr Grabar_archRes;
029700211115
029800211115               Exec Sql
029900211115                  INSERT INTO QTEMP/RESPTEMP1
030000211223                    SELECT * FROM TPPRW;
030100211117
030200211117            Exec SQL INSERT INTO QTEMP/RESPTEMP2 VALUES (' ');
030300211117
030400211115         Endsr;
030500211115
030600211115       //------------------------------------------------------------//
030700211115       //?  Copia hacia el archivo diario de TransferenciA      //
030800211115       //------------------------------------------------------------//
030900211115         Begsr Copia_archRes;
031000211115
031100211115           Comando = 'CPYF FROMFILE(QTEMP/' + (RESPTEMP1) + ')'+
031200211115                     ' TOFILE(QTEMP/' + (File_400) + ')'+
031300211115                     ' MBROPT(*REPLACE) '               +
031400211115                     ' FMTOPT(*NOCHK)';
031500211115           Exsr  Run_cmd;
031600211117
031700211117           Comando = 'CPYF FROMFILE(QTEMP/' + (RESPTEMP2) + ')'+
031800211117                     ' TOFILE(QTEMP/' + (File_400_1) + ')'+
031900211117                     ' MBROPT(*REPLACE) '               +
032000211117                     ' FMTOPT(*NOCHK)';
032100211117           Exsr  Run_cmd;
032200211117
032300211115
032400211115         Endsr;
032500211117       //------------------------------------------------------------//
032600211117       //Envio Archivo Base
032700211115       //------------------------------------------------------------//
032800211115
032900210601          BegSr EnvCC;
033000211115
033100210806
033200211223         Etiqueta   = %TRIM('SSHTPP');
033300211115         Biblioteca = 'QTEMP     ';
033400210806         Programa   = 'TPPR02  ';
033500211115
033600210601
033700211117         @tam = 2000;
033800211123         NomArchivo = 'RG915' + %char(dia_jul) + %EDITC(RG:'X');
033900211115         Nom400     = %trim(File_400);
034000210601         Tamano = %char(@tam);
034100210806
034200210601         Salida     = 'S';
034300210601         CondError  = '          ';
034400210601         SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
034500211117                      Programa: Tamano: CondError);
034600210809
034700211117       //------------------------------------------------------------//
034800211117       //Envio MQCONTROL
034900211117       //------------------------------------------------------------//
035000211117
035100211117           Nom400     = %trim(File_400_1);
035200211117           NomArchivo = %trim(File_400_1);
035300211117           Salida     = 'S';
035400211117           Tamano = '1';
035500211117           CondError  = '          ';
035600210809
035700211117            SSH003(Biblioteca: Nom400: NomArchivo: Salida: Etiqueta:
035800211117            Programa: Tamano: CondError);
035900211117
036000210601
036100210601          ENDSR;
036200210601
036300210601     ?*?------------------------------HIST---------------------------------
036400210526          BegSr HistCC;
036500210526
036600210526       EXEC SQL
036700210526
036800210526       INSERT INTO  TPHERDA  SELECT TPTIPREG, TPTDA, TPCODBAN,
036900210526       TPCODAS,TPCODRE,TPFECGEN,TPNCOR,
037000210601       TPCANTAJ,TPTOTMOV FROM TPRDA01 ;
037100211026
037200211026       EXEC SQL
037300211026       UPDATE TPRDA02 SET TPPCODRESP= 'E' WHERE TPPCODRESP = '';
037400210526
037500210601          ENDSR;
037600211115
037700211115       //------------------------------------------------------------//
037800211115       //?  Rutina de Ejecución de Comando.                          //
037900211115       //------------------------------------------------------------//
038000211115         Begsr Run_cmd;
038100211115
038200211115               Clear ExceptionMsg;
038300211115               Monitor;
038400211115               RunCmd(Comando);
038500211115               On-Error;
038600211115               EndMon;
038700211115           If (ExceptionMsg <> *Blanks);
038800211115             Dperror = 'Error en copiar';
038900211115           Endif;
039000211115         Endsr;
039100210526
039200210510      /END-FREE
039300210601     c*?---------------------------------------------------------------
039400210601     c*?Graba Trazas de Auditoria
039500210601     c*?---------------------------------------------------------------
039600210601     c     Auditoria     Begsr
039700210601     c*?   Graba registro en Mntor
039800210601     c                   Clear                   rmntor
039900210601     c                   Eval      MntBnk = wbanco
040000210601     c                   eval      MntFil = 'TPRDA02'
040100210601     c                   eval      Mntusr = User_Id
040200210601     c                   Time                    MntSyt
040300210601     c                   eval      MntSyd = Uday
040400210601     c                   eval      MntSym = Umonth
040500210601     c                   eval      MntSYy = Uyear
040600210601     c                   eval      MntMod = 'CARGA'
040700210601     c                   eval      MntMnu = 'SCHEDUL'
040800210601     c                   eval      MntPrg = 'TPPR02'
040900210601     c                   eval      MntOpt = *Zeros
041000210601     c                   eval      MntPrc = 'TPPR02'
041100210601     c                   eval      MntCrt = Job
041200210601     c                   Write     Rmntor
041300210601     c                   Endsr
