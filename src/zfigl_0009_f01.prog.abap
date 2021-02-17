*&---------------------------------------------------------------------*
*&      Form  CARGO_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cargo_fieldcat .

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '01'.
  wa_fieldcat-fieldname = 'SOCIEDAD'.
  wa_fieldcat-seltext_l = 'SOCIEDAD'.
  wa_fieldcat-tabname = 'i_resul_cubo1'.
  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '02'.
*  wa_fieldcat-fieldname = 'PAIS'.
*  wa_fieldcat-seltext_l = 'PAIS'.
*  wa_fieldcat-tabname = 'i_resul_cubo1'.
*  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '02'.
  wa_fieldcat-fieldname = 'CUENTA'.
  wa_fieldcat-seltext_l = 'CUENTA'.
  wa_fieldcat-tabname = 'i_resul_cubo1'.
  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '04'.
*  wa_fieldcat-fieldname = 'PLAN_CUENTAS'.
*  wa_fieldcat-seltext_l = 'PLAN_CUENTAS'.
*  wa_fieldcat-tabname = 'i_resul_cubo1'.
*  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '03'.
  wa_fieldcat-fieldname = 'SOC_GL_ORIGEN'.
  wa_fieldcat-seltext_l = 'SOC_GL_ORIGEN'.
  wa_fieldcat-tabname = 'i_resul_cubo1'.
  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '04'.
  wa_fieldcat-fieldname = 'CONC_BPC'.
  wa_fieldcat-seltext_l = 'CONC_BPC'.
  wa_fieldcat-tabname = 'i_resul_cubo1'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '05'.
  wa_fieldcat-fieldname = 'SOC_GL_DESTINO'.
  wa_fieldcat-seltext_l = 'SOC_GL_DESTINO'.
  wa_fieldcat-tabname = 'i_resul_cubo1'.
  APPEND wa_fieldcat TO it_fieldcat.


*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '07'.
*  wa_fieldcat-fieldname = 'CL_MOV'.
*  wa_fieldcat-seltext_l = 'CL_MOV'.
*  wa_fieldcat-tabname = 'i_resul_cubo1'.
*  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '09'.
*  wa_fieldcat-fieldname = 'TIPO_MONEDA'.
*  wa_fieldcat-seltext_l = 'TIPO_MONEDA'.
*  wa_fieldcat-tabname = 'i_resul_cubo1'.
*  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '10'.
*  wa_fieldcat-fieldname = 'MONEDA_DOCU'.
*  wa_fieldcat-seltext_l = 'MONEDA_DOCU'.
*  wa_fieldcat-tabname = 'i_resul_cubo1'.
*  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '06'.
  wa_fieldcat-fieldname = 'VALOR_ACUMULADO'.
  wa_fieldcat-seltext_l = 'VALOR_ACUMULADO'.
  wa_fieldcat-tabname = 'i_resul_cubo1'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '07'.
  wa_fieldcat-fieldname = 'MONEDA'.
  wa_fieldcat-seltext_l = 'MONEDA'.
  wa_fieldcat-tabname = 'i_resul_cubo1'.
  APPEND wa_fieldcat TO it_fieldcat.


*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '12'.
*  wa_fieldcat-fieldname = 'VALOR_NETO'.
*  wa_fieldcat-seltext_l = 'VALOR_NETO'.
*  wa_fieldcat-tabname = 'i_resul_cubo1'.
*  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '13'.
*  wa_fieldcat-fieldname = 'TOTAL_CREDIT'.
*  wa_fieldcat-seltext_l = 'TOTAL_CREDIT'.
*  wa_fieldcat-tabname = 'i_resul_cubo1'.
*  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '14'.
*  wa_fieldcat-fieldname = 'TOTAL_DEBIT'.
*  wa_fieldcat-seltext_l = 'TOTAL_DEBIT'.
*  wa_fieldcat-tabname = 'i_resul_cubo1'.
*  APPEND wa_fieldcat TO it_fieldcat.


ENDFORM.                    " CARGO_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SETEO_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seteo_layout .

  CLEAR wa_layout.
  wa_layout-zebra = 'X'.
*  wa_layout-detail_popup = 'X'.
  wa_layout-colwidth_optimize = 'X'.
  wa_layout-window_titlebar = 'Balance report'(002).



ENDFORM.                    " SETEO_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
     i_callback_program                = sy-repid
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*     i_callback_top_of_page            = 'CARGA_ALV'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     is_layout                         = wa_layout
     it_fieldcat                       = it_fieldcat[]
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           = it_sort[]
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
     i_save                            = 'A'
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = i_resul_cubo1
 EXCEPTIONS
   program_error                     = 1
   OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV




*&---------------------------------------------------------------------*
*&      Form  f_tratamiento_asignacion_bpc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_tratamiento_asignacion_bpc.

  DATA: lv_cta(8) TYPE c.
  DATA: lv_valor_neto TYPE zoizvalnet.
  DATA: lv_valor_acum TYPE zoibalance.
  DATA: lv_soc_gl_origen TYPE zoipcompany.
  DATA: lv_cta1(10) TYPE c.
  DATA: lv_valor_acum1 TYPE zoibalance,
        it_cube_aux TYPE TABLE OF s_resul_cubo1.
       " wa_resul_aux TYPE s_resul_cubo1.

  DATA: lv_cuenta_aux TYPE zoigl_account,
        lv_valor_acum_aux TYPE zoibalance,
        lv_soc_gl_origen1 TYPE zoipcompany,
        lv_soc_gl_destino TYPE zoipcompany,
         vindex  LIKE sy-subrc.

  DATA: gc_flag  TYPE c,
        lv_first TYPE c.

* Se cargan los campos que se deben buscar del cubo zgl_c01
  REFRESH: i_sfc1,
           i_sfk1,
           i_range1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0COMP_CODE'.
  wa_sfc1-chaalias = 'SOCIEDAD'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0COMP_CODE__0COUNTRY'.
  wa_sfc1-chaalias = 'PAIS'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0GL_ACCOUNT'.
  wa_sfc1-chaalias = 'CUENTA'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0CHRT_ACCTS'.
  wa_sfc1-chaalias = 'PLAN_CUENTAS'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0PCOMPANY'.
*  wa_sfc1-chaalias = 'SOCIEDAD_GL'.
  wa_sfc1-chaalias = 'SOC_GL_ORIGEN'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = 'ZCONCBPC'.
  wa_sfc1-chaalias = 'CONC_BPC'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0PCOMPANY'.
*  wa_sfc1-chaalias = 'SOCIEDAD_GL'.
  wa_sfc1-chaalias = 'SOC_GL_DESTINO'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0MOVE_TYPE'.
  wa_sfc1-chaalias = 'CL_MOV'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.

  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0CURRENCY'.
  wa_sfc1-chaalias = 'MONEDA'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.


  CLEAR: wa_sfc1.
  wa_sfc1-chanm = '0CURTYPE'.
  wa_sfc1-chaalias = 'TIPO_MONEDA'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.


  CLEAR: wa_sfc1.
  wa_sfc1-chanm = 'ZMONDOC'.
  wa_sfc1-chaalias = 'MONEDA_DOCU'.
  wa_sfc1-orderby = 0.
  INSERT wa_sfc1 INTO TABLE i_sfc1.


  CLEAR: wa_sfk1.
  wa_sfk1-kyfnm = '0BALANCE'.
  wa_sfk1-kyfalias = 'VALOR_ACUMULADO'.
  wa_sfk1-aggr = 'SUM'.
  INSERT wa_sfk1 INTO TABLE i_sfk1.


  CLEAR: wa_sfk1.
  wa_sfk1-kyfnm = 'ZVALNET'.
  wa_sfk1-kyfalias = 'VALOR_NETO'.
  wa_sfk1-aggr = 'SUM'.
  INSERT wa_sfk1 INTO TABLE i_sfk1.


  CLEAR: wa_sfk1.
  wa_sfk1-kyfnm = '0CREDIT'.
  wa_sfk1-kyfalias = 'TOTAL_CREDIT'.
  wa_sfk1-aggr = 'SUM'.
  INSERT wa_sfk1 INTO TABLE i_sfk1.


  CLEAR: wa_sfk1.
  wa_sfk1-kyfnm = '0DEBIT'.
  wa_sfk1-kyfalias = 'TOTAL_DEBIT'.
  wa_sfk1-aggr = 'SUM'.
  INSERT wa_sfk1 INTO TABLE i_sfk1.


  CLEAR: wa_range1.
  wa_range1-chanm = '0FISCPER'.
  wa_range1-sign = 'I'.
  wa_range1-compop = 'EQ'.
  wa_range1-low = p_perio.
  APPEND wa_range1 TO i_range1.

  CLEAR: wa_range.
  wa_range-chanm = '0CURTYPE'.
  wa_range-sign = 'I'.
  wa_range-compop = 'EQ'.
  CASE 'X'.
    WHEN p_monloc.
      v_tipo_mon = '10'.
    WHEN p_monfte.
      v_tipo_mon = '40'.
  ENDCASE.  "case 'X'.
  wa_range-low = v_tipo_mon.
  APPEND wa_range TO i_range.

  LOOP AT so_bukrs INTO so_bukrs.
    CLEAR: wa_range1.
    wa_range1-chanm = '0COMP_CODE'.
    wa_range1-sign = so_bukrs-sign.
    wa_range1-compop = so_bukrs-option.
    wa_range1-low = so_bukrs-low.
    wa_range1-high = so_bukrs-high.
    APPEND wa_range1 TO i_range1.
  ENDLOOP.  "loop at so_bukrs.

****************************
* The reading module is called:
* the result is collected in packages of size 10.

* --- this variable will be set to TRUE when the last data
*     package is read
  gv_end_of_data = rs_c_false.
* --- this variable indicates whether this is an initial
*     call to the reading module or a follow-up call (which
*     simply retrieves already selected data)
  gv_first_call  = rs_c_true.

  WHILE gv_end_of_data = rs_c_false.

    REFRESH gt_data.

    CALL FUNCTION 'RSDRI_INFOPROV_READ'
      EXPORTING
        i_infoprov                   = 'ZGL_C01'
        i_th_sfc                     = i_sfc1
        i_th_sfk                     = i_sfk1
        i_t_range                    = i_range1
*   I_TH_TABLESEL                =
*   I_T_RTIME                    =
*   I_REFERENCE_DATE             = SY-DATUM
*   I_ROLLUP_ONLY                = RS_C_TRUE
*   I_T_REQUID                   =
*   I_SAVE_IN_TABLE              = ' '
*   I_TABLENAME                  =
*   I_SAVE_IN_FILE               = ' '
*   I_FILENAME                   =
       i_packagesize                = 1000
*   I_MAXROWS                    = 0
*   I_AUTHORITY_CHECK            = RSDRC_C_AUTHCHK-READ
*   I_CURRENCY_CONVERSION        = 'X'
*   I_USE_DB_AGGREGATION         = RS_C_TRUE
*   I_USE_AGGREGATES             = RS_C_TRUE
*   I_READ_ODS_DELTA             = RS_C_FALSE
*   I_CALLER                     = RSDRS_C_CALLER-RSDRI
*   I_DEBUG                      = RS_C_FALSE
*   I_CLEAR                      = RS_C_FALSE
     IMPORTING
       e_t_data                     = gt_data
       e_end_of_data                = gv_end_of_data
*   E_AGGREGATE                  =
*   E_SPLIT_OCCURRED             =
*   E_T_MSG                      =
*   E_STEPUID                    =
      CHANGING
        c_first_call                 = gv_first_call
     EXCEPTIONS
       illegal_input                = 1
       illegal_input_sfc            = 2
       illegal_input_sfk            = 3
       illegal_input_range          = 4
       illegal_input_tablesel       = 5
       no_authorization             = 6
       illegal_download             = 7
       illegal_tablename            = 8
       trans_no_write_mode          = 9
       inherited_error              = 10
       x_message                    = 11
       OTHERS                       = 12
              .

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ELSE.
      APPEND LINES OF gt_data TO i_resul_cubo1.
    ENDIF.
  ENDWHILE.

** <INICIO> Modif. -  Ticket: 6000260257 - Usuario: HPI069 - Fecha: 22.11.2017

it_cube_aux[] = i_resul_cubo1[].

SORT it_cube_aux BY soc_gl_origen.
DELETE ADJACENT DUPLICATES FROM it_cube_aux COMPARING soc_gl_origen.

IF NOT it_cube_aux IS INITIAL.
SELECT company /bic/zsoc_abs
      FROM zpcompany
      INTO TABLE it_comp_abs
     FOR ALL ENTRIES IN it_cube_aux
     WHERE  company EQ it_cube_aux-soc_gl_origen
       AND objvers EQ 'A'.

      IF sy-subrc EQ 0.
        SORT it_comp_abs BY company.
      ENDIF.

      LOOP AT i_resul_cubo1 INTO wa_resul_cubo1.
        vindex = sy-tabix.
        READ TABLE it_comp_abs INTO wa_comp_abs
                    WITH KEY company = wa_resul_cubo1-soc_gl_origen
                    BINARY SEARCH.
          IF sy-subrc EQ 0 AND wa_comp_abs-company NE
          wa_comp_abs-zsoc_abs.
              wa_resul_cubo1-soc_gl_origen = wa_comp_abs-zsoc_abs.
              MODIFY i_resul_cubo1 FROM wa_resul_cubo1 INDEX vindex.
          ENDIF.
      ENDLOOP.
ENDIF.

** <FIN> Modif.-  Ticket: 6000260257 - Usuario: HPI069 - Fecha: 22.11.2017
****** ZTFIGL_0001: Parámetros
  SELECT  mandt "#EC CI_NOWHERE
          fecha
          hora
          usuario
          sociedad
          periodo
    INTO TABLE i_ztfigl_0001
  FROM ztfigl_0001
  ORDER BY fecha DESCENDING hora DESCENDING.

  READ TABLE i_ztfigl_0001 INTO wa_ztfigl_0001 INDEX 1.


****** ztfigl_0004: Asignaciones
  SELECT    mandt  "#EC CI_NOWHERE
            asignacion
            apartado_soc_gl
            suma_euros
  INTO TABLE i_ztfigl_0004
    FROM ztfigl_0004
  ORDER BY asignacion.


  DELETE i_resul_cubo1 WHERE valor_acumulado = ''.

  DELETE i_resul_cubo1 WHERE cuenta(1) EQ 'N' OR
                             cuenta(1) EQ 'S' OR
                             cuenta(1) EQ 'C'.

  SORT i_resul_cubo1 BY cuenta.
  IF p_monloc = 'X'.
    DELETE i_resul_cubo1 WHERE tipo_moneda <> '10'.
  ELSEIF p_monfte = 'X'.
    DELETE i_resul_cubo1 WHERE tipo_moneda <> '40'.
  ENDIF.


  APPEND LINES OF i_resul_cubo1 TO gt_data_aux.
  SORT gt_data_aux BY cuenta plan_cuentas.
  DELETE ADJACENT DUPLICATES FROM gt_data_aux COMPARING cuenta plan_cuentas.
* Seleccionamos la tabla de mapeo de las ctas con el concepto BPC
  IF NOT gt_data_aux[] IS INITIAL.
    SELECT  mandt
            ktopl
            hkont_ini
            hkont_fin
            asignacion
            conc_debe
            conc_haber
     INTO TABLE i_ztfigl_0002
      FROM ztfigl_0002
      FOR ALL ENTRIES IN gt_data_aux
      WHERE ktopl = gt_data_aux-plan_cuentas
        AND  ( hkont_ini = gt_data_aux-cuenta OR
               hkont_ini <= gt_data_aux-cuenta AND
               hkont_fin >= gt_data_aux-cuenta ).
    SORT i_ztfigl_0002 BY ktopl hkont_ini hkont_fin.
    DELETE ADJACENT DUPLICATES FROM i_ztfigl_0002 COMPARING ALL FIELDS.
    SORT i_ztfigl_0002 BY ktopl hkont_ini hkont_fin asignacion.
    ENDIF.


  APPEND LINES OF i_resul_cubo1 TO gt_data_aux2.
  SORT gt_data_aux2 BY soc_gl_origen.
  DELETE ADJACENT DUPLICATES FROM gt_data_aux2 COMPARING soc_gl_origen.
* Seleccionamos la tabla de apartados
IF NOT gt_data_aux2[] IS INITIAL.
    SELECT company objvers
           changed comp_code
           /bic/zcode_pa pobox
    INTO TABLE i_apartados
    FROM zpcompany
    FOR ALL ENTRIES IN gt_data_aux2
    WHERE company = gt_data_aux2-soc_gl_origen
          AND objvers = 'A'.
    SORT i_apartados BY company objvers.
    DELETE ADJACENT DUPLICATES FROM i_apartados COMPARING ALL FIELDS.
  ENDIF.


  SORT i_resul_cubo1 BY soc_gl_origen cuenta.
  LOOP AT i_resul_cubo1 INTO wa_resul_cubo1.
    PERFORM f_busca_asignacion.
  ENDLOOP.


** <INICIO> Modif. - Usuario: IND679 - Fecha: 15.09.2014
* Se tienen que determinar el concepto BPC del debe o del haber
* teniendo en cuenta el acumulado de las cuentas a nivel de 8 dígitos.
  SORT i_resul_cubo1 BY Sociedad soc_gl_destino cuenta.
  LOOP AT i_resul_cubo1 INTO wa_resul_cubo1.

    lv_soc_gl_destino = wa_resul_cubo1-soc_gl_destino.

    AT NEW cuenta(8).
      IF wa_resul_cubo1-cuenta(3) EQ '572'.
        lv_cta = wa_resul_cubo1-cuenta(8).
      ENDIF.
    ENDAT.

    IF wa_resul_cubo1-cuenta(8) EQ lv_cta AND wa_resul_cubo1-cuenta(3) EQ '572'
       AND wa_resul_cubo1-soc_gl_destino EQ lv_soc_gl_destino.
      lv_valor_neto = lv_valor_neto + wa_resul_cubo1-valor_neto.
      lv_valor_acum = lv_valor_acum + wa_resul_cubo1-valor_acumulado.
    ENDIF.

    AT END OF cuenta(8).
      IF wa_resul_cubo1-cuenta(3) EQ '572'.
        MOVE lv_cta TO wa_resul_572-cuenta.
        MOVE lv_valor_neto TO wa_resul_572-valor_neto.
        MOVE lv_valor_acum TO wa_resul_572-valor_acumulado.
        MOVE lv_soc_gl_destino TO wa_resul_572-soc_gl.
        COLLECT wa_resul_572 INTO i_resul_572.
        CLEAR: lv_cta,
               lv_valor_neto,
               lv_valor_acum.
      ENDIF.
    ENDAT.
  ENDLOOP.
** <FIN> Modif.- Usuario: IND679 - Fecha: 15.09.2014


* Se acumulan los importes por sociedad y se indica la sociedad GL destino.
* Se borran los registros con valor acumulado = cero.
  LOOP AT i_resul_cubo1 INTO wa_resul_cubo1.
    MOVE-CORRESPONDING wa_resul_cubo1 TO wa_resul_aux.
    APPEND wa_resul_aux TO i_resul_aux.
  ENDLOOP.

  SORT i_resul_aux BY sociedad cuenta soc_gl_origen. "destino.
  LOOP AT i_resul_aux INTO wa_resul_aux.

    MOVE-CORRESPONDING wa_resul_aux TO wa_resul_aux2.

    AT NEW soc_gl_origen. "destino.
      lv_cuenta_aux = wa_resul_aux-cuenta.
    ENDAT.

    IF wa_resul_aux-cuenta EQ lv_cuenta_aux.
      lv_valor_acum = lv_valor_acum + wa_resul_aux-valor_acumulado.
    ENDIF.

    AT END OF soc_gl_origen. "destino.
      IF wa_resul_aux-cuenta EQ lv_cuenta_aux.
        MOVE lv_cuenta_aux TO wa_resul_aux2-cuenta.
        MOVE lv_valor_acum TO wa_resul_aux2-valor_acumulado.
        COLLECT wa_resul_aux2 INTO i_resul_aux2.
        CLEAR: lv_cuenta_aux,
               lv_valor_acum,
               lv_soc_gl_origen1.
      ENDIF.
    ENDAT.

  ENDLOOP.

  DELETE i_resul_aux2 WHERE valor_acumulado = ''.


  LOOP AT i_resul_aux2 INTO wa_resul_aux2.
* Se tienen que determinar el concepto BPC del debe o del haber
* teniendo en cuenta el acumulado de las cuentas a nivel de 8 dígitos.
    IF wa_resul_aux2-cuenta(3) = '572' AND wa_resul_aux2-cuenta(5) <> '57299'.
      READ TABLE i_resul_572 INTO wa_resul_572 WITH KEY cuenta = wa_resul_aux2-cuenta(8)
                                                        soc_gl = wa_resul_aux2-soc_gl_destino.
      IF sy-subrc = 0 AND wa_resul_572-valor_acumulado > 0.
        wa_resul_aux2-conc_bpc = wa_resul_aux2-conc_debe.
      ELSE.
        wa_resul_aux2-conc_bpc = wa_resul_aux2-conc_haber.
      ENDIF.
    ELSE.
      IF wa_resul_aux2-valor_acumulado > 0.
        wa_resul_aux2-conc_bpc = wa_resul_aux2-conc_debe.
      ELSE.
        wa_resul_aux2-conc_bpc = wa_resul_aux2-conc_haber.
      ENDIF.
    ENDIF.
    MODIFY i_resul_aux2 FROM wa_resul_aux2.
  ENDLOOP.

  REFRESH i_resul_cubo1.

*  i_resul_cubo1[] = i_resul_aux[].
  LOOP AT i_resul_aux2 INTO wa_resul_aux2.
    MOVE-CORRESPONDING wa_resul_aux2 TO wa_resul_cubo1.
    APPEND wa_resul_cubo1 TO i_resul_cubo1.
  ENDLOOP.


" 6000339894 Mejora para transaccion ZFIGL_0009 de Al
"Inicio de desarrollo tk 6000339894
"Solicitado por Carlos Camps
"Desarrollado por Javier Valletta (HPI069)

i_resul_cubo1_aux[] = i_resul_cubo1[].

SORT i_resul_cubo1_aux BY sociedad.

DELETE ADJACENT DUPLICATES FROM i_resul_cubo1_aux COMPARING sociedad.

IF i_resul_cubo1_aux[] IS NOT INITIAL.
  SELECT comp_code
   FROM ztbw_0113
   INTO TABLE it_comp_code
    FOR ALL ENTRIES IN i_resul_cubo1_aux
   WHERE comp_code EQ i_resul_cubo1_aux-sociedad.
ENDIF.

 IF sy-subrc EQ 0.

 SORT it_comp_code BY company.

   SELECT zgsoaccnt "#EC CI_NOWHERE
   INTO TABLE it_account
   FROM ztbw_0114
   UP TO 2000 ROWS.


    IF sy-subrc EQ 0.

      SORT it_account BY zgsoaccnt.
       SELECT SINGLE hieid FROM rshiedir INTO v_hierID
     WHERE objvers = 'A'
       AND hienm = 'BPC2'
       AND iobjnm = 'ZGSOACCNT'. "#EC CI_GENBUFF

       SELECT nodeid nodename parentid
        INTO TABLE it_hierarchy
        FROM ZHZGSOACCNT  "#EC CI_GENBUFF
        WHERE hieid EQ v_hierID.

IF it_hierarchy IS NOT INITIAL.

      LOOP AT it_hierarchy INTO wa_hierarchy.
         vindex = sy-tabix.
         wa_hierarchy-nodename = wa_hierarchy-nodename+4.
         MODIFY it_hierarchy FROM wa_hierarchy INDEX vindex.
      ENDLOOP.

       it_hierarchy_padre[] = it_hierarchy.

         SORT : it_hierarchy_padre BY nodeid,
                it_hierarchy BY nodename.



      LOOP AT i_resul_cubo1 INTO wa_resul_cubo1.
         vindex = sy-tabix.

         READ TABLE it_comp_code TRANSPORTING NO FIELDS WITH KEY
                             company = wa_resul_cubo1-sociedad
                                       BINARY SEARCH.
         IF sy-subrc EQ 0.
              READ TABLE it_account INTO wa_account
                                WITH KEY zgsoaccnt =
                                wa_resul_cubo1-cuenta
                                BINARY SEARCH.
          IF sy-subrc EQ 0.
             READ TABLE it_hierarchy INTO wa_hierarchy WITH KEY
                                            nodename =
                                            wa_resul_cubo1-cuenta
                                           BINARY SEARCH.
              IF sy-subrc EQ 0.
                READ TABLE it_hierarchy_padre INTO wa_hierarchy_padre
                WITH KEY
                                                     nodeid =
wa_hierarchy-parentid
                                                     BINARY SEARCH.
                 IF sy-subrc EQ 0.
                    wa_resul_cubo1-conc_bpc = wa_hierarchy_padre-nodename.
                    MODIFY i_resul_cubo1 FROM wa_resul_cubo1 INDEX vindex.
                  ENDIF.
              ENDIF.

          ENDIF.
          ENDIF.

      ENDLOOP.
 ENDIF.
    ENDIF.

 ENDIF.



"Fin de desarrollo tk 6000339894


ENDFORM.                    " f_tratamiento_asignacion_bpc


*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_ASIGNACION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_asignacion.

* Para buscar si la cuenta está excepcionada en la tabla de asignacines, primero buscamos
* por valor y si no se encuentra lo buscamos en el rango.
  CLEAR v_asig_encontrada.
  LOOP AT i_ztfigl_0002 INTO wa_ztfigl_0002 WHERE ktopl = wa_resul_cubo1-plan_cuentas AND
                                                  hkont_ini = wa_resul_cubo1-cuenta.

    PERFORM f_busca_apartado.
    IF v_asig_encontrada = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF sy-subrc NE 0.
    CLEAR v_asig_encontrada.
    LOOP AT i_ztfigl_0002 INTO wa_ztfigl_0002 WHERE ktopl = wa_resul_cubo1-plan_cuentas AND
                                                  ( hkont_ini <= wa_resul_cubo1-cuenta
                                              AND hkont_fin >= wa_resul_cubo1-cuenta ).

      PERFORM f_busca_apartado.
      IF v_asig_encontrada = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      wa_resul_cubo1-conc_debe = gc_noasig1.
      wa_resul_cubo1-conc_haber = gc_noasig1.
      MODIFY i_resul_cubo1 FROM wa_resul_cubo1.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_BUSCA_ASIGNACION


*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_APARTADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_apartado.

  READ TABLE i_ztfigl_0004 INTO wa_ztfigl_0004 WITH KEY asignacion =  wa_ztfigl_0002-asignacion.
  IF sy-subrc EQ 0.
    CASE wa_ztfigl_0004-suma_euros.
      WHEN ''.
            v_asig_encontrada = 'X'.
      WHEN 'EU'.  "Euros
        IF wa_resul_cubo1-moneda_docu = 'EUR'.
          v_asig_encontrada = 'X'.
        ENDIF.
      WHEN 'DEU'.  "Distinto de Euros
        IF wa_resul_cubo1-moneda_docu <> 'EUR'.
          v_asig_encontrada = 'X'.
        ENDIF.
    ENDCASE.
  ELSE.
    CLEAR v_asig_encontrada.
  ENDIF.

  IF v_asig_encontrada = 'X'.
    IF wa_ztfigl_0004-apartado_soc_gl NE '<>' AND
       wa_ztfigl_0004-apartado_soc_gl NE space AND
       wa_ztfigl_0004-apartado_soc_gl(1) NE '-'. "F, A

      wa_resul_cubo1-soc_gl_destino = wa_resul_cubo1-soc_gl_origen.

      CLEAR wa_apartados.
      READ TABLE i_apartados INTO wa_apartados
                         WITH KEY company = wa_resul_cubo1-soc_gl_origen.
      IF sy-subrc EQ 0.
        IF wa_ztfigl_0004-apartado_soc_gl = wa_apartados-pobox.

          wa_resul_cubo1-conc_debe = wa_ztfigl_0002-conc_debe.
          wa_resul_cubo1-conc_haber = wa_ztfigl_0002-conc_haber.

        ELSE.
          CLEAR v_asig_encontrada.  "tiene que buscar el siguiente mapeo
          wa_resul_cubo1-conc_debe = gc_noasig1.
          wa_resul_cubo1-conc_haber = gc_noasig1.
        ENDIF.

      ELSE.
        wa_resul_cubo1-conc_debe = gc_noasig1.
        wa_resul_cubo1-conc_haber = gc_noasig1.
      ENDIF.


    ELSE.  "SIN DESGLOSE GL
*     Como no tiene desglose, en la sociedad GL destino no se indica la sociedad GL origen.
      CLEAR wa_resul_cubo1-soc_gl_destino.
*      wa_resul_cubo1-soc_gl_destino = wa_resul_cubo1-soc_gl_origen.
      wa_resul_cubo1-conc_debe = wa_ztfigl_0002-conc_debe.
      wa_resul_cubo1-conc_haber = wa_ztfigl_0002-conc_haber.
    ENDIF.

  ELSE.
    wa_resul_cubo1-conc_debe = gc_noasig1.
    wa_resul_cubo1-conc_haber = gc_noasig1.
  ENDIF.
  MODIFY i_resul_cubo1 FROM wa_resul_cubo1.

ENDFORM.                    " F_BUSCA_APARTADO
