*&---------------------------------------------------------------------*
*& Report  ZFIGL_0001
*&
*&---------------------------------------------------------------------*
*& Creacion: Leandro Fal
*& Fecha: 06/10/2011
*& Descripcion: Programa que baja el archivo de texto para BPC
*& Ticket: 8000005333 - GL - Global SAP Reporting
*&---------------------------------------------------------------------*

REPORT  zfigl_0001 MESSAGE-ID zbw001.


INCLUDE zfigl_0001_top.


****************************************
* Eventos
****************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'GUI_FILE_SAVE_DIALOG'
*   EXPORTING
*     WINDOW_TITLE            =
*     DEFAULT_EXTENSION       =
*     DEFAULT_FILE_NAME       =
*     WITH_ENCODING           =
*     FILE_FILTER             =
*     INITIAL_DIRECTORY       =
*     DEFAULT_ENCODING        =
   IMPORTING
*     FILENAME                =
*     PATH                    =
     fullpath                = v_str_file
     user_action             = v_user_action
*     FILE_ENCODING           =
            .
  IF v_user_action = 0 OR v_user_action = 1.
    p_file = v_str_file.
  ENDIF.  "if v_user_action = 0 or v_user_action = 1.

***********************
* AT SELECTION-SCREEN *
***********************
AT SELECTION-SCREEN.
  IF sy-ucomm = 'ONLI' OR sy-ucomm = 'SJOB'.
    PERFORM f_validar_autorizaciones.
  ENDIF.  "if sy-ucomm = 'ONLI' or sy-ucomm = 'SJOB'.

**********************
* START-OF-SELECTION *
**********************
START-OF-SELECTION.
***inicio Taborda ML
  IF NOT cbx_baja IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
       titlebar                    = 'Cuentas sin concepto '(004)
*   DIAGNOSE_OBJECT             = ' '
        text_question               = '¿Está seguro que desea bajar las cuentas sin concepto? '(003)
       text_button_1               = 'SI'(001)
*   ICON_BUTTON_1               = ' '
       text_button_2               = 'NO'(002)
*   ICON_BUTTON_2               = ' '
*   DEFAULT_BUTTON              = '1'
   display_cancel_button       = space
*   USERDEFINED_F1_HELP         = ' '
*   START_COLUMN                = 25
*   START_ROW                   = 6
*   POPUP_TYPE                  =
*   IV_QUICKINFO_BUTTON_1       = ' '
*   IV_QUICKINFO_BUTTON_2       = ' '
 IMPORTING
       answer                      = l_answer
* TABLES
*   PARAMETER                   =
 EXCEPTIONS
   text_not_found              = 1
   OTHERS                      = 2
              .
    IF sy-subrc <> 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSEIF l_answer = '2' . "si marca NO destildo el check

      EXIT.
    ENDIF.

  ENDIF.
***fin Taborda ML
  REFRESH: i_sfc,
           i_sfk,
           i_range.

* Se cargan los campos que se deben buscar
  CLEAR: wa_sfc.
  wa_sfc-chanm = '0CURRENCY'.
  wa_sfc-chaalias = 'MONEDA'.
  wa_sfc-orderby = 0.
  INSERT wa_sfc INTO TABLE i_sfc.

  CLEAR: wa_sfc.
  wa_sfc-chanm = '0COMP_CODE'.
  wa_sfc-chaalias = 'SOCIEDAD'.
  wa_sfc-orderby = 0.
  INSERT wa_sfc INTO TABLE i_sfc.

  CLEAR: wa_sfc.
  wa_sfc-chanm = 'ZCONCBPC'.
  wa_sfc-chaalias = 'CUENTA'.
  wa_sfc-orderby = 0.
  INSERT wa_sfc INTO TABLE i_sfc.

  CLEAR: wa_sfc.
  wa_sfc-chanm = '0PCOMPANY'.
  wa_sfc-chaalias = 'SOCIEDAD_GL'.
  wa_sfc-orderby = 0.
  INSERT wa_sfc INTO TABLE i_sfc.

  CLEAR: wa_sfc.
  wa_sfc-chanm = '0MOVE_TYPE'.
  wa_sfc-chaalias = 'CL_MOV'.
  wa_sfc-orderby = 0.
  INSERT wa_sfc INTO TABLE i_sfc.

  CLEAR: wa_sfk.
  wa_sfk-kyfnm = '0BALANCE'.
  wa_sfk-kyfalias = 'IMPORTE'.
  wa_sfk-aggr = 'SUM'.
  INSERT wa_sfk INTO TABLE i_sfk.

  CLEAR: wa_range.
  wa_range-chanm = '0FISCPER'.
  wa_range-sign = 'I'.
  wa_range-compop = 'EQ'.
  wa_range-low = p_perio.
  APPEND wa_range TO i_range.

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

  LOOP AT so_bukrs.
    CLEAR: wa_range.
    wa_range-chanm = '0COMP_CODE'.
    wa_range-sign = so_bukrs-sign.
    wa_range-compop = so_bukrs-option.
    wa_range-low = so_bukrs-low.
    wa_range-high = so_bukrs-high.
    APPEND wa_range TO i_range.
  ENDLOOP.  "loop at so_bukrs.


  REFRESH i_resul_cubo.
  CALL FUNCTION 'RSDRI_INFOPROV_READ'
    EXPORTING
      i_infoprov                   = 'ZGL_C02'
      i_th_sfc                     = i_sfc
      i_th_sfk                     = i_sfk
      i_t_range                    = i_range
*     I_TH_TABLESEL                =
*     I_T_RTIME                    =
*     I_REFERENCE_DATE             = SY-DATUM
*     I_ROLLUP_ONLY                = RS_C_TRUE
*     I_T_REQUID                   =
*     I_SAVE_IN_TABLE              = ' '
*     I_TABLENAME                  =
*     I_SAVE_IN_FILE               = ' '
*     I_FILENAME                   =
      i_packagesize                = 9999999
*     I_MAXROWS                    = 0
*     I_AUTHORITY_CHECK            = RSDRC_C_AUTHCHK-READ
*     I_CURRENCY_CONVERSION        = 'X'
*     I_USE_DB_AGGREGATION         = RS_C_TRUE
*     I_USE_AGGREGATES             = RS_C_TRUE
*     I_READ_ODS_DELTA             = RS_C_FALSE
*     I_CALLER                     = RSDRS_C_CALLER-RSDRI
*     I_DEBUG                      = RS_C_FALSE
*     I_CLEAR                      = RS_C_FALSE
   IMPORTING
     e_t_data                     = i_resul_cubo
*     E_END_OF_DATA                =
*     E_AGGREGATE                  =
*     E_SPLIT_OCCURRED             =
*     E_T_MSG                      =
*     E_STEPUID                    =
    CHANGING
      c_first_call                 = v_first_call
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
  ENDIF.

  REFRESH i_salida.
***inicio MLT  15/05/2012
  SORT i_resul_cubo ASCENDING BY cuenta.
***fin mlt
  LOOP AT i_resul_cubo INTO wa_resul_cubo.
***inicio MLT 15/05/2012
*Si el campo cbx_baja es inicial, filtrar el campo CUENTA(1) <> S
    IF cbx_baja IS INITIAL AND
      ( wa_resul_cubo-cuenta(1) EQ 'S' OR wa_resul_cubo-cuenta(1) EQ 's').
      CONTINUE.
    ENDIF.
***fin MLT 15/05/2012
    CLEAR wa_salida.
*Inicio mod ticket 8100019582
    CLEAR wa_decimales.
*Fin mod ticket 8100019582
    wa_salida-escenario = '0101'.
    CONCATENATE p_perio+2(2) p_perio+5(2) INTO wa_salida-ano_mes.
    wa_salida-sociedad = wa_resul_cubo-sociedad.
    SHIFT wa_salida-sociedad LEFT DELETING LEADING '0'.
    wa_salida-cuenta = wa_resul_cubo-cuenta.
    wa_salida-clase_mov = wa_resul_cubo-cl_mov.
    wa_salida-importe = wa_resul_cubo-importe.

*se filtra por sociedad para dejar el importe en miles para aquellas sociedades distintas a Chile y Colombia
*    IF wa_salida-sociedad <> '1025' OR wa_salida-sociedad <> '1167' OR wa_salida-sociedad <> '1070'.
*
*      wa_salida-importe = wa_salida-importe * 1000.
*
*    ENDIF.
***fin manejo de moneda.

    READ TABLE i_decimales INTO wa_decimales WITH KEY moneda = wa_resul_cubo-moneda.
    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE currkey currdec
      INTO wa_decimales
      FROM tcurx
      WHERE currkey = wa_resul_cubo-moneda.
      IF sy-subrc IS NOT INITIAL.
        wa_decimales-moneda = wa_resul_cubo-moneda.
        wa_decimales-decimales = 2.
*Inicio mod ticket 8100019582
      ELSE.
        IF wa_decimales-decimales = '0'.
** <INICIO> Modif. -  Ticket: 8000002934 - Usuario: HPI069 - Fecha: 07.11.2019
        " wa_salida-importe = wa_salida-importe / 100.
          wa_decimales-decimales = 2.
        IF wa_salida-importe = 0.
          wa_salida-importe = wa_salida-importe / 100.
        ENDIF.
** <FIN> Modif.-  Ticket: 8000002934 - Usuario: HPI069 - Fecha: 07.11.2019
        ELSEIF wa_decimales-decimales = '1'.
          wa_salida-importe = wa_salida-importe / 10.
        ELSEIF wa_decimales-decimales = '3'.
          wa_salida-importe = wa_salida-importe * 10.
        ELSEIF wa_decimales-decimales = '4'.
          wa_salida-importe = wa_salida-importe * 100.
        ENDIF.
*Fin mod ticket 8100019582
      ENDIF.  "if sy-subrc is not initial. (De la TCURX)
*      APPEND wa_decimales TO i_decimales.
    ENDIF.  "if sy-subrc is not initial. (De la tabla interna)
    TRANSLATE wa_salida-importe USING '. '.
    IF wa_resul_cubo-importe < 0.
      TRANSLATE wa_salida-importe USING '- '.
      CONCATENATE '-' wa_salida-importe
                  INTO wa_salida-importe.
    ENDIF.  "if wa_resul_cubo-importe < 0.
    CONDENSE wa_salida-importe NO-GAPS.
    SHIFT wa_salida-importe RIGHT DELETING TRAILING space.
*Inicio mod ticket 8100019582
*    shift wa_salida-importe right BY wa_decimales-decimales PLACES.
*Fin mod ticket 8100019582
    wa_salida-soc_gl = wa_resul_cubo-sociedad_gl.
    SHIFT wa_salida-soc_gl LEFT DELETING LEADING '0'.

    CONCATENATE wa_salida-escenario
                wa_salida-ano_mes
                wa_salida-sociedad
                wa_salida-cuenta
                wa_salida-clase_mov
                wa_salida-importe
                wa_salida-soc_gl
                INTO wa_salida_txt-linea
                SEPARATED BY ';'.
    CONDENSE wa_salida_txt-linea NO-GAPS.
    APPEND wa_salida_txt TO i_salida_txt.
  ENDLOOP.  "loop at i_resul_cubo into wa_resul_cubo.

  v_str_file = p_file.
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE                    =
      filename                        = v_str_file
*     FILETYPE                        = 'ASC'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS            = ABAP_TRUE
*   IMPORTING
*     FILELENGTH                      =
    TABLES
      data_tab                        = i_salida_txt
*     FIELDNAMES                      =
   EXCEPTIONS
     file_write_error                = 1
     no_batch                        = 2
     gui_refuse_filetransfer         = 3
     invalid_type                    = 4
     no_authority                    = 5
     unknown_error                   = 6
     header_not_allowed              = 7
     separator_not_allowed           = 8
     filesize_not_allowed            = 9
     header_too_long                 = 10
     dp_error_create                 = 11
     dp_error_send                   = 12
     dp_error_write                  = 13
     unknown_dp_error                = 14
     access_denied                   = 15
     dp_out_of_memory                = 16
     disk_full                       = 17
     dp_timeout                      = 18
     file_not_found                  = 19
     dataprovider_exception          = 20
     control_flush_error             = 21
     OTHERS                          = 22
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE text-m01 TYPE 'S'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_AUTORIZACIONES
*&---------------------------------------------------------------------*
* Se valida que el usuario tenga permisos para todas las sociedades
* seleccionadas
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_validar_autorizaciones .
  DATA: lt_sociedades TYPE STANDARD TABLE OF bukrs,
        lwa_sociedades TYPE bukrs,
        l_soc_error TYPE string.

  SELECT comp_code INTO TABLE lt_sociedades
  FROM zpcomp_code                                "#EC CI_SGLSELECT
  WHERE comp_code IN so_bukrs
    AND objvers = 'A'.

  LOOP AT lt_sociedades INTO lwa_sociedades.
    AUTHORITY-CHECK OBJECT 'Z_SOCIEDAD'
     ID 'BUKRS' FIELD lwa_sociedades.

    IF sy-subrc IS NOT INITIAL.
      CONCATENATE l_soc_error lwa_sociedades
      INTO l_soc_error
      SEPARATED BY space.
    ENDIF.  "if sy-subrc is not initial.

  ENDLOOP.  "loop at lt_sociedades into lwa_sociedades.

  IF l_soc_error IS NOT INITIAL.
    MESSAGE ID 'ZFIGL_0001' TYPE 'E' NUMBER 002
    WITH l_soc_error.
  ENDIF.  "if l_soc_error is not initial.

ENDFORM.                    " F_VALIDAR_AUTORIZACIONES
