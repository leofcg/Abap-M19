*----------------------------------------------------------------------*
***INCLUDE ZBW_BPC_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_INICIA_PANTALLA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_inicia_pantalla .

*  CONCATENATE '@BJ@' text-004
  CONCATENATE '@BJ@' 'ZTFIGL_0002.- BPC- Cuentas y conceptos'
            INTO com4 SEPARATED BY space.
*  CONCATENATE '@BJ@' text-005
  CONCATENATE '@BJ@' 'ZTFIGL_0003.- Asignaciones'
            INTO com5 SEPARATED BY space.
*  CONCATENATE '@BJ@' text-006
  CONCATENATE '@BJ@' 'ZTFIGL_0006.- Fecha mensual de reporte a BPC'
            INTO com6 SEPARATED BY space.
*  CONCATENATE '@BJ@' text-008
  CONCATENATE '@BJ@' 'T880.- Global Company Data (for KONS Ledger)'
            INTO com8 SEPARATED BY space.


ENDFORM.                    " F_INICIA_PANTALLA
*&---------------------------------------------------------------------*
*&      Form  OBTENER_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM obtener_datos .

  TYPES:
**    "Estructuras
       BEGIN OF tys_0006,
         periodo     TYPE ZTFIGL_0007-periodo,
         fecha_desde TYPE ZTFIGL_0007-fecha_desde,
         fecha_hasta TYPE ZTFIGL_0007-fecha_hasta,
      END OF tys_0006,

      BEGIN OF tys_0002,
        ktopl       TYPE ztfigl_0002-ktopl,
        hkont_ini       TYPE ztfigl_0002-hkont_ini,
        hkont_fin TYPE ztfigl_0002-hkont_fin,
        asignacion  TYPE ztfigl_0002-asignacion,
        conc_debe   TYPE ztfigl_0002-conc_debe,
        conc_haber  TYPE ztfigl_0002-conc_haber,
      END OF tys_0002,

      BEGIN OF tys_0003,
        asignacion       TYPE ZTFIGL_0004-asignacion,
        apartado_soc_gl  TYPE ZTFIGL_0004-apartado_soc_gl,
        suma_euros       TYPE ZTFIGL_0004-suma_euros,
      END OF tys_0003,

*     BEGIN OF tys_880,
*       rcomp    TYPE t880-rcomp,
*     END OF tys_880.
      BEGIN OF tys_pcompany,
        company        TYPE  zpcompany-company,
        objvers        TYPE  zpcompany-objvers,
        changed        TYPE  zpcompany-changed,
        comp_code      TYPE  zpcompany-comp_code,
        /bic/zcode_pa  TYPE  zpcompany-/bic/zcode_pa,
        pobox          TYPE  zpcompany-pobox,
*       txtmd          TYPE  /bi0/tcompany-txtmd,
      END OF tys_pcompany,

   BEGIN OF tys_tcompany,
     company     TYPE  ztcompany-company,
     txtmd       TYPE  ztcompany-txtmd,
   END OF tys_tcompany,

   BEGIN OF tys_fcompany,
        company        TYPE  zpcompany-company,
        objvers        TYPE  zpcompany-objvers,
        changed        TYPE  zpcompany-changed,
        comp_code      TYPE  zpcompany-comp_code,
        /bic/zcode_pa  TYPE  zpcompany-/bic/zcode_pa,
        pobox          TYPE  zpcompany-pobox,
        txtmd          TYPE  ztcompany-txtmd,
   END OF tys_fcompany.

**    "Tablas Internas
  DATA: gt_0006      TYPE TABLE OF tys_0006,
        gt_0002      TYPE TABLE OF tys_0002,
        gt_0003      TYPE TABLE OF tys_0003,
        gt_pcompany  TYPE TABLE OF tys_pcompany,
        gt_tcompany  TYPE TABLE OF tys_tcompany,
        gt_fcompany  TYPE TABLE OF tys_fcompany.

  DATA: gs_0006      TYPE tys_0006,
        gs_0002      TYPE tys_0002,
        gs_0003      TYPE tys_0003,
        gs_pcompany  TYPE tys_pcompany,
        gs_tcompany  TYPE tys_tcompany,
        gs_fcompany  TYPE tys_fcompany.
** <INICIO> Modif. - Usuario: IND679 - Fecha: 27.03.2015
  DATA: lv_lim1 TYPE i VALUE '11000',
        lv_lim2 TYPE i VALUE '10000',
     	 lv_lim3 TYPE i VALUE '30000',
   lv_nlineas TYPE i.
** <FIN> Modif. - Usuario: IND679 - Fecha: 27.03.2015

  IF rb_0006 = 'X'.
*    SELECT * FROM ZTFIGL_0007
*       INTO CORRESPONDING FIELDS OF gs_0006.
*
*      APPEND gs_0006 TO gt_0006.
*      CLEAR gs_0006.
*
*    ENDSELECT.

** <INICIO> Modif. - Usuario: IND679 - Fecha: 27.03.2015
*    SELECT  periodo
*            fecha_desde
*            fecha_hasta
*    INTO TABLE gt_0006
*     FROM ZTFIGL_0007.
    SELECT  periodo fecha_desde fecha_hasta
      INTO TABLE gt_0006 UP TO lv_lim1 ROWS
        FROM ZTFIGL_0007.
    DESCRIBE TABLE gt_0006 LINES lv_nlineas.
    IF lv_nlineas = lv_lim1.
      MESSAGE 'Alcanzado el límite en ZTFIGL_0007' TYPE 'E'.
    ENDIF.
** <FIN> Modif. - Usuario: IND679 - Fecha: 27.03.2015


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
   i_buffer_active                   = 'X'
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
   i_structure_name                  = 'ZTFIGL_0007'
*   I_BACKGROUND_ID                   = ' '
   i_grid_title                      = 'Report monthly data to BPC'(001)
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
*   IT_FIELDCAT                       =
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
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
        t_outtab                     = gt_0006
 EXCEPTIONS
   program_error                     = 1
   OTHERS                            = 2
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF  rb_0002 = 'X'.
*    SELECT * FROM ztfigl_0002
*       INTO CORRESPONDING FIELDS OF gs_0002.
*
*      APPEND gs_0002 TO gt_0002.
*      CLEAR gs_0002.

** <INICIO> Modif. - Usuario: IND679 - Fecha: 27.03.2015
*    SELECT  ktopl
*            hkont
*            hkont_hasta
*            asignacion
*            conc_debe
*            conc_haber
*    INTO TABLE gt_0002
*     FROM ztfigl_0002.
    SELECT  ktopl hkont_ini hkont_fin asignacion conc_debe conc_haber
      INTO TABLE gt_0002 UP TO lv_lim2 ROWS
        FROM ztfigl_0002.
    DESCRIBE TABLE gt_0002 LINES lv_nlineas.
    IF lv_nlineas = lv_lim2.
      MESSAGE 'Alcanzado el límite en ztfigl_0002' TYPE 'E'.
    ENDIF.
** <FIN> Modif. - Usuario: IND679 - Fecha: 27.03.2015


* Se ordena la tabla por cuenta
    SORT gt_0002 BY hkont_ini.

*    ENDSELECT.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
   i_buffer_active                   = 'X'
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
   i_structure_name                  = 'ZTFIGL_0002'
*   I_BACKGROUND_ID                   = ' '
   i_grid_title                      = 'BPC – Accounts and concepts'(009)
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
*   IT_FIELDCAT                       =
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
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
        t_outtab                     = gt_0002
 EXCEPTIONS
   program_error                     = 1
   OTHERS                            = 2
              .
*    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

  ELSEIF rb_0003 = 'X'.
*    SELECT * FROM ZTFIGL_0004
*       INTO CORRESPONDING FIELDS OF gs_0003.
*
*      APPEND gs_0003 TO gt_0003.
*      CLEAR gs_0003.
*
*    ENDSELECT.
** <INICIO> Modif. - Usuario: IND679 - Fecha: 27.03.2015
*    SELECT   asignacion
*             apartado_soc_gl
*             suma_euros
*     INTO TABLE gt_0003
*     FROM ZTFIGL_0004.
    SELECT   asignacion apartado_soc_gl suma_euros
      INTO TABLE gt_0003 UP TO lv_lim3 ROWS
        FROM ZTFIGL_0004.
    DESCRIBE TABLE gt_0003 LINES lv_nlineas.
    IF lv_nlineas = lv_lim3.
      MESSAGE 'Alcanzado el límite en ZTFIGL_0004' TYPE 'E'.
    ENDIF.
** <FIN> Modif. - Usuario: IND679 - Fecha: 27.03.2015


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
 EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
   i_buffer_active                   = 'X'
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
   i_structure_name                  = 'ZTFIGL_0004'
*   I_BACKGROUND_ID                   = ' '
   i_grid_title                      = 'BPC – Asignations'(010)
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
*   IT_FIELDCAT                       =
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
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
        t_outtab                     = gt_0003
 EXCEPTIONS
   program_error                     = 1
   OTHERS                            = 2
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ELSEIF rb_0008 = 'X'.

*    select  /BI0/PCOMPANY~company
*            /BI0/PCOMPANY~objvers
*            /BI0/PCOMPANY~changed
*            /BI0/PCOMPANY~comp_code
*            /BI0/PCOMPANY~/bic/zcode_pa
*            /BI0/PCOMPANY~pobox
*            /BI0/TCOMPANY~txtmd
*    FROM /BI0/PCOMPANY
*    INNER JOIN /BI0/TCOMPANY ON
*    /BI0/PCOMPANY~company = /BI0/TCOMPANY~company
*    INTO TABLE gt_fcompany.


*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*  EXPORTING
**   I_INTERFACE_CHECK                 = ' '
**   I_BYPASSING_BUFFER                = ' '
*   i_buffer_active                   = 'X'
**   I_CALLBACK_PROGRAM                = ' '
**   I_CALLBACK_PF_STATUS_SET          = ' '
**   I_CALLBACK_USER_COMMAND           = ' '
**   I_CALLBACK_TOP_OF_PAGE            = ' '
**   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   i_structure_name                  = '/BI0/PCOMPANY'
**   I_BACKGROUND_ID                   = ' '
*   i_grid_title                      = 'Master data: Caract.Society GL'(011)
**   I_GRID_SETTINGS                   =
**   IS_LAYOUT                         =
**   IT_FIELDCAT                       =
**   IT_EXCLUDING                      =
**   IT_SPECIAL_GROUPS                 =
**   IT_SORT                           =
**   IT_FILTER                         =
**   IS_SEL_HIDE                       =
**   I_DEFAULT                         = 'X'
**   I_SAVE                            = ' '
**   IS_VARIANT                        =
**   IT_EVENTS                         =
**   IT_EVENT_EXIT                     =
**   IS_PRINT                          =
**   IS_REPREP_ID                      =
**   I_SCREEN_START_COLUMN             = 0
**   I_SCREEN_START_LINE               = 0
**   I_SCREEN_END_COLUMN               = 0
**   I_SCREEN_END_LINE                 = 0
**   I_HTML_HEIGHT_TOP                 = 0
**   I_HTML_HEIGHT_END                 = 0
**   IT_ALV_GRAPHICS                   =
**   IT_HYPERLINK                      =
**   IT_ADD_FIELDCAT                   =
**   IT_EXCEPT_QINFO                   =
**   IR_SALV_FULLSCREEN_ADAPTER        =
** IMPORTING
**   E_EXIT_CAUSED_BY_CALLER           =
**   ES_EXIT_CAUSED_BY_USER            =
*      TABLES
*        t_outtab                     = gt_fcompany
*       EXCEPTIONS
*   program_error                     = 1
*   OTHERS                            = 2
*              .
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

*PERFORM cargo_fieldcat.
    PERFORM seteo_layout.
    PERFORM display_alv.

  ENDIF.

ENDFORM.                    "OBTENER_DATOS
