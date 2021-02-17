************************************************************************
*& PROGRAMA ID: ZFIGL_0008                                            &*
*& TITLE      :                                                       &*
*& CREATE DATE: 09.05.2014                                            &*
*& AUTHOR USER: IND679                                                &*
*& AUTHOR NAME: IVP                                                   &*
*& OT         : BD0K915101                                            &*
*& TKT/RICEF  :                                                       &*
*&--------------------------------------------------------------------&*
*& DESCRIPTION:  Programa visualización de tablas                     &*
*&                                                                    &*
*&                                                                    &*
*&                                                                    &*
************************************************************************
*& CHANGE HISTORY                                                     &*
************************************************************************
*& CREATE DATE:                                                       &*
*& AUTHOR NAME:                                                       &*
*& AUTHOR USER:                                                       &*
*& OT         :                                                       &*
*& TICKET     :                                                       &*
*& DESCRIPTION:                                                       &*
*&--------------------------------------------------------------------&*
REPORT  zfigl_0008.
INCLUDE zbw_bpc_top.         " Declaraciones Globales
INCLUDE zbw_bpc_s01.         " Selection Screen & SubScreen
INCLUDE zbw_bpc_f01.         " Subrutinas
*INCLUDE zbw_bpc_bi.          " Batch Input
*INCLUDE zbw_bpc_alv.         " ALV
*INCLUDE zbw_bpc_0100_pbo.    " Screen 0100 - PBO
*INCLUDE zbw_bpc_0100_pai.    " Screen 0100 - PAI


*----------------------------------------------------------------------*
*  INITIALIZATION                                                      *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_inicia_pantalla.

*----------------------------------------------------------------------*
*  START-OF-SELECTION                                                  *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM obtener_datos.

*----------------------------------------------------------------------*
*  END-OF-SELECTION                                                    *
*----------------------------------------------------------------------*
END-OF-SELECTION.
*PERFORM generar_alv.

*  INCLUDE zbw_bpc_alv.
*&---------------------------------------------------------------------*
*&      Form  CARGO_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cargo_fieldcat .

*TYPE-POOLS: slis.
*
*DATA: wa_fieldcat TYPE slis_fieldcat_alv,
*      it_fieldcat TYPE slis_t_fieldcat_alv,
*      wa_layout TYPE slis_layout_alv.
*
*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '01'.
*  wa_fieldcat-fieldname = 'company'.
*  wa_fieldcat-seltext_l = 'company'.
*  wa_fieldcat-tabname = 'gt_fcompany'.
*  APPEND wa_fieldcat TO it_fieldcat.
*
*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '02'.
*  wa_fieldcat-fieldname = 'objvers'.
*  wa_fieldcat-seltext_l = 'objvers'.
*  wa_fieldcat-tabname = 'gt_fcompany'.
*  APPEND wa_fieldcat TO it_fieldcat.
*
*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '03'.
*  wa_fieldcat-fieldname = 'changed'.
*  wa_fieldcat-seltext_l = 'changed'.
*  wa_fieldcat-tabname = 'gt_fcompany'.
*  APPEND wa_fieldcat TO it_fieldcat.
*
*   wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '04'.
*  wa_fieldcat-fieldname = 'comp_code'.
*  wa_fieldcat-seltext_l = 'comp_code'.
*  wa_fieldcat-tabname = 'gt_fcompany'.
*  APPEND wa_fieldcat TO it_fieldcat.
*
*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '05'.
*  wa_fieldcat-fieldname = '/bic/zcode_pa'.
*  wa_fieldcat-seltext_l = '/bic/zcode_pa'.
*  wa_fieldcat-tabname = 'gt_fcompany'.
*  APPEND wa_fieldcat TO it_fieldcat.
*
*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '06'.
*  wa_fieldcat-fieldname = 'pobox'.
*  wa_fieldcat-seltext_l = 'pobox'.
*  wa_fieldcat-tabname = 'gt_fcompany'.
*  APPEND wa_fieldcat TO it_fieldcat.
*
*  CLEAR wa_fieldcat.
*  wa_fieldcat-row_pos = '01'.
*  wa_fieldcat-col_pos = '07'.
*  wa_fieldcat-fieldname = 'txtmd'.
*  wa_fieldcat-seltext_l = 'txtmd'.
*  wa_fieldcat-tabname = 'gt_fcompany'.
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

  DATA:  wa_layout TYPE slis_layout_alv.


  CLEAR wa_layout.
  wa_layout-zebra = 'X'.
  wa_layout-detail_popup = 'X'.
  wa_layout-colwidth_optimize = 'X'.
  wa_layout-window_titlebar = 'T880'.

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

  TYPES:
    BEGIN OF tys_fcompany,
         company        TYPE  zpcompany-company,
         objvers        TYPE  zpcompany-objvers,
         changed        TYPE  zpcompany-changed,
         comp_code      TYPE  zpcompany-comp_code,
         /bic/zcode_pa  TYPE  zpcompany-/bic/zcode_pa,
         pobox          TYPE  zpcompany-pobox,
         txtmd          TYPE  ztcompany-txtmd,
    END OF tys_fcompany.

*DATA:  wa_layout TYPE slis_layout_alv,
*       it_fieldcat TYPE slis_t_fieldcat_alv,

*DATA:   gt_fcompany  TYPE TABLE OF tys_fcompany.

  DATA: gt_fcompany TYPE STANDARD TABLE OF tys_fcompany.


  DATA: gs_fcompany  TYPE tys_fcompany.

  TYPE-POOLS: slis.

  DATA: wa_fieldcat TYPE slis_fieldcat_alv,
        it_fieldcat TYPE slis_t_fieldcat_alv,
        wa_layout TYPE slis_layout_alv.

  SELECT  zpcompany~company
        zpcompany~objvers
        zpcompany~changed
        zpcompany~comp_code
        zpcompany~/bic/zcode_pa
        zpcompany~pobox
        ztcompany~txtmd
FROM zpcompany
INNER JOIN ztcompany ON
zpcompany~company = ztcompany~company
INTO TABLE gt_fcompany.


  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '01'.
  wa_fieldcat-fieldname = 'COMPANY'.
  wa_fieldcat-seltext_l = 'COMPANY'.
  wa_fieldcat-tabname = 'GT_FCOMPANY'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '02'.
  wa_fieldcat-fieldname = 'OBJVERS'.
  wa_fieldcat-seltext_l = 'OBJVERS'.
  wa_fieldcat-tabname = 'GT_FCOMPANY'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '03'.
  wa_fieldcat-fieldname = 'CHANGED'.
  wa_fieldcat-seltext_l = 'CHANGED'.
  wa_fieldcat-tabname = 'GT_FCOMPANY'.
  APPEND wa_fieldcat TO it_fieldcat.

  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '04'.
  wa_fieldcat-fieldname = 'COMP_CODE'.
  wa_fieldcat-seltext_l = 'COMP_CODE'.
  wa_fieldcat-tabname = 'GT_FCOMPANY'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '05'.
  wa_fieldcat-fieldname = '/BIC/ZCODE_PA'.
  wa_fieldcat-seltext_l = '/BIC/ZCODE_PA'.
  wa_fieldcat-tabname = 'GT_FCOMPANY'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '06'.
  wa_fieldcat-fieldname = 'POBOX'.
  wa_fieldcat-seltext_l = 'POBOX'.
  wa_fieldcat-tabname = 'GT_FCOMPANY'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-row_pos = '01'.
  wa_fieldcat-col_pos = '07'.
  wa_fieldcat-fieldname = 'TXTMD'.
  wa_fieldcat-seltext_l = 'TXTMD'.
  wa_fieldcat-tabname = 'GT_FCOMPANY'.
  APPEND wa_fieldcat TO it_fieldcat.


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
      t_outtab                          = gt_fcompany
 EXCEPTIONS
   program_error                     = 1
   OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV
