*&---------------------------------------------------------------------*
*& Include          ZCFR_0004_CLS
*&---------------------------------------------------------------------*

CLASS locl_get_data DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-DATA o_get_data TYPE REF TO locl_get_data READ-ONLY.
    METHODS read_data.
    METHODS create_file.
    METHODS get_alv.
    METHODS get_alv_error.
    METHODS field_cat.
    METHODS field_sort.
    METHODS field_layout.
    METHODS show_alv.
    METHODS show_alv_error.
    METHODS autorizaciones.
  PRIVATE SECTION.
    CONSTANTS gc_es1 TYPE bptaxtype VALUE 'ES1'.

ENDCLASS.                    "locl_get_data DEFINITION

CLASS locl_get_data IMPLEMENTATION.

* Método constructor...
  METHOD class_constructor.
    CREATE OBJECT locl_get_data=>o_get_data.
  ENDMETHOD.                    "class_constructor

* Método para lectura de datos...
  METHOD read_data.

    DATA: lv_ryear TYPE gjahr,
          lv_rpmax TYPE rpmax,
          ls_t880  TYPE ty_t880.

    CLEAR: gt_faglflext[], lv_ryear, lv_rpmax, gt_t880[],
           gt_0003[], gt_0005[], ls_t880.

    lv_ryear = p_perio(4).
    lv_rpmax = p_perio+4(2).

    SELECT *
      FROM ztfigl_soc
      INTO TABLE gt_soc
      WHERE bukrs EQ p_rbukrs.

    IF sy-subrc = 0.
      SORT gt_soc BY bukrs.
    ENDIF.

    SELECT ryear rpmax rbukrs rmvct hslvt hsl01 hsl02 hsl03 hsl04 hsl05 hsl06 hsl07 hsl08 hsl09 hsl10 hsl11 hsl12 hsl13 hsl14 hsl15 hsl16 rassc racct
           FROM faglflext
           INTO TABLE gt_faglflext
           WHERE rbukrs EQ p_rbukrs
             AND rldnr = '0L'
*             AND racct = '4300000000'
*             AND rpmax  EQ lv_rpmax
             AND ryear  EQ lv_ryear.
    IF sy-subrc EQ 0.
      SORT gt_faglflext.
* Busca el Apartado...
** <INICIO> Modif. -  Ticket/Proy: REQ-23971 - Usuario: STR526 - Fecha: 21.10.2019
      "Se quita la selección con FAE para utilizar select single ya que es
      "tabla buffer
*      SELECT rcomp pobox
*             FROM t880
*             INTO TABLE gt_t880
*             FOR ALL ENTRIES IN gt_faglflext
*             WHERE rcomp EQ gt_faglflext-rassc.
*      IF sy-subrc EQ 0.
*        SORT gt_t880.
** <FIN> Modif.-  Ticket/Proy: REQ-23971 - Usuario: STR526 - Fecha: 21.10.2019

* Respeta valores A y/o F, el resto lo complementa...
*        LOOP AT gt_t880 INTO ls_t880 WHERE pobox NE 'A'
*                                        OR pobox NE 'F'.
*          ls_t880-pobox = '<>'.
*          MODIFY gt_t880 FROM ls_t880.
*        ENDLOOP.
* Busca la Asignación...
** <INICIO> Modif. -REQ-72691- Usuario: STR540 - Fecha: 28.05.2020
*      SELECT  * FROM ztfigl_0003
*               INTO TABLE gt_0003
*               UP TO 99999 ROWS.
**                 FOR ALL ENTRIES IN gt_t880
**                 WHERE apartado_soc_gl EQ gt_t880-pobox(2).
      SELECT  * FROM ztfigl_0006
               INTO TABLE gt_0003
               UP TO 99999 ROWS.
*                 FOR ALL ENTRIES IN gt_t880
*                 WHERE apartado_soc_gl EQ gt_t880-pobox(2).
** <FIN> Modif. -  REQ-72691 - Usuario: STR540 - Fecha: 28.05.2020

      IF sy-subrc EQ 0.
        SORT gt_0003.
      ELSE.
        MESSAGE TEXT-001 TYPE 'I'.
      ENDIF.
** <INICIO> Modif. -  Ticket/Proy: REQ-23971 - Usuario: STR526 - Fecha: 21.10.2019
      "Se quita la selección con FAE para utilizar select single ya que es
      "tabla buffer
*      ENDIF.
** <FIN> Modif.-  Ticket/Proy: REQ-23971- Usuario: STR526 - Fecha: 21.10.2019

* Busca el Concepto...
      SELECT ktopl hkont_ini hkont_fin asignacion conc_debe_f conc_haber_f conc_debe_h conc_haber_h
         FROM ztfigl_0005
               INTO CORRESPONDING FIELDS OF TABLE gt_0005
               UP TO 99999 ROWS.
*               FOR ALL ENTRIES IN gt_faglflext.
*               WHERE hkont EQ gt_faglflext-racct
*               WHERE hkont_ini GE gt_faglflext-racct AND
*                     hkont_fin LE gt_faglflext-racct.
*        AND
*                     hkont_fin LE gt_faglflext-racct.
      IF sy-subrc EQ 0.
        SORT gt_0005.
      ENDIF.
    ELSE.
      o_get_data->get_alv_error( ).
    ENDIF.

  ENDMETHOD.                    "read_data

  METHOD create_file.

    TYPES: BEGIN OF ty_asignacion,
             asignacion TYPE zgl_asignacion,
           END OF ty_asignacion.
    TYPES: BEGIN OF ty_importes,
             importe TYPE string,
           END OF ty_importes.
    TYPES: BEGIN OF ty_racct,
             racct TYPE racct,
           END OF ty_racct.
    DATA: lt_asignacion    TYPE TABLE OF ty_asignacion,
          lt_asignacion_au TYPE TABLE OF ty_asignacion,
          lt_importes      TYPE TABLE OF ty_importes,
          lt_racct         TYPE TABLE OF ty_racct,
          lt_racct_au      TYPE TABLE OF ty_racct.

    DATA: ls_faglflext     TYPE ty_faglflext,
          ls_report        TYPE ty_report,
          ls_report_au     TYPE ty_report2,
          ls_report_aux    TYPE ty_report,
          ls_fichero       TYPE ty_fichero,
          ls_importes      LIKE LINE OF lt_importes,
          lv_file          TYPE string,
          ls_t880          TYPE ty_t880,
          ls_0003          TYPE ztfigl_0003,
          ls_0005          TYPE ztfigl_0005,
          lv_max           TYPE tslxx12,
          lv_tam_ch(2)     TYPE c,
          lv_tam           TYPE i,
          lv_campo         TYPE string,
          lv_cont          TYPE i,
          lv_importe(30)   TYPE c,
          lv_concep        TYPE i,
          lv_cont_gl       TYPE i,
          lv_cont_au       TYPE i,
          lv_concep_si     TYPE i,
          lv_impordebe     TYPE string,
          lv_imporhaber    TYPE string,
          lv_index         TYPE i,
          lv_impor         TYPE char21,
          lv_impor_tot     TYPE p LENGTH 16 DECIMALS 2,
          lv_tam_report    TYPE i,
          lv_cuentas       TYPE i,
          lv_conce(11)     TYPE c,
          lv_debeyhaber(1) TYPE c,
          lv_conce_au      TYPE zconcbpc.


    FIELD-SYMBOLS: <ls_any>           TYPE any,
                   <ls_imp>           TYPE tslxx12,
                   <ls_0003>          LIKE LINE OF gt_0003,
                   <ls_0005>          LIKE LINE OF gt_0005,
                   <ls_asig>          LIKE LINE OF lt_asignacion,
                   <ls_asig_au>       LIKE LINE OF lt_asignacion,
                   <ls_report>        TYPE ty_report,
                   <ls_report_fin>    TYPE ty_report2,
                   <ls_report_fin_si> TYPE ty_report,
                   <ls_fin_si>        TYPE ty_report,
                   <ls_report_au>     TYPE ty_report,
                   <ls_report_ant>    TYPE ty_report,
                   <ls_importes>      LIKE LINE OF  lt_importes,
                   <ls_racct>         LIKE LINE OF lt_racct,
                   <ls_racct_au>      LIKE LINE OF lt_racct.

    CONSTANTS: lc_si(2) TYPE c VALUE 'SI',
               lc_no(2) TYPE c VALUE 'NO'.
*
*    CLEAR: ls_faglflext, ls_report, ls_fichero, lv_file.

    SORT gt_t880 BY rcomp.
    SORT gt_0003 BY asignacion.
    SORT gt_0005 BY hkont_ini hkont_fin conc_debe_f conc_haber_f conc_debe_h conc_haber_h.
    SORT gt_faglflext BY rassc racct.
    LOOP AT gt_faglflext INTO ls_faglflext.
      ls_report-racct = ls_faglflext-racct.
      ls_report-rassc = ls_faglflext-rassc.
* Cálculo del Concepto...

      IF ls_report-racct <> ls_report_aux-racct AND
       ls_report-rassc <> ls_report_aux-rassc AND
*       ls_report-conce_debe <> ls_report_aux-conce_debe.
        ls_report-conce <> ls_report_aux-conce.
        CLEAR ls_report-impor.
      ENDIF.

      WHILE lv_tam < p_perio+4(2).
        lv_tam = lv_tam + 1.
        ASSIGN lv_tam TO <ls_any>.
        lv_tam_ch = <ls_any>.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_tam_ch
          IMPORTING
            output = lv_tam_ch.
        ASSIGN lv_tam_ch TO <ls_any>.
        CONCATENATE 'HSL' <ls_any>  INTO lv_campo.
        ASSIGN COMPONENT lv_campo OF STRUCTURE ls_faglflext TO <ls_imp>.
        ASSIGN <ls_imp> TO <ls_any>.
        ls_report-impor = ls_report-impor + <ls_any>.
      ENDWHILE.
      ls_report-impor = ls_report-impor + ls_faglflext-hslvt.
      CLEAR: ls_t880.
** <INICIO> Modif. -  Ticket/Proy: REQ-23971 - Usuario: STR526 - Fecha: 21.10.2019
      "Se sustituye el read table por select single ya que es tabla buffer
*      READ TABLE gt_t880 INTO ls_t880 WITH KEY rcomp = ls_faglflext-rassc BINARY SEARCH.
      SELECT SINGLE rcomp pobox
        INTO ls_t880
        FROM t880
        WHERE rcomp EQ ls_faglflext-rassc.
** <FIN> Modif. -  Ticket/Proy: REQ-23971 - Usuario: STR526 - Fecha: 21.10.2019
      IF sy-subrc = 0.
        LOOP AT gt_0003 ASSIGNING <ls_0003>.
          IF <ls_0003>-signo = '='.
            IF ( ls_t880-pobox = <ls_0003>-apartado_soc_gl1 AND <ls_0003>-apartado_soc_gl1 IS NOT INITIAL ) OR
              ( ls_t880-pobox = <ls_0003>-apartado_soc_gl2 AND <ls_0003>-apartado_soc_gl2 IS NOT INITIAL ) OR
              ( ls_t880-pobox = <ls_0003>-apartado_soc_gl3 AND <ls_0003>-apartado_soc_gl3 IS NOT INITIAL ) OR
              ( ls_t880-pobox = <ls_0003>-apartado_soc_gl4 AND <ls_0003>-apartado_soc_gl4 IS NOT INITIAL ) OR
              ( ls_t880-pobox = <ls_0003>-apartado_soc_gl5 AND <ls_0003>-apartado_soc_gl5 IS NOT INITIAL ) .
              APPEND INITIAL LINE TO lt_asignacion ASSIGNING <ls_asig>.
              ASSIGN <ls_0003>-asignacion TO <ls_any>.
              <ls_asig> = <ls_any>.
            ENDIF.
          ELSEIF <ls_0003>-signo = '<>'.
            IF ( ls_t880-pobox <> <ls_0003>-apartado_soc_gl1  OR <ls_0003>-apartado_soc_gl1 IS INITIAL ) AND
               ( ls_t880-pobox <> <ls_0003>-apartado_soc_gl2  OR <ls_0003>-apartado_soc_gl2 IS INITIAL ) AND
               ( ls_t880-pobox <> <ls_0003>-apartado_soc_gl3  OR <ls_0003>-apartado_soc_gl3 IS INITIAL ) AND
               ( ls_t880-pobox <> <ls_0003>-apartado_soc_gl4  OR <ls_0003>-apartado_soc_gl4 IS INITIAL ) AND
               ( ls_t880-pobox <> <ls_0003>-apartado_soc_gl5  OR <ls_0003>-apartado_soc_gl5 IS INITIAL ).
              APPEND INITIAL LINE TO lt_asignacion ASSIGNING <ls_asig>.
              ASSIGN  <ls_0003>-asignacion TO <ls_any>.
              <ls_asig> = <ls_any>.
            ENDIF.
          ELSE.
            APPEND INITIAL LINE TO lt_asignacion ASSIGNING <ls_asig>.
            ASSIGN <ls_0003>-asignacion TO <ls_any>.
            <ls_asig> = <ls_any>.
          ENDIF.
        ENDLOOP.
      ELSE.
*        READ TABLE gt_003 WITH KEY
        LOOP AT gt_0005 ASSIGNING <ls_0005>.
          IF ( ls_faglflext-racct >= <ls_0005>-hkont_ini AND ls_faglflext-racct <= <ls_0005>-hkont_fin )
            OR ( ls_faglflext-racct = <ls_0005>-hkont_ini )
            OR ( ls_faglflext-racct = <ls_0005>-hkont_fin ) .
            APPEND INITIAL LINE TO lt_asignacion_au ASSIGNING <ls_asig>.
            ASSIGN <ls_0005>-asignacion TO <ls_any>.
            <ls_asig> = <ls_any>.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF lt_asignacion_au[] IS NOT INITIAL OR lt_asignacion[] IS NOT INITIAL.
        IF lt_asignacion_au[] IS NOT INITIAL.

          LOOP AT lt_asignacion_au ASSIGNING <ls_asig>.
            READ TABLE gt_0003 WITH KEY asignacion =  <ls_asig>-asignacion ASSIGNING <ls_0003> BINARY SEARCH.
            IF <ls_0003> IS ASSIGNED AND <ls_0003>-signo = '<>'.
              APPEND INITIAL LINE TO lt_asignacion ASSIGNING <ls_asig_au>.
              ASSIGN <ls_0003>-asignacion TO <ls_any>.
              <ls_asig_au> = <ls_any>.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF lt_asignacion[] IS INITIAL.
          lt_asignacion[] = lt_asignacion_au[].
        ENDIF.
        SORT  lt_asignacion BY asignacion.
        LOOP AT gt_0005 ASSIGNING <ls_0005>.

          IF ( ls_faglflext-racct >= <ls_0005>-hkont_ini AND ls_faglflext-racct <= <ls_0005>-hkont_fin ) OR
             ( ls_faglflext-racct = <ls_0005>-hkont_ini ) OR
             ( ls_faglflext-racct = <ls_0005>-hkont_fin ).
            READ TABLE lt_asignacion WITH KEY asignacion = <ls_0005>-asignacion ASSIGNING <ls_asig> BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE gt_0003 WITH KEY asignacion = <ls_asig>-asignacion ASSIGNING <ls_0003> BINARY SEARCH.
              IF sy-subrc = 0.
                ls_report-desg_sociedadgl = <ls_0003>-desg_sociedadgl.
                ls_report-asignacion      = <ls_0003>-asignacion.
                ls_report-desg_debeyhaber = <ls_0003>-desg_debeyhaber.
              ENDIF.

              READ TABLE gt_soc TRANSPORTING NO FIELDS WITH KEY bukrs = ls_faglflext-rbukrs BINARY SEARCH.
              IF sy-subrc = 0.
                ls_report-conce_debe = <ls_0005>-conc_debe_h.
                ls_report-conce_haber = <ls_0005>-conc_haber_h.
              ELSE.
                ls_report-conce_debe = <ls_0005>-conc_debe_f.
                ls_report-conce_haber = <ls_0005>-conc_haber_f.
              ENDIF.

              CONTINUE.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

* Lógica para el report...
      IF p_fisc EQ abap_true.
        ls_report-escen = gc_0101.
      ELSEIF p_fisce EQ abap_true.
        ls_report-escen = gc_0102.
      ENDIF.

      CONCATENATE ls_faglflext-ryear+2(2) ls_faglflext-rpmax+1(2)
                  INTO ls_report-perio.
      ls_report-perio = p_perio+2(4).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_faglflext-rbukrs
        IMPORTING
          output = ls_report-bukrs.
*      ls_report-rmvct = ls_faglflext-rmvct.
* Quitar cero a la izquierda en sociedad GL
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_faglflext-rassc
        IMPORTING
          output = ls_report-rassc.

*
      ls_report-impor_new = ls_report-impor * 100.
      APPEND ls_report TO gt_report.
      CLEAR ls_report.
      CLEAR lv_tam.
      CLEAR ls_fichero.
      CLEAR ls_0005.
      CLEAR ls_0003.
      CLEAR ls_faglflext.
      CLEAR ls_t880.
      CLEAR lt_asignacion[].
      CLEAR lt_asignacion_au[].
    ENDLOOP.


    DELETE gt_report WHERE desg_sociedadgl IS INITIAL AND desg_debeyhaber IS INITIAL.
    SORT gt_report BY racct conce_haber conce_debe desg_sociedadgl asignacion .

    gt_report_au[] = gt_report[].
    SORT gt_report_au BY racct conce_haber conce_debe asignacion.
    CLEAR gt_report[].
    LOOP AT gt_report_au ASSIGNING <ls_report_au>.
      IF <ls_report> IS ASSIGNED.
        IF ( <ls_report_au>-racct = <ls_report>-racct ) AND
          ( <ls_report_au>-conce_debe <> <ls_report>-conce_debe AND
            <ls_report_au>-conce_haber <> <ls_report>-conce_haber ) .
          CLEAR lv_impor_tot.
        ENDIF.
      ENDIF.
      lv_impor_tot = lv_impor_tot + <ls_report_au>-impor.
      IF <ls_report_au>-desg_sociedadgl = 'NO'.
        AT END OF racct.
          APPEND INITIAL LINE TO gt_report ASSIGNING <ls_report>.
          <ls_report> = <ls_report_au>.
          <ls_report>-impor = lv_impor_tot.
          <ls_report>-impor_new = <ls_report>-impor * 100.
          IF <ls_report>-impor >= 0.
            <ls_report>-conce = <ls_report_au>-conce_debe.
          ELSE.
            <ls_report>-conce = <ls_report_au>-conce_haber.
          ENDIF.
** <INICIO> Modif. - Ticket/Proy: 8000005674 - Usuario: STR563 - Fecha: 23.04.2020
          CLEAR <ls_report>-rassc.
** <FIN> Modif. - Ticket/Proy: 8000005674 - Usuario: STR563 - Fecha: 23.04.2020
          IF <ls_report>-desg_debeyhaber = 'SI'.
            APPEND INITIAL LINE TO gt_report_fin_si ASSIGNING <ls_report_fin_si>.
            <ls_report_fin_si> = <ls_report>.
          ENDIF.
          CLEAR lv_impor_tot.
        ENDAT.
      ELSE.
        AT END OF rassc.
          APPEND INITIAL LINE TO gt_report ASSIGNING <ls_report>.
          <ls_report> = <ls_report_au>.
          <ls_report>-impor = lv_impor_tot.
          <ls_report>-impor_new = <ls_report>-impor * 100.
          IF <ls_report>-impor >= 0.
            <ls_report>-conce = <ls_report_au>-conce_debe.
          ELSE.
            <ls_report>-conce = <ls_report_au>-conce_haber.
          ENDIF.
          IF <ls_report>-desg_debeyhaber = 'SI'.
            APPEND INITIAL LINE TO gt_report_fin_si ASSIGNING <ls_report_fin_si>.
            <ls_report_fin_si> = <ls_report>.
          ENDIF.
          CLEAR lv_impor_tot.
        ENDAT.
      ENDIF.
      ASSIGN <ls_report_au> TO <ls_report>.
    ENDLOOP.
    CLEAR lv_cont.
***    SORT gt_report BY conce rassc racct asignacion.
    SORT gt_report BY conce rassc asignacion.
    gt_report_au[] = gt_report[].
    LOOP AT gt_report_au ASSIGNING <ls_report_au>.
      AT FIRST.
        lv_cont_au = lv_cont_au + 1.
      ENDAT.
      IF ( lv_cont_au >= 1 ) AND ( <ls_report_au>-conce <> lv_conce ) AND
        <ls_report_au>-conce IS NOT INITIAL.
*        ( <ls_report_au>-conce_debe = <ls_report_au>-conce_haber ).
        LOOP AT gt_report ASSIGNING <ls_report> WHERE conce = <ls_report_au>-conce.
          IF ( <ls_report>-conce_debe <> <ls_report>-conce_haber OR
            "NEW
            <ls_report>-conce_debe = <ls_report>-conce_haber ).
            <ls_report>-asignacion = <ls_report_au>-asignacion.
            <ls_report>-desg_sociedadgl =  <ls_report_au>-desg_sociedadgl.
            <ls_report>-desg_debeyhaber = <ls_report_au>-desg_debeyhaber.
          ENDIF.
        ENDLOOP.
      ENDIF.
      lv_conce = <ls_report_au>-conce.
    ENDLOOP.
    CLEAR gt_report_au[].
    CLEAR lv_conce.
    CLEAR lv_cont_au.

    SORT gt_report BY conce rassc racct asignacion.
    SORT gt_report_fin_si BY conce rassc impor.
* Recorremos la tabla intermedia que nos servirá para rellenar la tabla interna de salir del ALV y del fichero
    DESCRIBE TABLE gt_report LINES lv_tam_report.
    LOOP AT gt_report ASSIGNING <ls_report>.
      lv_concep = lv_concep + 1.
      lv_concep_si = lv_concep_si + 1.
      lv_index = lv_index + 1.
      IF lv_index = 1.
        APPEND INITIAL LINE TO lt_racct ASSIGNING <ls_racct>.
        <ls_racct> = <ls_report>-racct.
        ls_report_au-cuentas = ls_report_au-cuentas + 1.
*        lv_cont_gl = lv_cont_gl + 1.
      ELSE.
        IF ls_report_au-rassc <> <ls_report>-rassc AND ls_report_au-conce = <ls_report>-conce.
          lv_cont_gl = lv_cont_gl + 1.
        ENDIF.
        IF lt_racct_au[] IS NOT INITIAL.
          READ TABLE lt_racct_au INDEX 1 ASSIGNING <ls_racct>.
          IF <ls_racct> IS ASSIGNED.
            APPEND INITIAL LINE TO lt_racct ASSIGNING <ls_racct_au>.
            <ls_racct_au> = <ls_racct>.
          ENDIF.
          CLEAR lt_racct_au.
          CLEAR lv_cuentas.
        ENDIF.
        READ TABLE lt_racct WITH KEY racct = <ls_report>-racct ASSIGNING <ls_racct>.
        IF <ls_racct> IS NOT ASSIGNED.
*          IF <ls_report>-conce_debe = ls_report_au-conce_debe AND ( <ls_report>-racct <> ls_report_au-racct  ).
          IF <ls_report>-conce = ls_report_au-conce AND ( <ls_report>-racct <> ls_report_au-racct  ).
            APPEND INITIAL LINE TO lt_racct ASSIGNING <ls_racct>.
            <ls_racct> = <ls_report>-racct.
            ls_report_au-cuentas = ls_report_au-cuentas + 1.

          ELSEIF <ls_report>-racct <> ls_report_au-racct .
            APPEND INITIAL LINE TO lt_racct_au ASSIGNING <ls_racct>.
            <ls_racct> = <ls_report>-racct.
            lv_cuentas = lv_cuentas + 1.
          ENDIF.
        ENDIF.
      ENDIF.

      AT FIRST.
        ls_report_au-racct = <ls_report>-racct.
        ls_report_au-rassc = <ls_report>-rassc.
        ls_report_au-conce_debe = <ls_report>-conce_debe.
        ls_report_au-conce_haber = <ls_report>-conce_haber.
        ls_report_au-conce = <ls_report>-conce.
        ls_report_au-desg_sociedadgl = <ls_report>-desg_sociedadgl.
        ls_report_au-desg_debeyhaber = <ls_report>-desg_debeyhaber.
        ls_report_au-escen = <ls_report>-escen.
        ls_report_au-perio = <ls_report>-perio.
        ls_report_au-bukrs = <ls_report>-bukrs.
*        ls_report_au-rmvct = <ls_report>-rmvct.
      ENDAT.
****      IF <ls_report>-conce_debe <> ls_report_au-conce_debe AND
****         <ls_report>-conce_haber = ls_report_au-conce_haber.
****          ls_report_au-conce_debe = <ls_report>-conce_haber.
****          <ls_report>-asignacion = ls_report_au-asignacion.
****          <ls_report>-desg_debeyhaber = ls_report_au-desg_debeyhaber.
****          lv_debeyhaber = 'X'.
****
****      ENDIF.
      IF ( ls_report_au-desg_sociedadgl = 'NO' AND
       <ls_report>-conce <> ls_report_au-conce  AND
       lv_index > 1 ) OR
       ( lv_index = lv_tam_report AND ls_report_au-desg_sociedadgl = 'NO' ) .
        IF ls_report_au-desg_sociedadgl = 'NO' AND lv_cont_gl > 1.
          ls_report_au-rassc = 'GLs'.
        ENDIF.
        IF ls_report_au-desg_debeyhaber = 'SI'.
          IF lv_impordebe IS NOT INITIAL.
            ls_importes-importe = lv_impordebe.
            APPEND ls_importes TO lt_importes.
          ENDIF.
          IF lv_imporhaber IS NOT INITIAL.
            ls_importes-importe = lv_imporhaber.
            APPEND ls_importes TO lt_importes.
          ENDIF.
          LOOP AT lt_importes ASSIGNING <ls_importes>.
            ls_report_au-impor_new = <ls_importes>-importe.
            ASSIGN <ls_importes>-importe TO <ls_any>.
            ls_report_au-impor     = <ls_any>.
* Rellenar posición para el fichero de salida.
            ASSIGN  ls_report_au-impor_new TO <ls_any>.
            CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
              CHANGING
                value = ls_report_au-impor_new.
            ls_report_au-impor_new = ls_report_au-impor * 100.
            CONDENSE  ls_report_au-impor_new NO-GAPS.
*            IF ls_report_au-impor_new >= 0.
*              ls_report_au-conce = ls_report_au-conce_debe.
*            ELSE.
*              ls_report_au-conce = ls_report_au-conce_haber.
*            ENDIF.
            APPEND ls_report_au TO gt_report_fin.
            IF <ls_report>-desg_sociedadgl = 'NO' AND lv_concep > 1.
              CLEAR <ls_report>-rassc.
            ENDIF.
* Limiar texto Agrupación GL´s en fichero de salida
            IF ls_report_au-desg_sociedadgl = 'NO' AND lv_concep > 1 AND ls_report_au-rassc = 'GLs'.
              CLEAR ls_report_au-rassc.
            ENDIF.

            CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
              CHANGING
                value = ls_report_au-impor_new.

*            CONCATENATE ls_report_au-escen
*                        ls_report_au-perio
*                        ls_report_au-bukrs
*                        ls_report_au-conce
**                        lv_conce
**                        ls_report_au-rmvct
*                        ls_report_au-impor_new
*                        ls_report_au-rassc
*                        INTO ls_fichero-lines SEPARATED BY ';'.
*            APPEND ls_fichero TO gt_fichero.
          ENDLOOP.

          CLEAR lt_importes[].
          CLEAR lv_imporhaber.
          CLEAR lv_impordebe.
          CLEAR lv_impor.
          CLEAR ls_importes.
          CLEAR lv_concep.
          CLEAR ls_report_au.
          CLEAR lv_cont_gl.
          lv_cont_gl = lv_cont_gl + 1.
        ELSE.
          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              value = ls_report_au-impor_new.
          ls_report_au-impor_new = ls_report_au-impor * 100.
          CONDENSE  ls_report_au-impor_new NO-GAPS.
*          IF ls_report_au-impor_new >= 0.
*            ls_report_au-conce = ls_report_au-conce_debe.
*          ELSE.
*            ls_report_au-conce = ls_report_au-conce_haber.
*          ENDIF.
          APPEND ls_report_au TO gt_report_fin.
* Limiar texto Agrupación GL´s en fichero de salida
          IF ls_report_au-desg_sociedadgl = 'NO' AND lv_concep > 1 AND ls_report_au-rassc = 'GLs'.
            CLEAR ls_report_au-rassc.
          ENDIF.

          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              value = ls_report_au-impor_new.

* Rellenar posición para el fichero de salida.
*          CONCATENATE ls_report_au-escen
*                      ls_report_au-perio
*                      ls_report_au-bukrs
*                      ls_report_au-conce
**                      lv_conce
**                      ls_report_au-rmvct
*                      ls_report_au-impor_new
**                        lv_impor
*                      ls_report_au-rassc
*                      INTO ls_fichero-lines SEPARATED BY ';'.
*          APPEND ls_fichero TO gt_fichero.

        ENDIF.

        CLEAR lv_concep.
        CLEAR ls_report_au.
        CLEAR lv_impor.
        CLEAR lv_imporhaber.
        CLEAR lv_impordebe.
        CLEAR lv_concep.
        CLEAR lv_cuentas.
        ls_report_au-cuentas = lv_cuentas.
        CLEAR lt_racct[].
        lt_racct[] = lt_racct_au[].
        CLEAR lt_racct_au[].
        CLEAR lv_cont_gl.
        lv_cont_gl = lv_cont_gl + 1.
* Si la parametrización indica que tiene desglose sociedad GL y desglose para debe y haber, se
* calculan los importes por separados en debe y haber para un mismo concepto, pero se agrupa
* por sociedad GL distinta
      ELSEIF ( ls_report_au-desg_sociedadgl = 'SI' ).
* Comentado para pruebas por incidencia en XFP
*        AND
*            ( <ls_report>-desg_sociedadgl = ls_report_au-desg_sociedadgl ) OR
*        ( lv_index = lv_tam_report AND ls_report_au-desg_sociedadgl = 'SI'  AND <ls_report>-desg_sociedadgl = ls_report_au-desg_sociedadgl  ).

        IF  ls_report_au-desg_debeyhaber = 'NO'.
          IF    (   <ls_report>-rassc <> ls_report_au-rassc ) OR
              ( <ls_report>-conce <> ls_report_au-conce ).

            CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
              CHANGING
                value = ls_report_au-impor_new.
            ls_report_au-impor_new = ls_report_au-impor * 100.
            CONDENSE  ls_report_au-impor_new NO-GAPS.
*            IF ls_report_au-impor_new >= 0.
*              ls_report_au-conce = ls_report_au-conce_debe.
*            ELSE.
*              ls_report_au-conce = ls_report_au-conce_haber.
*            ENDIF.
            APPEND ls_report_au TO gt_report_fin.

            CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
              CHANGING
                value = ls_report_au-impor_new.

* Rellenar posición para el fichero de salida.
*            CONCATENATE ls_report_au-escen
*                        ls_report_au-perio
*                        ls_report_au-bukrs
*                        ls_report_au-conce
**                        lv_conce
**                        ls_report_au-rmvct
*                        ls_report_au-impor_new
*                        ls_report_au-rassc
*                        INTO ls_fichero-lines SEPARATED BY ';'.
*            APPEND ls_fichero TO gt_fichero.
            CLEAR lv_imporhaber.
            CLEAR lv_impordebe.
            CLEAR ls_report_au.
            CLEAR lv_impor.
            CLEAR lv_concep.
            CLEAR ls_report_au.
            CLEAR lt_importes[].
            CLEAR lt_racct[].
            ls_report_au-cuentas = lv_cuentas.
            CLEAR lv_cuentas.
            lt_racct[] = lt_racct_au[].
            CLEAR lt_racct_au[].
            CLEAR lv_cont_gl.
            lv_cont_gl = lv_cont_gl + 1.
          ENDIF.
        ELSEIF ( <ls_report>-desg_debeyhaber = 'SI' ) AND
               ( <ls_report>-conce = ls_report_au-conce AND
                 <ls_report>-rassc <> ls_report_au-rassc ) OR
                ( <ls_report>-conce <> ls_report_au-conce ).
* Debe/Haber
          IF lv_impordebe IS NOT INITIAL.
            ls_importes-importe = lv_impordebe .
            APPEND ls_importes TO lt_importes.
          ENDIF.
          IF lv_imporhaber IS NOT INITIAL.
            ls_importes-importe = lv_imporhaber.
            APPEND ls_importes TO lt_importes.
          ENDIF.
          IF lv_impordebe IS INITIAL AND lv_imporhaber IS INITIAL.
            ls_importes-importe = ls_report_au-impor_new.
            APPEND ls_importes TO lt_importes.
          ENDIF.
          LOOP AT lt_importes ASSIGNING <ls_importes>.
            ls_report_au-impor_new = <ls_importes>-importe.
            ASSIGN <ls_importes>-importe TO <ls_any>.
            ls_report_au-impor     = <ls_any>.
            IF ls_report_au-cuentas = 0 AND lv_cuentas IS NOT INITIAL.
              ls_report_au-cuentas = lv_cuentas.
            ENDIF.
*            IF ls_report_au-impor_new >= 0.
*              ls_report_au-conce = ls_report_au-conce_debe.
*            ELSE.
*              ls_report_au-conce = ls_report_au-conce_haber.
*            ENDIF.
            CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
              CHANGING
                value = ls_report_au-impor_new.
            ls_report_au-impor_new = ls_report_au-impor * 100.
            CONDENSE  ls_report_au-impor_new NO-GAPS.

            APPEND ls_report_au TO gt_report_fin.
* Limiar texto Agrupación GL´s en fichero de salida
            IF ls_report_au-desg_sociedadgl = 'NO' AND lv_concep > 1 AND ls_report_au-rassc = 'GLs'.
              CLEAR ls_report_au-rassc.
            ENDIF.

            CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
              CHANGING
                value = ls_report_au-impor_new.

* Rellenar posición para el fichero de salida.
*            CONCATENATE ls_report_au-escen
*                        ls_report_au-perio
*                        ls_report_au-bukrs
*                        ls_report_au-conce
**                        lv_conce
**                        ls_report_au-rmvct
*                        ls_report_au-impor_new
**                        lv_impor
*                        ls_report_au-rassc
*                        INTO ls_fichero-lines SEPARATED BY ';'.
*            APPEND ls_fichero TO gt_fichero.
          ENDLOOP.
          CLEAR lt_importes[].
          CLEAR lv_imporhaber.
          CLEAR lv_impordebe.
          CLEAR lv_impor.
          CLEAR ls_importes.
          CLEAR lv_concep.
          CLEAR ls_report_au.
          CLEAR lt_racct[].
          ls_report_au-cuentas = lv_cuentas.
          CLEAR lv_cuentas.
          lt_racct[] = lt_racct_au[].
          CLEAR lt_racct_au[].
          CLEAR lv_cont_gl.
          lv_cont_gl = lv_cont_gl + 1.
        ENDIF.
      ENDIF.
      CLEAR gt_report_fin_au[].
      ls_report_au-escen = <ls_report>-escen.
      ls_report_au-perio = <ls_report>-perio.
      ls_report_au-bukrs = <ls_report>-bukrs.
*      ls_report_au-rmvct = <ls_report>-rmvct.
      ls_report_au-racct = <ls_report>-racct.
      ls_report_au-rassc = <ls_report>-rassc.
      ls_report_au-impor_new = <ls_report>-impor_new.
      ls_report_au-conce     = <ls_report>-conce.
***      ls_report_au-conce_debe = <ls_report>-conce_debe.
***      ls_report_au-conce_haber = <ls_report>-conce_haber.
      ls_report_au-conce_debe = <ls_report>-conce_debe.
      ls_report_au-conce_haber = <ls_report>-conce_haber.
      ls_report_au-asignacion = <ls_report>-asignacion.
      ls_report_au-desg_sociedadgl = <ls_report>-desg_sociedadgl.
      ls_report_au-desg_debeyhaber = <ls_report>-desg_debeyhaber.

      IF ( <ls_report>-desg_debeyhaber = 'NO').
        ls_report_au-impor = ls_report_au-impor + <ls_report>-impor.
      ELSE.
* Si tiene paramatrizado desglose de importes por debe y haber en la tabla ZTFIGL_0003, los importes se calculan
* separados,dependiendo si son positivos(Haber) o negativos(Debe).
*        READ TABLE gt_report_fin_si WITH KEY conce_debe = <ls_report>-conce_debe rassc = <ls_report>-rassc impor = <ls_report>-impor ASSIGNING <ls_fin_si> BINARY SEARCH.
        READ TABLE gt_report_fin_si WITH KEY conce = <ls_report>-conce rassc = <ls_report>-rassc impor = <ls_report>-impor ASSIGNING <ls_fin_si> BINARY SEARCH.
        IF <ls_fin_si> IS ASSIGNED.
          IF <ls_fin_si>-impor < 0.
            lv_impordebe =  lv_impordebe + <ls_fin_si>-impor.
          ELSE.
            lv_imporhaber = lv_imporhaber + <ls_fin_si>-impor.
          ENDIF.

        ENDIF.

      ENDIF.

      AT LAST.
        APPEND ls_report_au TO gt_report_fin.
      ENDAT.

      CLEAR lv_tam.
      CLEAR ls_report.
      CLEAR ls_fichero.
      CLEAR ls_0005.
      CLEAR ls_0003.
      CLEAR ls_faglflext.
      CLEAR ls_t880.
      UNASSIGN <ls_racct>.
    ENDLOOP.

    lv_file = p_path.

    SORT gt_report_fin BY escen perio bukrs racct.

    CLEAR lv_impor_tot.

    LOOP AT gt_report_fin ASSIGNING <ls_report_fin>.

      lv_impor_tot = lv_impor_tot + <ls_report_fin>-impor.

      AT END OF racct.

        MOVE-CORRESPONDING <ls_report_fin> TO ls_report.

        ls_report-impor = lv_impor_tot.

        ls_report-impor_new = ls_report-impor * 100.
        CONDENSE  ls_report-impor_new NO-GAPS.

        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            value = ls_report-impor_new.

        IF ls_report-desg_debeyhaber EQ lc_no.
          ls_report-conce = ls_report-conce_debe.

        ELSEIF ls_report-desg_debeyhaber EQ lc_si.

          IF ls_report-impor GE 0.
            ls_report-conce = ls_report-conce_haber.
          ELSE.
            ls_report-conce = ls_report-conce_debe.
          ENDIF.
        ENDIF.

* Rellenar posición para el fichero de salida.
        CONCATENATE ls_report-escen
                    ls_report-perio
                    ls_report-bukrs
                    ls_report-conce
                    ls_report-impor_new
                    ls_report-rassc
               INTO ls_fichero-lines SEPARATED BY ';'.
        APPEND ls_fichero TO gt_fichero.

        APPEND ls_report TO gt_report_fin2.

        CLEAR: ls_fichero, lv_impor_tot, ls_report.
      ENDAT.

    ENDLOOP.

*Construcción del fichero
*    SORT gt_report_fin BY racct rassc conce_debe.
    SORT gt_report_fin2 BY racct rassc conce.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_file
        filetype                = 'ASC'
      TABLES
        data_tab                = gt_fichero
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc NE 0.
      CLEAR: gt_fichero[].
    ENDIF.
    CLEAR gt_faglflext[].

  ENDMETHOD.

  METHOD get_alv.

    o_get_data->field_cat( ).    "Data Catalog Building...
    o_get_data->field_sort( ).   "Sort Catalog Building...
    o_get_data->field_layout( ). "Layout Building...
    o_get_data->show_alv( ).     "ALV...

  ENDMETHOD.

  METHOD get_alv_error.

    o_get_data->field_cat( ).      "Data Catalog Building...
    o_get_data->field_sort( ).     "Sort Catalog Building...
    o_get_data->field_layout( ).   "Layout Building...
    o_get_data->show_alv_error( ). "ALV...

  ENDMETHOD.

  METHOD field_cat.

    DATA: lv_colpos      TYPE i,
          ls_fieldcat_ln LIKE LINE OF gt_fieldcat.

    CLEAR: lv_colpos, ls_fieldcat_ln, gt_fieldcat[].

* Escenario...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'ESCEN'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t01.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Periodo...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'PERIO'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t02.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Sociedad...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'BUKRS'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t03.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Cuenta...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'RACCT'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t08.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Concepto...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'CONCE'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t04.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Clase de movimiento...
*    lv_colpos = lv_colpos + 1.
*    ls_fieldcat_ln-fieldname = 'RMVCT'.   "Field from internal tabla
*    ls_fieldcat_ln-seltext_l = TEXT-t05.  "Column text
*    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
*    ls_fieldcat_ln-just      = 'C'.
*    APPEND ls_fieldcat_ln TO gt_fieldcat.
*    CLEAR: ls_fieldcat_ln.

* Importe...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'IMPOR'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t06.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'R'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Importe con formato sin comas...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'IMPOR_NEW'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t06.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'R'.
    ls_fieldcat_ln-no_out       = 'X'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Sociedad GL...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'RASSC'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t07.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Desglose Sociedad GL...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'DESG_SOCIEDADGL'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t10.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Asignación...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'ASIGNACION'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t11.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Indicador Debe/Haber...
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'DESG_DEBEYHABER'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t12.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

* Número de cuentas
    lv_colpos = lv_colpos + 1.
    ls_fieldcat_ln-fieldname = 'CUENTAS'.   "Field from internal tabla
    ls_fieldcat_ln-seltext_l = TEXT-t13.  "Column text
    ls_fieldcat_ln-col_pos   = lv_colpos. "Column position
    ls_fieldcat_ln-just      = 'C'.
    APPEND ls_fieldcat_ln TO gt_fieldcat.
    CLEAR: ls_fieldcat_ln.

  ENDMETHOD.

  METHOD field_sort.

    DATA: ls_sortcat_ln LIKE LINE OF gt_sortcat.

    CLEAR: ls_sortcat_ln, gt_sortcat[].
*
    ls_sortcat_ln-fieldname = 'ESCEN'.
    ls_sortcat_ln-up        = abap_true.
    APPEND ls_sortcat_ln TO gt_sortcat.


    ls_sortcat_ln-fieldname = 'CONCE'.
    ls_sortcat_ln-up        = abap_true.
    APPEND ls_sortcat_ln TO gt_sortcat.

*    ls_sortcat_ln-fieldname = 'RASSC'.
*    ls_sortcat_ln-up        = abap_true.
*    APPEND ls_sortcat_ln TO gt_sortcat.
*            ls_sortcat_ln-fieldname = 'IMPOR'.
*      ls_sortcat_ln-up        = abap_true.
*      APPEND ls_sortcat_ln TO gt_sortcat.
*            ls_sortcat_ln-fieldname = 'IMPOR_NEW'.
*      ls_sortcat_ln-up        = abap_true.
*      APPEND ls_sortcat_ln TO gt_sortcat.

  ENDMETHOD.

  METHOD field_layout.

    gs_layout-zebra             = abap_true.
    gs_layout-detail_popup      = abap_true.
    gs_layout-colwidth_optimize = abap_true.

  ENDMETHOD.

  METHOD show_alv.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = gs_layout
        it_fieldcat        = gt_fieldcat
        it_sort            = gt_sortcat
        i_save             = abap_true
      TABLES
        t_outtab           = gt_report_fin2
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1
                 sy-msgv2
                 sy-msgv3
                 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD show_alv_error.

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = gs_layout
        it_fieldcat        = gt_fieldcat[]
        i_default          = abap_true
        i_save             = abap_true
      TABLES
        t_outtab           = gt_faglflext[]
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
        TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1
                 sy-msgv2
                 sy-msgv3
                 sy-msgv4.
    ENDIF.

  ENDMETHOD.
* Autorización Sociedad
  METHOD autorizaciones.
* Autorización Sociedad

    AUTHORITY-CHECK OBJECT 'Z_SOCIEDAD'
     ID 'BUKRS' FIELD p_rbukrs.

    IF sy-subrc <> 0.
      MESSAGE ID 'ZCF' TYPE 'E' NUMBER 003.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
