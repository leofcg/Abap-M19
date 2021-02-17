*&---------------------------------------------------------------------*
*& Report ZCFR_0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE zcfr_0001_top.
INCLUDE zcfr_0001_sel.
INCLUDE zcfr_0001_cls.

* 1. Seleccionamos el fichero de destino...
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = sy-cprog
      dynpro_number = sy-dynnr
    IMPORTING
      file_name     = p_path.

AT SELECTION-SCREEN.
  gv_lenght = strlen( p_perio ).
  IF p_perio+(1) NE '0'.
    IF p_perio+4(2) GT 16.
      MESSAGE e000(oo) WITH TEXT-e01.
    ELSEIF p_perio+4(2) EQ 0.
      MESSAGE e000(oo) WITH TEXT-e01.
    ELSEIF gv_lenght NE 6.
      MESSAGE e000(oo) WITH TEXT-e01.
    ENDIF.
  ELSE.
    MESSAGE e000(oo) WITH TEXT-e01.
  ENDIF.

CLEAR: gt_report[], gt_report_au[], gt_report_fin_si[],gt_faglflext[] .

*START-OF-SELECTION.
  locl_get_data=>o_get_data->autorizaciones( ).
* 2. Lee la información de la tabla estándar...
  locl_get_data=>o_get_data->read_data( ).

  IF gt_faglflext[] IS NOT INITIAL.
* Graba contenido en el fichero a generar...
    locl_get_data=>o_get_data->create_file( ).
* Muestra logs...
    locl_get_data=>o_get_data->get_alv( ).
  ENDIF.
