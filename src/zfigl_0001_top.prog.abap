*&---------------------------------------------------------------------*
*&  Include          ZFIGL_0001_TOP
*&---------------------------------------------------------------------*
*************************
* Declaración de datos*
*************************

TYPES: BEGIN OF s_salida,
         escenario(4),
         ano_mes(4),
         sociedad(4),
         cuenta(11),
         clase_mov(3),
         importe(21),
         soc_gl(6),
       END OF s_salida.

TYPES: BEGIN OF s_resul_cubo,
         moneda TYPE zbioicurrency,
         sociedad TYPE zoicomp_code,
         cuenta TYPE zoizconcbpc,
         sociedad_gl TYPE zoipcompany,
         cl_mov TYPE zoimove_type,
*         importe type /SDF/DEC30_3,
         importe TYPE zoizvalnet,
       END OF s_resul_cubo.

TYPES: BEGIN OF s_salida_txt,
         linea TYPE string,
       END OF s_salida_txt.

TYPES: BEGIN OF s_decimales,
         moneda TYPE /bi0/oicurrency,
         decimales TYPE i,
       END OF s_decimales.

DATA: wa_salida TYPE s_salida,
      i_salida TYPE STANDARD TABLE OF s_salida,
      wa_resul_cubo TYPE s_resul_cubo,
      i_resul_cubo TYPE STANDARD TABLE OF s_resul_cubo,
      wa_sfc TYPE LINE OF rsdri_th_sfc,
      i_sfc TYPE rsdri_th_sfc,
      wa_sfk TYPE LINE OF rsdri_th_sfk,
      i_sfk TYPE rsdri_th_sfk,
      wa_range TYPE LINE OF rsdri_t_range,
      i_range TYPE rsdri_t_range,
      wa_salida_txt TYPE s_salida_txt,
      i_salida_txt TYPE STANDARD TABLE OF s_salida_txt,
      wa_decimales TYPE s_decimales,
      i_decimales TYPE STANDARD TABLE OF s_decimales,
      v_first_call TYPE rs_bool.

DATA: v_perio_low TYPE zoifiscper,
      v_tipo_mon TYPE zoicurtype,
      v_str_file TYPE string,
      v_user_action TYPE i.
DATA l_answer(1)      TYPE c. "MOD MLT

*************************
* Pantalla de seleccion *
*************************
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS so_bukrs FOR wa_resul_cubo-sociedad.
PARAMETERS: p_perio TYPE /bi0/oifiscper OBLIGATORY.
***Mod inicio Taborda Maria Laura 15/5/2012
PARAMETERS cbx_baja AS CHECKBOX .
***Mod Fin Taborda Maria Laura 15/5/2012
SELECTION-SCREEN SKIP 1.

PARAMETERS: p_monloc RADIOBUTTON GROUP g01 DEFAULT 'X',
            p_monfte RADIOBUTTON GROUP g01.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b02.
