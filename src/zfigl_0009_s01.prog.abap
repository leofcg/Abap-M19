*&---------------------------------------------------------------------*
*&  Include           ZFIGL_0009_S01
*&---------------------------------------------------------------------*
*************************
* Pantalla de seleccion *
*************************
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-s01.

SELECT-OPTIONS: so_bukrs FOR ztfigl_0001-sociedad.

PARAMETERS: p_perio TYPE ztfigl_0001-periodo OBLIGATORY.

parameters: p_monloc RADIOBUTTON GROUP g01 DEFAULT 'X',
            p_monfte RADIOBUTTON GROUP g01.

SELECTION-SCREEN END OF BLOCK b01.
