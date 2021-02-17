*&---------------------------------------------------------------------*
*& Include          ZCFR_0001_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_rbukrs   TYPE bukrs,
            p_perio(6) TYPE n.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN: BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_path  LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b02.
