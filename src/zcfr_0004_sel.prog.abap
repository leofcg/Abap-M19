*&---------------------------------------------------------------------*
*& Include          ZCFR_0004_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_rbukrs   TYPE bukrs OBLIGATORY,
            p_perio(6) TYPE n,
            p_fisc     RADIOBUTTON GROUP g1,
            p_fisce    RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN: BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_path  LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b02.
