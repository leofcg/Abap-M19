*&---------------------------------------------------------------------*
*&  Include           ZBW_BPC_S01
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_0002 RADIOBUTTON GROUP rb1 default 'X'.
SELECTION-SCREEN COMMENT 5(70) com4 FOR FIELD rb_0002.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_0003 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 5(70) com5 FOR FIELD rb_0003.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_0006 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 5(70) com6 FOR FIELD rb_0006.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_0008 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 5(70) com8 FOR FIELD rb_0008.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK b01.
