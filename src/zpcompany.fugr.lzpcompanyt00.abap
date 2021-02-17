*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12.02.2021 at 04:26:05
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPCOMPANY.......................................*
DATA:  BEGIN OF STATUS_ZPCOMPANY                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPCOMPANY                     .
CONTROLS: TCTRL_ZPCOMPANY
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPCOMPANY                     .
TABLES: ZPCOMPANY                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
