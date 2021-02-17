*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12.02.2021 at 04:56:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTBW_0113.......................................*
DATA:  BEGIN OF STATUS_ZTBW_0113                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTBW_0113                     .
CONTROLS: TCTRL_ZTBW_0113
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTBW_0113                     .
TABLES: ZTBW_0113                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
