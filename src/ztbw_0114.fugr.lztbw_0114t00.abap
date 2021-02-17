*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12.02.2021 at 05:03:46
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTBW_0114.......................................*
DATA:  BEGIN OF STATUS_ZTBW_0114                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTBW_0114                     .
CONTROLS: TCTRL_ZTBW_0114
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTBW_0114                     .
TABLES: ZTBW_0114                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
