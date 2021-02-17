*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12.02.2021 at 05:01:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZZTBW_0114......................................*
DATA:  BEGIN OF STATUS_ZZTBW_0114                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZZTBW_0114                    .
CONTROLS: TCTRL_ZZTBW_0114
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZZTBW_0114                    .
TABLES: ZZTBW_0114                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
