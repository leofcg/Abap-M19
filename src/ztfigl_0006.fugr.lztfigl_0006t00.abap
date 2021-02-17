*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.02.2021 at 13:38:22
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFIGL_0006.....................................*
DATA:  BEGIN OF STATUS_ZTFIGL_0006                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFIGL_0006                   .
CONTROLS: TCTRL_ZTFIGL_0006
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTFIGL_0006                   .
TABLES: ZTFIGL_0006                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
