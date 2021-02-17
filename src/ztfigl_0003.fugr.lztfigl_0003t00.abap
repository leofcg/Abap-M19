*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.02.2021 at 03:56:00
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFIGL_0003.....................................*
DATA:  BEGIN OF STATUS_ZTFIGL_0003                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFIGL_0003                   .
CONTROLS: TCTRL_ZTFIGL_0003
            TYPE TABLEVIEW USING SCREEN '0004'.
*.........table declarations:.................................*
TABLES: *ZTFIGL_0003                   .
TABLES: ZTFIGL_0003                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
