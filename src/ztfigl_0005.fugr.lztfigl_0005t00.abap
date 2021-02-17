*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.02.2021 at 13:46:33
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFIGL_0005.....................................*
DATA:  BEGIN OF STATUS_ZTFIGL_0005                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFIGL_0005                   .
CONTROLS: TCTRL_ZTFIGL_0005
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTFIGL_0005                   .
TABLES: ZTFIGL_0005                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
