*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12.02.2021 at 15:13:37
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFIGL_0007.....................................*
DATA:  BEGIN OF STATUS_ZTFIGL_0007                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFIGL_0007                   .
CONTROLS: TCTRL_ZTFIGL_0007
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZTFIGL_0007                   .
TABLES: ZTFIGL_0007                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
