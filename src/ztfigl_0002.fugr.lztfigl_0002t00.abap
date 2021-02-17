*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 11.02.2021 at 04:02:31
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFIGL_0002.....................................*
DATA:  BEGIN OF STATUS_ZTFIGL_0002                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFIGL_0002                   .
CONTROLS: TCTRL_ZTFIGL_0002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTFIGL_0002                   .
TABLES: ZTFIGL_0002                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
