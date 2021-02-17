*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTFIGL_0003
*   generation date: 11.02.2021 at 03:55:59
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTFIGL_0003        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
