*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTFIGL_0007
*   generation date: 12.02.2021 at 15:13:36
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTFIGL_0007        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
