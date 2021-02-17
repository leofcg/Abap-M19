*&---------------------------------------------------------------------*
*&  Include           ZBW_BPC_TOP
*&---------------------------------------------------------------------*
*INCLUDE <icon>.

TYPE-POOLS: slis.

TABLES: sscrfields.

DATA: gv_tabla   TYPE tabname.
DATA: gs_table   TYPE REF TO data.

*FIELD-SYMBOLS  <gt_result> TYPE STANDARD TABLE.

TYPES:
**    "Estructuras
    BEGIN OF tys_0006,
      periodo     TYPE /BI0/OIFISCPER,   "Fiscal year / period
      fecha_desde TYPE /BI0/OICALDAY,    "Calendar Day
      fecha_hasta TYPE /BI0/OICALDAY,    "Calendar Day
   END OF tys_0006,

**    "Tablas Internas
    tyt_0006 TYPE STANDARD TABLE OF tys_0006.

DATA: gt_0006  TYPE tyt_0006.

*DATA: ls_layout TYPE slis_layout_alv.

DATA: desc TYPE  ZTCOMPANY-txtmd.

DATA: wa_fieldcat TYPE slis_fieldcat_alv,
      it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_layout TYPE slis_layout_alv.
