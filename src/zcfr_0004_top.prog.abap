*&---------------------------------------------------------------------*
*& Include          ZCFR_0004_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_faglflext,
         ryear  TYPE gjahr,
         rpmax  TYPE rpmax,
         rbukrs TYPE bukrs,
         rmvct  TYPE rmvct,
         hslvt  TYPE tslvt12,
         hsl01  TYPE hslxx12,
         hsl02  TYPE hslxx12,
         hsl03  TYPE hslxx12,
         hsl04  TYPE hslxx12,
         hsl05  TYPE hslxx12,
         hsl06  TYPE hslxx12,
         hsl07  TYPE hslxx12,
         hsl08  TYPE hslxx12,
         hsl09  TYPE hslxx12,
         hsl10  TYPE hslxx12,
         hsl11  TYPE hslxx12,
         hsl12  TYPE hslxx12,
         hsl13  TYPE hslxx12,
         hsl14  TYPE hslxx12,
         hsl15  TYPE hslxx12,
         hsl16  TYPE hslxx12,
         rassc  TYPE rassc,
         racct  TYPE racct,
       END OF ty_faglflext.

TYPES: BEGIN OF ty_report,
         desg_sociedadgl TYPE zgl_desglose,
         racct           TYPE racct,
         conce           TYPE char11,
         conce_haber     TYPE char11,
         conce_debe      TYPE char11,
         rassc           TYPE rassc,
         escen           TYPE char4,
         perio           TYPE char4,
         bukrs           TYPE bukrs,
         impor_new       TYPE char30,
         impor           TYPE p LENGTH 16 DECIMALS 2,
         asignacion      TYPE zgl_asignacion,
         desg_debeyhaber TYPE zgl_desglosedh,
         cuentas         TYPE string,
       END OF ty_report.

TYPES: BEGIN OF ty_report2,
         escen           TYPE char4,
         perio           TYPE char4,
         bukrs           TYPE bukrs,
         racct           TYPE racct,
         desg_sociedadgl TYPE zgl_desglose,
         conce           TYPE char11,
         conce_haber     TYPE char11,
         conce_debe      TYPE char11,
         rassc           TYPE rassc,
         impor_new       TYPE char30,
         impor           TYPE p LENGTH 16 DECIMALS 2,
         asignacion      TYPE zgl_asignacion,
         desg_debeyhaber TYPE zgl_desglosedh,
         cuentas         TYPE string,
       END OF ty_report2.

TYPES: BEGIN OF ty_fichero,
         lines TYPE char100,
       END OF ty_fichero.

TYPES: BEGIN OF ty_t880,
         rcomp TYPE rcomp_d,
         pobox TYPE postfach,
       END OF ty_t880.

DATA: gt_faglflext     TYPE TABLE OF ty_faglflext,
      gt_t880          TYPE TABLE OF ty_t880,
** <INICIO> Modif. -REQ-72691- Usuario: STR540 - Fecha: 28.05.2020
*      gt_0003        TYPE TABLE OF ztfigl_0003,
      gt_0003          TYPE TABLE OF ztfigl_0006,
** <FIN> Modif. -REQ-72691- Usuario: STR540 - Fecha: 28.05.2020
      gt_0005          TYPE TABLE OF ztfigl_0005,
      gt_soc           TYPE TABLE OF ztfigl_soc,
      gt_report        TYPE TABLE OF ty_report,
      gt_report_au     TYPE TABLE OF ty_report,
      gt_report_fin    TYPE TABLE OF ty_report2,
      gt_report_fin2   TYPE TABLE OF ty_report,
      gt_report_fin_si TYPE TABLE OF ty_report,
      gt_report_fin_au TYPE TABLE OF ty_report,
      gt_fichero       TYPE TABLE OF ty_fichero,
      gt_fieldcat      TYPE slis_t_fieldcat_alv,
      gt_sortcat       TYPE slis_t_sortinfo_alv,
      gs_layout        TYPE slis_layout_alv,
      gv_lenght        TYPE i.

CONSTANTS: gc_0101 TYPE char4 VALUE '0101',
           gc_0102 TYPE char4 VALUE '0102'.
