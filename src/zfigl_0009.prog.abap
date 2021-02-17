************************************************************************
*& PROGRAMA ID: ZFIGL_0009                                            &*
*& TITLE      :                                                       &*
*& CREATE DATE: 19.05.2014                                            &*
*& AUTHOR USER: IND679                                                &*
*& AUTHOR NAME: IVP                                                   &*
*& OT         :                                                       &*
*& TKT/RICEF  :                                                       &*
*&--------------------------------------------------------------------&*
*& DESCRIPTION:  Realización de un informe que vincule/indique        &*
*&  los saldos a nivel de cuenta pcc10 (informe C01) y el resultado   &*
*&  convertido a conceptos bpc (informe C02)                          &*
*&                                                                    &*
*&                                                                    &*
*&                                                                    &*
************************************************************************
*& CHANGE HISTORY                                                     &*
************************************************************************
*& CREATE DATE:                                                       &*
*& AUTHOR NAME:                                                       &*
*& AUTHOR USER:                                                       &*
*& OT         :                                                       &*
*& TICKET     :                                                       &*
*& DESCRIPTION:                                                       &*
*&--------------------------------------------------------------------&*
REPORT  ZFIGL_0009.
INCLUDE zfigl_0009_top.    " Declaraciones Globales
INCLUDE zfigl_0009_s01.    " Selection Screen & SubScreen
INCLUDE zfigl_0009_f01.    " Subrutinas
*INCLUDE zfigl_0009_bi.     " Batch Input
*INCLUDE zfigl_0009_alv.    " ALV
*INCLUDE zfigl_0009_pbo.    " Screen 0100 - PBO
*INCLUDE zfigl_0009_pai.    " Screen 0100 - PAI


*----------------------------------------------------------------------*
*  INITIALIZATION                                                      *
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
*  START-OF-SELECTION                                                  *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Tratamiento de datos.
  PERFORM f_tratamiento_asignacion_bpc.
  PERFORM cargo_fieldcat.
  PERFORM seteo_layout.
  PERFORM display_alv.

*----------------------------------------------------------------------*
*  END-OF-SELECTION                                                    *
*----------------------------------------------------------------------*
END-OF-SELECTION.
