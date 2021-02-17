*&---------------------------------------------------------------------*
*&  Include           ZFIGL_0009_TOP
*&---------------------------------------------------------------------*
TABLES: ztfigl_0001.

TYPE-POOLS: slis, rs, rsdrc.

TYPES: BEGIN OF s_resul_cubo1,
         sociedad TYPE bukrs,
         pais TYPE land,
         soc_gl_origen TYPE vbund,
*         moneda  TYPE /bi0/oicurrency,
*         plan_cuentas TYPE /bi0/oichrt_accts,
*         moneda_docu TYPE /bic/oizmondoc,
         cuenta TYPE saknr,
         valor_neto TYPE zoizvalnet,
         plan_cuentas TYPE zoichrt_accts,
*         sociedad_gl TYPE /bi0/oipcompany,
*         soc_gl_origen TYPE /bi0/oipcompany,
         conc_bpc TYPE zoizconcbpc,
         soc_gl_destino TYPE vbund,
         cl_mov TYPE zoimove_type,
         moneda  TYPE waers,
         tipo_moneda TYPE zoicurtype,
         moneda_docu TYPE zoizmondoc,
         valor_acumulado TYPE zoibalance,
*         valor_neto TYPE /bic/oizvalnet,
         total_credit TYPE zoicredit,
         total_debit TYPE zoidebit,
         asign TYPE c,
         conc_debe TYPE zoizconcbpc,
         conc_haber TYPE zoizconcbpc,
       END OF s_resul_cubo1.

TYPES: BEGIN OF s_resul_aux,
         sociedad TYPE bukrs,
         pais TYPE land,
*         moneda  TYPE /bi0/oicurrency,
*         plan_cuentas TYPE /bi0/oichrt_accts,
*         moneda_docu TYPE /bic/oizmondoc,
         cuenta TYPE saknr,
         plan_cuentas TYPE zoichrt_accts,
*         sociedad_gl TYPE /bi0/oipcompany,
         conc_bpc TYPE zoizconcbpc,
         soc_gl_destino TYPE vbund,
         soc_gl_origen TYPE vbund,
         cl_mov TYPE zoimove_type,
         moneda  TYPE waers,
         tipo_moneda TYPE zoicurtype,
         moneda_docu TYPE zoibalance,
         valor_acumulado TYPE zoibalance,
         valor_neto TYPE zoizvalnet,
         total_credit TYPE zoicredit,
         total_debit TYPE zoidebit,
         conc_debe TYPE zoizconcbpc,
         conc_haber TYPE zoizconcbpc,
       END OF s_resul_aux.

TYPES: BEGIN OF s_apartados,
         soc_gl TYPE vbund,
         apartado TYPE zoipobox,
       END OF s_apartados.

DATA:  i_resul_cubo1 TYPE STANDARD TABLE OF s_resul_cubo1,
       i_resul_cubo1_aux TYPE STANDARD TABLE OF s_resul_cubo1,
       gt_data TYPE STANDARD TABLE OF s_resul_cubo1,
       gt_data_aux TYPE STANDARD TABLE OF s_resul_cubo1,
       gt_data_aux2 TYPE STANDARD TABLE OF s_resul_cubo1,
       v_first_call1 TYPE rs_bool,
       wa_resul_cubo1 TYPE s_resul_cubo1,
       i_sfc1 TYPE rsdri_th_sfc,
       i_sfk1 TYPE rsdri_th_sfk,
       i_range1 TYPE rsdri_t_range,
       i_range TYPE rsdri_t_range,
       wa_sfc1 TYPE LINE OF rsdri_th_sfc,
       wa_sfk1 TYPE LINE OF rsdri_th_sfk,
       wa_range1 TYPE LINE OF rsdri_t_range,
       wa_range TYPE LINE OF rsdri_t_range,
       i_ztfigl_0001 TYPE STANDARD TABLE OF ztfigl_0001,
       wa_ztfigl_0001 TYPE ztfigl_0001,
       v_periodo TYPE /bi0/oifiscper,
       i_ztfigl_0004 TYPE STANDARD TABLE OF ztfigl_0004,
       vindex LIKE sy-tabix,
       v_concepto TYPE zoizconcbpc,
       i_ztfigl_0002 TYPE STANDARD TABLE OF ztfigl_0002,
       wa_ztfigl_0002 TYPE ztfigl_0002,
       v_asig_encontrada,
       v_siguiente,
*       i_apartados TYPE STANDARD TABLE OF s_apartados,
*       wa_apartados TYPE s_apartados,
       i_apartados TYPE STANDARD TABLE OF zpcompany,
       wa_apartados TYPE zpcompany,
       wa_ztfigl_0004 TYPE ztfigl_0004,
       it_fieldcat TYPE slis_t_fieldcat_alv,
       wa_fieldcat TYPE slis_fieldcat_alv,
       wa_layout TYPE slis_layout_alv.
*       wa_range TYPE LINE OF rsdri_t_range.
*       wa_errores type /BIC/AZGL_O0200.

CONSTANTS: gc_noasig1 TYPE zoizconcbpc VALUE 'NO_ASSIG'.


TYPES: BEGIN OF s_resul_572,
         cuenta TYPE saknr,
         conc_bpc TYPE zoizconcbpc,
         soc_gl TYPE zoipcompany,
         valor_acumulado TYPE zoibalance,
         valor_neto TYPE zoizvalnet,
       END OF s_resul_572.

*TYPES: BEGIN OF s_resul_aux,
*         cuenta TYPE /bi0/oigl_account,
*         valor_acumulado TYPE /bi0/oibalance,
**         valor_neto TYPE /bic/oizvalnet,
*         soc_gl_origen TYPE /bi0/oipcompany,
**         conc_bpc TYPE /bic/oizconcbpc,
*         soc_gl_destino TYPE /bi0/oipcompany,
**         moneda  TYPE /bi0/oicurrency,
**         moneda_docu TYPE /bic/oizmondoc,
**         plan_cuentas TYPE /bi0/oichrt_accts,
**         sociedad TYPE /bi0/oicomp_code,
*       END OF s_resul_aux.

** <INICIO> Modif. -  Ticket: & - Usuario: HPI069 - Fecha: 22.11.2017

    TYPES: BEGIN OF s_comp_abs,
             company TYPE zoicompany,
             zsoc_abs TYPE zoizsoc_abs,
           END OF s_comp_abs.

DATA: it_comp_abs TYPE TABLE OF s_comp_abs,
      wa_comp_abs TYPE s_comp_abs.

** <FIN> Modif.-  Ticket: & - Usuario: HPI069 - Fecha: 22.11.2017

DATA:  wa_resul_572 TYPE s_resul_572,
       i_resul_572 TYPE STANDARD TABLE OF s_resul_572.

DATA:  wa_resul_aux TYPE s_resul_aux,
       i_resul_aux TYPE STANDARD TABLE OF s_resul_aux.
DATA:  wa_resul_aux2 TYPE s_resul_aux,
       i_resul_aux2 TYPE STANDARD TABLE OF s_resul_aux.
*DATA:  wa_resul_aux type s_resul_cubo1,
*       i_resul_aux TYPE STANDARD TABLE OF s_resul_cubo1.

DATA: v_tipo_mon TYPE zoicurtype.


  DATA: gv_end_of_data  TYPE rs_bool,
        gv_first_call   TYPE rs_bool.



     TYPES: BEGIN OF s_COMP_CODE,
             company TYPE zoicompany,
           END OF s_COMP_CODE.

DATA : it_comp_code TYPE TABLE OF s_comp_code,
       wa_comp_code TYPE s_comp_code.


TYPES : BEGIN OF st_hier,
     nodeid      TYPE rshienodid,
     nodename    TYPE rsshnodenamestr,
     parentid    TYPE rsparent,
  END OF st_hier.

    TYPES: BEGIN OF st_account,
             zgsoaccnt TYPE zoizgsoaccnt,
           END OF st_account.

  DATA : v_hierID TYPE rshieid,
      it_hierarchy TYPE  TABLE OF st_hier,
      it_hierarchy_padre TYPE  TABLE OF st_hier,
      wa_hierarchy TYPE st_hier,
      wa_hierarchy_padre TYPE st_hier,
      wa_account TYPE st_account,
      it_account TYPE TABLE OF st_account.
