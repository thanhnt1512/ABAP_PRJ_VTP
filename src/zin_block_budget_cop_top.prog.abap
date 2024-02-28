*&---------------------------------------------------------------------*
*& Include          ZIN_BLOCK_BUDGET_TOP
*&---------------------------------------------------------------------*
REPORT zpg_block_budget.
TABLES :zfmavct,fmavct.
TABLES :sscrfields,fmrpf_sel_screen,fmrbcs,usr21.
TYPES :BEGIN OF ty_data,
         rldnr             TYPE fmavct-rldnr,
         rfundsctr         TYPE fmavct-rfundsctr,
         rmeasure          TYPE fmavct-rmeasure,
         rcmmtitem         TYPE fmavct-rcmmtitem,
         fc_name           TYPE fm_bezeich,
         ci_name           TYPE fm_bezeich,
         fm_name           TYPE fm_bezeich,
         consumable_budget TYPE hslvt9_cs,
         consumed_amount   TYPE hslvt9_cs,
         available_amount  TYPE hslvt9_cs,
         blocked_amount    TYPE hslvt9_cs,
         waers             TYPE waers,
       END OF ty_data.
TYPES : BEGIN OF ty_his,
          nguoi_tao TYPE char50,
          ngay_tao  TYPE dats,
          tgian_tao TYPE tims,
          type      TYPE char15,
          status    TYPE char1,
          fm_area   TYPE fikrs,
          fc_year   TYPE gjahr,
          doc_num   TYPE kblnr,
          error     TYPE char255,
          variant   TYPE char255,
        END OF ty_his.
DATA :gt_data     TYPE TABLE OF ty_data,
      gt_fieldcat TYPE lvc_t_fcat,
      g_alv       TYPE REF TO cl_gui_alv_grid,
      g_container TYPE REF TO cl_gui_custom_container.

DATA  :g_error    TYPE REF TO cx_root,
       gt_history TYPE TABLE OF  ty_his.
DATA :valutab  TYPE TABLE OF rsparams,
      valutabl TYPE TABLE OF rsparamsl.
PARAMETERS :p_area   TYPE fmrpf_sel_screen-fikrs MODIF ID ob,
            p_year   TYPE gjahr MODIF ID ob,
            p_budat  TYPE dats MODIF ID ob,
            p_period TYPE char2 MODIF ID ob,
*            p_budcat TYPE fmrbcs-budcat MODIF ID ob.
            p_budcat TYPE rldnr MODIF ID ob.
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_bsline FOR zfmavct-zfm_busline,
                s_supfc FOR fmrpf_sel_screen-fistl,
                s_fun_c FOR fmrpf_sel_screen-fistl,
                s_c_itm FOR fmrpf_sel_screen-fipex,
                s_fun_p FOR fmrpf_sel_screen-measure.
SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN FUNCTION KEY 1.

PARAMETERS :rd_bl RADIOBUTTON GROUP rdb DEFAULT 'X',
            rd_rt RADIOBUTTON GROUP rdb.

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW TITLE TEXT-002.
SELECT-OPTIONS : s_user FOR usr21-bname.
SELECT-OPTIONS : s_date FOR sy-datum.
SELECTION-SCREEN END OF SCREEN 1001.
