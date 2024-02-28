*&---------------------------------------------------------------------*
*& Include          ZIN_ADJUST_BUDGET_TOP
*&---------------------------------------------------------------------*
REPORT zpg_adjust_budget.
TABLES :fmrpf_sel_screen,sscrfields,usr21.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
PARAMETERS :p_fma  TYPE fikrs DEFAULT 1000 MODIF ID obl,
            p_year TYPE gjahr DEFAULT sy-datum(4) MODIF ID obl,
            p_date TYPE dats DEFAULT sy-datum MODIF ID obl,
            p_fund TYPE fmrpf_sel_screen-fonds DEFAULT 'VTP' MODIF ID obl,
            p_per  TYPE char2 MODIF ID obl.
SELECT-OPTIONS :
*                s_fund FOR fmrpf_sel_screen-fonds,
*                s_bud FOR fmrpf_sel_screen-budget_pd,
                s_parfc FOR fmrpf_sel_screen-fistl MATCHCODE OBJECT zsh_par_fc,
                s_fundc FOR fmrpf_sel_screen-fistl,
                s_ci FOR fmrpf_sel_screen-fipex,
                s_prg FOR fmrpf_sel_screen-measure.
SELECTION-SCREEN END OF BLOCK a1.
SELECTION-SCREEN FUNCTION KEY 1.
TYPES :BEGIN OF ty_data_amt,
         fc_par TYPE fistl,
         ci_par TYPE zdd_ci,
         func   TYPE fistl,
         ci     TYPE zdd_ci,
         amt    TYPE hslxx9_cs,
       END OF ty_data_amt.

TYPES :BEGIN OF ty_fc_ci_amt,
         fc  TYPE fistl,
         ci  TYPE zdd_ci,
         amt TYPE hslxx9_cs,
       END OF ty_fc_ci_amt.

TYPES :BEGIN OF ty_data_collect,
         fc_par       TYPE fistl,
         ci_par       TYPE zdd_ci,
         amt_root     TYPE hslxx9_cs,
         percent(10)  TYPE p DECIMALS 2,
         amt_trans    TYPE hslxx9_cs,
         child        TYPE STANDARD TABLE OF ty_fc_ci_amt WITH EMPTY KEY,
         amt_child    TYPE hslxx9_cs,
         fc_rec       TYPE fistl,
         ci_rec       TYPE zdd_ci,
         amt_rec      TYPE hslxx9_cs,
         amt_rec_retn TYPE hslxx9_cs,

       END OF ty_data_collect.

TYPES :BEGIN OF ty_data_tree,
         fc            TYPE fistl,
         ci            TYPE zdd_ci,
         amt           TYPE hslxx9_cs,
         percent(10)   TYPE p DECIMALS 2,
         amt_trans     TYPE hslxx9_cs,
         amt_child_tot TYPE hslxx9_cs,
         fc_rec        TYPE fistl,
         ci_rec        TYPE zdd_ci,
         amt_rec_retn  TYPE hslxx9_cs,
         amt_rec       TYPE hslxx9_cs,
       END OF ty_data_tree.

TYPES :BEGIN OF ty_mess_log,
         status     TYPE char4,
         id         TYPE symsgid,
         number     TYPE symsgno,
         message    TYPE bapi_msg,
         message_v1 TYPE symsgv,
         message_v2 TYPE symsgv,
         message_v3 TYPE symsgv,
         message_v4 TYPE symsgv,
       END OF ty_mess_log.
DATA :gt_fcat_log TYPE slis_t_fieldcat_alv,
      gt_mess_log TYPE TABLE OF ty_mess_log.
DATA :gt_data_collect TYPE TABLE OF ty_data_collect,
      gt_data_tree    TYPE TABLE OF ty_data_tree.

DATA :go_tree_alv  TYPE REF TO cl_gui_alv_tree,
      go_cont_tree TYPE REF TO cl_gui_custom_container,
      go_cont_hdr  TYPE REF TO cl_gui_container,
      go_cont_main TYPE REF TO cl_gui_container,
      go_doc_hdr   TYPE REF TO cl_dd_document,
      gt_fc_tree   TYPE lvc_t_fcat.
DATA :gw_stt      TYPE string,
      gw_bud_per  TYPE fm_budget_period,
      gw_doc_retn TYPE bued_docnr,
      gw_doc_entr TYPE bued_docnr.
DATA : gt_log_zfm03_h TYPE TABLE OF ztb_log_zfm03_h,
       gt_log_zfm03_i TYPE TABLE OF ztb_log_zfm03_i.
SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW TITLE TEXT-002.
SELECT-OPTIONS : s_user FOR usr21-bname,
                 s_date FOR sy-datum,
                 s_time FOR sy-uzeit.
SELECTION-SCREEN END OF SCREEN 1001.
