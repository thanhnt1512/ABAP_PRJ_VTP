*&---------------------------------------------------------------------*
*& Include          ZPG_CREATE_XDCBDD_TOP
*&---------------------------------------------------------------------*
REPORT zpg_create_xdcbdd.
TABLES :sscrfields,anla,anlz,usr21.

TYPES :BEGIN OF ty_data,
         stt_rule    TYPE char4,
         stt_settle  TYPE char4,
         bukrs       TYPE bukrs,
         anln1       TYPE anla-anln1,
         anlkl       TYPE anla-anlkl,
         txt50       TYPE anla-txt50,
         as_amt      TYPE fins_vhcur12,
         as_rule_gr  TYPE brsgr,
         as_set_per  TYPE swfp7_2dec,
         as_amt_rule TYPE fins_vhcur12,
         as_set_amt  TYPE fins_vhcur12,
         fi_doc      TYPE belnr_d,
         waers       TYPE waers,
       END OF ty_data.

TYPES :BEGIN OF ty_history,
         date_entr   TYPE ztb_auc_log-date_entr,
         time_entr   TYPE  ztb_auc_log-time_entr,
         user_cre    TYPE ztb_auc_log-user_cre,
         comp        TYPE ztb_auc_log-comp,
         psting_date TYPE budat,
         asset_auc   TYPE ztb_auc_log-asset_auc,
         bureg       TYPE ztb_auc_log-bureg,
         fi_doc      TYPE ztb_auc_log-fi_doc,
         asset_comp  TYPE ztb_auc_log-asset_comp,
       END OF ty_history.
DATA :gt_history TYPE TABLE OF ty_history.

DATA : gt_data       TYPE TABLE OF ty_data,
       gt_fcat       TYPE lvc_t_fcat,
       g_alv         TYPE REF TO cl_gui_alv_grid,
       g_cont_header TYPE REF TO cl_gui_container,
       g_doc_header  TYPE REF TO cl_dd_document,
       g_ass_n       TYPE anla-anln1,
       g_stt         TYPE char1,
       g_cont        TYPE REF TO cl_gui_custom_container.

DATA :gs_ass_get TYPE anla,
      gw_ass_crd TYPE bf_anln1,
      gw_tt_as   TYPE answl.


DATA : gw_aufnr TYPE am_aufnr.

TYPES :tt_his TYPE TABLE OF ztb_auc_log.
TYPES:
  BEGIN OF gty_s_postab.                                                 "Begin of S2I
    INCLUDE TYPE aimtv.
TYPES:objnr         TYPE anla-objnr,              " merker co-objekt
      zaehl         TYPE sy-tabix,    " indexzaehler
      workf(1)      TYPE c, " value ' ', " workflag = x wenn bureg bearb.
      ampel(1)      TYPE c, "value '0', " alv ampel    "4.6c
      xextend_afabe TYPE xfeld,
      END OF gty_s_postab .
TYPES:
*    TYPES:
*      gty_t_postab TYPE STANDARD TABLE OF gty_s_postab .
  BEGIN OF gty_s_ass_post.
TYPES :asset  TYPE anla-anln1,
       postab TYPE STANDARD TABLE OF gty_s_postab WITH DEFAULT KEY,

       END OF gty_s_ass_post .
TYPES:
  gty_t_ass_post TYPE TABLE OF gty_s_ass_post .
TYPES:
  BEGIN OF gty_s_ass_n,
    anln1 TYPE anln1,
  END OF  gty_s_ass_n .
TYPES:
  gty_t_ass_n TYPE TABLE OF gty_s_ass_n .



SELECTION-SCREEN :FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW TITLE TEXT-002.
  SELECT-OPTIONS : s_user FOR usr21-bname NO INTERVALS.
  SELECT-OPTIONS : s_date FOR sy-datum.
SELECTION-SCREEN END OF SCREEN 1001.

SELECTION-SCREEN :BEGIN OF TABBED BLOCK bl1 FOR 20 LINES.
SELECTION-SCREEN :TAB (20) tab1 USER-COMMAND tab1 DEFAULT SCREEN 100,
                  TAB (20) tab2 USER-COMMAND tab2 DEFAULT SCREEN 101,
                  TAB (20) tab3 USER-COMMAND tab3 DEFAULT SCREEN 102.
SELECTION-SCREEN :END OF BLOCK bl1.

SELECTION-SCREEN :BEGIN OF SCREEN 100 AS SUBSCREEN.
PARAMETERS :p_comp TYPE bukrs MODIF ID t1.
SELECT-OPTIONS :s_as_n FOR anla-anln1,
                s_as_c FOR anla-anlkl,
                s_as_des FOR anla-txt50 NO INTERVALS,
                s_as_or FOR anla-eaufn MATCHCODE OBJECT orde,
                s_as_seg FOR anlz-segment,
                s_as_pc FOR anlz-prctr,
                s_as_cc FOR anlz-kostl.
SELECTION-SCREEN :END OF SCREEN 100 .

SELECTION-SCREEN :BEGIN OF SCREEN 101 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rd1 RADIOBUTTON GROUP g1 USER-COMMAND rd DEFAULT 'X'.
SELECTION-SCREEN COMMENT 10(15) FOR FIELD p_rd1.
PARAMETERS: p_rd2 RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT 27(15) FOR FIELD p_rd2.
SELECTION-SCREEN END OF LINE.
PARAMETERS :p_as_c   TYPE anla-anlkl MODIF ID t21,
            p_as_des TYPE anla-txt50 MODIF ID t21,
            p_as_cc  TYPE anlz-kostl MODIF ID t21,
            p_as_ufl TYPE ndjar MODIF ID t21,
            p_as_or  TYPE anla-eaufn MODIF ID t21,
            p_ev_gr1 TYPE anla-ord41 MODIF ID t21,
            p_ev_gr2 TYPE anla-ord42 MODIF ID t21,
            p_ev_gr3 TYPE anla-ord43 MODIF ID t21.


PARAMETERS :p_as_n TYPE anla-anln1 MODIF ID t22 .
SELECTION-SCREEN END OF SCREEN 101.

SELECTION-SCREEN :BEGIN OF SCREEN 102 AS SUBSCREEN.
PARAMETERS :p_blart  TYPE blart MODIF ID t3 DEFAULT 'AA',
            p_text   TYPE sgtxt,
            p_budat  TYPE budat MODIF ID t3,
            p_bldat  TYPE bldat MODIF ID t3,
            p_as_vld TYPE bzdat MODIF ID t3,
            p_asgmt  TYPE dzuonr,
            p_ref    TYPE xblnr1.
SELECTION-SCREEN END OF SCREEN 102.
