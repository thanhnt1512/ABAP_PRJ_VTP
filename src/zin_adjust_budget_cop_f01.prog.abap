*&---------------------------------------------------------------------*
*& Include          ZIN_ADJUST_BUDGET_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main .
  DATA :lw_col_sel TYPE string.
  DATA :lt_amt_par   TYPE TABLE OF ty_data_amt,
        lt_amt_recv  TYPE TABLE OF ty_data_amt,
        lt_amt_child TYPE TABLE OF ty_data_amt.
  SELECT * FROM ztb_rib_rule_use WHERE ci_send IN @s_ci AND fm_area = @p_fma AND fc_send IN @s_parfc AND fc_recv IN @s_fundc
  INTO TABLE @DATA(lt_rib_rule_use).
  PERFORM get_budget_period.

  IF lt_rib_rule_use IS NOT INITIAL.
    SELECT * FROM ztb_rib_rule_itm FOR ALL ENTRIES IN @lt_rib_rule_use
    WHERE id_hdr = @lt_rib_rule_use-id_hdr
    INTO TABLE @DATA(lt_rib_rule_itm).

    SELECT fikrs,parent_st,fistl,ci_send,ci_recv FROM fmhisv AS a INNER JOIN @lt_rib_rule_use AS b
    ON a~parent_st = b~fc_send AND a~fikrs = b~fm_area
    WHERE fistl <> b~fc_recv
    INTO TABLE @DATA(lt_par_child_fc).

    lw_col_sel = | RFUNDSCTR as FUNC, RCMMTITEM as CI, SUM( HSL{ p_per ALPHA = IN } ) as AMT |.

    SELECT (lw_col_sel) FROM fmavct AS a INNER JOIN @lt_rib_rule_use AS b
    ON a~rfikrs = b~fm_area AND a~rcmmtitem = b~ci_send AND a~rfundsctr = b~fc_send
    WHERE
        ryear = @p_year  AND rfund = @p_fund AND
        budget_pd_9 = @gw_bud_per AND
        rmeasure IN @s_prg AND rldnr = '9H'
    GROUP BY rfundsctr, rcmmtitem
    INTO CORRESPONDING FIELDS OF TABLE @lt_amt_par.

    SELECT (lw_col_sel) FROM fmavct AS a INNER JOIN @lt_rib_rule_use AS b
    ON a~rfikrs = b~fm_area AND a~rcmmtitem = b~ci_recv AND a~rfundsctr = b~fc_recv
    WHERE
        ryear = @p_year  AND rfund = @p_fund AND
        budget_pd_9 = @gw_bud_per AND
        rmeasure IN @s_prg AND rldnr = '9H'
    GROUP BY rfundsctr, rcmmtitem
    INTO CORRESPONDING FIELDS OF TABLE @lt_amt_recv.
    SORT lt_amt_recv BY func ci.

    lw_col_sel = | b~PARENT_ST as FC_PAR, b~CI_SEND as CI_PAR, RFUNDSCTR as FUNC, RCMMTITEM as CI, SUM( HSL{ p_per ALPHA = IN } ) as AMT |.

    SELECT (lw_col_sel) FROM fmavct AS a INNER JOIN @lt_par_child_fc AS b
    ON a~rfikrs = b~fikrs AND a~rcmmtitem = b~ci_recv AND a~rfundsctr = b~fistl
    WHERE
        ryear = @p_year  AND rfund = @p_fund AND
        budget_pd_9 = @gw_bud_per AND
        rmeasure IN @s_prg AND rldnr = '9H'
    GROUP BY b~parent_st,ci_send,rfundsctr, rcmmtitem
    INTO CORRESPONDING FIELDS OF TABLE @lt_amt_child.

    LOOP AT lt_rib_rule_use INTO DATA(ls_rib_rule_use).
      APPEND INITIAL LINE TO gt_data_collect ASSIGNING FIELD-SYMBOL(<fs_data_collect>).
      <fs_data_collect>-fc_par = ls_rib_rule_use-fc_send.
      <fs_data_collect>-ci_par = ls_rib_rule_use-ci_send.
      <fs_data_collect>-amt_root = REDUCE #(
                                             INIT val TYPE hslxx9_cs  FOR wa IN  lt_amt_par
                                             WHERE ( func = <fs_data_collect>-fc_par AND ci = <fs_data_collect>-ci_par )
                                             NEXT val = val + wa-amt
                                           ) * -1.
      <fs_data_collect>-child = VALUE #(
                                         FOR wa IN lt_amt_child
                                         WHERE ( fc_par = <fs_data_collect>-fc_par  AND ci_par = <fs_data_collect>-ci_par )
                                               ( fc = wa-func ci = wa-ci amt = wa-amt )
                                        ).
      <fs_data_collect>-amt_child = REDUCE #(
                                             INIT val TYPE hslxx9_cs FOR wa1 IN  <fs_data_collect>-child
                                             NEXT val = val + wa1-amt
                                           ).
      READ TABLE lt_amt_recv WITH KEY func = ls_rib_rule_use-fc_recv ci = ls_rib_rule_use-ci_recv BINARY SEARCH INTO DATA(ls_amt_recv).
      IF sy-subrc = 0.
        <fs_data_collect>-amt_rec_retn = ls_amt_recv-amt.
      ENDIF.
      <fs_data_collect>-fc_rec = ls_rib_rule_use-fc_recv.
      <fs_data_collect>-ci_rec = ls_rib_rule_use-ci_recv.
      LOOP AT lt_rib_rule_itm INTO DATA(ls_rib_rule_itm) WHERE id_hdr = ls_rib_rule_use-id_hdr.
        IF ls_rib_rule_itm-amt_from <= <fs_data_collect>-amt_root AND ls_rib_rule_itm-amt_to >= <fs_data_collect>-amt_root.
          <fs_data_collect>-percent = ls_rib_rule_itm-percent.
          EXIT.
        ENDIF.
      ENDLOOP.
      <fs_data_collect>-amt_trans = <fs_data_collect>-amt_root * <fs_data_collect>-percent / 100.
      <fs_data_collect>-amt_rec = <fs_data_collect>-amt_trans - <fs_data_collect>-amt_child.
      <fs_data_collect>-amt_rec = COND #( WHEN <fs_data_collect>-amt_rec > 0 THEN <fs_data_collect>-amt_rec ELSE 0 ).
    ENDLOOP.

    IF gt_data_collect IS NOT INITIAL.
      IF sy-batch = 'X'.
        PERFORM post_all.
      ELSE.
        CALL SCREEN 0100.
      ENDIF.
    ELSE.
      MESSAGE 'No data found !' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    MESSAGE 'No data found !' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETUP_TREE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM setup_tree .
  DATA: ls_hier_hdr   TYPE treev_hhdr,
        ls_variant    TYPE disvariant,
        ls_layout_src TYPE lvc_s_layo.
  CHECK go_tree_alv IS INITIAL OR sy-ucomm = 'POST'.
  REFRESH :gt_data_tree,gt_fc_tree.
*  CHECK go_tree_alv IS INITIAL.
  PERFORM build_header CHANGING ls_hier_hdr.
  PERFORM init_alv.
*  PERFORM add_toolbar.
  "display
  ls_variant-report = sy-repid.
  go_tree_alv->set_table_for_first_display(
     EXPORTING

*      is_layout            = ls_layout_src
      is_variant         = ls_variant
      i_save = 'X'
      is_hierarchy_header  = ls_hier_hdr
    CHANGING
      it_outtab            =  gt_data_tree
      it_fieldcatalog      =  gt_fc_tree
  ).

  go_tree_alv->top_of_page_event( ).
  PERFORM create_data_hier.
  CALL METHOD go_tree_alv->update_calculations.
  CALL METHOD go_tree_alv->frontend_update.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Automation Queue failure'(801)
        txt1  = 'Internal error:'(802)
        txt2  = 'A method in the automation queue'(803)
        txt3  = 'caused a failure.'(804).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_HIER_HDR
*&---------------------------------------------------------------------*
FORM build_header  CHANGING lps_hier_hdr TYPE treev_hhdr.
  lps_hier_hdr-heading = 'Account Assigment'.
  lps_hier_hdr-width = 40.
  lps_hier_hdr-width_pix = ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_alv .
  DATA: lobj_stdesc TYPE REF TO cl_abap_structdescr.
  DATA :ls_fcat      LIKE LINE OF gt_fc_tree,
        ls_tree_data LIKE LINE OF gt_data_tree,
        lt_dfies     TYPE ddfields.
*  DATA :lo_cont_tree LIKE go_cont_tree.
  DATA :lo_splitter  TYPE REF TO cl_gui_splitter_container,
        lo_cont_main TYPE REF TO cl_gui_container.
  DEFINE set_fcat.
    ls_fcat-fieldname = &1.
    ls_fcat-coltext = &2.
    ls_fcat-scrtext_m = &2.
    ls_fcat-scrtext_l = &2.
    ls_fcat-scrtext_s = &2.
    ls_fcat-reptext = &2.
    ls_fcat-no_zero = 'X'.
    ls_fcat-currency = &3.
    ls_fcat-outputlen = &4.
    APPEND ls_fcat TO gt_fc_tree.
    CLEAR ls_fcat.
  END-OF-DEFINITION.
  IF go_cont_tree IS NOT INITIAL.
    go_cont_tree->free( ).
  ENDIF.
  go_cont_tree = NEW cl_gui_custom_container( container_name = 'CONT_TREE' ).
  lo_splitter = NEW cl_gui_splitter_container( parent = go_cont_tree rows = 2 columns = 1 ).
  lo_splitter->set_row_height( id = 1  height = 10 ).
  go_cont_hdr = lo_splitter->get_container( row = 1 column = 1 ).
  go_cont_main = lo_splitter->get_container( row = 2 column = 1 ).
  go_tree_alv = NEW cl_gui_alv_tree(  parent =  go_cont_main node_selection_mode = cl_gui_column_tree=>node_sel_mode_single item_selection = 'X' no_html_header = 'X' no_toolbar = '' ).
  go_doc_hdr = NEW cl_dd_document( style = 'ALV_GRID' ).
  DATA(lo_event) = NEW lcl_event( ).
  SET HANDLER lo_event->handle_set_title FOR go_tree_alv.
  go_doc_hdr->initialize_document(
*    EXPORTING
*      first_time       =                  " Internal Use
*      style            =                  " Adjusting to the Style of a Particular GUI Environment
*      background_color =                  " Color ID
*      bds_stylesheet   =                  " Stylesheet Stored in BDS
*      no_margins       =  'X'                " Document Generated Without Free Margins
  ).
  set_fcat :"'NAME' '' 'Name',
*        'FC' 'FC' '' '15',
*        'CI' 'CI' '' '15',
        'AMT' 'Revenue' 'VND' '23',
        'PERCENT' 'Rate' '' '10',
        'AMT_TRANS' 'Calculated Amount' 'VND' '23',
        'AMT_CHILD_TOT' 'Posted Budget' 'VND' '23',
        'FC_REC' 'Receiver FC' '' '15',
        'CI_REC' 'Receiver CI' '' '15',
        'AMT_REC_RETN' 'Old Budget' 'VND' '23',
        'AMT_REC' 'New Budget' 'VND' '23'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DATA_HIER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_data_hier .
  DATA :ls_data LIKE LINE OF gt_data_tree,
        lw_key  TYPE lvc_nkey.
  LOOP AT gt_data_collect INTO DATA(ls_data_collect).
    ls_data-fc = ls_data_collect-fc_par.
    ls_data-ci = ls_data_collect-ci_par.
    ls_data-amt = ls_data_collect-amt_root.
    ls_data-percent = ls_data_collect-percent.
    ls_data-amt_child_tot = ls_data_collect-amt_child.
    ls_data-amt_trans = ls_data_collect-amt_trans.
    ls_data-fc_rec = ls_data_collect-fc_rec.
    ls_data-ci_rec = ls_data_collect-ci_rec.
    ls_data-amt_rec = ls_data_collect-amt_rec.
    ls_data-amt_rec_retn = ls_data_collect-amt_rec_retn.
    PERFORM add_hdr CHANGING lw_key ls_data.
    LOOP AT ls_data_collect-child INTO DATA(ls_child).
*      MOVE-CORRESPONDING ls_child TO ls_data.
      ls_data-amt_child_tot =  ls_child-amt.
      ls_data-fc = ls_child-fc.
      ls_data-ci = ls_child-ci.
      PERFORM add_itm USING lw_key CHANGING ls_data.
    ENDLOOP.
    CLEAR lw_key.
*    loop at
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_HDR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LW_KEY
*&      <-- LS_DATA
*&---------------------------------------------------------------------*
FORM add_hdr  CHANGING lpw_key TYPE lvc_nkey
                       lps_data TYPE ty_data_tree.
  DATA :ls_node_layout TYPE lvc_s_layn,
        lt_item_layout TYPE lvc_t_layi.
  lt_item_layout = VALUE #(
                            ( fieldname = 'AMT_REC' style = 6 )
*                            ( fieldname = 'FC_REC' style = 6 )
*                            ( fieldname = 'CI_REC' style = 6 )
                            ( fieldname = 'AMT_REC_RETN' style = 7 )
                          ).
  go_tree_alv->add_node(
  EXPORTING
    i_relat_node_key     = ''                " Node Already in Tree Hierarchy
    i_relationship       = cl_gui_column_tree=>relat_last_child                 " How to Insert Node
    i_node_text      = |{ lps_data-fc  } { lps_data-ci } ( Sender )|
    is_outtab_line   = lps_data
*    is_node_layout = ls_node_layout
    it_item_layout = lt_item_layout
  IMPORTING
    e_new_node_key   = lpw_key
).

  CLEAR lps_data.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_ITM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_KEY
*&      <-- LS_DATA
*&---------------------------------------------------------------------*
FORM add_itm  USING    lpw_key TYPE lvc_nkey
              CHANGING lps_data TYPE ty_data_tree.
  go_tree_alv->add_node(
    EXPORTING
      i_relat_node_key     = lpw_key                " Node Already in Tree Hierarchy
      i_relationship       = cl_gui_column_tree=>relat_last_child                 " How to Insert Node
      i_node_text      = |{ lps_data-fc  } { lps_data-ci }|
      is_outtab_line   = lps_data
*    IMPORTING
*      e_new_node_key   = lpw_key_new
  ).
  CLEAR lps_data.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM add_toolbar .
  DATA :lo_toolbar          TYPE REF TO cl_gui_toolbar.
  CALL METHOD go_tree_alv->get_toolbar_object
    IMPORTING
      er_toolbar = lo_toolbar.

  CHECK NOT lo_toolbar IS INITIAL.
  CALL METHOD lo_toolbar->add_button
    EXPORTING
      fcode     = 'POST'
      icon      = '@15@'
      butn_type = cntb_btype_button
      text      = 'POST'
      quickinfo = 'POST'.

  DATA :l_event_receiver TYPE REF TO lcl_event.
  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->on_function_selected FOR lo_toolbar.
  SET HANDLER l_event_receiver->handle_set_title FOR go_tree_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM post USING lpw_type CHANGING lpw_docnr TYPE bued_docnr
                                  lps_log_hdr TYPE ztb_log_zfm03_h.
  DATA :ls_bapi_hdr    TYPE bapi_0050_header,
        lt_bapi_period TYPE bapi_0050_t_period,
        lt_bapi_itm    TYPE bapi_0050_t_item,
        lt_bapi_return TYPE bapiret2_t.
  PERFORM fill_header USING lpw_type  CHANGING ls_bapi_hdr.
  PERFORM fill_itm USING lpw_type  CHANGING lt_bapi_itm lt_bapi_period.
  PERFORM create_doc USING ls_bapi_hdr lt_bapi_itm lt_bapi_period lt_bapi_return lpw_type CHANGING lpw_docnr lps_log_hdr.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILL_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_BAPI_HDR
*&---------------------------------------------------------------------*
FORM fill_header USING lpw_type TYPE char1
                 CHANGING lps_hdr TYPE bapi_0050_header.
  lps_hdr-docdate = p_date.
  lps_hdr-fm_area = p_fma.
  lps_hdr-version = '000'.
  lps_hdr-docstate = 1.
  IF lpw_type = 'P'.
    lps_hdr-process = 'RBBT'.
  ELSE.
    lps_hdr-process = 'RBBS'.
  ENDIF.
  lps_hdr-doctype = 'ZRIB'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_ITM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_BAPI_ITM
*&      <-- LT_BAPI_PERIOD
*&---------------------------------------------------------------------*
FORM fill_itm USING
                        lpw_type TYPE char1
              CHANGING
                        lpt_bapi_itm TYPE bapi_0050_t_item
                        lpt_bapi_period TYPE bapi_0050_t_period.
  DATA :lw_index TYPE int4.
  DATA: ls_bapi_item   TYPE bapi_0050_item,
        ls_bapi_period TYPE bapi_0050_period.
  SELECT SINGLE * FROM tcurx WHERE currkey = 'VND' INTO @DATA(ls_curr).
  LOOP AT gt_data_collect INTO DATA(ls_data_collect).
    CHECK ls_data_collect-amt_rec <> ls_data_collect-amt_rec_retn AND ls_data_collect-amt_rec > 0.
    IF lpw_type = 'R'.
      CHECK ls_data_collect-amt_rec_retn <> 0.
    ELSEIF lpw_type = 'P'.
      CHECK ls_data_collect-amt_rec <> 0.
    ENDIF.
    lw_index = lw_index + 1.
    ls_bapi_item-item_num = lw_index.
    ls_bapi_item-fisc_year = p_year.
    ls_bapi_item-trans_curr = 'VND'.
    ls_bapi_item-budget_period = gw_bud_per.
    ls_bapi_item-fund = p_fund.
*    ls_bapi_item-trans_curr_iso = 'VND'.
    ls_bapi_item-valtype  = 'B1' .
    ls_bapi_item-budcat = '9F' .
*    ls_bapi_item-budtype  = 'ZRIB'.
    IF lpw_type = 'P'.
      ls_bapi_item-budtype  = 'ZRBT'.
    ELSE.
      ls_bapi_item-budtype  = 'ZRBS'.
    ENDIF.
    ls_bapi_item-funds_ctr = ls_data_collect-fc_rec.
    ls_bapi_item-cmmt_item = ls_data_collect-ci_rec.
    APPEND INITIAL LINE TO lpt_bapi_period ASSIGNING FIELD-SYMBOL(<fs_bapi_period>).
    <fs_bapi_period>-budgeting_period = |{ p_per ALPHA = IN }|.
    <fs_bapi_period>-item_num  = lw_index.
    IF lpw_type = 'P'.
      <fs_bapi_period>-period_amount = ls_data_collect-amt_rec .
    ELSE.
      <fs_bapi_period>-period_amount = ls_data_collect-amt_rec_retn.
    ENDIF.
    <fs_bapi_period>-period_amount = <fs_bapi_period>-period_amount * ( 10 ** ( 2 - ls_curr-currdec  ) ).
    APPEND ls_bapi_item TO lpt_bapi_itm.
    CLEAR ls_bapi_item.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_BAPI_HDR
*&      <-- LT_BAPI_ITM
*&      <-- LT_BAPI_PERIOD
*&---------------------------------------------------------------------*
FORM create_doc
                  USING   lps_bapi_hdr TYPE bapi_0050_header
                          lpt_bapi_itm TYPE bapi_0050_t_item
                          lpt_bapi_period TYPE  bapi_0050_t_period
                          lpt_bapi_return TYPE bapiret2_t
                          lpw_type TYPE char1
                  CHANGING lpw_docnr TYPE bued_docnr
                           lps_log_hdr TYPE ztb_log_zfm03_h.

  CHECK NOT ( lpt_bapi_period IS INITIAL OR lpt_bapi_itm IS INITIAL ).
  DATA: lv_docnr TYPE bued_docnr.
  DATA :lt_log_i TYPE TABLE OF ztb_log_zfm03_i.
  REFRESH gt_mess_log.
  lps_log_hdr-fm_area = p_fma.
  lps_log_hdr-gjahr = p_year.
  CALL FUNCTION 'BAPI_0050_CREATE'
    EXPORTING
      language       = 'E'
      header_data    = lps_bapi_hdr
      testrun        = ''
    IMPORTING
      documentnumber = lv_docnr
    TABLES
      item_data      = lpt_bapi_itm
      period_data    = lpt_bapi_period
      return         = lpt_bapi_return.
  SORT lpt_bapi_return BY type id number.
  DELETE lpt_bapi_return WHERE id = 'FMBAPI' AND number = '004'.

  READ TABLE lpt_bapi_return WITH KEY type = 'E' TRANSPORTING NO FIELDS .
  IF sy-subrc = 0.
    gw_stt = 'Error'.
    SELECT CASE type WHEN 'I' THEN '@09@' ELSE '@0A@' END AS status,
                id,number,message,message_v1,message_v2,message_v3,message_v4
    FROM @lpt_bapi_return AS lpt_bapi_return INTO CORRESPONDING FIELDS OF TABLE @gt_mess_log.
    LOOP AT gt_mess_log INTO DATA(ls_mess_log).
      APPEND INITIAL LINE TO lt_log_i ASSIGNING FIELD-SYMBOL(<fs_log_i>).
      <fs_log_i>-guid = cl_system_uuid=>create_uuid_x16_static( ).
      <fs_log_i>-guid_hdr = lps_log_hdr-guid.
      <fs_log_i>-id = ls_mess_log-id.
      <fs_log_i>-mess_num = ls_mess_log-number.
      <fs_log_i>-message = ls_mess_log-message.
      <fs_log_i>-message_v1 = ls_mess_log-message_v1.
      <fs_log_i>-message_v2 = ls_mess_log-message_v2.
      <fs_log_i>-message_v3 = ls_mess_log-message_v3.
      <fs_log_i>-message_v4 = ls_mess_log-message_v4.
    ENDLOOP.
    lps_log_hdr-status = 'Error'.
    MODIFY ztb_log_zfm03_h FROM lps_log_hdr.
    INSERT ztb_log_zfm03_i FROM TABLE lt_log_i.
    COMMIT WORK AND WAIT.

  ELSE.
    COMMIT WORK AND WAIT.
    gw_stt = 'Success'.
    lpw_docnr = lv_docnr.
    IF lpw_type = 'P'.
      lps_log_hdr-doc_entr = lv_docnr.
    ELSE.
      lps_log_hdr-doc_retn = lv_docnr.
    ENDIF.
    lps_log_hdr-status = 'Success'.
    MODIFY ztb_log_zfm03_h FROM lps_log_hdr.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUID_FCAT_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buid_fcat_log .
  REFRESH gt_fcat_log.
  DATA : ls_fcat_log LIKE LINE OF gt_fcat_log.
  DEFINE set_fcat_log.
    ls_fcat_log-fieldname = &1.
    ls_fcat_log-tabname   = &2.
    ls_fcat_log-seltext_m = &3.
    ls_fcat_log-icon      = &4.
*    ls_fcat_log-outputlen = &5.
    APPEND ls_fcat_log TO gt_fcat_log.
    CLEAR ls_fcat_log.
  END-OF-DEFINITION.

  set_fcat_log : 'STATUS' 'GT_MESS_LOG' 'Status' 'X',
               'ID' 'GT_MESS_LOG' 'Message class' '',
               'NUMBER' 'GT_MESS_LOG' 'Number message' '',
               'MESSAGE' 'GT_MESS_LOG' 'Message text' '',
               'MESSAGE_V1' 'GT_MESS_LOG' 'Message V1' '',
               'MESSAGE_V2' 'GT_MESS_LOG' 'Message V2' '',
               'MESSAGE_V3' 'GT_MESS_LOG' 'Message V3' '',
               'MESSAGE_V4' 'GT_MESS_LOG' 'Message V4' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_log .
  DATA :ls_variant TYPE disvariant,
        ls_layout  TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = 'X'.
  ls_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
*     i_callback_program = sy-repid
      is_layout   = ls_layout
      is_variant  = ls_variant
      it_fieldcat = gt_fcat_log
    TABLES
      t_outtab    = gt_mess_log.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LOG_HDR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_LOG_HDR
*&---------------------------------------------------------------------*
FORM set_log_hdr  CHANGING lps_log_hdr TYPE ztb_log_zfm03_h.
  CLEAR lps_log_hdr.
  IF sy-batch = 'X'.
    lps_log_hdr-bgd = 'X'.
  ENDIF.
  lps_log_hdr-guid = cl_system_uuid=>create_uuid_x16_static( ).
  lps_log_hdr-date_cre = sy-datum.
  lps_log_hdr-time_cre = sy-timlo.
  lps_log_hdr-user_cre = sy-uname.
  INSERT ztb_log_zfm03_h FROM lps_log_hdr.
  COMMIT WORK AND WAIT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM post_all .
  CLEAR gw_stt.
  DATA :ls_log_hdr TYPE ztb_log_zfm03_h.
  PERFORM set_log_hdr CHANGING ls_log_hdr.
  PERFORM post USING 'R' CHANGING gw_doc_retn ls_log_hdr.
  IF gw_stt <> 'Error'.
    PERFORM post USING 'P' CHANGING gw_doc_entr ls_log_hdr.
  ENDIF.

  IF gw_stt = ''.
    ls_log_hdr-status = 'No data'.
    MODIFY ztb_log_zfm03_h FROM ls_log_hdr.
    COMMIT WORK AND WAIT.
    MESSAGE 'No data post' TYPE 'I'." DISPLAY LIKE 'S'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_HISTORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_history .
  CALL SELECTION-SCREEN '1001' STARTING AT 10 10.
  CHECK sy-subrc = 0.
  PERFORM view_his_hdr.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_HIS_HDR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

DEFINE set_fc_his .
*  DATA : ls_fc_his TYPE slis_fieldcat_alv.
  &4-fieldname = &1.
  &4-seltext_m = &2.
  &4-seltext_s = &2.
  &4-seltext_l = &2.
  &4-hotspot = &3.
  APPEND &4 TO &5.
  CLEAR &4.
END-OF-DEFINITION.

FORM view_his_hdr .

  SELECT * FROM ztb_log_zfm03_h INTO TABLE @gt_log_zfm03_h WHERE user_cre IN @s_user AND date_cre IN @s_date AND time_cre IN @s_time.
  IF sy-subrc = 0.
    DATA :lt_fc_hdr_his TYPE slis_t_fieldcat_alv,
          lt_event      TYPE slis_t_event.
    lt_event = VALUE #( ( name = 'USER_COMMAND' form = 'UC_HDR') ).
    PERFORM set_fc_his_hdr CHANGING lt_fc_hdr_his.
    PERFORM display_his TABLES gt_log_zfm03_h USING lt_fc_hdr_his lt_event 'H'.
  ELSE.
    MESSAGE 'No data found' TYPE 'I'.
  ENDIF.
ENDFORM.

FORM uc_hdr USING ucomm LIKE sy-ucomm selfied TYPE slis_selfield.
  DATA(ls_data) = gt_log_zfm03_h[ selfied-tabindex ].
  CASE selfied-fieldname.
    WHEN 'DOC_RETN' OR 'DOC_ENTR'.
      DATA :lw_doc TYPE char10.
      CHECK ls_data-doc_retn IS NOT INITIAL OR ls_data-doc_entr IS NOT INITIAL.
      lw_doc = COND #( WHEN selfied-fieldname = 'DOC_RETN' THEN ls_data-doc_retn ELSE ls_data-doc_entr ).
      SUBMIT rffmed_print
      WITH   p_fikrs = ls_data-fm_area
      WITH   p_dyear =  ls_data-gjahr
      WITH   p_docnr = lw_doc AND RETURN.
    WHEN 'STATUS'.
      REFRESH gt_log_zfm03_i.
      CHECK ls_data-status = 'Error'.
      SELECT * FROM ztb_log_zfm03_i WHERE guid_hdr = @ls_data-guid INTO TABLE @gt_log_zfm03_i.
      CHECK sy-subrc = 0.
      PERFORM view_his_itm.


  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_FC_HIS_HDR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_fc_his_hdr CHANGING lpt_fc_hdr_his TYPE slis_t_fieldcat_alv.

  DATA : ls_fc_his TYPE slis_fieldcat_alv.
  set_fc_his : 'USER_CRE' 'User Create' '' ls_fc_his lpt_fc_hdr_his,
               'DATE_CRE' 'Date Create' '' ls_fc_his lpt_fc_hdr_his,
               'TIME_CRE' 'Time Create' '' ls_fc_his lpt_fc_hdr_his,
               'BGD' 'Background' '' ls_fc_his lpt_fc_hdr_his,
               'STATUS' 'Status' 'X' ls_fc_his lpt_fc_hdr_his,
               'DOC_RETN' 'Doc RETN' 'X' ls_fc_his lpt_fc_hdr_his,
               'DOC_ENTR' 'Doc ENTR' 'X' ls_fc_his lpt_fc_hdr_his.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_HIS_HDR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_his     TABLES lpt_data TYPE STANDARD TABLE
                     USING
                           lpt_fc TYPE slis_t_fieldcat_alv
                           lpt_event TYPE slis_t_event
                           lpw_type TYPE char1.
  DATA :ls_layout_slis TYPE slis_layout_alv,
        lw_title       TYPE lvc_title.
  lw_title = COND #( WHEN lpw_type = 'I' THEN 'Error Details'  ELSE 'Header History' ) .
*  DATA:lo_tab_data  TYPE REF TO data.
  ls_layout_slis-colwidth_optimize = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_title       = lw_title
      i_callback_program = sy-cprog
      is_layout          = ls_layout_slis
      it_fieldcat        = lpt_fc
      it_events          = lpt_event
    TABLES
      t_outtab           = lpt_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID   sy-msgid
      TYPE 'E'
      NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_HIS_ITM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM view_his_itm .
  DATA :lt_fc_itm TYPE slis_t_fieldcat_alv,
        lt_event  TYPE slis_t_event.
*  lt_event = VALUE #( ( name = 'USER_COMMAND' form = 'UC_HDR') ).
  PERFORM set_fc_his_itm CHANGING lt_fc_itm.
  PERFORM display_his TABLES gt_log_zfm03_i USING lt_fc_itm lt_event 'I'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FC_HIS_ITM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FC_ITM
*&---------------------------------------------------------------------*
FORM set_fc_his_itm  CHANGING lpt_fc_itm TYPE slis_t_fieldcat_alv..
  DATA : ls_fc_his TYPE slis_fieldcat_alv.
  set_fc_his : 'ID' 'Message ID' '' ls_fc_his lpt_fc_itm,
               'MESS_NUM' 'Message Number' '' ls_fc_his lpt_fc_itm,
               'MESSAGE' 'Message Text' '' ls_fc_his lpt_fc_itm,
               'MESSAGE_V1' 'Message V1' '' ls_fc_his lpt_fc_itm,
               'MESSAGE_V2' 'Message V2' '' ls_fc_his lpt_fc_itm,
               'MESSAGE_V3' 'Message V3' '' ls_fc_his lpt_fc_itm,
               'MESSAGE_V4' 'Message V4' '' ls_fc_his lpt_fc_itm.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_REQUIRE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_require .
  IF p_fma  = ''.
    MESSAGE 'FM AREA is required' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_year  = ''.
    MESSAGE 'Fiscal Year is required' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_date  = '00000000'.
    MESSAGE 'Document Date is required' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_fund = ''.
    MESSAGE 'Fund is required' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_per = ''.
    MESSAGE 'Period is required' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BUDET_PERIOD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_budget_period .
  DATA :lw_quarter TYPE char2,lw_bud_tar   TYPE string.
  IF p_per MOD 3 <> 0.
    lw_quarter = |Q{ p_per DIV 3 + 1 }|.
  ELSE.
    lw_quarter = |Q{ p_per DIV 3 }|.
  ENDIF.
  lw_bud_tar = |{ p_year }.{ lw_quarter }|.
  SELECT SINGLE budget_pd FROM fmbudgetpd INTO gw_bud_per WHERE budget_pd = lw_bud_tar .

  IF gw_bud_per IS INITIAL.
    MESSAGE |Budget period { lw_bud_tar } không tồn tại| TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
