*&---------------------------------------------------------------------*
*& Include          ZPG_CREATE_XDCBDD_F01
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
  PERFORM get_data.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  DATA :lt_tab_ass TYPE ztt_asset.
  DATA :lw_tt_as TYPE fins_vhcur12.
  SELECT a~bukrs,a~anln1,a~anlkl,txt50 FROM anla AS a INNER JOIN anlz AS b ON a~bukrs = b~bukrs AND
                                                                        a~anln1 = b~anln1 AND
                                                                        a~anln2 = b~anln2
  INTO CORRESPONDING FIELDS OF TABLE @gt_data
  WHERE a~bukrs = @p_comp AND a~anln1 IN @s_as_n AND anlkl IN @s_as_c AND
        txt50 IN @s_as_des AND eaufn IN @s_as_or AND segment IN @s_as_seg AND
        prctr IN @s_as_pc AND kostl IN @s_as_cc.
*  DELETE gt_data WHERE as_amt = 0.
  IF gt_data IS INITIAL.
    MESSAGE 'Không có dữ liệu !' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  lt_tab_ass = CORRESPONDING #( gt_data MAPPING asset_num = anln1 ).

  PERFORM set_main_data_from_postab USING lt_tab_ass p_comp.

  IF p_rd2 = 'X'.
    SELECT SINGLE anln1 , anlkl, txt50 FROM anla INTO CORRESPONDING FIELDS OF @gs_ass_get WHERE anln1 = @p_as_n.
    gw_ass_crd = p_as_n.
    gw_ass_crd = |{ p_as_n ALPHA = IN }|.
    SELECT SINGLE answl FROM anlc WHERE anln1 = @gw_ass_crd AND bukrs = @p_comp AND afabe = '01' INTO @gw_tt_as.
*    WRITE lw_tt_as TO gw_tt_as CURRENCY 'VND'.
*    CONDENSE gw_tt_as.
  ELSE.
    gs_ass_get-anlkl = p_as_c.
    gs_ass_get-txt50 = p_as_des.
  ENDIF.

  gs_ass_get-anlkl = |{ gs_ass_get-anlkl ALPHA = OUT }|.

  CALL SCREEN 0200.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUID_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buid_alv .

  PERFORM buid_fcat CHANGING gt_fcat.
  PERFORM display_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUID_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buid_fcat CHANGING lpt_fcat LIKE gt_fcat.
  REFRESH lpt_fcat.
  FIELD-SYMBOLS :<fs_fcat> LIKE LINE OF lpt_fcat.

  DEFINE buid_fcat.
    APPEND INITIAL LINE TO lpt_fcat ASSIGNING <fs_fcat>.
    <fs_fcat>-col_pos = &1.
    <fs_fcat>-fieldname = &2.
    <fs_fcat>-scrtext_m = &3.
    <fs_fcat>-cfieldname = &4.
    <fs_fcat>-no_zero = &5.
    <fs_fcat>-no_out = &6.
    <fs_fcat>-edit = &7.
    <fs_fcat>-hotspot = &8.
    <fs_fcat>-icon = &9.
  END-OF-DEFINITION.
  buid_fcat :
              '1' 'BUKRS' 'Comp Code' '' '' '' '' '' '',
              '2' 'ANLN1' 'Asset Number' '' 'X' '' '' '' '',
              '3' 'ANLKL' 'Asset Class' '' 'X' '' '' '' '',
              '4' 'TXT50' 'Asset Description' '' '' '' '' '' '',
              '5' 'AS_AMT' 'Amount' 'WAERS' 'X' '' '' '' '',
              '6' 'STT_RULE' 'Status Rule' '' '' '' '' '' 'X',
              '7' 'AS_RULE_GR' 'Dist.Rule Group' '' '' '' '' 'X' '',
              '8' 'AS_SET_PER' 'Settlement Percentage ' '' 'X' '' 'X' '' '',
              '9' 'AS_AMT_RULE' 'Amount Rule' 'WAERS' 'X' '' 'X' '' '',
              '10' 'AS_SET_AMT' 'Settlement Amount' 'WAERS' 'X' '' '' '' '',
              '11' 'STT_SETTLE' 'Status Settle' '' '' '' '' '' 'X',
              '12' 'FI_DOC' 'FI Document' '' 'X' '' '' 'X' '',
              '13' 'WAERS' 'Cuky' '' '' 'X' '' '' ''.

  READ TABLE lpt_fcat  ASSIGNING <fs_fcat> WITH KEY fieldname = 'AS_SET_PER'.
  <fs_fcat>-decimals = 2.
  <fs_fcat>-inttype = 'P'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .
  DATA :lo_cont_main TYPE REF TO cl_gui_container.
  DATA:ls_layout_src TYPE lvc_s_layo,
       ls_layout     TYPE disvariant,
       lt_exclude    TYPE ui_functions.
  ls_layout-report = sy-repid.

  FREE :g_cont_header,g_alv.
  ls_layout_src-cwidth_opt = 'X'.
  ls_layout_src-sel_mode = 'A'.
  DATA(lo_handler) = NEW lcl_event_handler( ).
  IF g_cont IS BOUND.
    g_cont->free( ).
  ENDIF.
  g_cont = NEW cl_gui_custom_container( container_name = 'CONTAINER' ).
  DATA(lo_split) = NEW cl_gui_splitter_container( parent  = g_cont rows = 2 columns = 1 ).
  g_cont_header = lo_split->get_container( row = 1 column = 1 ).
  g_doc_header = NEW cl_dd_document( style = 'ALV_GRID' ).
  lo_split->set_row_height( id = 1  height = 17 ).
  lo_cont_main = lo_split->get_container( row = 2 column = 1 ).
  g_alv = NEW cl_gui_alv_grid( i_parent = lo_cont_main ).
  SET HANDLER lo_handler->handle_set_title FOR g_alv.
  SET HANDLER lo_handler->handle_data_changed FOR g_alv.
  SET HANDLER lo_handler->handle_hotspot_click FOR g_alv.

  g_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
  g_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

  g_alv->list_processing_events(  i_event_name = 'TOP_OF_PAGE' i_dyndoc_id  = g_doc_header ).

  PERFORM exclude_tb_functions CHANGING lt_exclude.
  CALL METHOD g_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = ls_layout_src
      is_variant                    = ls_layout
      i_save                        = 'X'
*     i_structure_name              = p_structure_name
      it_toolbar_excluding          = lt_exclude
    CHANGING
      it_outtab                     = gt_data
      it_fieldcatalog               = gt_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc IS NOT INITIAL.
*        Display system error messgae
    MESSAGE ID   sy-msgid
            TYPE 'E'
            NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ASS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_ass .

  DATA: ls_key                  TYPE bapi1022_key,
        ls_generaldata          TYPE bapi1022_feglg001,
        ls_generaldatax         TYPE bapi1022_feglg001x,
        ls_timedependentdata    TYPE bapi1022_feglg003,
        ls_timedependentdatax   TYPE bapi1022_feglg003x,
        ls_allocations          TYPE bapi1022_feglg004,
        ls_allocationsx         TYPE bapi1022_feglg004x,
        ls_investacctassignmnt  TYPE bapi1022_feglg010,
        ls_investacctassignmntx TYPE bapi1022_feglg010x,
        ls_origin               TYPE bapi1022_feglg009,
        ls_originx              TYPE bapi1022_feglg009x,
        ls_reference            TYPE bapi1022_reference,
        ls_return               TYPE bapiret2,
        ls_depreciationareas    TYPE bapi1022_dep_areas,
        lt_depreciationareas    TYPE TABLE OF bapi1022_dep_areas,
        ls_depreciationareasx   TYPE bapi1022_dep_areasx,
        lt_depreciationareasx   TYPE TABLE OF bapi1022_dep_areasx.
* build key
  ls_key-companycode = p_comp.
*  ls_key-asset = lfs_display-asset.
  ls_key-subnumber = '0'.
* build generaldata
  ls_generaldata-assetclass	= p_as_c.
  ls_generaldata-descript	= p_as_des.

  ls_generaldatax-assetclass = 'X'.
  ls_generaldatax-descript = 'X'.
* build timedependentdata
  ls_timedependentdata-costcenter = p_as_cc.
  ls_timedependentdatax-costcenter = 'X'.
* build allocations
  ls_allocations-evalgroup1	=  p_ev_gr1.
  ls_allocations-evalgroup2	=  p_ev_gr2.
  ls_allocations-evalgroup3	=  p_ev_gr3.

  ls_allocationsx-evalgroup1 = 'X'.
  ls_allocationsx-evalgroup2 = 'X'.
  ls_allocationsx-evalgroup3 = 'X'.
* Build investment
  ls_investacctassignmnt-invest_ord = p_as_or.
  ls_investacctassignmntx-invest_ord = 'X'.

*  build depreciationareas
  ls_depreciationareas-area	= '01'.
  ls_depreciationareas-ulife_yrs  = p_as_ufl.
  APPEND ls_depreciationareas TO lt_depreciationareas.

  ls_depreciationareasx-area = '01'.
  ls_depreciationareasx-ulife_yrs = 'X'.
  APPEND ls_depreciationareasx TO lt_depreciationareasx.

  ls_depreciationareas-area	= '02'.
  APPEND ls_depreciationareas TO lt_depreciationareas.

  ls_depreciationareasx-area = '02'.
  ls_depreciationareasx-ulife_yrs = 'X'.
  APPEND ls_depreciationareasx TO lt_depreciationareasx.

  CALL FUNCTION 'BAPI_FIXEDASSET_CREATE1'
    EXPORTING
      key                  = ls_key
      generaldata          = ls_generaldata
      generaldatax         = ls_generaldatax
      timedependentdata    = ls_timedependentdata
      timedependentdatax   = ls_timedependentdatax
      allocations          = ls_allocations
      allocationsx         = ls_allocationsx
      investacctassignmnt  = ls_investacctassignmnt
      investacctassignmntx = ls_investacctassignmntx
    IMPORTING
      assetcreated         = ls_reference
      return               = ls_return
    TABLES
      depreciationareas    = lt_depreciationareas
      depreciationareasx   = lt_depreciationareasx.

  IF ls_return-type = 'E'.
*    MESSAGE 'Lỗi tạo asset' TYPE 'E' DISPLAY LIKE 'I'.
    MESSAGE ID ls_return-id TYPE 'E' NUMBER ls_return-number WITH |{ ls_return-message_v1 }| ls_return-message_v2 ls_return-message_v3 |{ ls_return-message_v4 }| DISPLAY LIKE 'I'.
    LEAVE LIST-PROCESSING.
  ELSE.
    gw_ass_crd = ls_reference-asset.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RULE_SETTLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM set_rule_settle.
  DATA :lt_tab_mess TYPE esp1_message_tab_type.
  DATA :lt_postab TYPE gty_t_ass_post.
  DATA :lt_mess TYPE bapiret2_tab.
  DATA :lt_history TYPE TABLE OF ztb_auc_log.
  DATA :lt_data_sel    LIKE gt_data,
        lt_row_sel     TYPE lvc_t_row,
        lt_ass_post    TYPE ztt_asset,
        ls_data_settle TYPE zst_settlement,
        lt_data_settle TYPE TABLE OF zst_settlement.
  DATA :lw_error TYPE char1.
  g_alv->check_changed_data( ).
*  g_alv->refresh_table_display( ).

  CALL METHOD g_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_row_sel.

  IF lt_row_sel IS INITIAL.
    MESSAGE 'Chọn line để post' TYPE 'E' DISPLAY LIKE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.


  LOOP AT lt_row_sel INTO DATA(ls_row_sel).
    READ TABLE gt_data INTO DATA(ls_data) INDEX ls_row_sel-index.
    IF sy-subrc = 0.
      APPEND ls_data TO lt_data_sel.
    ENDIF.
  ENDLOOP.

  PERFORM check_before_sett USING lt_data_sel.
  PERFORM set_history USING lt_data_sel CHANGING lt_history.

  IF p_rd1 = 'X'.
    PERFORM create_ass.
    gs_ass_get-anln1 = gw_ass_crd.
  ENDIF.

  lt_ass_post = VALUE #( FOR wa IN lt_data_sel ( asset_num = wa-anln1 per_set = wa-as_set_per set_recv = gw_ass_crd amt_set = wa-as_amt_rule ) ).

  g_stt = 'S'.

  CALL FUNCTION 'ZFM_COMMON_AIAB_AIBU'
    EXPORTING
      list_asset    = lt_ass_post
      ass_cat       = 'AN'
      comp          = p_comp
      handle        = 'SET_RULE'
    EXCEPTIONS
      error_message = 1.
  IF sy-subrc <> 0.
    g_stt = 'E'.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      EXPORTING
        i_msgid  = sy-msgid
        i_msgty  = sy-msgty
        i_msgno  = sy-msgno
        i_msgv1  = sy-msgv1
        i_msgv2  = sy-msgv2
        i_msgv3  = sy-msgv3
        i_msgv4  = sy-msgv4
        i_lineno = 1.
    CLEAR gs_ass_get-anln1.
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  IF g_stt = 'S'.
    COMMIT WORK AND WAIT.
  ENDIF.

  CALL FUNCTION 'ZFM_COMMON_AIAB_AIBU'
    EXPORTING
      list_asset = lt_ass_post
      comp       = p_comp
      handle     = 'GET_POSTAB'
    IMPORTING
      postab     = lt_postab.
  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    READ TABLE lt_postab WITH KEY asset = <fs_data>-anln1 INTO DATA(ls_ass_postab).
    IF sy-subrc = 0.
      <fs_data>-as_rule_gr = VALUE #( ls_ass_postab-postab[ 1 ]-bureg OPTIONAL ).
      <fs_data>-stt_rule = COND #( WHEN <fs_data>-as_rule_gr <> '000' THEN '@5B@' ELSE '@5C@' ).
      READ TABLE lt_history INTO DATA(ls_his) WITH KEY asset_auc = <fs_data>-anln1.
      IF sy-subrc = 0.
        UPDATE ztb_auc_log SET bureg = <fs_data>-as_rule_gr
                               asset_comp = gs_ass_get-anln1
                           WHERE guid = ls_his-guid.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDLOOP.


  LOOP AT lt_ass_post INTO DATA(ls_ass_post).
    ls_data_settle-bukrs =  p_comp.
    ls_data_settle-anln1 =  ls_ass_post-asset_num.
    ls_data_settle-monat =  p_budat+4(2).
    ls_data_settle-bldat =  p_bldat.
    ls_data_settle-bzdat =  p_as_vld.
    ls_data_settle-budat =  p_budat.
    ls_data_settle-blart =  p_blart.
    ls_data_settle-zuonr =  p_asgmt.
    ls_data_settle-xblnr =  p_ref.
    ls_data_settle-sgtxt =  p_text.


    CALL FUNCTION 'ZFM_COMMON_AIAB_AIBU'
      EXPORTING
        handle         = 'SETTLEMENT'
        data_settle    = ls_data_settle
        test           = 'X'
      CHANGING
        message_settle = lt_mess
      EXCEPTIONS
        error_message  = 1.

    IF sy-subrc <> 0.
      g_stt = 'E'.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      APPEND INITIAL LINE TO lt_tab_mess ASSIGNING FIELD-SYMBOL(<fs_tab_mess>).
      <fs_tab_mess>-msgid = sy-msgid.
      <fs_tab_mess>-msgty = sy-msgty.
      <fs_tab_mess>-msgno = sy-msgno.
      <fs_tab_mess>-msgv1 = sy-msgv1.
      <fs_tab_mess>-msgv2 = sy-msgv2.
      <fs_tab_mess>-msgv3 = sy-msgv3.
      <fs_tab_mess>-msgv4 = sy-msgv4.
*      <fs_tab_mess>-lineno = sy-tabix.
      READ TABLE gt_data ASSIGNING <fs_data> WITH KEY anln1 = ls_ass_post-asset_num.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO lt_tab_mess ASSIGNING <fs_tab_mess>.
        <fs_tab_mess>-msgid = 'ZFI'.
        <fs_tab_mess>-msgty = 'E'.
        <fs_tab_mess>-msgno = '021'.
        <fs_tab_mess>-msgv1 = ls_ass_post-asset_num.
        <fs_data>-stt_settle = '@2O@'.
      ENDIF.

    ELSE.
      APPEND ls_data_settle TO lt_data_settle.
      CLEAR ls_data_settle.
    ENDIF.
    REFRESH lt_mess.
  ENDLOOP.


  IF lt_tab_mess IS NOT INITIAL.
    IF p_rd1 = 'X'.
      APPEND INITIAL LINE TO lt_tab_mess ASSIGNING <fs_tab_mess>.
      <fs_tab_mess>-msgid = 'ZFI'.
      <fs_tab_mess>-msgty = 'S'.
      <fs_tab_mess>-msgno = '022'.
      <fs_tab_mess>-msgv1 = gw_ass_crd.
    ENDIF.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_tab_mess.
    LEAVE LIST-PROCESSING.
  ENDIF.


  IF g_stt = 'S'.
    COMMIT WORK AND WAIT.
    LOOP AT lt_data_settle INTO DATA(ls_post_set).
      CALL FUNCTION 'ZFM_COMMON_AIAB_AIBU'
        EXPORTING
          handle         = 'SETTLEMENT'
          data_settle    = ls_post_set
        CHANGING
          message_settle = lt_mess
        EXCEPTIONS
          error_message  = 1.

      READ TABLE gt_data ASSIGNING <fs_data> WITH KEY anln1 = ls_post_set-anln1.
      IF sy-subrc = 0.
        <fs_data>-fi_doc = lt_mess[ type = 'S' id = 'FAA_POST' number = 92 ]-message_v2.
        <fs_data>-stt_settle = '@2K@'.
        READ TABLE lt_history INTO ls_his WITH KEY asset_auc = ls_post_set-anln1.
        IF sy-subrc = 0.
          UPDATE ztb_auc_log SET fi_doc = <fs_data>-fi_doc  WHERE guid = ls_his-guid.
        ENDIF.
      ENDIF.
      REFRESH lt_mess.
    ENDLOOP.
    CLEAR gw_tt_as.
    SELECT SINGLE answl FROM anlc WHERE anln1 = @gw_ass_crd AND bukrs = @p_comp AND afabe = '01' INTO @gw_tt_as.

  ENDIF.

*  gw_tt_as =  lw_tt_as.

  g_alv->refresh_table_display( ).
*  CALL SCREEN 0200.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_MAIN_DATA_FROM_POSTAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_TAB_ASS
*&      --> P_COMP
*&---------------------------------------------------------------------*
FORM set_main_data_from_postab  USING    lpt_tab_ass TYPE ztt_asset
                                         lwp_comp TYPE bukrs.
  DATA :lt_postab TYPE gty_t_ass_post.

  CALL FUNCTION 'ZFM_COMMON_AIAB_AIBU'
    EXPORTING
      list_asset = lpt_tab_ass
      comp       = lwp_comp
      handle     = 'GET_POSTAB'
    IMPORTING
      postab     = lt_postab.

  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    <fs_data>-waers = 'VND'.
    <fs_data>-as_set_per = 100.
    READ TABLE lt_postab WITH KEY asset = <fs_data>-anln1 INTO DATA(ls_ass_postab).
    IF sy-subrc = 0.
      SELECT SUM( anbtr ) FROM @ls_ass_postab-postab AS lt_postab INTO @<fs_data>-as_amt.
      <fs_data>-as_rule_gr = VALUE #( ls_ass_postab-postab[ 1 ]-bureg OPTIONAL ).
      <fs_data>-stt_rule = COND #( WHEN <fs_data>-as_rule_gr <> '000' THEN '@5B@' )."'ELSE '@5C@'
      <fs_data>-as_set_amt = <fs_data>-as_amt.
    ENDIF.
  ENDLOOP.

  DELETE gt_data WHERE as_amt = 0.
  IF gt_data IS INITIAL.
    MESSAGE 'Không có dữ liệu !' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_IN_AMT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DATA_SEL
*&---------------------------------------------------------------------*
FORM check_before_sett USING lpt_data_sel LIKE gt_data.

  LOOP AT lpt_data_sel INTO DATA(ls_data_sel).

    IF ls_data_sel-stt_rule = '@5B@'.
      MESSAGE e020(zfi) DISPLAY LIKE 'I' WITH ls_data_sel-anln1.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF NOT ( ls_data_sel-as_set_per * ls_data_sel-as_amt_rule = 0 AND ls_data_sel-as_set_per + ls_data_sel-as_amt_rule <> 0 ).
      MESSAGE |{ ls_data_sel-anln1 }: Settlement Percentage và Amount Rule không hợp lệ| TYPE 'E' DISPLAY LIKE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF ( ls_data_sel-as_amt * ls_data_sel-as_set_per / 100 + ls_data_sel-as_amt_rule ) > ls_data_sel-as_amt.
      MESSAGE |{ ls_data_sel-anln1 }: Giá trị tập hợp không được vượt tổng nguyên giá của TS dở dang| TYPE 'E' DISPLAY LIKE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_table .
  DATA(is_stable) = VALUE lvc_s_stbl( row = 'X' col = 'X' ).
  CALL METHOD g_alv->refresh_table_display
    EXPORTING
      is_stable      = is_stable
      i_soft_refresh = 'X'
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    "Display system error messgae
    MESSAGE ID   sy-msgid
            TYPE 'E'
            NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_REQUIRED_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_required_field .

  CASE ''.
    WHEN p_comp.
      MESSAGE 'Company Code is requried !' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    WHEN p_blart.
      MESSAGE 'Document Type is requried !' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDCASE.

  CASE 'X'.
    WHEN p_rd1.
      CASE ''.
        WHEN p_as_c.
          MESSAGE 'Asset Class is requried !' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN p_as_des.
          MESSAGE 'Asset Description is requried !' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN p_as_cc.
          MESSAGE 'Cost Center is requried !' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN p_as_ufl.
          MESSAGE 'Useful Life is requried !' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN p_ev_gr1.
          MESSAGE 'Evaluation Group 1 is requried !' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN p_ev_gr2.
          MESSAGE 'Evaluation Group 2 is requried !' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        WHEN p_ev_gr3.
          MESSAGE 'Evaluation Group 3 is requried !' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
      ENDCASE.
    WHEN p_rd2.
      IF p_as_n IS INITIAL.
        MESSAGE 'Asset Number is requried !' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
  ENDCASE.

  CASE '00000000'.
    WHEN p_budat.
      MESSAGE 'Posting Date is requried !' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    WHEN p_bldat.
      MESSAGE 'Document Date is requried !' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    WHEN p_as_vld.
      MESSAGE 'Document Date is requried !' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDCASE.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HISTORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HISTORY
*&---------------------------------------------------------------------*
FORM set_history  USING lpt_data_sel LIKE gt_data CHANGING lpt_history TYPE tt_his.
  DATA(lw_date_cr) = sy-datum.
  DATA(lw_time_cr) = sy-uzeit.
  LOOP AT lpt_data_sel INTO DATA(ls_data_sel).
    APPEND INITIAL LINE TO lpt_history ASSIGNING FIELD-SYMBOL(<fs_history>).
    <fs_history>-guid = cl_system_uuid=>create_uuid_x16_static( ).
    <fs_history>-comp = p_comp.
    <fs_history>-date_entr = lw_date_cr.
    <fs_history>-time_entr = lw_time_cr.
    <fs_history>-psting_date = p_budat.
    <fs_history>-asset_comp = gw_ass_crd.
    <fs_history>-user_cre = sy-uname.
    <fs_history>-asset_auc = ls_data_sel-anln1.
    <fs_history>-bureg = ls_data_sel-as_rule_gr.
  ENDLOOP.
  INSERT ztb_auc_log FROM TABLE lpt_history .
  COMMIT WORK AND WAIT.

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
  SELECT date_entr , time_entr ,user_cre,comp,asset_auc,bureg,fi_doc,asset_comp FROM ztb_auc_log INTO CORRESPONDING FIELDS OF TABLE @gt_history
  WHERE user_cre IN @s_user AND date_entr IN @s_date.
  IF sy-subrc = 0.
    DATA :lt_fc     TYPE slis_t_fieldcat_alv,
          ls_layout TYPE slis_layout_alv,
          lt_event  TYPE slis_t_event.
    lt_event = VALUE #( ( name = 'USER_COMMAND' form = 'FI_VIEW') ).
    "build fieldcat
    PERFORM buid_fc_his CHANGING lt_fc.
    "build layout
    ls_layout-colwidth_optimize = 'X'.
    "build event

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_title       = 'History'
        i_callback_program = sy-cprog
        is_layout          = ls_layout
        it_fieldcat        = lt_fc
        it_events          = lt_event
      TABLES
        t_outtab           = gt_history
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
  ELSE.
    MESSAGE 'No data found' TYPE 'I'.
  ENDIF.

ENDFORM.
FORM fi_view USING ucomm LIKE sy-ucomm selfied TYPE slis_selfield.
  IF ucomm = '&IC1'.
    DATA(ls_data) = gt_history[ selfied-tabindex ].
    CHECK selfied-value IS NOT INITIAL.
    CASE selfied-fieldname.
      WHEN 'FI_DOC'.
        SET PARAMETER ID 'BLN' FIELD ls_data-fi_doc.
        SET PARAMETER ID 'BUK' FIELD ls_data-comp.
        SET PARAMETER ID 'GJR' FIELD ls_data-psting_date+0(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      WHEN 'ASSET_COMP' OR 'ASSET_AUC'.
        DATA :lw_asset TYPE bf_anln1.
        lw_asset = selfied-value.
        PERFORM call_as03 USING lw_asset ls_data-comp.",
    ENDCASE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUID_FC_HIS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FC
*&---------------------------------------------------------------------*
FORM buid_fc_his  CHANGING lpt_fc_his TYPE slis_t_fieldcat_alv.

  FIELD-SYMBOLS :<fs_fcat> LIKE LINE OF lpt_fc_his.

  DEFINE build_fc_his.
    APPEND INITIAL LINE TO lpt_fc_his ASSIGNING <fs_fcat>.
   <fs_fcat>-col_pos = &1.
   <fs_fcat>-fieldname = &2.
   <fs_fcat>-seltext_l = &3.
   <fs_fcat>-key = &4.
   <fs_fcat>-hotspot = &5.
   <fs_fcat>-no_zero = &6.
  END-OF-DEFINITION.
  build_fc_his :
       '1' 'DATE_ENTR' 'Date Entry' 'X' '' '',
       '2' 'TIME_ENTR' 'Time Entry' 'X' '' '',
       '3' 'USER_CRE' 'User Created' '' '' '',
       '4' 'ASSET_COMP' 'Completed Asset' '' 'X' 'X',
       '5' 'COMP' 'Company Code' '' '' '',
       '6' 'ASSET_AUC' 'Asset Auc' '' 'X' 'X',
       '7' 'BUREG' 'Distribution Rule Group' '' '' '',
       '8' 'FI_DOC' 'Accounting Document Number' '' 'X' 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_EXCLUDE
*&---------------------------------------------------------------------*
FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions.
  DATA ls_exclude TYPE ui_func.

* Row manipulation
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_select_all.
  APPEND ls_exclude TO pt_exclude.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_ASSET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM view_asset .
  CHECK gw_ass_crd IS NOT INITIAL.
  PERFORM call_as03 USING gw_ass_crd p_comp.",
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_AS03
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GW_ASS_CRD
*&      --> P_COMP
*&---------------------------------------------------------------------*
FORM call_as03  USING  lpw_ass_crd TYPE bf_anln1
  lpw_comp TYPE bukrs.
  SET PARAMETER ID 'AN1' FIELD lpw_ass_crd.
  SET PARAMETER ID 'BUK' FIELD lpw_comp.
  CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.

ENDFORM.
