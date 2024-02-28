*&---------------------------------------------------------------------*
*& Include          ZIN_BLOCK_BUDGET_F01
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
  PERFORM get_content_var.
  PERFORM get_data.
  PERFORM process.

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
  p_period = |{ p_period ALPHA = IN }|.
  SELECT
       rldnr,
       rfundsctr,
       rcmmtitem,
       rmeasure,
       fc_name,
       ci_name,
       fm_name,
       SUM( consumable_budget ) AS consumable_budget,
       SUM( consumed_amount ) AS consumed_amount,
       SUM( available_amount ) AS available_amount,
       SUM( available_amount ) AS blocked_amount,
       'VND' AS waers
  FROM zv_fmavct( p_period = @p_period , p_year = @p_year ,p_budcat = @p_budcat ) INTO CORRESPONDING FIELDS OF TABLE @gt_data
  WHERE rcmmtitem IN @s_c_itm AND rfundsctr IN @s_fun_c AND rmeasure IN @s_fun_p AND  parent_st IN @s_supfc AND zfm_busline IN @s_bsline
  GROUP BY   rldnr,
             rfundsctr,
             rcmmtitem,
             rmeasure,
             fc_name,
             fm_name,
             ci_name.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FIELD_CAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_fieldcat USING l_pos l_field l_text l_edit l_tech l_no_out l_curr.
  APPEND INITIAL LINE TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
  <fs_fieldcat>-col_pos = l_pos.
  <fs_fieldcat>-fieldname = l_field.
  <fs_fieldcat>-coltext = l_text.
  <fs_fieldcat>-edit = l_edit.
  <fs_fieldcat>-tech = l_tech.
  <fs_fieldcat>-no_out = l_no_out.
  <fs_fieldcat>-cfieldname = l_curr.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_alv .
  DATA:ls_layout_src TYPE lvc_s_layo,
       ls_layout     TYPE disvariant.
  IF g_container IS BOUND.
    g_container->free( ).
  ENDIF.
  g_container = NEW cl_gui_custom_container( container_name = 'CONTAINER' ).

  g_alv = NEW cl_gui_alv_grid( i_parent = g_container ).
  " Set table for first display
  ls_layout-report = sy-repid.
  ls_layout_src-cwidth_opt = 'X'.
  ls_layout_src-sel_mode = 'A'.

  CALL METHOD g_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = ls_layout_src
      is_variant                    = ls_layout
      i_save                        = 'X'
*     i_structure_name              = p_structure_name
    CHANGING
      it_outtab                     = gt_data
      it_fieldcatalog               = gt_fieldcat
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
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .
  IF gt_data IS NOT INITIAL.
    PERFORM build_fieldcat USING:
                                   '1' 'RFUNDSCTR' 'Funds Center' '' '' '' '',
                                   '2' 'FC_NAME' 'FC Name' '' '' '' '',
                                   '3' 'RCMMTITEM' 'Commitment Item' '' '' '' '',
                                   '4' 'CI_NAME' 'CI Name' '' '' '' '',
                                   '5' 'RMEASURE' 'Fund Program' '' '' '' '',
                                   '6' 'FM_NAME' 'Fund Program Name' '' '' '' '',
                                   '7' 'CONSUMABLE_BUDGET' 'Comsumable Budget' '' '' '' 'WAERS',
                                   '8' 'CONSUMED_AMOUNT' 'Consumed Amount' '' '' '' 'WAERS',
                                   '9' 'AVAILABLE_AMOUNT' 'Available Amount' '' '' '' 'WAERS',
                                   '10' 'BLOCKED_AMOUNT' 'Blocked Amount' 'X' '' '' 'WAERS',
                                   '11' 'WAERS' '' '' 'X' 'X' ''.

    CALL SCREEN 0100.
  ELSE.
    MESSAGE 'No data !' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SY_UCOMM
*&---------------------------------------------------------------------*
FORM execute_block USING p_command.
  DATA :ls_header   TYPE fmr_interface_head,
        lt_detail   TYPE TABLE OF fmr_interface_det,
        lt_extend   TYPE TABLE OF  zfmr_interface_det,
        lt_mess     TYPE tsmesg,
        lt_mess_dis TYPE TABLE OF bapiret2,
        ls_mess_dis TYPE bapiret2.


  DATA :ls_log_h TYPE ztb_budget_log_h,
        ls_log_i TYPE ztb_budget_log_i,
        lt_log_i TYPE TABLE OF ztb_budget_log_i.

  IF sy-batch = ''.
    g_alv->check_changed_data( ).
    g_alv->refresh_table_display( ).
  ENDIF.


  ls_header-bltyp = '020'.
  ls_header-bldat = p_budat.
  ls_header-budat = p_budat.
  ls_header-bukrs = p_area.
  ls_header-waers = 'VND'.
  ls_header-blart = 'Z2'.

  LOOP AT gt_data INTO DATA(ls_data) WHERE blocked_amount > '0.00'.
    APPEND INITIAL LINE TO lt_detail ASSIGNING FIELD-SYMBOL(<fs_detail>).
    <fs_detail>-fistl = ls_data-rfundsctr.
    <fs_detail>-fipos = ls_data-rcmmtitem.
    <fs_detail>-wrbtr = ls_data-blocked_amount.
    <fs_detail>-blpos = sy-tabix.
    APPEND INITIAL LINE TO lt_extend ASSIGNING FIELD-SYMBOL(<fs_extend>).
    <fs_extend>-blpos = <fs_detail>-blpos.
    <fs_extend>-measure = ls_data-rmeasure.
  ENDLOOP.
*  lt_detail = CORRESPONDING #( gt_data MAPPING fistl = rfundsctr fipos = rcmmtitem  wrbtr = blocked_amount ).
*  lt_extend  = CORRESPONDING #( gt_data ).
*  DELETE lt_detail WHERE wrbtr <= '0.00'.
  CHECK lt_detail IS NOT INITIAL.
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_16 = ls_log_h-guid.
  ls_log_h-nguoi_tao = sy-uname.
  ls_log_h-ngay_tao = sy-datum.
  ls_log_h-tgian_tao = sy-uzeit.
  ls_log_i-guid_h = ls_log_h-guid.
  ls_log_h-type = 'BLOCK'.

  CALL FUNCTION 'ZFMFR_CREATE_FROM_DATA'
    EXPORTING
      i_flg_checkonly     = COND #( WHEN p_command = 'TEST' THEN 'X' ELSE '' )
    TABLES
      t_posdata           = lt_detail
      t_extend            = lt_extend
    CHANGING
      c_f_headdata        = ls_header
    EXCEPTIONS
      doctype_not_allowed = 1
      error_occured       = 2
      error_message       = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    CALL FUNCTION 'MESSAGES_GIVE'
      EXPORTING
        i_zeile      = ' '
        i_incl_title = 'X'
      TABLES
        t_mesg       = lt_mess.
    READ TABLE lt_mess WITH KEY msgty = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ls_log_h-status = 'E'.
      DELETE lt_mess WHERE msgty <> 'E'.
      LOOP AT lt_mess INTO DATA(ls_msg).
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = ls_log_i-guid.

        ls_mess_dis-number = ls_msg-txtnr.
        ls_mess_dis-type = ls_msg-msgty.
        MESSAGE ID ls_msg-arbgb TYPE ls_msg-msgty NUMBER ls_msg-txtnr WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
        INTO ls_mess_dis-message.
        ls_log_i-error = ls_mess_dis-message.
        APPEND ls_mess_dis TO lt_mess_dis.
        APPEND ls_log_i TO lt_log_i.
      ENDLOOP.

    ENDIF.
  ELSE.
    ls_log_i-doc_num = ls_header-belnr.
    ls_log_i-fm_area = p_area.
    ls_log_i-fc_year = p_year.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = ls_log_i-guid.
    APPEND ls_log_i TO lt_log_i.
    ls_log_h-status = 'S'.
    IF p_command = 'TEST'.
      MESSAGE | No error ! | TYPE 'E' DISPLAY LIKE 'S'.
    ENDIF.
  ENDIF.

  IF p_command = 'RUN'.
    IF sy-batch  = 'X'.
      IF valutabl IS NOT INITIAL.
        LOOP AT valutabl INTO DATA(ls_var).
          ls_log_h-variant  = |{ ls_log_h-variant }/{ ls_var-selname }:({ ls_var-low }){ COND #( WHEN ls_var-high IS NOT INITIAL THEN |->({ ls_var-high })| ELSE '' ) }|.
        ENDLOOP.
      ENDIF.
    ENDIF.
    INSERT ztb_budget_log_h FROM ls_log_h.
    INSERT ztb_budget_log_i FROM TABLE lt_log_i.
    COMMIT WORK AND WAIT.
  ENDIF.
  IF sy-batch  = ''.
    IF lt_mess_dis IS NOT INITIAL.
      CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
        TABLES
          it_return = lt_mess_dis.
    ELSE.
      IF ls_header-belnr IS NOT INITIAL.
        MESSAGE | Document :{ ls_header-belnr } | TYPE 'I' DISPLAY LIKE 'S'.
      ENDIF.
    ENDIF.
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
  REFRESH gt_history[].
  CALL SELECTION-SCREEN '1001' STARTING AT 10 10.
  CHECK sy-subrc = 0.
  SELECT nguoi_tao ,
         ngay_tao,
         tgian_tao,
         type,
         status,
         fm_area,
         fc_year,
         doc_num,
         error,
         variant
  FROM ztb_budget_log_h AS h INNER JOIN ztb_budget_log_i AS i ON h~guid = i~guid_h INTO CORRESPONDING FIELDS OF TABLE @gt_history
  WHERE nguoi_tao IN @s_user AND ngay_tao IN @s_date.
  IF sy-subrc = 0.
    DATA :lt_fc_hdr_his  TYPE slis_t_fieldcat_alv,
          ls_layout_slis TYPE slis_layout_alv,
          lt_event       TYPE slis_t_event.
    FIELD-SYMBOLS : <fs_hdr_his> LIKE LINE OF lt_fc_hdr_his.
    "build fieldcat
    DEFINE build_fc.
      APPEND INITIAL LINE TO lt_fc_hdr_his ASSIGNING <fs_hdr_his>.
      <fs_hdr_his>-col_pos = &1.
      <fs_hdr_his>-fieldname = &2.
      <fs_hdr_his>-seltext_m = &3.
      <fs_hdr_his>-seltext_s = &3.
      <fs_hdr_his>-seltext_l = &3.
      <fs_hdr_his>-key = &4.
    END-OF-DEFINITION.


    build_fc 1 'NGUOI_TAO' 'Người tạo' 'X'.
    build_fc 2 'NGAY_TAO' 'Ngày tạo' ''.
    build_fc 3 'TYPE' 'Type' ''.
    build_fc 4 'TGIAN_TAO' 'Thời gian tạo' ''.
    build_fc 5 'STATUS' 'Trạng thái' ''.
    build_fc 5 'FM_AREA' 'FM Area' ''.
    build_fc 6 'FC_YEAR' 'Fiscal Year' ''.
    build_fc 7 'DOC_NUM' 'Số chứng từ' ''.
    build_fc 8 'ERROR' 'Lỗi' ''.
    build_fc 9 'VARIANT' 'Variant' ''.
    "build layout
    ls_layout_slis-colwidth_optimize = 'X'.

    lt_event = VALUE #( ( name = 'USER_COMMAND' form = 'VIEW_DOC') ).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_title       = 'History'
        i_callback_program = sy-cprog
        is_layout          = ls_layout_slis
        it_fieldcat        = lt_fc_hdr_his
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
    MESSAGE 'No history' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.

FORM view_doc USING ucomm LIKE sy-ucomm selfied TYPE slis_selfield.
  IF selfied-fieldname = 'DOC_NUM' AND ucomm = '&IC1'.
    READ TABLE gt_history INDEX selfied-tabindex INTO DATA(ls_history).
    CHECK sy-subrc = 0.
    CASE ls_history-type.
      WHEN 'BLOCK'.
        DATA : bdcdata LIKE TABLE OF bdcdata WITH HEADER LINE.
        CHECK selfied-value IS NOT INITIAL.
        bdcdata-program = 'SAPLFMFR'.
        bdcdata-dynpro = '0511'.
        bdcdata-dynbegin = 'X'.
        APPEND bdcdata.
        CLEAR bdcdata.
        bdcdata-fnam = 'BDC_CURSOR'.
        bdcdata-fval = 'KBLD-BELNR'.
        APPEND bdcdata.
        CLEAR bdcdata.
        bdcdata-fnam = 'BDC_OKCODE'.
        bdcdata-fval = '/00'.
        CLEAR bdcdata.
        APPEND bdcdata.
        bdcdata-fnam = 'KBLD-BELNR'.
        bdcdata-fval = selfied-value.
        APPEND bdcdata.
        CLEAR bdcdata.

        CALL TRANSACTION 'FMW3' USING bdcdata MODE 'E'.
      WHEN 'RETURN'.
        SUBMIT rffmed_print
        WITH   p_fikrs = ls_history-fm_area
        WITH   p_dyear =  ls_history-fc_year
        WITH   p_docnr = ls_history-doc_num AND RETURN.
    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CONTENT_VAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_content_var .
  IF sy-slset IS NOT INITIAL.
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report   = sy-repid
        variant  = sy-slset
      TABLES
        valutab  = valutab
        valutabl = valutabl.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process .
  IF sy-batch = 'X'. "chay job
    CASE 'X'.
      WHEN rd_bl.
        PERFORM execute_block USING 'RUN'.
      WHEN rd_rt.
        PERFORM execute_return USING 'RUN'.
    ENDCASE.

  ELSE.
    PERFORM display_data.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_RETURN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SY_UCOMM
*&---------------------------------------------------------------------*
FORM execute_return USING p_command.


  DATA :ls_header   TYPE bapi_0050_header,
        lw_fmarea   TYPE bapi_0050_fields-fm_area,
        lw_doc_year TYPE bapi_0050_fields-doc_year,
        lw_doc_num  TYPE bapi_0050_fields-document.
  DATA :lt_item_data   TYPE TABLE OF bapi_0050_item,
        lt_period_data TYPE TABLE OF bapi_0050_period,
        lt_return      TYPE TABLE OF bapiret2.
  DATA :ls_log_h    TYPE ztb_budget_log_h,
        ls_log_i    TYPE ztb_budget_log_i,
        lt_log_i    TYPE TABLE OF ztb_budget_log_i,
        lt_mess_dis TYPE TABLE OF bapiret2,
        ls_mess_dis TYPE bapiret2,
        lw_amt.

  ls_header-fm_area = p_area.
  ls_header-version = '0'.
  ls_header-version = '000'.
  ls_header-docdate = p_budat.
  ls_header-doctype = 'ZGPG'.
  ls_header-docstate = 1.
  ls_header-process = 'RETN'.

  LOOP AT gt_data INTO DATA(ls_data) WHERE blocked_amount > '0.00'.
    APPEND INITIAL LINE TO lt_item_data ASSIGNING FIELD-SYMBOL(<fs_item_data>).
*    <fs_detail>-wrbtr = ls_data-blocked_amount.
    <fs_item_data>-item_num = sy-tabix.
    <fs_item_data>-item_num = |{ <fs_item_data>-item_num ALPHA = IN }|.
    <fs_item_data>-fisc_year = p_year.
    <fs_item_data>-budcat = '9F'.
    <fs_item_data>-budtype = 'VTRL'.
    <fs_item_data>-funds_ctr = ls_data-rfundsctr.
    <fs_item_data>-cmmt_item = ls_data-rcmmtitem.
    <fs_item_data>-measure = <fs_item_data>-measure.
    <fs_item_data>-trans_curr = 'VND'.
    <fs_item_data>-valtype = 'R1'.
    APPEND INITIAL LINE TO lt_period_data ASSIGNING FIELD-SYMBOL(<fs_period_data>).
    <fs_period_data>-item_num = <fs_item_data>-item_num.
    <fs_period_data>-budgeting_period = '002'.
*    WRITE ls_data-blocked_amount TO lw_amt CURRENCY 'VND'.
    <fs_period_data>-period_amount = ls_data-blocked_amount * 100.
    CLEAR lw_amt.

  ENDLOOP.
  CHECK lt_item_data IS NOT INITIAL.
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_16 = ls_log_h-guid.
  ls_log_h-nguoi_tao = sy-uname.
  ls_log_h-ngay_tao = sy-datum.
  ls_log_h-tgian_tao = sy-uzeit.
  ls_log_h-type = 'RETURN'.
  ls_log_i-guid_h = ls_log_h-guid.


  CALL FUNCTION 'BAPI_0050_CREATE'
    EXPORTING
      language       = 'E'
      header_data    = ls_header
      testrun        = COND #( WHEN p_command = 'TEST' THEN 'X' ELSE '' )
    IMPORTING
      fmarea         = lw_fmarea
      documentyear   = lw_doc_year
      documentnumber = lw_doc_num
    TABLES
      item_data      = lt_item_data
      period_data    = lt_period_data
      return         = lt_return.

  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc  = 0.
    ls_log_h-status = 'E'.
    LOOP AT lt_return  INTO DATA(ls_return).
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = ls_log_i-guid.
      ls_mess_dis-number = ls_return-number.
      ls_mess_dis-type = ls_return-type.
      ls_mess_dis-message = ls_return-message.
      ls_log_i-error = ls_mess_dis-message.
      APPEND ls_mess_dis TO lt_mess_dis.
      APPEND ls_log_i TO lt_log_i.
    ENDLOOP.
  ELSE.
    ls_log_i-doc_num = lw_doc_num.
    ls_log_i-fm_area = p_area.
    ls_log_i-fc_year = p_year.
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = ls_log_i-guid.
    APPEND ls_log_i TO lt_log_i.
    ls_log_h-status = 'S'.
    IF p_command = 'TEST'.
      MESSAGE | No error ! | TYPE 'E' DISPLAY LIKE 'S'.
    ENDIF.
  ENDIF.

  IF p_command = 'RUN'.
    IF sy-batch  = 'X'.
      IF valutabl IS NOT INITIAL.
        LOOP AT valutabl INTO DATA(ls_var).
          ls_log_h-variant  = |{ ls_log_h-variant }/{ ls_var-selname }:({ ls_var-low }){ COND #( WHEN ls_var-high IS NOT INITIAL THEN |->({ ls_var-high })| ELSE '' ) }|.
        ENDLOOP.
      ENDIF.
    ENDIF.
    INSERT ztb_budget_log_h FROM ls_log_h.
    INSERT ztb_budget_log_i FROM TABLE lt_log_i.
    COMMIT WORK AND WAIT.
  ENDIF.
  IF sy-batch  = ''.
    IF lt_mess_dis IS NOT INITIAL.
      CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
        TABLES
          it_return = lt_mess_dis.
    ELSE.
      IF lw_doc_num IS NOT INITIAL.
        MESSAGE | Document :{ lw_doc_num } | TYPE 'I' DISPLAY LIKE 'S'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
