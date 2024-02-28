*&---------------------------------------------------------------------*
*& Include          ZIN_UPDATE_BP_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_NAME_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_name_file .
  DATA :lt_file TYPE filetable,
        lw_rc   TYPE i.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      file_filter             = 'File excel (*.XLS)|*.XLS*'                 " File Extension Filter String
    CHANGING
      file_table              = lt_file                 " Table Holding Selected Files
      rc                      = lw_rc
  ).
  IF sy-subrc <> 0 OR lw_rc = 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    LEAVE LIST-PROCESSING.
  ELSE.
    p_file = lt_file[ 1 ]-filename.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main .
  PERFORM buid_fcat_log.
  PERFORM get_config.
  CASE 'X'.
    WHEN p_gen.
      PERFORM get_data_form_file USING 'TY_GEN' .
      PERFORM create_fi_gen.
    WHEN p_ven.
      PERFORM get_data_form_file USING 'TY_VEN' .
      PERFORM create_fi_ven.
    WHEN p_cus.
      PERFORM get_data_form_file USING 'TY_CUS' .
      PERFORM create_fi_cus.
  ENDCASE.

  PERFORM view_log.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_FORM_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_form_file USING lpw_type TYPE string.
  DATA :lr_data TYPE REF TO data.
  DATA : lt_type   TYPE truxs_t_text_data.
  FIELD-SYMBOLS:<fs_tab_data> TYPE STANDARD TABLE.
  CREATE DATA lr_data TYPE TABLE OF (lpw_type).
  ASSIGN lr_data->* TO <fs_tab_data>.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
*     I_LINE_HEADER        =
      i_tab_raw_data       = lt_type
      i_filename           = p_file
    TABLES
      i_tab_converted_data = <fs_tab_data>.
  IF sy-subrc <> 0.
    MESSAGE 'Lỗi đọc file' TYPE 'E' DISPLAY LIKE 'S'.
    LEAVE LIST-PROCESSING.
* Implement suitable error handling here
  ENDIF.
  CASE 'X'.
    WHEN p_gen.
      MOVE <fs_tab_data> TO gt_gen.
    WHEN p_ven.
      MOVE <fs_tab_data> TO gt_ven.
    WHEN p_cus.
      MOVE <fs_tab_data> TO gt_cus.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_FI_GEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_fi_gen .
  DATA: lt_cvis_ei_extern_t TYPE cvis_ei_extern_t,
        lt_return           TYPE  bapiretm,
        ls_cvis_ei_extern   TYPE cvis_ei_extern,
        lw_index            TYPE int4,
        lv_name             TYPE char200,
        lt_out_line         TYPE TABLE OF char100,
        lw_guid             TYPE bu_partner_guid_bapi.

  TYPES :BEGIN OF lty_bp_guid,
           line         TYPE int4,
           bpartnerguid TYPE bu_partner_guid,
         END OF lty_bp_guid.
  DATA :lw_err     TYPE char1,
        lt_bp_guid TYPE TABLE OF  lty_bp_guid.
  DATA :ls_tax TYPE bus_ei_bupa_taxnumber.
  DELETE gt_gen INDEX 1.

  DEFINE work_wrap.
    lv_name = &1.
    CALL FUNCTION 'RKD_WORD_WRAP'
      EXPORTING
        textline            = lv_name
        outputlen           = &2
      TABLES
        out_lines           = lt_out_line
      EXCEPTIONS
        outputlen_too_large = 1
        OTHERS              = 2.
  END-OF-DEFINITION.

  LOOP AT gt_gen INTO DATA(ls_gen).
    lw_index = sy-tabix + 1.
    ls_cvis_ei_extern-partner-header-object = 'BusinessPartner'.
    ls_cvis_ei_extern-partner-header-object_task = 'I'.
    lw_guid = cl_system_uuid=>create_uuid_x16_static( ).
    ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid = lw_guid.
    APPEND INITIAL LINE TO lt_bp_guid ASSIGNING FIELD-SYMBOL(<fs_bp_guid>).
    <fs_bp_guid>-bpartnerguid = ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid.
    <fs_bp_guid>-line = sy-tabix.
    ls_cvis_ei_extern-partner-central_data-common-data-bp_control-category = ls_gen-bp_type.
    ls_cvis_ei_extern-partner-central_data-common-data-bp_control-grouping = ls_gen-bp_group.
    IF ls_gen-bp_type = '1'.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-firstname = ls_gen-name1.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-fullname = ls_gen-name1.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-lastname  = ls_gen-name2.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-birthname  = COND #( WHEN ls_gen-name3 <> '' THEN ls_gen-name3
                                                                                        WHEN ls_gen-name4 <> '' THEN ls_gen-name4
                                                                                      ).
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-correspondlanguageiso = 'EN'.
*      IF ls_gen-bp_group = 'E001' OR ls_gen-bp_group = 'E002' OR ls_gen-bp_group = 'I001' .
      READ TABLE gt_cfg_bugrp INTO DATA(ls_cfg_bugrp) WITH KEY bu_group = ls_gen-bp_group.
      IF sy-subrc = 0 AND ls_cfg_bugrp-externind = 'X'.
        IF ls_gen-bp_group = 'E004'.
          PERFORM gen_bp_vtl CHANGING ls_cvis_ei_extern-partner-header-object_instance-bpartner.
        ELSE.
          ls_cvis_ei_extern-partner-header-object_instance-bpartner = |{ ls_gen-bp_cus1 ALPHA = IN }|.
        ENDIF.
      ENDIF.

      IF ls_gen-taxnumxl IS NOT INITIAL AND ls_gen-taxtype IS NOT INITIAL.
        APPEND INITIAL LINE TO  ls_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<fs_tax>).
        <fs_tax>-data_key-taxnumber = ls_gen-taxnumxl.
        <fs_tax>-data_key-taxtype = ls_gen-taxtype.
      ENDIF.
    ELSE.
      IF ls_gen-name2 IS NOT INITIAL.
        work_wrap ls_gen-name2 40.
        IF sy-subrc = 0.
          LOOP AT lt_out_line INTO DATA(ls_out_line).
            CASE sy-tabix.
              WHEN 1.
                ls_cvis_ei_extern-partner-central_data-common-data-bp_organization-name2 = ls_out_line.
              WHEN 2.
                ls_cvis_ei_extern-partner-central_data-common-data-bp_organization-name3 = ls_out_line.
              WHEN 3.
                ls_cvis_ei_extern-partner-central_data-common-data-bp_organization-name4 = ls_out_line.
            ENDCASE.
          ENDLOOP.
          REFRESH: lt_out_line.
        ENDIF.
      ENDIF.

      ls_cvis_ei_extern-partner-central_data-common-data-bp_organization-name1 = ls_gen-name1.

      APPEND INITIAL LINE TO  ls_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_tax>.
      <fs_tax>-data_key-taxnumber = ls_gen-taxnumxl.
      <fs_tax>-data_key-taxtype = ls_gen-taxtype.
      IF ls_gen-bp_cus1 IS NOT INITIAL AND ls_gen-taxnumxl IS INITIAL.
        FREE ls_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers.
      ENDIF.
      ls_cvis_ei_extern-partner-header-object_instance-bpartner = |{ ls_gen-bp_cus1 ALPHA = IN }|.
    ENDIF.
*    ls_cvis_ei_extern-partner-central_data-common-data-
    ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-partnerexternal = ls_gen-bp_cus.
    ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-searchterm1 = ls_gen-search_t.
    ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-title_key = ls_gen-bp_title.
*    ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-partnerexternal = ls_gen-bp_cus.
    APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-address-addresses ASSIGNING FIELD-SYMBOL(<fs_address>).

    IF ls_gen-street_h IS NOT INITIAL.
      work_wrap ls_gen-street_h 60.
      IF sy-subrc IS INITIAL.
        <fs_address>-data-postal-data-street = lt_out_line[ 1 ].

        REPLACE FIRST OCCURRENCE OF lt_out_line[ 1 ] IN ls_gen-street_h WITH space.
        CONDENSE ls_gen-street_h.
        REFRESH lt_out_line.
        IF ls_gen-street_h IS NOT INITIAL.
          work_wrap ls_gen-street_h 40.
          IF sy-subrc = 0.
            LOOP AT lt_out_line INTO ls_out_line.
              CASE sy-tabix.
                WHEN 1.
                  <fs_address>-data-postal-data-str_suppl3 = ls_out_line.
                WHEN 2.
                  <fs_address>-data-postal-data-location = ls_out_line.
                WHEN 3.
                  <fs_address>-data-postal-data-district = ls_out_line.
              ENDCASE.
            ENDLOOP.
            REFRESH: lt_out_line.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    <fs_address>-data-postal-data-country = COND #( WHEN ls_gen-country = '' THEN 'VN' ELSE ls_gen-country ).
    <fs_address>-data-postal-data-langu = 'EN'.


    IF ls_gen-tell <> ''.
      APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phone>).
      <fs_phone>-contact-data-extension = '011'.
      <fs_phone>-contact-data-telephone = ls_gen-tell.
      <fs_phone>-contact-data-r_3_user = '1'.
    ENDIF.

    IF ls_gen-mobi <> ''.
      APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING <fs_phone>.
      <fs_phone>-contact-data-telephone = ls_gen-mobi.
      <fs_phone>-contact-data-r_3_user = '3'.
    ENDIF.


    APPEND INITIAL LINE TO <fs_address>-data-communication-fax-fax ASSIGNING FIELD-SYMBOL(<fs_fax>).
    <fs_fax>-contact-data-country = 'VN'.
    <fs_fax>-contact-data-fax = COND #( WHEN ls_gen-fax IS NOT INITIAL THEN ls_gen-fax ).
    APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_smtp>).
    <fs_smtp>-contact-data-e_mail = COND #( WHEN ls_gen-email IS NOT INITIAL THEN ls_gen-email ).

    IF ls_gen-id_number IS NOT INITIAL.
      APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-ident_number-ident_numbers ASSIGNING FIELD-SYMBOL(<fs_identifi>).
      <fs_identifi>-data_key-identificationnumber = ls_gen-id_number.
      <fs_identifi>-data_key-identificationcategory = ls_gen-id_type.
      <fs_identifi>-data-identrydate = sy-datum.
      <fs_identifi>-data-idvalidfromdate = sy-datum.
      <fs_identifi>-data-idvalidtodate = '99991231'.
      <fs_identifi>-data-country = 'VN'.
    ENDIF.

    APPEND ls_cvis_ei_extern TO lt_cvis_ei_extern_t.
    CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
      EXPORTING
        i_data   = lt_cvis_ei_extern_t
      IMPORTING
        e_return = lt_return.

    PERFORM set_log USING lt_return lw_index '' '' lw_guid.

    CLEAR :ls_cvis_ei_extern,lw_index,lw_guid.
    REFRESH :lt_return,lt_cvis_ei_extern_t.
  ENDLOOP.

*  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
*    EXPORTING
*      i_data   = lt_cvis_ei_extern_t
*    IMPORTING
*      e_return = lt_return.
*
*  LOOP AT lt_return INTO DATA(ls_return).
*    LOOP AT ls_return-object_msg INTO DATA(ls_ob_msg).
*      IF ls_ob_msg-type = 'E' OR  ls_ob_msg-type = 'A'.
*        lw_err = 'X'.
*        APPEND INITIAL LINE TO gt_mess_log ASSIGNING FIELD-SYMBOL(<fs_mess_log>).
*        <fs_mess_log>-line = ls_return-object_idx.
*        <fs_mess_log>-status = '@0A@'.
*        <fs_mess_log>-id = ls_ob_msg-id.
*        <fs_mess_log>-number = ls_ob_msg-number.
*        MESSAGE ID <fs_mess_log>-id TYPE 'E' NUMBER <fs_mess_log>-number INTO <fs_mess_log>-message WITH ls_ob_msg-message_v1 ls_ob_msg-message_v2 ls_ob_msg-message_v3 ls_ob_msg-message_v4.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*  IF lw_err = 'X'.
*    DELETE gt_fcat_log WHERE fieldname = 'BP_CREATED'.
*    ROLLBACK WORK.
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*    DELETE gt_fcat_log WHERE fieldname = 'ID' OR fieldname = 'NUMBER' OR fieldname = 'MESSAGE'.
*    SELECT line,partner FROM but000 AS a INNER JOIN @lt_bp_guid AS b ON a~partner_guid = b~bpartnerguid INTO TABLE @DATA(lt_bp_created).
*    SORT lt_bp_created BY line.
*    LOOP AT lt_bp_created INTO DATA(ls_bp_created).
*      APPEND INITIAL LINE TO gt_mess_log ASSIGNING <fs_mess_log>.
*      <fs_mess_log>-status = '@08@'.
*      <fs_mess_log>-line = ls_bp_created-line.
*      <fs_mess_log>-bp_created = ls_bp_created-partner.
*    ENDLOOP.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_FI_VEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_fi_ven .
  DATA: lt_cvis_ei_extern_t TYPE cvis_ei_extern_t,
        lt_return           TYPE  bapiretm,
        ls_cvis_ei_extern   TYPE cvis_ei_extern,
        ls_cvis_ei_extern1  TYPE cvis_ei_extern,
        lw_index            TYPE int4.

  TYPES :BEGIN OF lty_bp,
           line     TYPE int4,
           bpartner TYPE bu_partner,
         END OF lty_bp.
  DATA :lw_err   TYPE char1,
        lt_bp    TYPE TABLE OF  lty_bp,
        lt_bp_in TYPE TABLE OF lty_bp.

  DELETE gt_ven INDEX 1.
  lt_bp_in = VALUE #( FOR wa IN gt_ven ( bpartner = |{ wa-bp_cus ALPHA = IN }| ) ).
  SELECT partner,partner_guid FROM but000 FOR ALL ENTRIES IN @lt_bp_in
  WHERE partner = @lt_bp_in-bpartner
  INTO TABLE @DATA(lt_map_bp_guid).
  SORT lt_map_bp_guid BY partner.

  SELECT partner,rltyp FROM but100 INTO TABLE @DATA(lt_bp_role) FOR ALL ENTRIES IN @lt_bp_in
  WHERE  partner = @lt_bp_in-bpartner.
  SORT lt_bp_role BY partner rltyp.
*  lt_bp_in = CORRESPONDING #( gt_ven MAPPING bpartner = bp_cus ).
  LOOP AT gt_ven INTO DATA(ls_ven).
    lw_index = sy-tabix + 1.
    ls_cvis_ei_extern-partner-header-object = 'BusinessPartner'.
    ls_cvis_ei_extern-partner-header-object_task = 'U'.
    ls_cvis_ei_extern-partner-header-object_instance-bpartner = |{ ls_ven-bp_cus ALPHA = IN }|.
    READ TABLE lt_map_bp_guid INTO DATA(ls_map_bp_guid) WITH KEY partner =  ls_cvis_ei_extern-partner-header-object_instance-bpartner BINARY SEARCH.
    IF sy-subrc = 0.
      ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid = ls_map_bp_guid-partner_guid.
    ENDIF.
*    APPEND INITIAL LINE TO lt_bp ASSIGNING FIELD-SYMBOL(<fs_bp>).
*    <fs_bp>-line = sy-tabix.
*    <fs_bp>-bpartner = ls_ven-bp_cus.
    READ TABLE lt_bp_role WITH KEY partner = ls_cvis_ei_extern-partner-header-object_instance-bpartner rltyp = 'FLVN00' BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-role-roles ASSIGNING FIELD-SYMBOL(<fs_role>).
      <fs_role>-task =  'I'.
      <fs_role>-data_key = 'FLVN00'.
      <fs_role>-data-rolecategory = 'FLVN00'.
      ls_cvis_ei_extern-vendor-header-object_task = 'I'.
    ELSE.
      ls_cvis_ei_extern-vendor-header-object_task = 'U'.
    ENDIF.


    APPEND INITIAL LINE TO ls_cvis_ei_extern-vendor-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company_ven>).

    <fs_company_ven>-data_key-bukrs = ls_ven-v_company.
    <fs_company_ven>-task = 'M'.
    <fs_company_ven>-data-akont = ls_ven-v_acc.
    <fs_company_ven>-datax-akont = 'X'.

    ls_cvis_ei_extern-vendor-central_data-address-postal-data-county = 'VN'.
    ls_cvis_ei_extern-vendor-central_data-address-postal-datax-county = 'X'.
    IF ls_ven-v_head <> ''.
      <fs_company_ven>-data-lnrze =  ls_ven-v_head.
      <fs_company_ven>-datax-lnrze =  'X'.
    ENDIF.
    IF ls_ven-v_pterm <> ''.
      <fs_company_ven>-data-zterm =  ls_ven-v_pterm.
      <fs_company_ven>-datax-zterm =  'X'.
    ENDIF.
    IF ls_ven-v_pmethod <> ''.
      <fs_company_ven>-data-zwels =  ls_ven-v_pmethod.
      <fs_company_ven>-datax-zwels =  'X'.
    ENDIF.
    IF ls_ven-v_hb <> ''.
      <fs_company_ven>-data-hbkid = ls_ven-v_hb.
      <fs_company_ven>-datax-hbkid = 'X'.
    ENDIF.
    IF ls_ven-v_pterm_block <> ''.
      <fs_company_ven>-data-zahls =  ls_ven-v_pterm_block.
      <fs_company_ven>-datax-zahls = 'X'.
    ENDIF.
    IF  ls_ven-v_zpterm <> ''.
      <fs_company_ven>-data-guzte = ls_ven-v_zpterm.
      <fs_company_ven>-datax-guzte = 'X'.
    ENDIF.
    IF ls_ven-v_clear <> ''.
      <fs_company_ven>-data-xverr =  ls_ven-v_clear.
      <fs_company_ven>-datax-xverr =  'X'.

      ls_cvis_ei_extern-vendor-central_data-central-data-kunnr = ls_cvis_ei_extern-partner-header-object_instance-bpartner.
      ls_cvis_ei_extern-vendor-central_data-central-datax-kunnr = 'X'.
    ENDIF.
    IF  ls_ven-v_alter <> ''.
      <fs_company_ven>-data-xlfzb = 'X'.
      <fs_company_ven>-datax-xlfzb = 'X'.
      ls_cvis_ei_extern-vendor-central_data-central-data-xzemp = 'X'.
      ls_cvis_ei_extern-vendor-central_data-central-datax-xzemp = 'X'.
    ENDIF.

    IF  ls_ven-reprf <> ''.
      <fs_company_ven>-data-reprf = ls_ven-reprf.
      <fs_company_ven>-datax-reprf = 'X'.
    ENDIF.
*    ls_cvis_ei_extern-vendor-company_data-current_state = 'X'.

    APPEND ls_cvis_ei_extern TO lt_cvis_ei_extern_t.
    CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
      EXPORTING
        i_data   = lt_cvis_ei_extern_t
      IMPORTING
        e_return = lt_return.

    PERFORM set_log USING lt_return lw_index ls_ven-bp_cus 'Vendor' ''.

    CLEAR :ls_cvis_ei_extern,lw_index.
    REFRESH :lt_return,lt_cvis_ei_extern_t.
  ENDLOOP.
*  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
*    EXPORTING
*      i_data   = lt_cvis_ei_extern_t
*    IMPORTING
*      e_return = lt_return.
*
*  LOOP AT lt_return INTO DATA(ls_return).
*    SORT ls_return-object_msg BY number id.
*    DELETE ADJACENT DUPLICATES FROM ls_return-object_msg COMPARING number id.
*    LOOP AT ls_return-object_msg INTO DATA(ls_ob_msg).
*      IF ls_ob_msg-type = 'E' OR  ls_ob_msg-type = 'A'.
*        lw_err = 'X'.
*        APPEND INITIAL LINE TO gt_mess_log ASSIGNING FIELD-SYMBOL(<fs_mess_log>).
*        <fs_mess_log>-line = ls_return-object_idx.
*        <fs_mess_log>-status = '@0A@'.
*        <fs_mess_log>-id = ls_ob_msg-id.
*        <fs_mess_log>-number = ls_ob_msg-number.
*        MESSAGE ID <fs_mess_log>-id TYPE 'E' NUMBER <fs_mess_log>-number INTO <fs_mess_log>-message WITH ls_ob_msg-message_v1 ls_ob_msg-message_v2 ls_ob_msg-message_v3 ls_ob_msg-message_v4.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*  IF lw_err = 'X'.
*    DELETE gt_fcat_log WHERE fieldname = 'BP_CREATED'.
*    ROLLBACK WORK.
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*    DELETE gt_fcat_log WHERE fieldname = 'ID' OR fieldname = 'NUMBER' OR fieldname = 'BP_CREATED'.
*    SORT lt_bp BY line.
*
*    LOOP AT lt_bp INTO DATA(ls_bp).
*      APPEND INITIAL LINE TO gt_mess_log ASSIGNING <fs_mess_log>.
*      <fs_mess_log>-status = '@08@'.
*      <fs_mess_log>-line = ls_bp-line.
*      <fs_mess_log>-message = |Create role vendor sucess for BP :{ ls_bp-bpartner }|.
*    ENDLOOP.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_FI_CUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_fi_cus .

  DATA: lt_cvis_ei_extern_t TYPE cvis_ei_extern_t,
        lt_return           TYPE  bapiretm,
        ls_cvis_ei_extern   TYPE cvis_ei_extern,
        ls_cvis_ei_extern1  TYPE cvis_ei_extern,
        lw_index            TYPE int4.

  TYPES :BEGIN OF lty_bp,
           line     TYPE int4,
           bpartner TYPE bu_partner,
         END OF lty_bp.
  DATA :lw_err   TYPE char1,
        lt_bp    TYPE TABLE OF  lty_bp,
        lt_bp_in TYPE TABLE OF lty_bp.

  DELETE gt_cus INDEX 1.
  lt_bp_in = VALUE #( FOR wa IN gt_cus ( bpartner = |{ wa-bp_cus ALPHA = IN }| ) ).
  SELECT partner,partner_guid FROM but000 FOR ALL ENTRIES IN @lt_bp_in
  WHERE partner = @lt_bp_in-bpartner
  INTO TABLE @DATA(lt_map_bp_guid).
  SORT lt_map_bp_guid BY partner.

  SELECT partner,rltyp FROM but100 INTO TABLE @DATA(lt_bp_role) FOR ALL ENTRIES IN @lt_bp_in
  WHERE  partner = @lt_bp_in-bpartner.
  SORT lt_bp_role BY partner rltyp.
*  lt_bp_in = CORRESPONDING #( gt_cusMAPPING bpartner = bp_cus ).
  LOOP AT gt_cus INTO DATA(ls_cus).
    lw_index = sy-tabix + 1.
    ls_cvis_ei_extern-partner-header-object = 'BusinessPartner'.
    ls_cvis_ei_extern-partner-header-object_task = 'U'.
    ls_cvis_ei_extern-partner-header-object_instance-bpartner = |{ ls_cus-bp_cus ALPHA = IN }|.
    READ TABLE lt_map_bp_guid INTO DATA(ls_map_bp_guid) WITH KEY partner =  ls_cvis_ei_extern-partner-header-object_instance-bpartner BINARY SEARCH.
    IF sy-subrc = 0.
      ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid = ls_map_bp_guid-partner_guid.
    ENDIF.
*    APPEND INITIAL LINE TO lt_bp ASSIGNING FIELD-SYMBOL(<fs_bp>).
*    <fs_bp>-line = sy-tabix.
*    <fs_bp>-bpartner = ls_cus-bp_cus.
    READ TABLE lt_bp_role WITH KEY partner = ls_cvis_ei_extern-partner-header-object_instance-bpartner rltyp = 'FLCU00' BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO ls_cvis_ei_extern-partner-central_data-role-roles ASSIGNING FIELD-SYMBOL(<fs_role>).
      <fs_role>-task =  'I'.
      <fs_role>-data_key = 'FLCU00'.
      <fs_role>-data-rolecategory = 'FLCU00'.
      ls_cvis_ei_extern-customer-header-object_task = 'I'.
    ELSE.
      ls_cvis_ei_extern-customer-header-object_task = 'U'.
    ENDIF.


    APPEND INITIAL LINE TO ls_cvis_ei_extern-customer-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company_cus>).

    <fs_company_cus>-data_key-bukrs = ls_cus-c_company.
    <fs_company_cus>-task = 'M'.
    <fs_company_cus>-data-akont = ls_cus-c_acc.
    <fs_company_cus>-datax-akont = 'X'.

    ls_cvis_ei_extern-customer-central_data-address-postal-data-county = 'VN'.
    ls_cvis_ei_extern-customer-central_data-address-postal-datax-county = 'X'.
    IF ls_cus-c_head <> ''.
      <fs_company_cus>-data-knrze =  ls_cus-c_head.
      <fs_company_cus>-datax-knrze =  'X'.
    ENDIF.
    IF ls_cus-c_pterm <> ''.
      <fs_company_cus>-data-zterm =  ls_cus-c_pterm.
      <fs_company_cus>-datax-zterm =  'X'.
    ENDIF.
    IF ls_cus-c_pmethod <> ''.
      <fs_company_cus>-data-zwels =  ls_cus-c_pmethod.
      <fs_company_cus>-datax-zwels =  'X'.
    ENDIF.
    IF ls_cus-c_hb <> ''.
      <fs_company_cus>-data-hbkid = ls_cus-c_hb.
      <fs_company_cus>-datax-hbkid = 'X'.
    ENDIF.
    IF ls_cus-c_pterm_block <> ''.
      <fs_company_cus>-data-zahls =  ls_cus-c_pterm_block.
      <fs_company_cus>-datax-zahls = 'X'.
    ENDIF.
    IF  ls_cus-c_zpterm <> ''.
      <fs_company_cus>-data-guzte = ls_cus-c_zpterm.
      <fs_company_cus>-datax-guzte = 'X'.
    ENDIF.
    IF ls_cus-c_clear <> ''.
      <fs_company_cus>-data-xverr =  ls_cus-c_clear.
      <fs_company_cus>-datax-xverr =  'X'.

      ls_cvis_ei_extern-customer-central_data-central-data-lifnr = ls_cvis_ei_extern-partner-header-object_instance-bpartner.
      ls_cvis_ei_extern-customer-central_data-central-datax-lifnr = 'X'.
    ENDIF.
    IF  ls_cus-c_alter <> ''.
      <fs_company_cus>-data-xknzb = 'X'.
      <fs_company_cus>-datax-xknzb = 'X'.
    ENDIF.
*    ls_cvis_ei_extern-CUSTOMER-company_data-current_state = 'X'.

    APPEND ls_cvis_ei_extern TO lt_cvis_ei_extern_t.
    CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
      EXPORTING
        i_data   = lt_cvis_ei_extern_t
      IMPORTING
        e_return = lt_return.

    PERFORM set_log USING lt_return lw_index ls_cus-bp_cus 'Customer' ''.
    CLEAR :ls_cvis_ei_extern,lw_index.
    REFRESH :lt_return,lt_cvis_ei_extern_t.

  ENDLOOP.
*  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
*    EXPORTING
*      i_data   = lt_cvis_ei_extern_t
*    IMPORTING
*      e_return = lt_return.
*
*  LOOP AT lt_return INTO DATA(ls_return).
*    SORT ls_return-object_msg BY number id.
*    DELETE ADJACENT DUPLICATES FROM ls_return-object_msg COMPARING number id.
*    LOOP AT ls_return-object_msg INTO DATA(ls_ob_msg).
*      IF ls_ob_msg-type = 'E' OR  ls_ob_msg-type = 'A'.
*        lw_err = 'X'.
*        APPEND INITIAL LINE TO gt_mess_log ASSIGNING FIELD-SYMBOL(<fs_mess_log>).
*        <fs_mess_log>-line = ls_return-object_idx.
*        <fs_mess_log>-status = '@0A@'.
*        <fs_mess_log>-id = ls_ob_msg-id.
*        <fs_mess_log>-number = ls_ob_msg-number.
*        MESSAGE ID <fs_mess_log>-id TYPE 'E' NUMBER <fs_mess_log>-number INTO <fs_mess_log>-message WITH ls_ob_msg-message_v1 ls_ob_msg-message_v2 ls_ob_msg-message_v3 ls_ob_msg-message_v4.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*  IF lw_err = 'X'.
*    DELETE gt_fcat_log WHERE fieldname = 'BP_CREATED'.
*    ROLLBACK WORK.
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*    DELETE gt_fcat_log WHERE fieldname = 'ID' OR fieldname = 'NUMBER' OR fieldname = 'BP_CREATED'.
*    SORT lt_bp BY line.
*
*    LOOP AT lt_bp INTO DATA(ls_bp).
*      APPEND INITIAL LINE TO gt_mess_log ASSIGNING <fs_mess_log>.
*      <fs_mess_log>-status = '@08@'.
*      <fs_mess_log>-line = ls_bp-line.
*      <fs_mess_log>-message = |Create role customer sucess for BP :{ ls_bp-bpartner }|.
*    ENDLOOP.
*  ENDIF.
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

  set_fcat_log :
               'STATUS' 'GT_MESS_LOG' 'Status' 'X',
               'LINE' 'GT_MESS_LOG' 'Line' '',
               'ID' 'GT_MESS_LOG' 'Message class' '',
               'NUMBER' 'GT_MESS_LOG' 'Number message' '',
               'MESSAGE' 'GT_MESS_LOG' 'Message text' '',
               'BP_CREATED' 'GT_MESS_LOG' 'BP created' '',
               'BP' 'GT_MESS_LOG' 'BP' ''.
*               'MESSAGE_V1' 'GT_MESS_LOG' 'Message V1' '',
*               'MESSAGE_V2' 'GT_MESS_LOG' 'Message V2' '',
*               'MESSAGE_V3' 'GT_MESS_LOG' 'Message V3' '',
*               'MESSAGE_V4' 'GT_MESS_LOG' 'Message V4' ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM view_log .
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
*& Form DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM download_template .
*  DATA: lw_viewer_suppress TYPE xmark,
*        lw_fullpath        TYPE string,
*        lw_filename        TYPE string,
*        lw_path            TYPE string,
*        lw_filter          TYPE string,
*        lw_user_action     TYPE i.
*  DATA: lw_string      TYPE string,
*        iv_context_ref TYPE string.
*
*  lw_filter = '(*.XLSX)|*.XLSX|'.
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
**     window_title         = i_window_title
*      with_encoding        = 'X'
**     initial_directory    = i_initial_directory
*      default_file_name    = COND #( WHEN p_gen = 'X' THEN 'BP_TEMP'
*                                     WHEN p_ven = 'X' THEN 'VENDOR_TEMP'
*                                     WHEN p_cus = 'X' THEN 'CUSTOMER_TEMP'
*                                   )
*      file_filter          = lw_filter
*    CHANGING
*      filename             = lw_filename
*      path                 = lw_path
*      fullpath             = lw_fullpath
*      user_action          = lw_user_action
*    EXCEPTIONS
*      cntl_error           = 1
*      error_no_gui         = 2
*      not_supported_by_gui = 3
*      OTHERS               = 4.
*  IF sy-subrc <> 0.
*    RAISE error.
*    RETURN.
*  ENDIF.
*  IF lw_user_action <> cl_gui_frontend_services=>action_ok.
*    RETURN.
*  ENDIF.
  DATA :lw_formname TYPE string.
  lw_formname = COND #(   WHEN p_gen = 'X' THEN 'ZXLS_BP'
                          WHEN p_ven = 'X' THEN 'ZXLS_VEN'
                          WHEN p_cus = 'X' THEN 'ZXLS_CUS'
                      ).

  CALL FUNCTION 'ZXLWB_WORKBENCH'
    EXPORTING
      iv_formname        = lw_formname
      iv_action          = 'EXPORT'
    EXCEPTIONS
      process_terminated = 1
      OTHERS             = 2.
  IF sy-subrc NE 0 .
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
  ENDIF .
*  CALL FUNCTION 'ZXLWB_CALLFORM'
*    EXPORTING
*      iv_formname        = COND #(   WHEN p_gen = 'X' THEN 'ZXLS_BP'
*                                     WHEN p_ven = 'X' THEN 'ZXLS_VEN'
*                                     WHEN p_cus = 'X' THEN 'ZXLS_CUS'
*                                   )
*      iv_context_ref     = iv_context_ref
*      iv_viewer_suppress = 'X'
*      iv_save_as         = lw_fullpath
*    EXCEPTIONS
*      OTHERS             = 2.
*  IF sy-subrc NE 0 .
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
*  ENDIF .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CONFIG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_config .

  SELECT
    tb001~bu_group,
    nriv~externind
  FROM
    tb001
  INNER JOIN nriv ON tb001~nrrng = nriv~nrrangenr AND nriv~object = 'BU_PARTNER'
  INTO TABLE @gt_cfg_bugrp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_RETURN
*&      --> LW_INDEX
*&      --> LS_VEN_BP_CUS
*&      --> P_
*&---------------------------------------------------------------------*
FORM set_log  USING   lpt_return TYPE bapiretm
                      lpw_index TYPE int4
                      lpw_bp  TYPE char20
                      lpw_type TYPE string
                      lpw_guid TYPE bu_partner_guid_bapi.
  DATA :lw_err TYPE char1.

  IF p_gen = 'X'.
    DELETE gt_fcat_log WHERE fieldname = 'BP'.
  ELSE.
    DELETE gt_fcat_log WHERE fieldname = 'BP_CREATED'.
  ENDIF.
  LOOP AT lpt_return INTO DATA(ls_return).
    SORT ls_return-object_msg BY number id.
    DELETE ADJACENT DUPLICATES FROM ls_return-object_msg COMPARING number id.
    LOOP AT ls_return-object_msg INTO DATA(ls_ob_msg).
      IF ls_ob_msg-type = 'E' OR  ls_ob_msg-type = 'A'.
        lw_err = 'X'.
        APPEND INITIAL LINE TO gt_mess_log ASSIGNING FIELD-SYMBOL(<fs_mess_log>).
        <fs_mess_log>-line = lpw_index.
        <fs_mess_log>-status = '@0A@'.
        <fs_mess_log>-id = ls_ob_msg-id.
        <fs_mess_log>-number = ls_ob_msg-number.
        <fs_mess_log>-bp = lpw_bp.
        MESSAGE ID <fs_mess_log>-id TYPE 'E' NUMBER <fs_mess_log>-number INTO <fs_mess_log>-message WITH ls_ob_msg-message_v1 ls_ob_msg-message_v2 ls_ob_msg-message_v3 ls_ob_msg-message_v4.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  IF lw_err = 'X'.
    ROLLBACK WORK.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    APPEND INITIAL LINE TO gt_mess_log ASSIGNING <fs_mess_log>.
    CASE 'X'.
      WHEN p_gen.
        SELECT SINGLE partner FROM but000 WHERE partner_guid = @lpw_guid INTO @DATA(lw_partner).
        <fs_mess_log>-status = '@08@'.
        <fs_mess_log>-line = lpw_index.
        <fs_mess_log>-bp_created = lw_partner.
      WHEN OTHERS.
        <fs_mess_log>-status = '@08@'.
        <fs_mess_log>-line = lpw_index.
        <fs_mess_log>-message = |Create role { lpw_type } sucess for BP :{ lpw_bp ALPHA = OUT }|.
        <fs_mess_log>-bp = lpw_bp.
    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GEN_BP_VTL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gen_bp_vtl CHANGING lpw_partner TYPE bu_partner.
  DATA :lv_number(7) TYPE n.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZBP_VTL'
    IMPORTING
      number                  = lv_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc = 0.
    lpw_partner = |VTL{ lv_number ALPHA = IN }|.
  ENDIF.
ENDFORM.
