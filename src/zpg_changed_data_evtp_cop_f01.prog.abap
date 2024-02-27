*&---------------------------------------------------------------------*
*& Include          ZPG_CHANGED_DATA_EVTP_F01
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
  PERFORM get_file_from_folder.
  CALL SCREEN 0100.
*  PERFORMFORM setup_salv.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FILE_FROM_FOLDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_file_from_folder .
  DATA :lw_folder           TYPE eps2filnam,
        lt_data_4_build_tmp LIKE gt_data_4_build.
  lw_folder = p_path.
  CALL FUNCTION 'ZEPS2_GET_DIRECTORY_LISTING'
    EXPORTING
      iv_dir_name            = lw_folder
*     FILE_MASK              = gc_filemask
    TABLES
      dir_list               = gt_file_list
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.

  LOOP AT gt_file_list INTO DATA(ls_file_list).
    APPEND INITIAL LINE TO gt_data_4_build ASSIGNING FIELD-SYMBOL(<fs_data_4_build>).
    <fs_data_4_build>-parent = p_path.
    <fs_data_4_build>-name = ls_file_list-name.
    CONVERT TIME STAMP ls_file_list-time TIME ZONE 'UTC+7' INTO DATE <fs_data_4_build>-date TIME <fs_data_4_build>-time.
  ENDLOOP.
  lt_data_4_build_tmp = VALUE #( FOR wa IN gt_data_4_build WHERE ( date IN s_date AND time IN s_time AND name IN s_file ) ( wa ) ).
  REFRESH gt_data_4_build.
  gt_data_4_build[] = lt_data_4_build_tmp[].
  REFRESH lt_data_4_build_tmp.
*  SELECT * FROM @gt_data_4_build  as data_buid INTO TABLE @DATA(lt_data_buid_tmp) WHERE date in @s_date AND time in @s_time.
  CHECK gt_data_4_build IS INITIAL.
  MESSAGE 'Không tìm được file !' TYPE 'S' DISPLAY LIKE 'E'.
  LEAVE LIST-PROCESSING .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUID_TREE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM buid_tree_data .


ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_header CHANGING lps_hier_hdr TYPE treev_hhdr..
  lps_hier_hdr-heading = 'Find changed data'.
  lps_hier_hdr-width = 50.
  lps_hier_hdr-width_pix = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETUP_TREE_SALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM setup_tree .
  REFRESH :gt_fcat,gt_data_tree.
  DATA: ls_hier_hdr   TYPE treev_hhdr,
        ls_layout_src TYPE lvc_s_layo.
  CHECK go_alv IS INITIAL.


  PERFORM build_header CHANGING ls_hier_hdr.
  PERFORM init_alv.
  ls_layout_src-cwidth_opt = 'X'.


  "display
  go_alv->set_table_for_first_display(
     EXPORTING
*      is_layout            = ls_layout_src
      is_hierarchy_header  = ls_hier_hdr
    CHANGING
      it_outtab            =  gt_data_tree
      it_fieldcatalog      =  gt_fcat
  ).


  PERFORM create_data_hier.
  CALL METHOD go_alv->update_calculations.
  CALL METHOD go_alv->frontend_update.
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
*& Form INIT_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_alv .

  DATA: lobj_stdesc TYPE REF TO cl_abap_structdescr,
        lv_stname   TYPE dd02l-tabname,
        lt_dfies    TYPE ddfields.
  DATA: lt_events TYPE cntl_simple_events.
*  DATA: lt_events TYPE cntl_simple_events.
  DATA(lo_handler) = NEW lcl_event_handler( ).
  IF go_cont IS BOUND.
    go_cont->free( ).
  ENDIF.

  go_cont = NEW cl_gui_custom_container( container_name = 'CONTAINER' ).
  go_alv = NEW cl_gui_alv_tree(  parent =  go_cont node_selection_mode = cl_gui_column_tree=>node_sel_mode_single item_selection = 'X' no_html_header = 'X' no_toolbar = '' ).
  "buid fcat
  TRY.
      lobj_stdesc ?= cl_abap_structdescr=>describe_by_data( gs_data_tree ).
    CATCH cx_root.
      RAISE no_field_catalog.
  ENDTRY.
  lt_dfies =  cl_salv_data_descr=>read_structdescr( lobj_stdesc ).
*gt_fcat = CORRESPONDING #( lt_dfies ).
  LOOP AT lt_dfies INTO DATA(ls_dfies).
    APPEND INITIAL LINE TO gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    MOVE-CORRESPONDING ls_dfies TO <fs_fcat>.
    <fs_fcat>-outputlen = '20'.
    <fs_fcat>-no_zero = 'X'.
  ENDLOOP.

  CALL METHOD go_alv->get_registered_events
    IMPORTING
      events = lt_events.

  lt_events = VALUE #( BASE lt_events ( eventid = cl_gui_list_tree=>eventid_node_double_click appl_event = 'X' ) ).
  CALL METHOD go_alv->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  SET HANDLER lo_handler->handler_node_click FOR go_alv.

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
*  SORT gt_data_4_build by
  DATA: l_parr_key TYPE lvc_nkey.
  LOOP AT gt_data_4_build INTO DATA(ls_data_4_build).
    ON CHANGE OF ls_data_4_build-parent.
      PERFORM add_header USING ls_data_4_build CHANGING l_parr_key.
    ENDON.
    PERFORM add_line  USING ls_data_4_build l_parr_key.

  ENDLOOP.

ENDFORM.

FORM add_header  USING    lps_data LIKE LINE OF gt_data_4_build
                 CHANGING lpw_parr_key TYPE lvc_nkey.
  DATA :ls_data TYPE gty_data_tree.
  ls_data = CORRESPONDING #( lps_data ).
  go_alv->add_node(
    EXPORTING
      i_relat_node_key     = ''                " Node Already in Tree Hierarchy
      i_relationship       = cl_gui_column_tree=>relat_last_child                 " How to Insert Node
      i_node_text      = |{ p_path }|
*      is_outtab_line   = ls_data
    IMPORTING
      e_new_node_key   = lpw_parr_key
  ).
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.


FORM add_line  USING    lps_data LIKE LINE OF gt_data_4_build
                        lpw_parr_key TYPE lvc_nkey.
  DATA :ls_data TYPE gty_data_tree.
  ls_data = CORRESPONDING #( lps_data ).
  go_alv->add_node(
    EXPORTING
      i_relat_node_key     = lpw_parr_key                " Node Already in Tree Hierarchy
      i_relationship       = cl_gui_column_tree=>relat_last_child                 " How to Insert Node
      i_node_text      = |{ lps_data-name }|
      is_outtab_line   = ls_data
*    IMPORTING
*      e_new_node_key   = lpw_parr_key
  ).
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Include          ZPG_EVTP_READ_FILE_F01
*&---------------------------------------------------------------------*
FORM read_csv  USING  p_folder TYPE string
                      p_file_name TYPE lvc_value.
  DATA: lw_filename TYPE string,
        lw_data     TYPE string,
        ls_data     LIKE LINE OF gt_data,
        c_crlf(1)   TYPE c VALUE cl_abap_char_utilities=>cr_lf.
  DATA: lw_file_date TYPE dats,
        lw_file_time TYPE sytime.
  DATA :lr_list_idtype TYPE fccx_t_range_row."add thanhnt 25.05.2023

  DATA: lw_junk   TYPE string.
  IF strlen( p_file_name ) >= 13.
    lw_file_date = p_file_name+5(8).
  ENDIF.

  CALL METHOD zcl_utility=>get_tvarv_s(
    EXPORTING
      i_name    = 'ZVAR_IDTYPE_READ_FILE'
*     i_na_add  = 'X'
    IMPORTING
      e_t_range = lr_list_idtype ).
  IF strlen( p_file_name ) >= 17.
    lw_file_time = p_file_name+13(4).
    CONCATENATE lw_file_time '00' INTO lw_file_time.
  ENDIF.

  lw_filename = |{ p_folder }/{ p_file_name }|.
  CONDENSE lw_filename.
  REFRESH: gt_data.
  OPEN DATASET lw_filename FOR INPUT IN TEXT MODE ENCODING UTF-8.
  IF sy-subrc = 0.
    DO.
      READ DATASET lw_filename INTO lw_data.
      IF sy-subrc NE 0.
        EXIT.
      ELSE.
        SPLIT lw_data AT '|' INTO ls_data-h_bukrs
                                  ls_data-h_gjahr
                                  ls_data-h_zbelnr
                                  ls_data-h_zkmuc
                                  ls_data-h_zid_no
                                  ls_data-h_zid_type
                                  ls_data-h_zid_ref1
                                  ls_data-h_zid_ref2
                                  ls_data-h_zstatus
                                  ls_data-h_zbcps
                                  ls_data-h_zprctr1
                                  ls_data-h_zcnps
                                  ls_data-h_zprctr2
                                  ls_data-h_budat
                                  ls_data-h_ev_partner1
                                  ls_data-h_zpartner1
                                  ls_data-h_ev_partner_bc
                                  ls_data-h_ev_userid
                                  ls_data-h_amount_h
                                  ls_data-h_rhcur
                                  ls_data-h_ztknh
                                  ls_data-h_znh
                                  ls_data-h_zcnnh
                                  ls_data-h_zpl
                                  ls_data-h_bktxt
                                  ls_data-h_lastupdate
                                  ls_data-h_create_time
                                  ls_data-h_zid_no_des
                                  ls_data-h_zid_ref1_des
                                  ls_data-h_zid_ref2_des
                                  ls_data-i_bukrs
                                  ls_data-i_gjahr
                                  ls_data-i_zbelnr
                                  ls_data-i_docln
                                  ls_data-i_zid_no
                                  ls_data-i_zid_type
                                  ls_data-i_ev_partner1
                                  ls_data-i_zpartner1
                                  ls_data-i_ev_partner_bc
                                  ls_data-i_ev_userid
                                  ls_data-i_zbcgui
                                  ls_data-i_zprctr3
                                  ls_data-i_zcngui
                                  ls_data-i_zdichvu
                                  ls_data-i_servgroup
                                  ls_data-i_zdoanhthu
                                  ls_data-i_zstvat
                                  ls_data-i_zamount
                                  ls_data-i_zstcod
                                  ls_data-i_zstckhoan
                                  ls_data-i_sgtxt
                                  ls_data-i_zgjahr
                                  ls_data-i_rhcur
                                  ls_data-i_zfbdt.
        " delete CR_LF at end of line
        SPLIT ls_data-i_zfbdt AT c_crlf INTO:ls_data-i_zfbdt lw_junk.
        CONDENSE ls_data-i_zfbdt.
        ls_data-f_date = lw_file_date.
        ls_data-f_time = lw_file_time.

*        IF ls_data-h_zid_type = 'NH01' OR ls_data-h_zid_type = 'NH02' OR ls_data-h_zid_type = 'V01' OR ls_data-h_zid_type = 'V02'
*                                       OR ls_data-h_zid_type = 'NH04' OR ls_data-h_zid_type = 'BK19' OR ls_data-h_zid_type = 'BK20'
*                                       OR ls_data-h_zid_type = 'NH05' OR ls_data-h_zid_type = 'BK21'"change by thanhnt 11.02.2023
*                                       OR ls_data-h_zid_type = 'NH06'.
*          ls_data-h_ztknh = ls_data-h_zid_ref2_des.
**          CLEAR ls_data-h_zid_ref2_des.
*        ENDIF.
        IF ls_data-h_zid_type IN lr_list_idtype.
          ls_data-h_ztknh = ls_data-h_zid_ref2_des. "change thanhnt 07.06.2023
        ENDIF.

        TRANSLATE ls_data-h_ev_partner1 TO UPPER CASE.
        TRANSLATE ls_data-h_ev_partner_bc TO UPPER CASE.
        TRANSLATE ls_data-i_ev_partner1 TO UPPER CASE.
        TRANSLATE ls_data-i_ev_partner_bc TO UPPER CASE.

        IF ( ls_data-h_zid_type = 'PT' AND ( ls_data-h_zkmuc = '2' OR ls_data-h_zkmuc = '5' OR ls_data-h_zkmuc = '6' OR ls_data-h_zkmuc = '7' ) ) OR ls_data-h_zid_type = 'BK16'.
          ls_data-i_ev_partner_bc = ls_data-i_ev_partner1.
        ENDIF.

        APPEND ls_data TO gt_data.
        CLEAR ls_data.
      ENDIF.
    ENDDO.
    CLOSE DATASET lw_filename.

    "delete row 1 from gt_data.
    DELETE gt_data INDEX 1.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_data TABLES gt_header STRUCTURE  zev_trans_h
                       gt_item STRUCTURE  zev_trans_i
                USING p_file_name TYPE lvc_value.
  DATA: lt_header      TYPE STANDARD TABLE OF zev_trans_h,
        lt_header_tmp1 TYPE STANDARD TABLE OF zev_trans_h,
*        lt_header_bk01 TYPE STANDARD TABLE OF zev_trans_h,
        lt_item        TYPE STANDARD TABLE OF zev_trans_i,
        lt_item_tmp1   TYPE STANDARD TABLE OF zev_trans_i,
        lw_belnr       TYPE numc10,
        lv_id_number   TYPE bu_id_number.

  DATA: ls_pst_rule TYPE zmap_ev_pst_rule.

  TYPES :BEGIN OF lty_id_number,
           id_num_bp TYPE bu_id_number,
         END OF lty_id_number.
  DATA :lt_id_number TYPE TABLE OF lty_id_number.
  TYPES: tt_ev_partner1 TYPE RANGE OF bu_bpext.
  lt_header = CORRESPONDING #( gt_data MAPPING bukrs         = h_bukrs
                                               gjahr         = h_gjahr
                                               zbelnr        = h_zbelnr
                                               zkmuc         = h_zkmuc
                                               zid_no        = h_zid_no
                                               zid_type      = h_zid_type
                                               zid_ref1      = h_zid_ref1
                                               zid_ref2      = h_zid_ref2
                                               zstatus       = h_zstatus
                                               zbcps         = h_zbcps
                                               zprctr1       = h_zprctr1
                                               zcnps         = h_zcnps
                                               budat         = h_budat
                                               ev_partner1   = h_ev_partner1
                                               zpartner1     = h_zpartner1
                                               ev_partner_bc = h_ev_partner_bc
                                               ev_userid     = h_ev_userid
                                               amount_h      = h_amount_h
                                               rhcur         = h_rhcur
                                               ztknh         = h_ztknh
                                               znh           = h_znh
                                               zcnnh         = h_zcnnh
                                               zpl           = h_zpl
                                               bktxt         = h_bktxt
                                               lastupdate    = h_lastupdate
                                               create_time   = h_create_time
                                               zid_no_des    = h_zid_no_des
                                               zid_ref1_des  = h_zid_ref1_des
                                               zid_ref2_des  = h_zid_ref2_des
                                               file_date     = f_date
                                               file_time     = f_time
                                               charge_batch_id = i_servgroup
                                                ).
*
  LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<fs_header>).
    <fs_header>-file_name = p_file_name.
    IF  <fs_header>-ev_partner1 = <fs_header>-ev_partner_bc.
      IF <fs_header>-ev_partner1 <> '0' AND <fs_header>-ev_partner1 CO '1234567890 '.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_header>-ev_partner1
          IMPORTING
            output = <fs_header>-ev_partner1.
        CONCATENATE 'S' <fs_header>-ev_partner1 INTO <fs_header>-ev_partner1.
      ENDIF.

      IF <fs_header>-ev_partner1(1) = 'D' AND <fs_header>-ev_partner1+1 CO '1234567890 '.
        CONCATENATE 'S' <fs_header>-ev_partner1 INTO <fs_header>-ev_partner1.
      ENDIF.

    ENDIF.
    IF <fs_header>-ev_partner1 <> '0'.
      lt_id_number = VALUE #( BASE lt_id_number ( id_num_bp = <fs_header>-ev_partner1 ) ).
    ELSE.
      lt_id_number = VALUE #( BASE lt_id_number ( id_num_bp = |KXD{ <fs_header>-ev_partner_bc }| ) ).
    ENDIF.

  ENDLOOP.

  SORT lt_header.
  DELETE ADJACENT DUPLICATES FROM lt_header COMPARING ALL FIELDS.
  " Change document number for item
  lt_item = CORRESPONDING #( gt_data MAPPING  bukrs         = i_bukrs
                                              gjahr         = i_gjahr
                                              zbelnr        = i_zbelnr
                                              docln         = i_docln
                                              zid_no        = i_zid_no
                                              zid_type      = i_zid_type
                                              ev_partner1   = i_ev_partner1
                                              zpartner1     = i_zpartner1
                                              ev_partner_bc = i_ev_partner_bc
                                              ev_userid     = i_ev_userid
                                              zbcgui        = i_zbcgui
                                              zprctr3       = i_zprctr3
                                              zcngui        = i_zcngui
                                              zdichvu       = i_zdichvu
*                                              servgroup     = i_servgroup
                                              zdoanhthu     = i_zdoanhthu
                                              zstvat        = i_zstvat
                                              zamount       = i_zamount
                                              zstcod        = i_zstcod
                                              zstckhoan     = i_zstckhoan
                                              sgtxt         = i_sgtxt
                                              zgjahr        = i_zgjahr
                                              rhcur         = i_rhcur
                                              zfbdt         = i_zfbdt ).

  LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
    IF <fs_item>-ev_partner1 = <fs_item>-ev_partner_bc.
      IF <fs_item>-ev_partner1 <> '0' AND <fs_item>-ev_partner1 CO '1234567890 '.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_item>-ev_partner1
          IMPORTING
            output = <fs_item>-ev_partner1.
        CONCATENATE 'S' <fs_item>-ev_partner1 INTO <fs_item>-ev_partner1.
      ENDIF.

*      IF strlen( <fs_item>-ev_partner1 ) > 8 AND <fs_item>-ev_partner1(2) = 'D0'.
      IF <fs_item>-ev_partner1(1) = 'D' AND <fs_item>-ev_partner1+1 CO '1234567890 '.
        CONCATENATE 'S' <fs_item>-ev_partner1 INTO <fs_item>-ev_partner1.
      ENDIF.
    ENDIF.
    IF <fs_item>-ev_partner1 <> '0'.
      lt_id_number = VALUE #( BASE lt_id_number ( id_num_bp = <fs_item>-ev_partner1 ) ).
    ELSE.
      lt_id_number = VALUE #( BASE lt_id_number ( id_num_bp = |KXD{ <fs_item>-ev_partner_bc }| ) ).
    ENDIF.
  ENDLOOP.

  SORT lt_item.

  SELECT  idnumber AS ev_partner1, partner
        FROM but0id
        INTO TABLE @DATA(lt_bp_mapping)
        FOR ALL ENTRIES IN @lt_id_number
        WHERE idnumber = @lt_id_number-id_num_bp
          AND type = 'FS0004'.
  IF sy-subrc = 0.
    SORT lt_bp_mapping BY ev_partner1.
  ENDIF.


  LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<lfs_header>).
    IF <lfs_header>-ev_partner1 <> '0'.
      READ TABLE lt_bp_mapping INTO DATA(ls_bp_mapping) WITH KEY ev_partner1 = <lfs_header>-ev_partner1 BINARY SEARCH.
      IF sy-subrc = 0.
        <lfs_header>-zpartner1 = ls_bp_mapping-partner.
      ENDIF.
    ELSE.
      lv_id_number = |KXD{ <lfs_header>-ev_partner_bc }|.
      READ TABLE lt_bp_mapping INTO ls_bp_mapping
      WITH KEY ev_partner1 = lv_id_number
      BINARY SEARCH.
      IF sy-subrc = 0.
        <lfs_header>-zpartner1 = ls_bp_mapping-partner.
      ENDIF.
    ENDIF.
*    "convert amount base on currency to save data
    CALL FUNCTION 'ZFM_CURR_AMOUNT_DISPLAY_TO_SAP'
      EXPORTING
        currency        = 'VND'
        amount_display  = <lfs_header>-amount_h
      IMPORTING
        amount_internal = <lfs_header>-amount_h
      EXCEPTIONS
        internal_error  = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDLOOP.

  LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<lfs_item>).
    READ TABLE lt_header WITH KEY bukrs = <lfs_item>-bukrs
                                  gjahr    = <lfs_item>-gjahr
                                  zid_no   = <lfs_item>-zid_no
                                  zid_type = 'PC'
                                  zkmuc = 7

    INTO DATA(ls_header_pc7).


    IF ls_header_pc7-zid_type = 'PC' AND ls_header_pc7-zkmuc = 7.
      <lfs_item>-zpartner1 = 'THUENGOAI'.
    ELSEIF <lfs_item>-ev_partner1 <> '0'.
      "set information mapping for BP
      READ TABLE lt_bp_mapping INTO ls_bp_mapping
      WITH KEY ev_partner1 = <lfs_item>-ev_partner1
      BINARY SEARCH.
      IF sy-subrc = 0.
        <lfs_item>-zpartner1 = ls_bp_mapping-partner.
      ENDIF.
    ELSE.
      lv_id_number = |KXD{ <lfs_item>-ev_partner_bc }|.
      "set information mapping for BP
      READ TABLE lt_bp_mapping INTO ls_bp_mapping
      WITH KEY ev_partner1 = lv_id_number
      BINARY SEARCH.
      IF sy-subrc = 0.
        <lfs_item>-zpartner1 = ls_bp_mapping-partner.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ZFM_CURR_AMOUNT_DISPLAY_TO_SAP'
      EXPORTING
        currency        = 'VND'
        amount_display  = <lfs_item>-zdoanhthu
      IMPORTING
        amount_internal = <lfs_item>-zdoanhthu
      EXCEPTIONS
        internal_error  = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
*
*    "convert tiền VAT base on currency to save data
    CALL FUNCTION 'ZFM_CURR_AMOUNT_DISPLAY_TO_SAP'
      EXPORTING
        currency        = 'VND'
        amount_display  = <lfs_item>-zstvat
      IMPORTING
        amount_internal = <lfs_item>-zstvat
      EXCEPTIONS
        internal_error  = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
*
    "convert Total Amount base on currency to save data
    CALL FUNCTION 'ZFM_CURR_AMOUNT_DISPLAY_TO_SAP'
      EXPORTING
        currency        = 'VND'
        amount_display  = <lfs_item>-zamount
      IMPORTING
        amount_internal = <lfs_item>-zamount
      EXCEPTIONS
        internal_error  = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    "convert Tiền COD base on currency to save data
    CALL FUNCTION 'ZFM_CURR_AMOUNT_DISPLAY_TO_SAP'
      EXPORTING
        currency        = 'VND'
        amount_display  = <lfs_item>-zstcod
      IMPORTING
        amount_internal = <lfs_item>-zstcod
      EXCEPTIONS
        internal_error  = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
*
    "convert Số tiền CK base on currency to save data
    CALL FUNCTION 'ZFM_CURR_AMOUNT_DISPLAY_TO_SAP'
      EXPORTING
        currency        = 'VND'
        amount_display  = <lfs_item>-zstckhoan
      IMPORTING
        amount_internal = <lfs_item>-zstckhoan
      EXCEPTIONS
        internal_error  = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CLEAR :lv_id_number,ls_header_pc7.
  ENDLOOP.



  gt_header[] = lt_header[].
  gt_item[] = lt_item[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MOVE_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_FILE_LIST_NAME
*&---------------------------------------------------------------------*
FORM move_file  USING    p_file_name TYPE eps2filnam.
  DATA: lw_param TYPE sxpgcolist-parameters.

  lw_param = |{ gw_folder }/{ p_file_name } { gw_folder_arc }/{ p_file_name }|.

  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
    EXPORTING
      commandname                   = 'ZMOVE'
      additional_parameters         = lw_param
    EXCEPTIONS
      no_permission                 = 1
      command_not_found             = 2
      parameters_too_long           = 3
      security_risk                 = 4
      wrong_check_call_interface    = 5
      program_start_error           = 6
      program_termination_error     = 7
      x_error                       = 8
      parameter_expected            = 9
      too_many_parameters           = 10
      illegal_command               = 11
      wrong_asynchronous_parameters = 12
      cant_enq_tbtco_entry          = 13
      jobcount_generation_error     = 14
      OTHERS                        = 15.
  IF sy-subrc <> 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_LOG_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

* FORM LOCK_CURRENT_INSTANCE
FORM lock_current_instance CHANGING c_ok TYPE mark.
*---FM to lock the current instance of the program
  CALL FUNCTION 'ENQUEUE_E_TRDIR'
    EXPORTING
      mode_trdir     = abap_true
      name           = sy-repid
      _scope         = '1'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc IS NOT INITIAL.
    CLEAR c_ok.
    WRITE 'Already one Instance of the Program is Running!'.
  ELSE.
    c_ok = 'X'.
  ENDIF.
ENDFORM. "LOCK_CURRENT_INSTANCE

* FORM UNLOCK_CURRENT_INSTANCE
FORM unlock_current_instance.
*---FM to release the lock on the running program
  CALL FUNCTION 'DEQUEUE_E_TRDIR'
    EXPORTING
      mode_trdir = abap_true
      name       = sy-repid.
ENDFORM. "UNLOCK_CURRENT_INSTANCE
*&---------------------------------------------------------------------*
*& Form PROCESS_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_TEXT
*&---------------------------------------------------------------------*
FORM process_file  USING lwp_file TYPE lvc_value.
  SELECT * FROM  zmap_ev_bc INTO TABLE gt_mapping_pc.
  IF sy-subrc = 0.
    SORT gt_mapping_pc BY zev_bc zev_cn valid_fr.
  ENDIF.
  "get data of mapping Services
  SELECT * FROM zmap_ev_dichvu INTO TABLE gt_mapping_srv.
  IF sy-subrc = 0.
    SORT gt_mapping_srv BY zdichvu.
  ENDIF.

  SELECT * FROM t012k INTO TABLE gt_t012k.
  SORT gt_t012k BY bukrs bankn.

  PERFORM read_csv USING p_path lwp_file.

ENDFORM.

FORM setup_alv USING lpw_type TYPE string
                     lpw_tab TYPE char30
                     lpw_cont_name TYPE c
                CHANGING
                     go_cont TYPE REF TO cl_gui_custom_container.

  DATA :lo_splitter    TYPE REF TO cl_gui_splitter_container,
        lo_splitter2   TYPE REF TO cl_gui_splitter_container,
        lo_splitter3   TYPE REF TO cl_gui_splitter_container,

        lo_alv1        TYPE REF TO cl_gui_alv_grid,
        lo_alv2        TYPE REF TO cl_gui_alv_grid,
        lo_cont_tab1   TYPE REF TO cl_gui_container,
        lo_cont_tab2   TYPE REF TO cl_gui_container,
        lo_cont_tab1_1 TYPE REF TO cl_gui_container,
        lo_cont_tab1_2 TYPE REF TO cl_gui_container,
        lo_cont_tab2_1 TYPE REF TO cl_gui_container,
        lo_cont_tab2_2 TYPE REF TO cl_gui_container.
  DATA: ls_layout_src TYPE lvc_s_layo,
        lt_fcat_alv2  TYPE  lvc_t_fcat,
        ls_fcat_alv2  TYPE lvc_s_fcat.

  DATA:lo_trans_tab1  TYPE REF TO data,
       lo_trans_tab2  TYPE REF TO data,
       lo_doc_header1 TYPE REF TO cl_dd_document,
       lo_doc_header2 TYPE REF TO cl_dd_document.
  DATA :lt_exclude TYPE ui_functions.
  FREE :go_container_header1,go_container_header2.

  FIELD-SYMBOLS:<fs_evtp_tab1> TYPE STANDARD TABLE .
  CREATE DATA lo_trans_tab1 TYPE TABLE OF (lpw_type). "Dynamic Itab
  ASSIGN lo_trans_tab1->* TO <fs_evtp_tab1>.
  FIELD-SYMBOLS:<fs_trans_tab2> TYPE STANDARD TABLE.
  CREATE DATA lo_trans_tab2 TYPE TABLE OF (lpw_type). "Dynamic Itab
  ASSIGN lo_trans_tab2->* TO <fs_trans_tab2>.


  IF go_cont IS BOUND.
    go_cont->free( ).
  ENDIF.

  go_cont = NEW cl_gui_custom_container( container_name = lpw_cont_name ).
  lo_splitter = NEW cl_gui_splitter_container( parent = go_cont rows = 1 columns = 2 ).

  lo_splitter2 = NEW cl_gui_splitter_container( parent =  lo_splitter->get_container( row  = 1 column = 1 ) rows = 2 columns = 1 ).
  lo_splitter2->set_row_height( id = 1  height = 5 ).
  lo_cont_tab1_1 = lo_splitter2->get_container( row = 1 column = 1 ).
  lo_cont_tab1_2 = lo_splitter2->get_container( row = 2 column = 1 ).

  lo_splitter3 = NEW cl_gui_splitter_container( parent =  lo_splitter->get_container( row  = 1 column = 2 ) rows = 2 columns = 1 ).
  lo_splitter3->set_row_height( id = 1  height = 5 ).
  lo_cont_tab2_1 = lo_splitter3->get_container( row = 1 column = 1 ).
  lo_cont_tab2_2 = lo_splitter3->get_container( row = 2 column = 1 ).

  go_container_header1 = lo_splitter2->get_container( row = 1 column = 1 ).
  go_container_header2 = lo_splitter3->get_container( row = 1 column = 1 ).
  lo_doc_header1 = NEW cl_dd_document( style = 'ALV_GRID' ).
  lo_doc_header2 = NEW cl_dd_document( style = 'ALV_GRID' ).

  lo_alv1 = NEW cl_gui_alv_grid( i_parent = lo_cont_tab1_2 ).
  lo_alv2 = NEW cl_gui_alv_grid( i_parent = lo_cont_tab2_2 ).
  DATA(lo_handler) = NEW lcl_event_handler( ).
  SET HANDLER lo_handler->handle_set_title_301 FOR lo_alv1.
  SET HANDLER lo_handler->handle_set_title_302 FOR lo_alv2.
  SET HANDLER lo_handler->handle_add_toolbar FOR lo_alv2.
  SET HANDLER lo_handler->handle_user_command FOR lo_alv2.

  CASE lpw_type.
    WHEN 'GTY_TRANS_H'.
      SORT gt_header BY bukrs gjahr zid_no zid_type.
      SELECT * FROM zev_trans_h FOR ALL ENTRIES IN @gt_header
      WHERE bukrs = @gt_header-bukrs AND gjahr = @gt_header-gjahr AND zid_no = @gt_header-zid_no AND zid_type =  @gt_header-zid_type
      INTO TABLE @DATA(lt_header_trans).
      LOOP AT lt_header_trans INTO DATA(ls_header_trans).
        READ TABLE gt_header INTO DATA(ls_header) WITH KEY bukrs = ls_header_trans-bukrs  gjahr = ls_header_trans-gjahr  zid_no = ls_header_trans-zid_no zid_type =  ls_header_trans-zid_type BINARY SEARCH.
        IF sy-subrc = 0.
          PERFORM set_tab_view USING ls_header ls_header_trans CHANGING <fs_evtp_tab1> <fs_trans_tab2>.
        ENDIF.
      ENDLOOP.
    WHEN 'GTY_TRANS_I'.
      SORT gt_item BY bukrs gjahr zid_no zid_type docln.
      SELECT * FROM zev_trans_i FOR ALL ENTRIES IN @gt_item
      WHERE bukrs = @gt_item-bukrs AND gjahr = @gt_item-gjahr  AND zid_no = @gt_item-zid_no AND docln = @gt_item-docln AND zid_type = @gt_item-zid_type
      INTO TABLE @DATA(lt_items_trans).
      LOOP AT lt_items_trans INTO DATA(ls_item_trans).
        READ TABLE gt_item INTO DATA(ls_item) WITH KEY bukrs = ls_item_trans-bukrs  gjahr = ls_item_trans-gjahr  zid_no = ls_item_trans-zid_no zid_type = ls_item_trans-zid_type docln = ls_item_trans-docln  BINARY SEARCH.
        IF sy-subrc = 0.
          PERFORM set_tab_view USING ls_item ls_item_trans CHANGING <fs_evtp_tab1> <fs_trans_tab2>.
        ENDIF.
      ENDLOOP.
  ENDCASE.

  PERFORM exclude_tb_functions CHANGING lt_exclude.
  ls_layout_src-ctab_fname = 'CELL_COLOR'.
  ls_layout_src-cwidth_opt = 'X'.
  lo_alv1->set_table_for_first_display(
    EXPORTING
      is_layout                     = ls_layout_src
      it_toolbar_excluding          = lt_exclude
      i_structure_name              =  CONV #( lpw_tab )                " Internal Output Table Structure Name
      i_save                        =  'X'                " Save Layout
    CHANGING
      it_outtab                     =   <fs_evtp_tab1>               " Output Table
  ).
  lo_alv1->list_processing_events(  i_event_name = 'TOP_OF_PAGE' i_dyndoc_id  = lo_doc_header1 ).

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = lpw_tab
    CHANGING
      ct_fieldcat      = lt_fcat_alv2.
  ls_fcat_alv2-tooltip = 'Select'.
  ls_fcat_alv2-coltext = 'Select'.
  ls_fcat_alv2-fieldname = 'FLAG'.
  ls_fcat_alv2-col_pos = 1.
  ls_fcat_alv2-checkbox = 'X'.
  ls_fcat_alv2-edit = 'X'.
  INSERT ls_fcat_alv2 INTO lt_fcat_alv2 INDEX 2.

  lo_alv2->set_table_for_first_display(
    EXPORTING
      is_layout                     = ls_layout_src
      it_toolbar_excluding          = lt_exclude
*      i_structure_name              =  CONV #( lpw_tab )                " Internal Output Table Structure Name
*      is_variant                    =                  " Layout
      i_save                        =  'X'                " Save Layout
    CHANGING
      it_outtab                     =   <fs_trans_tab2>               " Output Table
      it_fieldcatalog = lt_fcat_alv2
  ).
  lo_alv2->list_processing_events(  i_event_name = 'TOP_OF_PAGE' i_dyndoc_id  = lo_doc_header2 ).
  IF gw_type = lpw_type.
    go_alv_grid =  lo_alv2.
    ASSIGN <fs_evtp_tab1> TO <fs_evtp_tab1_g>.
    ASSIGN <fs_trans_tab2> TO <fs_trans_tab2_g>.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TAB_VIEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_HEADER
*&      --> LS_HEADER_TRANS
*&      <-- <FS_EVTP_TAB1>
*&      <-- <FS_TRANS_TAB2>
*&---------------------------------------------------------------------*
FORM set_tab_view  USING    lps_evtp TYPE any
                            lps_trans TYPE any
                   CHANGING lpt_evtp_tab1 TYPE STANDARD TABLE
                            lpt_trans_tab2 TYPE STANDARD TABLE.
  DATA:    lo_field     TYPE REF TO cl_abap_structdescr.
  DATA :ls_cell_color       TYPE lvc_s_scol,
        lt_cell_color_trans TYPE lvc_t_scol,
        lt_cell_color_evtp  TYPE lvc_t_scol.
  lo_field ?= cl_abap_typedescr=>describe_by_data( lps_evtp ).

  LOOP AT lo_field->components INTO DATA(ls_field) WHERE ( ( type_kind = 'P' AND length = 12 AND decimals = 2 ) OR name = 'ZTKNH' OR name = 'EV_PARTNER1' OR name = 'EV_PARTNER_BC' or name = 'ZID_REF2_DES' ).
    ASSIGN COMPONENT ls_field-name OF STRUCTURE lps_evtp TO FIELD-SYMBOL(<fs_val_evtp>).
    ASSIGN COMPONENT ls_field-name OF STRUCTURE lps_trans TO FIELD-SYMBOL(<fs_val_trans>).
    CHECK <fs_val_evtp> <> <fs_val_trans>.
    ls_cell_color-fname = ls_field-name.
    ls_cell_color-color-col = 7.
    ls_cell_color-color-int = '1'.
    ls_cell_color-color-inv = '0'.
    APPEND ls_cell_color TO lt_cell_color_trans.
    ls_cell_color-color-col = 3.
    APPEND ls_cell_color TO lt_cell_color_evtp.
    CLEAR ls_cell_color.
  ENDLOOP.
  CHECK lt_cell_color_trans IS NOT INITIAL.
  APPEND INITIAL LINE TO lpt_trans_tab2 ASSIGNING FIELD-SYMBOL(<fs_tab2>).
  MOVE-CORRESPONDING lps_trans TO <fs_tab2>.
  ASSIGN COMPONENT 'CELL_COLOR' OF STRUCTURE <fs_tab2> TO FIELD-SYMBOL(<fs_color>).
  <fs_color> = lt_cell_color_trans.

  APPEND INITIAL LINE TO lpt_evtp_tab1 ASSIGNING FIELD-SYMBOL(<fs_tab1>).
  MOVE-CORRESPONDING lps_evtp TO <fs_tab1>.
  ASSIGN COMPONENT 'CELL_COLOR' OF STRUCTURE <fs_tab1> TO <fs_color>.
  <fs_color> = lt_cell_color_evtp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM select_all .
  LOOP AT <fs_trans_tab2_g> ASSIGNING FIELD-SYMBOL(<fs_trans_tab2_g_line>).
    ASSIGN COMPONENT 'FLAG' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_value>).
    <fs_value> = 'X'.
  ENDLOOP.
  go_alv_grid->refresh_table_display( ).
  go_alv_grid->check_changed_data( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DESELECT_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM deselect_all .
  LOOP AT <fs_trans_tab2_g> ASSIGNING FIELD-SYMBOL(<fs_trans_tab2_g_line>).
    ASSIGN COMPONENT 'FLAG' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_value>).
    CLEAR <fs_value>.
  ENDLOOP.
  go_alv_grid->refresh_table_display( ).
  go_alv_grid->check_changed_data( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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
*& Form UPDATE_TRANS_H
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_trans_h .
  DATA :ls_return TYPE bapiret2.
  DATA :lt_evtp_h  TYPE TABLE OF gty_trans_h,
        ls_trans_h TYPE gty_trans_h.
  DATA :lt_trans_h_upd TYPE TABLE OF zev_trans_h,
        ls_trans_h_upd TYPE  zev_trans_h.
  DATA :lt_trans_prev TYPE TABLE OF gty_trans_h.
  DATA :lt_trans_h_log TYPE TABLE OF ztb_log_trans_h,
        ls_trans_h_log TYPE ztb_log_trans_h.
  DATA :lw_time_cr TYPE sy-uzeit,
        lw_date_cr TYPE sy-datum,
        lw_user_cr TYPE sy-uname.
*  LOOP AT <fs_evtp_tab1_g> ASSIGNING FIELD-SYMBOL(<fs_evtp_tab1_g_line>).
*    APPEND <fs_evtp_tab1_g_line> TO lt_evtp_h.
*  ENDLOOP.
  MOVE <fs_evtp_tab1_g> TO lt_evtp_h.
  SORT lt_evtp_h BY bukrs gjahr zid_no zid_type.
  LOOP AT <fs_trans_tab2_g> ASSIGNING FIELD-SYMBOL(<fs_trans_tab2_g_line>).
    ASSIGN COMPONENT 'FLAG' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_flag>).
    CHECK <fs_flag> = 'X'.
    APPEND <fs_trans_tab2_g_line> TO lt_trans_prev.
    ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_bukrs>).
    ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_gjahr>).
    ASSIGN COMPONENT 'ZID_NO' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zid_no>).
    ASSIGN COMPONENT 'ZID_TYPE' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zid_type>).
*    ASSIGN COMPONENT 'ZBELNR' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zbelnr>).
    READ TABLE lt_evtp_h INTO DATA(ls_evtp_h) WITH KEY bukrs = <fs_bukrs> gjahr = <fs_gjahr> zid_no = <fs_zid_no> zid_type = <fs_zid_type> BINARY SEARCH.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'AMOUNT_H' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_amount_h>).
      <fs_amount_h> = ls_evtp_h-amount_h.
      ASSIGN COMPONENT 'ZTKNH' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_ztknh>).
      <fs_ztknh> = ls_evtp_h-ztknh.
      ASSIGN COMPONENT 'EV_PARTNER1' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_ev_partner1>).
      <fs_ev_partner1> = ls_evtp_h-ev_partner1.
      ASSIGN COMPONENT 'EV_PARTNER_BC' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_ev_partner_bc>).
      <fs_ev_partner_bc> = ls_evtp_h-ev_partner_bc.
      ASSIGN COMPONENT 'ZPARTNER1' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zpartner1>).
      <fs_zpartner1> = ls_evtp_h-zpartner1.
      ASSIGN COMPONENT 'ZID_REF2_DES' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zid_ref2_des>).
      <fs_zid_ref2_des> = ls_evtp_h-zid_ref2_des.
      MOVE-CORRESPONDING <fs_trans_tab2_g_line> TO ls_trans_h.
      APPEND INITIAL LINE TO lt_trans_h_upd ASSIGNING FIELD-SYMBOL(<fs_trans_h_upd>).
      MOVE-CORRESPONDING ls_trans_h TO <fs_trans_h_upd>.
      CLEAR ls_trans_h.
      DELETE <fs_trans_tab2_g>.
    ENDIF.
  ENDLOOP.
  IF lt_trans_h_upd IS NOT INITIAL.
    lw_time_cr = sy-uzeit.
    lw_date_cr = sy-datum.
    lw_user_cr = sy-uname.

    lt_trans_h_log = CORRESPONDING #( lt_trans_prev ).
*    ls_trans_h_log-date_cr = lw_date_cr.
*    ls_trans_h_log-time_cr = lw_time_cr.
*    ls_trans_h_log-user_cr = lw_user_cr.
    LOOP AT lt_trans_h_log ASSIGNING FIELD-SYMBOL(<fs_trans_h_log>).
      <fs_trans_h_log>-date_cr = lw_date_cr.
      <fs_trans_h_log>-time_cr = lw_time_cr.
      <fs_trans_h_log>-user_cr = lw_user_cr.
      <fs_trans_h_log>-guid = cl_system_uuid=>create_uuid_x16_static( ).
    ENDLOOP.

    INSERT ztb_log_trans_h FROM TABLE lt_trans_h_log.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    UPDATE zev_trans_h FROM TABLE lt_trans_h_upd.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = ls_return.
    IF ls_return-type = 'E'.
      MESSAGE 'Update lỗi !' TYPE 'I' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
    ELSE.
      MESSAGE 'Update thành công !' TYPE 'I' DISPLAY LIKE 'S'.
    ENDIF.
  ELSE.
    MESSAGE 'Chưa chọn bản ghi cần update !' TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.

  go_alv_grid->check_changed_data( ).
  go_alv_grid->refresh_table_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_TRANS_I
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_trans_i .
  DATA :ls_return TYPE bapiret2.
  DATA :lt_evtp_i  TYPE TABLE OF gty_trans_i,
        ls_trans_i TYPE gty_trans_i.
  DATA :lt_trans_i_upd TYPE TABLE OF zev_trans_i,
        ls_trans_i_upd TYPE  zev_trans_i.
  DATA :lt_trans_prev TYPE TABLE OF gty_trans_i.
  DATA :lt_trans_i_log TYPE TABLE OF ztb_log_trans_i,
        ls_trans_i_log TYPE ztb_log_trans_i.
  DATA :lw_time_cr TYPE sy-uzeit,
        lw_date_cr TYPE sy-datum,
        lw_user_cr TYPE sy-uname.
*  LOOP AT <fs_evtp_tab1_g> ASSIGNING FIELD-SYMBOL(<fs_evtp_tab1_g_line>).
*    APPEND <fs_evtp_tab1_g_line> TO lt_evtp_i.
*  ENDLOOP.
  MOVE <fs_evtp_tab1_g> TO lt_evtp_i.

  SORT lt_evtp_i BY bukrs gjahr zid_no zid_type docln .
  LOOP AT <fs_trans_tab2_g> ASSIGNING FIELD-SYMBOL(<fs_trans_tab2_g_line>).
    ASSIGN COMPONENT 'FLAG' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_flag>).
    CHECK <fs_flag> = 'X'.
    APPEND <fs_trans_tab2_g_line> TO lt_trans_prev.
    ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_bukrs>).
    ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_gjahr>).
    ASSIGN COMPONENT 'ZID_NO' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zid_no>).
    ASSIGN COMPONENT 'ZID_TYPE' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zid_type>).
    ASSIGN COMPONENT 'DOCLN' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_docln>).
*    ASSIGN COMPONENT 'ZBELNR' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zbelnr>).
    READ TABLE lt_evtp_i INTO DATA(ls_evtp_i) WITH KEY bukrs = <fs_bukrs> gjahr = <fs_gjahr> zid_no = <fs_zid_no> zid_type = <fs_zid_type> docln = <fs_docln> BINARY SEARCH.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'ZDOANHTHU' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zdoanhthu>).
      ASSIGN COMPONENT 'ZSTVAT' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zstvat>).
      ASSIGN COMPONENT 'ZAMOUNT' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zamount>).
      ASSIGN COMPONENT 'ZSTCOD' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zstcod>).
      ASSIGN COMPONENT 'ZSTCKHOAN' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zstckhoan>).
      ASSIGN COMPONENT 'EV_PARTNER1' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_ev_partner1>).
      ASSIGN COMPONENT 'EV_PARTNER_BC' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_ev_partner_bc>).
      ASSIGN COMPONENT 'ZPARTNER1' OF STRUCTURE <fs_trans_tab2_g_line> TO FIELD-SYMBOL(<fs_zpartner1>).
      <fs_zpartner1> = ls_evtp_i-zpartner1.
      <fs_ev_partner1> = ls_evtp_i-ev_partner1.
      <fs_ev_partner_bc> = ls_evtp_i-ev_partner_bc.
      <fs_zdoanhthu> = ls_evtp_i-zdoanhthu.
      <fs_zstvat> = ls_evtp_i-zstvat.
      <fs_zamount> = ls_evtp_i-zamount.
      <fs_zstcod> = ls_evtp_i-zstcod.
      <fs_zstckhoan> = ls_evtp_i-zstckhoan.

      MOVE-CORRESPONDING <fs_trans_tab2_g_line> TO ls_trans_i.
      APPEND INITIAL LINE TO lt_trans_i_upd ASSIGNING FIELD-SYMBOL(<fs_trans_i_upd>).
      MOVE-CORRESPONDING ls_trans_i TO <fs_trans_i_upd>.
      CLEAR ls_trans_i.
      DELETE <fs_trans_tab2_g>.
    ENDIF.
  ENDLOOP.
  IF lt_trans_i_upd IS NOT INITIAL.
    lw_time_cr = sy-uzeit.
    lw_date_cr = sy-datum.
    lw_user_cr = sy-uname.

    lt_trans_i_log = CORRESPONDING #( lt_trans_prev ).
*    ls_trans_i_log-date_cr = lw_date_cr.
*    ls_trans_i_log-time_cr = lw_time_cr.
*    ls_trans_i_log-user_cr = lw_user_cr.
    LOOP AT lt_trans_i_log ASSIGNING FIELD-SYMBOL(<fs_trans_i_log>).
      <fs_trans_i_log>-date_cr = lw_date_cr.
      <fs_trans_i_log>-time_cr = lw_time_cr.
      <fs_trans_i_log>-user_cr = lw_user_cr.
      <fs_trans_i_log>-guid = cl_system_uuid=>create_uuid_x16_static( ).
    ENDLOOP.

*    MODIFY lt_trans_i_log FROM ls_trans_i_log TRANSPORTING date_cr time_cr user_cr WHERE zid_no IS NOT INITIAL .
    INSERT ztb_log_trans_i FROM TABLE lt_trans_i_log.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    UPDATE zev_trans_i FROM TABLE lt_trans_i_upd.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = ls_return.
    IF ls_return-type = 'E'.
      MESSAGE 'Update lỗi !' TYPE 'I' DISPLAY LIKE 'E'.
      ROLLBACK WORK.
    ELSE.
      MESSAGE 'Update thành công !' TYPE 'I' DISPLAY LIKE 'S'.
    ENDIF.
  ELSE.
    MESSAGE 'Chưa chọn bản ghi cần update !' TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.

  go_alv_grid->check_changed_data( ).
  go_alv_grid->refresh_table_display( ).
ENDFORM.
