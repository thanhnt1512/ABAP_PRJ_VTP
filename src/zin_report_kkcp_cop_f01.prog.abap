*&---------------------------------------------------------------------*
*& Include          ZIN_REPORT_KKCP_F01
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
  DATA :lw_count_bkc2 TYPE REF TO int8,
        lw_count_bkc1 TYPE REF TO int8,
        lw_count_hdon TYPE REF TO int8.
  DATA :lw_amt_bkc3 TYPE REF TO dmbtr,
        lw_amt_bkc2 TYPE REF TO dmbtr,
        lw_amt_bkc1 TYPE REF TO dmbtr.
  SELECT xref1_hd AS bkc3 , xref2_hd AS bkc2 ,bktxt AS bkc1,belnr,bukrs,gjahr,ccnum,usnam
  FROM bkpf WHERE xref1_hd IN @s_bkc3 AND xref2_hd  IN @s_bkc2 AND
                  bktxt IN @s_bkc1 AND stblg = '' AND blart = 'Z1'
                  AND gjahr IN @s_year
  INTO TABLE @DATA(lt_bkpf).

  SORT lt_bkpf BY belnr bukrs gjahr.

  SELECT b~belnr,b~bukrs,b~gjahr, SUM( dmbtr ) AS amt FROM bseg AS a INNER JOIN @lt_bkpf AS b
  ON a~belnr = b~belnr AND a~bukrs = b~bukrs AND a~gjahr = b~gjahr
  WHERE shkzg = 'S'
  GROUP BY b~belnr,b~bukrs,b~gjahr
  INTO TABLE @DATA(lt_bseg).

  SORT lt_bseg BY belnr bukrs gjahr.

  DATA :ls_bkpf LIKE LINE OF lt_bkpf.
  ls_bkpf-bkc1 = 'NULL'.
  ls_bkpf-bkc2 = 'NULL'.
  ls_bkpf-bkc3 = 'NULL'.
  MODIFY lt_bkpf FROM ls_bkpf TRANSPORTING bkc1 WHERE bkc1 = ''.
  MODIFY lt_bkpf FROM ls_bkpf TRANSPORTING bkc2 WHERE bkc2 = ''.
  MODIFY lt_bkpf FROM ls_bkpf TRANSPORTING bkc3 WHERE bkc3 = ''.


  LOOP AT lt_bkpf INTO DATA(ls_data) GROUP BY ( bkc3 = ls_data-bkc3 size = GROUP SIZE ) INTO DATA(ls_group1).
    APPEND INITIAL LINE TO gt_alv_data ASSIGNING FIELD-SYMBOL(<fs_alv_data>).
    <fs_alv_data>-bkc3 = ls_group1-bkc3.
    <fs_alv_data>-cell_color = VALUE #( ( fname = 'BKC3' color-col = 6 color-int = 1 color-inv = 0 ) ( fname = 'AMOUNT' color-col = 6 color-int = 0 color-inv = 1 ) ).
    lw_count_bkc2 = REF int8( <fs_alv_data>-count ).
    lw_amt_bkc3 = REF dmbtr( <fs_alv_data>-amount ).
    LOOP AT GROUP ls_group1 INTO DATA(ls_data1) GROUP BY ( bkc2 = ls_data1-bkc2 size = GROUP SIZE index = GROUP INDEX ) INTO DATA(ls_group2).
      lw_count_bkc2->* = CONV int8( ls_group2-index ).
      APPEND INITIAL LINE TO gt_alv_data ASSIGNING <fs_alv_data>.
      <fs_alv_data>-bkc2 = ls_group2-bkc2.
      <fs_alv_data>-cell_color = VALUE #( ( fname = 'BKC2' color-col = 7 color-int = 1 color-inv = 0 ) ( fname = 'AMOUNT' color-col = 7 color-int = 0 color-inv = 1 ) ).
      lw_count_bkc1 = REF int8( <fs_alv_data>-count ).
      lw_amt_bkc2 = REF dmbtr( <fs_alv_data>-amount ).
      LOOP AT GROUP ls_group2 INTO DATA(ls_data2) GROUP BY ( bkc1 = ls_data2-bkc1 size = GROUP SIZE index = GROUP INDEX ) INTO DATA(ls_group3).
        lw_count_bkc1->* = CONV int8( ls_group3-index ).
        APPEND INITIAL LINE TO gt_alv_data ASSIGNING <fs_alv_data>.
        <fs_alv_data>-bkc1 = ls_group3-bkc1.
        <fs_alv_data>-cell_color = VALUE #( ( fname = 'BKC1' color-col = 5 color-int = 1 color-inv = 0 ) ( fname = 'AMOUNT' color-col = 5 color-int = 0 color-inv = 1 ) ).
        <fs_alv_data>-count = ls_group3-size.
        lw_amt_bkc1 = REF dmbtr( <fs_alv_data>-amount ).
        LOOP AT GROUP ls_group3 INTO DATA(ls_data3).
          APPEND INITIAL LINE TO gt_alv_data ASSIGNING <fs_alv_data>.
          <fs_alv_data>-doc = ls_data3-belnr.
          <fs_alv_data>-year = ls_data3-gjahr.
          <fs_alv_data>-bukrs = ls_data3-bukrs.
          <fs_alv_data>-ctgs = ls_data3-ccnum.
          <fs_alv_data>-amount = VALUE #( lt_bseg[ belnr = ls_data3-belnr gjahr = ls_data3-gjahr bukrs = ls_data3-bukrs ]-amt OPTIONAL ).
          <fs_alv_data>-user = ls_data3-usnam.
          lw_amt_bkc1->* = lw_amt_bkc1->* + <fs_alv_data>-amount.
        ENDLOOP.
        lw_amt_bkc2->* = lw_amt_bkc2->* + lw_amt_bkc1->*.
      ENDLOOP.
      lw_amt_bkc3->* = lw_amt_bkc3->* + lw_amt_bkc2->*.
    ENDLOOP.
  ENDLOOP.

  CALL SCREEN 2000.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FIND_IN_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM find_in_out .
  DATA :lw_break TYPE sychar01.
  cl_ci_query_attributes=>generic(
  EXPORTING
    " unique screen ID
    p_name       = CONV #( sy-repid )
    " Screen title
    p_title      = 'Search In/Out'
    p_attributes = VALUE #(
                            ( kind = 'S' obligatory = abap_true text = 'Key in/out'   ref = REF #( p_key ) )
                            ( kind = 'G' text = 'Option' ref = REF #( sy-index ) )
                            ( kind = 'R' text = 'Save in/out txt'  button_group = 'MOD' ref = REF #( rb_down ) )
                            ( kind = 'R' text = 'View in/out'  button_group = 'MOD' ref = REF #( rb_view ) )
                            ( kind = 'R' text = 'List BK/invoices of request'  button_group = 'MOD' ref = REF #( rb_dtl ) )
                          )
    p_display    = abap_false
  RECEIVING
    p_break = lw_break
   ).

  CHECK lw_break = ''.
  CASE 'X'.
    WHEN rb_view.
      CALL SCREEN '0100'.
    WHEN rb_down.
      PERFORM download_in_out.
    WHEN OTHERS.
      PERFORM view_list_bk.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETUP_TEXT_VIEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM setup_text_view .
*  DATA :lt_content TYPE STANDARD TABLE OF textline.
  DATA :lt_tab_in  TYPE STANDARD TABLE OF line,
        lt_tab_out TYPE STANDARD TABLE OF line,
        lo_txt_i   TYPE REF TO cl_gui_textedit,
        lo_txt_o   TYPE REF TO cl_gui_textedit,
        lw_in      TYPE string,
        lw_out     TYPE string.

  IF go_cont_i IS BOUND.
    go_cont_i->free( ).
  ENDIF.
  IF go_cont_o IS BOUND.
    go_cont_o->free( ).
  ENDIF.
  go_cont_i = NEW cl_gui_custom_container(  container_name = 'CONT_REQ' ).
  go_cont_o = NEW cl_gui_custom_container(  container_name = 'CONT_RES' ).
  lo_txt_i = NEW  cl_gui_textedit( parent = go_cont_i wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position ).
  lo_txt_o = NEW  cl_gui_textedit( parent = go_cont_o wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position ).



*  PERFORM read_text USING '1000' CHANGING lw_in.
  PERFORM read_in_out USING 'I' CHANGING lw_in.

  CALL METHOD lo_txt_i->set_textstream
    EXPORTING
      text = lw_in.
*  PERFORM read_text USING '1010' CHANGING lw_out.
  PERFORM read_in_out USING 'O' CHANGING lw_out.
  CALL METHOD lo_txt_o->set_textstream
    EXPORTING
      text = lw_out.

*  lo_txt_i->set_statusbar_mode( statusbar_mode = cl_gui_textedit=>true ).
*  lo_txt_i->set_toolbar_mode( toolbar_mode = cl_gui_textedit=>false ).

ENDFORM.

FORM read_text USING lpw_id TYPE thead-tdid CHANGING lpw_data TYPE string.
  DATA :lt_lines TYPE TABLE OF tline.
  TYPES :BEGIN OF ty_concat,
           tdline TYPE tdline,
         END OF ty_concat.
  DATA :lt_concat TYPE TABLE OF ty_concat.
  DATA :lw_data TYPE string.
  DATA :lw_name TYPE tdobname.
  DATA :lt_str_tab TYPE TABLE OF swastrtab.
  DATA :lw_err TYPE string.
  lw_name = p_key.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT          = SY-MANDT
      id              = lpw_id
      language        = 'E'
      name            = lw_name
      object          = 'ZKKCP'
    TABLES
      lines           = lt_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      not_found       = 4
      object          = 5
      reference_check = 6
      OTHERS          = 7.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        lw_err = 'Text ID invalid'.
      WHEN 2.
        lw_err = 'Invalid language'.
      WHEN 3.
        lw_err = 'Invalid text name'.
      WHEN 4.
        lw_err = 'Text not found'.
      WHEN 5.
        lw_err = 'Invalid text object'.
      WHEN 6.
        lw_err = 'Reference chain interrupted'.
      WHEN 7.
        lw_err = 'Some error found !'.
    ENDCASE.
  ENDIF.

  IF lw_err <> ''.
    lpw_data = lw_err.
    EXIT.
  ENDIF.


  lt_concat = CORRESPONDING #( lt_lines ).
  lw_data = concat_lines_of( table = lt_concat sep = '' ).

  DATA(json_xstring) = cl_abap_conv_codepage=>create_out( )->convert( lw_data ).
  DATA(reader) = cl_sxml_string_reader=>create( json_xstring ).
  DATA(writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
  writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
  writer->set_option( option = if_sxml_writer=>co_opt_indent ).
  reader->next_node( ).
  reader->skip_node( writer ).
  lpw_data = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( writer )->get_output( ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_IN_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM download_in_out .
  DATA : lw_in  TYPE string,
         lw_out TYPE string.
  DATA: lw_folder TYPE string,
        lw_path1  TYPE string,
        lw_path2  TYPE string,
        lw_dat    TYPE string,
        lt_data   TYPE TABLE OF string.
  PERFORM read_in_out USING 'I' CHANGING lw_in.
  PERFORM read_in_out USING 'O' CHANGING lw_out.
*  PERFORM read_text USING '1000' CHANGING lw_in.
*  PERFORM read_text USING '1010' CHANGING lw_out.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder      = lw_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  CHECK lw_folder <> ''.
  lw_dat = |{ sy-datum }_{ sy-uzeit }|.
  APPEND lw_in TO lt_data.

  lw_path1 = |{ lw_folder }{ '\' }Request_{ lw_dat }.txt|.
  cl_gui_frontend_services=>gui_download( EXPORTING filename = lw_path1
                                                    filetype = 'ASC'
                                                    codepage = '4310'
                                          CHANGING  data_tab =  lt_data ).
  REFRESH lt_data.
  APPEND lw_out TO lt_data.
  lw_path2 = |{ lw_folder }{ '\' }Response_{ lw_dat }.txt|.
  cl_gui_frontend_services=>gui_download( EXPORTING filename = lw_path2
                                                    filetype = 'ASC'
                                                    codepage = '4310'
                                          CHANGING  data_tab = lt_data ).
  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel = 'Information'
      txt1  = lw_path1
      txt2  = lw_path2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_LIST_BK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM view_list_bk .
  DATA :ls_input_kkcp TYPE zmt_bkcptx_n_sender-mt_bkcptx_n_sender,
        lw_in         TYPE string.
  CLEAR gs_mesh.
  REFRESH :gt_fc_tree,gt_tree_data.
*  PERFORM read_text USING '1000' CHANGING lw_in.
  PERFORM read_in_out USING 'I' CHANGING lw_in.
  IF sy-subrc <> 0.
    MESSAGE 'Kiểm tra lại key input' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  CHECK sy-subrc = 0.
  CALL METHOD /ui2/cl_json=>deserialize(
    EXPORTING
      json = lw_in
*     pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    CHANGING
      data = ls_input_kkcp ).



  gs_mesh-hdon = VALUE #( FOR lw_hdon IN ls_input_kkcp-hoa_don
                             (
                              bkc1_name = lw_hdon-bk1_id
                              name = lw_hdon-uuid_hd
                              so_hd = lw_hdon-so_hd
                              mau_hd = lw_hdon-mau_hd
                              kh_hd = lw_hdon-kihieu_hd
                              amount = REDUCE #( INIT lw_amt TYPE int8 FOR lw_item IN lw_hdon-item NEXT lw_amt = lw_amt + lw_item-amount + lw_item-tax_amount + lw_item-phu_phi )
                             )
                          ).
  gs_mesh-bkc1 = VALUE #( FOR lw_bkc2 IN ls_input_kkcp-header
                            FOR lw_bkc1 IN lw_bkc2-item
                            (
                              bkc2_name = lw_bkc2-bk_id name = lw_bkc1-bk_id
                              hdon_count = REDUCE #( INIT lw_count TYPE int8 FOR lw_hdon IN ls_input_kkcp-hoa_don WHERE ( bk1_id = lw_bkc1-bk_id ) NEXT lw_count = lw_count + 1 )
                              amount = REDUCE #( INIT lw_amt TYPE int8 FOR lw_mesh_hdo IN gs_mesh-hdon WHERE ( bkc1_name = lw_bkc1-bk_id ) NEXT lw_amt = lw_amt + lw_mesh_hdo-amount )
                            )

                       ).
  gs_mesh-bkc2 = VALUE #( FOR lw_bkc2 IN ls_input_kkcp-header
                             (
                               name = lw_bkc2-bk_id
                               bkc1_count = lines( lw_bkc2-item )
                               amount = REDUCE #( INIT lw_amt TYPE int8 FOR lw_mesh_bkc1 IN gs_mesh-bkc1 WHERE (  bkc2_name = lw_bkc2-bk_id ) NEXT lw_amt = lw_amt + lw_mesh_bkc1-amount )
                             )
                        ).

  CALL SCREEN 0101.

* PERFORM setup_tree.

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
*  REFRESH :gt_fc_tree,gt_tree_data.
  DATA: ls_hier_hdr   TYPE treev_hhdr,
        ls_layout_src TYPE lvc_s_layo.
  CHECK go_tree_in IS INITIAL.
*  IF go_tree_in IS BOUND.
*    go_tree_in->free( ).
*  ENDIF.
*  IF go_cont_tree IS BOUND.
*    go_cont_tree->free( ).
*  ENDIF.
  PERFORM build_header CHANGING ls_hier_hdr.
  PERFORM init_alv.

  "display
  go_tree_in->set_table_for_first_display(
     EXPORTING
*      is_layout            = ls_layout_src
      is_hierarchy_header  = ls_hier_hdr
    CHANGING
      it_outtab            =  gt_tree_data
      it_fieldcatalog      =  gt_fc_tree
  ).


  PERFORM create_data_hier.
  CALL METHOD go_tree_in->update_calculations.
  CALL METHOD go_tree_in->frontend_update.

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
FORM build_header CHANGING lps_hier_hdr TYPE treev_hhdr..
  lps_hier_hdr-heading = 'Request detail'.
  lps_hier_hdr-width = 70.
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
        ls_tree_data LIKE LINE OF gt_tree_data,
        lt_dfies     TYPE ddfields.
  DATA :lo_cont_tree LIKE go_cont_tree.
  DEFINE set_fcat.
    ls_fcat-fieldname = &1.
    ls_fcat-scrtext_m = &2.
    ls_fcat-scrtext_l = &2.
    ls_fcat-scrtext_s = &2.
    ls_fcat-reptext = &2.
    ls_fcat-no_zero = 'X'.
    ls_fcat-currency = 'VND'.
    ls_fcat-outputlen = '30'.
    APPEND ls_fcat TO gt_fc_tree.
    CLEAR ls_fcat.
  END-OF-DEFINITION.
  go_cont_tree = NEW cl_gui_custom_container( container_name = 'CONT_TREE' ).
*  IF go_tree_in IS NOT BOUND.
  go_tree_in = NEW cl_gui_alv_tree(  parent =  go_cont_tree node_selection_mode = cl_gui_column_tree=>node_sel_mode_single item_selection = 'X' no_html_header = 'X' no_toolbar = '' ).
*  ENDIF.
*  TRY.
*      lobj_stdesc ?= cl_abap_structdescr=>describe_by_data( ls_tree_data ).
*    CATCH cx_root.
*      RAISE no_field_catalog.
*  ENDTRY.
*  lt_dfies =  cl_salv_data_descr=>read_structdescr( lobj_stdesc ).
*  gt_fc_tree = CORRESPONDING #( lt_dfies  ).

  set_fcat :"'NAME' '' 'Name',
          'COUNT' 'Count',
          'AMOUNT' 'Amount',
          'SO_HD' 'Số HD',
          'MAU_HD' 'Mẫu HD',
          'KH_HD' 'Kí hiệu'.
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


  DATA: lw_key_bk3 TYPE lvc_nkey,
        lw_key_bk2 TYPE lvc_nkey,
        lw_key_bk1 TYPE lvc_nkey,
        lw_key_hdo TYPE lvc_nkey,
        ls_data    LIKE LINE OF gt_tree_data.

  PERFORM add_header CHANGING lw_key_bk3 ls_data.

  LOOP AT gs_mesh-bkc2 INTO DATA(ls_bkc2).
*    ls_data-name = ls_bkc2-name.
    ls_data-amount = ls_bkc2-amount.
    ls_data-count = ls_bkc2-bkc1_count.
    PERFORM add_line USING lw_key_bk3 ls_bkc2-name CHANGING lw_key_bk2 ls_data .
    LOOP AT gs_mesh-bkc2\_bkc1[ ls_bkc2 ] INTO DATA(ls_bkc1).
      ls_data-amount = ls_bkc1-amount.
      ls_data-count = ls_bkc1-hdon_count.
      PERFORM add_line USING lw_key_bk2 ls_bkc1-name CHANGING lw_key_bk1 ls_data.
      LOOP AT gs_mesh-bkc1\_hdon[ ls_bkc1 ] INTO DATA(ls_hdon).
        ls_data = CORRESPONDING #( ls_hdon ).
        PERFORM add_line USING lw_key_bk1 ls_hdon-name CHANGING lw_key_hdo ls_data.
      ENDLOOP.
      CLEAR lw_key_bk1.
    ENDLOOP.
    CLEAR lw_key_bk2.
  ENDLOOP.




ENDFORM.

FORM add_header  CHANGING lpw_parr_key TYPE lvc_nkey
                          lps_data  LIKE LINE OF gt_tree_data.
  SPLIT p_key AT '/' INTO TABLE DATA(lt_spl).
  lps_data-amount = REDUCE #( INIT lw_amt TYPE int8 FOR lw_line IN gs_mesh-bkc2 NEXT lw_amt = lw_amt + lw_line-amount ).
  lps_data-count = REDUCE #( INIT lw_count TYPE int8 FOR lw_line IN gs_mesh-bkc2 NEXT lw_count = lw_count + 1 ).
  SELECT SINGLE * FROM tcurx WHERE currkey = 'VND' INTO @DATA(ls_curr).
  IF sy-subrc = 0.
    lps_data-amount = lps_data-amount * ( 10 ** ( ls_curr-currdec - 2 ) ).
  ENDIF.

  go_tree_in->add_node(
  EXPORTING
    i_relat_node_key     = ''                " Node Already in Tree Hierarchy
    i_relationship       = cl_gui_column_tree=>relat_last_child                 " How to Insert Node
    i_node_text      = |{ lt_spl[ 1 ] }|
    is_outtab_line   = lps_data
  IMPORTING
    e_new_node_key   = lpw_parr_key
  ).

  CLEAR lps_data.
ENDFORM.
FORM add_line  USING    "lps_data LIKE LINE OF gt_tree_data
                        lpw_parr_key TYPE lvc_nkey
                        lpw_name TYPE string
                CHANGING lpw_key_new TYPE lvc_nkey
                         lps_data LIKE LINE OF gt_tree_data.
*                        lpw_new_par TYPE char1.
  SELECT SINGLE * FROM tcurx WHERE currkey = 'VND' INTO @DATA(ls_curr).
  IF sy-subrc = 0.
    lps_data-amount = lps_data-amount * ( 10 ** ( ls_curr-currdec - 2 ) ).
  ENDIF.

  go_tree_in->add_node(
    EXPORTING
      i_relat_node_key     = lpw_parr_key                " Node Already in Tree Hierarchy
      i_relationship       = cl_gui_column_tree=>relat_last_child                 " How to Insert Node
      i_node_text      = | { lpw_name }|
      is_outtab_line   = lps_data
    IMPORTING
      e_new_node_key   = lpw_key_new
  ).
  CLEAR lps_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_UP_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_up_alv .
  DATA :lt_fcat TYPE lvc_t_fcat .
  PERFORM set_fcat_alv CHANGING lt_fcat.
  PERFORM view_alv USING lt_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FCAT_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_fcat_alv CHANGING lps_fcat TYPE lvc_t_fcat.
  DATA :ls_fcat TYPE lvc_s_fcat.
  DEFINE set_fcat_alv.
    ls_fcat-fieldname = &1.
    ls_fcat-scrtext_l = &2.
    ls_fcat-scrtext_m = &2.
    ls_fcat-scrtext_s = &2.
    ls_fcat-currency = 'VND'.
    ls_fcat-no_zero = 'X'.
    APPEND ls_fcat TO lps_fcat.
    CLEAR ls_fcat.
  END-OF-DEFINITION.
  set_fcat_alv : 'BKC3' 'B.Kê C3',
                 'BKC2' 'B.Kê C2',
                 'BKC1' 'B.Kê C1',
                 'DOC' 'Doc number',
                 'YEAR' 'Year',
                 'BUKRS' 'Comp Code',
                 'COUNT' 'Child node',
                 'AMOUNT' 'Amount',
                 'CTGS' 'Ctgs',
                 'USER' 'User'.

  IF go_cont_alv IS BOUND.
    go_cont_alv->free( ).
  ENDIF.
  go_cont_alv = NEW cl_gui_custom_container( container_name = 'CONT_ALV' ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_FCAT
*&---------------------------------------------------------------------*
FORM view_alv  USING lpt_fcat TYPE lvc_t_fcat.
  DATA:ls_layout_src TYPE lvc_s_layo,
       ls_layout     TYPE disvariant.
  DATA :lo_alv              TYPE REF TO cl_gui_alv_grid.
  ls_layout-report = sy-repid.
  ls_layout_src-cwidth_opt = 'X'.
*  ls_layout_src-zebra = 'X'.
  ls_layout_src-sel_mode = 'A'.
  ls_layout_src-ctab_fname = 'CELL_COLOR'.
  lo_alv  = NEW  cl_gui_alv_grid( i_parent = go_cont_alv ).
  CALL METHOD lo_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = ls_layout_src
      is_variant                    = ls_layout
      i_save                        = 'X'
*     i_structure_name              = p_structure_name
*     it_toolbar_excluding          = lt_exclude
    CHANGING
      it_outtab                     = gt_alv_data
      it_fieldcatalog               = lpt_fcat
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
*& Form READ_IN_OUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LW_IN
*&---------------------------------------------------------------------*
FORM read_in_out USING lpw_type TYPE char1 CHANGING lpw_res TYPE string.
  CASE lpw_type.
    WHEN 'I'.
      SELECT SINGLE request FROM ztb_api_io_log WHERE guid = @p_key AND object = 'ZKKCP' INTO @lpw_res.
    WHEN 'O'.
      SELECT SINGLE response FROM ztb_api_io_log WHERE guid = @p_key AND object = 'ZKKCP' INTO @lpw_res.
  ENDCASE.
  DATA(json_xstring) = cl_abap_conv_codepage=>create_out( )->convert( lpw_res ).
  DATA(reader) = cl_sxml_string_reader=>create( json_xstring ).
  DATA(writer) = CAST if_sxml_writer( cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ) ).
  writer->set_option( option = if_sxml_writer=>co_opt_linebreaks ).
  writer->set_option( option = if_sxml_writer=>co_opt_indent ).
  reader->next_node( ).
  reader->skip_node( writer ).
  lpw_res = cl_abap_conv_codepage=>create_in( )->convert( CAST cl_sxml_string_writer( writer )->get_output( ) ).
ENDFORM.
