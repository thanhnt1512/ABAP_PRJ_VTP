*&---------------------------------------------------------------------*
*& Include          ZIN_READ_IN_OUT_F01
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
  DATA :lt_event       TYPE slis_t_event,
        ls_layout_slis TYPE slis_layout_alv,
        lt_fieldcat    TYPE slis_t_fieldcat_alv.
  REFRESH gt_log.
  SELECT * FROM ztb_api_io_log INTO TABLE @gt_log WHERE cr_date IN @s_date AND cr_time IN @s_time AND object = @p_object AND guid IN @s_guid.
  IF sy-subrc <> 0.
    MESSAGE 'Không có dữ liệu !' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  lt_event = VALUE #( ( name = 'USER_COMMAND' form = 'UC_CLICK') ).
  ls_layout_slis-colwidth_optimize = 'X'.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZTB_API_IO_LOG'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  PERFORM change_fcat CHANGING lt_fieldcat.

  IF sy-subrc <> 0.
    MESSAGE ID   sy-msgid
    TYPE 'E'
    NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-cprog
      i_grid_title       = 'List IO'
*     i_structure_name   = 'ZTB_LOG_BAL_CONT'
      is_layout          = ls_layout_slis
      it_fieldcat        = lt_fieldcat
      it_events          = lt_event
    TABLES
      t_outtab           = gt_log.

  IF sy-subrc <> 0.
    MESSAGE ID   sy-msgid
    TYPE 'E'
    NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
FORM uc_click USING ucomm LIKE sy-ucomm selfied TYPE slis_selfield.
  IF ucomm = '&IC1' AND selfied-fieldname = 'GUID'.
    CLEAR gw_guid.
    DATA(ls_data) = gt_log[ selfied-tabindex ].
    CHECK ls_data-guid IS NOT INITIAL.
    gw_guid = ls_data-guid.
    CALL SCREEN '0100'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SETUP_TEXT_VIEW
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM setup_text_view USING lpw_guid TYPE char50.
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
*  lo_txt_i = NEW  cl_gui_textedit( parent = go_cont_i wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position ).
*  lo_txt_o = NEW  cl_gui_textedit( parent = go_cont_o wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position ).
  lo_txt_i = NEW  cl_gui_textedit( parent = go_cont_i wordwrap_to_linebreak_mode = cl_gui_textedit=>true ).
  lo_txt_o = NEW  cl_gui_textedit( parent = go_cont_o wordwrap_to_linebreak_mode = cl_gui_textedit=>true ).
  PERFORM read_in_out USING 'I' lpw_guid CHANGING lw_in.
  CALL METHOD lo_txt_i->set_textstream
    EXPORTING
      text = lw_in.
*  PERFORM read_text USING '1010' CHANGING lw_
  PERFORM read_in_out USING 'O' lpw_guid CHANGING lw_out.
  CALL METHOD lo_txt_o->set_textstream
    EXPORTING
      text = lw_out.

ENDFORM.

FORM read_in_out USING lpw_type TYPE char1
                       lpw_guid TYPE char50
                CHANGING lpw_res TYPE string.
  CASE lpw_type.
    WHEN 'I'.
      SELECT SINGLE request FROM ztb_api_io_log WHERE guid = @lpw_guid AND object = @p_object INTO @lpw_res.
    WHEN 'O'.
      SELECT SINGLE response FROM ztb_api_io_log WHERE guid = @lpw_guid AND object = @p_object INTO @lpw_res.
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
*&---------------------------------------------------------------------*
*& Form CHANGE_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_FIELDCAT
*&---------------------------------------------------------------------*
FORM change_fcat  CHANGING lpt_fieldcat TYPE slis_t_fieldcat_alv.
  FIELD-SYMBOLS :<fs_fcat> LIKE LINE OF lpt_fieldcat.
  DEFINE set_fcat.
    READ TABLE lpt_fieldcat ASSIGNING <fs_fcat> WITH KEY fieldname = &1.
    IF sy-subrc = 0.
      <fs_fcat>-seltext_l = &2.
      <fs_fcat>-seltext_m = &2.
      <fs_fcat>-seltext_s = &2.
      IF &1 = 'GUID'.
       <fs_fcat>-hotspot = 'X'.
      ENDIF.
    ENDIF.
  END-OF-DEFINITION.

  set_fcat :'GUID' 'Guid',
            'OBJECT' 'Object',
            'REQUEST' 'Request',
            'RESPONSE' 'Reponse',
            'CR_DATE' 'Create Date',
            'CR_TIME' 'Create Time'.
ENDFORM.
