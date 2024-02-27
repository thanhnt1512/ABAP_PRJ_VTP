*&---------------------------------------------------------------------*
*& Include          ZPG_CHANGED_DATA_EVTP_CL
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handler_node_click FOR EVENT node_double_click OF cl_gui_alv_tree IMPORTING node_key,
      handle_set_title_301 FOR EVENT top_of_page OF cl_gui_alv_grid IMPORTING e_dyndoc_id,
      handle_set_title_302 FOR EVENT top_of_page OF cl_gui_alv_grid IMPORTING e_dyndoc_id,
      handle_add_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handler_node_click.
    go_alv->get_outtab_line(
      EXPORTING
        i_node_key     =  node_key                " Node Key
      IMPORTING
        e_node_text    = gw_file              " node text
    ).
    IF sy-subrc = 0.
      REFRESH :gt_header,gt_item,gt_data.
      FREE :go_alv_grid.
      IF <fs_evtp_tab1_g> IS ASSIGNED.
        UNASSIGN <fs_evtp_tab1_g>.
      ENDIF.
      IF <fs_trans_tab2_g> IS ASSIGNED.
        UNASSIGN <fs_trans_tab2_g>.
      ENDIF.

      PERFORM process_file USING gw_file.
      PERFORM build_data TABLES gt_header gt_item USING gw_file.
      gw_type = 'GTY_TRANS_H'.
      CALL SCREEN 0300.

    ENDIF.

  ENDMETHOD.
  METHOD handle_set_title_301.
    DATA : lw_text TYPE sdydo_text_element.
    e_dyndoc_id->add_gap( width = 100 ).
    e_dyndoc_id->add_text( text  = 'Dữ liệu từ EVTP' sap_style = cl_dd_area=>heading  ).
    e_dyndoc_id->display_document(  parent = go_container_header1 ).
  ENDMETHOD.
  METHOD handle_set_title_302.
    DATA : lw_text TYPE sdydo_text_element.
    e_dyndoc_id->add_gap( width = 100 ).
    e_dyndoc_id->add_text( text  = 'Dữ liệu từ bảng ZTRANS' sap_style = cl_dd_area=>heading ).
    e_dyndoc_id->display_document(  parent = go_container_header2 ).
  ENDMETHOD.


  METHOD handle_add_toolbar.
    DATA: ls_toolbar TYPE stb_button.
    MOVE 'UPDATE' TO ls_toolbar-function.
    MOVE icon_operation TO ls_toolbar-icon.
    MOVE 'Update' TO ls_toolbar-text.
    MOVE 'Update' TO ls_toolbar-quickinfo.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'SELECT_ALL' TO ls_toolbar-function.
    MOVE icon_select_all TO ls_toolbar-icon.
    MOVE 'Select all' TO ls_toolbar-text.
    MOVE 'Select all' TO ls_toolbar-quickinfo.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'DESELECT_ALL' TO ls_toolbar-function.
    MOVE icon_deselect_all TO ls_toolbar-icon.
    MOVE 'Deselect all' TO ls_toolbar-text.
    MOVE 'Deselect all' TO ls_toolbar-quickinfo.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.


  METHOD handle_user_command.
    DATA: lt_index TYPE lvc_t_row.
    CASE e_ucomm.
      WHEN 'UPDATE'.
        CASE gw_type.
          WHEN 'GTY_TRANS_H'.
            PERFORM update_trans_h.
          WHEN 'GTY_TRANS_I'.
            PERFORM update_trans_i.
        ENDCASE.
      WHEN 'SELECT_ALL'.
        PERFORM select_all.
      WHEN 'DESELECT_ALL'.
        PERFORM deselect_all.

*      bukrs = ls_header_trans-bukrs  gjahr = ls_header_trans-gjahr  zid_no = ls_header_trans-zid_no zid_type =  ls_header_trans-zid_type
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
