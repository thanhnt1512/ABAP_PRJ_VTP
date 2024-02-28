*&---------------------------------------------------------------------*
*& Include          ZPG_CREATE_XDCBDD_CL
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:  handle_set_title FOR EVENT top_of_page OF cl_gui_alv_grid
      IMPORTING e_dyndoc_id.
    METHODS : handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed e_onf4_after e_ucomm.
    METHODS : handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_set_title.
    DATA : lw_text TYPE sdydo_text_element.
    DATA :r_dd_tab TYPE REF TO cl_dd_table_element.

*    g_doc_header->add_table( no_of_columns = 2 width = '40%' tablearea =  r_dd_tab ).
    CALL METHOD g_doc_header->add_table
      EXPORTING
        no_of_columns = 2
*       border        = '1'
        width         = '100%'
      IMPORTING
*       table         =
        table         = r_dd_tab
*      EXCEPTIONS
*       table_already_used          = 1
*       others        = 2
      .
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.

    CALL METHOD r_dd_tab->add_column
      EXPORTING
        width  = '50%'
      IMPORTING
        column = DATA(ld_col1).

    CALL METHOD r_dd_tab->add_column
      EXPORTING
        width  = '50%'
      IMPORTING
        column = DATA(ld_col2).


    ld_col1->add_text( text  = 'Status :' sap_emphasis = cl_dd_area=>strong ) .
    lw_text = COND #( WHEN g_stt = 'S' THEN 'Success' WHEN  g_stt = 'E' THEN 'Error' ELSE '').
    ld_col1->add_text(  text  = lw_text
                        sap_emphasis = cl_dd_area=>sans_serif
                        sap_style = COND #( WHEN g_stt = 'S' THEN cl_dd_area=>success ELSE  cl_dd_area=>warning ) ).
    ld_col1->new_line( ).
    ld_col1->new_line( ).
    ld_col1->add_text( text  = 'Completed Asset Number :' sap_emphasis = cl_dd_area=>strong ).
    lw_text = |{ gs_ass_get-anln1 ALPHA = OUT }|.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>strong  sap_color = cl_dd_area=>list_negative_inv ).
    ld_col1->new_line( ).
    ld_col1->new_line( ).
    ld_col1->add_text( text  = 'Asset Class :' sap_emphasis = cl_dd_area=>sans_serif ).
    lw_text = gs_ass_get-anlkl.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col1->new_line( ).
    ld_col1->new_line( ).
    ld_col1->add_text( text  = 'Asset Description :' sap_emphasis = cl_dd_area=>sans_serif ).
    lw_text = gs_ass_get-txt50.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col1->new_line( ).
    ld_col1->new_line( ).
    ld_col1->add_text( text  = 'Total Asset Value :' sap_emphasis = cl_dd_area=>sans_serif ).
    WRITE gw_tt_as TO lw_text CURRENCY 'VND'.
    CONDENSE lw_text.
*    lw_text = gw_tt_as.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
*    lw_text = p_as_c.


*    ld_col2->new_line( ).
    ld_col2->add_text( text  = 'FI Posting Date :') .
    WRITE p_budat TO lw_text DD/MM/YYYY.
    ld_col2->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col2->new_line( ).
    ld_col2->new_line( ).
    ld_col2->add_text( text  = 'FI Document Date :') .
    WRITE p_bldat TO lw_text DD/MM/YYYY.
    ld_col2->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col2->new_line( ).
    ld_col2->new_line( ).
    ld_col2->add_text( text  = 'FI Document Text :') .
    lw_text = p_as_des.
    ld_col2->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
*    ld_col2->new_line( ).

    g_doc_header->merge_document( ).
    g_doc_header->display_document(  parent = g_cont_header ).

  ENDMETHOD.


  METHOD handle_data_changed.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_mt_good_cells).
      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) INDEX  ls_mt_good_cells-row_id.
      IF sy-subrc = 0.
        CASE ls_mt_good_cells-fieldname.
          WHEN 'AS_SET_PER'. "OR 'AS_AMT_RULE'.
            <fs_data>-as_set_amt = <fs_data>-as_amt * ( ls_mt_good_cells-value / 100 ) + <fs_data>-as_amt_rule.
*            IF NOT ( ls_mt_good_cells-value * <fs_data>-as_amt_rule = 0 AND ls_mt_good_cells-value + <fs_data>-as_amt_rule <> 0 ).
*              MESSAGE |{ <fs_data>-anln1 }: Settlement Percentage và Amount Rule không hợp lệ| TYPE 'I' DISPLAY LIKE 'E'.
*
*            ELSE.
*              <fs_data>-as_set_amt = <fs_data>-as_amt * ( ls_mt_good_cells-value / 100 ) + <fs_data>-as_amt_rule.
*            ENDIF.

          WHEN 'AS_AMT_RULE'.
            <fs_data>-as_set_amt = ls_mt_good_cells-value + <fs_data>-as_amt * ( <fs_data>-as_set_per / 100 ).
*            IF NOT ( ls_mt_good_cells-value * <fs_data>-as_set_per = 0 AND ls_mt_good_cells-value + <fs_data>-as_set_per <> 0 ).
*              MESSAGE |{ <fs_data>-anln1 }: Settlement Percentage và Amount Rule không hợp lệ| TYPE 'I' DISPLAY LIKE 'E'.
*            ELSE.
*              <fs_data>-as_set_amt = ls_mt_good_cells-value + <fs_data>-as_amt * ( <fs_data>-as_set_per / 100 ).
*            ENDIF.

        ENDCASE.
      ENDIF.
    ENDLOOP.
    g_alv->refresh_table_display( ).
  ENDMETHOD.

  METHOD handle_hotspot_click .

    CASE e_column_id.
      WHEN 'FI_DOC'.
        DATA(ls_data) = gt_data[ e_row_id ].
        CHECK ls_data-fi_doc IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD ls_data-fi_doc.
        SET PARAMETER ID 'BUK' FIELD p_comp.
        SET PARAMETER ID 'GJR' FIELD p_budat+0(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      WHEN 'AS_RULE_GR'.
        DATA :ls_anla TYPE anla.
        ls_data = gt_data[ e_row_id ].
        CHECK ls_data-as_rule_gr <> '000'.
        CALL METHOD cl_faa_mdo_services=>read_asset_for_posting
          EXPORTING
            iv_comp_code   = p_comp
            iv_asset_no    = CONV #( |{ ls_data-anln1 ALPHA = IN }| )
            iv_asset_subno = '0000'
            ib_lock        = 'X'                              "2069770
          IMPORTING
            es_anla        = ls_anla.
        CALL FUNCTION 'K_SETTLEMENT_RULE_CALL'
          EXPORTING
            aprof  = 'AI'
            ibureg = ls_data-as_rule_gr
            mode   = 'AUCL'
            objnr  = ls_anla-objnr.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

    ENDCASE.


  ENDMETHOD.
ENDCLASS.
