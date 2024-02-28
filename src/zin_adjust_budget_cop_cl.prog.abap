*&---------------------------------------------------------------------*
*& Include          ZIN_ADJUST_BUDGET_CL
*&---------------------------------------------------------------------*
CLASS lcl_event DEFINITION.

  PUBLIC SECTION.
* §4. Define an event handler method to react to fired function codes
*     of the toolbar.                   .
    METHODS: on_function_selected
                  FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode,
             handle_set_title FOR EVENT top_of_page OF cl_gui_alv_tree.

ENDCLASS.

CLASS lcl_event IMPLEMENTATION.
*
  METHOD on_function_selected.
    DATA: lt_selected_nodes TYPE lvc_t_nkey,
          l_selected_node   TYPE lvc_nkey,
          l_rc              TYPE c.

* §5. Query the function codes of the toolbar in your implementation.
    CASE fcode.
      WHEN 'POST'.
        BREAK-POINT.
    ENDCASE.
  ENDMETHOD.
  METHOD handle_set_title.
    DATA :r_dd_tab TYPE REF TO cl_dd_table_element,
          lw_text  TYPE sdydo_text_element.
    DATA: lv_stype TYPE sdydo_attribute.
    CASE gw_stt.
      WHEN 'Error'.
        lv_stype = cl_dd_area=>warning.
      WHEN 'Success'.
        lv_stype = cl_dd_area=>success.
      WHEN OTHERS.
    ENDCASE.
    CALL METHOD go_doc_hdr->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = r_dd_tab.
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

    ld_col1->add_text( text  = 'FM Area : ' sap_emphasis = cl_dd_area=>strong ).
    WRITE p_fma TO lw_text.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col1->new_line( ).
    ld_col1->add_text( text  = 'Fiscal Year : ' sap_emphasis = cl_dd_area=>strong ).
    WRITE p_year TO lw_text.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col1->new_line( ).
    ld_col1->add_text( text  = 'Document Date : ' sap_emphasis = cl_dd_area=>strong ).
    WRITE p_date TO lw_text.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col1->new_line( ).
    ld_col1->add_text( text  = 'Period : ' sap_emphasis = cl_dd_area=>strong ).
    WRITE p_per TO lw_text.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col1->new_line( ).
    ld_col1->add_text( text  = 'Budget Period : ' sap_emphasis = cl_dd_area=>strong ).
    WRITE gw_bud_per TO lw_text.
    ld_col1->add_text( text  = lw_text sap_emphasis = cl_dd_area=>sans_serif ).
    ld_col1->new_line( ).
    ld_col2->add_text( text  = 'Post Status: ' sap_emphasis = cl_dd_area=>strong ).
    ld_col2->add_text( text  = |{ gw_stt }| sap_emphasis = cl_dd_area=>sans_serif sap_style = lv_stype ).
    IF gw_stt = 'Success'.
      ld_col2->new_line( ).
      ld_col2->add_text( text  = 'Doc Return: ' sap_emphasis = cl_dd_area=>strong ).
      ld_col2->add_text( text  = |{ gw_doc_retn }| sap_emphasis = cl_dd_area=>sans_serif sap_style = cl_dd_area=>strong ).
      ld_col2->new_line( ).
      ld_col2->add_text( text  = 'Doc Enter: ' sap_emphasis = cl_dd_area=>strong ).
      ld_col2->add_text( text  = |{ gw_doc_entr }| sap_emphasis = cl_dd_area=>sans_serif sap_style = cl_dd_area=>strong ).
    ENDIF.
    go_doc_hdr->merge_document( ).
    go_doc_hdr->display_document(  parent = go_cont_hdr ).
  ENDMETHOD.


ENDCLASS.
