*&---------------------------------------------------------------------*
*& Report ZPG_CREATE_XDCBDD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZPG_CREATE_XDCBDD_COP_TOP.
*INCLUDE zpg_create_xdcbdd_top.
INCLUDE ZPG_CREATE_XDCBDD_COP_CL.
*INCLUDE zpg_create_xdcbdd_cl.
INCLUDE ZPG_CREATE_XDCBDD_COP_F01.
*INCLUDE zpg_create_xdcbdd_f01.
INCLUDE ZPG_CREATE_XDCBDD_COP_I01.
*INCLUDE zpg_create_xdcbdd_i01.
INCLUDE ZPG_CREATE_XDCBDD_COP_O01.
*INCLUDE zpg_create_xdcbdd_o01.

INITIALIZATION.
  sscrfields-functxt_01 = icon_history && 'History'.
  tab1 = 'AuC Selection'.
  tab2 = 'Completed Asset'.
  tab3 = 'Additional'.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN .
    IF ( ( screen-group1 = 'T1' OR screen-group1 = 'T21' OR screen-group1 = 'T22' OR screen-group1 = 'T3' ) AND screen-name <> 'P_AS_OR' )."
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.

    IF p_rd1 = 'X'.
      IF screen-group1 = 'T22'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'T21'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 = 'T21'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'T22'.
        screen-active = '1'.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM display_history.
  ENDCASE.

START-OF-SELECTION.
  PERFORM check_required_field.
  PERFORM main.
