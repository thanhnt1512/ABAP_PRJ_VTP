*&---------------------------------------------------------------------*
*& Report ZPG_BLOCK_BUDGET
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZIN_BLOCK_BUDGET_COP_TOP.
*INCLUDE zin_block_budget_top.
INCLUDE ZIN_BLOCK_BUDGET_COP_F01.
*INCLUDE zin_block_budget_f01.
INCLUDE ZIN_BLOCK_BUDGET_COP_O01.
*INCLUDE zin_block_budget_o01.
INCLUDE ZIN_BLOCK_BUDGET_COP_I01.
*INCLUDE zin_block_budget_i01.


INITIALIZATION.
  CONCATENATE icon_history 'History'
                 INTO sscrfields-functxt_01 SEPARATED BY space.
  APPEND INITIAL LINE TO s_fun_p[] ASSIGNING FIELD-SYMBOL(<fs_fun_p>).
  <fs_fun_p>-sign = 'I'.
  <fs_fun_p>-option = 'EQ'.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM display_history.
  ENDCASE.

  IF sy-batch = 'X'.
    p_period = p_budat+4(2).
  ENDIF.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'OB'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_budcat.

  SELECT rldnr FROM t881 WHERE tab = 'FMAVCT' INTO TABLE @DATA(lt_tab_budcat).
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'RLDNR'
      dynprofield = 'p_budcat'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      value_org   = 'S'
    TABLES
      value_tab   = lt_tab_budcat.

START-OF-SELECTION.
  IF p_area IS INITIAL OR p_year IS INITIAL OR p_period IS INITIAL OR p_budat IS INITIAL.
    MESSAGE 'Fill out all required entry fields' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  PERFORM main.
