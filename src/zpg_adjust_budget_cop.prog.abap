*&---------------------------------------------------------------------*
*& Report ZPG_ADJUST_BUDGET
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE ZIN_ADJUST_BUDGET_COP_TOP.
*INCLUDE zin_adjust_budget_top.
INCLUDE ZIN_ADJUST_BUDGET_COP_CL.
*INCLUDE zin_adjust_budget_cl.
INCLUDE ZIN_ADJUST_BUDGET_COP_I01.
*INCLUDE zin_adjust_budget_i01.
INCLUDE ZIN_ADJUST_BUDGET_COP_O01.
*INCLUDE zin_adjust_budget_o01.
INCLUDE ZIN_ADJUST_BUDGET_COP_F01.
*INCLUDE zin_adjust_budget_f01.

INITIALIZATION.
  CONCATENATE icon_history 'History'
               INTO sscrfields-functxt_01 SEPARATED BY space.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'OBL'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM display_history.
  ENDCASE.

START-OF-SELECTION.
  PERFORM check_require.
  PERFORM main.
