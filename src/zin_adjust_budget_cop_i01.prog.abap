*&---------------------------------------------------------------------*
*& Include          ZIN_ADJUST_BUDGET_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
*      go_cont_tree->free( ).
*      FREE :go_tree_alv,go_cont_tree.
*      REFRESH gt_data_tree[].
      LEAVE TO SCREEN 0.
    WHEN 'POST'.
      PERFORM post_all.
    WHEN 'VIEWLOG'.
      PERFORM buid_fcat_log.
      PERFORM display_log.
*      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
ENDMODULE.
