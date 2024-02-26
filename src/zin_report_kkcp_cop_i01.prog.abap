*&---------------------------------------------------------------------*
*& Include          ZIN_REPORT_KKCP_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      go_cont_tree->free( ).
      FREE :go_tree_in,go_cont_tree.
      REFRESH gt_tree_data[].
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
*  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
