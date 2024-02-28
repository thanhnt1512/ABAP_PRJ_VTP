*&---------------------------------------------------------------------*
*& Include          ZIN_BLOCK_BUDGET_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'TEST' OR 'RUN'.
      CASE 'X'.
        WHEN rd_bl.
          PERFORM execute_block USING sy-ucomm.
        WHEN rd_rt.
          PERFORM execute_return USING sy-ucomm.
      ENDCASE.

  ENDCASE.
ENDMODULE.
