*&---------------------------------------------------------------------*
*& Include          ZPG_CHANGED_DATA_EVTP_I01
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
    WHEN OTHERS.
*      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
*  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'TAB1'.
      tabstr-activetab = 'TAB1'.
      gw_type =  'GTY_TRANS_H'.
    WHEN 'TAB2'.
      tabstr-activetab = 'TAB2'.
      gw_type =  'GTY_TRANS_I'.
    WHEN OTHERS.
      CLEAR gw_type.
*    WHEN OTHERS.
*      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
*  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0302  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0302 INPUT.


ENDMODULE.
