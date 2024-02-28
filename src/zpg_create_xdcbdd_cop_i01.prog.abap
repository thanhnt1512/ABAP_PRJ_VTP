*&---------------------------------------------------------------------*
*& Include          ZPG_CREATE_XDCBDD_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR :gw_ass_crd,gs_ass_get,g_stt,gw_tt_as.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
*    WHEN 'TEST'.
*      PERFORM SET_RULE_SETTLE USING 'T'.
    WHEN 'SETTLE'.
      PERFORM set_rule_settle .
    WHEN 'VIEW_ASSET'.
      PERFORM view_asset.
  ENDCASE.
ENDMODULE.
