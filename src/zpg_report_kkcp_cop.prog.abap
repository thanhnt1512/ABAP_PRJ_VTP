*&---------------------------------------------------------------------*
*& Report ZPG_REPORT_KKCP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE ZIN_REPORT_KKCP_COP_TOP.
*INCLUDE zin_report_kkcp_top.
INCLUDE ZIN_REPORT_KKCP_COP_F01.
*INCLUDE zin_report_kkcp_f01.
INCLUDE ZIN_REPORT_KKCP_COP_I01.
*INCLUDE zin_report_kkcp_i01.
INCLUDE ZIN_REPORT_KKCP_COP_O01.
*INCLUDE zin_report_kkcp_o01.

INITIALIZATION.
  CONCATENATE icon_search 'Find In/Out'
                 INTO sscrfields-functxt_01 SEPARATED BY space.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_key.
*  SELECT * FROM ztb_api_io_log INTO TABLE @gt_tb_sh.
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield    = 'GUID'
*      dynpprog    = sy-cprog
*      dynpnr      = sy-dynnr
*      dynprofield = 'P_KEY'
*      value_org   = 'S'
*    TABLES
*      value_tab   = gt_tb_sh.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM find_in_out.
  ENDCASE.


START-OF-SELECTION.
  PERFORM main.
