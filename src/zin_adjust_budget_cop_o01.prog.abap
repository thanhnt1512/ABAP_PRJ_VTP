*&---------------------------------------------------------------------*
*& Include          ZIN_ADJUST_BUDGET_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module INIT_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_0100 OUTPUT.
  PERFORM setup_tree.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: lt_exclude TYPE STANDARD TABLE OF sy-ucomm.
  REFRESH lt_exclude.
  IF gw_stt = 'Success' OR gw_stt = ''.
    APPEND 'VIEWLOG' TO lt_exclude.
  ENDIF.
  SET PF-STATUS 'STT_0100' EXCLUDING lt_exclude.

**  SET PF-STATUS 'STT_0100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
