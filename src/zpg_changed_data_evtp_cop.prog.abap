*&---------------------------------------------------------------------*
*& Report ZPG_CHANGED_DATA_EVTP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE ZPG_CHANGED_DATA_EVTP_COP_TOP.
*INCLUDE zpg_changed_data_evtp_top.
INCLUDE ZPG_CHANGED_DATA_EVTP_COP_CL.
*INCLUDE zpg_changed_data_evtp_cl.
INCLUDE ZPG_CHANGED_DATA_EVTP_COP_I01.
*INCLUDE zpg_changed_data_evtp_i01.
INCLUDE ZPG_CHANGED_DATA_EVTP_COP_O01.
*INCLUDE zpg_changed_data_evtp_o01.
INCLUDE ZPG_CHANGED_DATA_EVTP_COP_F01.
*INCLUDE zpg_changed_data_evtp_f01.

START-OF-SELECTION.
  PERFORM main.
