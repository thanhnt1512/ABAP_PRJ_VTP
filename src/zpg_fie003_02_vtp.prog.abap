*&---------------------------------------------------------------------*
*& Report ZPG_FIE003_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZIN_FIE003_02_VTP_TOP.
*include zin_fie003_02_top                       .    " Global Data

* INCLUDE ZIN_FIE003_02_O01                       .  " PBO-Modules
* INCLUDE ZIN_FIE003_02_I01                       .  " PAI-Modules
INCLUDE ZIN_FIE003_02_VTP_F01.
*include zin_fie003_02_f01                       .  " FORM-Routines
*---------------------------------------------------------------------*
*       I N I T I A L I Z A T I O N
*---------------------------------------------------------------------*
initialization.
  CALL METHOD zcl_utility=>get_tvarv_s(
    EXPORTING
      i_name    = 'ZSV_DOCTYPE_PARTNER_GR_UP'
      i_na_add  = 'X'
    IMPORTING
      e_t_range = gr_type_pgr_up ).
  CALL METHOD zcl_utility=>get_tvarv_s(
    EXPORTING
      i_name    = 'ZSV_ACC_PARTNER_GR_UP'
      i_na_add  = 'X'
    IMPORTING
      e_t_range = gr_acc_pgr_up ).
  SELECT * FROM ztb_partner_gr INTO TABLE gt_partner_gr.
  SORT gt_partner_gr BY partner_value.

at selection-screen output.
  perform modify_screen.

*---------------------------------------------------------------------*
*       S E A R C H   H E L P                                         *
*---------------------------------------------------------------------*
at selection-screen on value-request for p_fpath.
  perform get_path.

at selection-screen on value-request for p_frunn.
  perform get_server_file_help.
*---------------------------------------------------------------------*
*       S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------*
start-of-selection.
  perform check_al11_folder.
  case 'X'.
    when p_upload.
      perform upload_file.
    when p_downlo.
      perform download_file.
    when p_remove.
      perform remove_file.
    when others.
* Get data
      perform get_data.
* Process data
      perform process_data.
* Display data
      perform display_data.
  endcase.
