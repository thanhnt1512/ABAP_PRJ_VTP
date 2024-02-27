*&---------------------------------------------------------------------*
*& Include          ZPG_CHANGED_DATA_EVTP_TOP
*&---------------------------------------------------------------------*
REPORT zpg_changed_data_evtp.
TABLES :agr_string.
PARAMETERS :p_path TYPE string OBLIGATORY LOWER CASE.
SELECT-OPTIONS:s_file FOR agr_string-string1 NO INTERVALS.
SELECT-OPTIONS :s_date FOR sy-datum ,
                s_time FOR sy-uzeit.

DATA: gt_file_list TYPE STANDARD TABLE OF zeps2fili,
      go_cont      TYPE REF TO cl_gui_custom_container,
      go_alv       TYPE REF TO cl_gui_alv_tree.
CONTROLS :tabstr TYPE TABSTRIP.
TYPES :BEGIN OF gty_data_4_build,
         parent TYPE string,
         name   TYPE eps2filnam,
         date   TYPE sy-datum,
         time   TYPE sy-uzeit,
*         size   TYPE eps2filsiz,
*         mtim   TYPE eps2timestmp,
       END OF gty_data_4_build.

TYPES :BEGIN OF gty_data_tree,
*         name   TYPE eps2filnam,
         date TYPE sy-datum,
         time TYPE sy-uzeit,
       END OF gty_data_tree.

DATA :gt_data_4_build TYPE TABLE OF gty_data_4_build,
      gt_data_tree    TYPE TABLE OF gty_data_tree,
      gt_fcat         TYPE lvc_t_fcat,
      gs_data_tree    TYPE gty_data_tree,
      gw_file         TYPE lvc_value,
      gt_header       TYPE TABLE OF zev_trans_h,
      gt_item         TYPE TABLE OF zev_trans_i.
TYPES: BEGIN OF gty_data,
         h_bukrs         TYPE string,
         h_gjahr         TYPE string,
         h_zbelnr        TYPE string,
         h_zkmuc         TYPE string,
         h_zid_no        TYPE string,
         h_zid_type      TYPE string,
         h_zid_ref1      TYPE string,
         h_zid_ref2      TYPE string,
         h_zstatus       TYPE string,
         h_zbcps         TYPE string,
         h_zprctr1       TYPE string,
         h_zcnps         TYPE string,
         h_zprctr2       TYPE string,
         h_budat         TYPE string,
         h_ev_partner1   TYPE string,
         h_zpartner1     TYPE string,
         h_ev_partner_bc TYPE string,
         h_ev_userid     TYPE string,
         h_amount_h      TYPE string,
         h_rhcur         TYPE string,
         h_ztknh         TYPE string,
         h_znh           TYPE string,
         h_zcnnh         TYPE string,
         h_zpl           TYPE string,
         h_bktxt         TYPE string,
         h_lastupdate    TYPE string,
         h_create_time   TYPE string,
         h_zid_no_des    TYPE string,
         h_zid_ref1_des  TYPE string,
         h_zid_ref2_des  TYPE string,
         i_bukrs         TYPE string,
         i_gjahr         TYPE string,
         i_zbelnr        TYPE string,
         i_docln         TYPE string,
         i_zid_no        TYPE string,
         i_zid_type      TYPE string,
         i_ev_partner1   TYPE string,
         i_zpartner1     TYPE string,
         i_ev_partner_bc TYPE string,
         i_ev_userid     TYPE string,
         i_zbcgui        TYPE string,
         i_zprctr3       TYPE string,
         i_zcngui        TYPE string,
         i_zdichvu       TYPE string,
         i_servgroup     TYPE string,
         i_zdoanhthu     TYPE string,
         i_zstvat        TYPE string,
         i_zamount       TYPE string,
         i_zstcod        TYPE string,
         i_zstckhoan     TYPE string,
         i_sgtxt         TYPE string,
         i_zgjahr        TYPE string,
         i_rhcur         TYPE string,
         i_zfbdt         TYPE string,
         f_date          TYPE string,
         f_time          TYPE string.
TYPES: END OF gty_data.
DATA: gt_data        TYPE TABLE OF gty_data,
      gt_data_full   TYPE TABLE OF gty_data,
      gt_log         TYPE TABLE OF zev_trans_l,
      gt_mapping_pc  TYPE TABLE OF zmap_ev_bc,
      gt_mapping_srv TYPE TABLE OF zmap_ev_dichvu,
      gt_t012k       TYPE TABLE OF t012k,
*      gt_mapping_cn  TYPE TABLE OF zev_cn,
      gt_loc_evtp    TYPE TABLE OF ztb_loc_evtp,
      gr_type_pgr    TYPE fccx_t_range_row.
DATA: gw_folder     TYPE eps2filnam,
      gw_folder_arc TYPE epsf-epsdirnam,
      gw_folder_log TYPE epsf-epsdirnam,
      gw_email      TYPE adr6-smtp_addr.

DATA :go_cont_0301         TYPE REF TO cl_gui_custom_container,
      go_cont_0302         TYPE REF TO cl_gui_custom_container,
      go_container_header1 TYPE REF TO cl_gui_container,
      go_container_header2 TYPE REF TO cl_gui_container.
*      go_doc_header1       TYPE REF TO cl_dd_document,
*      go_doc_header2       TYPE REF TO cl_dd_document.

TYPES:BEGIN OF gty_trans_h.
TYPES :flag TYPE c1.
    INCLUDE TYPE zev_trans_h.
TYPES :cell_color  TYPE lvc_t_scol.
*TYPES :cell_style  TYPE lvc_t_styl.
TYPES END OF gty_trans_h.

TYPES:BEGIN OF gty_trans_i.
TYPES :flag TYPE c1.
    INCLUDE TYPE zev_trans_i.
TYPES :cell_color  TYPE lvc_t_scol.
*TYPES :cell_style  TYPE lvc_t_styl.
TYPES END OF gty_trans_i.

FIELD-SYMBOLS:<fs_trans_tab2_g> TYPE STANDARD TABLE,
              <fs_evtp_tab1_g>  TYPE STANDARD TABLE.
DATA :go_alv_grid TYPE REF TO cl_gui_alv_grid,
      gw_type     TYPE string.
*      lo_alv2     TYPE REF TO cl_gui_alv_grid.
