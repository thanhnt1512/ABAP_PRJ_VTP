*&---------------------------------------------------------------------*
*& Include          ZIN_REPORT_KKCP_TOP
*&---------------------------------------------------------------------*
REPORT zpg_report_kkcp.
TABLES :sscrfields,bkpf.
DATA :string(30) TYPE c.
DATA :gt_tb_sh TYPE TABLE of ZTB_API_IO_LOG.
*DATA :p_key   TYPE zdd_tdname,
DATA :p_key TYPE zdd_tdname,
      rb_down TYPE xfeld,
      rb_view TYPE xfeld,
      rb_dtl  TYPE xfeld.
*PARAMETERS :p_key TYPE zdd_tdname LOWER CASE NO-DISPLAY MATCHCODE OBJECT ZSH_NAME_STXH.
DATA : go_cont_i    TYPE REF TO cl_gui_custom_container,
       go_cont_o    TYPE REF TO cl_gui_custom_container,
       go_cont_tree TYPE REF TO cl_gui_custom_container,
       go_cont_alv TYPE REF TO cl_gui_custom_container.
DATA go_tree_in       TYPE REF TO cl_gui_alv_tree.

TYPES :
  BEGIN OF ty_bkc2,
    name       TYPE string,
    bkc1_count TYPE int8,
    amount     TYPE int8,
  END OF ty_bkc2,

  BEGIN OF ty_bkc1,
    name       TYPE string,
    bkc2_name  TYPE string,
    hdon_count TYPE int8,
    amount     TYPE int8,
  END OF ty_bkc1,

  BEGIN OF ty_hdon,
    name      TYPE string,
    bkc1_name TYPE string,
    mau_hd    TYPE string,
    kh_hd     TYPE string,
    so_hd     TYPE string,
    amount    TYPE int8,
  END OF ty_hdon.

TYPES :tt_bkc2 TYPE SORTED TABLE OF ty_bkc2 WITH UNIQUE KEY name,
       tt_bkc1 TYPE SORTED TABLE OF ty_bkc1 WITH UNIQUE KEY name bkc2_name,
       tt_hdon TYPE SORTED TABLE OF ty_hdon WITH UNIQUE KEY name bkc1_name,

       BEGIN OF MESH ty_mesh,
         bkc2 TYPE tt_bkc2 ASSOCIATION _bkc1 TO bkc1 ON  bkc2_name = name,
         bkc1 TYPE tt_bkc1 ASSOCIATION _hdon TO hdon ON  bkc1_name = name,
         hdon TYPE tt_hdon,
       END OF MESH ty_mesh.

DATA gs_mesh TYPE ty_mesh.
TYPES :BEGIN OF ty_tree_data,
*         name   TYPE string,
         count  TYPE int8,
         amount TYPE dmbtr,
         mau_hd TYPE string,
         kh_hd  TYPE string,
         so_hd  TYPE string,
       END OF ty_tree_data,

       BEGIN OF ty_data_alv,
         bkc3   TYPE string,
         bkc2   TYPE string,
         bkc1   TYPE string,
         doc    TYPE string,
         year TYPE gjahr,
         bukrs TYPE bukrs,
         count  TYPE int8,
         amount TYPE dmbtr,
         user   TYPE usnam,
         ctgs   TYPE string,
         cell_color TYPE lvc_t_scol,
       END OF ty_data_alv.

TYPES :
  BEGIN OF ty_bkc3,
    bkc3   TYPE string,
    count  TYPE int8,
    amount TYPE dmbtr,
  END OF ty_bkc3,

  BEGIN OF ty_bkc2_1,
    bkc2   TYPE string,
    bkc3   TYPE string,
    count  TYPE int8,
    amount TYPE dmbtr,
  END OF ty_bkc2_1,

  BEGIN OF ty_bkc1_1,
    bkc1   TYPE string,
    bkc2   TYPE string,
    count  TYPE int8,
    amount TYPE dmbtr,
  END OF ty_bkc1_1,

  BEGIN OF ty_hdon_1,
    belnr  TYPE belnr_d,
    bkc1   TYPE string,
    amount TYPE dmbtr,
    ctgs   TYPE string,
  END OF ty_hdon_1.

TYPES :tt_bkc3   TYPE SORTED TABLE OF ty_bkc3  WITH UNIQUE KEY bkc3,
       tt_bkc2_1 TYPE SORTED TABLE OF ty_bkc2_1 WITH UNIQUE KEY bkc2 bkc3,
       tt_bkc1_1 TYPE SORTED TABLE OF ty_bkc1_1 WITH UNIQUE KEY bkc1 bkc2,
       tt_hdon_1 TYPE STANDARD TABLE OF ty_hdon_1  WITH KEY belnr bkc1, "belnr,

       BEGIN OF MESH ty_mesh1,
         bkc3 TYPE tt_bkc3 ASSOCIATION _bkc2 TO bkc2 ON bkc3 = bkc3,
         bkc2 TYPE tt_bkc2_1 ASSOCIATION _bkc1 TO bkc1 ON bkc2 = bkc2,
         bkc1 TYPE  tt_bkc1_1 ASSOCIATION _hdon TO hdon ON bkc1 = bkc1,
         hdon TYPE tt_hdon_1,
       END OF MESH ty_mesh1.
DATA gs_mesh_alv TYPE ty_mesh1.
DATA : gt_tree_data TYPE TABLE OF ty_tree_data,
       gt_alv_data  TYPE TABLE OF  ty_data_alv.
DATA gt_fc_tree         TYPE lvc_t_fcat.
SELECTION-SCREEN FUNCTION KEY 1.
*SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW TITLE TEXT-002.
*PARAMETERS :p_key TYPE char70.
*SELECTION-SCREEN END OF SCREEN 1001.
SELECT-OPTIONS :s_year FOR bkpf-bukrs NO INTERVALS.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS :s_bkc3 FOR string NO INTERVALS.
SELECT-OPTIONS :s_bkc2 FOR string NO INTERVALS.
SELECT-OPTIONS :s_bkc1 FOR string NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.
