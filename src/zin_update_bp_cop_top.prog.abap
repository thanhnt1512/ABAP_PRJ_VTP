*&---------------------------------------------------------------------*
*& Include          ZIN_UPDATE_BP_TOP
*&---------------------------------------------------------------------*
REPORT zpg_update_bp.
TABLES :sscrfields.
TYPES: BEGIN OF ty_gen,
         bp_cus1(10)   TYPE c,
         bp_cus(20)    TYPE c, "
         bp_group(10)  TYPE c, "
         bp_type(5)    TYPE c,
         bp_title(20)  TYPE c,
         name1(80)     TYPE c, "
         name2         TYPE string,
*         name2(40)     TYPE c,
         name3(40)     TYPE c,
         name4(40)     TYPE c,
         search_t(40)  TYPE c,
         street_h      TYPE string, "
*         street_h(60)  TYPE c, "
         street4(40)   TYPE c,
         street5(40)   TYPE c,
         district(60)  TYPE c,
         post_code(10) TYPE c,
         city(50)      TYPE c, "*
         country(20)   TYPE c, "
         language(10)  TYPE c,
         tell(20)      TYPE c, "
         mobi(20)      TYPE c,
         fax(20)       TYPE c,
         email(40)     TYPE c,
         id_type(40)   TYPE c,
         id_number(40) TYPE c,
         taxtype(4)	   TYPE c,
         taxnumxl(60)  TYPE c,

       END OF ty_gen.

TYPES: BEGIN OF ty_ven,
         bp_cus(20)        TYPE c,
         v_company(20)     TYPE c,
         v_acc(50)         TYPE c,
         v_head(50)        TYPE c,
         v_pterm(50)       TYPE c,
         v_zpterm(50)      TYPE c,
         v_pmethod(50)     TYPE c,
         v_hb(50)          TYPE c,
         v_pterm_block(50) TYPE c,
         reprf(2)          TYPE c,
         v_clear(5)        TYPE c,
         v_alter(50)       TYPE c,
       END OF ty_ven.

TYPES: BEGIN OF ty_cus,
         bp_cus(20)        TYPE c,
         c_company(20)     TYPE c,
         c_acc(50)         TYPE c,
         c_head(50)        TYPE c,
         c_pterm(50)       TYPE c,
         c_zpterm(50)      TYPE c,
         c_pmethod(50)     TYPE c,
         c_hb(50)          TYPE c,
         c_pterm_block(50) TYPE c,
         c_clear(5)        TYPE c,
         c_alter(50)       TYPE c,

       END OF ty_cus.

TYPES :BEGIN OF ty_mess_log,
         status     TYPE char4,
         line       TYPE int4,
         id         TYPE symsgid,
         number     TYPE symsgno,
         message    TYPE string,
         bp_created TYPE bu_partner,
         bp         TYPE bu_partner,
*         message_v1 TYPE symsgv,
*         message_v2 TYPE symsgv,
*         message_v3 TYPE symsgv,
*         message_v4 TYPE symsgv,
       END OF ty_mess_log.
DATA :gt_gen TYPE TABLE OF ty_gen,
      gt_ven TYPE TABLE OF ty_ven,
      gt_cus TYPE TABLE OF ty_cus.

DATA :gt_mess_log TYPE TABLE OF ty_mess_log,
      gt_fcat_log TYPE slis_t_fieldcat_alv.

DATA: BEGIN OF gt_cfg_bugrp OCCURS 1,
        bu_group  TYPE bu_group,
        externind TYPE nrind,
      END   OF gt_cfg_bugrp.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_file TYPE rfbifile MODIF ID ob,
            p_gen  RADIOBUTTON GROUP gr1 USER-COMMAND usr1  DEFAULT 'X',
            p_cus  RADIOBUTTON GROUP gr1,
            p_ven  RADIOBUTTON GROUP gr1.
*            p_add_c AS CHECKBOX MODIF ID cp.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY 1.
