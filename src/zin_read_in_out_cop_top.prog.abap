*&---------------------------------------------------------------------*
*& Include          ZIN_READ_IN_OUT_TOP
*&---------------------------------------------------------------------*
REPORT zpg_read_in_out.
DATA :type_50 TYPE char50.
PARAMETERS :p_object TYPE char10 OBLIGATORY.

DATA : go_cont_i TYPE REF TO cl_gui_custom_container,
       go_cont_o TYPE REF TO cl_gui_custom_container.
SELECT-OPTIONS :  s_guid FOR type_50,
                  s_date FOR sy-datum,
                  s_time FOR sy-uzeit.

*DATA :BEGIN OF value_sel OCCURS 0,
*        guid      TYPE ztb_api_io_log-guid,
*        object    TYPE ztb_api_io_log-object,
*        cr_date   TYPE dats,
*        cr_time   TYPE tims,
*        user_name TYPE usnam,
*      END OF value_sel.

DATA :value_sel TYPE TABLE OF ztb_api_io_log.
DATA: dynpro_values TYPE STANDARD TABLE OF dynpread WITH HEADER LINE.
DATA :gt_log TYPE TABLE OF ztb_api_io_log,
      gw_guid TYPE char50.
