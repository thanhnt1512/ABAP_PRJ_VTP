*&---------------------------------------------------------------------*
*& Report ZPG_READ_IN_OUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
INCLUDE ZIN_READ_IN_OUT_COP_TOP.
*INCLUDE zin_read_in_out_top.
INCLUDE ZIN_READ_IN_OUT_COP_F01.
*INCLUDE zin_read_in_out_f01.
INCLUDE ZIN_READ_IN_OUT_COP_O01.
*INCLUDE zin_read_in_out_o01.
INCLUDE ZIN_READ_IN_OUT_COP_I01.
*INCLUDE zin_read_in_out_i01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_guid-low.
  DATA :s_date_tmp   LIKE TABLE OF s_date WITH HEADER LINE,
        s_time_tmp   LIKE TABLE OF s_time WITH HEADER LINE,
        lw_time      TYPE sy-uzeit,
        p_object_tmp LIKE p_object.

  REFRESH :value_sel,dynpro_values,s_date_tmp,s_time_tmp.

  dynpro_values-fieldname = 'P_OBJECT'.
  APPEND dynpro_values.
  dynpro_values-fieldname = 'S_DATE-LOW'.
  APPEND dynpro_values.
  dynpro_values-fieldname = 'S_TIME-LOW'.
  APPEND dynpro_values.
  dynpro_values-fieldname = 'S_DATE-HIGH'.
  APPEND dynpro_values.
  dynpro_values-fieldname = 'S_TIME-HIGH'.
  APPEND dynpro_values.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
      translate_to_upper   = 'X'
    TABLES
      dynpfields           = dynpro_values
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  p_object_tmp = dynpro_values[ fieldname = 'P_OBJECT']-fieldvalue.
  s_date_tmp-sign = 'I'.
  s_date_tmp-low = |{ dynpro_values[ fieldname = 'S_DATE-LOW']-fieldvalue+6(4) }{ dynpro_values[ fieldname = 'S_DATE-LOW']-fieldvalue+3(2) }{ dynpro_values[ fieldname = 'S_DATE-LOW']-fieldvalue+0(2) }|.
  s_date_tmp-high = |{ dynpro_values[ fieldname = 'S_DATE-HIGH']-fieldvalue+6(4) }{ dynpro_values[ fieldname = 'S_DATE-HIGH']-fieldvalue+3(2) }{ dynpro_values[ fieldname = 'S_DATE-HIGH']-fieldvalue+0(2) }|.
  s_time_tmp-sign = 'I'.
  s_time_tmp-low = |{ dynpro_values[ fieldname = 'S_TIME-LOW']-fieldvalue+0(2) }{ dynpro_values[ fieldname = 'S_TIME-LOW']-fieldvalue+3(2) }{ dynpro_values[ fieldname = 'S_TIME-LOW']-fieldvalue+6(2) }|.
  s_time_tmp-high = |{ dynpro_values[ fieldname = 'S_TIME-HIGH']-fieldvalue+0(2) }{ dynpro_values[ fieldname = 'S_TIME-HIGH']-fieldvalue+3(2) }{ dynpro_values[ fieldname = 'S_TIME-HIGH']-fieldvalue+6(2) }|.
  s_date_tmp-option = COND #( WHEN s_date_tmp-high <> '00000000' THEN 'BT' ELSE 'EQ' ).
  s_time_tmp-option = COND #( WHEN s_time_tmp-high <> '000000' THEN 'BT' ELSE 'EQ' ).
  IF NOT s_time_tmp-low = '000000' AND s_time_tmp-high = '000000'.
    APPEND s_time_tmp.
  ENDIF.
  IF NOT s_date_tmp-low = '00000000' AND s_date_tmp-high = '00000000'.
    APPEND s_date_tmp.
  ENDIF.
  SELECT * FROM ztb_api_io_log INTO TABLE @value_sel WHERE cr_date IN @s_date_tmp AND cr_time IN @s_time_tmp AND object = @p_object_tmp.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'GUID'
      dynprofield = 's_guid-low'
      dynpprog    = sy-cprog
      dynpnr      = sy-dynnr
      value_org   = 'S'
    TABLES
      value_tab   = value_sel.
  IF sy-subrc    = 0.

  ENDIF.


START-OF-SELECTION.
  PERFORM main.
