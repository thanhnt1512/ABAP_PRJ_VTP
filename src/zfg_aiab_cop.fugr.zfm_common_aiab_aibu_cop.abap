FUNCTION ZFM_COMMON_AIAB_AIBU_COP.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(LIST_ASSET) TYPE  ZTT_ASSET OPTIONAL
*"     REFERENCE(ASS_CAT) TYPE  KONTY OPTIONAL
*"     REFERENCE(COMP) TYPE  BUKRS OPTIONAL
*"     REFERENCE(HANDLE) TYPE  STRING
*"     REFERENCE(DATA_SETTLE) TYPE  ZST_SETTLEMENT OPTIONAL
*"     REFERENCE(TEST) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(POSTAB) TYPE  ANY TABLE
*"  CHANGING
*"     REFERENCE(MESSAGE_SETTLE) TYPE  BAPIRET2_TAB OPTIONAL
*"--------------------------------------------------------------------

  CASE handle.
    WHEN 'GET_POSTAB'.
      PERFORM get_postab USING list_asset comp CHANGING postab.
    WHEN 'SETTLEMENT'.
      PERFORM settlement USING data_settle test CHANGING message_settle.
    WHEN 'SET_RULE'.
      PERFORM set_rule USING list_asset comp ass_cat.
  ENDCASE.




ENDFUNCTION.
