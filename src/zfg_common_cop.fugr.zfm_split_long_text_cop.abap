FUNCTION ZFM_SPLIT_LONG_TEXT_COP.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_STRING) TYPE  STRING
*"     VALUE(IV_LENGHT) TYPE  INT4
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  LXE_TLINE
*"--------------------------------------------------------------------
  DATA :lt_tab TYPE TABLE OF tline.
  DATA :lw_str_cop TYPE string,
        lw_times   TYPE int4.
  lw_str_cop = iv_string.
  IF strlen( lw_str_cop ) <= iv_lenght.
    result = VALUE #( ( tdformat = '*' tdline = lw_str_cop ) ).
  ELSE.
    lw_times = strlen( lw_str_cop ) DIV iv_lenght.
    DO lw_times TIMES.
      result = VALUE #( BASE result ( tdformat = '*' tdline = substring( val = lw_str_cop off = 0 len = iv_lenght ) ) ).
      SHIFT lw_str_cop LEFT BY iv_lenght PLACES.
    ENDDO.

    IF strlen( lw_str_cop ) > 0.
      result = VALUE #( BASE result ( tdformat = '*' tdline =  lw_str_cop ) ).
    ENDIF.
  ENDIF.

ENDFUNCTION.
