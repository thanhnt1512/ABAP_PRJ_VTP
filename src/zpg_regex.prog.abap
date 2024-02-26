*&---------------------------------------------------------------------*
*& Report ZPG_TEST_REGEX
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_regex.
DATA: regex   TYPE REF TO cl_abap_regex,       " Regex Object
      matcher TYPE REF TO cl_abap_matcher.     " Matcher Object
CREATE OBJECT regex
  EXPORTING
    pattern     = '^[^#]*#[^#]*$'
    ignore_case = abap_true.

matcher = regex->create_matcher( text = '#123' ).
IF matcher IS BOUND.
  IF matcher->match( ) = 'X'.
    WRITE 'ok'.
    FREE matcher.
  ENDIF.
ENDIF.
