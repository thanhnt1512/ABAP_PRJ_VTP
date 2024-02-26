CLASS zcl_amdp_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES :if_amdp_marker_hdb.
    TYPES :BEGIN OF ty_bseg,
             belnr TYPE bseg-belnr,
             docln TYPE bseg-docln,
             gjahr TYPE bseg-gjahr,
             bukrs TYPE bseg-bukrs,
           END OF ty_bseg,
           tt_bseg TYPE TABLE OF ty_bseg.

    METHODS : amdp_get_bseg
      IMPORTING
        VALUE(iv_where)  TYPE string
      EXPORTING
        VALUE(et_result) TYPE tt_bseg.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_amdp_demo IMPLEMENTATION.
  METHOD amdp_get_bseg BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING bseg.
    itab = select belnr,docln,gjahr,bukrs from bseg;
    et_result = APPLY_FILTER( :itab,:iv_where);
  ENDMETHOD.
ENDCLASS.
