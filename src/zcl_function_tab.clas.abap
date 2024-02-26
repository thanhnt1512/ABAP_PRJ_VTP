CLASS zcl_function_tab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS: get_bseg FOR TABLE FUNCTION ztf_get_bseg.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_function_tab IMPLEMENTATION.
  method get_bseg by DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING bseg bkpf.
    bseg_tab = APPLY_FILTER ( bseg , :sel_belnr );
    return  select bs.mandt,
                      bs.belnr,
                      bs.bukrs,
                      bs.gjahr,
                      bs.buzei,
                      bs.hkont,
                      bk.budat
                FROM :bseg_tab bs inner join bkpf bk on bs.bukrs = bk.bukrs and bs.belnr =  bk.belnr and bs.gjahr = bk.gjahr;

  endmethod.
ENDCLASS.
