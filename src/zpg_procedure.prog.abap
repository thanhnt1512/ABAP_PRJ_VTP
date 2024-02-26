*&---------------------------------------------------------------------*
*& Report zpg_procedure
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_procedure.
TABLES :bseg.
SELECT-OPTIONS : s_belnr FOR bseg-belnr.
DATA(lv_where) = cl_shdb_seltab=>combine_seltabs(
  it_named_seltabs = VALUE #( ( name = 'BELNR' dref = REF #( s_belnr[] ) ) )
).
TYPES :BEGIN OF ty_bseg,
         belnr TYPE bseg-belnr,
         docln TYPE bseg-docln,
         gjahr TYPE bseg-gjahr,
         bukrs TYPE bseg-bukrs,
       END OF ty_bseg,
       tt_bseg TYPE TABLE OF ty_bseg.
data : lt_bseg TYPE tt_bseg.
new zcl_amdp_demo( )->amdp_get_bseg( EXPORTING iv_where = lv_where  importing et_result = lt_bseg ).
cl_demo_output=>display_data( lt_bseg ).
