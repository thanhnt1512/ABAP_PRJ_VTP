*&---------------------------------------------------------------------*
*& Report zpg_get_bseg_functab
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_get_bseg_functab.
TABLES :bseg.
DATA : lt_tab  TYPE TABLE OF ztf_get_bseg,
       l_where TYPE string.
SELECT-OPTIONS :s_belnr FOR bseg-belnr.
l_where = cl_shdb_seltab=>combine_seltabs(
                                      EXPORTING it_named_seltabs =
                                            VALUE #( ( name = 'BELNR' dref = REF #( s_belnr[] ) ) )
                                            iv_client_field = 'MANDT'
                                ).
select * from ztf_get_bseg( sel_belnr = @l_where ) into table @lt_tab.
cl_demo_output=>display_data( lt_tab ).
