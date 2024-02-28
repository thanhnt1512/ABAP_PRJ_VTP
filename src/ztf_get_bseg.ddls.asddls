@EndUserText.label: 'ztf_get_bseg'
define table function ztf_get_bseg
  with parameters
    sel_belnr : abap.char(1000)
returns
{
  mandt : abap.clnt;
  belnr : belnr_d;
  bukrs : bukrs;
  gjahr : gjahr;
  buzei : buzei;
  hkont : hkont;
  budat : budat;

}
implemented by method
  zcl_function_tab=>get_bseg;
