class ZCL_SI_BKCPTX_N_COP_IN definition
  public
  create public .

public section.

  interfaces ZII_SI_BKCPTX_N_IN .

  data PHAN_BO type ZDT_BKCPTX_N_SENDER_PHAN_B_TAB .
  data HDON_I type ZDT_BKCPTX_N_SENDER_ITEM_TAB .
  data GT_OPEN_POSTING type FAC_OPP_T_T001B .

  methods POSTING_WITHOUT_ALLOCATE
    importing
      !POSTING_DATE type BUDAT
      !COST_CENTER type KOSTL
      !PROFIT_CENTER type PRCTR
      !BK_ID type STRING
      !BK_C3 type STRING
      !USER_ID type STRING
      !MA_NV_HD type STRING
      !TK_CONG_NO type STRING
      !TAX_AMOUNT type STRING
    exporting
      !LT_ACCOUNTGL type BAPIACGL09_TAB
      !LT_ACCOUNTPAYABLE type BAPIACAP09_TAB
      !LT_ACCOUNTTAX type BAPIACTX09_TAB
      !LT_CURRENCYAMOUNT type BAPIACCR09_TAB
      !LT_EXT type BAPIACEXTC_TAB
      !LT_EXT2 type BAPIPAREX_TAB .
  methods POSTING_WITH_ALLOCATE
    importing
      !TAX_KONTS type SAKNR optional
      !POSTING_DATE type BUDAT
      !COST_CENTER type KOSTL
      !PROFIT_CENTER type PRCTR
      !BK_ID type STRING
      !BK_C3 type STRING
      !USER_ID type STRING
      !MA_NV_HD type STRING
      !ORDERID type RVARI_VAL_255 optional
      !TAX_CODE type MWSKZ optional
      !TK_CONG_NO type STRING
      !TAX_AMOUNT type STRING
      !PHU_PHI type STRING optional
    exporting
      !LT_ACCOUNTGL type BAPIACGL09_TAB
      !LT_ACCOUNTPAYABLE type BAPIACAP09_TAB
      !LT_ACCOUNTTAX type BAPIACTX09_TAB
      !LT_CURRENCYAMOUNT type BAPIACCR09_TAB
      !LT_EXT type BAPIACEXTC_TAB
      !LT_EXT2 type BAPIPAREX_TAB .
  methods CHECK_OPEN_POSTING_DATE
    importing
      !BK_DATE type STRING
    returning
      value(VALID) type ABAP_BOOL .
  methods CREATE_CTGS
    importing
      value(BUKRS) type CHAR4
      value(GJAHR) type CHAR4
      value(TEN_ND) type CHAR20
      value(BKC3) type STRING
      value(LIST_BELNR) type ZTT_BELNR_BKC2 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SI_BKCPTX_N_COP_IN IMPLEMENTATION.


  METHOD CHECK_OPEN_POSTING_DATE.
    valid = abap_true.
    LOOP AT gt_open_posting INTO DATA(ls_open_posting).
      IF |{ ls_open_posting-frye1 }{ ls_open_posting-frpe1+1(2) }| > bk_date OR |{ ls_open_posting-toye1 }{ ls_open_posting-tope1+1(2) }| < bk_date.
        valid = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD CREATE_CTGS.
    DATA: lv_number TYPE numc10.
    DATA: lv_mact TYPE zdt_mact.
    DATA: lt_ctgs_i    TYPE TABLE OF ztb_ctgs_i,
          ls_ctgs_h    TYPE ztb_ctgs_h,
          ls_ctgs_i    TYPE ztb_ctgs_i,
          ls_ctgs_kkcp TYPE ztb_ctgs_kkcp.
    SELECT SINGLE * FROM ztb_ctgs_kkcp INTO @DATA(ls_data) WHERE bk_c3 = @bkc3.
    IF sy-subrc = 0.
      lv_mact = ls_data-ctgs.
    ELSE.
      SELECT SINGLE * FROM nriv INTO @DATA(ls_nriv) WHERE object = 'ZCTGS1'
                                               AND subobject = @bukrs
                                               AND nrrangenr = '01'
                                               AND toyear =  @gjahr.
      IF sy-subrc = 0.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = ls_nriv-nrrangenr
            object                  = ls_nriv-object
*           QUANTITY                = '1'
            subobject               = ls_nriv-subobject
            toyear                  = ls_nriv-toyear
*           IGNORE_BUFFER           = ' '
          IMPORTING
            number                  = lv_number
*           QUANTITY                =
*           RETURNCODE              =
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        IF sy-subrc NE 0.
* Implement suitable error handling here
          EXIT.
        ENDIF.
      ENDIF.
      CONCATENATE 'CTGS' lv_number INTO lv_mact.
    ENDIF.


    ls_ctgs_h-mact = lv_mact.
    ls_ctgs_h-bukrs = bukrs.
    ls_ctgs_h-gjahr = gjahr.
*    ls_ctgs_h-segment = p_segm.
    ls_ctgs_h-usnam = ten_nd.
    ls_ctgs_h-crdate = sy-datum.

    ls_ctgs_kkcp-ctgs = ls_ctgs_h-mact.
    ls_ctgs_kkcp-bk_c3 = bkc3.
    ls_ctgs_kkcp-crdate = ls_ctgs_h-crdate.
    ls_ctgs_kkcp-crtime = sy-uzeit.
    ls_ctgs_kkcp-usnam = ls_ctgs_h-usnam.

*    ls_ctgs_kkcp-bk_c3 =
    LOOP AT list_belnr INTO DATA(ls_belnr).

      ls_ctgs_i-mact = lv_mact.
      ls_ctgs_i-belnr = ls_belnr-belnr.
      ls_ctgs_i-bukrs = bukrs. "add by HUNGVT - add companycode
      ls_ctgs_i-gjahr = gjahr. "add by HUNGVT - add fiscal year
      APPEND ls_ctgs_i TO lt_ctgs_i.


      UPDATE bkpf SET ccnum = lv_mact
      WHERE belnr = ls_belnr-belnr
        AND bukrs = bukrs
        AND gjahr = gjahr.
      COMMIT WORK AND WAIT.

      ls_ctgs_kkcp-bk_c2 = ls_belnr-bkc2.
      INSERT ztb_ctgs_kkcp FROM ls_ctgs_kkcp.
      COMMIT WORK AND WAIT.

      CLEAR :ls_ctgs_i,ls_ctgs_kkcp-bk_c2.
    ENDLOOP.

    "add by HUNGVT
    IF lt_ctgs_i IS NOT INITIAL.
      SELECT bukrs, belnr, gjahr, budat FROM bkpf INTO TABLE @DATA(lt_bkpf1) FOR ALL ENTRIES IN @lt_ctgs_i WHERE belnr = @lt_ctgs_i-belnr
                                                                        AND bukrs = @lt_ctgs_i-bukrs
                                                                        AND gjahr = @lt_ctgs_i-gjahr.
      IF sy-subrc = 0.
        SORT lt_bkpf1 BY budat DESCENDING.
        READ TABLE lt_bkpf1 INTO DATA(ls_bkpf1) INDEX 1.
        CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
          EXPORTING
            iv_date           = ls_bkpf1-budat
          IMPORTING
            ev_month_end_date = ls_bkpf1-budat.
        ls_ctgs_h-budat = ls_bkpf1-budat.
      ENDIF.
    ENDIF.
    "end by HUNGVT
    INSERT ztb_ctgs_h FROM ls_ctgs_h.
    INSERT ztb_ctgs_i FROM TABLE lt_ctgs_i.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD POSTING_WITHOUT_ALLOCATE.
    DATA :ls_accountgl      TYPE bapiacgl09,
          ls_accountpayable TYPE bapiacap09,
          ls_accounttax     TYPE bapiactx09,
          ls_ext            TYPE bapiacextc,
          ls_ext2           TYPE bapiparex,
          ls_amount         TYPE bapiaccr09.

    DATA :lw_lines        TYPE int4,
          lw_awt          TYPE tfm_amountfinanced,
          lw_am_i         TYPE tfm_amountfinanced,
          lw_descr        TYPE string,
          lw_descr_627    TYPE string VALUE 'TTCP:',
          lw_tax_amount_i TYPE string,
          lw_assgmt       TYPE string,
          lw_xref1_hd     TYPE string.
    DATA : lw_tax_code_hd_i  TYPE  mwskz,
           ls_fi_acc_doc_ext TYPE zst_fi_acc_doc_ext.

*    lw_assgmt = substring( val = bk_id off = 5 len = strlen( bk_id ) - 5 ).
    lw_assgmt = substring_after( val = bk_id sub = bk_id+0(5) ).
    lw_xref1_hd  = substring_after( val = bk_c3 sub = bk_c3+0(5) ).
    ls_ext2-structure = 'XREF1_HD'.
    ls_ext2-valuepart1 = lw_xref1_hd.
    APPEND ls_ext2 TO lt_ext2.

    LOOP AT hdon_i INTO DATA(ls_hoa_don_i) GROUP BY ( tkht = ls_hoa_don_i-tk_hach_toan tax = ls_hoa_don_i-tax_item ).
      lw_lines =  lw_lines + 1.
      LOOP AT GROUP ls_hoa_don_i ASSIGNING FIELD-SYMBOL(<fs_hd_i>).
        lw_am_i = lw_am_i + <fs_hd_i>-phu_phi + <fs_hd_i>-amount.
        lw_descr = | { lw_descr } { <fs_hd_i>-descr } |.
        lw_tax_amount_i = lw_tax_amount_i + <fs_hd_i>-tax_amount.
      ENDLOOP.
*      if lw_descr_627 is INITIAL .
*        lw_descr_627 = |{ lw_descr }|.
*      else.
      lw_descr_627 = |{ lw_descr_627 } { lw_descr }|.
*      ENDIF.

      CONDENSE lw_descr_627.
      ls_accountgl-itemno_acc = lw_lines.
      ls_accountgl-gl_account = ls_hoa_don_i-tk_hach_toan.
      ls_accountgl-pstng_date = posting_date.
      ls_accountgl-item_text = lw_descr.
      ls_accountgl-costcenter = cost_center.
      ls_accountgl-profit_ctr = profit_center.
      ls_accountgl-alloc_nmbr = lw_assgmt.
      ls_accountgl-ref_key_1 = user_id.

      CASE ls_hoa_don_i-tax_item.
        WHEN '10' OR '8'.
          lw_tax_code_hd_i = 'I1'.
        WHEN '5'.
          lw_tax_code_hd_i = 'I5'.
        WHEN '0'.
          lw_tax_code_hd_i = 'I0'.
      ENDCASE.

      "19.12.2022
      IF ls_hoa_don_i-tax_item NE -1.
        ls_accountgl-tax_code = lw_tax_code_hd_i.
      ENDIF.
      "


      APPEND ls_accountgl TO lt_accountgl.
      CLEAR ls_accountgl.

      "extension table using in longtext
      ls_ext-field1 = 'LONG_TEXT'.
      ls_ext-field2 = lw_descr.
      ls_ext-field3 = lw_lines.
      APPEND ls_ext TO lt_ext.
      CLEAR ls_ext.
*    CLEAR :ls_fi_acc_doc_ext,ls_ext.
*    ls_fi_acc_doc_ext-itemno_acc = lw_lines.
*    ls_fi_acc_doc_ext-xref1 = profit_center.
*    ls_ext = ls_fi_acc_doc_ext.
*    APPEND ls_ext TO lt_ext.

      ls_amount-itemno_acc = lw_lines.
      ls_amount-currency_iso = 'VND'.
      ls_amount-amt_doccur = lw_am_i.
      ls_amount-currency = 'VND'.
      ls_amount-curr_type = '00'.  "Doc currency
      APPEND ls_amount TO lt_currencyamount.
      CLEAR: ls_amount.

      " tax
      IF ls_hoa_don_i-tax_item NE -1.
        lw_lines =  lw_lines + 1.
        ls_amount-itemno_acc = lw_lines.
        ls_amount-currency_iso = 'VND'.
        ls_amount-tax_amt = lw_tax_amount_i.
        ls_amount-amt_base = lw_am_i.
        ls_amount-amt_doccur = lw_tax_amount_i.
        ls_amount-currency = 'VND'.
        ls_amount-curr_type = '00'.  "Doc currency
        APPEND ls_amount TO lt_currencyamount.
        CLEAR: ls_amount.

        ls_accounttax-itemno_acc = lw_lines.
        ls_accounttax-gl_account =  '1331100000'.
        ls_accounttax-tax_code = lw_tax_code_hd_i.
        APPEND ls_accounttax TO lt_accounttax.
        CLEAR ls_accounttax.

        ls_fi_acc_doc_ext-itemno_acc = lw_lines.
*      ls_fi_acc_doc_ext-zuonr = bk_id.
        ls_fi_acc_doc_ext-zuonr = lw_assgmt.
        ls_ext = ls_fi_acc_doc_ext.
        APPEND ls_ext TO lt_ext.
        CLEAR :ls_ext,ls_fi_acc_doc_ext.

      ENDIF.


      lw_awt = lw_awt + lw_am_i.
      CLEAR :lw_am_i,lw_descr, lw_tax_amount_i,
             ls_amount,ls_ext,ls_accountgl,lw_tax_code_hd_i.
    ENDLOOP.
    lw_lines = lw_lines + 1.
    "Vendor Item
    ls_accountpayable-itemno_acc = lw_lines.
    ls_accountpayable-vendor_no = ma_nv_hd.
    ls_accountpayable-ref_key_1 = profit_center.
*    ls_accountpayable-item_text = 'Thanh toán chi phí'.
    ls_accountpayable-item_text = lw_descr_627.
    ls_accountpayable-alloc_nmbr = lw_assgmt.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accountpayable-vendor_no
      IMPORTING
        output = ls_accountpayable-vendor_no.
    ls_accountpayable-gl_account = tk_cong_no.
    APPEND ls_accountpayable TO lt_accountpayable.
    CLEAR ls_accountpayable.


    lw_awt = lw_awt + tax_amount.
    " Currency
    ls_amount-itemno_acc = lw_lines.
    ls_amount-currency_iso = 'VND'.
    ls_amount-amt_doccur = lw_awt * -1.
    ls_amount-currency = 'VND'.
    ls_amount-curr_type = '00'.  "Doc currency
    APPEND ls_amount TO lt_currencyamount.

    "Tax Item
*      LOOP AT lt_tax_line INTO ls_tax_line.
*  CLEAR ls_accounttax.
*
*  lw_lines = lw_lines + 1.
*  ls_accounttax-itemno_acc = lw_lines.
*  ls_accounttax-gl_account =  '1331100000'.
*
*  APPEND ls_accounttax TO lt_accounttax.
*  LOOP AT lt_currencyamount ASSIGNING FIELD-SYMBOL(<fs_amount>) WHERE itemno_acc IS INITIAL.
*    lw_lines = lw_lines + 1.
*    <fs_amount>-itemno_acc = lw_lines.
*    CLEAR ls_accounttax.
*    ls_accounttax-itemno_acc = lw_lines.
*    ls_accounttax-gl_account =  '1331100000'.
*    APPEND ls_accounttax TO lt_accounttax.
*  ENDLOOP.

*    ENDLOOP.
  ENDMETHOD.


  METHOD POSTING_WITH_ALLOCATE.
    DATA :ls_accountgl      TYPE bapiacgl09,
          ls_accountpayable TYPE bapiacap09,
          ls_accounttax     TYPE bapiactx09,
          ls_ext            TYPE bapiacextc,
          ls_ext2           TYPE bapiparex,
          ls_amount         TYPE bapiaccr09.
    DATA :lw_lines        TYPE int4,
          lw_rate         TYPE rates,
          lw_awt          TYPE tfm_amountfinanced,
          lw_am_i         TYPE tfm_amountfinanced,
          lw_tax_amount_i TYPE tfm_amountfinanced,
          lw_base_amt_i   TYPE tfm_amountfinanced,
          lw_descr        TYPE string,
          lw_descr_627    TYPE string VALUE 'TTCP:',
          lw_assgmt       TYPE string,
          lw_xref1_hd     TYPE string.

    DATA : lw_tax            TYPE  mwskz,
           ls_fi_acc_doc_ext TYPE zst_fi_acc_doc_ext.

*    lw_assgmt = substring( val = bk_id off = 5 len = strlen( bk_id ) - 5 ).
    lw_assgmt = substring_after( val = bk_id sub = bk_id+0(5) ).
    lw_xref1_hd  = substring_after( val = bk_c3 sub = bk_c3+0(5) ).
    ls_ext2-structure = 'XREF1_HD'.
    ls_ext2-valuepart1 = lw_xref1_hd.
    APPEND ls_ext2 TO lt_ext2.
    LOOP AT phan_bo INTO DATA(ls_phan_bo).
      lw_lines =  lw_lines + 1.
      LOOP AT hdon_i INTO DATA(ls_hdoni) WHERE tk_hach_toan = ls_phan_bo-tk_hach_toan.
        lw_descr = | { lw_descr } { ls_hdoni-descr } |.
      ENDLOOP.
      ls_accountgl-itemno_acc = lw_lines.
      ls_accountgl-gl_account = ls_phan_bo-tk_hach_toan.
      ls_accountgl-pstng_date = posting_date.
      ls_accountgl-item_text = lw_descr.
      ls_accountgl-costcenter = CONV #( ls_phan_bo-cc_pb ).
      ls_accountgl-profit_ctr = substring( val = substring( val = ls_accountgl-costcenter off = 0 len = strlen( ls_accountgl-costcenter ) - 1 )  off = 0 len = strlen( ls_accountgl-costcenter ) - 2  ) .
      ls_accountgl-alloc_nmbr = lw_assgmt .
*      ls_accountgl-ref_key_1 = user_id.
      CASE ls_phan_bo-tax_item.
        WHEN '10' OR '8'.
          lw_tax = 'I1'.
        WHEN '5'.
          lw_tax = 'I5'.
        WHEN '0'.
          lw_tax = 'I0'.
      ENDCASE.
      IF ls_phan_bo-tax_item NE -1.
        ls_accountgl-tax_code = lw_tax.
      ENDIF.


      APPEND ls_accountgl TO lt_accountgl.
      CLEAR ls_accountgl.

      "extension table using in longtext
      ls_ext-field1 = 'LONG_TEXT'.
      ls_ext-field2 = lw_descr.
      ls_ext-field3 = lw_lines.
      APPEND ls_ext TO lt_ext.
      CLEAR ls_ext .

      ls_amount-itemno_acc = lw_lines.
      ls_amount-currency_iso = 'VND'.
      ls_amount-amt_doccur = CONV #( ls_phan_bo-amount ).
      lw_awt = lw_awt + ls_amount-amt_doccur.
      ls_amount-currency = 'VND'.
      ls_amount-curr_type = '00'.  "Doc currency
      APPEND ls_amount TO lt_currencyamount.
      CLEAR : lw_descr,ls_amount,ls_ext,ls_accountgl,lw_tax.

*      tax info
*      IF ls_phan_bo-tax_item NE -1.
*        lw_lines = lw_lines + 1.
*        ls_accounttax-itemno_acc = lw_lines.
*        ls_accounttax-gl_account =  '1331100000'.
*        CASE ls_phan_bo-tax_item.
*          WHEN '10' OR '8'.
*            ls_accounttax-tax_code = 'I1'.
*          WHEN '5'.
*            ls_accounttax-tax_code = 'I5'.
*          WHEN '0'.
*            ls_accounttax-tax_code = 'I0'.
*        ENDCASE.
*        APPEND ls_accounttax TO lt_accounttax.
*        CLEAR ls_accounttax.
*
*        ls_amount-itemno_acc = lw_lines.
*        ls_amount-currency_iso = 'VND'.
*        ls_amount-amt_doccur = CONV #( ls_phan_bo-amount ).
*        lw_rate = CONV #( ls_phan_bo-rate ).
*        ls_amount-amt_doccur = ls_amount-amt_doccur * lw_rate.
*        lw_awt = lw_awt + ls_amount-amt_doccur.
*        ls_amount-amt_base = CONV #( ls_phan_bo-amount ).
*        ls_amount-tax_amt = ls_amount-amt_doccur.
*        ls_amount-currency = 'VND'.
*        ls_amount-curr_type = '00'.  "Doc currency
*        APPEND ls_amount TO lt_currencyamount.
*        CLEAR: ls_amount.
*      ENDIF.
    ENDLOOP.

*   tax info
    LOOP AT hdon_i INTO DATA(ls_hoa_don_i) GROUP BY ( tax = ls_hoa_don_i-tax_item ).
      LOOP AT GROUP ls_hoa_don_i ASSIGNING FIELD-SYMBOL(<fs_hd_i>).
        lw_tax_amount_i = lw_tax_amount_i + <fs_hd_i>-tax_amount.
        lw_base_amt_i = lw_base_amt_i + <fs_hd_i>-amount.
        lw_descr_627 = |{ lw_descr_627 } { <fs_hd_i>-descr }|.
        CONDENSE lw_descr_627.
      ENDLOOP.

      IF ls_hoa_don_i-tax_item NE -1.
        lw_lines = lw_lines + 1.
        ls_accounttax-itemno_acc = lw_lines.
        ls_accounttax-gl_account =  '1331100000'.
        CASE ls_hoa_don_i-tax_item.
          WHEN '10' OR '8'.
            ls_accounttax-tax_code = 'I1'.
          WHEN '5'.
            ls_accounttax-tax_code = 'I5'.
          WHEN '0'.
            ls_accounttax-tax_code = 'I0'.
        ENDCASE.
        APPEND ls_accounttax TO lt_accounttax.
        CLEAR ls_accounttax.

        ls_amount-itemno_acc = lw_lines.
        ls_amount-currency_iso = 'VND'.
        ls_amount-amt_doccur = CONV #( lw_tax_amount_i ).
*        ls_amount-amt_doccur = CONV #( ls_hoa_don_i-tax_amount ).
        ls_amount-amt_base = CONV #( lw_base_amt_i ).
*        ls_amount-amt_base = CONV #( ls_hoa_don_i-amount ).
*        ls_amount-tax_amt = CONV #( ls_hoa_don_i-tax_amount ).
        ls_amount-tax_amt = CONV #( lw_tax_amount_i ).
        ls_amount-currency = 'VND'.
        ls_amount-curr_type = '00'.  "Doc currency
        APPEND ls_amount TO lt_currencyamount.

        ls_fi_acc_doc_ext-itemno_acc = lw_lines.
*        ls_fi_acc_doc_ext-zuonr = bk_id.
        ls_fi_acc_doc_ext-zuonr = lw_assgmt.
        ls_ext = ls_fi_acc_doc_ext.
        APPEND ls_ext TO lt_ext.
        CLEAR :ls_ext,ls_fi_acc_doc_ext.
      ENDIF.
      CLEAR: ls_amount,
             lw_tax_amount_i,
             lw_base_amt_i.
    ENDLOOP.

*   Vendor Item
    lw_lines = lw_lines + 1.
    ls_accountpayable-itemno_acc = lw_lines.
    ls_accountpayable-vendor_no = ma_nv_hd.
    ls_accountpayable-ref_key_1 = profit_center.
*    ls_accountpayable-item_text = 'Thanh toán chi phí'.
    ls_accountpayable-item_text = lw_descr_627.
    ls_accountpayable-alloc_nmbr = lw_assgmt.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_accountpayable-vendor_no
      IMPORTING
        output = ls_accountpayable-vendor_no.
    ls_accountpayable-gl_account = tk_cong_no.
    APPEND ls_accountpayable TO lt_accountpayable.
    CLEAR ls_accountpayable.

    lw_awt = lw_awt + tax_amount.
    " Currency
    ls_amount-itemno_acc = lw_lines.
    ls_amount-currency_iso = 'VND'.
    ls_amount-amt_doccur = lw_awt * -1.
    ls_amount-currency = 'VND'.
    ls_amount-curr_type = '00'.  "Doc currency
    APPEND ls_amount TO lt_currencyamount.
  ENDMETHOD.


  METHOD ZII_SI_BKCPTX_N_IN~SI_BKCPTX_N_IN.
*** **** INSERT IMPLEMENTATION HERE **** ***
    DATA :input_cop TYPE zmt_bkcptx_n_sender.
    TYPES: BEGIN OF lty_tax_line,
             tax_code TYPE mwskz,
             tax_am   TYPE tfm_amountfinanced,
             amt_base TYPE  bapiamtbase.
    TYPES: END OF lty_tax_line.

    TYPES: BEGIN OF lty_fi_doc,
             fi_doc     TYPE numc10,
             zurl       TYPE string_table,
             mau_hd     TYPE  text100,
             pstng_date TYPE budat,
             comp_code  TYPE bukrs,
           END OF lty_fi_doc.

*    DATA: lt_tax_line TYPE TABLE OF lty_tax_line,
*          ls_tax_line TYPE lty_tax_line.

    DATA: lt_fi_doc TYPE TABLE OF lty_fi_doc,
          ls_fi_doc TYPE lty_fi_doc,
          lt_belnr  TYPE ztt_belnr_bkc2.

    DATA :flag TYPE char1.

*  DATA: lt_tax TYPE TABLE OF t030k,
*        ls_tax TYPE t030k.

    " Get thông tin Profit center from Mã bưu cục
    DATA: lw_obj_key      TYPE bapiache09-obj_key,
          lw_buccuc(10)   TYPE c,
          lw_sdate        TYPE string,
          lw_post_date    TYPE dats,
          lw_pc           TYPE prctr,
          lw_cost_center  TYPE kostl,
          lw_comcode      TYPE bukrs,
          lw_tax_konts    TYPE saknr,
          lw_tk_hach_toan TYPE char10.

    DATA:
      lt_bkcptx_hdon   TYPE TABLE OF ztb_kcptx_hdon_n,
      lt_bkcptx_hdo_i  TYPE TABLE OF ztb_kcptx_hdi_n,
      lt_bkcptx_hdr    TYPE TABLE OF ztb_bkcptx_hdr_n,
      lt_bkcptx_pb     TYPE TABLE OF ztb_cptx_hd_pb_n,
      ls_bkcptx_hdr_c2 TYPE ztb_bkcptx_hdr_n,
      ls_bkcptx_hdr_c1 TYPE ztb_bkcptx_hdr_n,
      ls_bkcptx_hdon   TYPE ztb_kcptx_hdon_n,
      ls_bkcptx_hdo_i  TYPE ztb_kcptx_hdi_n,
      ls_bkcptx_pb     TYPE ztb_cptx_hd_pb_n,
      lw_hdon_line     TYPE int4,
      lw_hdon_i_line   TYPE int4,
      lw_hdrc2_line    TYPE int4,
      lw_hdrc1_line    TYPE int4,
      lw_pb_i_line     TYPE int4.

    "check date is valid?
    DATA: lw_ngay_hd TYPE dats,
          lw_str     TYPE string,
          lw_str_out TYPE string.
    DATA: ls_header         TYPE bapiache09,
          ls_customercpd    TYPE bapiacpa09,
          lt_accountgl      TYPE TABLE OF bapiacgl09,
          ls_accountgl      TYPE bapiacgl09,
          lt_accountpayable TYPE TABLE OF bapiacap09,
          ls_accountpayable TYPE bapiacap09,
          lt_accounttax     TYPE TABLE OF bapiactx09,
          ls_accounttax     TYPE bapiactx09,
          lt_currencyamount TYPE TABLE OF bapiaccr09,
          ls_amount         TYPE bapiaccr09,
          lt_extension1     TYPE TABLE OF bapiacextc,
          ls_extension1     TYPE bapiacextc,
          lt_ext            TYPE TABLE OF bapiacextc,
          ls_ext            TYPE bapiacextc,
          lt_exten2         TYPE TABLE OF bapiparex.

    DATA: lw_name1     TYPE name1_gp,
          lw_name2     TYPE name1_gp,
          lw_name3     TYPE name1_gp,
          lw_name4     TYPE name1_gp,
          lw_nguoi_ban TYPE text150.

    DATA: lt_pi_header  TYPE zdt_bkcptx_n_sender_header_tab,
          lt_pi_hoa_don TYPE zdt_bkcptx_n_sender_hoa_do_tab.
*        lt_pi_phanbo   TYPE zdt_bkcptx_sender_phan_bo_tab,
*          lt_item_header TYPE zdt_bkcptx_sender_item_tab1.
    DATA: lw_lines    TYPE i,
          lw_awt      TYPE tfm_amountfinanced, " total amount with tax
          lw_am       TYPE tfm_amountfinanced, " amount tong chi nhanh
          lw_am_pb    TYPE tfm_amountfinanced, " amount tong phan bo
          lw_am_i     TYPE tfm_amountfinanced,
          lw_descr    TYPE string,
          lw_tax_am   TYPE tfm_amountfinanced, " total tax amount
          lw_am_base  TYPE tfm_amountfinanced,
          lw_flag_tax TYPE c. " total amount base
    DATA: lw_tax_code_hd_i TYPE  mwskz,
          lw_tax_code_hd   TYPE  mwskz.

    DATA:
      lt_return  TYPE TABLE OF bapiret2,
      ls_return1 TYPE bapiret2,
      ls_return  TYPE bapiret2,
      lw_objkey  TYPE swo_typeid,
      lw_fi_doc  TYPE numc10.

    DATA: lt_url   TYPE TABLE OF so_text255,
          lw_url   TYPE so_obj_des,
          lt_fline TYPE TABLE OF tline.
    "add
*    TYPES :tt_accountgl      TYPE TABLE OF bapiacgl09,
*           tt_accountpayable TYPE TABLE OF bapiacap09,
*           tt_accounttax     TYPE TABLE OF bapiactx09,
*           tt_currencyamount TYPE TABLE OF bapiaccr09,
*           tt_extension1     TYPE TABLE OF bapiacextc,
*           tt_extension2     TYPE TABLE OF bapiparex.
    TYPES :BEGIN OF lty_struct_post,
             header         TYPE bapiache09,
             customercpd    TYPE bapiacpa09,
             accountgl      TYPE bapiacgl09_tab,
             accountpayable TYPE bapiacap09_tab,
             accounttax     TYPE bapiactx09_tab,
             currencyamount TYPE bapiaccr09_tab,
             extension1     TYPE bapiacextc_tab,
             extension2     TYPE bapiparex_tab,
           END OF lty_struct_post.

    TYPES :BEGIN OF lty_bkc2_hdon_post,
             bkc3        TYPE string,
             bkc2        TYPE string,
             year        TYPE char4,
             key_hdon    TYPE string,
             mau_hd      TYPE string,
             nguoi_duyet TYPE char20,
             url         TYPE string_table,
             data_post   TYPE lty_struct_post,
             error       TYPE char1,
           END OF lty_bkc2_hdon_post.

    TYPES :BEGIN OF lty_kostl,
             cost_center TYPE kostl,
           END OF lty_kostl.
    TYPES :BEGIN OF lty_prctr,
             prctr TYPE prctr,
           END OF lty_prctr.
    TYPES :BEGIN OF lty_item_hc1,
             item TYPE zdt_bkcptx_n_sender_item_tab1,
           END OF lty_item_hc1.
    TYPES :BEGIN OF lty_phan_bo,
             phan_bo TYPE zdt_bkcptx_n_sender_phan_b_tab,
           END OF  lty_phan_bo.
    TYPES :BEGIN OF lty_cc_prctr_comp,
             cc    TYPE kostl,
             prctr TYPE prctr,
             comp  TYPE bukrs,
           END OF  lty_cc_prctr_comp.
    TYPES :BEGIN OF lty_item_hd,
             item_hd TYPE zdt_bkcptx_n_sender_item_tab,
           END OF lty_item_hd.

    TYPES :BEGIN OF lty_bk_ref_value,
             line_hd_2 TYPE int4,
             line_hd_1 TYPE int4,
             value     TYPE ztb_bkcptx_hdr_n,
           END OF lty_bk_ref_value.

    DATA :lw_posting_date   TYPE dats,
          lt_cc_valid       TYPE TABLE OF lty_kostl,
          lt_pr_valid       TYPE TABLE OF lty_prctr,
          lt_item_header_c1 TYPE TABLE OF lty_item_hc1,
          lt_phan_bo        TYPE TABLE OF lty_phan_bo,
          lt_cc_prctr_comp  TYPE TABLE OF lty_cc_prctr_comp,
          lw_konto          TYPE t001b,
          lt_bk_ref_value   TYPE TABLE OF lty_bk_ref_value,
          lt_item_hd        TYPE TABLE OF lty_item_hd.

    DATA: lw_amount_hd_bkc1    TYPE string,
          lw_tt_amount_hd_bkc1 TYPE string,
          lw_tt_amount_hd_bkc2 TYPE string,
          lw_un                TYPE string.

    DATA :lt_bkc2_hdon_post TYPE TABLE OF lty_bkc2_hdon_post.
    DATA:
      lt_konto  TYPE TABLE OF t001b,
      lw_return TYPE bapiret2,
      lw_erc    TYPE sy-subrc.

*   FIELD-SYMBOLS: <fs_bkc2> TYPE zdt_bkcptx_receiver_bkc2.
    DATA :lw_stop TYPE abap_bool.

    TYPES :BEGIN OF lty_mapping_bk,
             bkc1 TYPE string,
             bkc2 TYPE string,
           END OF lty_mapping_bk.
    TYPES :BEGIN OF lty_bp,
             bp TYPE char40,
           END OF lty_bp.

    DATA :lt_mapping_bk TYPE  TABLE OF lty_mapping_bk,
          lt_bp         TYPE TABLE OF lty_bp.

    FIELD-SYMBOLS: <fs_err_input> TYPE zdt_bkcptx_n_receiver_err_input.
    FIELD-SYMBOLS: <fs_err_acct> TYPE zdt_bkcptx_n_receiver_err_acct.
    FIELD-SYMBOLS: <fs_err_hdon> TYPE zdt_bkcptx_n_receiver_hdon.
    FIELD-SYMBOLS: <fs_err_bck1> TYPE zdt_bkcptx_n_receiver_bkc11.
    "Inset data to tables from input

    LOOP AT input-mt_bkcptx_n_sender-header INTO DATA(ls_header_c2).
      lw_hdrc2_line = sy-tabix.
      MOVE-CORRESPONDING ls_header_c2 TO ls_bkcptx_hdr_c2.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = ls_bkcptx_hdr_c2-guid.
      ls_bkcptx_hdr_c2-type = '02'.


      "bkc1
      LOOP AT ls_header_c2-item INTO DATA(ls_header_c1).
        lw_hdrc1_line = sy-tabix.
        MOVE-CORRESPONDING ls_header_c1 TO ls_bkcptx_hdr_c1.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = ls_bkcptx_hdr_c1-guid.
        ls_bkcptx_hdr_c1-type = '01'.
        ls_bkcptx_hdr_c1-bk_c2 = ls_header_c2-bk_id.
        APPEND ls_bkcptx_hdr_c1 TO lt_bkcptx_hdr.

        "cal amount tct phe duyet
        LOOP AT input-mt_bkcptx_n_sender-hoa_don INTO DATA(ls_hoa_don) WHERE bk1_id = ls_header_c1-bk_id.
          lw_amount_hd_bkc1 = REDUCE #( INIT lw_sum = lw_un FOR lw_data IN ls_hoa_don-item
                                        NEXT lw_sum = lw_sum + lw_data-amount + lw_data-tax_amount + lw_data-phu_phi
                                       ).
          lw_tt_amount_hd_bkc1 = lw_tt_amount_hd_bkc1 + lw_amount_hd_bkc1.
        ENDLOOP.
        lw_tt_amount_hd_bkc2 = lw_tt_amount_hd_bkc2 + lw_tt_amount_hd_bkc1.
        CLEAR :lw_amount_hd_bkc1,lw_tt_amount_hd_bkc1.
      ENDLOOP.
      ls_bkcptx_hdr_c2-tct_pd = lw_tt_amount_hd_bkc2.
      APPEND ls_bkcptx_hdr_c2 TO lt_bkcptx_hdr.
      CLEAR lw_tt_amount_hd_bkc2.
    ENDLOOP.
    "hoa don
    LOOP AT input-mt_bkcptx_n_sender-hoa_don INTO ls_hoa_don.
      lw_hdon_line = sy-tabix.
      ls_bkcptx_hdon = CORRESPONDING #( ls_hoa_don EXCEPT url ).
      LOOP AT ls_hoa_don-url INTO DATA(ls_url_hd).
        SPLIT ls_url_hd AT '/' INTO TABLE DATA(lt_url_hd).
        READ TABLE lt_url_hd INDEX lines( lt_url_hd ) INTO DATA(lw_url_sel).
*        DATA(lw_url_sel) = VALUE #( lt_url_hd[ strlen( lt_url_hd ) ] OPTIONAL ).
        IF ls_bkcptx_hdon-url IS INITIAL.
          ls_bkcptx_hdon-url =  lw_url_sel.
        ELSE.
          ls_bkcptx_hdon-url = |{ ls_bkcptx_hdon-url };{ lw_url_sel }|.
        ENDIF.
        CLEAR lw_url_sel.
        REFRESH lt_url_hd .
      ENDLOOP.


*      MOVE-CORRESPONDING ls_hoa_don TO ls_bkcptx_hdon.
      CALL FUNCTION 'GUID_CREATE'
        IMPORTING
          ev_guid_16 = ls_bkcptx_hdon-guid_hdon.
      ls_bkcptx_hdon-guid_hdr = lt_bkcptx_hdr[ type = '01' bk_id = ls_hoa_don-bk1_id ]-guid.
      ls_bkcptx_hdon-hdon_id = lw_hdon_line.
      APPEND  ls_bkcptx_hdon TO  lt_bkcptx_hdon.
      "hoa don line
      LOOP AT ls_hoa_don-item INTO DATA(ls_hoa_don_i).
        lw_hdon_i_line = sy-tabix.
        MOVE-CORRESPONDING ls_hoa_don_i TO ls_bkcptx_hdo_i.
        ls_bkcptx_hdo_i-item_line = lw_hdon_i_line.
        ls_bkcptx_hdo_i-guid_hdon = ls_bkcptx_hdon-guid_hdon.
        ls_bkcptx_hdo_i-tax = ls_hoa_don_i-tax_item.
        APPEND ls_bkcptx_hdo_i TO lt_bkcptx_hdo_i.
      ENDLOOP.
      "phan bo
      LOOP AT ls_hoa_don-phan_bo INTO DATA(ls_phan_bo).
        MOVE-CORRESPONDING ls_phan_bo TO ls_bkcptx_pb.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = ls_bkcptx_pb-guid_pb.
        ls_bkcptx_pb-guid_hdon = ls_bkcptx_hdon-guid_hdon.
        APPEND ls_bkcptx_pb TO lt_bkcptx_pb.
        CLEAR ls_bkcptx_pb.
      ENDLOOP.

    ENDLOOP.

*    lt_item_hd = CORRESPONDING #( input-ZMT_BKCPTX_N_SENDER-hoa_don ).
*
*    delete lt_item_hd where
*    EXPORT item_hd =  lt_item_hd TO DATABASE indx(nt) CLIENT sy-mandt ID 'ITM'.
    "Add all cost center to internal table from BKC1 and BKC2
    lt_cc_valid = CORRESPONDING #( input-mt_bkcptx_n_sender-header ).
    lt_item_header_c1 = CORRESPONDING #( input-mt_bkcptx_n_sender-header ).
    lt_phan_bo = CORRESPONDING #( input-mt_bkcptx_n_sender-hoa_don ).
    lt_bp = CORRESPONDING #( input-mt_bkcptx_n_sender-header MAPPING bp = nguoi_duyet ).
    LOOP AT lt_item_header_c1 INTO DATA(ls_item_header_c1).
      LOOP AT ls_item_header_c1-item INTO DATA(ls_item_header_c1_1).
        APPEND INITIAL LINE TO lt_cc_valid ASSIGNING FIELD-SYMBOL(<fs_cc_valid>).
        <fs_cc_valid>-cost_center = ls_item_header_c1_1-cost_center.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_phan_bo INTO DATA(ls_phan_bo_tab).
      LOOP AT ls_phan_bo_tab-phan_bo INTO DATA(ls_phan_bo_line).
        APPEND INITIAL LINE TO lt_cc_valid ASSIGNING <fs_cc_valid>.
        <fs_cc_valid>-cost_center = ls_phan_bo_line-cc_pb.
      ENDLOOP.
    ENDLOOP.

*    loop at lt_bp ASSIGNING FIELD-SYMBOL(<fs_bp>)
    "Get user name bp nguoi duyet
    SELECT * FROM usr05 FOR ALL ENTRIES IN @lt_bp
    WHERE parva = @lt_bp-bp AND parid = 'BPA'
    INTO TABLE @DATA(lt_bp_nd).


    "Get profit center from cost center
    SELECT kostl,prctr FROM csks INTO TABLE @DATA(lt_prctr)
    FOR ALL ENTRIES IN @lt_cc_valid
    WHERE kostl = @lt_cc_valid-cost_center AND kokrs = '1000'.
    SORT lt_prctr BY kostl.

    "Get company code from profit center
    SELECT bukrs,prctr FROM cepc_bukrs INTO TABLE @DATA(lt_comcode)
    FOR ALL ENTRIES IN @lt_prctr
    WHERE prctr = @lt_prctr-prctr AND kokrs = '1000'.
    SORT lt_comcode BY prctr.
    "Get t030k
    SELECT * FROM t030k INTO TABLE @DATA(lt_tax)
      WHERE ktopl = '1000'
        AND ktosl = 'VST'
        AND mwskz IN ( 'I0', 'I1', 'I5' ).
*
    "Check bk_date bkc1 and return all error
*    SELECT * FROM t001b INTO TABLE gt_open_posting WHERE bukrs = '1000' AND rrcty = '0'.
*    DELETE ADJACENT DUPLICATES FROM gt_open_posting COMPARING frye1 frpe1 toye1 tope1.
*
    input_cop = input.

    "Check error
    output-mt_bkcptx_n_receive-ev_error = '1'.
    LOOP AT input-mt_bkcptx_n_sender-header INTO DATA(ls_input_header_c2).
      LOOP AT ls_input_header_c2-item INTO DATA(ls_input_header_c1).
        READ TABLE lt_prctr INTO DATA(ls_prctr1) WITH KEY kostl = ls_input_header_c1-cost_center BINARY SEARCH.
        IF sy-subrc NE 0.
          output-mt_bkcptx_n_receive-ev_error = '0'.
          IF <fs_err_input> IS NOT ASSIGNED.
            APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_input ASSIGNING <fs_err_input>.
          ENDIF.
          <fs_err_input>-bk_no2 = ls_input_header_c2-bk_id.
          APPEND INITIAL LINE TO <fs_err_input>-bkc1 ASSIGNING FIELD-SYMBOL(<fs_err_input_bkc1>).
          <fs_err_input_bkc1>-bk_no1 = ls_input_header_c1-bk_id.
          APPEND INITIAL LINE TO lt_mapping_bk ASSIGNING FIELD-SYMBOL(<fs_mapping_bk>).
          <fs_mapping_bk>-bkc2 = <fs_err_input>-bk_no2.
          <fs_mapping_bk>-bkc1 = <fs_err_input_bkc1>-bk_no1.
*            lw_str = |Costcenter { ls_input_header_c1-cost_center } không tồn tại|.
          <fs_err_input_bkc1>-error = |Costcenter { ls_input_header_c1-cost_center } không tồn tại|.
          <fs_err_input_bkc1>-err_code = '2'.
        ENDIF.
*        ENDIF.
*        CLEAR lw_str.
      ENDLOOP .
      UNASSIGN <fs_err_input>.
    ENDLOOP.


    "set error input bkc2
    LOOP AT output-mt_bkcptx_n_receive-err_input INTO DATA(ls_err_input).
      READ TABLE lt_bkcptx_hdr WITH KEY type = '02' bk_id = ls_err_input-bk_no2 ASSIGNING FIELD-SYMBOL(<fs_bkcptx_hdr>).
      IF sy-subrc = 0.
        <fs_bkcptx_hdr>-status = 0.
      ENDIF.
    ENDLOOP.

    INSERT ztb_bkcptx_hdr_n FROM TABLE lt_bkcptx_hdr.
    COMMIT WORK AND WAIT.
    INSERT ztb_kcptx_hdon_n FROM TABLE lt_bkcptx_hdon.
    COMMIT WORK AND WAIT.
    INSERT ztb_kcptx_hdi_n FROM TABLE lt_bkcptx_hdo_i.
    COMMIT WORK AND WAIT.
    INSERT ztb_cptx_hd_pb_n FROM TABLE lt_bkcptx_pb.
    COMMIT WORK AND WAIT.

    CLEAR lw_hdon_line.
    LOOP AT lt_mapping_bk INTO DATA(ls_mapping_bk).
      DELETE input_cop-mt_bkcptx_n_sender-header WHERE bk_id = ls_mapping_bk-bkc2.
      DELETE input_cop-mt_bkcptx_n_sender-hoa_don WHERE bk1_id = ls_mapping_bk-bkc1.
    ENDLOOP.


    CHECK input_cop-mt_bkcptx_n_sender-hoa_don IS NOT INITIAL.
    DATA : lw_post_bkc2     TYPE abap_bool,
           lw_post_bkc1     TYPE abap_bool,
           lw_error_hdo_str TYPE string.

    "Check hach toan
    LOOP AT input_cop-mt_bkcptx_n_sender-header INTO ls_header_c2.
      READ TABLE lt_bkcptx_hdr INTO DATA(ls_bkcptx_hdr_c2_u) WITH KEY type = '02' bk_id = ls_header_c2-bk_id.
      CHECK sy-subrc = 0.
      lw_post_bkc2 = abap_true.
      LOOP AT ls_header_c2-item INTO ls_header_c1 .
        lw_post_bkc1 = abap_true.
        LOOP AT input_cop-mt_bkcptx_n_sender-hoa_don INTO ls_hoa_don WHERE bk1_id = ls_header_c1-bk_id.
          hdon_i = ls_hoa_don-item.
          phan_bo = ls_hoa_don-phan_bo.
          CLEAR : lw_name1,lw_name2,lw_name3,lw_name4,lw_nguoi_ban.
          READ TABLE lt_bkcptx_hdr INTO DATA(ls_bkcptx_hdr) WITH KEY type = '01' bk_id = ls_hoa_don-bk1_id.
          CHECK sy-subrc = 0.
          READ TABLE lt_prctr INTO DATA(ls_prctr) WITH KEY kostl = ls_bkcptx_hdr-cost_center BINARY SEARCH.
          CHECK sy-subrc = 0.
*         lt_exten2 = VALUE #( ( structure = 'XREF1' valuepart1 = ls_prctr-prctr ) ).
          READ TABLE lt_comcode INTO DATA(ls_comcode) WITH KEY prctr = ls_prctr-prctr BINARY SEARCH.
          CHECK sy-subrc = 0.
          READ TABLE lt_bkcptx_hdon INTO DATA(ls_bkcptx_hdo) WITH KEY guid_hdr = ls_bkcptx_hdr-guid uuid_hd = ls_hoa_don-uuid_hd.
          CHECK sy-subrc = 0.
          lw_posting_date = CONV #( ls_bkcptx_hdr-bk_date ).
          lw_ngay_hd = ls_hoa_don-ngay_hd.


          ls_header = VALUE #( doc_date   = lw_ngay_hd
                              pstng_date = lw_posting_date
                              doc_type   = 'Z1'
                              doc_status = '4'
                              username   = sy-uname
*                               fis_period = lw_post_date+4(2)
                              comp_code  = ls_comcode-bukrs
                              ref_doc_no = |{ ls_hoa_don-kihieu_hd }#{ ls_hoa_don-so_hd }|
                              header_txt = ls_bkcptx_hdr-bk_id
                               ).
          lw_nguoi_ban = ls_hoa_don-ten_nguoi_ban.
*          lw_nguoi_ban = ls_hoa_don-nguoi_ban.
          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length       = 35
              text         = lw_nguoi_ban
              as_character = 'X'
            IMPORTING
              line         = lw_name1
              rest         = lw_nguoi_ban.

          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length       = 35
              text         = lw_nguoi_ban
              as_character = 'X'
            IMPORTING
              line         = lw_name2
              rest         = lw_nguoi_ban.
          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length       = 35
              text         = lw_nguoi_ban
              as_character = 'X'
            IMPORTING
              line         = lw_name3
              rest         = lw_nguoi_ban.
          CALL FUNCTION 'TEXT_SPLIT'
            EXPORTING
              length       = 35
              text         = lw_nguoi_ban
              as_character = 'X'
            IMPORTING
              line         = lw_name4
              rest         = lw_nguoi_ban.

          ls_customercpd = VALUE #( name        = lw_name1
                                    name_2      = lw_name2
                                    name_3      = lw_name3
                                    name_4      = lw_name4
                                    langu_iso   = 'VI'
                                    city        = 'Hà nội'
                                    country     = 'VN'
                                    bank_ctry   = 'VN'
                                    tax_no_1    = ls_hoa_don-mst ).
          "TH khong phan bo
          IF phan_bo[] IS INITIAL.
            CALL METHOD me->posting_without_allocate
              EXPORTING
                posting_date      = lw_posting_date
                cost_center       = CONV #( ls_bkcptx_hdr-cost_center )
                profit_center     = ls_prctr-prctr
                bk_id             = CONV #( ls_bkcptx_hdr-bk_id )
                bk_c3             = CONV #( ls_header_c2-bk_c3 )
                user_id           = CONV #( ls_bkcptx_hdr-user_id )
*               ma_nv_hd          = ls_hoa_don-ma_nv
                ma_nv_hd          = COND #( WHEN ls_hoa_don-tk_cong_no CP '141*' THEN ls_hoa_don-ma_nv
                                            WHEN ls_hoa_don-tk_cong_no CP '331*' THEN ls_hoa_don-nguoi_ban )
                tk_cong_no        = ls_hoa_don-tk_cong_no
                tax_amount        = ls_hoa_don-tax_amount
*               phu_phi           =
              IMPORTING
                lt_accountgl      = lt_accountgl
                lt_accountpayable = lt_accountpayable
                lt_accounttax     = lt_accounttax
                lt_currencyamount = lt_currencyamount
*               lt_extension1     = lt_extension1
                lt_ext2           = lt_exten2
                lt_ext            = lt_ext.
          ELSE.
            "TH có PB
            CALL METHOD posting_with_allocate
              EXPORTING
                posting_date      = lw_posting_date
                cost_center       = CONV #( ls_bkcptx_hdr-cost_center )
                profit_center     = ls_prctr-prctr
                bk_id             = CONV #( ls_bkcptx_hdr-bk_id )
                bk_c3             = CONV #( ls_header_c2-bk_c3 )
                user_id           = CONV #( ls_bkcptx_hdr-user_id )
*               ma_nv_hd          = ls_hoa_don-ma_nv
                ma_nv_hd          = COND #( WHEN ls_hoa_don-tk_cong_no CP '141*' THEN ls_hoa_don-ma_nv
                                            WHEN ls_hoa_don-tk_cong_no CP '331*' THEN ls_hoa_don-nguoi_ban )
                tk_cong_no        = ls_hoa_don-tk_cong_no
                tax_amount        = ls_hoa_don-tax_amount
*               phu_phi           =
              IMPORTING
                lt_accountgl      = lt_accountgl
                lt_accountpayable = lt_accountpayable
                lt_accounttax     = lt_accounttax
                lt_currencyamount = lt_currencyamount
                lt_ext2           = lt_exten2
                lt_ext            = lt_ext.
          ENDIF.

          CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
            EXPORTING
              documentheader = ls_header
              customercpd    = ls_customercpd
            TABLES
              accountgl      = lt_accountgl
              accountpayable = lt_accountpayable
              accounttax     = lt_accounttax
              currencyamount = lt_currencyamount
              extension1     = lt_ext
              extension2     = lt_exten2
              return         = lt_return.
          APPEND INITIAL LINE TO lt_bkc2_hdon_post ASSIGNING FIELD-SYMBOL(<fs_bkc2_hdon_post>).
          <fs_bkc2_hdon_post>-bkc2 = ls_header_c2-bk_id.
          <fs_bkc2_hdon_post>-bkc3 = ls_header_c2-bk_c3.
          READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
          IF sy-subrc = 0.
            output-mt_bkcptx_n_receive-ev_error = '0'.
            <fs_bkc2_hdon_post>-error = 'X'.
            DELETE lt_return INDEX 1.
            SORT lt_return.
            DELETE ADJACENT DUPLICATES FROM lt_return COMPARING number.
            IF <fs_err_acct> IS NOT ASSIGNED.
              APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-err_acct ASSIGNING <fs_err_acct>.
            ENDIF.
            <fs_err_acct>-bk_no2 = ls_header_c2-bk_id.
            IF <fs_err_bck1> IS NOT ASSIGNED.
              APPEND INITIAL LINE TO <fs_err_acct>-bkc1 ASSIGNING <fs_err_bck1>.
            ENDIF.
            <fs_err_bck1>-bk_no1 = ls_header_c1-bk_id.
            APPEND INITIAL LINE TO <fs_err_bck1>-hdon ASSIGNING <fs_err_hdon>.
            <fs_err_hdon>-id = ls_hoa_don-uuid_hd.

*           <fs_err_acct_bkc1>-err_code = 2.
            LOOP AT lt_return INTO ls_return WHERE type = 'E'.
              CONCATENATE <fs_err_hdon>-error ls_return-message INTO <fs_err_hdon>-error SEPARATED BY '|'.
              CONCATENATE lw_error_hdo_str <fs_err_hdon>-error  INTO lw_error_hdo_str SEPARATED BY '-'.
            ENDLOOP.
            lw_post_bkc1 = abap_false.
            lw_post_bkc2 = abap_false.
          ELSE.
            <fs_bkc2_hdon_post>-key_hdon = ls_hoa_don-uuid_hd.
            <fs_bkc2_hdon_post>-mau_hd = ls_hoa_don-mau_hd.
            <fs_bkc2_hdon_post>-url = ls_hoa_don-url.
            <fs_bkc2_hdon_post>-data_post-header = ls_header.
            <fs_bkc2_hdon_post>-data_post-customercpd = ls_customercpd.
            <fs_bkc2_hdon_post>-data_post-accountgl = lt_accountgl.
            <fs_bkc2_hdon_post>-data_post-accountpayable = lt_accountpayable.
            <fs_bkc2_hdon_post>-data_post-accounttax     = lt_accounttax.
            <fs_bkc2_hdon_post>-data_post-currencyamount = lt_currencyamount.
            <fs_bkc2_hdon_post>-data_post-extension1 = lt_ext.
            <fs_bkc2_hdon_post>-data_post-extension2 = lt_exten2.
            <fs_bkc2_hdon_post>-nguoi_duyet = VALUE #( lt_bp_nd[ parva = ls_header_c2-nguoi_duyet ]-bname OPTIONAL ).
            <fs_bkc2_hdon_post>-year = ls_header_c2-bk_date+0(4).
          ENDIF.
          CLEAR: ls_header,
                 ls_customercpd,
                 ls_bkcptx_hdo,
                 ls_hoa_don.
          REFRESH: lt_accountgl[],
                   lt_accountpayable[],
                   lt_accounttax[],
                   lt_currencyamount[],
                   lt_ext[],lt_exten2[],
                   lt_return[].
          UNASSIGN <fs_err_hdon>.
        ENDLOOP.
        IF lw_post_bkc1 = abap_false.
          ls_bkcptx_hdr-status  = '0'.
          ls_bkcptx_hdr-status_des =  lw_error_hdo_str.
          MODIFY ztb_bkcptx_hdr_n FROM ls_bkcptx_hdr.
          COMMIT WORK AND WAIT.
        ENDIF.
        UNASSIGN <fs_err_bck1>.
        CLEAR :lw_error_hdo_str,ls_bkcptx_hdr,lw_post_bkc1.
      ENDLOOP.
      IF lw_post_bkc2 = abap_false.
        ls_bkcptx_hdr_c2_u-status = '0'.
        MODIFY ztb_bkcptx_hdr_n FROM  ls_bkcptx_hdr_c2_u.
        COMMIT WORK AND WAIT.
      ENDIF.
      UNASSIGN <fs_err_acct>.
    ENDLOOP.

    "Hach toan bang ke khong loi
    DATA:lt_bkc2_no_post LIKE lt_bkc2_hdon_post.

    lt_bkc2_no_post = VALUE #( FOR data IN lt_bkc2_hdon_post WHERE ( error = 'X' ) ( bkc2 = data-bkc2 ) ).
    DELETE ADJACENT DUPLICATES FROM lt_bkc2_no_post COMPARING bkc2.
    LOOP AT lt_bkc2_no_post  INTO DATA(ls_bkc2_no_post).
      DELETE lt_bkc2_hdon_post WHERE bkc2 = ls_bkc2_no_post-bkc2.
    ENDLOOP.

    LOOP AT lt_bkc2_hdon_post INTO DATA(ls_bkc2_hdon_post)." GROUP BY ( bkc2 = ls_bkc2_hdon_post_gr-bkc2 ) ASCENDING.
*      LOOP AT GROUP ls_bkc2_hdon_post_gr INTO DATA(ls_bkc2_hdon_post).
      READ TABLE lt_bkcptx_hdon INTO ls_bkcptx_hdo WITH KEY uuid_hd = ls_bkc2_hdon_post-key_hdon.
      CHECK sy-subrc = 0.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = ls_bkc2_hdon_post-data_post-header
          customercpd    = ls_bkc2_hdon_post-data_post-customercpd
        IMPORTING
          obj_key        = lw_obj_key
        TABLES
          accountgl      = ls_bkc2_hdon_post-data_post-accountgl
          accountpayable = ls_bkc2_hdon_post-data_post-accountpayable
          accounttax     = ls_bkc2_hdon_post-data_post-accounttax
          currencyamount = ls_bkc2_hdon_post-data_post-currencyamount
          extension1     = ls_bkc2_hdon_post-data_post-extension1
          extension2     = ls_bkc2_hdon_post-data_post-extension2
          return         = lt_return.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO output-mt_bkcptx_n_receive-bk2 ASSIGNING FIELD-SYMBOL(<fs_bkc2_success>).
        <fs_bkc2_success> = ls_bkc2_hdon_post-bkc2.
        output-mt_bkcptx_n_receive-fi_doc = |{ output-mt_bkcptx_n_receive-fi_doc }/{ lw_obj_key(10) }|.
        ls_bkcptx_hdo-belnr = lw_obj_key(10).
        APPEND INITIAL LINE TO lt_belnr ASSIGNING FIELD-SYMBOL(<fs_belnr>).
        <fs_belnr>-belnr = lw_obj_key(10).
        <fs_belnr>-bkc2 = ls_bkc2_hdon_post-bkc2.
        MODIFY ztb_kcptx_hdon_n FROM ls_bkcptx_hdo.
        COMMIT WORK AND WAIT.
        " Lấy thông tin số chứng từ, gán vào line post thành công
        ls_fi_doc-fi_doc = lw_obj_key(10).
        ls_fi_doc-zurl = ls_bkc2_hdon_post-url.
        ls_fi_doc-mau_hd = ls_bkc2_hdon_post-mau_hd.
        ls_fi_doc-comp_code = ls_bkc2_hdon_post-data_post-header-comp_code.
        ls_fi_doc-pstng_date = ls_bkc2_hdon_post-data_post-header-pstng_date.
        APPEND ls_fi_doc TO lt_fi_doc.
        CLEAR :ls_fi_doc,ls_return.
      ENDIF.
      REFRESH lt_return[].
      CLEAR ls_bkcptx_hdo.
*      ENDLOOP.

    ENDLOOP.
    "tao chung tu ghi so
    IF lt_bkc2_hdon_post IS NOT INITIAL.
      me->create_ctgs( bukrs = '1000' gjahr = VALUE #( lt_bkc2_hdon_post[ 1 ]-year )
                       bkc3 = VALUE #( lt_bkc2_hdon_post[ 1 ]-bkc3 )
                       ten_nd = VALUE #( lt_bkc2_hdon_post[ 1 ]-nguoi_duyet )
                       list_belnr = lt_belnr
                       ).
      REFRESH lt_belnr[].
    ENDIF.

    LOOP AT lt_fi_doc INTO ls_fi_doc.
      CLEAR lw_obj_key.
      lw_objkey = |{ ls_fi_doc-comp_code }{ ls_fi_doc-fi_doc ALPHA = IN }{ ls_fi_doc-pstng_date(4) }|.
      "Creat text ID trường Mẫu hóa đơn
      lt_fline = VALUE #( ( tdformat = '*'
                          tdline = ls_fi_doc-mau_hd ) ).
      CALL FUNCTION 'CREATE_TEXT'
        EXPORTING
          fid       = '1040'
          flanguage = 'E'
          fname     = lw_objkey
          fobject   = 'BELEG'
        TABLES
          flines    = lt_fline
        EXCEPTIONS
          no_init   = 1
          no_save   = 2
          OTHERS    = 3.
      REFRESH lt_fline.

      "Update thông tin Url
      lt_url[] =  ls_fi_doc-zurl[].
*      SPLIT ls_fi_doc-zurl AT ';' INTO TABLE lt_url.
      SORT lt_url.
      DELETE ADJACENT DUPLICATES FROM lt_url COMPARING ALL FIELDS.
      LOOP AT lt_url INTO DATA(ls_url).
        SPLIT ls_url AT '/' INTO TABLE lt_url_hd.
        READ TABLE lt_url_hd INDEX lines( lt_url_hd ) INTO lw_url_sel.
*        lw_url_index = |Document Link { sy-tabix }|.
        lw_url = lw_url_sel.
        CALL FUNCTION 'ZFM_UPDATE_URL_FI_DOC'
          EXPORTING
            im_object_key = lw_objkey
*           url           = ls_fi_doc-zurl
            url           = ls_url
            i_urldes      = lw_url.
        CLEAR lw_url_sel.
        REFRESH lt_url_hd.
      ENDLOOP.
      REFRESH lt_url.
*      CLEAR: lw_objkey.

*      CONCATENATE lw_str ls_fi_doc-fi_doc INTO lw_str SEPARATED BY '|'.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM output-mt_bkcptx_n_receive-bk2.

  ENDMETHOD.
ENDCLASS.
