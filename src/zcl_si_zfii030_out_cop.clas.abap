class ZCL_SI_ZFII030_OUT_COP definition
  public
  create public .

public section.

  interfaces ZII_SI_ZFII030_OUT .
protected section.
private section.

  data GW_HKONT_T012K type HKONT .

  methods BUID_DATA_LOAI_1
    importing
      value(INPUT) type ZDT_ZFII030_IN_HEADER
    exporting
      !CURRENCYAMOUNT type BAPIACCR09_TAB
      !ACCOUNTRECEIVABLE type BAPIACAR09_TAB
      !HEADER type BAPIACHE09 .
  methods BUID_DATA_LOAI_2
    importing
      value(INPUT) type ZDT_ZFII030_IN_HEADER
    exporting
      !CURRENCYAMOUNT type BAPIACCR09_TAB
      !ACCOUNTRECEIVABLE type BAPIACAR09_TAB
      !HEADER type BAPIACHE09 .
  methods BUID_DATA_LOAI_3
    importing
      value(INPUT) type ZDT_ZFII030_IN_HEADER
    exporting
      !CURRENCYAMOUNT type BAPIACCR09_TAB
      !ACCOUNTRECEIVABLE type BAPIACAR09_TAB
      !ACCOUNTGL type BAPIACGL09_TAB
      !HEADER type BAPIACHE09 .
  methods BUID_DATA_LOAI_4
    importing
      value(INPUT) type ZDT_ZFII030_IN_HEADER
    exporting
      !CURRENCYAMOUNT type BAPIACCR09_TAB
      !ACCOUNTRECEIVABLE type BAPIACAR09_TAB
      !ACCOUNTGL type BAPIACGL09_TAB
      !ACCOUNTTAX type BAPIACTX09_TAB
      !CRITERIA type BAPIACKEC9_TAB
      !HEADER type BAPIACHE09 .
  methods BUID_TAB_LOG
    importing
      !INPUT type ZDT_ZFII030_IN_HEADER
    exporting
      !LOG_DATA type ZTB_LOG_RUT_TIEN .
  methods CREATE_LONG_TEXT
    importing
      !GL_ACCOUNT type CHAR10
      !TEXT type STRING
      !OBJECT_KEY type AWKEY .
  methods VALIDATE_INPUT
    importing
      !HEADER type ZDT_ZFII030_IN_HEADER
    changing
      !LOG_DATA type ZTB_LOG_RUT_TIEN
      !OUTPUT type ZDT_ZFII030_OUT
    returning
      value(EXIT) type CHAR1 .
ENDCLASS.



CLASS ZCL_SI_ZFII030_OUT_COP IMPLEMENTATION.


  METHOD BUID_DATA_LOAI_1.

    DATA :lw_cusid   TYPE bu_partner,
          lw_kh_evtp TYPE bu_partner.

    DATA :lw_line TYPE int4.
    header-comp_code = '1000'.
    header-doc_date = input-doc_date.
    header-pstng_date = input-post_date.
    header-ref_doc_no = input-bk_id.
    header-doc_status = '4'.
    header-fisc_year = input-post_date+0(4).
    header-fis_period = input-post_date+4(2).
    header-doc_type = 'Z7'.
    IF input-create_by = 'Hệ thống'.
      header-username = 'SYS_COD'.
    ELSE.
      SELECT SINGLE bname INTO header-username
      FROM usr05
      WHERE parid = 'BPA'
      AND parva = input-create_by.
    ENDIF.


    READ TABLE input-item INTO DATA(ls_item) INDEX 1.
    lw_cusid =  |{ ls_item-cusid ALPHA = IN }|.
    lw_kh_evtp =  |{ ls_item-kh_evtp ALPHA = IN }|.

    DO 2 TIMES.
      lw_line = lw_line + 1.
      APPEND INITIAL LINE TO accountreceivable ASSIGNING FIELD-SYMBOL(<fs_customer>).
      <fs_customer>-itemno_acc = lw_line.
      <fs_customer>-comp_code = '1000'.
      <fs_customer>-bline_date = input-baseline_date.
      <fs_customer>-item_text = ls_item-item_text.
      <fs_customer>-alloc_nmbr = input-bk_id.
      <fs_customer>-ref_key_3 = |{ input-baseline_date+6(2) }.{ input-baseline_date+4(2) }.{ input-baseline_date(4) }|.

      APPEND INITIAL LINE TO currencyamount ASSIGNING FIELD-SYMBOL(<fs_curr_amt>).
      <fs_curr_amt>-itemno_acc = lw_line.
*      <fs_curr_amt>-amt_base = abs( ls_item-amount ).
      <fs_curr_amt>-currency = 'VND'.
      IF lw_line = 1.
        <fs_customer>-customer = lw_cusid.
        <fs_customer>-gl_account = ls_item-tk_no.
        <fs_customer>-profit_ctr = ls_item-profit_center_cusid.
        <fs_curr_amt>-amt_doccur = abs( ls_item-amount ).
      ELSE.
        <fs_customer>-customer = lw_kh_evtp.
        <fs_customer>-gl_account = ls_item-tk_co.
        <fs_customer>-profit_ctr = ls_item-profit_center_evtp.
        <fs_curr_amt>-amt_doccur = -1 * abs( ls_item-amount ).
      ENDIF.

*      <fs_customer>-ref_key_2 = <fs_customer>-profit_ctr.
      <fs_customer>-ref_key_1 = <fs_customer>-profit_ctr.
    ENDDO.
  ENDMETHOD.


  METHOD BUID_DATA_LOAI_2.
    DATA :lw_line TYPE int4.
    DATA :lw_cusid   TYPE bu_partner.
    header-comp_code = '1000'.
    header-doc_date = input-doc_date.
    header-pstng_date = input-post_date.
    header-ref_doc_no = input-bk_id.
    header-doc_status = '4'.
    header-fisc_year = input-post_date+0(4).
    header-fis_period = input-post_date+4(2).
    header-doc_type = 'YG'.
    IF input-create_by = 'Hệ thống'.
      header-username = 'SYS_COD'.
    ELSE.
      SELECT SINGLE bname INTO header-username
      FROM usr05
      WHERE parid = 'BPA'
      AND parva = input-create_by.
    ENDIF.

    READ TABLE input-item INTO DATA(ls_item) INDEX 1.
    lw_cusid =  |{ ls_item-cusid ALPHA = IN }|.
    DO 2 TIMES.
      lw_line = lw_line + 1.
      APPEND INITIAL LINE TO accountreceivable ASSIGNING FIELD-SYMBOL(<fs_customer>).
      <fs_customer>-itemno_acc = lw_line.
      <fs_customer>-profit_ctr = ls_item-profit_center_cusid.
      <fs_customer>-comp_code = '1000'.
      <fs_customer>-bline_date = input-baseline_date.
      <fs_customer>-item_text = ls_item-item_text.
      <fs_customer>-alloc_nmbr = input-bk_id.
*      <fs_customer>-ref_key_2 = <fs_customer>-profit_ctr.
      <fs_customer>-ref_key_3 = |{ input-baseline_date+6(2) }.{ input-baseline_date+4(2) }.{ input-baseline_date(4) }|.
      <fs_customer>-customer = lw_cusid.
      <fs_customer>-ref_key_1 = <fs_customer>-customer.
      APPEND INITIAL LINE TO currencyamount ASSIGNING FIELD-SYMBOL(<fs_curr_amt>).
      <fs_curr_amt>-itemno_acc = lw_line.
      <fs_curr_amt>-currency = 'VND'.
      IF lw_line = 1.
        <fs_customer>-gl_account = ls_item-tk_no.
        <fs_curr_amt>-amt_doccur = abs( ls_item-amount ).
        <fs_curr_amt>-amt_base = abs( ls_item-amount ).
      ELSE.
        <fs_customer>-gl_account = ls_item-tk_co.
        <fs_curr_amt>-amt_base = -1 * abs( ls_item-amount ).
        <fs_curr_amt>-amt_doccur = -1 * abs( ls_item-amount ).
      ENDIF.
    ENDDO.


  ENDMETHOD.


  METHOD BUID_DATA_LOAI_3.
    DATA :lw_cusid   TYPE bu_partner,
          lw_kh_evtp TYPE bu_partner.
    READ TABLE input-item INTO DATA(ls_item) INDEX 1.
    lw_cusid =  |{ ls_item-cusid ALPHA = IN }|.
    lw_kh_evtp =  |{ ls_item-kh_evtp ALPHA = IN }|.
    SELECT SINGLE hkont
    FROM t012k
    INTO @DATA(lw_hkont)
          WHERE bukrs = '1000'
          AND bankn = @ls_item-so_tk_chi.
    gw_hkont_t012k = lw_hkont.
    DATA :lw_line TYPE int4.
    header-comp_code = '1000'.
    header-doc_date = input-doc_date.
    header-pstng_date = input-post_date.
    header-ref_doc_no = input-bk_id.
    header-doc_status = '4'.
    header-fisc_year = input-post_date+0(4).
    header-fis_period = input-post_date+4(2).
    header-doc_type = 'YD'.

    IF input-create_by = 'Hệ thống'.
      header-username = 'SYS_COD'.
    ELSE.
      SELECT SINGLE bname INTO header-username
      FROM usr05
      WHERE parid = 'BPA'
      AND parva = input-create_by.
    ENDIF.


    APPEND INITIAL LINE TO accountgl ASSIGNING FIELD-SYMBOL(<fs_acc_gl>).
    <fs_acc_gl>-itemno_acc = 1.
    <fs_acc_gl>-gl_account = lw_hkont.
    <fs_acc_gl>-doc_type   = 'YD'.
    <fs_acc_gl>-comp_code  = '1000'.
    <fs_acc_gl>-pstng_date = input-post_date.
    <fs_acc_gl>-item_text  = ls_item-item_text.
    <fs_acc_gl>-profit_ctr = 'P0000010'.
    <fs_acc_gl>-alloc_nmbr = input-bk_id.
    <fs_acc_gl>-ref_key_1  = lw_cusid.
*    <fs_acc_gl>-ref_key_2  = 'P0000010'.

    APPEND INITIAL LINE TO currencyamount ASSIGNING FIELD-SYMBOL(<fs_curr_amt>).
    <fs_curr_amt>-itemno_acc = 1.
    <fs_curr_amt>-currency   = 'VND'.
    <fs_curr_amt>-amt_doccur = abs( ls_item-amount ) * -1.
    <fs_curr_amt>-amt_base   = abs( ls_item-amount ) * -1.

    APPEND INITIAL LINE TO accountreceivable ASSIGNING FIELD-SYMBOL(<fs_customer>).
    <fs_customer>-itemno_acc = 2.
    <fs_customer>-profit_ctr = 'P0000010'.
    <fs_customer>-comp_code = '1000'.
    <fs_customer>-gl_account = ls_item-tk_no.
    <fs_customer>-bline_date = input-baseline_date.
    <fs_customer>-customer = lw_cusid.
    <fs_customer>-item_text = ls_item-item_text.
    <fs_customer>-alloc_nmbr = input-bk_id.
*    <fs_customer>-ref_key_2 = 'P0000010'.
    <fs_customer>-ref_key_3 = |{ input-baseline_date+6(2) }.{ input-baseline_date+4(2) }.{ input-baseline_date(4) }|.
    <fs_customer>-ref_key_1 = <fs_customer>-customer.

    APPEND INITIAL LINE TO currencyamount ASSIGNING <fs_curr_amt>.
    <fs_curr_amt>-itemno_acc = 2.
    <fs_curr_amt>-currency   = 'VND'.
    <fs_curr_amt>-amt_doccur = abs( ls_item-amount ) .
    <fs_curr_amt>-amt_base   = abs( ls_item-amount ) .

  ENDMETHOD.


  METHOD BUID_DATA_LOAI_4.
    DATA :lw_line TYPE int4.
    DATA :lw_cusid   TYPE bu_partner,
          lw_kh_evtp TYPE bu_partner.
    DATA :ls_criteria TYPE bapiackec9.
    DEFINE add_criteria.
      ls_criteria-itemno_acc = &1.
      ls_criteria-fieldname = &2.
      ls_criteria-character = &3.

      APPEND ls_criteria TO criteria.
      CLEAR ls_criteria.
    END-OF-DEFINITION.
    header-comp_code = '1000'.
    header-doc_date = input-doc_date.
    header-pstng_date = input-post_date.
    header-ref_doc_no = input-bk_id.
    header-doc_status = '4'.
    header-fisc_year = input-post_date+0(4).
    header-fis_period = input-post_date+4(2).
    header-doc_type = 'Y1'.
    IF input-create_by = 'Hệ thống'.
      header-username = 'SYS_COD'.
    ELSE.
      SELECT SINGLE bname INTO header-username
      FROM usr05
      WHERE parid = 'BPA'
      AND parva = input-create_by.
    ENDIF.

    READ TABLE input-item INTO DATA(ls_item) INDEX 1.
*    lw_cusid =  |{ ls_item-cusid ALPHA = IN }|.
    lw_kh_evtp =  |{ ls_item-kh_evtp ALPHA = IN }|.
    APPEND INITIAL LINE TO accountreceivable ASSIGNING FIELD-SYMBOL(<fs_customer>).
    <fs_customer>-itemno_acc = 1.
    <fs_customer>-comp_code = '1000'.
    <fs_customer>-bline_date = input-baseline_date.
    <fs_customer>-gl_account = ls_item-tk_no.
*    <fs_customer>-profit_ctr = 'P0000010'.
    <fs_customer>-profit_ctr = ls_item-profit_center_evtp.
    <fs_customer>-item_text = ls_item-item_text.
    <fs_customer>-alloc_nmbr = input-bk_id.
*    <fs_customer>-customer = lw_cusid.
    <fs_customer>-customer = lw_kh_evtp.
    <fs_customer>-tax_code = 'O1'.
*    <fs_customer>-ref_key_2 = 'P0000010'.
    <fs_customer>-ref_key_3 = |{ input-baseline_date+6(2) }.{ input-baseline_date+4(2) }.{ input-baseline_date(4) }|.
    <fs_customer>-ref_key_1 = <fs_customer>-customer.

    APPEND INITIAL LINE TO currencyamount ASSIGNING FIELD-SYMBOL(<fs_curr_amt>).
    <fs_curr_amt>-itemno_acc = 1.
    <fs_curr_amt>-currency   = 'VND'.
    <fs_curr_amt>-amt_doccur = abs( ls_item-amount ) .
    <fs_curr_amt>-amt_base   = abs( ls_item-amount ) .

    APPEND INITIAL LINE TO accountgl ASSIGNING FIELD-SYMBOL(<fs_acc_gl>).
    <fs_acc_gl>-itemno_acc = 2.
    <fs_acc_gl>-gl_account = ls_item-tk_co.
    <fs_acc_gl>-doc_type   = 'Y1'.
    <fs_acc_gl>-comp_code  = '1000'.
    <fs_acc_gl>-pstng_date = input-post_date.
    <fs_acc_gl>-item_text  = ls_item-item_text.
*    <fs_acc_gl>-profit_ctr = 'P0000010'.
    <fs_acc_gl>-profit_ctr = ls_item-profit_center_evtp.
    <fs_acc_gl>-alloc_nmbr = input-bk_id.
*    <fs_acc_gl>-ref_key_1  = lw_cusid.
    <fs_acc_gl>-ref_key_1 = lw_kh_evtp.
    <fs_acc_gl>-tax_code = 'O1'.
*    <fs_acc_gl>-ref_key_2  = 'P0000010'.

    SELECT SINGLE segment FROM cepc WHERE prctr = @<fs_acc_gl>-profit_ctr INTO @DATA(lw_segment).
    add_criteria :2 'BUKRS' '1000',
                  2 'KOKRS' '1000',
                  2 'SEGMENT' lw_segment,
                  2 'PRCTR' <fs_acc_gl>-profit_ctr,
                  2 'KNDNR' input-bk_id,
                  2 'WWLV' '100'.


    APPEND INITIAL LINE TO currencyamount ASSIGNING <fs_curr_amt>.
    <fs_curr_amt>-itemno_acc = 2.
    <fs_curr_amt>-currency   = 'VND'.
    <fs_curr_amt>-amt_doccur = -1 * abs( ls_item-net_amount ) .
    <fs_curr_amt>-amt_base   = -1 * abs( ls_item-net_amount ) .

    APPEND INITIAL LINE TO accounttax ASSIGNING FIELD-SYMBOL(<fs_acc_tax>).
    <fs_acc_tax>-itemno_acc = 3.
    SELECT SINGLE konth FROM t030k INTO <fs_acc_tax>-gl_account WHERE ktopl = 1000 AND ktosl = 'MWS' AND mwskz = 'O1'.
    <fs_acc_tax>-tax_code = 'O1'.

    APPEND INITIAL LINE TO currencyamount ASSIGNING <fs_curr_amt>.
    <fs_curr_amt>-itemno_acc = 3.
    <fs_curr_amt>-currency   = 'VND'.
    <fs_curr_amt>-amt_doccur = -1 * abs( ls_item-tax_amount ) .
    <fs_curr_amt>-amt_base   = -1 * abs( ls_item-tax_amount ) .



  ENDMETHOD.


  METHOD BUID_TAB_LOG.
    log_data = CORRESPONDING #( input ).
    log_data = CORRESPONDING #( BASE ( log_data ) input-item[ 1 ] MAPPING pc_cusid =  profit_center_cusid pc_evtp = profit_center_evtp amt = amount ).
    log_data-donvi = 'VND'.
    log_data-amt = log_data-amt / 100.
    log_data-net_amount = log_data-net_amount / 100.
    log_data-tax_amount = log_data-tax_amount / 100.
    log_data-guid = cl_system_uuid=>create_uuid_x16_static( ).
    log_data-date_cre = sy-datum.
    log_data-time_cre = sy-uzeit.
    log_data-gjahr = input-post_date+0(4).
    log_data-bukrs = '1000'.
    INSERT ztb_log_rut_tien FROM log_data.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD CREATE_LONG_TEXT.
    DATA :lt_tab   TYPE TABLE OF tline,
          lw_fname TYPE thead-tdname.
    CHECK text <> ''.
    CALL FUNCTION 'ZFM_SPLIT_LONG_TEXT'
      EXPORTING
        iv_string = text
        iv_lenght = 132
      IMPORTING
        result    = lt_tab.

    lw_fname = |{ object_key+10(4) }{ object_key+0(10) }{ object_key+14(4) }001|.
    CALL FUNCTION 'CREATE_TEXT'
      EXPORTING
        fid       = '0001'
        flanguage = 'E'
        fname     = lw_fname
        fobject   = 'DOC_ITEM'
      TABLES
        flines    = lt_tab.

  ENDMETHOD.


  METHOD VALIDATE_INPUT.
    DEFINE set_mess_err.
      output-bk_id = header-bk_id.
      output-bkc1 = header-item[ 1 ]-bkc1.
      output-status = &3.
      output-error = &1.
    REPLACE '@' IN output-error WITH &2.
    log_data-status = output-status.
    log_data-error = output-error.
    MODIFY  ztb_log_rut_tien FROM log_data.
    COMMIT WORK AND WAIT.
    exit = 'X'.
    RETURN.
    END-OF-DEFINITION.

    IF header-bk_id IS INITIAL.
      set_mess_err 'Bảng kê ID null' '' '0'.
    ENDIF.

    IF header-create_by <> 'Hệ thống'.
      SELECT SINGLE bname FROM usr05 WHERE parid = 'BPA' AND parva = @header-create_by INTO @DATA(lw_bp).
      IF sy-subrc <> 0.
        set_mess_err 'BP người tổng hợp bảng kê / hệ thống không tồn tại - @' header-create_by '0'.
      ENDIF.
    ENDIF.

*    SELECT SINGLE belnr FROM bkpf WHERE gjahr = @header-post_date+0(4) AND stblg = '' AND
*                                        bukrs = '1000' AND
*                                        xblnr = @header-bk_id INTO @DATA(lw_belnr).
    SELECT SINGLE doc_no FROM ztb_log_rut_tien AS a INNER JOIN bkpf AS b ON a~doc_no = b~belnr AND a~bukrs = b~bukrs AND a~gjahr = b~gjahr
    WHERE stblg = '' AND a~bk_id = @header-bk_id INTO @DATA(lw_belnr).
    IF sy-subrc = 0.
      output-doc_no = lw_belnr.
      set_mess_err 'Bảng kê @ đã được hạch toán' header-bk_id '1'.
    ENDIF.


    DATA(lw_tk_no) = VALUE #( header-item[ 1 ]-tk_no OPTIONAL ).
    DATA(lw_tk_co) =  VALUE #( header-item[ 1 ]-tk_co OPTIONAL ).
    SELECT saknr FROM ska1
    INTO TABLE @DATA(lt_gl) WHERE ktopl = '1000' AND saknr = @lw_tk_no OR saknr =  @lw_tk_co.
    SORT lt_gl BY saknr.

    IF header-id_hachtoan <> 3.
      IF NOT line_exists( lt_gl[ saknr = lw_tk_co ] ).
        set_mess_err 'TK_CO @ không tồn tại' lw_tk_co '0'.
      ENDIF.
    ENDIF.

    IF NOT line_exists( lt_gl[ saknr = lw_tk_no ] ).
      set_mess_err 'TK_NO @ không tồn tại' lw_tk_no '0'.
    ENDIF.


    DATA :lw_bp_cus  TYPE bu_partner,
          lw_bp_evtp TYPE  bu_partner.
    lw_bp_cus = VALUE #( header-item[ 1 ]-cusid OPTIONAL ).
    lw_bp_evtp = VALUE #( header-item[ 1 ]-kh_evtp OPTIONAL ).
    lw_bp_cus = |{ lw_bp_cus ALPHA = IN }|.
    lw_bp_evtp = |{ lw_bp_evtp ALPHA = IN }|.

    SELECT partner
    FROM but000
    INTO TABLE @DATA(lt_partner)
          WHERE partner = @lw_bp_cus OR partner = @lw_bp_evtp.
    IF header-id_hachtoan <> 4.
      IF NOT line_exists( lt_partner[ partner = lw_bp_cus ] ).
        set_mess_err 'CUSID @ không tồn tại' lw_bp_cus '0'.
      ENDIF.
    ENDIF.
    IF header-id_hachtoan = 1.
      IF NOT line_exists( lt_partner[ partner = lw_bp_evtp ] ).
        set_mess_err 'KH_EVTP @ không tồn tại' lw_bp_evtp '0'.
      ENDIF.
    ENDIF.


    IF header-id_hachtoan = 1 OR header-id_hachtoan = 2 OR header-id_hachtoan = 4.
      DATA(lw_pc_evtp) = VALUE #( header-item[ 1 ]-profit_center_evtp  OPTIONAL ).
      DATA(lw_pc_cusid) = VALUE #( header-item[ 1 ]-profit_center_cusid OPTIONAL ).
      SELECT  prctr FROM cepc
      INTO TABLE @DATA(lt_prctr)

      WHERE prctr =  @lw_pc_evtp OR prctr = @lw_pc_cusid.
      IF header-id_hachtoan = 1 OR header-id_hachtoan = 2.
        IF NOT line_exists( lt_prctr[ prctr = lw_pc_cusid ] ).
          set_mess_err 'PROFIT_CENTER_CUSID @ không tồn tại' lw_pc_cusid '0'.
        ENDIF.
      ENDIF.
      IF header-id_hachtoan = 1 OR header-id_hachtoan = 4.
        IF NOT line_exists( lt_prctr[ prctr = lw_pc_evtp ] ).
          set_mess_err 'PROFIT_CENTER_EVTP @ không tồn tại' lw_pc_evtp '0'.
        ENDIF.
      ENDIF.

*      IF line_exists( lt_prctr[ prctr = lw_pc_cusid ] ).
*        IF header-id_hachtoan = 1.
*          IF NOT line_exists( lt_prctr[ prctr = lw_pc_evtp ] ).
*            set_mess_err 'PROFIT_CENTER_EVTP @ không tồn tại' lw_pc_evtp.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        set_mess_err 'PROFIT_CENTER_CUSID @ không tồn tại' lw_pc_cusid.
*      ENDIF.
    ENDIF.

    IF header-id_hachtoan = 3.
      DATA(lw_so_tk_chi) = VALUE #( header-item[ 1 ]-so_tk_chi OPTIONAL ).
      SELECT SINGLE hkont
      FROM t012k
      INTO @DATA(lw_gl_chi)
            WHERE bukrs = '1000'
            AND bankn = @lw_so_tk_chi.
      IF sy-subrc <> 0.
        set_mess_err 'SO_TK_CHI @ không tồn tại' lw_so_tk_chi '0'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZII_SI_ZFII030_OUT~SI_ZFII030_OUT.
*** **** INSERT IMPLEMENTATION HERE **** ***
    DATA: ls_header            TYPE bapiache09,
          ls_customercpd       TYPE bapiacpa09,
          lt_accountgl         TYPE TABLE OF bapiacgl09,
          lt_accounttax        TYPE TABLE OF bapiactx09,
          lt_criteria          TYPE TABLE OF bapiackec9,
          lt_accountreceivable TYPE TABLE OF bapiacar09,
          lt_currencyamount    TYPE TABLE OF bapiaccr09,
          lt_accountpayable    TYPE TABLE OF bapiacap09,
          lt_ext               TYPE TABLE OF bapiacextc,
          lt_return            TYPE TABLE OF bapiret2.

    DATA: lv_obj_key  TYPE bapiache09-obj_key,
          ls_log_data TYPE ztb_log_rut_tien.

    me->buid_tab_log(
    EXPORTING
      input    = input-mt_zfii030_in-header                 " Proxy Structure (generated)
      IMPORTING
        log_data =  ls_log_data                " Rút tiền HDR
      ).

    CHECK  me->validate_input(
    EXPORTING
        header = input-mt_zfii030_in-header                 " Proxy Structure (generated)
    CHANGING
        log_data =  ls_log_data
        output = output-mt_zfii030_out                  " Proxy Structure (generated)
      )  <> 'X'.

    output-mt_zfii030_out-bk_id = input-mt_zfii030_in-header-bk_id.
    output-mt_zfii030_out-bkc1 = input-mt_zfii030_in-header-item[ 1 ]-bkc1.

    CASE input-mt_zfii030_in-header-id_hachtoan.
      WHEN 1.
        me->buid_data_loai_1(
        EXPORTING
            input             =  input-mt_zfii030_in-header                " Proxy Structure (generated)
          IMPORTING
            currencyamount    =  lt_currencyamount                " Table Type for Structure BAPIACGL09
            accountreceivable =  lt_accountreceivable               " Table Type for Structure BAPIACAP09
            header            =  ls_header                 " Header
          ).
      WHEN 2.
        me->buid_data_loai_2(
          EXPORTING
            input             =  input-mt_zfii030_in-header                " Proxy Structure (generated)
          IMPORTING
            currencyamount    =  lt_currencyamount                " Table Type for Structure BAPIACGL09
            accountreceivable =  lt_accountreceivable               " Table Type for Structure BAPIACAP09
            header            =  ls_header                 " Header
          ).
      WHEN 3.
        me->buid_data_loai_3(
        EXPORTING
          input             =  input-mt_zfii030_in-header                 " Proxy Structure (generated)
        IMPORTING
          currencyamount    =  lt_currencyamount                " Table Type for Structure BAPIACCR09
          accountreceivable =  lt_accountreceivable               " Table Type for Structure BAPIACAR09
          accountgl         =  lt_accountgl                  " Table Type for Structure BAPIACGL09
          header            =  ls_header             " Header
        ).
      WHEN 4.
        me->buid_data_loai_4(
        EXPORTING
          input             = input-mt_zfii030_in-header               " Proxy Structure (generated)
        IMPORTING
          currencyamount    = lt_currencyamount                " Table Type for Structure BAPIACCR09
          accountreceivable = lt_accountreceivable                  " Table Type for Structure BAPIACAR09
          accountgl         = lt_accountgl                 " Table Type for Structure BAPIACGL09
          accounttax        = lt_accounttax                " Tax line items
          criteria          = lt_criteria
          header            = ls_header               " Header
          ).
      WHEN OTHERS.
        output-mt_zfii030_out-error = |ID hạch toán { input-mt_zfii030_in-header-id_hachtoan } không hợp lệ|.
        output-mt_zfii030_out-status = '0'.
        EXIT .
    ENDCASE.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = ls_header
      IMPORTING
        obj_key           = lv_obj_key
      TABLES
        accounttax        = lt_accounttax
        accountreceivable = lt_accountreceivable
        accountgl         = lt_accountgl
        currencyamount    = lt_currencyamount
        return            = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      output-mt_zfii030_out-status = '0'.

      LOOP AT lt_return INTO DATA(ls_return) WHERE type = 'E'.
        output-mt_zfii030_out-error = COND #( WHEN output-mt_zfii030_out-error IS INITIAL THEN ls_return-message
                                              ELSE |{ output-mt_zfii030_out-error }, { ls_return-message }|
                                            ).

      ENDLOOP.
      ls_log_data-status = '0'.
      ls_log_data-error = output-mt_zfii030_out-error.
      MODIFY ztb_log_rut_tien FROM ls_log_data.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      ls_log_data-status = '1'.
      ls_log_data-doc_no = lv_obj_key+0(10).
      IF input-mt_zfii030_in-header-id_hachtoan = 3 AND ( gw_hkont_t012k CP '112*' OR gw_hkont_t012k CP '113*' ).
        me->create_long_text(
        EXPORTING
          gl_account = gw_hkont_t012k                 " Character Field with Length 10
          text       = VALUE #( input-mt_zfii030_in-header-item[ 1 ]-item_text OPTIONAL )
          object_key = lv_obj_key                 " Object key
          ).
      ENDIF.
      MODIFY ztb_log_rut_tien FROM ls_log_data.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      output-mt_zfii030_out-status = '1'.
      output-mt_zfii030_out-doc_no = lv_obj_key+0(10).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
