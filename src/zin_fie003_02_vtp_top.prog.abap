*&---------------------------------------------------------------------*
*& Include ZIN_FIE003_02_TOP                        - Report ZPG_FIE003_02
*&---------------------------------------------------------------------*
report zpg_fie003_02.

**********************************************************************
"For reading CSV formating
types:
  kcdu_srec(10000),                     "sender record (maximal length!)
  begin of kcdu_srecs_wa,
    srec type kcdu_srec,
  end of kcdu_srecs_wa,
  kcdu_srecs        type kcdu_srecs_wa occurs 10,

  kcde_intern_value type string,
  begin of kcde_intern_struc,
    row   type i,
    col   type numc3,
    value type string,
  end of kcde_intern_struc,
  kcde_intern type kcde_intern_struc  occurs 0.

constants: c_esc   value '"'.
**********************************************************************

*----------------------------------------------------------------------*
* D A T A
*----------------------------------------------------------------------*
data:
  begin of gt_input occurs 0,
    col001(50)  type c, "STT
    col002(10)  type c, "Ngày chứng từ
    col003(10)  type c, "Ngày hạch toán
    col004(25)  type c, "Tiêu đề chứng từ
    col005(20)  type c, "Mẫu hóa đơn
    col006(16)  type c, "Số hoá đơn
    col007(02)  type c, "Loại chứng từ
    col008(04)  type c, "Mã công ty
    col009(05)  type c, "Đ.vị tiền tệ
    col010(10)  type c, "Ngày lấy tỉ giá trong hệ thống
    col011(20)  type c, "Reference Key 1 HD
    col012(20)  type c, "Reference Key 2 HD
    col013(01)  type c, "Nợ/Có
    col014(10)  type c, "Tài khoản
    col015(10)  type c, "mã BP
    col016(01)  type c, "Special GL indicator
    col017(50)  type c, "Số tiền
    col018(02)  type c, "Thuế suất
    col019(50)  type c, "Số tiền thuế
    col020(04)  type c, "Chi cục thuế
    col021(10)  type c, "Chi nhánh
    col022(10)  type c, "TT Chi phí
    col023(10)  type c, "TT lợi nhuận
    col024(255) type c, "Diễn giải
    col025(18)  type c, "Nội dung gán
    col026(04)  type c, "Điều khoản thanh toán
    col027(10)  type c, "Ngày tính nợ
    col028(01)  type c, "Khóa thanh toán
    col029(01)  type c, "Phương thức thanh toán
    col030(04)  type c, "Mã tài khoản ngân hàng giao dịch
    col031(01)  type c, "Đánh dấu ghi âm
    col032(12)  type c, "Mã dòng tiền
    col033(12)  type c, "Reference Key 2 Line
    col034(20)  type c, "Reference Key 3 Line
    col035(140) type c, "Tên
    col036(35)  type c, "Địa chỉ
    col037(35)  type c, "Tỉnh/ thành phố
    col038(16)  type c, "Mã số thuế
    col039(04)  type c, "KOKRS (Controlling Area)
    col040(10)  type c, "KNDNR ( Customer)
    col041(40)  type c, "Product (ARTNR)
    col042(04)  type c, "Billing type (FKART)
    col043(06)  type c, "Sales ord. item (KDPOS)
    col044(25)  type c, "Order (KAUFN)
    col045(04)  type c, "Plant (WERKS)
    col046(16)  type c, "Functional Area (FKBER)
    col047(02)  type c, "SEGMENT
    col048(04)  type c, "Sales Org (VKORG ).
    col049(02)  type c, "Distr. Channel (VTWEG)
    col050(02)  type c, "Division (SPART)
    col051(08)  type c, "WBS Element (PSPNR)
    col052(12)  type c, "Cost Object(KSTRG)
    col053(10)  type c, "Partner PC (PPRCTR)
    col054(03)  type c, "Sales group (KMVKGR)
    col055(08)  type c, "Sales employee (KMVTNR)
    col056(10)  type c, "Bill-to party (KUNRE)
    col057(10)  type c, "Ship-to party (KUNWE)
    col058(18)  type c, "Service group (WWPGR)
    col059(04)  type c, "sale office (KMVKBU)
    col060(18)  type c, "Service type (WWSER )
    col061(18)  type c, "Lĩnh vực kinh doanh (WWLV)
  end of gt_input,

  begin of ls_addition,
    kokrs  type acdoca-kokrs,
    kndnr  type char10,
    artnr  type char40,
    fkart  type acdoca-fkart,
    kdpos  type acdoca-kdpos,
    kaufn  type char10,
    werks  type acdoca-werks,
    fkber  type acdoca-rfarea,
    vkorg  type acdoca-vkorg,
    vtweg  type acdoca-vtweg,
    spart  type acdoca-spart,
    pspnr  type acdoca-ps_psp_pnr,
    kstrg  type acdoca-kstrg,
    pprctr type acdoca-pprctr,
    kmvkrg type acdoca-kmvkgr_pa,
    kmvtnr type acdoca-kmvtnr_pa,
    kunre  type acdoca-kunre,
    kunwe  type acdoca-kunwe,
    wwpgr  type acdoca-wwpgr_pa,
    kmvkbu type acdoca-kmvkbu_pa,
    wwser  type acdoca-wwser_pa,
    wwlv   type acdoca-wwlv_pa,
  end of ls_addition,

  begin of gt_data occurs 0,
    stt           type i,
    doc_date      type bkpf-bldat,
    pstng_date    type bkpf-budat,
    header_txt    type bkpf-bktxt,
    mau_hd        type char20,
    ref_doc_no    type bkpf-xblnr,
    doc_type      type bkpf-blart,
    comp_code     type bkpf-bukrs,
    currency      type bkpf-waers,
    trans_date    type bkpf-wwert,
    xref1_hd      type bkpf-xref1_hd,
    xref2_hd      type bkpf-xref2_hd,
    de_cre_ind1   type bseg-shkzg,
    gl_account    type bseg-hkont,
    bp            type but000-partner,
    sp_gl_ind     type bseg-umskz,
    amt_doccur    type bapiaccr09-amt_doccur,
    tax_code      type bseg-mwskz,
    tax_amt       type bapiaccr09-tax_amt,
    businessplace type bseg-bupla,
    segment       type bseg-segment,
    costcenter    type bseg-kostl,
    profit_ctr    type bseg-prctr,
    item_text     type string,
    alloc_nmbr    type bseg-zuonr,
    pmnttrms      type bseg-zterm,
    bline_date    type bseg-zfbdt,
    pmnt_block    type bseg-zlspr,
    pymt_meth     type bseg-zlsch,
    partner_bk    type bseg-bvtyp,
    neg_postng    type bseg-xnegp,
    ref_key_1     type bseg-xref1,
    ref_key_2     type bseg-xref2,
    ref_key_3     type bseg-xref3,
    name_1        type char140,
    street        type char35,
    city          type char35,
    tax_no_1      type char16.
    include structure ls_addition.
data:
  belnr     type bkpf-belnr,
  buzei     type bseg-buzei,
  customer  type bseg-kunnr,
  vendor_no type bseg-lifnr,
  lineindex type int4,
  status    type icons-text,
  message   type string,
  end of gt_data,

  gt_t001            type table of t001,
  gt_but000          type table of but000,
  gt_a003            type table of a003,
  gt_j_1bbranch      type table of j_1bbranch,
  gt_cepc            type table of cepc,
  gt_csks            type table of csks,
  gt_skb1            type table of skb1,
  gt_fagl_segm       type table of fagl_segm,


  gt_glacc           type table of bapiacgl09,
  gt_customer        type table of bapiacar09,
  gt_vendor          type table of bapiacap09,
  gt_tax             type table of bapiactx09,
  gt_cr              type table of bapiaccr09,
  gt_criteria        type table of bapiackec9,
  gt_extens          type table of bapiparex,
  gt_return          type table of bapiret2,
  gt_data_tmp        type table of ztb_fie003_02_lo,

  gt_ztb_fie003_02_f type table of ztb_fie003_02_f,
  gs_selected_file   type ztb_fie003_02_f,
  go_salv            type ref to cl_salv_table,

  gw_server_dir      type string.

  DATA: gr_type_pgr_up TYPE fccx_t_range_row,
      gr_acc_pgr_up  TYPE fccx_t_range_row,
      gt_partner_gr  TYPE TABLE OF ztb_partner_gr.

constants: gc_server_temp type string value 'ZFIE003002_TEMP.xlsx'.
constants: gc_folder type string value 'DIR_FI00302'.
*----------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N
*----------------------------------------------------------------------*
selection-screen begin of block bl01 with frame title text-001.
parameters:
  p_fpath  type string,
  p_frunn  type ztb_fie003_02_f-guid,
  p_upload radiobutton group g1 default 'X' user-command c1,
  p_test   radiobutton group g1,
  p_run    radiobutton group g1,
  p_downlo radiobutton group g1,
  p_remove radiobutton group g1
  .
selection-screen end of block bl01.
