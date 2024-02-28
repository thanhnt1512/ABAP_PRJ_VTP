FUNCTION ZFM_CR_RULE_SET_COP.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(LT_ASSET) TYPE  ZTT_ASSET
*"     REFERENCE(COMP) TYPE  BUKRS
*"     REFERENCE(ASS_CAT) TYPE  KONTY
*"--------------------------------------------------------------------
  TYPES:
    BEGIN OF lty_s_anek_ext.
      INCLUDE STRUCTURE anek.
  TYPES:
    subta TYPE acdoca-subta,
    END OF lty_s_anek_ext.

  DATA:
    lo_fins_acdoc_change TYPE REF TO cl_fins_acdoc_change,
    ls_anek_ext          TYPE lty_s_anek_ext,
    lt_anek_ext          TYPE STANDARD TABLE OF lty_s_anek_ext,
    lt_acdoca_upd        TYPE finst_acdoca,
    lt_fieldname_upd     TYPE TABLE OF fieldname,
    l_ionr               LIKE ionr.

  TYPES:  BEGIN OF gty_s_postab.                                                 "Begin of S2I
      INCLUDE STRUCTURE aimtv.
  TYPES: objnr         LIKE anla-objnr,              " merker co-objekt
         zaehl         LIKE sy-tabix,    " indexzaehler
         workf(1)      TYPE c, " value ' ', " workflag = x wenn bureg bearb.
         ampel(1)      TYPE c, "value '0', " alv ampel    "4.6c
         xextend_afabe TYPE xfeld.
  TYPES:   END OF gty_s_postab.
  TYPES:  gty_t_postab TYPE STANDARD TABLE OF gty_s_postab.

  DATA: postab                  TYPE gty_t_postab WITH HEADER LINE.

  DATA :ls_anla  TYPE anla,
        ls_cobrb TYPE cobrb.


  TYPES: BEGIN OF ty_cobra_buf.
      INCLUDE STRUCTURE cobra.
  TYPES: uflag LIKE dkobr-upd_flag,
         END OF ty_cobra_buf.
  TYPES: ty_t_cobra_buf TYPE ty_cobra_buf OCCURS 10.

*.COBRB-Puffer mit Änderungsflag
  TYPES: BEGIN OF ty_cobrb_buf.
      INCLUDE STRUCTURE cobrb.
  TYPES: uflag LIKE dkobr-upd_flag,
         END OF ty_cobrb_buf.
  TYPES: ty_t_cobrb_buf TYPE ty_cobrb_buf OCCURS 10.

  DATA: lt_objnr LIKE TABLE OF ionrb WITH HEADER LINE,
        lt_cobra LIKE TABLE OF cobra WITH HEADER LINE,
        lt_cobrb LIKE TABLE OF cobrb WITH HEADER LINE.
*   Типы TY_T_COBRA_BUF, TY_T_COBRB_BUF см. в LKOBSTOP
  DATA: l_mem_cobra TYPE ty_t_cobra_buf WITH HEADER LINE,
        l_mem_cobrb TYPE ty_t_cobrb_buf WITH HEADER LINE.

  DATA: l_lfdnr LIKE cobrb-lfdnr.

  lo_fins_acdoc_change = new cl_fins_acdoc_change( ).

  LOOP AT lt_asset INTO DATA(ls_asset).

    CALL METHOD cl_faa_mdo_services=>read_asset_for_posting
      EXPORTING
        iv_comp_code   = comp
        iv_asset_no    = CONV #( |{ ls_asset-asset_num ALPHA = IN }| )
        iv_asset_subno = '0000'
        ib_lock        = 'X'                              "2069770
      IMPORTING
        es_anla        = ls_anla.

    SELECT MAX( lfdnr ) INTO l_lfdnr FROM cobrb WHERE objnr = ls_anla-objnr.
    SELECT MAX( bureg ) INTO @DATA(lw_bureg) FROM cobrb WHERE objnr = @ls_anla-objnr.

    lt_objnr-objnr = ls_anla-objnr.
    APPEND lt_objnr.
    CALL FUNCTION 'K_SRULE_PRE_READ'
      EXPORTING
        i_pflege         = ' '
      TABLES
        t_sender_objnr   = lt_objnr
      EXCEPTIONS
        wrong_parameters = 1
        OTHERS           = 2.


    CALL FUNCTION 'K_SETTLEMENT_RULE_GET'
      EXPORTING
        objnr     = ls_anla-objnr
        x_all     = 'X'
      TABLES
        e_cobra   = lt_cobra
        e_cobrb   = lt_cobrb
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.

    ENDIF.
    READ TABLE lt_cobra WITH KEY objnr = ls_anla-objnr.
    IF sy-subrc <> 0.
      CALL FUNCTION 'K_SRULE_CREATE'
        EXPORTING
          i_objnr             = ls_anla-objnr
          i_check_only_local  = ' '
        IMPORTING
          e_cobra             = lt_cobra
        EXCEPTIONS
          rule_already_exists = 1
          OTHERS              = 2.
      IF sy-subrc <> 0.

      ENDIF.
    ENDIF.


    ls_cobrb-objnr = ls_anla-objnr.
    ls_cobrb-lfdnr = '001'.
    ls_cobrb-perbz = 'EPA'.
    ls_cobrb-bureg = lw_bureg + 1.
    ls_cobrb-prozs = ls_asset-per_set.
    ls_cobrb-bwaer = 'VND'.
    ls_cobrb-avorg = 'KOAO'.
    ls_cobrb-konty = ass_cat.
    ls_cobrb-kokrs = comp.
    ls_cobrb-bukrs = comp.
    ls_cobrb-anln1 = |{ ls_asset-set_recv ALPHA = IN }|.
    ls_cobrb-anln2 = '0000'.

    ls_cobrb-extnr = '001'.
    l_ionr-obart = ass_cat.
    l_ionr-objid    = comp.
    l_ionr-objid+4  = ls_cobrb-anln1.
    l_ionr-objid+16 = ls_cobrb-anln2.
    ls_cobrb-rec_objnr1 = l_ionr.


    CALL FUNCTION 'K_SRULE_EXPORT_IMPORT'
      EXPORTING
        i_mode = 'EX'.

    IMPORT l_mem_cobra l_mem_cobrb FROM MEMORY ID 'K_SRULE'.

    READ TABLE l_mem_cobra WITH KEY objnr = lt_cobra-objnr.

    IF sy-subrc <> 0.
      MOVE-CORRESPONDING lt_cobra TO l_mem_cobra.
      l_mem_cobra-uflag = 'I'.
      APPEND l_mem_cobra.
    ENDIF.

    READ TABLE l_mem_cobrb WITH KEY objnr = ls_cobrb-objnr
                                    bureg = ls_cobrb-bureg
                                    lfdnr = ls_cobrb-lfdnr.

    IF sy-subrc <> 0.
      MOVE-CORRESPONDING ls_cobrb TO l_mem_cobrb.
      l_mem_cobrb-uflag = 'I'.
      APPEND l_mem_cobrb.
    ELSE.
      MOVE-CORRESPONDING ls_cobrb TO l_mem_cobrb.
      l_mem_cobrb-uflag = 'U'.
      MODIFY l_mem_cobrb INDEX sy-tabix.
    ENDIF.

    CALL FUNCTION 'K_SETTLEMENT_RULE_REFRESH'
      EXPORTING
        objnr = lt_objnr-objnr.

    EXPORT l_mem_cobra l_mem_cobrb TO MEMORY ID 'K_SRULE'.

    CALL FUNCTION 'K_SRULE_EXPORT_IMPORT'
      EXPORTING
        i_mode = 'IM'.
*       Сохраняем правила расчета
    CALL FUNCTION 'K_SETTLEMENT_RULE_SAVE'
      EXPORTING
        dialog            = 'X'
        objnr             = lt_objnr-objnr
        i_status_update   = ' '
      EXCEPTIONS
        no_rule_for_objnr = 1
        OTHERS            = 2.


*    DELETE lt_anek WHERE augln <> '00000'.
    SUBMIT zsapma15b_cop WITH p_bukrs = comp
                         WITH p_asset = ls_asset-asset_num AND RETURN.
    IMPORT postab TO postab FROM DATABASE indx(pt) ID 'PSTAB'.
    DELETE FROM DATABASE indx(pt) ID 'PSTAB'.


    LOOP AT postab.
      CLEAR: ls_anek_ext.
      MOVE-CORRESPONDING postab TO ls_anek_ext.
*      ls_anek_ext       = postab.
      ls_anek_ext-bureg = lw_bureg + 1.
      ls_anek_ext-subta = postab-lnran.
      APPEND ls_anek_ext TO lt_anek_ext.
    ENDLOOP.

    SELECT rldnr rbukrs gjahr belnr buzei docln anln1 anln2 subta
           FROM acdoca INTO CORRESPONDING FIELDS OF TABLE lt_acdoca_upd
           FOR ALL ENTRIES IN lt_anek_ext
           WHERE rbukrs = lt_anek_ext-bukrs
           AND   gjahr  = lt_anek_ext-gjahr
           AND   anln1  = lt_anek_ext-anln1
           AND   anln2  = lt_anek_ext-anln2
           AND   subta  = lt_anek_ext-subta.            "#EC CI_NOFIRST

    ASSERT lt_acdoca_upd IS NOT INITIAL.

    LOOP AT lt_anek_ext ASSIGNING FIELD-SYMBOL(<ls_anek_ext>).
      LOOP AT lt_acdoca_upd ASSIGNING FIELD-SYMBOL(<ls_acdoca>)
                            WHERE rbukrs = <ls_anek_ext>-bukrs
                            AND   gjahr  = <ls_anek_ext>-gjahr
                            AND   anln1  = <ls_anek_ext>-anln1
                            AND   anln2  = <ls_anek_ext>-anln2
                            AND   subta  = <ls_anek_ext>-subta.
        <ls_acdoca>-settlement_rule = <ls_anek_ext>-bureg.
      ENDLOOP.
    ENDLOOP.

    APPEND 'SETTLEMENT_RULE' TO lt_fieldname_upd.

    lo_fins_acdoc_change->change_acdoca( EXPORTING
                                           it_change_fields = lt_fieldname_upd
                                           it_acdoca_upd    = lt_acdoca_upd   ).
    COMMIT WORK AND WAIT.

    REFRESH :lt_objnr,lt_fieldname_upd,lt_acdoca_upd,lt_anek_ext,lt_cobra,lt_cobrb.
    CLEAR :ls_anla,l_lfdnr,lw_bureg,l_ionr,l_mem_cobra,l_mem_cobrb.

  ENDLOOP.




ENDFUNCTION.
