*&---------------------------------------------------------------------*
*& Include          ZIN_FIE003_02_F01
*&---------------------------------------------------------------------*
"class for paralel process
CLASS local DEFINITION.
  PUBLIC SECTION.
    DATA: handler TYPE REF TO zcl_thread_handler.

    METHODS:
      constructor,
      do_stuff_in_parallel
        IMPORTING
          ls_header   TYPE bapiache09
          ls_onetime  TYPE bapiacpa09
          ls_test     TYPE char1
          lt_glacc    LIKE gt_glacc
          lt_customer LIKE gt_customer
          lt_vendor   LIKE gt_vendor
          lt_tax      LIKE gt_tax
          lt_cr       LIKE gt_cr
          lt_criteria LIKE gt_criteria
          lt_extens   LIKE gt_extens
          lt_return   LIKE gt_return
          lt_data     LIKE gt_data_tmp,
      on_end_of_action
        IMPORTING p_task TYPE clike
        .
ENDCLASS.

CLASS local IMPLEMENTATION.
  METHOD constructor.
    DATA(prefix) = |{ sy-uname }{ sy-datum }{ sy-uzeit }|.
    DATA(lw_lines) = lines( gt_data ).
    CREATE OBJECT handler
      EXPORTING
        i_task_prefix = CONV char26( prefix )
        i_threads     = lw_lines
        i_group       = 'RFCGROUP'.
  ENDMETHOD.
  METHOD do_stuff_in_parallel.
    DATA: lw_count TYPE int4.

    lw_count = 0.
    DO.
      lw_count = lw_count + 1.
      DO.
        DATA(thread) = me->handler->get_free_thread( ).
        IF thread <> ''.
          EXIT.
        ENDIF.
      ENDDO.
      DATA errmsg TYPE char255.
      CALL FUNCTION 'ZFM_FIE00302_PARALLEL' STARTING NEW TASK thread
        DESTINATION IN GROUP zcl_thread_handler=>group
*        destination in group zcl_thread_handler=>c_default_group
        CALLING on_end_of_action ON END OF TASK
        EXPORTING
          documentheader        = ls_header
          customercpd           = ls_onetime
          i_test                = ls_test
        TABLES
          accountgl             = lt_glacc
          accountreceivable     = lt_customer
          accountpayable        = lt_vendor
          accounttax            = lt_tax
          currencyamount        = lt_cr
          criteria              = lt_criteria
          extension2            = lt_extens
          return                = lt_return
          it_data               = lt_data
        EXCEPTIONS
          communication_failure = 1 MESSAGE errmsg
          system_failure        = 2 MESSAGE errmsg
          resource_failure      = 3.
      IF sy-subrc IS INITIAL.
        EXIT.
      ELSEIF lw_count > 100.
        LOOP AT lt_data INTO DATA(ls_data).
          ls_data-status  = icon_red_light.
          ls_data-message = errmsg.
          APPEND ls_data TO gt_data_tmp.
        ENDLOOP.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD on_end_of_action.
    DATA: lt_return   LIKE gt_return,
          lw_guid     TYPE guid_32,
          lt_data_tmp LIKE gt_data_tmp.
    DATA errmsg TYPE c LENGTH 255.
*    data lt_ztb_fie003_02_lo type table of ztb_fie003_02_lo.

    RECEIVE RESULTS FROM FUNCTION 'ZFM_FIE00302_PARALLEL'
      IMPORTING
        e_guid                = lw_guid
      TABLES
        return                = lt_return
        it_data               = lt_data_tmp
      EXCEPTIONS
        communication_failure = 1 MESSAGE errmsg
        system_failure        = 2 MESSAGE errmsg.
    IF sy-subrc IS NOT INITIAL.
*      ...handle error
    ENDIF.

    " Free the thread for the next thread to run
    me->handler->clear_thread( CONV char50( p_task ) ).
*    ...handle receive logic
    IF lt_data_tmp IS NOT INITIAL.
      APPEND LINES OF lt_data_tmp TO gt_data_tmp.
*      if p_run = 'X'.
*        loop at lt_data_tmp into data(ls_data_tmp).
*          append initial line to lt_ztb_fie003_02_lo assigning field-symbol(<ls_log>).
*          move-corresponding ls_data_tmp to <ls_log>.
*          <ls_log>-guid = gs_selected_file-guid.
*        endloop.
**        modify ztb_fie003_02_lo from table lt_ztb_fie003_02_lo.
*      endif.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
**********************************************************************
* FOR splitting CSV file format

*---------------------------------------------------------------------*
*"CSV
FORM line_to_cell_esc_sep USING i_string
                                i_sic_int      TYPE i
                                i_separator    TYPE c
                                i_intern_value TYPE kcde_intern_value.
  DATA: l_int         TYPE i,
        l_cell_end(2).
  FIELD-SYMBOLS: <l_cell>.
  l_cell_end = c_esc.
  l_cell_end+1 = i_separator .

  IF i_string CS c_esc.
    i_string = i_string+1.
    IF i_string CS l_cell_end.
      l_int = sy-fdpos.
      IF l_int = 0.
        CLEAR i_intern_value.
      ELSE.
        ASSIGN i_string(l_int) TO <l_cell>.
        i_intern_value = <l_cell>.
      ENDIF.
      l_int = l_int + 2.
      i_sic_int = l_int.
      i_string = i_string+l_int.
    ELSEIF i_string CS c_esc.
*     letzte Celle
      l_int = sy-fdpos.
      IF l_int = 0.
        CLEAR i_intern_value.
      ELSE.
        ASSIGN i_string(l_int) TO <l_cell>.
        i_intern_value = <l_cell>.
      ENDIF.
      l_int = l_int + 1.
      i_sic_int = l_int.
      i_string = i_string+l_int.
      l_int = strlen( i_string ).
      IF l_int > 0 . MESSAGE 'Error: invalid CSV Format' TYPE  'I' DISPLAY LIKE 'E'. LEAVE LIST-PROCESSING. ENDIF.
    ELSE.
      MESSAGE 'Error: invalid CSV Format' TYPE 'I' DISPLAY LIKE 'E' . LEAVE LIST-PROCESSING."was ist mit csv-Format
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
FORM line_to_cell_separat TABLES i_intern    TYPE kcde_intern
                          USING  i_line
                                 i_row       LIKE sy-tabix
                                 ch_cell_col LIKE kcdehead-col
                                 i_separator TYPE c
                                 i_fdpos     LIKE sy-fdpos.
  DATA: l_string  TYPE kcde_sender_struc  .
  DATA  l_sic_int       TYPE i.

  CLEAR i_intern.
  l_sic_int     = i_fdpos.
  i_intern-row  = i_row.
  l_string      = i_line.
  i_intern-col  = ch_cell_col.
* csv Dateien mit separator in Zelle: --> ;"abc;cd";
  IF ( i_separator = ';' OR  i_separator = ',' ) AND
       l_string(1) = c_esc.
    PERFORM line_to_cell_esc_sep USING l_string
                                       l_sic_int
                                       i_separator
                                       i_intern-value.
  ELSE.
    IF l_sic_int > 0.
      i_intern-value = i_line(l_sic_int).
    ENDIF.
  ENDIF.
  IF l_sic_int > 0. "REMOVE EMPTY COLUMN
    APPEND i_intern.
  ENDIF.
  l_sic_int = l_sic_int + 1.
  i_line = i_line+l_sic_int.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEPERATED_TO_INTERN_CONVERT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM separated_to_intern_convert TABLES i_tab       TYPE kcdu_srecs
                                        i_intern    TYPE kcde_intern
                                 USING  i_separator TYPE c.

  DATA: l_sic_tabix LIKE sy-tabix,
        l_sic_col   LIKE kcdehead-col.
  DATA: l_fdpos     LIKE sy-fdpos.

  REFRESH i_intern.

  LOOP AT i_tab.
    l_sic_tabix = sy-tabix.
    l_sic_col = 0.
    WHILE i_tab CA i_separator.
      l_fdpos = sy-fdpos.
      l_sic_col = l_sic_col + 1.
      PERFORM line_to_cell_separat TABLES i_intern
                                   USING  i_tab l_sic_tabix l_sic_col
                                          i_separator l_fdpos.
    ENDWHILE.
    IF i_tab <> space.
      CLEAR i_intern.
      i_intern-row = l_sic_tabix.
      i_intern-col = l_sic_col + 1.
      i_intern-value = i_tab.
      APPEND i_intern.
    ENDIF.
  ENDLOOP.
ENDFORM.
**********************************************************************
* FOR READING FILE and UPLOAD FILE to AL11
*&---------------------------------------------------------------------*
*&      Form  CHECK_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_path .
  DATA: lt_file   TYPE filetable, "Table Holding Selected Files
        lw_rc     TYPE i,
        lw_result TYPE c
        .
  IF p_downlo = 'X'.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = CONV string( p_fpath )
      RECEIVING
        result               = lw_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0 OR lw_result IS INITIAL.
      MESSAGE 'The Folder does not exist' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

  ELSE.
    "Check opening file
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = CONV string( p_fpath )
      RECEIVING
        result               = lw_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF sy-subrc <> 0 OR lw_result IS INITIAL.
      MESSAGE 'The file does not exist' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_path .
  IF p_upload = '' AND p_downlo = ''.
    RETURN.
  ENDIF.
  DATA: lt_file   TYPE filetable, "Table Holding Selected Files
        lw_rc     TYPE i,
        lw_result TYPE c
        .
  "open file
  CASE 'X'.
    WHEN p_upload.
      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          file_filter             = 'Text (*.csv)|*.csv|'
        CHANGING
          file_table              = lt_file
          rc                      = lw_rc
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.
      IF lt_file[] IS INITIAL.
        MESSAGE 'Input file first' TYPE 'S' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
      p_fpath = CONV string( lt_file[ 1 ]-filename ).
    WHEN p_downlo.
      DATA(lw_folder_path) = VALUE string( ).
      cl_gui_frontend_services=>directory_browse(
        EXPORTING
          window_title    = 'Select Target Folder On Front End'
          initial_folder  = lw_folder_path
        CHANGING
          selected_folder = lw_folder_path ).

      p_fpath = lw_folder_path.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_SERVER_FILE_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_server_file_help .
  IF p_test = '' AND p_run = '' AND p_remove = ''.
    RETURN.
  ENDIF.
  DATA:
    lt_ztb_fie003_02_f LIKE gt_ztb_fie003_02_f,
    lt_return          TYPE TABLE OF ddshretval.

  SELECT * FROM ztb_fie003_02_f
    INTO TABLE lt_ztb_fie003_02_f
    WHERE executed = ''.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'GUID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_FRUNN'
*     STEPL           = 0
      window_title    = 'Select Files'
*     VALUE           = ' '
      value_org       = 'S'
      multiple_choice = ''
    TABLES
      value_tab       = lt_ztb_fie003_02_f
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  IF sy-subrc = 0.
    p_frunn = CONV string( lt_return[ 1 ]-fieldval ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_AL11_FOLDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_al11_folder.
  SELECT SINGLE *
    FROM user_dir
    INTO @DATA(ls_user_dir)
    WHERE aliass = @gc_folder.
  IF sy-subrc <> 0.
    MESSAGE 'Cannot find DIR_FI00302 folder in server' TYPE 'I' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  gw_server_dir = ls_user_dir-dirname.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file.
  PERFORM check_path. "check local existing file
  DATA: lw_flg_open_error  TYPE boolean,
        lw_os_message(200) TYPE c
        .

  DATA ls_ztb_fie003_02_f LIKE LINE OF gt_ztb_fie003_02_f.
  CALL FUNCTION 'GUID_CREATE' IMPORTING ev_guid_32 = ls_ztb_fie003_02_f-guid.
  DATA(lw_filename)               = |{ sy-uname }{ sy-datum }{ sy-uzeit }_{ ls_ztb_fie003_02_f-guid }.csv|.
  DATA(lw_file)                   = |{ gw_server_dir }/{ lw_filename }|. "Server file
  DATA(lw_cprog)                  = sy-cprog.   "init Program
  ls_ztb_fie003_02_f-cr_uname     = sy-uname.
  ls_ztb_fie003_02_f-cr_uzeit     = sy-uzeit.
  ls_ztb_fie003_02_f-cr_datum     = sy-datum.
  ls_ztb_fie003_02_f-server_dir   = gw_server_dir.
  ls_ztb_fie003_02_f-server_file  = lw_filename.
  ls_ztb_fie003_02_f-local_file   = p_fpath.
  sy-cprog                        = 'RC1TCG3Y'. "Upload Program
  CALL FUNCTION 'C13Z_FILE_UPLOAD_BINARY' "Upload file to server
    EXPORTING
      i_file_front_end    = p_fpath
      i_file_appl         = CONV rcgfiletr-ftappl( lw_file )
      i_file_overwrite    = 'X'
    IMPORTING
      e_flg_open_error    = lw_flg_open_error
      e_os_message        = lw_os_message
    EXCEPTIONS
      fe_file_open_error  = 1
      fe_file_exists      = 2
      fe_file_write_error = 3
      ap_no_authority     = 4
      ap_file_open_error  = 5
      ap_file_empty       = 6
      OTHERS              = 7.
  IF lw_flg_open_error = abap_true.
    MESSAGE lw_os_message TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    INSERT ztb_fie003_02_f FROM ls_ztb_fie003_02_f.
    COMMIT WORK AND WAIT.
    IF sy-subrc = 0.
      MESSAGE |Success! Your server file: "{ lw_file }" in table ZTB_FIE003_02_F| TYPE 'I' DISPLAY LIKE 'S'.
    ENDIF.
  ENDIF.
  sy-cprog = lw_cprog. "Running Program
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_file.
  PERFORM check_path. "check local existing file
  DATA: lw_flg_open_error  TYPE boolean,
        lw_os_message(200) TYPE c
        .
  DATA(lw_file)                   = |{ gw_server_dir }/{ gc_server_temp }|. "Server file
  DATA(lw_localfile)              = |{ p_fpath }\\{ gc_server_temp }|. "Server file
  DATA(lw_cprog)                  = sy-cprog.   "init Program
  sy-cprog                        = 'RC1TCG3Y'. "Upload Program
  CALL FUNCTION 'C13Z_FILE_DOWNLOAD_BINARY' "Upload file to server
    EXPORTING
      i_file_front_end    = lw_localfile
      i_file_appl         = CONV rcgfiletr-ftappl( lw_file )
      i_file_overwrite    = 'X'
    IMPORTING
      e_flg_open_error    = lw_flg_open_error
      e_os_message        = lw_os_message
    EXCEPTIONS
      fe_file_open_error  = 1
      fe_file_exists      = 2
      fe_file_write_error = 3
      ap_no_authority     = 4
      ap_file_open_error  = 5
      ap_file_empty       = 6
      OTHERS              = 7.
  IF lw_flg_open_error = abap_false .
    WRITE /1 |Download file { lw_localfile } successfully. \nNotes: Save your file as "CSV file with UTF-8 encoding (*.csv)" for UPLOADING|.
  ELSE.
    WRITE /1 |{ lw_os_message }|.
  ENDIF.
  sy-cprog = lw_cprog. "Running Program
ENDFORM.
**********************************************************************


*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.
  LOOP AT SCREEN.
    IF p_upload <> 'X' AND p_downlo <> 'X'.
      IF screen-name CS 'P_FPATH'.
        screen-input = 0.
        CLEAR p_fpath.
      ENDIF.
    ELSE.
      IF screen-name CS 'P_FRUNN'.
        screen-input = 0.
        CLEAR p_frunn.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: csv_format  TYPE kcdu_srecs,
        e_intern    TYPE kcde_intern,
        i_separator TYPE c VALUE ',',
        lw_line     TYPE string,
        lw_index    TYPE i,
        lw_temp_str TYPE string
        .

  SELECT SINGLE *
    FROM ztb_fie003_02_f
    INTO gs_selected_file
    WHERE guid      = p_frunn
      AND executed  = ''.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE `Selected File does not exsit in server` TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  DATA(lw_file)   = |{ gs_selected_file-server_dir }/{ gs_selected_file-server_file }|. "Server file
  IF p_run = 'X'.
    gs_selected_file-executed = 'X'.
    MODIFY ztb_fie003_02_f FROM gs_selected_file.
    COMMIT WORK AND WAIT.
  ENDIF.
  OPEN DATASET lw_file FOR INPUT IN TEXT MODE ENCODING UTF-8.
  IF sy-subrc = 0.
    DO.
      READ DATASET lw_file INTO lw_line.
      IF sy-subrc NE 0.
        EXIT.
      ELSE.
        IF sy-index = 1. "REMOVE HEADER
          CONTINUE.
        ENDIF.
*        refresh: csv_format, e_intern.
*        append initial line to csv_format assigning field-symbol(<ls_csv_format>).
*        <ls_csv_format>-srec = lw_line.
*        perform separated_to_intern_convert
*          tables  csv_format
*                  e_intern
*          using  i_separator.
*        if e_intern is initial.
*          continue.
*        endif.

        APPEND INITIAL LINE TO gt_input ASSIGNING FIELD-SYMBOL(<ls_input>).
        SPLIT lw_line AT cl_abap_char_utilities=>horizontal_tab INTO
          <ls_input>-col001
          <ls_input>-col002
          <ls_input>-col003
          <ls_input>-col004
          <ls_input>-col005
          <ls_input>-col006
          <ls_input>-col007
          <ls_input>-col008
          <ls_input>-col009
          <ls_input>-col010
          <ls_input>-col011
          <ls_input>-col012
          <ls_input>-col013
          <ls_input>-col014
          <ls_input>-col015
          <ls_input>-col016
          <ls_input>-col017
          <ls_input>-col018
          <ls_input>-col019
          <ls_input>-col020
          <ls_input>-col021
          <ls_input>-col022
          <ls_input>-col023
          <ls_input>-col024
          <ls_input>-col025
          <ls_input>-col026
          <ls_input>-col027
          <ls_input>-col028
          <ls_input>-col029
          <ls_input>-col030
          <ls_input>-col031
          <ls_input>-col032
          <ls_input>-col033
          <ls_input>-col034
          <ls_input>-col035
          <ls_input>-col036
          <ls_input>-col037
          <ls_input>-col038
          <ls_input>-col039
          <ls_input>-col040
          <ls_input>-col041
          <ls_input>-col042
          <ls_input>-col043
          <ls_input>-col044
          <ls_input>-col045
          <ls_input>-col046
          <ls_input>-col047
          <ls_input>-col048
          <ls_input>-col049
          <ls_input>-col050
          <ls_input>-col051
          <ls_input>-col052
          <ls_input>-col053
          <ls_input>-col054
          <ls_input>-col055
          <ls_input>-col056
          <ls_input>-col057
          <ls_input>-col058
          <ls_input>-col059
          <ls_input>-col060
          <ls_input>-col061
          lw_temp_str
          .

*        loop at e_intern into data(ls_e_intern).
*          assign component |col{ ls_e_intern-col }| of structure <ls_input> to field-symbol(<ls_value>).
*          if sy-subrc = 0.
*            <ls_value> = ls_e_intern-value.
*          endif.
*        endloop.
      ENDIF.
    ENDDO.
    CLOSE DATASET lw_file.
  ELSEIF sy-subrc EQ 8.
    CLOSE DATASET lw_file.
    MESSAGE 'File not found.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF sy-subrc NE 0.
    CLOSE DATASET lw_file.
    MESSAGE 'Unable to open file.' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  perform check_path.
*
*  field-symbols <lt_data> type any table.
*  assign gt_input[] to <lt_data>.
*
*  call method cl_gui_frontend_services=>gui_upload
*    exporting
*      filename                = p_fpath
*      filetype                = 'ASC'
*      has_field_separator     = cl_abap_char_utilities=>horizontal_tab "'#'
*      read_by_line            = 'X'
**     dat_mode                = 'X'
*      replacement             = ' '
*    changing
*      data_tab                = <lt_data>
*    exceptions
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      not_supported_by_gui    = 17
*      error_no_gui            = 18
*      others                  = 19.
*
*  if sy-subrc ne 0.
*    message id sy-msgid type 'I' number sy-msgno display like 'E'
*      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 .
*    leave list-processing.
*  endif.
*  unassign: <lt_data>.
*  delete gt_input index 1.
  IF gt_input[] IS INITIAL.
    MESSAGE 'The file is empty' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  LOOP AT gt_input ASSIGNING <ls_input>.
    lw_index = sy-tabix + 1.
    APPEND INITIAL LINE TO gt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    TRY .
        <ls_data>-lineindex     = |{ lw_index }|. "+1 FOR ONE LINE HEADER
        <ls_data>-stt           = |{ <ls_input>-col001 ALPHA = IN }|. "STT
        <ls_data>-doc_date      = <ls_input>-col002+6(4) && <ls_input>-col002+3(2) && <ls_input>-col002+0(2). "Ngày chứng từ
        <ls_data>-pstng_date    = <ls_input>-col003+6(4) && <ls_input>-col003+3(2) && <ls_input>-col003+0(2). "Ngày hạch toán
        <ls_data>-header_txt    = |{ <ls_input>-col004 ALPHA = IN }|. "Tiêu đề chứng từ
        <ls_data>-mau_hd        = |{ <ls_input>-col005 ALPHA = IN }|. "Mẫu hóa đơn
        <ls_data>-ref_doc_no    = |{ <ls_input>-col006 ALPHA = IN }|. "Số hoá đơn
        <ls_data>-doc_type      = |{ <ls_input>-col007 ALPHA = IN }|. "Loại chứng từ
        <ls_data>-comp_code     = |{ <ls_input>-col008 ALPHA = IN }|. "Mã công ty
        <ls_data>-currency      = |{ <ls_input>-col009 ALPHA = IN }|. "Đ.vị tiền tệ
        <ls_data>-trans_date    = <ls_input>-col010+6(4) && <ls_input>-col010+3(2) && <ls_input>-col010+0(2). "Ngày lấy tỉ giá trong hệ thống
        <ls_data>-xref1_hd      = <ls_input>-col011. "Reference Key 1 HD
        <ls_data>-xref2_hd      = <ls_input>-col012. "Reference Key 2 HD
        <ls_data>-de_cre_ind1   = 'H'.
        IF <ls_input>-col013    = 'N'. <ls_data>-de_cre_ind1 = 'S'. ENDIF. "Nợ/Có
        <ls_data>-gl_account    = |{ <ls_input>-col014 ALPHA = IN }|. "Tài khoản
        <ls_data>-bp            = |{ <ls_input>-col015 ALPHA = IN }|. "mã BP
        <ls_data>-sp_gl_ind     = |{ <ls_input>-col016 ALPHA = IN }|. "Special GL indicator
        <ls_data>-amt_doccur    = |{ <ls_input>-col017 ALPHA = IN }|. "Số tiền
        <ls_data>-tax_code      = |{ <ls_input>-col018 ALPHA = IN }|. "Thuế suất
        <ls_data>-tax_amt       = |{ <ls_input>-col019 ALPHA = IN }|. "Số tiền thuế
        <ls_data>-businessplace = |{ <ls_input>-col020 ALPHA = IN }|. "Chi cục thuế
        <ls_data>-segment       = |{ <ls_input>-col021 ALPHA = IN }|. "Chi nhánh
        <ls_data>-costcenter    = |{ <ls_input>-col022 ALPHA = IN }|. "TT Chi phí
        <ls_data>-profit_ctr    = |{ <ls_input>-col023 ALPHA = IN }|. "TT lợi nhuận
        <ls_data>-item_text     = |{ <ls_input>-col024 ALPHA = IN }|. "Diễn giải
        <ls_data>-alloc_nmbr    = |{ <ls_input>-col025 ALPHA = IN }|. "Nội dung gán
        <ls_data>-pmnttrms      = |{ <ls_input>-col026 ALPHA = IN }|. "Điều khoản thanh toán
        <ls_data>-bline_date    = <ls_input>-col027+6(4) && <ls_input>-col027+3(2) && <ls_input>-col027+0(2). "Ngày tính nợ
        <ls_data>-pmnt_block    = |{ <ls_input>-col028 ALPHA = IN }|. "Khóa thanh toán
        <ls_data>-pymt_meth     = |{ <ls_input>-col029 ALPHA = IN }|. "Phương thức thanh toán
        <ls_data>-partner_bk    = |{ <ls_input>-col030 ALPHA = IN }|. "Mã tài khoản ngân hàng giao dịch
        <ls_data>-neg_postng    = |{ <ls_input>-col031 ALPHA = IN }|. "Đánh dấu ghi âm
        <ls_data>-ref_key_1     = <ls_input>-col032. "Mã dòng tiền
        <ls_data>-ref_key_2     = <ls_input>-col033. "Reference Key 2 Line
        <ls_data>-ref_key_3     = <ls_input>-col034. "Reference Key 3 Line
        <ls_data>-name_1        = <ls_input>-col035. "Tên
        <ls_data>-street        = |{ <ls_input>-col036 ALPHA = IN }|. "Địa chỉ
        <ls_data>-city          = |{ <ls_input>-col037 ALPHA = IN }|. "Tỉnh/ thành phố
        <ls_data>-tax_no_1      = |{ <ls_input>-col038 ALPHA = IN }|. "Mã số thuế
        <ls_data>-kokrs         = |{ <ls_input>-col039 ALPHA = IN }|. "KOKRS (Controlling Area)
        <ls_data>-kndnr         = |{ <ls_input>-col040 ALPHA = IN }|. "KNDNR ( Customer)
        <ls_data>-artnr         = |{ <ls_input>-col041 ALPHA = IN }|. "Product (ARTNR)
        <ls_data>-fkart         = |{ <ls_input>-col042 ALPHA = IN }|. "Billing type (FKART)
        <ls_data>-kdpos         = |{ <ls_input>-col043 ALPHA = IN }|. "Sales ord. item (KDPOS)
        <ls_data>-kaufn         = |{ <ls_input>-col044 ALPHA = IN }|. "Order (KAUFN)
        <ls_data>-werks         = |{ <ls_input>-col045 ALPHA = IN }|. "Plant (WERKS)
        <ls_data>-fkber         = |{ <ls_input>-col046 ALPHA = IN }|. "Functional Area (FKBER)
        <ls_data>-vkorg         = |{ <ls_input>-col048 ALPHA = IN }|. "Sales Org (VKORG ).
        <ls_data>-vtweg         = |{ <ls_input>-col049 ALPHA = IN }|. "Distr. Channel (VTWEG)
        <ls_data>-spart         = |{ <ls_input>-col050 ALPHA = IN }|. "Division (SPART)
        <ls_data>-pspnr         = |{ <ls_input>-col051 ALPHA = IN }|. "WBS Element (PSPNR)
        <ls_data>-kstrg         = |{ <ls_input>-col052 ALPHA = IN }|. "Cost Object(KSTRG)
        <ls_data>-pprctr        = |{ <ls_input>-col053 ALPHA = IN }|. "Partner PC (PPRCTR)
        <ls_data>-kmvkrg        = |{ <ls_input>-col054 ALPHA = IN }|. "Sales group (KMVKGR)
        <ls_data>-kmvtnr        = |{ <ls_input>-col055 ALPHA = IN }|. "Sales employee (KMVTNR)
        <ls_data>-kunre         = |{ <ls_input>-col056 ALPHA = IN }|. "Bill-to party (KUNRE)
        <ls_data>-kunwe         = |{ <ls_input>-col057 ALPHA = IN }|. "Ship-to party (KUNWE)
        <ls_data>-wwpgr         = |{ <ls_input>-col058 ALPHA = IN }|. "Service group (WWPGR)
        <ls_data>-kmvkbu        = |{ <ls_input>-col059 ALPHA = IN }|. "sale office (KMVKBU)
        <ls_data>-wwser         = |{ <ls_input>-col060 ALPHA = IN }|. "Service type (WWSER )
        <ls_data>-wwlv          = |{ <ls_input>-col061 ALPHA = IN }|. "Lĩnh vực kinh doanh (WWLV)

*     <ls_data>-col047 to . "SEGMENT
      CATCH cx_sy_conversion_no_number.
        BREAK minhdv.
        MESSAGE |Line { lw_index } has an invalid cells. Check your excel template again| TYPE 'I'.
        STOP.
    ENDTRY.
  ENDLOOP.
  UNASSIGN: <ls_input>, <ls_data>.

  DATA: lt_data_tmp LIKE gt_data OCCURS 0.
  lt_data_tmp[] = gt_data[].
  SORT lt_data_tmp BY bp.
  DELETE ADJACENT DUPLICATES FROM lt_data_tmp COMPARING bp.

  "Company
  SELECT * FROM t001
    INTO TABLE gt_t001
    ORDER BY bukrs.

  IF lt_data_tmp[] IS NOT INITIAL.
    "Partner
    SELECT * FROM but000
      INTO TABLE gt_but000
      FOR ALL ENTRIES IN lt_data_tmp
      WHERE partner = lt_data_tmp-bp.
    SORT gt_but000 BY partner.
  ENDIF.

  "Tax code
  SELECT * FROM a003
    INTO TABLE gt_a003
    WHERE kappl = 'TX'
      AND aland = 'VN'
    ORDER BY mwskz kschl.
  "Business Place
  SELECT * FROM j_1bbranch
    INTO TABLE gt_j_1bbranch
    ORDER BY bukrs branch.
  "Profit center
  SELECT * FROM cepc
    INTO TABLE gt_cepc
    WHERE kokrs = '1000'
    ORDER BY prctr datbi.
  "Cost center
  SELECT * FROM csks
    INTO TABLE gt_csks
    WHERE kokrs = '1000'
    ORDER BY kostl datbi.
  "G/L
  SELECT * FROM skb1
    INTO TABLE gt_skb1
    ORDER BY bukrs saknr.
  "Master Data for Segments
  SELECT * FROM zddl_segment_zfir001
    INTO CORRESPONDING FIELDS OF TABLE @gt_fagl_segm
    ORDER BY segment.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .
  DATA: ls_header     TYPE bapiache09,
        ls_header_tmp LIKE LINE OF gt_data,
        ls_onetime    LIKE bapiacpa09,
        lw_itemno     TYPE i,
        lw_error      TYPE mark,
        lt_glacc      TYPE TABLE OF bapiacgl09,
        lt_customer   TYPE TABLE OF bapiacar09,
        lt_vendor     TYPE TABLE OF bapiacap09,
        lt_tax        TYPE TABLE OF bapiactx09,
        lt_cr         TYPE TABLE OF bapiaccr09,
        lt_criteria   TYPE TABLE OF bapiackec9,
        lt_extens     TYPE TABLE OF bapiparex,
        lt_return     TYPE TABLE OF bapiret2,
        lo_field      TYPE REF TO cl_abap_structdescr
        .

  DATA: local  TYPE REF TO local.
  DATA: lt_data_tmp LIKE gt_data_tmp.
  DATA: lw_count TYPE i.
  CREATE OBJECT local.

  lo_field ?= cl_abap_typedescr=>describe_by_data( ls_addition ).
  SORT gt_data BY stt.
  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    AT NEW stt.
      CLEAR:
        lw_itemno,
        ls_header,
        ls_header_tmp,
        ls_onetime,
        lt_glacc,
        lt_customer,
        lt_vendor,
        lt_tax,
        lt_cr,
        lt_criteria,
        lt_extens,
        lt_return,
        lt_data_tmp,
        lw_error.
    ENDAT.

    IF lw_error IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE gt_t001 TRANSPORTING NO FIELDS WITH KEY bukrs = <ls_data>-comp_code BINARY SEARCH.
    IF sy-subrc <> 0.
      <ls_data>-message = |Company code { <ls_data>-comp_code ALPHA = OUT } doesn't exist|.
      <ls_data>-status  = icon_red_light.
      APPEND INITIAL LINE TO gt_data_tmp ASSIGNING FIELD-SYMBOL(<ls_data_e>).
      MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
      lw_error = 'X'.
      CONTINUE.
    ENDIF.
    IF (     <ls_data>-stt          IS INITIAL
          OR <ls_data>-doc_date     IS INITIAL
          OR <ls_data>-pstng_date   IS INITIAL
          OR <ls_data>-doc_type     IS INITIAL
          OR <ls_data>-comp_code    IS INITIAL
          OR <ls_data>-currency     IS INITIAL
          OR <ls_data>-de_cre_ind1  IS INITIAL
          OR <ls_data>-amt_doccur   IS INITIAL
          OR <ls_data>-gl_account   IS INITIAL
          OR <ls_data>-profit_ctr   IS INITIAL )
      AND NOT ( <ls_data>-gl_account IS NOT INITIAL OR <ls_data>-bp IS NOT INITIAL ).
      <ls_data>-message = |Please fill all the required fields in line|.
      <ls_data>-status  = icon_red_light.
      APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
      MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
      lw_error = 'X'.
      CONTINUE.
    ENDIF.

    IF <ls_data>-bp IS NOT INITIAL.
      READ TABLE gt_but000 TRANSPORTING NO FIELDS WITH KEY partner = <ls_data>-bp BINARY SEARCH.
      IF sy-subrc <> 0.
        <ls_data>-message = |BP { <ls_data>-bp ALPHA = OUT } doesn't exist |.
        <ls_data>-status  = icon_red_light.
        APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
        MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
        lw_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF <ls_data>-tax_code IS NOT INITIAL.
      READ TABLE gt_a003 TRANSPORTING NO FIELDS WITH KEY mwskz = <ls_data>-tax_code BINARY SEARCH.
      IF sy-subrc <> 0.
        <ls_data>-message = |Tax code { <ls_data>-tax_code ALPHA = OUT } doesn't exist |.
        <ls_data>-status  = icon_red_light.
        EXIT.
      ENDIF.
    ENDIF.

    IF <ls_data>-businessplace IS NOT INITIAL.
      READ TABLE gt_j_1bbranch TRANSPORTING NO FIELDS WITH KEY
        bukrs   = <ls_data>-comp_code
        branch  = <ls_data>-businessplace
        BINARY SEARCH.
      IF sy-subrc <> 0.
        <ls_data>-message = |Business Place { <ls_data>-businessplace ALPHA = OUT } doesn't exist |.
        <ls_data>-status  = icon_red_light.
        APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
        MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
        lw_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

    " Check phan quyen segment
    READ TABLE gt_skb1 INTO DATA(ls_skb1) WITH KEY
      bukrs = <ls_data>-comp_code
      saknr = <ls_data>-gl_account BINARY SEARCH.
    IF sy-subrc = 0.
      IF ls_skb1-fstag = 'G004'.
        READ TABLE gt_csks INTO DATA(ls_csks) WITH KEY kostl = <ls_data>-costcenter.
        IF sy-subrc <> 0.
          <ls_data>-message = |Cost Center { <ls_data>-costcenter } doesn't exist |.
          <ls_data>-status  = icon_red_light.
          APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
          MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
          lw_error = 'X'.
          CONTINUE.
        ENDIF.
        READ TABLE gt_cepc INTO DATA(ls_cepc_1) WITH KEY prctr = ls_csks-prctr BINARY SEARCH.
        IF sy-subrc <> 0.
          <ls_data>-message = |Profit Center { ls_csks-prctr } doesn't exist |.
          <ls_data>-status  = icon_red_light.
          APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
          MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
          lw_error = 'X'.
          CONTINUE.
        ENDIF.
        READ TABLE gt_fagl_segm TRANSPORTING NO FIELDS WITH KEY segment = ls_cepc_1-segment BINARY SEARCH.
        IF sy-subrc <> 0.
          <ls_data>-message = |You are not authorized to post in Cost center { <ls_data>-costcenter }|.
          <ls_data>-status  = icon_red_light.
          APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
          MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
          lw_error = 'X'.
          CONTINUE.
        ENDIF.
      ENDIF.

      CASE ls_skb1-mitkz.
        WHEN 'D'. <ls_data>-customer = <ls_data>-bp.
        WHEN 'K'. <ls_data>-vendor_no = <ls_data>-bp.
      ENDCASE.
    ELSE.
      READ TABLE gt_cepc INTO DATA(ls_cepc) WITH KEY prctr = <ls_data>-profit_ctr BINARY SEARCH.
      IF sy-subrc <> 0.
        <ls_data>-message = |Profit Center { <ls_data>-profit_ctr } doesn't exist |.
        <ls_data>-status  = icon_red_light.
        APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
        MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
        lw_error = 'X'.
        CONTINUE.
      ENDIF.
      READ TABLE gt_fagl_segm TRANSPORTING NO FIELDS WITH KEY segment = ls_cepc-segment BINARY SEARCH.
      IF sy-subrc <> 0.
        <ls_data>-message = |You are not authorized to post in Profit center { <ls_data>-profit_ctr }|.
        <ls_data>-status  = icon_red_light.
        APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
        MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
        lw_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

    " Check segment
    IF <ls_data>-segment IS NOT INITIAL.
      READ TABLE gt_fagl_segm TRANSPORTING NO FIELDS WITH KEY segment = <ls_data>-segment.
      IF sy-subrc <> 0.
        <ls_data>-message = |Segment { <ls_data>-segment ALPHA = OUT } doesn't exist |.
        <ls_data>-status  = icon_red_light.
        APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
        MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
        lw_error = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.
    " Check amount > 0.
    IF <ls_data>-amt_doccur <= 0.
      <ls_data>-message = |Uploaded correctly: Amount in local currency > 0|.
      <ls_data>-status  = icon_red_light.
      APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
      MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
      lw_error = 'X'.
      CONTINUE.
    ENDIF.
    " Check TAX amount >= 0.
    IF <ls_data>-tax_amt < 0 AND <ls_data>-tax_amt IS NOT INITIAL..
      <ls_data>-message = |Uploaded correctly: Tax Amount in local currency >= 0|.
      <ls_data>-status  = icon_red_light.
      APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
      MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
      lw_error = 'X'.
      CONTINUE.
    ENDIF.
    IF <ls_data>-pstng_date IS INITIAL.
      <ls_data>-message = |Input Posting Date must be in the format 'dd.mm.yyyy|.
      <ls_data>-status  = icon_red_light.
      APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
      MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
      lw_error = 'X'.
      CONTINUE.
    ENDIF.
    IF <ls_data>-doc_date IS INITIAL.
      <ls_data>-message = |Input Document Date must be in the format 'dd.mm.yyyy|.
      <ls_data>-status  = icon_red_light.
      APPEND INITIAL LINE TO gt_data_tmp ASSIGNING <ls_data_e>.
      MOVE-CORRESPONDING <ls_data> TO <ls_data_e>.
      lw_error = 'X'.
      CONTINUE.
    ENDIF.

    IF ls_header IS INITIAL.
      ls_header-comp_code   = <ls_data>-comp_code.
      ls_header-doc_date    = <ls_data>-doc_date.
      ls_header-pstng_date  = <ls_data>-pstng_date.
      ls_header-header_txt  = <ls_data>-header_txt.
      ls_header-ref_doc_no  = <ls_data>-ref_doc_no.
      ls_header-neg_postng  = <ls_data>-neg_postng.
      ls_header-username    = sy-uname.
      ls_header-bus_act     = 'RFBU'.
      ls_header-fisc_year   = <ls_data>-pstng_date+0(4).
      ls_header-fis_period  = <ls_data>-pstng_date+4(2).
      ls_header-doc_status  = '4'.
      ls_header-doc_type    = <ls_data>-doc_type.
      ls_header_tmp         = <ls_data>.

      " ref. structure for bapi parameter extensionin/extensionout
      IF <ls_data>-xref1_hd IS NOT INITIAL.
        APPEND VALUE #( structure  = 'XREF1_HD' valuepart1 = <ls_data>-xref1_hd ) TO lt_extens.
      ENDIF.
      IF <ls_data>-xref2_hd IS NOT INITIAL.
        APPEND VALUE #( structure  = 'XREF2_HD' valuepart1 = <ls_data>-xref2_hd ) TO lt_extens.
      ENDIF.
      lw_itemno = 0.
    ENDIF.

    IF <ls_data>-ref_key_2 IS NOT INITIAL AND <ls_data>-doc_type IN gr_type_pgr_up AND <ls_data>-gl_account IN gr_acc_pgr_up.
      READ TABLE gt_partner_gr INTO DATA(ls_partner_gr)
        WITH KEY partner_value = <ls_data>-ref_key_2
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <ls_data>-ref_key_2 = ls_partner_gr-partner_grid.
      ENDIF.
    ENDIF.

    ADD 1 TO lw_itemno.
    IF <ls_data>-gl_account IS NOT INITIAL AND <ls_data>-bp IS INITIAL.
      " ITEM - GL Acc
      APPEND INITIAL LINE TO lt_glacc ASSIGNING FIELD-SYMBOL(<ls_glacc>).
      MOVE-CORRESPONDING <ls_data> TO <ls_glacc>.
      <ls_glacc>-itemno_acc       = lw_itemno.
      <ls_glacc>-acct_type        = 'S'.
    ELSEIF <ls_data>-customer IS NOT INITIAL.
      " ITEM - Customer
      APPEND INITIAL LINE TO lt_customer ASSIGNING FIELD-SYMBOL(<ls_customer>).
      MOVE-CORRESPONDING <ls_data> TO <ls_customer>.
      <ls_customer>-itemno_acc    = lw_itemno.
      <ls_customer>-customer      = <ls_customer>-customer.
      <ls_customer>-businessplace = ' '.
    ELSEIF <ls_data>-vendor_no IS NOT INITIAL.
      " ITEM - VENDOR
      APPEND INITIAL LINE TO lt_vendor ASSIGNING FIELD-SYMBOL(<ls_vendor>).
      MOVE-CORRESPONDING <ls_data> TO <ls_vendor>.
      <ls_vendor>-itemno_acc      = lw_itemno.
      <ls_vendor>-vendor_no       = <ls_vendor>-vendor_no.
    ENDIF.

    " Item text
    <ls_data>-buzei = lw_itemno.

    " Line currency
    APPEND INITIAL LINE TO lt_cr ASSIGNING FIELD-SYMBOL(<ls_cr>).
    MOVE-CORRESPONDING <ls_data> TO <ls_cr>.
    <ls_cr>-itemno_acc    = lw_itemno.
    <ls_cr>-currency      = ls_header_tmp-currency.
    <ls_cr>-currency_iso  = ls_header_tmp-currency.
    <ls_cr>-curr_type     = '00'.  "Doc currency
    <ls_cr>-tax_amt       = 0.
    IF <ls_data>-de_cre_ind1 = 'H'.
      <ls_cr>-amt_doccur  = <ls_cr>-amt_doccur * ( -1 ).
    ENDIF.


    " ITEM - tax
    IF <ls_data>-tax_code IS NOT INITIAL.
      ADD 1 TO lw_itemno.
      READ TABLE lt_tax INTO DATA(ls_tax_exist) WITH KEY tax_code = <ls_data>-tax_code.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_tax ASSIGNING FIELD-SYMBOL(<ls_tax>).
        <ls_tax>-itemno_acc   = lw_itemno.
        <ls_tax>-tax_code     = <ls_data>-tax_code.

        " Line currency
        APPEND INITIAL LINE TO lt_cr ASSIGNING <ls_cr>.
        MOVE-CORRESPONDING <ls_data> TO <ls_cr>.
        <ls_cr>-currency      = ls_header_tmp-currency.
        <ls_cr>-currency_iso  = ls_header_tmp-currency.
        <ls_cr>-curr_type     = '00'.  "Doc currency
        <ls_cr>-itemno_acc    = lw_itemno.
        IF <ls_data>-de_cre_ind1 = 'H'.
          <ls_cr>-tax_amt     = <ls_cr>-tax_amt * ( -1 ).
          <ls_cr>-amt_base    = <ls_cr>-amt_doccur * ( -1 ).
          <ls_cr>-amt_doccur  = <ls_cr>-tax_amt.
        ELSE.
          <ls_cr>-amt_base    = <ls_cr>-amt_doccur.
          <ls_cr>-amt_doccur  = <ls_cr>-tax_amt.
        ENDIF.
      ELSE.
        READ TABLE lt_cr ASSIGNING <ls_cr> WITH KEY itemno_acc = ls_tax_exist-itemno_acc.
        IF sy-subrc = 0.
          IF <ls_data>-de_cre_ind1 = 'H'.
            <ls_cr>-tax_amt     = <ls_cr>-tax_amt  + <ls_data>-tax_amt * ( -1 ).
            <ls_cr>-amt_base    = <ls_cr>-amt_base + <ls_data>-amt_doccur * ( -1 ).
            <ls_cr>-amt_doccur  = <ls_cr>-tax_amt.
          ELSE.
            <ls_cr>-tax_amt     = <ls_cr>-tax_amt  + <ls_data>-tax_amt .
            <ls_cr>-amt_base    = <ls_cr>-amt_base + <ls_data>-amt_doccur.
            <ls_cr>-amt_doccur  = <ls_cr>-tax_amt.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " Item one time
    IF <ls_data>-name_1 IS NOT INITIAL.
      ls_onetime-name       = <ls_data>-name_1+0(35).
      ls_onetime-name_2     = <ls_data>-name_1+35(35).
      ls_onetime-name_3     = <ls_data>-name_1+70(35).
      ls_onetime-name_4     = <ls_data>-name_1+105(35).
      ls_onetime-langu_iso  = 'VI'.
      ls_onetime-city       = <ls_data>-city.
      ls_onetime-street     = <ls_data>-street.
      ls_onetime-country    = 'VN'.
      ls_onetime-bank_ctry  = 'VN'.
      ls_onetime-tax_no_1   = <ls_data>-tax_no_1.
    ENDIF.

    " CO-PA
    IF <ls_data>-kokrs IS NOT INITIAL.
      LOOP AT lo_field->components INTO DATA(ls_field).
        ASSIGN COMPONENT ls_field-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<ls_value>).
        IF NOT ( ls_field-name = 'KMVKRG' AND <ls_value> IS INITIAL ).
          APPEND VALUE #(
            character   = <ls_value>
            fieldname   = ls_field-name
            itemno_acc  = lw_itemno
            ) TO lt_criteria.
        ENDIF.
        FREE <ls_value>.
      ENDLOOP.
    ENDIF.

    APPEND INITIAL LINE TO lt_data_tmp ASSIGNING FIELD-SYMBOL(<ls_data_tmp>).
    MOVE-CORRESPONDING <ls_data> TO <ls_data_tmp>.

    AT END OF stt.
      local->do_stuff_in_parallel(
        ls_header   = ls_header
        ls_onetime  = ls_onetime
        ls_test     = p_test
        lt_glacc    = lt_glacc
        lt_customer = lt_customer
        lt_vendor   = lt_vendor
        lt_tax      = lt_tax
        lt_cr       = lt_cr
        lt_criteria = lt_criteria
        lt_extens   = lt_extens
        lt_return   = lt_return
        lt_data     = lt_data_tmp
      ).
      ADD 1 TO lw_count.
      IF lw_count = local->handler->threads.
        lw_count = 0.
        WAIT UNTIL local->handler->all_threads_are_finished( ).
      ENDIF.
    ENDAT.
  ENDLOOP.
  WAIT UNTIL local->handler->all_threads_are_finished( ).
  SORT gt_data_tmp BY stt.
  IF p_run = 'X'.
    DATA lt_ztb_fie003_02_lo TYPE TABLE OF ztb_fie003_02_lo.
    LOOP AT gt_data_tmp INTO DATA(ls_data_tmp).
      APPEND INITIAL LINE TO lt_ztb_fie003_02_lo ASSIGNING FIELD-SYMBOL(<ls_log>).
      MOVE-CORRESPONDING ls_data_tmp TO <ls_log>.
      <ls_log>-guid = gs_selected_file-guid.
    ENDLOOP.
    MODIFY ztb_fie003_02_lo FROM TABLE lt_ztb_fie003_02_lo.
  ENDIF.
  CLEAR p_frunn.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  FIELD-SYMBOLS: <lf_table> TYPE ANY TABLE.
  ASSIGN gt_data_tmp[] TO <lf_table>.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_salv
        CHANGING
          t_table      = <lf_table> ).
      go_salv->get_columns( )->set_optimize( ).
      go_salv->get_columns( )->set_column_position( columnname = 'STATUS' position = 1 ).
      go_salv->get_columns( )->set_column_position( columnname = 'MESSAGE' position = 2 ).
      go_salv->get_columns( )->get_column('MANDT')->set_visible( abap_false ).
      go_salv->get_columns( )->get_column('STATUS')->set_long_text('Status').
      go_salv->get_columns( )->get_column('STATUS')->set_medium_text('Status').
      go_salv->get_columns( )->get_column('STATUS')->set_short_text('Status').
      go_salv->get_columns( )->get_column('MESSAGE')->set_long_text('Message').
      go_salv->get_columns( )->get_column('MESSAGE')->set_medium_text('Message').
      go_salv->get_columns( )->get_column('MESSAGE')->set_short_text('Message').
      go_salv->get_columns( )->get_column('CUSTOMER')->set_visible( abap_false ).
      go_salv->get_columns( )->get_column('VENDOR_NO')->set_visible( abap_false ).
      go_salv->get_functions( )->set_all( ).
      go_salv->display( ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_msg.
    CATCH cx_salv_not_found.
  ENDTRY.
  FREE go_salv.
  UNASSIGN <lf_table>.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REMOVE_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM remove_file.
  DATA: lw_param TYPE sxpgcolist-parameters.

  SELECT SINGLE *
    FROM ztb_fie003_02_f
    INTO gs_selected_file
    WHERE guid      = p_frunn
      AND executed  = ''.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE `Selected File does not exsit in server` TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  DELETE ztb_fie003_02_f FROM gs_selected_file.
  COMMIT WORK AND WAIT.

  lw_param = |{ gs_selected_file-server_dir }/{ gs_selected_file-server_file }|. "Server file

  CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
    EXPORTING
      commandname                   = 'ZREMOVE'
      additional_parameters         = lw_param
    EXCEPTIONS
      no_permission                 = 1
      command_not_found             = 2
      parameters_too_long           = 3
      security_risk                 = 4
      wrong_check_call_interface    = 5
      program_start_error           = 6
      program_termination_error     = 7
      x_error                       = 8
      parameter_expected            = 9
      too_many_parameters           = 10
      illegal_command               = 11
      wrong_asynchronous_parameters = 12
      cant_enq_tbtco_entry          = 13
      jobcount_generation_error     = 14
      OTHERS                        = 15.
  IF sy-subrc = 0.
    MESSAGE 'Remove file success' TYPE 'S'.
  ENDIF.
ENDFORM.
