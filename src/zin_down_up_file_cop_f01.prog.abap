*&---------------------------------------------------------------------*
*& Include          ZIN_DOWN_UP_FILE_F01
*&---------------------------------------------------------------------*
class lcl_main definition.
  public section.
    class-methods:
      start,
      get_local_path,
      get_file importing iv_value type rcgfiletr-ftappl.
  private section.
    class-data:
      lo_main   type ref to lcl_main.
    methods:
      download_file importing iw_sv    type eseftappl
                              iw_local type eseftfront
                              iw_file  type eseftfront,
      upload_file importing iw_sv    type eseftappl
                            iw_local type eseftfront
                            iw_file  type eseftfront.
endclass.

class lcl_main implementation.
  method start.
    " Assign Program Name Similar T-Code CG3Y
    sy-cprog = 'RC1TCG3Y'.
    create object lo_main.
    if p_dow = abap_true.
      loop at s_files into data(ls_file).
        lo_main->download_file( iw_sv    = |{ p_sv }/{ ls_file-low }|
                                iw_local = |{ p_local }\\{ ls_file-low }|
                                iw_file  = ls_file-low ).
      endloop.
    else.
      loop at s_files into ls_file.
        lo_main->upload_file( iw_sv    = |{ p_sv }/{ ls_file-low }|
                              iw_local = |{ p_local }\\{ ls_file-low }|
                              iw_file  = ls_file-low ).
      endloop.
    endif.
  endmethod.

  method get_local_path.
    data(lw_folder_path) = value string( ).
    cl_gui_frontend_services=>directory_browse(
      exporting
        window_title    = 'Select Target Folder On Front End'
        initial_folder  = lw_folder_path
      changing
        selected_folder = lw_folder_path ).

    p_local = lw_folder_path.
  endmethod.

  method get_file.
    if p_upl is initial.
      message 'For Upload only' type 'I'.
      return.
    endif.

    data: lt_file_table type table of sdokpath,
          lt_dir_table  type table of sdokpath,
          lt_return     type table of ddshretval.
    call function 'TMP_GUI_DIRECTORY_LIST_FILES'
      exporting
        directory  = p_local
*       FILTER     = '*.*'
*      importing
*       file_count =
*       DIR_COUNT  =
      tables
        file_table = lt_file_table
        dir_table  = lt_dir_table
      exceptions
        cntl_error = 1
        others     = 2.
    if lt_file_table is initial.
      message 'Cannot find any file' type 'I'.
      return.
    endif.

    clear lt_return.
    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'S_FILES'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'PATHNAME'
*       STEPL           = 0
        window_title    = 'Select Files'
*       VALUE           = ' '
        value_org       = 'S'
        multiple_choice = 'X'
      tables
        value_tab       = lt_file_table
        return_tab      = lt_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    loop at lt_return into data(ls_return).
      append value #( low = ls_return-fieldval option = 'EQ' sign = 'I' ) to s_files[].
      if s_files is initial.
        s_files = s_files[ 1 ].
      endif.
    endloop.

  endmethod.

  method download_file.
    data: lw_flg_open_error  type boolean,
          lw_os_message(100) type c.
    call function 'C13Z_FILE_DOWNLOAD_BINARY'
      exporting
        i_file_front_end    = iw_local
        i_file_appl         = iw_sv
        i_file_overwrite    = 'X'
      importing
        e_flg_open_error    = lw_flg_open_error
        e_os_message        = lw_os_message
      exceptions
        fe_file_open_error  = 1
        fe_file_exists      = 2
        fe_file_write_error = 3
        ap_no_authority     = 4
        ap_file_open_error  = 5
        ap_file_empty       = 6
        others              = 7.
    if lw_flg_open_error = abap_false .
      write /1 |{ iw_file } - Download successfully|.
    else.
      write /1 |{ iw_file } - { lw_os_message }|.
    endif.
  endmethod.

  method upload_file.
    data: lw_flg_open_error  type boolean,
          lw_os_message(100) type c.
    call function 'C13Z_FILE_UPLOAD_BINARY'
      exporting
        i_file_front_end    = iw_local
        i_file_appl         = iw_sv
        i_file_overwrite    = 'X'
      importing
        e_flg_open_error    = lw_flg_open_error
        e_os_message        = lw_os_message
      exceptions
        fe_file_open_error  = 1
        fe_file_exists      = 2
        fe_file_write_error = 3
        ap_no_authority     = 4
        ap_file_open_error  = 5
        ap_file_empty       = 6
        others              = 7.
    if lw_flg_open_error = abap_false .
      write /1 |{ iw_file } - Upload successfully|.
    else.
      write /1 |{ iw_file } - { lw_os_message }|.
    endif.
  endmethod.

endclass.
