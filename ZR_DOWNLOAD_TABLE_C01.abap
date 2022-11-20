*======================================================================*
*                                                                      *
*                       github.com/vitorcarlessi/                      *
*                                                                      *
*======================================================================*
* Program.....: ZR_DOWNLOAD_TABLE                                      *
* Include.....: ZR_DOWNLOAD_TABLE_C01                                  *
* Module......: ALL                                                    *
* Description.: Download Table Data to CSV File                        *
*----------------------------------------------------------------------*
* Author......: Vitor Crepaldi Carlessi                                *
* Date........: 18.11.2022                                             *
*======================================================================*
*----------------------------------------------------------------------*
*   Class GCL_DOWNLOAD_TABLE                                           *
*----------------------------------------------------------------------*
CLASS gcl_download_table DEFINITION FINAL.

  *----------------------------------------------------------------------*
  *   Public Section GCL_DOWNLOAD_TABLE                                  *
  *----------------------------------------------------------------------*
    PUBLIC SECTION.
  
      "Atributos Classe - Seção Publica
      DATA: mt_table_header_fields TYPE truxs_t_text_data.
      DATA: mv_year                TYPE string.
      DATA: mv_month               TYPE string.
      DATA: mv_day                 TYPE string.
      DATA: mv_hour                TYPE string.
      DATA: mv_minutes             TYPE string.
  
      METHODS:
        "1)Constructor - Constructor Method
        constructor,
  
        "2)Full Process -> Performs all processing
        full_process,
  
        "3)Generate CSV -> Generate CSV. file with the table chose
        generate_csv IMPORTING pv_table_name      TYPE tabname
                     CHANGING  pt_converted_table TYPE truxs_t_text_data,
  
        "4)Download CSV -> Download CSV file localy
        download_csv IMPORTING pv_table_name      TYPE tabname
                     CHANGING  pt_converted_table TYPE truxs_t_text_data,
  
        "5)Get Directory Browser
        get_directory_browser.
  
  ENDCLASS.
  
  CLASS gcl_download_table IMPLEMENTATION.
  *----------------------------------------------------------------------*
  * 1) Classe GCL_DOWNLOAD_TABLE->CONSTRUCTOR                            *
  *----------------------------------------------------------------------*
    METHOD constructor ##NEEDED.
  
      "Get Year
      mv_year    = sy-datum+0(4).
  
      "Get Month
      mv_month   = sy-datum+4(2).
  
      "Get Day
      mv_day     = sy-datum+6(2).
  
      "Get Hour
      mv_hour    = sy-uzeit+0(2).
  
      "Get Minutes
      mv_minutes = sy-uzeit+2(2).
  
    ENDMETHOD.
  *----------------------------------------------------------------------*
  * 2) Classe GCL_DOWNLOAD_TABLE->FULL_PROCESS                           *
  *----------------------------------------------------------------------*
    METHOD full_process.
  
  *----------------------------------------------------------------*
  * Tables                                                         *
  *----------------------------------------------------------------*
      DATA: lt_converted_table TYPE truxs_t_text_data.
  
  *----------------------------------------------------------------*
  * Variables                                                      *
  *----------------------------------------------------------------*
      DATA: lv_table_name   TYPE tabname.
  
      "LOOP -> Range of Tables
      LOOP AT s_rsrd1 ASSIGNING FIELD-SYMBOL(<fs_rsrd1>).
  
        "Clear Variables/Tables
        CLEAR: lv_table_name, lt_converted_table.
  
        "Get Table Name
        lv_table_name = <fs_rsrd1>-low.
  
        "1)Generate CSV
        go_download_table->generate_csv( EXPORTING pv_table_name      = lv_table_name
                                         CHANGING  pt_converted_table = lt_converted_table ).
  
        "2)Download CSV
        go_download_table->download_csv( EXPORTING pv_table_name      = lv_table_name
                                         CHANGING  pt_converted_table = lt_converted_table ).
  
      ENDLOOP.
  
      "3)Everything OK
      "Programa processado com sucesso!
      MESSAGE text-t09 TYPE 'S'.
  
    ENDMETHOD.
  *----------------------------------------------------------------------*
  * 3) Classe GCL_DOWNLOAD_TABLE->GENERATE_CSV                           *
  *----------------------------------------------------------------------*
    METHOD generate_csv.
  
  *----------------------------------------------------------------*
  * Constants                                                      *
  *----------------------------------------------------------------*
      CONSTANTS: lc_seperator TYPE char01 VALUE ';'.
  
  *----------------------------------------------------------------*
  * Classes                                                        *
  *----------------------------------------------------------------*
      DATA: lo_tabtype     TYPE REF TO cl_abap_tabledescr,
            lo_struct_type TYPE REF TO cl_abap_structdescr,
            lr_data        TYPE REF TO data.
  
  *----------------------------------------------------------------*
  * Tables                                                         *
  *----------------------------------------------------------------*
      DATA: lt_comp_tab TYPE cl_abap_structdescr=>component_table.
  
  *----------------------------------------------------------------*
  * Variables                                                      *
  *----------------------------------------------------------------*
      DATA: lv_table_name   TYPE tabname.
  
  *----------------------------------------------------------------*
  * Field-Symbols                                                  *
  *----------------------------------------------------------------*
      FIELD-SYMBOLS: <lt_table_values>        TYPE STANDARD TABLE.
  
      "Get Table Name
      lv_table_name = pv_table_name.
  
      "Description of the type using relative/absolute names
      lo_struct_type ?= cl_abap_typedescr=>describe_by_name( lv_table_name ).
  
      "Table LT_COMP_TAB receive the value
      lt_comp_tab     = lo_struct_type->get_components( ).
  
      "Factory method for generating structure types without reuse
      lo_struct_type = cl_abap_structdescr=>create( lt_comp_tab ).
  
      "Factory method for generating structure types without reuse
      lo_tabtype     = cl_abap_tabledescr=>create( lo_struct_type ).
  
      "Dynamic Table Create
      CREATE DATA lr_data TYPE HANDLE lo_tabtype.
      ASSIGN lr_data->* TO <lt_table_values>.
  
      "Select table
      SELECT *
      FROM (lv_table_name)
      INTO CORRESPONDING FIELDS OF TABLE <lt_table_values>.
  
      "Empty Table Entries
      IF <lt_table_values> IS INITIAL.
        "Não foram encontradas entradas na tabela passada
        MESSAGE text-t08 TYPE 'E'.
      ENDIF.
  
      "Generate CSV
      CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
        EXPORTING
          i_field_seperator    = lc_seperator
        TABLES
          i_tab_sap_data       = <lt_table_values>
        CHANGING
          i_tab_converted_data = pt_converted_table
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
  
      IF sy-subrc IS NOT INITIAL.
        "Erro  ao fazer download em CSV.
        MESSAGE text-t01 TYPE 'E'.
        RETURN.
      ENDIF.
  
      "Get Name of Fields - CSV Header
      lo_struct_type ?= lo_tabtype->get_table_line_type( ).
  
      "Get CSV Header
      APPEND INITIAL LINE TO mt_table_header_fields ASSIGNING FIELD-SYMBOL(<fs_table_header_fields>).
  
      "LOOP -> Name of fields in table
      LOOP AT lo_struct_type->components ASSIGNING FIELD-SYMBOL(<fs_components>).
  
        AT LAST.
          "Last Line - Get only the last field name
          <fs_table_header_fields> = |{ <fs_table_header_fields> }{ <fs_components>-name }|.
          EXIT.
        ENDAT.
  
        "Get field name and separator
        <fs_table_header_fields> = |{ <fs_table_header_fields> }{ <fs_components>-name }{ lc_seperator }|.
  
      ENDLOOP.
  
    ENDMETHOD.
  *----------------------------------------------------------------------*
  * 4) Classe GCL_DOWNLOAD_TABLE->DOWNLOAD_CSV                           *
  *----------------------------------------------------------------------*
    METHOD download_csv.
  
  *----------------------------------------------------------------*
  * Variables                                                      *
  *----------------------------------------------------------------*
      CONSTANTS: lc_filetype   TYPE char10 VALUE 'ASC',
                 lc_csv        TYPE char3  VALUE 'csv',
                 lc_dot        TYPE char1  VALUE '.',
                 lc_underscore TYPE char1  VALUE '_',
                 lc_dot_csv    TYPE char4  VALUE '.csv' ##NO_TEXT.
  
  *----------------------------------------------------------------*
  * Tables                                                         *
  *----------------------------------------------------------------*
      DATA: lt_converted_table      TYPE truxs_t_text_data.
  
  *----------------------------------------------------------------*
  * Variables                                                      *
  *----------------------------------------------------------------*
      DATA: lv_filename       TYPE string,
            lv_generated_name TYPE string,
            lv_download_file  TYPE abap_bool,
            lv_line_counter   TYPE i,
            lv_file_counter   TYPE i,
            lv_dot_count      TYPE i,
            lv_lines_all      TYPE i,
            lv_line_per_files TYPE i,
            lv_file_name      TYPE ibipparms-path.
  
      "Get CSV Header
      READ TABLE mt_table_header_fields ASSIGNING FIELD-SYMBOL(<fs_table_header_fields>) INDEX 1.
      IF sy-subrc IS NOT INITIAL.
        RETURN.
      ENDIF.
  
      "Get Number of lines per file
      DESCRIBE TABLE pt_converted_table LINES lv_lines_all.
      lv_line_per_files = lv_lines_all / p_files.
  
      "Fill CSV Header - First File
      APPEND INITIAL LINE TO lt_converted_table ASSIGNING FIELD-SYMBOL(<fs_lt_converted_table>).
      <fs_lt_converted_table> = <fs_table_header_fields>.
  
      "LOOP -> LT_CONVERTED_TABLE
      LOOP AT pt_converted_table ASSIGNING FIELD-SYMBOL(<fs_pt_converted_table>).
  
        "Count Line
        lv_line_counter = lv_line_counter + 1.
  
        "Get Values to Download
        APPEND INITIAL LINE TO lt_converted_table ASSIGNING <fs_lt_converted_table>.
        <fs_lt_converted_table> = <fs_pt_converted_table>.
  
        "Generate Files Per number of Files chosed
        IF lv_line_counter EQ lv_line_per_files.
          "Mark variable to download file
          lv_download_file = abap_true.
        ENDIF.
  
        "Verify Last Line
        AT LAST.
  
          IF lv_download_file IS INITIAL.
  
            "Less lines than expected on LT_CONVERTED_TABLE
            IF lv_line_counter LT lv_line_per_files.
  
              "LT_CONVERTED_TABLE is not empty
              IF lt_converted_table IS NOT INITIAL.
                "Mark variable to download file
                lv_download_file = abap_true.
              ENDIF.
  
            ENDIF.
  
          ENDIF.
  
        ENDAT.
  
        "Download ITAB
        IF lv_download_file EQ abap_true.
  
          "Count Files Generated
          lv_file_counter = lv_file_counter + 1.
  
          "All checks OK - Fill CSV Name Dynamic
          "Ex: C:\Users\Vitor\Desktop\Download BKPF\BKPF_20221118_1611_01.csv
          lv_filename       = |{ p_csv_p }{ pv_table_name }{ lc_underscore }{ mv_year }{ mv_month }{ mv_day }{ lc_underscore }{ mv_hour }{ mv_minutes }{ lc_underscore }{ lv_file_counter }{ lc_dot_csv }|.
  
          "Download CSV
          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              filename                = lv_filename
              filetype                = lc_filetype
            TABLES
              data_tab                = lt_converted_table
            EXCEPTIONS
              file_write_error        = 1
              no_batch                = 2
              gui_refuse_filetransfer = 3
              invalid_type            = 4
              no_authority            = 5
              unknown_error           = 6
              header_not_allowed      = 7
              separator_not_allowed   = 8
              filesize_not_allowed    = 9
              header_too_long         = 10
              dp_error_create         = 11
              dp_error_send           = 12
              dp_error_write          = 13
              unknown_dp_error        = 14
              access_denied           = 15
              dp_out_of_memory        = 16
              disk_full               = 17
              dp_timeout              = 18
              file_not_found          = 19
              dataprovider_exception  = 20
              control_flush_error     = 21
              OTHERS                  = 22.
  
          "Check Errors
          IF sy-subrc IS NOT INITIAL.
            "Erro ao fazer download do arquivo CSV localmente
            MESSAGE text-t07 TYPE 'E'.
          ENDIF.
  
          "Clear Tables and Variables
          CLEAR: lt_converted_table, lv_download_file, lv_line_counter.
  
          "Fill CSV Header - Not First File anymore
          APPEND INITIAL LINE TO lt_converted_table ASSIGNING <fs_lt_converted_table>.
          <fs_lt_converted_table> = <fs_table_header_fields>.
  
        ENDIF.
  
      ENDLOOP.
  
    ENDMETHOD.
  *----------------------------------------------------------------------*
  * 4) Classe GCL_DOWNLOAD_TABLE->GET_DIRECTORY_BROWSER                  *
  *----------------------------------------------------------------------*
    METHOD get_directory_browser.
  
  *----------------------------------------------------------------*
  * Constants                                                      *
  *----------------------------------------------------------------*
      DATA: lc_backslash TYPE c VALUE '\'.
  
  *----------------------------------------------------------------*
  * Variables                                                      *
  *----------------------------------------------------------------*
      DATA: lv_initial_folder  TYPE string,
            lv_selected_folder TYPE string.
  
      "Get Directory Browser
      CALL METHOD cl_gui_frontend_services=>directory_browse
        EXPORTING
          initial_folder       = lv_initial_folder
        CHANGING
          selected_folder      = lv_selected_folder
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
  
      "Pass the selected folder to parameter
      p_csv_p = |{ lv_selected_folder }{ lc_backslash }|.
  
    ENDMETHOD.
  
  ENDCLASS.