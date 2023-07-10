FUNCTION zfm_raf00_out_demo.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(_APINO) TYPE  ZTRAF_MAINTAIN-APINO
*"     REFERENCE(TEST) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(MESSAGE) TYPE  STRING
*"     REFERENCE(TIMESTAMP) TYPE  TIMESTAMP
*"     REFERENCE(DATE) TYPE  DATUM
*"     REFERENCE(TIME) TYPE  UZEIT
*"     REFERENCE(RESULT) TYPE  SFLIGHT
*"----------------------------------------------------------------------

  " _apino 必输 但不会放到输出内容

  DATA: lo_handler TYPE REF TO zif_raf_outbound.
  DATA: ls_header TYPE header_fb,
        lt_tables TYPE rsfb_para,
        lt_import TYPE rsfb_para,
        lt_export TYPE rsfb_para,
        lt_change TYPE rsfb_para.
  DATA: lo_table TYPE REF TO data.
  DATA: lt_req_comp TYPE cl_abap_structdescr=>component_table,
        lt_res_comp TYPE cl_abap_structdescr=>component_table,
        ls_dyn_comp LIKE LINE OF lt_req_comp.
  DATA: lo_req_tab  TYPE REF TO data,
        lo_req_data TYPE REF TO data,
        lo_res_tab  TYPE REF TO data,
        lo_res_data TYPE REF TO data.

  FIELD-SYMBOLS: <fs_req_tab> TYPE ANY TABLE.
  FIELD-SYMBOLS: <fs_res_tab> TYPE ANY TABLE.
  FIELD-SYMBOLS: <fs_table> TYPE ANY TABLE.
  FIELD-SYMBOLS: <ls_req_data> TYPE any.
  FIELD-SYMBOLS: <ls_res_data> TYPE any.

*1 调用工厂类创建接口实例
  lo_handler = zcl_raf_outbound_func=>factory( _apino ).
  IF lo_handler IS INITIAL. " => 配置后不会报错
    MESSAGE 'no set ztraf_oconf table' TYPE 'E'.
  ENDIF.

*2 构造传入传出参数对象
  DATA: lt_abap_stack TYPE abap_callstack,
        lt_sys_stack  TYPE sys_callst.
  CALL FUNCTION 'SYSTEM_CALLSTACK'
*  EXPORTING
*    MAX_LEVEL          = 0
    IMPORTING
      callstack    = lt_abap_stack
      et_callstack = lt_sys_stack.
  READ TABLE lt_abap_stack INTO DATA(ls_abap_stack) INDEX 1.

  ls_header-name  = ls_abap_stack-blockname.
  ls_header-state = 'A' .
  CALL METHOD cl_fb_parameter_db=>read
    IMPORTING
      tables = lt_tables
      import = lt_import
      export = lt_export
      change = lt_change
    CHANGING
      header = ls_header.
  LOOP AT lt_tables INTO DATA(ls_tables).
    IF ls_tables-typefield = 'LIKE'.
      CREATE DATA lo_table TYPE STANDARD TABLE OF (ls_tables-structure).
      ASSIGN lo_table->* TO <fs_table>.
      CLEAR ls_dyn_comp.
      ls_dyn_comp-name = ls_tables-parameter.
      ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_data( <fs_table> ).
      APPEND ls_dyn_comp TO lt_req_comp.
      APPEND ls_dyn_comp TO lt_res_comp.
    ELSEIF ls_tables-typefield = 'TYPE'.
      CLEAR ls_dyn_comp.
      ls_dyn_comp-name = ls_tables-parameter.
      ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( ls_tables-structure ).
      APPEND ls_dyn_comp TO lt_req_comp.
      APPEND ls_dyn_comp TO lt_res_comp.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_import INTO DATA(ls_import).
    CHECK ls_import-parameter <> '_APINO'.

    CLEAR ls_dyn_comp.
    ls_dyn_comp-name = ls_import-parameter.
    ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( ls_import-structure ).
    APPEND ls_dyn_comp TO lt_req_comp.
  ENDLOOP.
  LOOP AT lt_export INTO DATA(ls_export).
    CLEAR ls_dyn_comp.
    ls_dyn_comp-name = ls_export-parameter.
    ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( ls_export-structure ).
    APPEND ls_dyn_comp TO lt_res_comp.
  ENDLOOP.
  LOOP AT lt_change INTO DATA(ls_change).
    CLEAR ls_dyn_comp.
    ls_dyn_comp-name = ls_change-parameter.
    ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( ls_change-structure ).
    APPEND ls_dyn_comp TO lt_req_comp.
    APPEND ls_dyn_comp TO lt_res_comp.
  ENDLOOP.
  DATA(lo_req_type) = cl_abap_structdescr=>create( lt_req_comp ).
  DATA(lo_res_type) = cl_abap_structdescr=>create( lt_res_comp ).
  DATA(lo_req_tabdesc) = cl_abap_tabledescr=>create(
                  p_line_type  = lo_req_type
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).
  DATA(lo_res_tabdesc) = cl_abap_tabledescr=>create(
                  p_line_type  = lo_res_type
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).
  CREATE DATA lo_req_tab TYPE HANDLE lo_req_tabdesc.
  ASSIGN lo_req_tab->* TO <fs_req_tab>.
  CREATE DATA lo_req_data LIKE LINE OF <fs_req_tab>.
  ASSIGN lo_req_data->* TO <ls_req_data>.
  CREATE DATA lo_res_tab TYPE HANDLE lo_res_tabdesc.
  ASSIGN lo_res_tab->* TO <fs_res_tab>.
  CREATE DATA lo_res_data LIKE LINE OF <fs_res_tab>.
  ASSIGN lo_res_data->* TO <ls_res_data>.

*3 传入参数
  LOOP AT lt_tables INTO ls_tables.
    ASSIGN COMPONENT ls_tables-parameter OF STRUCTURE <ls_req_data>
      TO FIELD-SYMBOL(<fs_para_data>).
    IF sy-subrc = 0.
      ASSIGN (ls_tables-parameter) TO FIELD-SYMBOL(<fs_parameter>).
      <fs_para_data> = <fs_parameter>.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_import INTO ls_import.
    ASSIGN COMPONENT ls_import-parameter OF STRUCTURE <ls_req_data>
      TO <fs_para_data>.
    IF sy-subrc = 0.
      ASSIGN (ls_import-parameter) TO <fs_parameter> .
      <fs_para_data> = <fs_parameter>.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_change INTO ls_change.
    ASSIGN COMPONENT ls_change-parameter OF STRUCTURE <ls_req_data>
      TO <fs_para_data>.
    IF sy-subrc = 0.
      ASSIGN (ls_change-parameter) TO <fs_parameter> .
      <fs_para_data> = <fs_parameter>.
    ENDIF.
  ENDLOOP.

*4 调用实例方法发送数据
  CALL METHOD lo_handler->request
    EXPORTING
      i_data = lo_req_data
    IMPORTING
      e_data = lo_res_data.

  " 回写
  LOOP AT lt_tables INTO ls_tables.
    ASSIGN COMPONENT ls_tables-parameter OF STRUCTURE <ls_res_data>
      TO <fs_para_data>.
    IF sy-subrc = 0.
      ASSIGN (ls_tables-parameter) TO <fs_parameter>.
      <fs_parameter> = <fs_para_data>.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_export INTO ls_export.
    ASSIGN COMPONENT ls_export-parameter OF STRUCTURE <ls_res_data>
      TO <fs_para_data>.
    IF sy-subrc = 0.
      ASSIGN (ls_export-parameter) TO <fs_parameter> .
      <fs_parameter> = <fs_para_data>.
    ENDIF.
  ENDLOOP.
  LOOP AT lt_change INTO ls_change.
    ASSIGN COMPONENT ls_change-parameter OF STRUCTURE <ls_res_data>
      TO <fs_para_data>.
    IF sy-subrc = 0.
      ASSIGN (ls_change-parameter) TO <fs_parameter> .
      <fs_parameter> = <fs_para_data>.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
