*&---------------------------------------------------------------------*
*& Include ZRAF_FMACROS
*&---------------------------------------------------------------------*

" Func Macros

" /raf/check
" /raf/init
" /raf/end

" /raf/_store_data

DEFINE /raf/check.
  IF zcl_raf_inbound_func=>remotecall = abap_false.
    " 检查有效性
    SELECT SINGLE * FROM ztraf_maintain WHERE apino = &1 INTO @DATA(/raf/ls_main).
    IF /raf/ls_main-deact = 'X'.
      RETURN.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.

DEFINE /raf/init.
  DATA: /raf/callstack TYPE abap_callstack.
  DATA: /raf/ls_header TYPE header_fb,
        /raf/lt_tables TYPE rsfb_para,
        /raf/lt_import TYPE rsfb_para,
        /raf/lt_export TYPE rsfb_para,
        /raf/lt_change TYPE rsfb_para.
  DATA: /raf/lt_parameter TYPE rsfb_para.
  DATA: /raf/lt_dyn_compi TYPE cl_abap_structdescr=>component_table,
        /raf/lt_dyn_compo TYPE cl_abap_structdescr=>component_table,
        /raf/ls_dyn_comp LIKE LINE OF /raf/lt_dyn_compi.
  DATA: /raf/lo_table TYPE REF TO data.
  DATA: /raf/lo_req_tab  TYPE REF TO data,
        /raf/lo_req_data TYPE REF TO data,
        /raf/lo_res_tab  TYPE REF TO data,
        /raf/lo_res_data TYPE REF TO data.
  FIELD-SYMBOLS: </raf/lt_re_tab> TYPE ANY TABLE.
  FIELD-SYMBOLS: </raf/ls_re_data> TYPE data.
  FIELD-SYMBOLS: </raf/lo_func_data> TYPE data.
  FIELD-SYMBOLS: </raf/lo_json_data> TYPE data.

  DATA: BEGIN OF /raf/ls_key,
          apino TYPE ztraf_log_data-apino,
          dirio TYPE ztraf_log_data-dirio,
          dguid TYPE ztraf_log_data-dguid,
        END OF /raf/ls_key.
  DATA: /raf/ls_data TYPE ztraf_log_data,
        /raf/jsonstr TYPE string.

  IF zcl_raf_inbound_func=>remotecall = abap_false.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = /raf/callstack.

    DATA(/raf/func_name) = VALUE #( /raf/callstack[ 1 ]-blockname OPTIONAL ).

    IF /raf/ls_main-logdt = 'A'.
      " 获取报文
      /raf/ls_header = VALUE #( name = /raf/func_name state = 'A' ).

      cl_fb_parameter_db=>read(
        IMPORTING
          tables = /raf/lt_tables
          import = /raf/lt_import
          export = /raf/lt_export
          change = /raf/lt_change
        CHANGING
          header = /raf/ls_header
      ).
      APPEND LINES OF /raf/lt_import TO /raf/lt_parameter.
      APPEND LINES OF /raf/lt_export TO /raf/lt_parameter.
      APPEND LINES OF /raf/lt_change TO /raf/lt_parameter.
      APPEND LINES OF /raf/lt_tables TO /raf/lt_parameter.

      LOOP AT /raf/lt_parameter INTO DATA(/raf/ls_parameter).
        IF /raf/ls_parameter-paramtype = 'I'. " 仅传入参数
          /raf/ls_dyn_comp-name = /raf/ls_parameter-parameter.
          /raf/ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( /raf/ls_parameter-structure ).
          APPEND /raf/ls_dyn_comp TO /raf/lt_dyn_compi.
          CLEAR /raf/ls_dyn_comp.
          CONTINUE.
        ELSEIF /raf/ls_parameter-paramtype = 'E'.
          /raf/ls_dyn_comp-name = /raf/ls_parameter-parameter.
          /raf/ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( /raf/ls_parameter-structure ).
          APPEND /raf/ls_dyn_comp TO /raf/lt_dyn_compo.
          CLEAR /raf/ls_dyn_comp.
          CONTINUE.
        ENDIF.
        IF /raf/ls_parameter-typefield = 'LIKE'.
          CREATE DATA /raf/lo_table TYPE STANDARD TABLE OF (/raf/ls_parameter-structure).

          /raf/ls_dyn_comp-name = /raf/ls_parameter-parameter.
          /raf/ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_data_ref( /raf/lo_table ).
          APPEND /raf/ls_dyn_comp TO /raf/lt_dyn_compi.
          APPEND /raf/ls_dyn_comp TO /raf/lt_dyn_compo.
          CLEAR /raf/ls_dyn_comp.
        ELSEIF /raf/ls_parameter-typefield = 'TYPE'.
          /raf/ls_dyn_comp-name = /raf/ls_parameter-parameter.
          /raf/ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( /raf/ls_parameter-structure ).
          APPEND /raf/ls_dyn_comp TO /raf/lt_dyn_compi.
          APPEND /raf/ls_dyn_comp TO /raf/lt_dyn_compo.
          CLEAR /raf/ls_dyn_comp.
        ENDIF.
      ENDLOOP.
      DATA(/raf/lo_res_type) = cl_abap_structdescr=>create( /raf/lt_dyn_compi ).
      DATA(/raf/lo_res_tabdesc) = cl_abap_tabledescr=>create( p_line_type  = /raf/lo_res_type
                                                              p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                              p_unique     = abap_false ).
      CREATE DATA /raf/lo_res_tab TYPE HANDLE /raf/lo_res_tabdesc.
      ASSIGN /raf/lo_res_tab->* TO </raf/lt_re_tab>.
      CREATE DATA /raf/lo_res_data LIKE LINE OF </raf/lt_re_tab>.
      ASSIGN /raf/lo_res_data->* TO </raf/ls_re_data>.
      " 传入参数存储
      LOOP AT /raf/lt_parameter INTO DATA(/raf/ls_parameteri).
        ASSIGN (/raf/ls_parameteri-parameter) TO FIELD-SYMBOL(</raf/lo_func_datai>).
        IF sy-subrc = 0.
          ASSIGN COMPONENT /raf/ls_parameteri-parameter OF STRUCTURE </raf/ls_re_data>
            TO FIELD-SYMBOL(</raf/lo_json_datai>).
          IF sy-subrc = 0.
            </raf/lo_json_datai> = </raf/lo_func_datai>.
          ENDIF.
        ENDIF.
      ENDLOOP.
      " 转JSON
      /raf/_store_data 'I' </raf/ls_re_data>.
    ENDIF.

    zcl_raf_ilog=>factory(  /raf/ls_main-apino ).
  ENDIF.
END-OF-DEFINITION.

DEFINE /raf/end.
  IF zcl_raf_inbound_func=>remotecall = abap_false.
    IF /raf/ls_main-logdt = 'A'.
      /raf/lo_res_type = cl_abap_structdescr=>create( /raf/lt_dyn_compo ).
      /raf/lo_res_tabdesc = cl_abap_tabledescr=>create( p_line_type  = /raf/lo_res_type
                                                        p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                        p_unique     = abap_false ).
      CREATE DATA /raf/lo_res_tab TYPE HANDLE /raf/lo_res_tabdesc.
      ASSIGN /raf/lo_res_tab->* TO </raf/lt_re_tab>.
      CREATE DATA /raf/lo_res_data LIKE LINE OF </raf/lt_re_tab>.
      ASSIGN /raf/lo_res_data->* TO </raf/ls_re_data>.
      " 传出参数存储
      LOOP AT /raf/lt_parameter INTO /raf/ls_parameter WHERE paramtype <> 'I'.
        ASSIGN (/raf/ls_parameter-parameter) TO </raf/lo_func_data>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT /raf/ls_parameter-parameter OF STRUCTURE </raf/ls_re_data>
            TO </raf/lo_json_data>.
          IF sy-subrc = 0.
            </raf/lo_json_data> = </raf/lo_func_data>.
          ENDIF.
        ENDIF.
      ENDLOOP.
      " 转JSON
      /raf/_store_data 'O' </raf/ls_re_data>.
    ENDIF.
    zcl_raf_ilog=>end( ).
    zcl_raf_ilog=>free( ).
  ENDIF.
END-OF-DEFINITION.

DEFINE /raf/_store_data.
  /raf/ls_data-apino = /raf/ls_main-apino.
  /raf/ls_data-dirio = &1.
  IF zcl_raf_inbound_func=>dguid IS INITIAL.
    TRY.
        /raf/ls_data-dguid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
      CATCH cx_uuid_error.
        GET TIME. /raf/ls_data-dguid = sy-datum && sy-uzeit.
    ENDTRY.
    zcl_raf_inbound_func=>dguid = /raf/ls_data-dguid.
  ELSE.
    /raf/ls_data-dguid = zcl_raf_inbound_func=>dguid.
  ENDIF.
  /raf/ls_data-userid = sy-uname.
  GET TIME STAMP FIELD /raf/ls_data-timestamp.
  MOVE-CORRESPONDING /raf/ls_data TO /raf/ls_key.
  /raf/jsonstr = /ui2/cl_json=>serialize( data = &2 ).
  EXPORT str = /raf/jsonstr TO DATABASE ztraf_log_data(zz) FROM /raf/ls_data ID /raf/ls_key.
  COMMIT WORK.
END-OF-DEFINITION.
