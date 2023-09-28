CLASS zcl_raf_inbound_func DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    CLASS-DATA dguid TYPE ztraf_log-dguid READ-ONLY .
    CLASS-DATA targt TYPE ztraf_log-targt READ-ONLY .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA raf_maintain TYPE ztraf_maintain .
    DATA raf_iconf TYPE ztraf_iconf .
    DATA o_config TYPE REF TO zcl_raf_config .
    DATA fdump TYPE flag .

    CLASS-METHODS readme .
    METHODS check_apino
      IMPORTING
        !apino       TYPE ztraf_log-apino
      RETURNING
        VALUE(subrc) TYPE sysubrc .
    METHODS check_setting
      IMPORTING
        !server      TYPE REF TO if_http_server
      RETURNING
        VALUE(subrc) TYPE sysubrc .
    METHODS set_message
      IMPORTING
        !server        TYPE REF TO if_http_server
        !msg           TYPE symsg
        !reason        TYPE string
        !code          TYPE i DEFAULT 400
      RETURNING
        VALUE(message) TYPE string .
    METHODS maintain_inbound
      IMPORTING
        !iv_req       TYPE string
        !server       TYPE REF TO if_http_server
      RETURNING
        VALUE(rv_res) TYPE string .
    METHODS store_xdata
      IMPORTING
        !str   TYPE string
        !dirio TYPE ztraf_log_data-dirio .
ENDCLASS.



CLASS ZCL_RAF_INBOUND_FUNC IMPLEMENTATION.


  METHOD check_apino.

    SELECT SINGLE
      *
      FROM ztraf_maintain
      INTO me->raf_maintain
      WHERE apino = apino
        AND dirio = 'I'    " 入向
        AND deact = ''.    " 激活
    subrc = sy-subrc.

  ENDMETHOD.


  METHOD check_setting.

    SELECT SINGLE
      *
      FROM ztraf_iconf
      INTO me->raf_iconf
      WHERE apino = me->raf_maintain-apino.
    subrc = sy-subrc.

    IF subrc <> 0.
      " 未配置 & 处理函数，请联系相关人员!
      me->set_message( server = server
        msg    = VALUE #( msgno = 005 msgv1 = me->raf_maintain-apino )
        reason = `setting error ztraf_iconf`
      ).
      RETURN.
    ENDIF.

    " 检查函数是否可用
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = me->raf_iconf-func_name
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.

    subrc = sy-subrc.

    IF subrc <> 0.
      " ERP端函数名维护有误，请联系ERP管理员检查配置表ZTAPI_IN_CONF。
      me->set_message( server = server
        msg    = VALUE #( msgno = 006 msgv1 = me->raf_maintain-apino )
        reason = `setting error func`
      ).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.

    " sicf 处理 handle

    DATA: lv_req_json TYPE string,
          lv_res_json TYPE string.
    DATA: lv_apino TYPE ztraf_log-apino.

    " 配置信息
    me->o_config = NEW #( ).

    " 检查 请求类型
    DATA(lv_method) = server->request->get_method( ).
    IF lv_method <> 'POST'.
      " 003 & is Useless Method
      me->set_message( server = server
        msg    = VALUE #( msgno = 003 msgv1 = lv_method )
        reason = `Useless Method`
      ).
      RETURN.
    ENDIF.

    DATA(lv_content_type) = server->request->if_http_entity~get_content_type( ).
    FIND `application/json` IN lv_content_type.
    IF sy-subrc <> 0.
      " 004 & is not supported! 'application/json' is required !
      me->set_message( server = server
        msg    = VALUE #( msgno = 004 msgv1 = lv_content_type )
        reason = `unsupport ContentType`
      ).
      RETURN.
    ENDIF.

    " 检查 APINO 是否配置
    IF me->o_config->get( zcl_raf_config=>c_base_apino_in_subrouter ) = 'X'.
      DATA(lv_url_sub) = server->request->get_header_field( `~path_info_expanded` ).
      SPLIT lv_url_sub AT '/'  INTO TABLE DATA(lt_url_sub).
      lv_apino = VALUE #( lt_url_sub[ 2 ] OPTIONAL ).
    ELSE.
      lv_apino = server->request->if_http_entity~get_form_field( `apino` ).
    ENDIF.
    IF me->check_apino( lv_apino ).
      " 001  & no found. 002 apino in obligatory!
      me->set_message( server = server
        msg    = VALUE #( msgno = COND #( WHEN lv_apino IS INITIAL THEN 002 ELSE 001 ) msgv1 = lv_apino )
        reason = `apino check error`
      ).
      RETURN.
    ENDIF.

    " 检查类
    CHECK me->check_setting( server = server ) = 0.

    " 获取请求数据
    lv_req_json = server->request->get_cdata( ).

    " 额外参数
    targt = me->raf_maintain-targt.

    IF me->raf_maintain-logdt = 'A'. " 记录所有
      me->store_xdata( str = lv_req_json dirio = 'I' ).
    ENDIF.

    lv_res_json = me->maintain_inbound( server = server
                                        iv_req = lv_req_json ).

    IF me->raf_maintain-logdt = 'A'. " 记录所有
      me->store_xdata( str = lv_res_json dirio = 'O' ).
    ENDIF.

    IF me->fdump = 'X'.
      CLEAR me->fdump.
      RETURN.
    ENDIF.

    " 设置接口成功调用的的状态
    server->response->set_status( code = '200' reason = 'Success' ).

    " 设置接口返回的数据
    server->response->set_content_type( `application/json` ).
    server->response->set_cdata( data = lv_res_json ).
  ENDMETHOD.


  METHOD maintain_inbound.

    DATA: ls_header TYPE header_fb,
          lt_tables TYPE rsfb_para,
          lt_import TYPE rsfb_para,
          lt_export TYPE rsfb_para,
          lt_change TYPE rsfb_para.
    DATA: lt_parameter TYPE rsfb_para.

    DATA: lo_table TYPE REF TO data.
    DATA: lt_dyn_comp TYPE cl_abap_structdescr=>component_table,
          ls_dyn_comp LIKE LINE OF lt_dyn_comp.

    DATA: lo_req_tab  TYPE REF TO data,
          lo_req_data TYPE REF TO data,
          lo_res_tab  TYPE REF TO data,
          lo_res_data TYPE REF TO data.

    DATA: lt_ptab TYPE abap_func_parmbind_tab,
          ls_ptab LIKE LINE OF lt_ptab,
          lt_etab TYPE abap_func_excpbind_tab.
    DATA: lv_kind TYPE i .

    DATA: lv_error_flag TYPE abap_bool,
          lv_message    TYPE string.

    FIELD-SYMBOLS: <lt_req_tab> TYPE ANY TABLE.
    FIELD-SYMBOLS: <lt_res_tab> TYPE ANY TABLE.
    FIELD-SYMBOLS: <ls_req_data> TYPE data.
    FIELD-SYMBOLS: <ls_res_data> TYPE data.

    " 日志开始
    IF NOT me->raf_maintain-apino = 'N'. " --> N 不记录所有
      zcl_raf_ilog=>factory(  me->raf_maintain-apino ).
    ENDIF.

    " 函数参数结构读取
    ls_header-name  = me->raf_iconf-func_name.
    ls_header-state = 'A' .

    CALL METHOD cl_fb_parameter_db=>read
      IMPORTING
        tables = lt_tables
        import = lt_import
        export = lt_export
        change = lt_change
      CHANGING
        header = ls_header.
    APPEND LINES OF lt_import TO lt_parameter.
    APPEND LINES OF lt_export TO lt_parameter.
    APPEND LINES OF lt_change TO lt_parameter.
    APPEND LINES OF lt_tables TO lt_parameter.

    " 构造传入参数对象
    LOOP AT lt_parameter INTO DATA(ls_parameter).
      IF ls_parameter-typefield = 'LIKE'.
        CREATE DATA lo_table TYPE STANDARD TABLE OF (ls_parameter-structure).

        ls_dyn_comp-name = ls_parameter-parameter.
        ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_data_ref( lo_table ).
        APPEND ls_dyn_comp TO lt_dyn_comp.
        CLEAR ls_dyn_comp.
      ELSEIF ls_parameter-typefield = 'TYPE'.
        ls_dyn_comp-name = ls_parameter-parameter.
        ls_dyn_comp-type ?= cl_abap_tabledescr=>describe_by_name( ls_parameter-structure ).
        APPEND ls_dyn_comp TO lt_dyn_comp.
        CLEAR ls_dyn_comp.
      ENDIF.
    ENDLOOP.
    IF lt_dyn_comp IS NOT INITIAL.

      DATA(lo_req_type) = cl_abap_structdescr=>create( lt_dyn_comp ).
      DATA(lo_req_tabdesc) = cl_abap_tabledescr=>create(
                      p_line_type  = lo_req_type
                      p_table_kind = cl_abap_tabledescr=>tablekind_std
                      p_unique     = abap_false ).
      CREATE DATA lo_req_tab TYPE HANDLE lo_req_tabdesc.
      ASSIGN lo_req_tab->* TO <lt_req_tab>.
      CREATE DATA lo_req_data LIKE LINE OF <lt_req_tab>.
      ASSIGN lo_req_data->* TO <ls_req_data>.

      " 传输json数据解析
      /ui2/cl_json=>deserialize(
        EXPORTING
          json = iv_req
          pretty_name = me->raf_maintain-jmode
        CHANGING
          data = <ls_req_data> ).

      LOOP AT lt_parameter INTO ls_parameter.
        CASE ls_parameter-paramtype.
          WHEN 'I'.
            lv_kind = abap_func_exporting .
          WHEN 'E'.
            lv_kind = abap_func_importing.
          WHEN 'T'.
            lv_kind = abap_func_tables.
          WHEN 'C'.
            lv_kind = abap_func_changing.
          WHEN OTHERS.
        ENDCASE.

        ASSIGN COMPONENT ls_parameter-parameter OF STRUCTURE <ls_req_data>
          TO FIELD-SYMBOL(<fs_para_data>).

        ls_ptab-name = ls_parameter-parameter.
        ls_ptab-kind = lv_kind.
        GET REFERENCE OF <fs_para_data> INTO ls_ptab-value.
        INSERT ls_ptab INTO TABLE lt_ptab.
        CLEAR ls_ptab.
      ENDLOOP.
    ENDIF.
    lt_etab = VALUE #( ( name = 'OTHERS' value = 10 ) ) .

    " 调用函数实现各自的业务逻辑
    TRY .
        CALL FUNCTION me->raf_iconf-func_name
          PARAMETER-TABLE lt_ptab
          EXCEPTION-TABLE lt_etab.
      CATCH cx_root INTO DATA(lr_root).
        " 007 ERP端API调用异常，请联系ERP管理员。
        lv_error_flag = 'X'.
        lv_message = me->set_message( server = server
            msg    = VALUE #( msgno = 007 )
            reason = `call func error or dump`
            code   = 500 " 500 DUMP
        ) && lr_root->get_text( ).

        zcl_raf_ilog=>log( msgty = 'A' msgtx = lv_message ).
        me->fdump = 'X'.

        " 日志结束
        zcl_raf_ilog=>end( ).
        zcl_raf_ilog=>free( ).

        RETURN.
    ENDTRY .

    " 构造返回参数对象.
    LOOP AT lt_parameter INTO ls_parameter WHERE paramtype = 'I'.
      DELETE lt_dyn_comp WHERE name = ls_parameter-parameter.
    ENDLOOP.

    DATA(lo_res_type) = cl_abap_structdescr=>create( lt_dyn_comp ).
    DATA(lo_res_tabdesc) = cl_abap_tabledescr=>create(
                    p_line_type  = lo_res_type
                    p_table_kind = cl_abap_tabledescr=>tablekind_std
                    p_unique     = abap_false ).
    CREATE DATA lo_res_tab TYPE HANDLE lo_res_tabdesc.
    ASSIGN lo_res_tab->* TO <lt_res_tab>.
    CREATE DATA lo_res_data LIKE LINE OF <lt_res_tab>.
    ASSIGN lo_res_data->* TO <ls_res_data>.

    " 写入值
    LOOP AT lt_ptab INTO ls_ptab.
      ASSIGN COMPONENT ls_ptab-name OF STRUCTURE <ls_res_data> TO FIELD-SYMBOL(<fs_comp>).
      IF sy-subrc = 0.
        ASSIGN ls_ptab-value->* TO <fs_para_data>.
        <fs_comp> = <fs_para_data>.
      ENDIF.
    ENDLOOP.

    rv_res = /ui2/cl_json=>serialize( data = <ls_res_data>
                               pretty_name = me->raf_maintain-jmode ) .

    " 日志结束
    zcl_raf_ilog=>end( ).
    zcl_raf_ilog=>free( ).
  ENDMETHOD.


  METHOD readme.

    " 入栈 handler

    " sicf 处理类

    " http -> sicf -> class -> func

  ENDMETHOD.


  METHOD set_message.

    DATA: BEGIN OF ls_req,
            code    TYPE i,
            message TYPE string,
          END OF ls_req.

    ls_req-code = msg-msgno.
    MESSAGE ID 'ZRAF00' TYPE 'E' NUMBER msg-msgno
      WITH msg-msgv1 msg-msgv2 msg-msgv3 msg-msgv4 INTO ls_req-message.

    server->response->set_status( code = code reason = reason ).
    server->response->set_cdata( data = /ui2/cl_json=>serialize( data = ls_req
                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case ) ).
    server->response->set_content_type( `application/json` ).

    message = ls_req-message.
  ENDMETHOD.


  METHOD store_xdata.

    DATA: BEGIN OF ls_key,
            apino TYPE ztraf_log_data-apino,
            dirio TYPE ztraf_log_data-dirio,
            dguid TYPE ztraf_log_data-dguid,
          END OF ls_key.
    DATA: ls_data TYPE ztraf_log_data.

    ls_data-apino = me->raf_maintain-apino.
    ls_data-dirio = dirio.

    IF zcl_raf_inbound_func=>dguid IS INITIAL.
      TRY.
          ls_data-dguid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error.
          ls_data-dguid = sy-datum && sy-uzeit.
      ENDTRY.
      zcl_raf_inbound_func=>dguid = ls_data-dguid.
    ELSE.
      ls_data-dguid = zcl_raf_inbound_func=>dguid.
    ENDIF.

    ls_data-userid = sy-uname.
    GET TIME STAMP FIELD ls_data-timestamp.

    MOVE-CORRESPONDING ls_data TO ls_key.

    EXPORT str = str TO DATABASE ztraf_log_data(zz) FROM ls_data ID ls_key.
    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
