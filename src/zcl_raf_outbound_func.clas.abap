CLASS zcl_raf_outbound_func DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_raf_outbound .

    CLASS-METHODS factory
      IMPORTING
        !apino         TYPE ztraf_maintain-apino
      RETURNING
        VALUE(handler) TYPE REF TO zif_raf_outbound .
  PROTECTED SECTION.

    METHODS process_main
      IMPORTING
        !i_data TYPE REF TO data
      EXPORTING
        !e_data TYPE REF TO data .
  PRIVATE SECTION.

    CLASS-METHODS readme .
ENDCLASS.



CLASS ZCL_RAF_OUTBOUND_FUNC IMPLEMENTATION.


  METHOD factory.

    " 检查基础配置
    SELECT SINGLE
      *
      FROM ztraf_maintain
      WHERE apino = @apino
        AND dirio = 'O'
        AND deact = ''
      INTO @DATA(ls_maintain).

    CHECK sy-subrc = 0.

    " 检查URL配置
    SELECT SINGLE
      sysid,
      url
      FROM ztraf_url_base
      WHERE targt = @ls_maintain-targt
        AND sysid = @sy-sysid
      INTO @DATA(ls_url).

    CHECK ls_url IS NOT INITIAL.

    " 检查处理类
    SELECT SINGLE
      *
      FROM ztraf_oconf
      WHERE apino = @apino
      INTO @DATA(ls_oconf).

    IF ls_oconf-class_name IS NOT INITIAL.
      CREATE OBJECT handler TYPE (ls_oconf-class_name).
      IF handler IS BOUND.
        handler->maintain_info = ls_maintain.
        handler->oconf_info = ls_oconf.
        handler->base_url = ls_url-url.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD process_main.

    " HTTP 请求

    DATA: lv_url TYPE string.
    DATA: lr_http_client TYPE REF TO if_http_client.

    DATA: lv_debug     TYPE abap_bool,
          lv_error_msg TYPE string.

    DATA: lv_status_code TYPE i,
          lv_reason      TYPE string.

    DATA: lv_req_json TYPE string,
          lv_res_json TYPE string.

    DATA: lv_subrc TYPE sysubrc.

    " 返回消息初始化
    me->zif_raf_outbound~result = VALUE #( type = 'S' message = `http request success` ).

    " 日志时戳初始化
    IF me->zif_raf_outbound~maintain_info-logdt <> ''.
      zcl_raf_olog=>create_send_log( me->zif_raf_outbound~maintain_info-apino
        )->begin_flag( ).
    ENDIF.

    " data2json
    lv_req_json = /ui2/cl_json=>serialize( data = i_data
                                           pretty_name = me->zif_raf_outbound~maintain_info-jmode ).

    RAISE EVENT zif_raf_outbound~after_abap2json EXPORTING er_json = REF #( lv_req_json ).

    " 日志
    IF me->zif_raf_outbound~maintain_info-logdt = 'A'.
      " 记录所有日志
      zcl_raf_olog=>store_data( apino = me->zif_raf_outbound~maintain_info-apino
                                dirio = 'I'
                                data  = lv_req_json ).
    ENDIF.

    " url
    lv_url = me->zif_raf_outbound~base_url && me->zif_raf_outbound~oconf_info-url.

    " 请求初始化
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lr_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 7.
    IF sy-subrc <> 0.
      me->zif_raf_outbound~result-type = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO me->zif_raf_outbound~result-message.
      RETURN.
    ENDIF.

    " 设置http协议版本
    " lr_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

    " 设置http请求方法
    lr_http_client->request->set_method( CONV #( me->zif_raf_outbound~oconf_info-method ) ).

    " 头部信息 设定传输请求内容格式以及编码格式
    lr_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).

    " 设置报文
    lr_http_client->request->set_cdata( data = lv_req_json ).

    " 设置请求参数（请求参数也可以直接写在URI）
    "lr_http_client->request->set_form_field( name = 'grant_type' value = 'password' ).
    "lr_http_client->request->set_form_field( name = 'username' value = lv_username ).
    "lr_http_client->request->set_form_field( name = 'password' value = lv_password ).

    " 认证方式
    CALL METHOD lr_http_client->authenticate
      EXPORTING
        username = 'admin'
        password = 'admin!'.

    CALL METHOD lr_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      lr_http_client->get_last_error(
        IMPORTING
          code    = lv_subrc
          message = lv_error_msg
      ).
      me->zif_raf_outbound~result = VALUE #( type = 'E' message = |{ lv_error_msg }| ).
      RETURN.

      me->zif_raf_outbound~result-type = 'E'.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO me->zif_raf_outbound~result-message.
      RETURN.
    ENDIF.

    IF lv_debug = 'X'.
      DATA(lv_request_xstring) = lr_http_client->request->to_xstring( ).
    ENDIF.

    CALL METHOD lr_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      lr_http_client->get_last_error(
        IMPORTING
          code    = lv_subrc
          message = lv_error_msg
      ).
      me->zif_raf_outbound~result = VALUE #( type = 'E' message = |{ lv_error_msg }| ).
      RETURN.
    ENDIF.

    IF lv_debug = 'X'.
      DATA(lv_response_xstring) = lr_http_client->response->to_xstring( ).
    ENDIF.

    lv_res_json = lr_http_client->response->get_cdata( ).

    " 日志
    IF me->zif_raf_outbound~maintain_info-logdt = 'A'.
      " 记录所有日志
      zcl_raf_olog=>store_data( apino = me->zif_raf_outbound~maintain_info-apino
                                dirio = 'O'
                                data  = lv_res_json ).
    ENDIF.

    lr_http_client->response->get_status( IMPORTING code = lv_status_code reason = lv_reason ).
    IF lv_status_code <> 200.
      me->zif_raf_outbound~result = VALUE #( type = 'E' message = |{ lv_status_code } { lv_reason }| ).
    ENDIF.

    " json2abap
    ASSIGN e_data TO FIELD-SYMBOL(<ls_e_data>).
    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_res_json
        pretty_name = me->zif_raf_outbound~maintain_info-jmode
      CHANGING
        data = <ls_e_data> ) .

  ENDMETHOD.


  METHOD readme.

    " 通过函数进行数据向外发送

    " 本类包含演示示例

    " 实现流程 -> 新增对象系统实现流程
    " 1. 创建一个类，添加接口 ZIF_RAF_OUTBOUND
    " 2. 实现 PROCESS 方法 => 可参考本程序
    " 3. 配置 ZTRAF_MAINTAIN
    " 4. 配置 ZTRAF_OCONF

  ENDMETHOD.


  METHOD zif_raf_outbound~request.

    " 实际实施的时候可将此处直接复制
    " 调整逻辑拷贝 PROCESS_MAIN 逻辑 进行调整

    zcl_raf_olog=>log_in_clas = 'X'.

    process_main( EXPORTING i_data = i_data
                  IMPORTING e_data = e_data ).

    " 处理异常
    IF me->zif_raf_outbound~maintain_info-logdt <> ''.
      zcl_raf_olog=>create_send_log( me->zif_raf_outbound~maintain_info-apino
        )->log( i_msgty = me->zif_raf_outbound~result-type
                i_msgtx = me->zif_raf_outbound~result-message ).
    ENDIF.

    CLEAR zcl_raf_olog=>dguid.
    CLEAR zcl_raf_olog=>log_in_clas.

  ENDMETHOD.
ENDCLASS.
