CLASS zcl_raf_outbound_dirt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_raf_outbound .

    TYPES:
      BEGIN OF ty_key_value,
        key   TYPE string,
        value TYPE string,
      END OF ty_key_value .
    TYPES:
      tty_key_value TYPE TABLE OF ty_key_value .

    METHODS constructor
      IMPORTING
        !apino   TYPE ztraf_log-apino
        !url     TYPE ztraf_oconf-url OPTIONAL
        !method  TYPE ztraf_oconf-method DEFAULT 'POST'
        !headers TYPE tty_key_value OPTIONAL
        !params  TYPE tty_key_value OPTIONAL
        !jmode   TYPE ztraf_maintain-jmode OPTIONAL .
  PROTECTED SECTION.

    METHODS process
      IMPORTING
        !i_data TYPE data
      EXPORTING
        !e_data TYPE data .
  PRIVATE SECTION.

    ALIASES base_url
      FOR zif_raf_outbound~base_url .
    ALIASES maintain_info
      FOR zif_raf_outbound~maintain_info .
    ALIASES oconf_info
      FOR zif_raf_outbound~oconf_info .
    ALIASES result
      FOR zif_raf_outbound~result .

    DATA headers TYPE tty_key_value .
    DATA params TYPE tty_key_value .
    DATA url TYPE string .

    CLASS-METHODS readme .
ENDCLASS.



CLASS ZCL_RAF_OUTBOUND_DIRT IMPLEMENTATION.


  METHOD constructor.

    " 检查基础配置
    SELECT SINGLE
      *
      FROM ztraf_maintain
      WHERE apino = @apino
        AND dirio = 'O'
        AND deact = ''
      INTO @DATA(ls_maintain).
    IF sy-subrc <> 0.
      " 允许找不到
      me->maintain_info = VALUE #( apino = COND #( WHEN apino IS NOT INITIAL THEN apino ELSE sy-cprog )
                                   logdt = 'A'
                                   jmode = jmode ).

      me->zif_raf_outbound~oconf_info-method = method.

      me->url = url.
    ELSE.
      me->maintain_info = ls_maintain.

      SELECT SINGLE
        *
        FROM ztraf_oconf
        WHERE apino = @apino
        INTO @me->zif_raf_outbound~oconf_info.

      SELECT SINGLE
        sysid,
        url
        FROM ztraf_url_base
        WHERE targt = @ls_maintain-targt
          AND sysid = @sy-sysid
        INTO @DATA(ls_url).

      " 固定连接 拼接 常用连接
      me->url = ls_url-url && me->zif_raf_outbound~oconf_info-url.
    ENDIF.

    me->params = params.
    me->headers = headers.

  ENDMETHOD.


  METHOD process.

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
    me->result = VALUE #( type = 'S' message = `http request success` ).

    " 日志时戳初始化
    IF me->zif_raf_outbound~maintain_info-logdt <> ''.
      zcl_raf_olog=>create_send_log( me->maintain_info-apino
        )->begin_flag( ).
    ENDIF.

    " data2json
    lv_req_json = /ui2/cl_json=>serialize( data = i_data
                                           pretty_name = me->zif_raf_outbound~maintain_info-jmode ).

    " 日志
    IF me->zif_raf_outbound~maintain_info-logdt = 'A'.
      " 记录所有日志
      zcl_raf_olog=>store_data( apino = me->zif_raf_outbound~maintain_info-apino
                                dirio = 'I'
                                data  = lv_req_json ).
    ENDIF.

    " url
    lv_url = me->url.

    IF me->params IS NOT INITIAL.

      LOOP AT params INTO DATA(ls_params).
        cl_http_server=>append_field_url( EXPORTING name = ls_params-key value = ls_params-value CHANGING url = lv_url ).
      ENDLOOP.

    ENDIF.

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

    " 头部信息 设定传输请求内容格式以及编码格式
    IF headers IS NOT INITIAL.

      LOOP AT headers INTO DATA(ls_headers).
        lr_http_client->request->set_header_field( name = ls_headers-key value = ls_headers-value ).
      ENDLOOP.

    ENDIF.

    " 设置报文
    lr_http_client->request->set_cdata( data = lv_req_json ).

    " 设置请求参数（请求参数也可以直接写在URI）
    "lr_http_client->request->set_form_field( name = 'grant_type' value = 'password' ).
    "lr_http_client->request->set_form_field( name = 'username' value = lv_username ).
    "lr_http_client->request->set_form_field( name = 'password' value = lv_password ).

    " 认证方式
    " CALL METHOD lr_http_client->authenticate
    "   EXPORTING
    "     username = 'admin'
    "     password = 'admin!'.

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
      me->result = VALUE #( type = 'E' message = |{ lv_error_msg }| ).
      RETURN.

      me->result-type = 'E'.
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
      me->result = VALUE #( type = 'E' message = |{ lv_error_msg }| ).
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
      me->result = VALUE #( type = 'E' message = |{ lv_status_code } { lv_reason }| ).
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

    "

  ENDMETHOD.


  METHOD zif_raf_outbound~request.

    zcl_raf_olog=>log_in_clas = 'X'.

    process( EXPORTING i_data = i_data
             IMPORTING e_data = e_data ).

    " 处理异常
    IF me->maintain_info-logdt <> ''.
      zcl_raf_olog=>create_send_log( me->maintain_info-apino
        )->log( i_msgty = me->result-type
                i_msgtx = me->result-message ).
    ENDIF.

    CLEAR zcl_raf_olog=>dguid.
    CLEAR zcl_raf_olog=>log_in_clas.

  ENDMETHOD.
ENDCLASS.
