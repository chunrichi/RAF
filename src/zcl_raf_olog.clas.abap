CLASS zcl_raf_olog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA osend TYPE REF TO zcl_raf_olog .
    DATA apino TYPE ztraf_log-apino READ-ONLY .
    DATA ifpos TYPE ztraf_log-ifpos READ-ONLY .
    DATA logid TYPE ztraf_log-logid READ-ONLY .
    CLASS-DATA dguid TYPE ztraf_log-dguid .
    DATA ibtimestampl TYPE timestampl .
    DATA progname TYPE progname READ-ONLY .
    CLASS-DATA log_in_clas TYPE rs_bool .

    METHODS constructor
      IMPORTING
        !apino TYPE ztraf_log-apino .
    CLASS-METHODS create_send_log
      IMPORTING
        !apino      TYPE ztraf_log-apino
      RETURNING
        VALUE(rval) TYPE REF TO zcl_raf_olog .
    METHODS log
      IMPORTING
        !i_msgty     TYPE ztraf_log-msgty OPTIONAL
        !i_msgtx     TYPE ztraf_log-msgtx OPTIONAL
        !i_bskey     TYPE ztraf_log-bskey OPTIONAL
        !i_progname  TYPE progname OPTIONAL
        !i_no_commit TYPE abap_bool DEFAULT '' .
    METHODS end
      IMPORTING
        !i_msgty TYPE ztraf_log-msgty
        !i_bskey TYPE ztraf_log-bskey .
    CLASS-METHODS store_data
      IMPORTING
        !apino TYPE ztraf_log-apino
        !dirio TYPE ztraf_log-dirio
        !data  TYPE data .
    CLASS-METHODS free .
    METHODS begin_flag .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_RAF_OLOG IMPLEMENTATION.


  METHOD begin_flag.

    GET TIME STAMP FIELD me->ibtimestampl.

  ENDMETHOD.


  METHOD constructor.

    me->apino = apino.

  ENDMETHOD.


  METHOD create_send_log.

    IF osend IS INITIAL.
      osend = NEW #( apino ).
    ENDIF.

    rval = osend.

  ENDMETHOD.


  METHOD end.
    DATA: lv_timestampl TYPE timestampl.

    GET TIME STAMP FIELD lv_timestampl.

    UPDATE ztraf_log SET msgty = i_msgty
                         bskey = i_bskey
                         e_stamp_l = lv_timestampl
                   WHERE logid = me->dguid
                     AND ifpos = 1.
    COMMIT WORK.

  ENDMETHOD.


  METHOD free.

    FREE osend.

    " CL_ABAP_MEMORY_UTILITIES=>DO_GARBAGE_COLLECTION( ).

  ENDMETHOD.


  METHOD log.


    " note: 记录 发送日志
    " 局部 变量都替换为了实例属性

    DATA: ls_log TYPE ztraf_log.
    DATA: lv_timestampl TYPE timestampl.

    IF log_in_clas = ''.
      UPDATE ztraf_log SET msgty = i_msgty
                           msgtx = i_msgtx
                           bskey = i_bskey WHERE logid = me->logid
                                             AND ifpos = me->ifpos
                                             AND apino = me->apino.

      IF i_no_commit = abap_false.
        COMMIT WORK.
      ENDIF.

      RETURN.
    ENDIF.

    GET TIME STAMP FIELD lv_timestampl.
    GET TIME.

    " >> 行号区分
    IF me->ifpos = 0.
      me->ifpos = 1.
    ELSE.
      ADD 1 TO me->ifpos.
    ENDIF.

    " >> 生成 UUID
    IF me->logid IS INITIAL.
      TRY.
          me->logid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
        CATCH cx_uuid_error.
          me->logid = sy-datum && sy-uzeit.
      ENDTRY.
    ENDIF.

    " > 存表设置

    " 传入值设置
    ls_log-apino  = me->apino.                " 接口ID
    IF i_progname IS NOT INITIAL.
      me->progname = i_progname.              " ABAP 程序名
    ENDIF.
    ls_log-progname = me->progname.
    ls_log-msgty  = i_msgty.
    ls_log-msgtx  = i_msgtx.
    ls_log-bskey  = i_bskey.

    ls_log-dguid  = zcl_raf_olog=>dguid.     " 报文ID

    ls_log-logid  = me->logid.               " 日志ID
    ls_log-ifpos  = me->ifpos.               " 日志行号

    ls_log-dirio  = 'O'.
    ls_log-sdate  = sy-datum.
    ls_log-stime  = sy-uzeit.
    ls_log-uname  = sy-uname.                " 用户名

    IF me->ibtimestampl IS NOT INITIAL.
      " 有开始时戳时
      ls_log-b_stamp_l = me->ibtimestampl.
      ls_log-e_stamp_l = lv_timestampl.
    ELSE.
      ls_log-b_stamp_l = lv_timestampl.
    ENDIF.

    MODIFY ztraf_log FROM ls_log.

    " > 及时提交
    IF i_no_commit = abap_false.
      COMMIT WORK.
    ENDIF.

    CLEAR me->ibtimestampl.

  ENDMETHOD.


  METHOD store_data.

    DATA: BEGIN OF ls_key,
            apino TYPE ztraf_log_data-apino,
            dirio TYPE ztraf_log_data-dirio,
            dguid TYPE ztraf_log_data-dguid,
          END OF ls_key.
    DATA: ls_data TYPE ztraf_log_data.

    ls_data-apino = apino.
    ls_data-dirio = dirio.

    IF zcl_raf_olog=>dguid IS INITIAL.
      TRY.
          ls_data-dguid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error.
          ls_data-dguid = sy-datum && sy-uzeit.
      ENDTRY.
      zcl_raf_olog=>dguid = ls_data-dguid.
    ELSE.
      ls_data-dguid = zcl_raf_olog=>dguid.
    ENDIF.

    ls_data-userid = sy-uname.
    GET TIME STAMP FIELD ls_data-timestamp.

    MOVE-CORRESPONDING ls_data TO ls_key.

    EXPORT str = data TO DATABASE ztraf_log_data(zz) FROM ls_data ID ls_key.
    COMMIT WORK.

  ENDMETHOD.
ENDCLASS.
