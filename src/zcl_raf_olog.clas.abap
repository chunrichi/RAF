class ZCL_RAF_OLOG definition
  public
  final
  create public .

public section.

  class-data OSEND type ref to ZCL_RAF_OLOG .
  data APINO type ZTRAF_LOG-APINO read-only .
  data IFPOS type ZTRAF_LOG-IFPOS read-only .
  data LOGID type ZTRAF_LOG-LOGID read-only .
  class-data DGUID type ZTRAF_LOG-DGUID .
  data IBTIMESTAMPL type TIMESTAMPL .
  data PROGNAME type PROGNAME read-only .

  methods CONSTRUCTOR
    importing
      !APINO type ZTRAF_LOG-APINO .
  class-methods CREATE_SEND_LOG
    importing
      !APINO type ZTRAF_LOG-APINO
    returning
      value(RVAL) type ref to ZCL_RAF_OLOG .
  methods LOG
    importing
      !I_MSGTY type ZTRAF_LOG-MSGTY optional
      !I_MSGTX type ZTRAF_LOG-MSGTX optional
      !I_BSKEY type ZTRAF_LOG-BSKEY optional
      !I_PROGNAME type PROGNAME optional
      !I_NO_COMMIT type ABAP_BOOL default '' .
  methods END
    importing
      !I_MSGTY type ZTRAF_LOG-MSGTY
      !I_BSKEY type ZTRAF_LOG-BSKEY .
  class-methods STORE_DATA
    importing
      !APINO type ZTRAF_LOG-APINO
      !DIRIO type ZTRAF_LOG-DIRIO
      !DATA type DATA .
  class-methods FREE .
  methods BEGIN_FLAG .
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
