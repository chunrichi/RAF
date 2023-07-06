CLASS zcl_raf_ilog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA ilog TYPE REF TO zcl_raf_ilog .
    DATA ztraf_log TYPE ztraf_log .

    METHODS constructor
      IMPORTING
        !apino TYPE ztraf_log-apino .
    CLASS-METHODS factory
      IMPORTING
        !apino       TYPE ztraf_log-apino
      RETURNING
        VALUE(subrc) TYPE sysubrc .
    METHODS store .
    CLASS-METHODS log
      IMPORTING
        !msgty TYPE any
        !msgtx TYPE any
        !bskey TYPE any OPTIONAL .
    CLASS-METHODS end .
    CLASS-METHODS free .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_RAF_ILOG IMPLEMENTATION.


  METHOD constructor.

    me->ztraf_log-apino = apino.

  ENDMETHOD.


  METHOD end.

    IF ilog IS NOT INITIAL.
      ilog->store( ).
    ENDIF.

  ENDMETHOD.


  METHOD factory.
    " 工厂

    IF ilog IS INITIAL.
      ilog = NEW zcl_raf_ilog( apino = apino ).

      ilog->store( ).
    ENDIF.

  ENDMETHOD.


  METHOD free.

    FREE ilog.

    " CL_ABAP_MEMORY_UTILITIES=>DO_GARBAGE_COLLECTION( ).

  ENDMETHOD.


  METHOD log.

    IF ilog IS NOT INITIAL.
      ilog->ztraf_log-msgty = msgty.
      ilog->ztraf_log-msgtx = msgtx.
      ilog->ztraf_log-bskey = bskey.
    ENDIF.

  ENDMETHOD.


  METHOD store.

    IF me->ztraf_log-logid IS INITIAL.

      " 请求记录 UUID
      TRY.
          me->ztraf_log-logid = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
        CATCH cx_uuid_error.
          me->ztraf_log-logid = sy-datum && sy-uzeit.
      ENDTRY.

      me->ztraf_log-ifpos = 1.
      me->ztraf_log-dirio = 'I'.
      me->ztraf_log-targt = zcl_raf_inbound_func=>targt.

      GET TIME.
      me->ztraf_log-sdate = sy-datum.
      me->ztraf_log-stime = sy-uzeit.
      GET TIME STAMP FIELD me->ztraf_log-b_stamp_l.

      me->ztraf_log-dguid = zcl_raf_inbound_func=>dguid.
      me->ztraf_log-uname = sy-uname.

      MODIFY ztraf_log FROM me->ztraf_log.
      COMMIT WORK.
    ELSE.
      GET TIME STAMP FIELD me->ztraf_log-e_stamp_l.

      IF me->ztraf_log-msgty IS INITIAL.
        me->ztraf_log-msgty = 'I'.
      ENDIF.

      UPDATE ztraf_log SET msgty = me->ztraf_log-msgty
                           msgtx = me->ztraf_log-msgtx
                           bskey = me->ztraf_log-bskey        " 业务关键字
                           e_stamp_l = me->ztraf_log-e_stamp_l
                     WHERE logid = me->ztraf_log-logid
                       AND ifpos = 1
                       AND apino = me->ztraf_log-apino.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
