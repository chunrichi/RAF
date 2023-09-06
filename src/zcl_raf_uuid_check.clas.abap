CLASS zcl_raf_uuid_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS check_lock
      IMPORTING
        !objectclas TYPE ztraf_ikey-objectclas
        !uuid       TYPE ztraf_ikey-uuid
      EXPORTING
        !created    TYPE ztraf_ikey
        !return     TYPE bapiret2 .
    METHODS update_unlock
      IMPORTING
        !objectclas TYPE ztraf_ikey-objectclas
        !uuid       TYPE ztraf_ikey-uuid
        !objectid   TYPE ztraf_ikey-objectid
      EXPORTING
        !return     TYPE bapiret2 .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_varkey,
             mandt      TYPE ztraf_ikey-mandt,
             objectclas TYPE ztraf_ikey-objectclas,
             uuid       TYPE ztraf_ikey-uuid,
           END OF ty_varkey.
ENDCLASS.



CLASS ZCL_RAF_UUID_CHECK IMPLEMENTATION.


  METHOD check_lock.
    DATA: lv_varkey TYPE rstable-varkey,
          ls_varkey TYPE ty_varkey.

    " 程序加锁

    IF objectclas IS NOT INITIAL AND uuid IS NOT INITIAL.

      ls_varkey = VALUE #( mandt = sy-mandt
                           objectclas = objectclas
                           uuid  = uuid ).
      lv_varkey = ls_varkey.

      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          mode_rstable   = 'E'
          tabname        = 'ZTRAF_IKEY'
          varkey         = lv_varkey
          _scope         = '1' " 1 进程 2 更新 3 进程和更新最后一个
          _wait          = ' '
          _collect       = ' '
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        return-type = 'E'.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO return-message.
      ELSE.
        SELECT SINGLE
          *
          FROM ztraf_ikey
          INTO created
          WHERE objectclas = objectclas
            AND uuid = uuid.
        IF sy-subrc = 0.
          return-type = 'W'.
          CONCATENATE objectclas uuid '已经创建对象' created-objectid INTO return-message.

          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = 'E'
              tabname      = 'ZTRAF_IKEY'
              varkey       = lv_varkey
              x_tabname    = ' '
              x_varkey     = ' '
              _scope       = '3'
              _synchron    = ' '
              _collect     = ' '.

        ELSE.
          return-type = 'S'.
          CONCATENATE objectclas uuid '锁定成功' INTO return-message.
        ENDIF.
      ENDIF.

    ELSE.
      return-type = 'E'.
      return-message = TEXT-001.
    ENDIF.
  ENDMETHOD.


  METHOD update_unlock.
    DATA: ls_ztraf_ikey TYPE ztraf_ikey.
    DATA: lv_varkey TYPE rstable-varkey,
          ls_varkey TYPE ty_varkey.

    IF objectclas IS NOT INITIAL AND uuid IS NOT INITIAL.


      IF objectclas IS NOT INITIAL.

        ls_ztraf_ikey-objectclas = objectclas.
        ls_ztraf_ikey-uuid = uuid.
        ls_ztraf_ikey-objectid = objectid.
        ls_ztraf_ikey-udate = sy-datum.
        ls_ztraf_ikey-utime = sy-uzeit.
        ls_ztraf_ikey-uname = sy-uname.

        GET TIME STAMP FIELD ls_ztraf_ikey-timestamp.

        MODIFY ztraf_ikey FROM ls_ztraf_ikey.

      ENDIF.

      ls_varkey = VALUE #( mandt = sy-mandt
                           objectclas = objectclas
                           uuid  = uuid ).
      lv_varkey = ls_varkey.

      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          mode_rstable = 'E'
          tabname      = 'ZTRAF_IKEY'
          varkey       = lv_varkey
          x_tabname    = ' '
          x_varkey     = ' '
          _scope       = '3'
          _synchron    = ' '
          _collect     = ' '.

      return-type = 'S'.
      CONCATENATE objectclas uuid TEXT-005 INTO return-message.

    ELSE.

      return-type = 'E'.
      return-message = TEXT-001.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
