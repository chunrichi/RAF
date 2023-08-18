CLASS zcl_raf_send_key DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_crange,
        send  TYPE ztraf_okey-sendflag,
        range TYPE RANGE OF ztraf_okey-objectid,
      END OF ty_crange .
    TYPES:
      tty_cranges TYPE TABLE OF ty_crange .
    TYPES: tty_ztraf_okey TYPE TABLE OF ztraf_okey.

    DATA: logkeys TYPE tty_ztraf_okey.

    METHODS save IMPORTING
                   !send_flag TYPE ztraf_okey-sendflag DEFAULT 'S'
                   !send_guid TYPE ztraf_okey-senduuid OPTIONAL .
    METHODS add
      IMPORTING
        VALUE(data) TYPE ztraf_okey .
    METHODS change IMPORTING !crange TYPE tty_cranges OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_RAF_SEND_KEY IMPLEMENTATION.


  METHOD add.


    DATA: ls_ztraf_okey TYPE ztraf_okey.

    MOVE data TO ls_ztraf_okey.

    IF ls_ztraf_okey-uname IS INITIAL. ls_ztraf_okey-uname = sy-uname. ENDIF.
    IF ls_ztraf_okey-timestamp IS INITIAL. GET TIME STAMP FIELD ls_ztraf_okey-timestamp. ENDIF.

    APPEND ls_ztraf_okey TO me->logkeys.

  ENDMETHOD.


  METHOD change.

    IF crange IS SUPPLIED.

      LOOP AT crange INTO DATA(ls_crange).

        MODIFY me->logkeys FROM VALUE #( sendflag = ls_crange-send )
          TRANSPORTING sendflag
            WHERE objectid IN ls_crange-range.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD save.

    IF send_flag IS SUPPLIED.
      MODIFY me->logkeys FROM VALUE #( sendflag = send_flag ) TRANSPORTING sendflag WHERE sendflag IS INITIAL.
    ENDIF.

    IF send_guid IS SUPPLIED.
      MODIFY me->logkeys FROM VALUE #( senduuid = send_guid ) TRANSPORTING senduuid WHERE senduuid IS INITIAL.
    ENDIF.

    MODIFY ztraf_okey CONNECTION r/3*zapi_log_key FROM TABLE me->logkeys.

    COMMIT CONNECTION r/3*zapi_log_key.

    REFRESH me->logkeys.

  ENDMETHOD.
ENDCLASS.
