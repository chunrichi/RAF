FUNCTION zfm_raf00_in_demo.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(TEST) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(MESSAGE) TYPE  STRING
*"     REFERENCE(TIMESTAMP) TYPE  TIMESTAMP
*"     REFERENCE(DATE) TYPE  DATUM
*"     REFERENCE(TIME) TYPE  UZEIT
*"     REFERENCE(RESULT) TYPE  SFLIGHT
*"----------------------------------------------------------------------

  " 字符串
  message = `入栈测试` && test.

  " 数字
  GET TIME STAMP FIELD timestamp.

  " 日期 & 时间
  CONVERT TIME STAMP timestamp TIME ZONE sy-zonlo
    INTO DATE date TIME time.

  SELECT SINGLE * FROM sflight INTO result.

  zcl_raf_ilog=>log( msgty = 'S'
                     msgtx = '入栈测试'
                     bskey = 'test:' && test ).

ENDFUNCTION.
