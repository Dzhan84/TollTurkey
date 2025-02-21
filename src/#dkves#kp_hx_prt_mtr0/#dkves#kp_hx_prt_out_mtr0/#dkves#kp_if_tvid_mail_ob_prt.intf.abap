INTERFACE /dkves/kp_if_tvid_mail_ob_prt
  PUBLIC .

  METHODS register_email
    IMPORTING iv_customer  TYPE kunnr
              iv_order_nr  TYPE  /dkves/kp_ordernr
    RETURNING VALUE(rt_response) TYPE bapiret2_t .


ENDINTERFACE.
