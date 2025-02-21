CLASS /dkves/kp_cl_tvid_rest_ib_adp DEFINITION
  PUBLIC
  INHERITING FROM /dkves/ue_cl_hx_comp_base
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS /dkves/ue_if_hx_comp_creator .

  PUBLIC SECTION.

    INTERFACES /dkves/ue_if_hx_ib_adapter .
    INTERFACES /dkves/kp_if_tvid_rest_ib_adp .

    CONSTANTS:
      BEGIN OF c_status,
        accepted      TYPE string VALUE 'Accepted',         "#EC NOTEXT
        not_accepted  TYPE string VALUE 'Not Accepted',     "#EC NOTEXT
        part_accepted TYPE string VALUE 'Partially Accepted', "#EC NOTEXT
        forbidden     TYPE string VALUE 'Forbidden',        "#EC NOTEXT
      END OF c_status .
  PRIVATE SECTION.

    DATA mo_use_case     TYPE REF TO /dkves/ue_cl_hx_use_case_base.
    DATA mo_order_uc     TYPE REF TO /dkves/kp_if_tvid_order_ib_prt.
    DATA mo_terminate_uc TYPE REF TO /dkves/kp_if_tvid_term_ib_prt.

    METHODS add_ord_accepted_req_status
      IMPORTING iv_customer   TYPE kunnr
                iv_lpn        TYPE /dkves/kp_kfzkennzeichen
                iv_acpt_date  TYPE /dkves/sm_rest_utc_req_inb_dt
      CHANGING  xt_req_status TYPE /dkves/sm_mtr_video_ordr_rqs_t.

    METHODS add_ord_rejected_req_ststatus
      IMPORTING iv_customer   TYPE kunnr
                iv_lpn        TYPE /dkves/kp_kfzkennzeichen
                is_error      TYPE bapiret2
      CHANGING  xt_req_status TYPE /dkves/sm_mtr_video_ordr_rqs_t.

    METHODS add_term_accepted_req_status
      IMPORTING iv_customer   TYPE kunnr
                iv_equnr      TYPE equnr
                iv_acpt_date  TYPE /dkves/sm_rest_utc_req_inb_dt OPTIONAL
      CHANGING  xt_req_status TYPE /dkves/sm_mtr_video_term_rqs_t.

    METHODS add_term_rejected_req_status
      IMPORTING iv_customer   TYPE kunnr
                iv_equnr      TYPE equnr
                is_error      TYPE bapiret2
      CHANGING  xt_req_status TYPE /dkves/sm_mtr_video_term_rqs_t.

    METHODS handle_order_acceptance
      IMPORTING
        is_booking_request TYPE /dkves/kp_mtr_video_order_rq
        is_order_checked   TYPE /dkves/kp_mtr_vid_toll_order_s
        it_errors          TYPE bapirettab
      RETURNING
        VALUE(rs_response) TYPE /dkves/kp_mtr_video_order_rs.

    METHODS handle_total_order_rejection
      IMPORTING
        is_booking_request TYPE /dkves/kp_mtr_video_order_rq
        it_errors          TYPE bapirettab
      RETURNING
        VALUE(rs_response) TYPE /dkves/kp_mtr_video_order_rs.

    METHODS handle_total_term_rejection
      IMPORTING
        is_term_request    TYPE  /dkves/kp_mtr_video_term_rq
        it_errors          TYPE bapirettab
      RETURNING
        VALUE(rs_response) TYPE /dkves/kp_mtr_video_term_rs.

    METHODS handle_total_term_acceptance
      IMPORTING
        is_term_request    TYPE  /dkves/kp_mtr_video_term_rq
        is_term_checked    TYPE  /dkves/kp_mtr_vid_toll_term_s
        it_errors          TYPE bapirettab
      RETURNING
        VALUE(rs_response) TYPE /dkves/kp_mtr_video_term_rs .

ENDCLASS.



CLASS /DKVES/KP_CL_TVID_REST_IB_ADP IMPLEMENTATION.


  METHOD /dkves/kp_if_tvid_rest_ib_adp~schedule_video_tolling_booking.

    /dkves/ue_cl_breakpointer=>break_point( /dkves/if_aab_id_name=>c_toll_turkey ).

    "Downcast to specific Use Case
    mo_order_uc ?= mo_use_case.

    "REST Request ist fehlerhaft bzw. leer
    IF is_booking_request IS INITIAL.
      MESSAGE e008.
    ENDIF.

    "Map from REST Request to Business Structure
    DATA(ls_input) = VALUE /dkves/kp_mtr_vid_toll_order_s(
        customer_number               = is_booking_request-customer_number
        email_address                 = is_booking_request-email_address
        email_language                = is_booking_request-email_language
        video_tolling_order_items     = is_booking_request-video_tolling_turkey_requests
    ).



    "Lock customer against another request until this one is finished
    IF NOT /dkves/ue_cl_process_lock=>is_process_locked_for_customer( is_booking_request-customer_number  ).

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Call Use Case Methode for Order Scheduling
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      mo_order_uc->schedule_video_tolling_order(
        EXPORTING is_order_req     = ls_input
        IMPORTING es_order_checked = DATA(ls_order_checked) ).

      "Get the errors from log
      DATA(lt_errors) =  mo_logger->export_to_table( ).

      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "Evaluate Response - ES_ORDER_CHECKED contains only accepted Items
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      " Process results based on acceptance status
      IF ls_order_checked-video_tolling_order_items IS INITIAL.
        rs_response = handle_total_order_rejection(
          is_booking_request = is_booking_request
          it_errors          = lt_errors ).
      ELSE.
        rs_response = handle_order_acceptance(
          is_booking_request = is_booking_request
          is_order_checked   = ls_order_checked
          it_errors          = lt_errors ).
      ENDIF.

    ELSE.
      MESSAGE e709 WITH is_booking_request-customer_number
             INTO DATA(lv_msg).
      rs_response = handle_total_order_rejection(
          is_booking_request = is_booking_request
          it_errors = VALUE #( ( id         = sy-msgid
                                 type       = sy-msgty
                                 number     = sy-msgno
                                 message_v1 = sy-msgv1
                                 message    = lv_msg ) ) ).
    ENDIF.
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_rest_ib_adp~schedule_video_tolling_term.
    /dkves/ue_cl_breakpointer=>break_point( /dkves/if_aab_id_name=>c_toll_turkey ).

    "Downcast to specific Use Case
    mo_terminate_uc ?= mo_use_case.

    "REST Request ist fehlerhaft bzw. leer
    IF is_terminatioin_request IS INITIAL.
      MESSAGE e003.
    ENDIF.

    "Map from REST Request to Business Structure
    DATA(ls_input) = VALUE /dkves/kp_mtr_vid_toll_term_s(
        customer_number               = is_terminatioin_request-customer_number
        email_address                 = is_terminatioin_request-email_address
        email_language                = is_terminatioin_request-email_language
        video_tolling_term_items      = is_terminatioin_request-video_tolling_tr_term_requests
    ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Call Use Case Methode for Scheduling Termination
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    mo_terminate_uc->schedule_video_tolling_term(
      EXPORTING is_terminate_req = ls_input
      IMPORTING es_term_checked  = DATA(ls_term_checked) ).


    "Get the errors from log
    DATA(lt_errors) =  mo_logger->export_to_table( ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Evaluate Response - ES_ORDER_CHECKED contains only accepted Items
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Process results based on acceptance status
    IF ls_term_checked-video_tolling_term_items IS INITIAL.
      rs_response = handle_total_term_rejection(
            is_term_request = is_terminatioin_request
            it_errors       = lt_errors ).

    ELSE.
      rs_response = handle_total_term_acceptance(
        is_term_request    = is_terminatioin_request
        is_term_checked    = ls_term_checked
        it_errors          = lt_errors ).
    ENDIF.
  ENDMETHOD.


  METHOD /dkves/ue_if_hx_ib_adapter~set_use_case.
    mo_use_case = CAST #( io_use_case ).
  ENDMETHOD.


  METHOD add_ord_accepted_req_status.
    TRY.
        cl_bs_soa_convert_date_time=>map_utc_date_time_out(
          EXPORTING iv_timestamp = iv_acpt_date
          IMPORTING ev_utc_datetime = DATA(lv_utc)
       ).
      CATCH  cx_bs_soa_exception INTO DATA(lcx_exp).
        mo_logger->add( lcx_exp ).
    ENDTRY.


    APPEND VALUE #(
       customer_number                          = iv_customer
       license_plate                            = iv_lpn
       validationresult-result                  = c_status-accepted
       validationresult-acceptance_date         = lv_utc
         ) TO xt_req_status.
  ENDMETHOD.


  METHOD add_ord_rejected_req_ststatus.
    APPEND VALUE #(
      customer_number                          = iv_customer
      license_plate                            = iv_lpn
      validationresult-result                  = c_status-not_accepted
      validationresult-error-error_code        = is_error-number
      validationresult-error-error_description = is_error-message
      validationresult-error-field             = is_error-message_v1
    ) TO xt_req_status.
  ENDMETHOD.


  METHOD add_term_accepted_req_status.
    TRY.
        cl_bs_soa_convert_date_time=>map_utc_date_time_out(
          EXPORTING iv_timestamp = iv_acpt_date
          IMPORTING ev_utc_datetime = DATA(lv_utc)
       ).
      CATCH  cx_bs_soa_exception INTO DATA(lcx_exp).
        mo_logger->add( lcx_exp ).
    ENDTRY.

    APPEND VALUE #(
       customer_number                  = iv_customer
       equnr                            = iv_equnr
       validationresult-result          = c_status-accepted
       validationresult-acceptance_date = lv_utc
         ) TO xt_req_status.
  ENDMETHOD.


  METHOD add_term_rejected_req_status.
    APPEND VALUE #(
      customer_number                          = iv_customer
      equnr                                    = iv_equnr
      validationresult-result                  = c_status-not_accepted
      validationresult-error-error_code        = is_error-number
      validationresult-error-error_description = is_error-message
      validationresult-error-field             = is_error-message_v1
    ) TO xt_req_status.
  ENDMETHOD.


  METHOD handle_order_acceptance.
    DATA: lv_accepted_items TYPE i.

    LOOP AT is_booking_request-video_tolling_turkey_requests
      ASSIGNING FIELD-SYMBOL(<request_item>).

      IF line_exists( is_order_checked-video_tolling_order_items[ lpn = <request_item>-lpn ] ).
        " Handle accepted item
        add_ord_accepted_req_status(
          EXPORTING
            iv_customer  = is_booking_request-customer_number
            iv_lpn      = <request_item>-lpn
            iv_acpt_date = is_booking_request-acceptance_date
          CHANGING
            xt_req_status = rs_response-requeststatus ).

        ADD 1 TO lv_accepted_items.
      ELSE.
        " Handle rejected item
        DATA(ls_error) = it_errors[ message_v4 = <request_item>-lpn ].
        add_ord_rejected_req_ststatus(
          EXPORTING
            iv_customer   = is_booking_request-customer_number
            iv_lpn       = <request_item>-lpn
            is_error     = ls_error
          CHANGING
            xt_req_status = rs_response-requeststatus ).
      ENDIF.
    ENDLOOP.

    " Set final result status
    rs_response-result = COND #(
      WHEN lv_accepted_items = lines( is_booking_request-video_tolling_turkey_requests )
      THEN c_status-accepted
      ELSE c_status-part_accepted ).
  ENDMETHOD.


  METHOD handle_total_order_rejection.
    rs_response-result = c_status-not_accepted.

    LOOP AT it_errors ASSIGNING FIELD-SYMBOL(<error>).
      add_ord_rejected_req_ststatus(
        EXPORTING
          iv_customer   = is_booking_request-customer_number
          iv_lpn       = CONV #( <error>-message_v4 )
          is_error     = <error>
        CHANGING
          xt_req_status = rs_response-requeststatus ).
    ENDLOOP.
  ENDMETHOD.


  METHOD handle_total_term_acceptance.
    DATA: lv_accepted_items TYPE i.

    LOOP AT is_term_request-video_tolling_tr_term_requests
      ASSIGNING FIELD-SYMBOL(<request_item>).

      IF line_exists( is_term_checked-video_tolling_term_items[ equnr = <request_item>-equnr ] ).
        " Handle accepted item
        add_term_accepted_req_status(
          EXPORTING
            iv_customer  = is_term_request-customer_number
            iv_acpt_date = is_term_request-acceptance_date
            iv_equnr     = <request_item>-equnr
          CHANGING
            xt_req_status = rs_response-requeststatus ).

        ADD 1 TO lv_accepted_items.
      ELSE.
        " Handle rejected item
        DATA(ls_error) = it_errors[ message_v4 = <request_item>-equnr ].
        add_term_rejected_req_status(
          EXPORTING
            iv_customer   = is_term_request-customer_number
            iv_equnr      = <request_item>-equnr
            is_error     = ls_error
          CHANGING
            xt_req_status = rs_response-requeststatus ).
      ENDIF.
    ENDLOOP.

    " Set final result status
    rs_response-result = COND #(
      WHEN lv_accepted_items = lines( is_term_request-video_tolling_tr_term_requests )
      THEN c_status-accepted
      ELSE c_status-part_accepted ).
  ENDMETHOD.


  METHOD handle_total_term_rejection.
    rs_response-result = c_status-not_accepted.
    LOOP AT it_errors ASSIGNING FIELD-SYMBOL(<error>).
      APPEND VALUE #(
        customer_number                          = is_term_request-customer_number
        equnr                                    = CONV #( <error>-message_v4 )
        validationresult-result                  = c_status-not_accepted
        validationresult-error-error_code        = <error>-id
        validationresult-error-error_description = <error>-message
        validationresult-error-field             = <error>-message_v1
         ) TO rs_response-requeststatus.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
