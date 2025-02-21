CLASS /dkves/kp_cl_tvid_validator DEFINITION
  INHERITING FROM /dkves/ue_cl_hx_comp_base
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /dkves/kp_if_tvid_validator.

  PRIVATE SECTION.

    METHODS check_customer_exist
      IMPORTING iv_customer TYPE kunnr
      RAISING   /dkves/ue_cx_hx_t100_dynamic.

    METHODS check_lpn_status
      IMPORTING iv_lpn      TYPE /dkves/kp_kfzkennzeichen
                iv_customer TYPE kunnr
      RAISING   /dkves/ue_cx_hx_t100_dynamic.

    METHODS check_card_addon
      IMPORTING iv_addon TYPE /dkves/kp_kartenzusatz
      RAISING   /dkves/ue_cx_hx_t100_dynamic.

    METHODS check_lpn_matchcode
      IMPORTING iv_lpn TYPE /dkves/kp_kfzkennzeichen
      RAISING   /dkves/ue_cx_hx_t100_dynamic.

    METHODS check_special_chars
      IMPORTING iv_lpn TYPE /dkves/kp_kfzkennzeichen
      RAISING   /dkves/ue_cx_hx_t100_dynamic.

    METHODS check_mandatory_fields
      IMPORTING is_request_item TYPE /dkves/kp_mtr_video_order_rq_s
      RAISING   /dkves/ue_cx_hx_t100_dynamic.

    METHODS check_pan_status
      IMPORTING is_request_item TYPE /dkves/kp_mtr_video_term_rq_s
      RAISING   /dkves/ue_cx_hx_t100_dynamic.

ENDCLASS.



CLASS /DKVES/KP_CL_TVID_VALIDATOR IMPLEMENTATION.


  METHOD /dkves/kp_if_tvid_validator~validate_order_data.
    DATA lt_return  TYPE bapiret2_t.


    DATA(ls_order)
      = VALUE /dkves/kp_order_tc( vertragsnummer = iv_contract_id
                                  vertragsposnr  = iv_posnr
                                  isocode        = /dkves/if_kp_isocode=>c_mtr_video_tolling
                                  leo_art        = /dkves/if_kp_leoart=>c_mtr_video_tolling
                                  anzahl_leo     = '0001'
                                  status         = 'ANGELEGT'
                                  bstknr         = iv_order_id
                                  kfz_kennz      = is_request_item-lpn
                                  kfz_land       = is_request_item-country
                                  inhalt_kfz_kz  = '01'
                                  kartenzusatz   = is_request_item-add_on ).

    "Validate order
    CALL FUNCTION '/DKVES/KP_ORDER_CHECK_PLAUSI2'
      EXPORTING
        is_data     = ls_order
        iv_oblcheck = abap_false
      IMPORTING
        et_return   = lt_return.

    IF line_exists( lt_return[ type = 'E' ] ).

      DATA(ls_error) = VALUE #( lt_return[ type = 'E' ] ).

      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE ID ls_error-id
        TYPE ls_error-type
        NUMBER ls_error-number
        WITH ls_error-message_v1
        ls_error-message_v2
        ls_error-message_v3
        ls_error-message_v4.
    ENDIF.

  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_validator~validate_order_request.

    es_order_checked = is_order_req.


    TRY.
        "If it failes here -> full rejection
        check_customer_exist(  is_order_req-customer_number ).

        "Check the Items if some of them fails => partial rejection
        LOOP AT is_order_req-video_tolling_order_items
            ASSIGNING FIELD-SYMBOL(<request_item>).
          TRY.
              check_mandatory_fields( <request_item> ).

              check_lpn_status( iv_lpn      = <request_item>-lpn
                                iv_customer = is_order_req-customer_number ).

              check_lpn_matchcode( <request_item>-lpn ).

              check_special_chars( <request_item>-lpn ).

              check_card_addon( <request_item>-add_on ).

            CATCH /dkves/ue_cx_hx_t100_dynamic  INTO DATA(lcx_err).

              DATA(ls_bapieret2) = lcx_err->get_bapiret2( ).

              "Save corresponding LPN Number for the Error Log
              ls_bapieret2-message_v4 = <request_item>-lpn.

              mo_logger->add( ls_bapieret2 ).

              "Delete rejected items from delta table
              DELETE TABLE es_order_checked-video_tolling_order_items
                FROM <request_item>.

          ENDTRY.
        ENDLOOP.

      CATCH /dkves/ue_cx_hx_t100_dynamic  INTO lcx_err.
        mo_logger->add( lcx_err ).
        FREE es_order_checked.

    ENDTRY.

  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_validator~validate_termination_data.

    IF is_termination-equnr IS INITIAL.
      RAISE EXCEPTION TYPE  /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e075(/dkves/kp_erw)
          WITH 'PAN'.
    ENDIF.
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_validator~validate_termination_request.
    es_term_checked = is_term_req.


    TRY.
        "If it failes here -> full rejection
        check_customer_exist(  is_term_req-customer_number ).

        "Check the Items if some of them fails => partial rejection
        LOOP AT is_term_req-video_tolling_term_items
            ASSIGNING FIELD-SYMBOL(<request_item>).
          TRY.
              check_pan_status( <request_item> ).

            CATCH /dkves/ue_cx_hx_t100_dynamic  INTO DATA(lcx_err).

              DATA(ls_bapieret2) = lcx_err->get_bapiret2( ).

              "Save corresponding LPN Number for the Error Log
              ls_bapieret2-message_v4 = <request_item>-equnr.

              mo_logger->add( ls_bapieret2 ).

              "Delete rejected items from delta table
              DELETE TABLE es_term_checked-video_tolling_term_items
                FROM <request_item>.

          ENDTRY.
        ENDLOOP.


      CATCH /dkves/ue_cx_hx_t100_dynamic  INTO lcx_err.
        mo_logger->add( lcx_err ).
        FREE es_term_checked .

    ENDTRY.

  ENDMETHOD.


  METHOD check_card_addon.
    DATA lv_ok TYPE flag.

    CALL FUNCTION '/DKVES/KP_LEO_CHECK_STRING'
      EXPORTING
        iv_string = CONV string( iv_addon )
      IMPORTING
        iv_ok     = lv_ok.
    IF lv_ok = abap_false.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e707 WITH iv_addon.
    ENDIF.
  ENDMETHOD.


  METHOD check_customer_exist.
    SELECT COUNT(*)
      FROM kna1
    WHERE kunnr = iv_customer.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e706.
    ENDIF.

  ENDMETHOD.


  METHOD check_lpn_matchcode.
    DATA lv_lpn_match_code TYPE char14.

    CALL FUNCTION '/DKVES/KP_CLEAN_KFZ'
      EXPORTING
        iv_input  = iv_lpn
      IMPORTING
        iv_output = lv_lpn_match_code.

    IF lv_lpn_match_code IS INITIAL
   AND iv_lpn            IS NOT INITIAL.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e703 WITH iv_lpn.
    ENDIF.
  ENDMETHOD.


  METHOD check_lpn_status.
    SELECT COUNT(*)
    FROM equi
    INNER JOIN jest ON jest~objnr = equi~objnr
    WHERE /dkves/kfzkennz   = iv_lpn
      AND /dkves/leoart_id  = /dkves/if_kp_leoart=>c_mtr_video_tolling "9080'
      AND /dkves/isocode    = /dkves/if_kp_isocode=>c_mtr_video_tolling " '908000'
      AND kunde             = iv_customer
      AND jest~stat         = /dkves/if_j_status=>c_active "E0013'
      AND jest~inact        = abap_false.
    IF sy-subrc           = 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e708 WITH iv_lpn.
    ENDIF.

  ENDMETHOD.


  METHOD check_mandatory_fields.

    IF  is_request_item-lpn IS INITIAL.
      RAISE EXCEPTION TYPE /dkves/cx_t100_dynamic
        MESSAGE e901.
    ENDIF.

    IF  is_request_item-country IS INITIAL.
      RAISE EXCEPTION TYPE /dkves/cx_t100_dynamic
        MESSAGE e005.
    ENDIF.
  ENDMETHOD.


  METHOD check_pan_status.
    SELECT equi~equnr,
           equi~kunde,
           equi~/dkves/origleonr,
           equi~/dkves/kfzkennz,
           equi~/dkves/ka_zus,
           jest~stat
      FROM equi INNER
      JOIN jest ON jest~objnr = equi~objnr INNER
      JOIN equz ON equz~equnr = equi~equnr
      INTO @DATA(ls_pan)
      UP TO 1 ROWS
      WHERE equi~equnr = @is_request_item-equnr
        AND equi~/dkves/isocode   = @/dkves/if_kp_isocode=>c_mtr_video_tolling "908000'
        AND equi~/dkves/leoart_id = @/dkves/if_kp_leoart=>c_mtr_video_tolling    "9080'
        AND jest~stat LIKE 'E%'
        AND jest~inact = @abap_false.
    ENDSELECT.
    IF sy-subrc = 0.
      IF ls_pan-stat <> /dkves/kp_cl_constants=>gc_leostatus_active.
        RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
            MESSAGE e704.
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e702.
    ENDIF.
  ENDMETHOD.


  METHOD check_special_chars.
    DATA: lv_refstring TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ0123456789 -',
          lv_chk_str   TYPE string,
          lv_ok        TYPE flag.

    lv_chk_str = iv_lpn.

    " KFZ-Kennzeichen ist nicht leoart-abhängig
    /dkves/kp_original_letter=>create_refstring(
        EXPORTING iv_isocode = '900003' "iv_isocode
                  iv_leoart  = '8008'   "iv_leoart
                  iv_country = space
         CHANGING ev_refstring = lv_refstring ).

    CALL FUNCTION '/DKVES/KP_LEO_CHK_STRG_INEXACT'
      EXPORTING
        iv_string    = lv_chk_str
        iv_reference = lv_refstring
      IMPORTING
        iv_ok        = lv_ok.
    IF lv_ok = abap_false.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e705 WITH iv_lpn.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
