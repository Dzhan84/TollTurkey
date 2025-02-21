class /DKVES/SM_CL_TVID_REST_HANDLER definition
  public
  inheriting from /DKVES/SM_REST_CL_BASE_APP_HDL
  create public .

public section.

  constants C_CLASSNAME type SEOCLASS-CLSNAME value '/DKVES/CL_MTR_VID_REST_HANDLER' ##NO_TEXT.

  methods CONSTRUCTOR .
  methods HANDLE_CREATE_REQUEST
    importing
      !IS_REQUEST type /DKVES/KP_MTR_VIDEO_REST_ORDER
      !IR_RESPONSE type ref to IF_REST_RESPONSE optional
      !IR_REST_PROTOC type ref to /DKVES/KP_CL_REST_PROTOCOL optional .
  methods HANDLE_UPDATE_REQUEST
    importing
      !IS_REQUEST type /DKVES/KP_MTR_VIDEO_REST_TERM
      !IR_RESPONSE type ref to IF_REST_RESPONSE optional
      !IR_REST_PROTOC type ref to /DKVES/KP_CL_REST_PROTOCOL optional .

  methods CREATE
    redefinition .
  methods UPDATE
    redefinition .
  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS create_new
      IMPORTING ir_request  TYPE REF TO if_rest_request
                ir_response TYPE REF TO if_rest_response.

    METHODS create_old
      IMPORTING ir_request  TYPE REF TO if_rest_request
                ir_response TYPE REF TO if_rest_response.

    METHODS update_new
      IMPORTING ir_request  TYPE REF TO if_rest_request
                ir_response TYPE REF TO if_rest_response.

    METHODS update_old
      IMPORTING ir_request  TYPE REF TO if_rest_request
                ir_response TYPE REF TO if_rest_response.




    METHODS extract_order_request_data
      IMPORTING ir_request              TYPE REF TO if_rest_request
      RETURNING VALUE(rs_transfer_data) TYPE /dkves/kp_mtr_video_order_rq.

    METHODS extract_term_request_data
      IMPORTING ir_request              TYPE REF TO if_rest_request
      RETURNING VALUE(rs_transfer_data) TYPE  /dkves/kp_mtr_video_rest_term.

    METHODS extract_order_values
      IMPORTING ir_data                 TYPE REF TO data
      RETURNING VALUE(rs_transfer_data) TYPE /dkves/kp_mtr_video_order_rq.

    METHODS extract_termination_values
      IMPORTING ir_data                 TYPE REF TO data
      RETURNING VALUE(rs_transfer_data) TYPE  /dkves/kp_mtr_video_term_rq.

    METHODS create_base_metadata
      IMPORTING iv_request_id      TYPE  /dkves/sm_rest_lpn_uuid
                iv_customer        TYPE kunnr
                iv_message_type    TYPE bapi_mtype
      RETURNING VALUE(rs_metadata) TYPE /dkves/kp_log_metadata.






ENDCLASS.



CLASS /DKVES/SM_CL_TVID_REST_HANDLER IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.


  METHOD create.
    "Switch between new an old implementation
    IF /dkves/ue_active_developments=>is_development_active( /dkves/if_ue_devid=>c_mtr_leo3 ).
      "Implemanted with LEO 3.0 Framework
      create_new(
          ir_request  = ir_request
          ir_response = ir_response ).
    ELSE.
      "Old Implementation
      create_old(
          ir_request  = ir_request
          ir_response = ir_response  ).
    ENDIF.
  ENDMETHOD.


  METHOD create_base_metadata.
    "populates meta information for loggin
    rs_metadata = VALUE #(
      debitor         = iv_customer
      meldungstyp     = iv_message_type
      meldungsklasse  = /dkves/kp_if_ws_const=>const_meldungsklasse_fachlich
      parameter_name  = 'REQUESTID'
      parameter_value = to_upper( iv_request_id )
    ).
  ENDMETHOD.


  METHOD create_new.
    DATA ls_app_request TYPE /dkves/kp_mtr_video_rest_order.

    DATA(lo_rest_protoc) = NEW /dkves/kp_cl_rest_protocol(
      iv_funcname = |POST: | && get_path_info( ir_request )
      iv_calling_class = CONV #( c_classname )
    ).

*   Populates meta information for loggin incoming request
    DATA(ls_metadata) = VALUE /dkves/kp_log_metadata( context = /dkves/kp_if_ws_const=>const_rest_context
                                                      aufrufer = sy-uname
                                                      meldungstyp = /dkves/kp_if_ws_const=>const_meldungstyp_success
                                                      meldungsklasse = /dkves/kp_if_ws_const=>const_meldungsklasse_fachlich
                                                    ).

*   Extract actuals of the query parameters of the incoming variables
    ls_app_request-request = extract_order_request_data( ir_request ).

*   Set request Id for logging
    ls_metadata-parameter_name = 'REQUESTID'.
    ls_metadata-parameter_value = to_upper( ls_app_request-request-request_id ).

*   Log the rest header
    lo_rest_protoc->log_rest_header(
      is_meta_data = ls_metadata
      i_payload = mt_header_fields
    ).

*   Log the extracted actuals
    IF NOT ls_app_request IS INITIAL.
      lo_rest_protoc->log_sent_request(
        is_meta_data = ls_metadata
        i_payload    = ls_app_request-request
      ).
    ENDIF.

    DATA(lo_rest_old) = NEW /dkves/sm_rest_mtr_video_order(  ).

*   Process Video Toll Order request
    handle_create_request(
        is_request       = ls_app_request
        ir_response      = ir_response
        ir_rest_protoc   = lo_rest_protoc
    ).
  ENDMETHOD.


  METHOD create_old.
    DATA(lo_mtr_video) = NEW /dkves/sm_rest_mtr_video_order(  ).

    lo_mtr_video->create(
        ir_request  = ir_request
        ir_response = ir_response
    ).
  ENDMETHOD.


  METHOD extract_order_request_data.
*   For logging purpose
    mt_header_fields = get_header_fields( ir_request ).

*   URI attributes
    mt_uri_attribute = get_uri_attributes( ir_request ).

    mo_json_helper->deserialize(
      EXPORTING
        ir_request = ir_request
      IMPORTING
        r_data = mr_input_data
    ).

    IF mr_input_data IS BOUND.

      "  rs_transfer_data-request
      rs_transfer_data  = extract_order_values( mr_input_data ).

    ENDIF.
  ENDMETHOD.


  METHOD extract_order_values.

    rs_transfer_data-request_id = extract_value_of_component(
      ir_data = ir_data
      iv_component = `requestId`
    ).

    TRY.
        cl_bs_soa_convert_date_time=>map_utc_date_time_in(
          EXPORTING
            iv_utc_datetime = extract_value_of_component(
                          ir_data = ir_data
                          iv_component = `requestInboundDate`
                              )
          IMPORTING
            ev_timestamp = DATA(l_timestampl)
    ).

        rs_transfer_data-acceptance_date = l_timestampl.

      CATCH cx_bs_soa_exception INTO DATA(lo_err).
        MESSAGE lo_err TYPE 'X'.
    ENDTRY.

    rs_transfer_data-customer_number = extract_value_of_component(
      ir_data = ir_data
      iv_component = `customerNumber`
    ).

    rs_transfer_data-customer_number = |{ rs_transfer_data-customer_number ALPHA = IN }|.

    rs_transfer_data-email_address = extract_value_of_component(
         ir_data = ir_data
         iv_component = `customerEmailAddress`
       ).
    rs_transfer_data-email_language = extract_value_of_component(
        ir_data = ir_data
        iv_component = `emailLanguage`
      ).

    DO.
      DATA(l_index) = sy-index.
      APPEND INITIAL LINE TO rs_transfer_data-video_tolling_turkey_requests ASSIGNING FIELD-SYMBOL(<ls_video_order>).

      <ls_video_order>-lpn = extract_value_of_component(
       ir_data = ir_data
       iv_index = l_index
       iv_component = `VideoTollingBookingRequests[i]-licensePlate` "#EC NOTEXT
    ).

      <ls_video_order>-country = extract_value_of_component(
         ir_data = ir_data
         iv_index = l_index
         iv_component = `VideoTollingBookingRequests[i]-countryISO2` "#EC NOTEXT
     ).
      <ls_video_order>-add_on = extract_value_of_component(
      ir_data = ir_data
      iv_index = l_index
      iv_component = `VideoTollingBookingRequests[i]-additionalReference` "#EC NOTEXT
  ).

      IF <ls_video_order>-lpn EQ space.
        DELETE rs_transfer_data-video_tolling_turkey_requests WHERE lpn EQ space.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD extract_termination_values.
    rs_transfer_data-request_id = extract_value_of_component(
       ir_data = ir_data
       iv_component = `requestId`
     ).

    TRY.
        cl_bs_soa_convert_date_time=>map_utc_date_time_in(
          EXPORTING
            iv_utc_datetime = extract_value_of_component(
                                ir_data = ir_data
                                iv_component = `requestInboundDate`
                              )
          IMPORTING
            ev_timestamp = DATA(l_timestampl)
        ).

        rs_transfer_data-acceptance_date = l_timestampl.
      CATCH cx_bs_soa_exception INTO DATA(lo_err).
        MESSAGE lo_err TYPE 'X'.
    ENDTRY.

    rs_transfer_data-customer_number = extract_value_of_component(
      ir_data = ir_data
      iv_component = `customerNumber`
    ).

    rs_transfer_data-customer_number = |{ rs_transfer_data-customer_number ALPHA = IN }|.

    rs_transfer_data-email_address = extract_value_of_component(
         ir_data = ir_data
         iv_component = `customerEmailAddress`
       ).
    rs_transfer_data-email_language = extract_value_of_component(
        ir_data = ir_data
        iv_component = `emailLanguage`
      ).

    DO.
      DATA(l_index) = sy-index.
      APPEND INITIAL LINE TO rs_transfer_data-video_tolling_tr_term_requests ASSIGNING FIELD-SYMBOL(<ls_video_term>).

      <ls_video_term>-equnr = extract_value_of_component(
       ir_data = ir_data
       iv_index = l_index
       iv_component = `VideoTollingTerminateRequests[i]-equnr` "#EC NOTEXT
    ).

      <ls_video_term>-obu_id = extract_value_of_component(
         ir_data = ir_data
         iv_index = l_index
         iv_component = `VideoTollingTerminateRequests[i]-obuId` "#EC NOTEXT
     ).

      IF <ls_video_term>-equnr EQ space.
        DELETE rs_transfer_data-video_tolling_tr_term_requests WHERE equnr EQ space.
        EXIT.
      ENDIF.

    ENDDO.
  ENDMETHOD.


  METHOD extract_term_request_data.
*   For logging purpose
    mt_header_fields = get_header_fields( ir_request ).

*   URI attributes
    mt_uri_attribute = get_uri_attributes( ir_request ).

    mo_json_helper->deserialize(
      EXPORTING
        ir_request = ir_request
      IMPORTING
        r_data = mr_input_data
    ).

    IF mr_input_data IS BOUND.

      rs_transfer_data-request = extract_termination_values( mr_input_data ).

    ENDIF.
  ENDMETHOD.


  METHOD handle_create_request.

    DATA:
      lo_inbound_adp       TYPE REF TO  /dkves/kp_if_tvid_rest_ib_adp.


    TRY.
        DATA(lo_comp_creator) = /dkves/ue_cl_hx_creator_fac=>get_component_creator( ).

        "Create Use Case and Components
        lo_comp_creator->create_components_by_phase_id(
            /dkves/kp_if_tvid_order_ib_prt=>c_schedule_order_creation ).


        "Find Inbound Adapter
        lo_inbound_adp = CAST #(
             lo_comp_creator->get_inbound_adapter( /dkves/kp_if_tvid_order_ib_prt=>c_phase_1-rest_ib_adp ) ).

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        "Execute Use Case
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        DATA(ls_response)
            = lo_inbound_adp->schedule_video_tolling_booking( is_request-request  ).

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " Evaluate response
        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF ls_response-result = /dkves/kp_cl_tvid_rest_ib_adp=>c_status-not_accepted.
          " Set response metadata
          DATA(ls_metadata)
              = create_base_metadata( iv_customer     = is_request-request-customer_number
                                      iv_request_id   = is_request-request-request_id
                                      iv_message_type =  /dkves/kp_if_ws_const=>const_meldungstyp_error  ).

          " Get error messages
          ir_response->set_status(
            iv_status        = cl_rest_status_code=>gc_client_error_forbidden
            iv_reason_phrase = /dkves/kp_cl_tvid_rest_ib_adp=>c_status-forbidden
          ).
        ELSE.
          " Set response metadata
          ls_metadata
              = create_base_metadata( iv_customer     = is_request-request-customer_number
                                      iv_request_id   = is_request-request-request_id
                                      iv_message_type = /dkves/kp_if_ws_const=>const_meldungstyp_success ).
          "Set Response Code
          ir_response->set_status(
            iv_status = SWITCH #( ls_response-result
                                  WHEN /dkves/kp_cl_tvid_rest_ib_adp=>c_status-accepted
                                       THEN cl_rest_status_code=>gc_success_accepted
                                  WHEN /dkves/kp_cl_tvid_rest_ib_adp=>c_status-part_accepted
                                       THEN '207' )
            iv_reason_phrase =  ls_response-result
          ).
        ENDIF.

        "Save KP Log
        ir_rest_protoc->log_sent_response(
            is_meta_data = ls_metadata
            i_payload    = ls_response ).

        "Return the result
        mo_json_helper->serialize(
          iv_data     = ls_response
          ir_response = ir_response
          it_name_mappings = VALUE #( ( abap = 'REQUESTSTATUS'    json = 'requestStatus' )   "Transport hat gedumpt wegen dem fehlenden PDI Kontext.
                                      ( abap = 'VALIDATIONRESULT' json = 'validationResult' ) )
        ).
      CATCH /dkves/ue_cx_hx_t100_dynamic INTO DATA(lcx_err).
        MESSAGE lcx_err TYPE 'X'.
    ENDTRY.
  ENDMETHOD.


  METHOD handle_update_request.
    DATA:
      lo_inbound_adp       TYPE REF TO  /dkves/kp_if_tvid_rest_ib_adp.



    DATA(lo_comp_creator) = /dkves/ue_cl_hx_creator_fac=>get_component_creator( ).

    "Create Use Case and Components
    lo_comp_creator->create_components_by_phase_id(
        /dkves/kp_if_tvid_term_ib_prt=>c_schedule_order_termination ).


    "Find Inbound Adapter
    lo_inbound_adp = CAST #(
         lo_comp_creator->get_inbound_adapter( /dkves/kp_if_tvid_term_ib_prt=>c_phase_1-rest_ib_adp ) ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "Execute Use Case
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(ls_response)
        = lo_inbound_adp->schedule_video_tolling_term( is_request-request  ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Evaluate response
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF ls_response-result = /dkves/kp_cl_tvid_rest_ib_adp=>c_status-not_accepted.
      " Set response metadata   fffdfd
      DATA(ls_metadata)
          = create_base_metadata( iv_customer     = is_request-request-customer_number
                                  iv_request_id   = is_request-request-request_id
                                  iv_message_type =  /dkves/kp_if_ws_const=>const_meldungstyp_error  ).

      " Get error messages
      ir_response->set_status(
        iv_status        = cl_rest_status_code=>gc_client_error_forbidden
        iv_reason_phrase = /dkves/kp_cl_tvid_rest_ib_adp=>c_status-forbidden
      ).
    ELSE.
      " Set response metadata
      ls_metadata
          = create_base_metadata( iv_customer     = is_request-request-customer_number
                                  iv_request_id   = is_request-request-request_id
                                  iv_message_type = /dkves/kp_if_ws_const=>const_meldungstyp_success ).
      "Set Response Code
      ir_response->set_status(
        iv_status = SWITCH #( ls_response-result
                              WHEN /dkves/kp_cl_tvid_rest_ib_adp=>c_status-accepted
                                   THEN cl_rest_status_code=>gc_success_accepted
                              WHEN /dkves/kp_cl_tvid_rest_ib_adp=>c_status-part_accepted
                                   THEN '207' )
        iv_reason_phrase =  ls_response-result
      ).
    ENDIF.

    "Save KP Log
    ir_rest_protoc->log_sent_response(
        is_meta_data = ls_metadata
        i_payload    = ls_response ).

    "Return the result
    mo_json_helper->serialize(
      iv_data     = ls_response
      ir_response = ir_response
      it_name_mappings = VALUE #( ( abap = 'REQUESTSTATUS'    json = 'requestStatus' )   "Transport hat gedumpt wegen dem fehlenden PDI Kontext.
                                  ( abap = 'VALIDATIONRESULT' json = 'validationResult' ) )
    ).
  ENDMETHOD.


  METHOD update.

    "Switch between new an old implementation
    IF /dkves/ue_active_developments=>is_development_active( /dkves/if_ue_devid=>c_mtr_leo3 ).
      "Implemanted with LEO 3.0 Framework
      update_new(
          ir_request  = ir_request
          ir_response = ir_response ).
    ELSE.
      "Old Implementation
      update_old(
          ir_request  = ir_request
          ir_response = ir_response  ).
    ENDIF.

  ENDMETHOD.


  METHOD update_new.


    DATA ls_app_request TYPE /dkves/kp_mtr_video_rest_term.

    DATA(lo_rest_protoc) = NEW /dkves/kp_cl_rest_protocol(
      iv_funcname = |PUT: | && get_path_info( ir_request )
      iv_calling_class = CONV #( c_classname )
    ).

*   Populates meta information for loggin incoming request
    DATA(ls_metadata) = VALUE /dkves/kp_log_metadata( context = /dkves/kp_if_ws_const=>const_rest_context
                                                      aufrufer = sy-uname
                                                      meldungstyp = /dkves/kp_if_ws_const=>const_meldungstyp_success
                                                      meldungsklasse = /dkves/kp_if_ws_const=>const_meldungsklasse_fachlich
                                                    ).

    "Extract actuals of the query parameters of the incoming variables
    ls_app_request = extract_term_request_data( ir_request ).

*   Set request Id for logging
    ls_metadata-parameter_name = 'REQUESTID'.
    ls_metadata-parameter_value = to_upper( ls_app_request-request-request_id ).

*   Log the rest header
    lo_rest_protoc->log_rest_header(
      is_meta_data = ls_metadata
      i_payload = mt_header_fields
    ).

*   Log the extracted actualsbert
    IF NOT ls_app_request IS INITIAL.
      lo_rest_protoc->log_sent_request(
        is_meta_data = ls_metadata
        i_payload    = ls_app_request-request
      ).
    ENDIF.

*   Process Video Toll Order request
    handle_update_request(
      is_request         = ls_app_request
      ir_response      = ir_response
      ir_rest_protoc   = lo_rest_protoc
    ).
  ENDMETHOD.


  METHOD update_old.
    DATA(lo_mtr_video) = NEW /dkves/sm_rest_mtr_video_order(  ).

    lo_mtr_video->update(
        ir_request  = ir_request
        ir_response = ir_response
    ).
  ENDMETHOD.
ENDCLASS.
