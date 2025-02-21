
CLASS ltc_mtr_vid_rest_handler DEFINITION FOR TESTING
  DURATION SHORT FINAL
  RISK LEVEL HARMLESS.


  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO /dkves/sm_cl_tvid_rest_handler.

    METHODS setup.
    METHODS teardown.
    METHODS test_create_request     FOR TESTING RAISING cx_uuid_error .
    METHODS test_terminate_request  FOR TESTING RAISING cx_uuid_error .
ENDCLASS.       "ltc_Mtr_Ib_Restat


CLASS ltc_mtr_vid_rest_handler IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).

  ENDMETHOD.


  METHOD teardown.


  ENDMETHOD.



  METHOD test_create_request.
    DATA ls_transfer_data TYPE /dkves/kp_mtr_video_rest_order.

    DATA(lv_system_uuid) = cl_uuid_factory=>create_system_uuid( )->create_uuid_c22( ).


    ls_transfer_data-request = VALUE  /dkves/kp_mtr_video_order_rq(
        request_id                    =  lv_system_uuid
        acceptance_date               = '20240711135729.0000000'
        customer_number               = '0000519546'
        email_address                 = 'tpo-preprod1@dkv-euroservice.com'
        email_language                = 'DE'
        video_tolling_turkey_requests = VALUE #( ( lpn = 'DKH 312' country = 'DE' add_on = 'TEST'  ) )
        ).

    "Execute main method
    mo_cut->handle_create_request( ls_transfer_data ).
  ENDMETHOD.


  METHOD test_terminate_request.
*    DATA ls_transfer_data TYPE /dkves/kp_mtr_video_rest_term.
*
*    DATA(lv_system_uuid) = cl_uuid_factory=>create_system_uuid( )->create_uuid_c22( ).
*
*
*    ls_transfer_data-request = VALUE  /dkves/kp_mtr_video_term_rq(
*        request_id                     =  lv_system_uuid
*        acceptance_date                = '20240711135729.0000000'
*        customer_number                = '0000764130'
*        email_address                  = 'tpo-preprod1@dkv-euroservice.com'
*        email_language                 = 'DE'
*        video_tolling_tr_term_requests = VALUE #(
*            ( equnr = '908000000000000162'  ) )
*        ).
*
*    "Execute main method
*    mo_cut->handle_update_request( ls_transfer_data ).
  ENDMETHOD.

ENDCLASS.
