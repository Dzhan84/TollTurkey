CLASS /dkves/kp_cl_tvid_bgrfc_ob_adp DEFINITION
  PUBLIC
  INHERITING FROM /dkves/ue_cl_hx_comp_base
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS /dkves/ue_if_hx_comp_creator .

  PUBLIC SECTION.

    INTERFACES /dkves/kp_if_tvid_bgrfc_ob_prt .

    CONSTANTS:
      BEGIN OF c_bgrfc,
        destination TYPE bgrfc_dest_name_inbound VALUE 'BGRFC_PDI',
        unit_video  TYPE qrfc_queue_name VALUE 'VIDEO_TR',
      END OF c_bgrfc .
  PRIVATE SECTION.
ENDCLASS.



CLASS /dkves/kp_cl_tvid_bgrfc_ob_adp IMPLEMENTATION.


  METHOD /dkves/kp_if_tvid_bgrfc_ob_prt~schedule_service_booking.
    " transform business structure
    " to bapi structure
    " and call bapi and save/persist
    DATA: lr_destination TYPE REF TO if_bgrfc_destination_inbound,
          lr_unit        TYPE REF TO if_qrfc_unit_inbound.

    TRY.
*
        lr_destination = cl_bgrfc_destination_inbound=>create( c_bgrfc-destination ).
        lr_unit = lr_destination->create_qrfc_unit( ).
        lr_unit->if_bgrfc_unit~disable_commit_checks( ).

        "bgRFC Order Video Tolling Service
        CALL FUNCTION '/DKVES/KP_MTR_VTL_ORDER_BGRFC' IN BACKGROUND UNIT lr_unit
          EXPORTING
            is_order = is_order_request.

        lr_unit->add_queue_name_inbound( c_bgrfc-unit_video && |_| && is_order_request-customer_number ).
*
      CATCH cx_bgrfc_invalid_destination INTO DATA(lr_invalid_destination).
        mo_logger->add( lr_invalid_destination ).
      CATCH cx_bgrfc_invalid_context INTO DATA(lr_invalid_context).
        mo_logger->add( lr_invalid_context ).
      CATCH cx_bgrfc_invalid_unit INTO DATA(lr_invalid_unit).
        mo_logger->add( lr_invalid_unit ).
      CATCH cx_qrfc_duplicate_queue_name INTO DATA(lr_duplicate_queue_name).
        mo_logger->add( lr_duplicate_queue_name ).
      CATCH cx_qrfc_invalid_queue_name INTO DATA(lr_invalid_queue_name).
        mo_logger->add( lr_invalid_queue_name ).
    ENDTRY.


  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_bgrfc_ob_prt~schedule_service_termination.
    " transform business structure
    " to bapi structure
    " and call bapi and save/persist
    DATA: lr_destination TYPE REF TO if_bgrfc_destination_inbound,
          lr_unit        TYPE REF TO if_qrfc_unit_inbound.


    LOOP AT is_term_request-video_tolling_term_items
     ASSIGNING FIELD-SYMBOL(<term_item>).
      TRY.

          lr_destination = cl_bgrfc_destination_inbound=>create( c_bgrfc-destination ).
          lr_unit = lr_destination->create_qrfc_unit( ).
          lr_unit->if_bgrfc_unit~disable_commit_checks( ).

          CALL FUNCTION '/DKVES/KP_MTR_VTL_TERM_BGRFC' IN BACKGROUND UNIT lr_unit
            EXPORTING
              is_termination = <term_item>.

          lr_unit->add_queue_name_inbound( c_bgrfc-unit_video && |_| && is_term_request-customer_number ).
*
        CATCH cx_bgrfc_invalid_destination INTO DATA(lr_invalid_destination).
          mo_logger->add( lr_invalid_destination ).
        CATCH cx_bgrfc_invalid_context INTO DATA(lr_invalid_context).
          mo_logger->add( lr_invalid_context ).
        CATCH cx_bgrfc_invalid_unit INTO DATA(lr_invalid_unit).
          mo_logger->add( lr_invalid_unit ).
        CATCH cx_qrfc_duplicate_queue_name INTO DATA(lr_duplicate_queue_name).
          mo_logger->add( lr_duplicate_queue_name ).
        CATCH cx_qrfc_invalid_queue_name INTO DATA(lr_invalid_queue_name).
          mo_logger->add( lr_invalid_queue_name ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
