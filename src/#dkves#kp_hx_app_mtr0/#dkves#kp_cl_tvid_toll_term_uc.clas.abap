class /DKVES/KP_CL_TVID_TOLL_TERM_UC definition
  public
  inheriting from /DKVES/UE_CL_HX_USE_CASE_BASE
  final
  create public .

public section.

  interfaces /DKVES/KP_IF_TVID_TERM_IB_PRT .

  aliases C_PHASE_1
    for /dkves/kp_if_tvid_term_ib_prt~C_PHASE_1 .
  aliases C_PHASE_2
    for /dkves/kp_if_tvid_term_ib_prt~C_PHASE_2 .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mo_bgrfc_ob_adp       TYPE REF TO /dkves/kp_if_tvid_bgrfc_ob_prt.
    DATA: mo_validator    TYPE REF TO /dkves/kp_if_tvid_validator.
    DATA: mo_db_adp             TYPE REF TO /dkves/kp_if_tvid_db_ob_prt.
ENDCLASS.



CLASS /DKVES/KP_CL_TVID_TOLL_TERM_UC IMPLEMENTATION.


  METHOD /dkves/kp_if_tvid_term_ib_prt~execute_video_tolling_term.
    "Load Components
    mo_validator = CAST #( mo_comp_container->find_component_by_id( c_phase_2-validator ) ).
    mo_db_adp    = CAST #( mo_comp_container->find_component_by_id( c_phase_2-db_ob_adp ) ).

    mo_validator->validate_termination_data( is_termination ).

    "Service Termination = PAN Termination
    mo_db_adp->terminate_pan(
      EXPORTING iv_equnr  = is_termination-equnr
      IMPORTING et_return = DATA(lt_return)
    ).

    mo_logger->add( lt_return ).
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_term_ib_prt~schedule_video_tolling_term.
    "Load requiered components for the Use Case from component container
    mo_validator       = CAST #( mo_comp_container->find_component_by_id( c_phase_1-validator ) ).
    mo_bgrfc_ob_adp    = CAST #( mo_comp_container->find_component_by_id( c_phase_1-bgrfc_ob_adp ) ).

    "Validate Service Termination Request
    mo_validator->validate_termination_request(
        EXPORTING is_term_req       = is_terminate_req
        IMPORTING es_term_checked   = es_term_checked  ).

    IF es_term_checked-video_tolling_term_items  IS NOT INITIAL.
      "Schedule BGRFC for Service Termination
      mo_bgrfc_ob_adp->schedule_service_termination( es_term_checked ).

      COMMIT WORK.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
