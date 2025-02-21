FUNCTION /DKVES/KP_MTR_VTL_TERM_BGRFC.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IS_TERMINATION) TYPE  /DKVES/KP_MTR_VIDEO_TERM_RQ_S
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TT
*"     VALUE(EV_ERROR) TYPE  SAP_BOOL
*"     VALUE(EV_ERROR_MSG) TYPE  STRING
*"  EXCEPTIONS
*"      ERRORS_FOR_SBGRFCMON
*"----------------------------------------------------------------------
  DATA:
    lo_bgrfc_inbound_adp TYPE REF TO /dkves/kp_if_tvid_bgrfc_ib_adp.

    "Conditional Breakpoint
    /dkves/ue_cl_breakpointer=>break_point( /DKVES/IF_AAB_ID_NAME=>c_toll_turkey ).

  TRY.
      DATA(lo_comp_creator) = /dkves/ue_cl_hx_creator_fac=>get_component_creator( ).

      "Create Components for Use Case
      lo_comp_creator->create_components_by_phase_id(
        /dkves/kp_if_tvid_term_ib_prt=>c_execute_order_termination ).

      "Find Inbound Adapter
      lo_bgrfc_inbound_adp = CAST #(
        lo_comp_creator->get_inbound_adapter( /dkves/kp_if_tvid_term_ib_prt=>c_phase_2-bgrfc_ib_adp ) ) .

      "Execute Order Creatio Request
      DATA(ls_response)
          = lo_bgrfc_inbound_adp->execute_video_tolling_term( is_termination ).

    CATCH cx_ai_system_fault INTO DATA(lx_system_fault).
      NEW /dkves/kp_cl_bgrfc( )->raise_message_for_monitor( lx_system_fault->errortext ).
    CATCH /dkves/cx_t100_dynamic /dkves/ue_cx_hx_t100_dynamic INTO DATA(lx_t100).
      NEW /dkves/kp_cl_bgrfc( )->raise_message_for_monitor( lx_t100->get_text( ) ).
  ENDTRY.
ENDFUNCTION.
