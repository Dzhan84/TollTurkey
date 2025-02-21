INTERFACE /dkves/kp_if_tvid_term_ib_prt
  PUBLIC .


  CONSTANTS:
    c_schedule_order_termination TYPE /dkves/ue_hx_use_phase_ref_id VALUE 'SG001-SV004-UC002-PH003',
    c_execute_order_termination  TYPE /dkves/ue_hx_use_phase_ref_id VALUE 'SG001-SV004-UC002-PH004'.


  CONSTANTS:
    BEGIN OF c_phase_1,
      rest_ib_adp  TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP001',
      validator    TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP002',
      bgrfc_ob_adp TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP003',
    END OF c_phase_1,

    BEGIN OF c_phase_2,
      bgrfc_ib_adp TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP001',
      validator    TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP002',
      db_ob_adp    TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP003',
      mail_adp     TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP004',
    END OF c_phase_2.


  METHODS schedule_video_tolling_term
    IMPORTING is_terminate_req TYPE /dkves/kp_mtr_vid_toll_term_s
    EXPORTING es_term_checked  TYPE /dkves/kp_mtr_vid_toll_term_s.

  METHODS execute_video_tolling_term
    IMPORTING is_termination    TYPE /dkves/kp_mtr_video_term_rq_s
    RETURNING VALUE(rs_response) TYPE /dkves/kp_mtr_video_term_rq_s
    RAISING   /dkves/ue_cx_hx_t100_dynamic.







ENDINTERFACE.
