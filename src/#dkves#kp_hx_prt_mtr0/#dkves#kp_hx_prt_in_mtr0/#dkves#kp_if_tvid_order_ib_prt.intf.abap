INTERFACE /dkves/kp_if_tvid_order_ib_prt
  PUBLIC .


  CONSTANTS c_schedule_order_creation TYPE /dkves/ue_hx_use_phase_ref_id VALUE 'SG001-SV004-UC001-PH001' ##NO_TEXT.
  CONSTANTS c_execute_order_creation TYPE /dkves/ue_hx_use_phase_ref_id VALUE 'SG001-SV004-UC001-PH002' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_phase_1,
      rest_ib_adp  TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP001',
      validator    TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP002',
      bgrfc_ob_adp TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP003',
    END OF c_phase_1 .
  CONSTANTS:
    BEGIN OF c_phase_2,
      bgrfc_ib_adp TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP001',
      validator    TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP002',
      db_ob_adp    TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP003',
      mail_adp     TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP004',
      cntr_db_adp  TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP005',
      srd_bapi_adp  TYPE /dkves/ue_hx_cp-comp_id VALUE 'CP006',
    END OF c_phase_2 .

  " Method for booking a video tolling service
  METHODS schedule_video_tolling_order
    IMPORTING
      !is_order_req     TYPE /dkves/kp_mtr_vid_toll_order_s
    EXPORTING
      !es_order_checked TYPE /dkves/kp_mtr_vid_toll_order_s.

  " Method for terminating a video tolling service
  METHODS execute_video_tolling_order
    IMPORTING
      !is_order          TYPE /dkves/kp_mtr_vid_toll_order_s
    RETURNING
      VALUE(rs_response) TYPE /dkves/kp_mtr_video_order_rs
    RAISING
      /dkves/ue_cx_hx_t100_dynamic .
ENDINTERFACE.
