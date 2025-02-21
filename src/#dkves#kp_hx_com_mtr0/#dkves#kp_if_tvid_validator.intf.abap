INTERFACE /dkves/kp_if_tvid_validator
  PUBLIC .

  METHODS validate_order_request
    IMPORTING is_order_req     TYPE /dkves/kp_mtr_vid_toll_order_s
    EXPORTING es_order_checked TYPE /dkves/kp_mtr_vid_toll_order_s
    RAISING   /dkves/ue_cx_hx_t100_dynamic.


  METHODS validate_order_data
    IMPORTING is_request_item TYPE  /dkves/kp_mtr_video_order_rq_s
              iv_contract_id  TYPE vbeln
              iv_posnr        TYPE posnr
              iv_order_id     TYPE /dkves/kp_ordernr
    RAISING   /dkves/ue_cx_hx_t100_dynamic.


  METHODS validate_termination_request
    IMPORTING is_term_req     TYPE /dkves/kp_mtr_vid_toll_term_s  "/dkves/kp_mtr_video_order_rq_s
    EXPORTING es_term_checked TYPE /dkves/kp_mtr_vid_toll_term_s
    RAISING   /dkves/ue_cx_hx_t100_dynamic.


  METHODS validate_termination_data
    IMPORTING is_termination  TYPE /DKVES/KP_MTR_VIDEO_TERM_RQ_S  "/dkves/kp_mtr_video_order_rq_s
    RAISING   /dkves/ue_cx_hx_t100_dynamic.
ENDINTERFACE.
