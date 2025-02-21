INTERFACE /dkves/kp_if_tvid_rest_ib_adp
  PUBLIC.

  " Method for booking a video tolling service
  METHODS schedule_video_tolling_booking
    IMPORTING is_booking_request TYPE /dkves/kp_mtr_video_order_rq
    RETURNING VALUE(rs_response) TYPE /dkves/kp_mtr_video_order_rs
    RAISING /dkves/ue_cx_hx_t100_dynamic.

  " Method for terminating a video tolling service
  METHODS schedule_video_tolling_term
    IMPORTING is_terminatioin_request TYPE  /dkves/kp_mtr_video_term_rq
    RETURNING VALUE(rs_response)   TYPE /dkves/kp_mtr_video_term_rs.




ENDINTERFACE.
