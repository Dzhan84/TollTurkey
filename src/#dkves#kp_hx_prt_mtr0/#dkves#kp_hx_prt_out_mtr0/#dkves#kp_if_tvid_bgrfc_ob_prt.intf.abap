INTERFACE /DKVES/KP_IF_TVID_BGRFC_OB_PRT
  PUBLIC .


  METHODS schedule_service_booking
    IMPORTING !is_order_request  TYPE /dkves/kp_mtr_vid_toll_order_s
    RETURNING VALUE(rt_response) TYPE bapiret2_t .

  METHODS schedule_service_termination
    IMPORTING !is_term_request   TYPE /dkves/kp_mtr_vid_toll_term_s
    RETURNING VALUE(rt_response) TYPE bapiret2_t .

ENDINTERFACE.
