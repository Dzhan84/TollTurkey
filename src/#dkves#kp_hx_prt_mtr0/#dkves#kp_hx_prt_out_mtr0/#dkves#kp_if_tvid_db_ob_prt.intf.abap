INTERFACE /dkves/kp_if_tvid_db_ob_prt
  PUBLIC .



  METHODS get_contract_id
    IMPORTING iv_customer_id        TYPE kunnr
    RETURNING VALUE(rv_contract_id) TYPE vbeln.

  METHODS get_contract_entity
    IMPORTING iv_vbeln                  TYPE vbeln
    RETURNING VALUE(ro_entity_contract) TYPE REF TO /dkves/sm_cl_entity_contract .

  METHODS get_main_order_item
    IMPORTING io_entity_contract           TYPE REF TO /dkves/sm_cl_entity_contract
    RETURNING VALUE(rs_contract_item_data) TYPE /dkves/sm_attributes_con_itm
    RAISING   /dkves/ue_cx_hx_t100_dynamic.


  METHODS prepare_data_for_entry_inbound
    IMPORTING is_contract_hdr TYPE /dkves/sm_attributes_con
              is_contract_itm TYPE  /dkves/sm_attributes_con_itm
              is_order_hdr    TYPE  /dkves/kp_mtr_vid_toll_order_s
              is_order_itm    TYPE /dkves/kp_mtr_video_order_rq_s
              iv_order_nr     TYPE /dkves/kp_ordernr
    EXPORTING et_entry_input  TYPE /dkves/sm_entry_input_sgntr_t
    RAISING   /dkves/ue_cx_hx_t100_dynamic.

  METHODS entry_inbound_process
    IMPORTING it_input TYPE /dkves/sm_entry_input_sgntr_t
    EXPORTING ev_equnr TYPE equnr
    RAISING   /dkves/ue_cx_hx_t100_dynamic.

  METHODS save_mdlaat_attribute
    IMPORTING iv_equnr     TYPE equnr
              iv_attribute TYPE /dkves/kp_mdlaat_attribut
              iv_value     TYPE  /dkves/kp_clatr_value
    RAISING   /dkves/ue_cx_hx_t100_dynamic.

  METHODS save_order_head
    IMPORTING is_order_hdr TYPE  /dkves/kp_bestek
    RAISING   /dkves/ue_cx_hx_t100_dynamic.

  METHODS terminate_pan
    IMPORTING iv_equnr  TYPE equnr
    EXPORTING et_return TYPE bapiret2_t
    RAISING   /dkves/ue_cx_hx_t100_dynamic.

  METHODS get_sales_contract
    IMPORTING iv_customer_no  TYPE kunnr
    RETURNING VALUE(rv_vbeln) TYPE vbeln
    RAISING   /dkves/ue_cx_hx_t100_dynamic.


  METHODS get_tvid_items
    IMPORTING iv_vbeln        TYPE vbeln
    RETURNING VALUE(rt_posnr) TYPE  POSNR_TAB.

  METHODS create_order_item
    IMPORTING iv_vbeln       TYPE vbeln
              iv_customer_no TYPE kunnr
    RAISING   /dkves/ue_cx_hx_t100_dynamic.
ENDINTERFACE.
