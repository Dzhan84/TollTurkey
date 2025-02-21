CLASS /dkves/kp_cl_tvid_db_ob_adp DEFINITION
  PUBLIC
  INHERITING FROM /dkves/ue_cl_hx_comp_base
  FINAL
  CREATE PRIVATE

  GLOBAL FRIENDS /dkves/ue_if_hx_comp_creator .

  PUBLIC SECTION.

    INTERFACES /dkves/kp_if_tvid_db_ob_prt .

    ALIASES save_mdlaat_attribute
      FOR /dkves/kp_if_tvid_db_ob_prt~save_mdlaat_attribute .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS /dkves/kp_cl_tvid_db_ob_adp IMPLEMENTATION.


  METHOD /dkves/kp_if_tvid_db_ob_prt~create_order_item.
    DATA: ls_contract TYPE /dkves/dcon_create_itm_in_msg,
*          ls_create_itm_out TYPE /dkves/dcon_create_itm_out_msg,
          ls_response TYPE /dkves/dcon_create_itm_out_msg.

    SELECT SINGLE vkorg
      FROM vbak INTO @DATA(lv_vkorg)
     WHERE vbeln = @iv_vbeln.

    SELECT MAX( posnr )
           FROM  vbap
           INTO @DATA(lv_posnr)
           WHERE vbeln = @iv_vbeln
          AND uepos    = @space.

    ls_contract-drive_contract_create_item_req
        = VALUE #( kunde      = iv_customer_no
                   vbeln      = iv_vbeln
                   isocode    = /dkves/if_kp_isocode=>c_mtr_video_tolling
                   leoart_id  = /dkves/if_kp_leoart=>c_mtr_video_tolling
                   vkorg      = lv_vkorg
                   guelt_ab   = sy-datum
                   guelt_bis  = /dkves/sm_cl_constants=>gc_date_without_end  ).


    lv_posnr = lv_posnr + 10000.
    TRY."Contract Item Create durch Drive Webservice
        CALL FUNCTION '/DKVES/SM_T4E_CON_CRI_IDOC_STR'
          EXPORTING
            is_contract = ls_contract
            iv_posnr    = lv_posnr
          IMPORTING
            es_response = ls_response.

      CATCH /dkves/cx_dleo_drive_fault INTO DATA(lo_error).

        DATA(ls_message) = VALUE #( lo_error->standard-fault_detail[ 1 ]-text OPTIONAL ).

        cl_message_helper=>set_msg_vars_for_clike( ls_message ).

        RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.


  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~entry_inbound_process.

    "Order data
    DATA(lo_controller) = NEW /dkves/sm_cl_controller_sales( ).

    CREATE OBJECT lo_controller->/dkves/sm_if_entity_ctrl~mr_mapping TYPE /dkves/sm_cl_map_sales01.

    lo_controller->entry_inbound_process(
            EXPORTING  it_entry_input_sgntr_t = it_input
                       iv_test                = abap_false
                       iv_global_commit       = abap_false
            EXCEPTIONS error_occured = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(lt_log) = lo_controller->get_messages( /dkves/sm_cl_constants=>gc_false ).

    mo_logger->add( lt_log ).

    LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<log>) WHERE type CA 'EAX'.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE ID <log>-id TYPE <log>-type NUMBER <log>-number
        WITH <log>-message_v1 <log>-message_v2 <log>-message_v3 <log>-message_v4.
    ENDLOOP.

    ev_equnr = VALUE #( lt_log[ objtype = 'EQUI'
                                id      = '/DKVES/SM_APPL_ADT'
                                number  = '036' ]-message_v2 OPTIONAL ) .
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~get_contract_entity.
    "Instantiate contract objects
    DATA lo_entiy_ctrl TYPE REF TO  /dkves/sm_if_entity_ctrl.
    DATA lo_ctrl       TYPE REF TO  /dkves/sm_cl_controller.
    DATA lo_entity     TYPE REF TO  /dkves/sm_cl_entity.
    DATA ls_borid      TYPE         bapiborid.

    lo_entiy_ctrl = NEW /dkves/sm_cl_controller(  ).

    ls_borid-objkey   = iv_vbeln.
    ls_borid-objtype  = /dkves/sm_cl_constants=>gc_sapobj_contract.

    lo_entiy_ctrl->get_ref_by_objid(
      EXPORTING
        is_borid            = ls_borid
      IMPORTING
        er_entity           = lo_entity
      EXCEPTIONS
        objtype_not_allowed = 1
        entity_load_error   = 2
        OTHERS              = 3
    ).
    IF sy-subrc NE 0.
      "Error
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE ID sy-msgid
        TYPE sy-msgty
        NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      lo_ctrl ?= lo_entiy_ctrl.
      lo_ctrl->load( ).
      ro_entity_contract ?= lo_entity.
    ENDIF.
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~get_contract_id.
    SELECT vbeln
      FROM vbak
      INTO @rv_contract_id
      UP TO 1 ROWS
      WHERE kunnr = @iv_customer_id
        AND auart = @/dkves/if_auart=>c_contract
        AND vkorg = @/dkves/if_vkorg=>c_dkv
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e007 WITH iv_customer_id /dkves/if_auart=>c_contract.
    ENDIF.
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~get_main_order_item.

    io_entity_contract->load(
    EXCEPTIONS
      error_occured = 1                " Fehler aufgetreten --> get_messages
      OTHERS        = 2
  ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    io_entity_contract->get_data_item_bymat(
    EXPORTING
      iv_material        = /dkves/if_matnr=>c_mtr_video_tolling
    IMPORTING
      es_data            = rs_contract_item_data
    EXCEPTIONS
      not_found_item     = 1
      more_than_one_item = 2
      item_not_valid     = 3
      OTHERS             = 4
  ).
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~get_sales_contract.
    SELECT SINGLE vbak~vbeln
    FROM vbak
    WHERE kunnr = @iv_customer_no
      AND vbtyp = 'G' "Contract
      AND auart = @/dkves/if_auart=>c_dkv_contract
      INTO @rv_vbeln.
    IF sy-subrc = 0.
      "Check if also a Toll Turkey Position exists
      SELECT SINGLE @abap_true
        FROM vbap
       WHERE vbeln = @rv_vbeln
         AND matnr = @/dkves/if_matnr=>c_mtr_video_tolling
         INTO @DATA(lv_exists).
      IF sy-subrc = 0.
        "Zum Kunden &1 gibt es bereits die Position &2 im ZCC Vertrag &3
        RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
          MESSAGE w010 WITH iv_customer_no /dkves/if_matnr=>c_mtr_video_tolling rv_vbeln.
      ELSE.
        "Neue Position &1 zum bestehendem Vertrag &2 aufgenommen
        MESSAGE s011 WITH /dkves/if_matnr=>c_mtr_video_tolling rv_vbeln
         INTO DATA(lv_msg).
        mo_logger->add(  ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~prepare_data_for_entry_inbound.

    DATA ls_input TYPE  /dkves/sm_entry_input_sgntr.


    DATA: lr_order  TYPE REF TO /dkves/sm_attributes_ord,
          lr_orderx TYPE REF TO /dkves/sm_attributes_ordx.

    CREATE DATA lr_order.
    lr_order->* = VALUE /dkves/sm_attributes_ord(
        kunnr                        = is_contract_hdr-customerno
        vkorg                        = is_contract_hdr-sales_org
        vtweg                        = is_contract_hdr-distr_chan
        spart                        = is_contract_hdr-division
        matnr                        = is_contract_itm-material
        mandt                        = sy-mandt
        vertragsnummer               = is_contract_itm-vbeln
        vertragsposnr                = is_contract_itm-posnr
        isocode                      = /dkves/if_kp_isocode=>c_mtr_video_tolling
        leo_art                      = /dkves/if_kp_leoart=>c_mtr_video_tolling
        bstknr                       = iv_order_nr
        email_ad                     = is_order_hdr-email_address
        email_language               = is_order_hdr-email_language
        kfz_kennz                    = is_order_itm-lpn
        kfz_land                     = is_order_itm-country
*          kfz_art                      = 'L'
        kartenzusatz                 = is_order_itm-add_on
   ).

    CREATE DATA lr_orderx.
    lr_orderx->* = VALUE /dkves/sm_attributes_ordx(
      kunnr                        = abap_true
      vkorg                        = abap_true
      vtweg                        = abap_true
      spart                        = abap_true
      matnr                        = abap_true
      vertragsnummer               = abap_true
      vertragsposnr                = abap_true
      isocode                      = abap_true
      leo_art                      = abap_true
      kfz_kennz                    = abap_true
      kfz_land                     = abap_true
      kartenzusatz                 = abap_true ).

    ls_input-objdata       = lr_order.
    ls_input-objdatax      = lr_orderx.
    ls_input-objtask       = /dkves/sm_cl_constants=>gc_entity_mode_insert.
    ls_input-borid-objtype = /dkves/sm_cl_constants=>gc_sapobj_order.

    APPEND ls_input TO et_entry_input.


  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~save_mdlaat_attribute.
    DATA(ls_mdlaat)
         = VALUE /dkves/kp_mdlaat_vb( updkz    = 'I'
                                      equnr    = iv_equnr
                                      matnr    = /dkves/if_matnr=>c_mtr_video_tolling
                                      attribut = iv_attribute
                                      value    = iv_value ).
    CALL FUNCTION '/DKVES/KP_MDLAAT_SAVE'
      EXPORTING
        is_data       = ls_mdlaat
      EXCEPTIONS
        error_message = 99.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~save_order_head.
    "Insert values into /DKVES/KP_BESTEK
    INSERT /dkves/kp_bestek FROM is_order_hdr.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE e004(/dkves/kp_ordhead) WITH is_order_hdr-bstknr.
    ENDIF.

    "Save order header
    CALL FUNCTION '/DKVES/KP_ORDHEAD_SAVE_BUFFER'.
    CALL FUNCTION '/DKVES/KP_ORDHEAD_BUFFER_REFRE'.

  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_db_ob_prt~terminate_pan.
    DATA:
      lr_leo        TYPE REF TO   /dkves/sm_cl_entity_leo,
      lr_entity     TYPE REF TO   /dkves/sm_cl_entity,
      lr_ctrl       TYPE REF TO   /dkves/sm_cl_controller,
      ls_borid      TYPE           bapiborid,
      ls_attributes TYPE          /dkves/sm_attributes_leo.

    CREATE OBJECT lr_ctrl.

    ls_borid-objtype = /dkves/sm_cl_constants=>gc_sapobj_leo.
    ls_borid-objkey  = iv_equnr.

    lr_ctrl->/dkves/sm_if_entity_ctrl~get_ref_by_objid(
             EXPORTING  is_borid            = ls_borid
             IMPORTING  er_entity           = lr_entity
             EXCEPTIONS objtype_not_allowed = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    lr_leo ?= lr_entity.

    lr_leo->load( EXCEPTIONS error_occured = 1 ).
    IF sy-subrc <> 0.
    ELSE.
      ls_attributes = lr_leo->get_leo_data( ).
    ENDIF.


    lr_entity->set_mode( 'U' ). ">>>Update

    "Rückgabe-Grund setzen
    ls_attributes-/dkves/abmegrund = '01'.
    ls_attributes-/dkves/rueckrol = 'KUND'.
    ls_attributes-/dkves/abmedatum = sy-datum.
    ls_attributes-/dkves/verfdatum = ls_attributes-/dkves/abmedatum.
    "LEO-Daten wieder setzen
    lr_leo->set_leo_data( ls_attributes ).

    "SAVE
    lr_leo->save( EXCEPTIONS error_occured = 1 ).

    IF sy-subrc <> 0.
      et_return =  lr_leo->get_messages( ).
      lr_leo->rollback( ).
    ELSE.
      MESSAGE e006 WITH iv_equnr INTO DATA(lv_msg).
      mo_logger->add( lv_msg ).
      lr_leo->commit( ).
    ENDIF.
  ENDMETHOD.

  METHOD /dkves/kp_if_tvid_db_ob_prt~get_tvid_items.
  "Get main Item and FEE Positions
    SELECT posnr
    FROM vbap
   WHERE vbeln = @iv_vbeln
     AND ( matnr = @/dkves/if_matnr=>c_mtr_video_tolling
      OR uepos = ( SELECT posnr
                     FROM vbap
                    WHERE vbeln = @iv_vbeln
                      AND matnr = @/dkves/if_matnr=>c_mtr_video_tolling
                      AND uepos = '000000' ) )
   INTO TABLE @rt_posnr.

  ENDMETHOD.

ENDCLASS.
