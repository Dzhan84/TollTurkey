"! <p class="shorttext synchronized" lang="de">Toll Turkey: Video Tolling Order  Use Class</p>
CLASS /dkves/kp_cl_tvid_toll_ord_uc DEFINITION
  PUBLIC
  INHERITING FROM /dkves/ue_cl_hx_use_case_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /dkves/kp_if_tvid_order_ib_prt .

    ALIASES c_phase_1
      FOR /dkves/kp_if_tvid_order_ib_prt~c_phase_1 .
    ALIASES c_phase_2
      FOR /dkves/kp_if_tvid_order_ib_prt~c_phase_2 .
    ALIASES execute_video_tolling_order
      FOR /dkves/kp_if_tvid_order_ib_prt~execute_video_tolling_order .
    ALIASES schedule_video_tolling_order
      FOR /dkves/kp_if_tvid_order_ib_prt~schedule_video_tolling_order .
  PROTECTED SECTION.



  PRIVATE SECTION.
    	DATA:
      mo_bgrfc_ob_adp    TYPE REF TO /dkves/kp_if_tvid_bgrfc_ob_prt,
      mo_order_validator TYPE REF TO /dkves/kp_if_tvid_validator,
      mo_db_adp          TYPE REF TO /dkves/kp_if_tvid_db_ob_prt,
      mo_cntr_db_adp     TYPE REF TO /dkves/kp_if_cctr_ob_port,
      mo_mail_adp        TYPE REF TO /dkves/kp_if_tvid_mail_ob_prt.

    "! <p class="shorttext synchronized" lang="de">
    "! Creating a new order number and this comes from eclipse</p>
    METHODS create_new_order_number
      IMPORTING
        !iv_vbeln    TYPE vbeln
        !iv_posnr    TYPE /dkves/sm_attributes_con_itm-posnr
      EXPORTING
        !ev_order_nr TYPE /dkves/kp_ordernr
      RAISING
        /dkves/ue_cx_hx_t100_dynamic.

    METHODS process_order_items
      IMPORTING
        !iv_order_nr           TYPE /dkves/kp_ordernr
        !is_contract_head_data TYPE /dkves/sm_attributes_con
        !is_contract_item      TYPE /dkves/sm_attributes_con_itm
        !iv_contract_id        TYPE vbeln
        !is_order              TYPE /dkves/kp_mtr_vid_toll_order_s
      RAISING
        /dkves/ue_cx_hx_t100_dynamic.

    METHODS create_contract_with_tvid_pos
      IMPORTING is_sales_order_hdr TYPE  /dkves/sm_attributes_con
                is_sales_order_itm TYPE /dkves/sm_attributes_con_itm
      EXPORTING ev_vbeln           TYPE vbeln.


    METHODS add_tvid_pos_into_contract
      IMPORTING iv_contract_id     TYPE vbeln
                is_sales_order_itm TYPE /dkves/sm_attributes_con_itm.
    METHODS get_valid_to_date
      IMPORTING
        iv_date            TYPE dats
      RETURNING
        VALUE(rv_valid_to) TYPE dats.

    METHODS update_tvid_pos_dates
      IMPORTING
        iv_contract_id     TYPE vbeln
        it_tvid_posnr      TYPE posnr_tab
        is_sales_order_itm TYPE /dkves/sm_attributes_con_itm.

ENDCLASS.



CLASS /dkves/kp_cl_tvid_toll_ord_uc IMPLEMENTATION.


  METHOD /dkves/kp_if_tvid_order_ib_prt~execute_video_tolling_order.
    "Load Components
    mo_order_validator = CAST #( mo_comp_container->find_component_by_id( c_phase_2-validator ) ).
    mo_db_adp          = CAST #( mo_comp_container->find_component_by_id( c_phase_2-db_ob_adp ) ).
    mo_mail_adp        = CAST #( mo_comp_container->find_component_by_id( c_phase_2-mail_adp ) ).
    mo_cntr_db_adp     = CAST #( mo_comp_container->find_component_by_id( c_phase_2-cntr_db_adp ) ).



    "Get relevant Data
    DATA(lv_order_id)     = mo_db_adp->get_contract_id( is_order-customer_number ).
    DATA(lo_entity_contract) = mo_db_adp->get_contract_entity( lv_order_id ).


*   mo_order_validator->

    TRY. "Get the main Order Item for TollTurkex
        DATA(ls_sales_order_itm)   = mo_db_adp->get_main_order_item( lo_entity_contract ).

      CATCH /dkves/ue_cx_hx_t100_dynamic.
        "If Toll Turkey Main Position does not exist yet , we create one
        mo_db_adp->create_order_item(
            iv_vbeln       = lv_order_id
            iv_customer_no = is_order-customer_number
        ).
        "Get the order item after creation
        ls_sales_order_itm
            = mo_db_adp->get_main_order_item( lo_entity_contract ).
    ENDTRY.
    "Get Sales Order header
    DATA(ls_sales_order_hdr) = lo_entity_contract->get_data( ).

    create_new_order_number(
        EXPORTING iv_vbeln = lv_order_id
                  iv_posnr = ls_sales_order_itm-posnr
        IMPORTING ev_order_nr = DATA(lv_order_nr) ).

    "Process Order Items
    process_order_items(
           iv_order_nr           = lv_order_nr
           is_contract_head_data = ls_sales_order_hdr
           is_contract_item      = ls_sales_order_itm
           iv_contract_id        = lv_order_id
           is_order              = is_order ).

    TRY.
        "Get existing contract if exist
        DATA(lv_contract_id) =
             mo_db_adp->get_sales_contract( ls_sales_order_hdr-customerno ).

        IF lv_contract_id IS INITIAL.
          "Create a Sales Contract with Toll Turkey Position
          "if not exists
          create_contract_with_tvid_pos(
            EXPORTING is_sales_order_hdr = ls_sales_order_hdr
                      is_sales_order_itm = ls_sales_order_itm
            IMPORTING ev_vbeln           = lv_contract_id
          ).
        ELSE.
          "Contract exists => add Toll Turkey Position
          add_tvid_pos_into_contract( iv_contract_id     = lv_contract_id
                                      is_sales_order_itm = ls_sales_order_itm  ).
        ENDIF.

        DATA(lv_update_item_dates) = abap_true.

      CATCH  /dkves/ue_cx_hx_t100_dynamic INTO DATA(lcx_err).
        mo_logger->add( lcx_err ).


    ENDTRY.

    mo_mail_adp->register_email(
        iv_customer = ls_sales_order_hdr-customerno
        iv_order_nr = lv_order_nr ).

    COMMIT WORK AND WAIT.

    IF lv_update_item_dates = abap_true.
      "Not nice but did not find better solution yet
      "we need to update the veda dats for new contract Item
      "after it was created and commited.
      update_tvid_pos_dates(
          iv_contract_id     = lv_contract_id
          it_tvid_posnr      = mo_db_adp->get_tvid_items( lv_contract_id  )
          is_sales_order_itm = ls_sales_order_itm ).
      COMMIT WORK.
    ENDIF.

    "Release process lock for the next request
    /dkves/ue_cl_process_lock=>delete( iv_kunnr = ls_sales_order_hdr-customerno ).
  ENDMETHOD.


  METHOD /dkves/kp_if_tvid_order_ib_prt~schedule_video_tolling_order.


    "Load requiered components for the Use Case from component container
    mo_order_validator = CAST #( mo_comp_container->find_component_by_id( c_phase_1-validator )  ).
    mo_bgrfc_ob_adp    = CAST #( mo_comp_container->find_component_by_id( c_phase_1-bgrfc_ob_adp ) ).


    "Validate Service Booking Request
    mo_order_validator->validate_order_request(
        EXPORTING is_order_req       = is_order_req
        IMPORTING es_order_checked   = es_order_checked  ).

    IF es_order_checked-video_tolling_order_items  IS NOT INITIAL.

      "Set the process lock for the customer
      /dkves/ue_cl_process_lock=>create_for_customer(
          iv_kunnr   = is_order_req-customer_number
          iv_process = /dkves/if_ue_process_lock_id=>c_mtr_video_tolling_order
      ).

      "Schedule BGRFC for Service booking
      mo_bgrfc_ob_adp->schedule_service_booking( es_order_checked ).

      COMMIT WORK.

    ENDIF.


  ENDMETHOD.


  METHOD create_new_order_number.
    "Get order header
    DATA lt_ordhdr TYPE /dkves/kp_bestek_upd_t.

    CALL FUNCTION '/DKVES/KP_ORDHEAD_BUFFER_READ'
      IMPORTING
        et_ordhdr = lt_ordhdr.

    IF lt_ordhdr IS INITIAL.
      "Generate order header
      CALL FUNCTION '/DKVES/KP_ORDHEAD_CREATE'
        EXPORTING
          is_bestek = VALUE /dkves/kp_bestek( vbeln = iv_vbeln
                                              posnr = iv_posnr
                                              bestelldatum = sy-datum ).

      "Get order header
      CALL FUNCTION '/DKVES/KP_ORDHEAD_BUFFER_READ'
        IMPORTING
          et_ordhdr = lt_ordhdr.
    ENDIF.


    ev_order_nr = VALUE #( lt_ordhdr[ vbeln = iv_vbeln posnr = iv_posnr
                                    task = /dkves/kp_cl_constants=>gc_insert ]-bstknr ).
    IF ev_order_nr IS INITIAL.
      "Error generating ORDHDR
      RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
           MESSAGE e601(/dkves/kp_erw)
            WITH 'Error while generating Order Header'.     "#EC NOTEXT
    ENDIF.


  ENDMETHOD.


  METHOD create_contract_with_tvid_pos.
    DATA:
      lt_extension_in      TYPE TABLE OF bapiparex,
      lt_contract_data_in  TYPE STANDARD TABLE OF bapictr,
      lt_contract_data_inx TYPE STANDARD TABLE OF bapictrx.


    mo_cntr_db_adp->setup( ).

    DATA(ls_bape_vbap) = VALUE bape_vbap(
        /dkves/prod_ab   = is_sales_order_itm-/dkves/prod_ab
        /dkves/prod_bis  = get_valid_to_date( is_sales_order_itm-/dkves/prod_bis )   ).


    "Mark new position fields
    APPEND VALUE #( structure  = 'BAPE_VBAP'
                    valuepart1 = ls_bape_vbap ) TO lt_extension_in.


    "Set Begin/End Date for new Contract and TollTurkey Position
    APPEND VALUE #( con_st_dat = is_sales_order_hdr-valid_from
                    con_en_dat = get_valid_to_date( is_sales_order_hdr-valid_to )
                  ) TO lt_contract_data_in.

    APPEND VALUE #( updateflag = 'I'
                    con_st_dat = 'X'
                    con_en_dat = 'X'  ) TO lt_contract_data_inx.

    mo_cntr_db_adp->new(
        iv_kunnr        = is_sales_order_hdr-customerno
        is_header       = VALUE #( doc_type   = /dkves/if_auart=>c_dkv_contract
                                   sales_org  = /dkves/if_vkorg=>c_dkv
                                   distr_chan = '00'
                                   division   = /dkves/if_spart=>c_dkv )
        it_item         =  VALUE #( ( material = /dkves/if_matnr=>c_mtr_video_tolling ) )
        it_extension    = lt_extension_in
        it_contract_in  = lt_contract_data_in
        it_contract_inx = lt_contract_data_inx ).

    mo_cntr_db_adp->save(
     IMPORTING
        et_bapiret2 = DATA(lt_return)
        ev_vbeln    = ev_vbeln
    ).

    mo_cntr_db_adp->teardown( ).

    mo_logger->add( lt_return ).

  ENDMETHOD.

  METHOD get_valid_to_date.
    rv_valid_to = COND #(  WHEN iv_date = /dkves/if_dats=>c_date_max
                           THEN /dkves/if_dats=>c_date_max_zcc
                           ELSE iv_date ).
  ENDMETHOD.

  METHOD add_tvid_pos_into_contract.

    mo_cntr_db_adp->setup( iv_vbeln =  iv_contract_id ).

    DATA(ls_bape_vbap) = VALUE bape_vbap(
        /dkves/prod_ab   = is_sales_order_itm-/dkves/prod_ab
        /dkves/prod_bis  = get_valid_to_date( is_sales_order_itm-/dkves/prod_bis )
         ).

    mo_cntr_db_adp->new_item(
        is_item      = VALUE #( material = /dkves/if_matnr=>c_mtr_video_tolling )
        is_extension = VALUE #( structure  = 'BAPE_VBAP'
                                valuepart1 = ls_bape_vbap ) )  .

    mo_cntr_db_adp->save(
     IMPORTING
        et_bapiret2 = DATA(lt_return)
    ).

    mo_cntr_db_adp->teardown( ).
    mo_logger->add( lt_return ).
  ENDMETHOD.

  METHOD update_tvid_pos_dates.
    mo_cntr_db_adp->setup( iv_vbeln = iv_contract_id ).


    "When adding new position, header structures must be provided but can be initial
    mo_cntr_db_adp->change_header(
        is_header  = VALUE bapisdh1(  )
        is_headerx = VALUE bapisdh1x( updateflag = 'U')
    ).

    LOOP AT it_tvid_posnr ASSIGNING FIELD-SYMBOL(<tvid_posnr>).

      DATA(ls_item) = VALUE bapisditm( itm_number = <tvid_posnr> ).

      DATA(ls_itemx) = VALUE bapisditmx( itm_number = <tvid_posnr>
                                         updateflag = 'U').

      DATA(ls_contract_data_in)
          = VALUE bapictr( itm_number = <tvid_posnr>
                           con_st_dat = is_sales_order_itm-/dkves/prod_ab
                           con_en_dat = get_valid_to_date( /dkves/if_dats=>c_date_max_zcc ) ).

      DATA(ls_contract_data_inx)
          = VALUE bapictrx( itm_number = <tvid_posnr>
                            con_st_dat = 'X'
                            con_en_dat = 'X' ).


      mo_cntr_db_adp->change_item(
          is_item         = ls_item
          is_itemx        = ls_itemx
          is_contract_in  = ls_contract_data_in
          is_contract_inx = ls_contract_data_inx ).

    ENDLOOP.


    mo_cntr_db_adp->save(
     IMPORTING
        et_bapiret2 = DATA(lt_return)
    ).

    mo_cntr_db_adp->teardown( ).
    mo_logger->add( lt_return ).


  ENDMETHOD.

  METHOD process_order_items.
    DATA lt_entry_input TYPE /dkves/sm_entry_input_sgntr_t.

    LOOP AT  is_order-video_tolling_order_items
    ASSIGNING FIELD-SYMBOL(<order_itm>).
      FREE lt_entry_input.

      TRY.
          mo_order_validator->validate_order_data(
               is_request_item    = <order_itm>
               iv_contract_id     = iv_contract_id
               iv_posnr           = is_contract_item-posnr
               iv_order_id        = iv_order_nr ).

          mo_db_adp->prepare_data_for_entry_inbound(
              EXPORTING is_contract_hdr = is_contract_head_data
                        is_contract_itm = is_contract_item
                        is_order_hdr    = is_order
                        is_order_itm    = <order_itm>
                        iv_order_nr     = iv_order_nr
              IMPORTING et_entry_input  = lt_entry_input ).

          mo_db_adp->entry_inbound_process(
            EXPORTING it_input = lt_entry_input
            IMPORTING ev_equnr = DATA(lv_equnr) ).

          mo_db_adp->save_mdlaat_attribute(
              iv_equnr     = lv_equnr
              iv_attribute = 'EMAIL_AD'
              iv_value     = CONV #( is_order-email_address )
          ).

          mo_db_adp->save_mdlaat_attribute(
              iv_equnr     = lv_equnr
              iv_attribute = 'EMAIL_LANGUAGE'
              iv_value     = CONV #( is_order-email_language ) ).


          "Pan &1 zur Bestellung &2 wurde angelegt
          MESSAGE s009 WITH lv_equnr iv_order_nr
              INTO DATA(lv_msg).                            "#EC NEEDED
          mo_logger->add(  ).


        CATCH /dkves/ue_cx_hx_t100_dynamic INTO DATA(lcx_err).
          ROLLBACK WORK .
          RAISE EXCEPTION TYPE /dkves/ue_cx_hx_t100_dynamic
            EXPORTING
              previous = lcx_err.
      ENDTRY.
    ENDLOOP.

    "Bestellkopf anlegen
    mo_db_adp->save_order_head(
      VALUE #( bstknr       = iv_order_nr
               vbeln        = is_contract_item-vbeln
               posnr        = is_contract_item-posnr
               bestelldatum = sy-datum
               ernam        = sy-uname
               erdat        = sy-datum
               erzet        = sy-uzeit )  ).


  ENDMETHOD.


ENDCLASS.



